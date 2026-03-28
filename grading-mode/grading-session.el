;;; grading-session.el --- Session state and persistence -*- lexical-binding: t; -*-

;;; Commentary:
;; Manages the grading session state, including per-student grades,
;; save/load to disk, and retroactive rubric changes.

;;; Code:

(require 'cl-lib)
(require 'grading-rubric)

;; Data structures

(cl-defstruct grading-question-grade
  "Grade data for a single question for a single student."
  (applied-items nil)      ; list of rubric item id symbols
  (custom-adjustment 0)    ; integer
  (custom-reason nil)      ; string or nil
  (note nil))              ; string or nil

(cl-defstruct grading-student-grades
  "All grade data for a single student."
  username         ; string, key into student list
  question-grades) ; alist of (question-number . grading-question-grade)

(cl-defstruct grading-session
  "Complete grading session state."
  working-dir           ; string, path to working directory
  rubric-file           ; string, relative path to rubric file
  student-list-file     ; string, relative path to student list CSV
  submissions-dir       ; string, relative path to submissions directory
  rubric                ; grading-rubric struct (parsed)
  students              ; list of grading-student structs
  grades                ; list of grading-student-grades
  current-student-index ; integer, index into students list
  current-question      ; integer, question number
  unmatched-files)      ; list of (filename . reason)

(defvar grading-session--current nil
  "The current active grading session.")

;; Session initialization

(defun grading-session-init-grades (session)
  "Initialize empty grade records for all students in SESSION."
  (let ((rubric (grading-session-rubric session))
        (grades nil))
    (dolist (student (grading-session-students session))
      (let ((qgrades nil))
        (dolist (q (grading-rubric-questions rubric))
          (push (cons (grading-question-number q)
                      (make-grading-question-grade))
                qgrades))
        (push (make-grading-student-grades
               :username (grading-student-username student)
               :question-grades (nreverse qgrades))
              grades)))
    (setf (grading-session-grades session) (nreverse grades))))

;; Grade access

(defun grading-session-get-student-grades (session username)
  "Get grading-student-grades for USERNAME in SESSION."
  (cl-find username (grading-session-grades session)
           :key #'grading-student-grades-username
           :test #'string=))

(defun grading-session-get-question-grade (session username question-number)
  "Get grading-question-grade for USERNAME and QUESTION-NUMBER in SESSION."
  (let ((sg (grading-session-get-student-grades session username)))
    (when sg
      (cdr (assq question-number
                  (grading-student-grades-question-grades sg))))))

(defun grading-session-current-student (session)
  "Get the current student struct from SESSION."
  (nth (grading-session-current-student-index session)
       (grading-session-students session)))

(defun grading-session-student-graded-p (session username)
  "Return t if any rubric item has been toggled or note added for USERNAME."
  (let ((sg (grading-session-get-student-grades session username)))
    (when sg
      (cl-some (lambda (qg-pair)
                 (let ((qg (cdr qg-pair)))
                   (or (grading-question-grade-applied-items qg)
                       (grading-question-grade-note qg)
                       (/= 0 (grading-question-grade-custom-adjustment qg)))))
               (grading-student-grades-question-grades sg)))))

;; Grade modification

(defun grading-session-toggle-item (session username question-number item-id)
  "Toggle ITEM-ID for USERNAME on QUESTION-NUMBER in SESSION."
  (let ((qg (grading-session-get-question-grade session username question-number)))
    (if (memq item-id (grading-question-grade-applied-items qg))
        (setf (grading-question-grade-applied-items qg)
              (delq item-id (grading-question-grade-applied-items qg)))
      (push item-id (grading-question-grade-applied-items qg)))))

(defun grading-session-set-note (session username question-number note)
  "Set NOTE for USERNAME on QUESTION-NUMBER in SESSION."
  (let ((qg (grading-session-get-question-grade session username question-number)))
    (setf (grading-question-grade-note qg) note)))

(defun grading-session-set-custom-adjustment (session username question-number points reason)
  "Set custom POINTS adjustment with REASON for USERNAME on QUESTION-NUMBER."
  (let ((qg (grading-session-get-question-grade session username question-number)))
    (setf (grading-question-grade-custom-adjustment qg) points)
    (setf (grading-question-grade-custom-reason qg) reason)))

;; Score computation

(defun grading-session-question-score (session username question-number)
  "Compute the score for USERNAME on QUESTION-NUMBER."
  (let* ((rubric (grading-session-rubric session))
         (question (grading-rubric-question-by-number rubric question-number))
         (qg (grading-session-get-question-grade session username question-number)))
    (grading-rubric-compute-score question
                                  (grading-question-grade-applied-items qg)
                                  (grading-question-grade-custom-adjustment qg))))

(defun grading-session-total-score (session username)
  "Compute total score for USERNAME across all questions."
  (let ((total 0)
        (rubric (grading-session-rubric session)))
    (dolist (q (grading-rubric-questions rubric))
      (cl-incf total (grading-session-question-score
                      session username (grading-question-number q))))
    total))

;; Retroactive changes

(defun grading-session-retroactive-change (session item-id new-points)
  "Change the points for ITEM-ID to NEW-POINTS in SESSION.
Returns a list of (username old-score new-score) for affected students."
  (let* ((rubric (grading-session-rubric session))
         (found (grading-rubric-find-item rubric item-id))
         (affected nil))
    (unless found
      (error "Rubric item not found: %s" item-id))
    (let ((question (car found))
          (item (cdr found))
          (old-points (grading-rubric-item-points (cdr found))))
      ;; Collect affected students before changing
      (dolist (sg (grading-session-grades session))
        (let* ((username (grading-student-grades-username sg))
               (qg (cdr (assq (grading-question-number question)
                               (grading-student-grades-question-grades sg)))))
          (when (memq item-id (grading-question-grade-applied-items qg))
            (let ((old-total (grading-session-total-score session username)))
              (push (list username old-total nil) affected)))))
      ;; Apply the change
      (setf (grading-rubric-item-points item) new-points)
      ;; Compute new totals
      (dolist (entry affected)
        (setf (nth 2 entry)
              (grading-session-total-score session (car entry)))))
    affected))

;; Persistence

(defun grading-session--session-file (working-dir)
  "Return the session file path for WORKING-DIR."
  (expand-file-name ".grading-session.el" working-dir))

(defun grading-session--serialize-question-grade (qg)
  "Serialize a grading-question-grade QG to a plist."
  (list :applied-items (grading-question-grade-applied-items qg)
        :custom-adjustment (grading-question-grade-custom-adjustment qg)
        :custom-reason (grading-question-grade-custom-reason qg)
        :note (grading-question-grade-note qg)))

(defun grading-session--deserialize-question-grade (plist)
  "Deserialize a plist into a grading-question-grade."
  (make-grading-question-grade
   :applied-items (plist-get plist :applied-items)
   :custom-adjustment (or (plist-get plist :custom-adjustment) 0)
   :custom-reason (plist-get plist :custom-reason)
   :note (plist-get plist :note)))

(defun grading-session-save (session)
  "Save SESSION to disk."
  (let ((file (grading-session--session-file
               (grading-session-working-dir session)))
        (data (list
               'grading-session
               :rubric-file (grading-session-rubric-file session)
               :student-list-file (grading-session-student-list-file session)
               :submissions-dir (grading-session-submissions-dir session)
               :current-student-index (grading-session-current-student-index session)
               :current-question (grading-session-current-question session)
               :grades
               (mapcar
                (lambda (sg)
                  (list
                   :username (grading-student-grades-username sg)
                   :question-grades
                   (mapcar
                    (lambda (qg-pair)
                      (cons (car qg-pair)
                            (grading-session--serialize-question-grade (cdr qg-pair))))
                    (grading-student-grades-question-grades sg))))
                (grading-session-grades session)))))
    (with-temp-file file
      (let ((print-level nil)
            (print-length nil))
        (pp data (current-buffer))))
    (message "Grading session saved to %s" file)))

(defun grading-session-load (working-dir rubric students)
  "Load a saved session from WORKING-DIR.
RUBRIC and STUDENTS must already be loaded.
Returns a grading-session or nil if no saved session exists."
  (let ((file (grading-session--session-file working-dir)))
    (when (file-exists-p file)
      (let* ((data (with-temp-buffer
                     (insert-file-contents file)
                     (read (buffer-string))))
             (plist (cdr data))
             (session (make-grading-session
                       :working-dir working-dir
                       :rubric-file (plist-get plist :rubric-file)
                       :student-list-file (plist-get plist :student-list-file)
                       :submissions-dir (plist-get plist :submissions-dir)
                       :rubric rubric
                       :students students
                       :current-student-index (or (plist-get plist :current-student-index) 0)
                       :current-question (or (plist-get plist :current-question) 1)
                       :grades nil)))
        ;; Restore grades
        (let ((saved-grades (plist-get plist :grades))
              (grades nil))
          (dolist (sg-data saved-grades)
            (let* ((username (plist-get sg-data :username))
                   (qg-data (plist-get sg-data :question-grades))
                   (question-grades
                    (mapcar (lambda (qg-pair)
                              (cons (car qg-pair)
                                    (grading-session--deserialize-question-grade
                                     (cdr qg-pair))))
                            qg-data)))
              (push (make-grading-student-grades
                     :username username
                     :question-grades question-grades)
                    grades)))
          ;; Add grades for any new students not in saved session
          (dolist (student students)
            (unless (cl-find (grading-student-username student) grades
                             :key #'grading-student-grades-username
                             :test #'string=)
              (let ((qgrades nil))
                (dolist (q (grading-rubric-questions rubric))
                  (push (cons (grading-question-number q)
                              (make-grading-question-grade))
                        qgrades))
                (push (make-grading-student-grades
                       :username (grading-student-username student)
                       :question-grades (nreverse qgrades))
                      grades))))
          (setf (grading-session-grades session) (nreverse grades)))
        session))))

(defun grading-session-export-grades-alist (session)
  "Convert SESSION grades to the alist format expected by `grading-moodle-export-grades'.
Returns alist of (username . ((qnum . plist) ...))."
  (mapcar
   (lambda (sg)
     (cons (grading-student-grades-username sg)
           (mapcar
            (lambda (qg-pair)
              (cons (car qg-pair)
                    (grading-session--serialize-question-grade (cdr qg-pair))))
            (grading-student-grades-question-grades sg))))
   (grading-session-grades session)))

(provide 'grading-session)
;;; grading-session.el ends here
