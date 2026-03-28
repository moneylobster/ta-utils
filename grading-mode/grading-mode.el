;;; grading-mode.el --- Emacs grading mode for Moodle homework -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry point for the grading system. Provides `grading-start',
;; `grading-resume', and `grading-finalize' commands.
;;
;; Usage:
;; 1. Place in your working directory:
;;    - rubric.el     (rubric in sexp format)
;;    - participants.csv (Moodle participant list)
;;    - submissions/  (extracted Moodle zip or manually-added PDFs)
;; 2. M-x grading-start, select the working directory
;; 3. Grade using keyboard shortcuts in the panel
;; 4. M-x grading-finalize to export grades

;;; Code:

(require 'cl-lib)
(require 'grading-rubric)
(require 'grading-moodle)
(require 'grading-session)
(require 'grading-panel)
(require 'grading-overview)
(require 'grading-stats)

(defgroup grading nil
  "Grading mode for Moodle homework."
  :group 'tools
  :prefix "grading-")

(defcustom grading-rubric-filename "rubric.el"
  "Default filename for the rubric file."
  :type 'string
  :group 'grading)

(defcustom grading-student-list-filename "participants.csv"
  "Default filename for the Moodle participant list."
  :type 'string
  :group 'grading)

(defcustom grading-submissions-dirname "submissions"
  "Default directory name for student submissions."
  :type 'string
  :group 'grading)

;;;###autoload
(defun grading-start (dir)
  "Start a grading session in directory DIR.
Looks for rubric.el, participants.csv, and submissions/ in DIR.
If a saved session exists, offers to resume it."
  (interactive "DGrading directory: ")
  (let* ((dir (expand-file-name dir))
         (rubric-file (expand-file-name grading-rubric-filename dir))
         (student-file (expand-file-name grading-student-list-filename dir))
         (subs-dir (expand-file-name grading-submissions-dirname dir)))
    ;; Validate required files exist
    (unless (file-exists-p rubric-file)
      (error "Rubric file not found: %s" rubric-file))
    (unless (file-exists-p student-file)
      (error "Student list not found: %s" student-file))
    (unless (file-directory-p subs-dir)
      (error "Submissions directory not found: %s" subs-dir))
    ;; Load rubric and students
    (let* ((rubric (grading-rubric-load rubric-file))
           (warnings (grading-rubric-validate rubric))
           (students (grading-moodle-parse-student-list student-file)))
      ;; Show rubric warnings
      (when warnings
        (with-output-to-temp-buffer "*Rubric Warnings*"
          (dolist (w warnings)
            (princ (format "⚠ %s\n" w))))
        (unless (y-or-n-p "Rubric has warnings (see buffer). Continue? ")
          (error "Aborted")))
      ;; Match submissions
      (let ((unmatched (grading-moodle-match-submissions students subs-dir)))
        ;; Report unmatched
        (when unmatched
          (with-output-to-temp-buffer "*Unmatched Submissions*"
            (princ "The following files could not be matched to students:\n\n")
            (dolist (entry unmatched)
              (princ (format "  %s (%s)\n" (car entry) (cdr entry)))))
          (unless (y-or-n-p "Some submissions unmatched (see buffer). Continue? ")
            (error "Aborted")))
        ;; Report students without submissions
        (let ((no-sub (cl-remove-if #'grading-student-has-submission students)))
          (when no-sub
            (message "%d student(s) have no submission: %s"
                     (length no-sub)
                     (string-join
                      (mapcar (lambda (s)
                                (format "%s %s"
                                        (grading-student-first-name s)
                                        (grading-student-last-name s)))
                              no-sub)
                      ", "))))
        ;; Check for saved session
        (let ((session (grading-session-load dir rubric students)))
          (if (and session
                   (y-or-n-p "Saved session found. Resume? "))
              (progn
                (setf (grading-session-unmatched-files session) unmatched)
                (grading-panel-setup session))
            ;; Create new session
            (let ((new-session (make-grading-session
                                :working-dir dir
                                :rubric-file grading-rubric-filename
                                :student-list-file grading-student-list-filename
                                :submissions-dir grading-submissions-dirname
                                :rubric rubric
                                :students students
                                :current-student-index 0
                                :current-question (grading-question-number
                                                   (car (grading-rubric-questions rubric)))
                                :unmatched-files unmatched)))
              (grading-session-init-grades new-session)
              (grading-session-save new-session)
              (grading-panel-setup new-session))))))))

;;;###autoload
(defun grading-resume (dir)
  "Resume a saved grading session from DIR."
  (interactive "DResume grading in directory: ")
  (let* ((dir (expand-file-name dir))
         (session-file (expand-file-name ".grading-session.el" dir)))
    (unless (file-exists-p session-file)
      (error "No saved session found in %s" dir))
    (let* ((rubric-file (expand-file-name grading-rubric-filename dir))
           (student-file (expand-file-name grading-student-list-filename dir))
           (subs-dir (expand-file-name grading-submissions-dirname dir))
           (rubric (grading-rubric-load rubric-file))
           (students (grading-moodle-parse-student-list student-file)))
      (grading-moodle-match-submissions students subs-dir)
      (let ((session (grading-session-load dir rubric students)))
        (unless session
          (error "Failed to load session from %s" dir))
        (grading-panel-setup session)))))

;;;###autoload
(defun grading-finalize ()
  "Finalize grading: show statistics and export to Moodle CSV."
  (interactive)
  (unless grading-session--current
    (error "No active grading session"))
  (let* ((session grading-session--current)
         (rubric (grading-session-rubric session))
         (students (grading-session-students session))
         (graded (grading-stats-graded-count session))
         (total (length students)))
    ;; Warn about ungraded students
    (when (< graded total)
      (unless (y-or-n-p (format "%d/%d students graded. Finalize anyway? "
                                graded total))
        (error "Aborted")))
    ;; Show final statistics
    (with-output-to-temp-buffer "*Grading Summary*"
      (princ (format "══ Final Grading Summary ══\n\n"))
      (princ (format "Students graded: %d/%d\n\n" graded total))
      (princ "Per-Question Statistics:\n")
      (dolist (q (grading-rubric-questions rubric))
        (let ((summary (grading-stats-question-summary
                        session (grading-question-number q))))
          (princ (format "  Q%d (%s): mean %.1f/%d  stdev %.1f\n"
                         (grading-question-number q)
                         (grading-question-title q)
                         (car summary)
                         (grading-question-max-points q)
                         (cdr summary)))))
      (let ((overall (grading-stats-overall-summary session)))
        (princ (format "\n  Overall: mean %.1f/%d  stdev %.1f\n"
                       (car overall)
                       (grading-rubric-total rubric)
                       (cdr overall))))
      (princ "\nGrade Distribution:\n")
      (let* ((dist (grading-stats-distribution session 5))
             (max-count (max 1 (apply #'max (mapcar #'cdr dist)))))
        (dolist (bin dist)
          (let* ((bar-len (round (* 30 (/ (float (cdr bin)) max-count))))
                 (bar (make-string bar-len ?█)))
            (princ (format "  %7s: %s %d\n" (car bin) bar (cdr bin)))))))
    ;; Export
    (let* ((default-name (format "grades-%s.csv"
                                 (format-time-string "%Y%m%d")))
           (output-file (read-file-name "Export grades to: "
                                        (grading-session-working-dir session)
                                        nil nil default-name)))
      (grading-moodle-export-grades
       students
       (grading-session-export-grades-alist session)
       rubric
       output-file)
      (message "Grades exported to %s" output-file))))

(provide 'grading-mode)
;;; grading-mode.el ends here
