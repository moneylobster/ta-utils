;;; grading-moodle.el --- Moodle CSV/zip import and export -*- lexical-binding: t; -*-

;;; Commentary:
;; Handles importing student lists and submissions from Moodle,
;; and exporting grades back to Moodle-compatible CSV.

;;; Code:

(require 'cl-lib)

;; Data structures

(cl-defstruct grading-student
  "A student record from the Moodle participant list."
  first-name
  last-name
  id-number
  username
  email
  department
  groups
  submission-file  ; path to PDF, nil if no submission
  has-submission)  ; t if submission was found

;; CSV Parsing

(defun grading-moodle--parse-csv-line (line)
  "Parse a single CSV LINE into a list of fields.
Handles quoted fields with commas inside."
  (let ((result nil)
        (current "")
        (in-quote nil)
        (i 0)
        (len (length line)))
    (while (< i len)
      (let ((ch (aref line i)))
        (cond
         ((and (= ch ?\") (not in-quote))
          (setq in-quote t))
         ((and (= ch ?\") in-quote)
          (if (and (< (1+ i) len) (= (aref line (1+ i)) ?\"))
              (progn
                (setq current (concat current "\""))
                (cl-incf i))
            (setq in-quote nil)))
         ((and (= ch ?,) (not in-quote))
          (push (string-trim current) result)
          (setq current ""))
         (t
          (setq current (concat current (string ch))))))
      (cl-incf i))
    (push (string-trim current) result)
    (nreverse result)))

(defun grading-moodle-parse-student-list (file)
  "Parse a Moodle participant list CSV FILE.
Returns a list of `grading-student' structs."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((lines (split-string (buffer-string) "\n" t))
           (header (grading-moodle--parse-csv-line (car lines)))
           (data-lines (cdr lines))
           (students nil))
      ;; Verify expected columns
      (unless (and (member "First name" header)
                   (member "Last name" header))
        (error "CSV missing expected columns (First name, Last name). Got: %s"
               (string-join header ", ")))
      (let ((fi (cl-position "First name" header :test #'string=))
            (li (cl-position "Last name" header :test #'string=))
            (ii (cl-position "ID number" header :test #'string=))
            (ui (cl-position "Username" header :test #'string=))
            (ei (cl-position "Email address" header :test #'string=))
            (di (cl-position "Department" header :test #'string=))
            (gi (cl-position "Groups" header :test #'string=)))
        (dolist (line data-lines)
          (let ((fields (grading-moodle--parse-csv-line line)))
            (when (and fields (> (length fields) 1)
                       (not (string-empty-p (nth (or fi 0) fields))))
              (push (make-grading-student
                     :first-name (nth fi fields)
                     :last-name (nth li fields)
                     :id-number (and ii (nth ii fields))
                     :username (and ui (nth ui fields))
                     :email (and ei (nth ei fields))
                     :department (and di (nth di fields))
                     :groups (and gi (nth gi fields))
                     :submission-file nil
                     :has-submission nil)
                    students)))))
      (nreverse students))))

;; Submission matching

(defun grading-moodle--student-full-name (student)
  "Return \"FIRST LAST\" for STUDENT."
  (format "%s %s"
          (grading-student-first-name student)
          (grading-student-last-name student)))

(defun grading-moodle--normalize-name (name)
  "Normalize NAME for fuzzy matching: downcase, strip extra whitespace."
  (string-trim (downcase (replace-regexp-in-string "\\s-+" " " name))))

(defun grading-moodle--extract-name-from-moodle-dir (dirname)
  "Extract the student name from a Moodle submission directory name.
Format: \"NAME SURNAME_<numbers>_assignsubmission_file\"."
  (when (string-match "\\`\\(.+?\\)_[0-9]+_assignsubmission_file\\'" dirname)
    (match-string 1 dirname)))

(defun grading-moodle-match-submissions (students submissions-dir)
  "Match submission files in SUBMISSIONS-DIR to STUDENTS.
Returns a list of (student . match-status) where match-status is:
  'matched, 'no-submission, or 'unmatched.
Also returns unmatched files.
Modifies STUDENTS in place by setting submission-file and has-submission."
  (let* ((entries (directory-files submissions-dir t))
         (unmatched-files nil)
         (name-to-student (make-hash-table :test 'equal)))
    ;; Build name lookup table
    (dolist (s students)
      (puthash (grading-moodle--normalize-name
                (grading-moodle--student-full-name s))
               s name-to-student))
    ;; Pass 1: Match Moodle-format directories
    (dolist (entry entries)
      (when (file-directory-p entry)
        (let* ((dirname (file-name-nondirectory entry))
               (extracted-name (grading-moodle--extract-name-from-moodle-dir dirname)))
          (when extracted-name
            (let* ((normalized (grading-moodle--normalize-name extracted-name))
                   (student (gethash normalized name-to-student)))
              (if student
                  (let ((pdfs (directory-files entry t "\\.pdf\\'")))
                    (when pdfs
                      (setf (grading-student-submission-file student) (car pdfs))
                      (setf (grading-student-has-submission student) t)))
                (push (cons dirname "Moodle dir, no matching student") unmatched-files)))))))
    ;; Pass 2: Match loose PDFs (manually-added email submissions)
    (dolist (entry entries)
      (when (and (file-regular-p entry)
                 (string-match-p "\\.pdf\\'" entry))
        (let* ((basename (file-name-sans-extension (file-name-nondirectory entry)))
               (normalized (grading-moodle--normalize-name basename))
               (student (gethash normalized name-to-student)))
          (if (and student (not (grading-student-has-submission student)))
              (progn
                (setf (grading-student-submission-file student) entry)
                (setf (grading-student-has-submission student) t))
            (unless student
              (push (cons (file-name-nondirectory entry) "PDF, no matching student")
                    unmatched-files))))))
    (nreverse unmatched-files)))

(defun grading-moodle-extract-zip (zip-file target-dir)
  "Extract ZIP-FILE to TARGET-DIR using system unzip."
  (unless (file-exists-p zip-file)
    (error "Zip file not found: %s" zip-file))
  (make-directory target-dir t)
  (let ((default-directory target-dir))
    (call-process "unzip" nil nil nil "-o" "-q" (expand-file-name zip-file))))

;; Export

(defun grading-moodle--escape-csv-field (field)
  "Escape FIELD for CSV output."
  (if (string-match-p "[,\"\n]" field)
      (concat "\"" (replace-regexp-in-string "\"" "\"\"" field) "\"")
    field))

(defun grading-moodle-export-grades (students grades rubric output-file)
  "Export GRADES for STUDENTS to OUTPUT-FILE in Moodle CSV format.
RUBRIC is used to generate feedback comments.
GRADES is an alist of (username . question-grades)."
  (with-temp-file output-file
    (insert "Identifier,Full name,Grade,Feedback comments\n")
    (dolist (student students)
      (let* ((username (grading-student-username student))
             (full-name (grading-moodle--student-full-name student))
             (student-grades (assoc username grades))
             (total 0)
             (feedback-parts nil))
        (when student-grades
          (dolist (q (grading-rubric-questions rubric))
            (let* ((qnum (grading-question-number q))
                   (qgrade (cdr (assq qnum (cdr student-grades))))
                   (applied (plist-get qgrade :applied-items))
                   (custom-adj (or (plist-get qgrade :custom-adjustment) 0))
                   (note (plist-get qgrade :note))
                   (score (grading-rubric-compute-score q applied custom-adj))
                   (detail-parts nil))
              (cl-incf total score)
              ;; Build feedback for this question
              (dolist (item (grading-question-items q))
                (when (memq (grading-rubric-item-id item) applied)
                  (push (format "%s (%+d)"
                                (grading-rubric-item-description item)
                                (grading-rubric-item-points item))
                        detail-parts)))
              (when (/= custom-adj 0)
                (let ((reason (plist-get qgrade :custom-reason)))
                  (push (if (and reason (not (string-empty-p reason)))
                            (format "%s (%+d)" reason custom-adj)
                          (format "(%+d)" custom-adj))
                        detail-parts)))
              (let ((feedback (format "Q%d: %d/%d"
                                      qnum score (grading-question-max-points q))))
                (when detail-parts
                  (setq feedback (concat feedback " - "
                                         (string-join (nreverse detail-parts) ", "))))
                (when (and note (not (string-empty-p note)))
                  (setq feedback (concat feedback ". " note)))
                (push feedback feedback-parts))))
          (insert (format "%s,%s,%d,%s\n"
                          (grading-moodle--escape-csv-field username)
                          (grading-moodle--escape-csv-field full-name)
                          total
                          (grading-moodle--escape-csv-field
                           (string-join (nreverse feedback-parts) ". ")))))))))

(provide 'grading-moodle)
;;; grading-moodle.el ends here
