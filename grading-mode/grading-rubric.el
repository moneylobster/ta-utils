;;; grading-rubric.el --- Rubric parsing and data model -*- lexical-binding: t; -*-

;;; Commentary:
;; Parses rubric sexp format and provides data structures for the grading system.

;;; Code:

(require 'cl-lib)

;; Data structures

(cl-defstruct grading-rubric-item
  "A single rubric item (mistake or milestone)."
  type        ; 'mistake or 'milestone
  id          ; symbol, e.g. 'wrong-formula
  points      ; integer (negative for mistakes, positive for milestones)
  description ; string
  letter)     ; char, assigned during parsing for keybinding

(cl-defstruct grading-question
  "A single question in the rubric."
  number       ; integer
  title        ; string
  max-points   ; integer
  default-grade ; integer (max-points for deductive, 0 for additive)
  items)        ; list of grading-rubric-item

(cl-defstruct grading-rubric
  "A complete rubric."
  total      ; integer, total possible points
  questions) ; list of grading-question

;; Parsing

(defun grading-rubric--parse-item (sexp letter)
  "Parse a rubric item SEXP, assigning LETTER for keybinding."
  (let ((type (nth 0 sexp))
        (plist (cdr sexp)))
    (unless (memq type '(mistake milestone))
      (error "Unknown rubric item type: %s" type))
    (make-grading-rubric-item
     :type type
     :id (plist-get plist :id)
     :points (plist-get plist :points)
     :description (plist-get plist :description)
     :letter letter)))

(defun grading-rubric--parse-question (sexp)
  "Parse a question SEXP."
  (let* ((number (nth 1 sexp))
         (plist (cddr sexp))
         (item-sexps (plist-get plist :items))
         (letter ?a)
         (items (mapcar (lambda (item-sexp)
                          (prog1
                              (grading-rubric--parse-item item-sexp letter)
                            (setq letter (1+ letter))))
                        item-sexps)))
    (make-grading-question
     :number number
     :title (plist-get plist :title)
     :max-points (plist-get plist :max-points)
     :default-grade (plist-get plist :default-grade)
     :items items)))

(defun grading-rubric-parse (sexp)
  "Parse a complete rubric SEXP.
SEXP should be of the form (grading-rubric :total N :questions (...))."
  (unless (eq (car sexp) 'grading-rubric)
    (error "Expected (grading-rubric ...), got: %s" (car sexp)))
  (let* ((plist (cdr sexp))
         (total (plist-get plist :total))
         (question-sexps (plist-get plist :questions))
         (questions (mapcar #'grading-rubric--parse-question question-sexps)))
    (make-grading-rubric
     :total total
     :questions questions)))

(defun grading-rubric-load (file)
  "Load and parse a rubric from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (grading-rubric-parse (read (buffer-string)))))

;;;###autoload
(defun grading-rubric-init (dir)
  "Create a rubric.el template in DIR and open it for editing."
  (interactive "DCreate rubric in directory: ")
  (let ((file (expand-file-name "rubric.el" dir)))
    (when (file-exists-p file)
      (unless (y-or-n-p "rubric.el already exists. Overwrite? ")
        (user-error "Aborted")))
    (with-temp-file file
      (insert "\
;; Grading rubric — edit this file then run M-x grading-start
;;
;; :total           sum of all :max-points
;;
;; Per question:
;;   :max-points    maximum score for this question
;;   :default-grade starting score before items are applied
;;                  set to :max-points value for deductive (start high, subtract mistakes)
;;                  set to 0 for additive (start at zero, add milestones)
;;
;; Item types:
;;   mistake    typically negative :points (deductions)
;;   milestone  typically positive :points (additions)
;;
;; Each item needs a unique :id (a symbol) within its question.
;; Toggle keys a-z are assigned in order.

\(grading-rubric
 :total 100
 :questions
 ((question 1
   :title \"Problem 1\"
   :max-points 50
   :default-grade 50
   :items
   ((mistake :id incomplete
             :points -10
             :description \"Incomplete solution\")
    (mistake :id arithmetic-error
             :points -5
             :description \"Arithmetic/algebra error\")))
  (question 2
   :title \"Problem 2\"
   :max-points 50
   :default-grade 0
   :items
   ((milestone :id correct-setup
               :points 20
               :description \"Correct problem setup\")
    (milestone :id correct-answer
               :points 30
               :description \"Correct final answer\")))))
"))
    (find-file file)
    (message "Rubric template created. Edit and save, then run M-x grading-start.")))

(defun grading-rubric-validate (rubric)
  "Validate RUBRIC, returning a list of warnings (empty if valid)."
  (let ((warnings nil)
        (question-total 0))
    (dolist (q (grading-rubric-questions rubric))
      (cl-incf question-total (grading-question-max-points q))
      (when (and (> (grading-question-default-grade q) 0)
                 (/= (grading-question-default-grade q)
                     (grading-question-max-points q)))
        (push (format "Q%d: default-grade (%d) differs from max-points (%d) — intentional?"
                       (grading-question-number q)
                       (grading-question-default-grade q)
                       (grading-question-max-points q))
              warnings))
      (let ((ids (mapcar #'grading-rubric-item-id (grading-question-items q))))
        (unless (= (length ids) (length (cl-remove-duplicates ids :test #'eq)))
          (push (format "Q%d: duplicate item IDs" (grading-question-number q))
                warnings))))
    (unless (= question-total (grading-rubric-total rubric))
      (push (format "Sum of max-points (%d) != declared total (%d)"
                     question-total (grading-rubric-total rubric))
            warnings))
    (nreverse warnings)))

(defun grading-rubric-find-item (rubric item-id)
  "Find the rubric item with ITEM-ID in RUBRIC. Returns (question . item) or nil."
  (cl-block nil
    (dolist (q (grading-rubric-questions rubric))
      (dolist (item (grading-question-items q))
        (when (eq (grading-rubric-item-id item) item-id)
          (cl-return (cons q item)))))))

(defun grading-rubric-question-by-number (rubric n)
  "Get question number N from RUBRIC."
  (cl-find n (grading-rubric-questions rubric)
           :key #'grading-question-number))

(defun grading-rubric-compute-score (question applied-items custom-adjustment)
  "Compute score for QUESTION given APPLIED-ITEMS (list of ids) and CUSTOM-ADJUSTMENT."
  (let ((score (grading-question-default-grade question)))
    (dolist (item (grading-question-items question))
      (when (memq (grading-rubric-item-id item) applied-items)
        (cl-incf score (grading-rubric-item-points item))))
    (cl-incf score custom-adjustment)
    (max 0 (min score (grading-question-max-points question)))))

(provide 'grading-rubric)
;;; grading-rubric.el ends here
