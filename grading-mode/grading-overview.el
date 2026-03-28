;;; grading-overview.el --- Overview and statistics buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Displays grading progress, per-question statistics, grade distribution,
;; and a student list with scores. Supports navigation and retroactive changes.

;;; Code:

(require 'cl-lib)
(require 'grading-rubric)
(require 'grading-session)
(require 'grading-moodle)
(require 'grading-stats)

(defvar grading-overview--buffer nil
  "The overview buffer.")

(defface grading-overview-header-face
  '((t :weight bold :height 1.3))
  "Face for the overview title.")

(defface grading-overview-section-face
  '((t :weight bold :underline t))
  "Face for section headers.")

(defface grading-overview-graded-face
  '((t :foreground "green"))
  "Face for graded students.")

(defface grading-overview-ungraded-face
  '((t :foreground "gray60"))
  "Face for ungraded students.")

(defface grading-overview-bar-face
  '((t :foreground "cyan"))
  "Face for histogram bars.")

;; Rendering

(defun grading-overview--render ()
  "Render the overview buffer."
  (let* ((session grading-session--current)
         (rubric (grading-session-rubric session))
         (students (grading-session-students session))
         (total-students (length students))
         (graded-count (grading-stats-graded-count session))
         (inhibit-read-only t))
    (with-current-buffer grading-overview--buffer
      (erase-buffer)
      ;; Title
      (insert (propertize "══ Grading Overview ══\n\n"
                          'face 'grading-overview-header-face))
      ;; Progress
      (insert (format "Progress: %d/%d students graded\n\n"
                      graded-count total-students))
      ;; Per-question statistics
      (insert (propertize "── Per-Question Statistics ──\n"
                          'face 'grading-overview-section-face))
      (dolist (q (grading-rubric-questions rubric))
        (let* ((qnum (grading-question-number q))
               (summary (grading-stats-question-summary session qnum)))
          (insert (format "  Q%d (%s):  mean %.1f/%d  stdev %.1f\n"
                          qnum
                          (grading-question-title q)
                          (car summary)
                          (grading-question-max-points q)
                          (cdr summary)))))
      (let ((overall (grading-stats-overall-summary session)))
        (insert (format "  Overall:  mean %.1f/%d  stdev %.1f\n\n"
                        (car overall)
                        (grading-rubric-total rubric)
                        (cdr overall))))
      ;; Grade distribution
      (insert (propertize "── Grade Distribution ──\n"
                          'face 'grading-overview-section-face))
      (let* ((dist (grading-stats-distribution session 5))
             (max-count (max 1 (apply #'max (mapcar #'cdr dist)))))
        (dolist (bin dist)
          (let* ((label (car bin))
                 (count (cdr bin))
                 (bar-len (round (* 30 (/ (float count) max-count))))
                 (bar (make-string bar-len ?█)))
            (insert (format "  %7s: %s %d\n"
                            label
                            (propertize bar 'face 'grading-overview-bar-face)
                            count)))))
      (insert "\n")
      ;; Unmatched files warning
      (when (grading-session-unmatched-files session)
        (insert (propertize "── Unmatched Files ──\n"
                            'face 'grading-overview-section-face))
        (dolist (entry (grading-session-unmatched-files session))
          (insert (format "  ⚠ %s (%s)\n" (car entry) (cdr entry))))
        (insert "\n"))
      ;; Student list
      (insert (propertize "── Student List ──\n"
                          'face 'grading-overview-section-face))
      (let ((idx 0))
        (dolist (student students)
          (let* ((username (grading-student-username student))
                 (graded (grading-session-student-graded-p session username))
                 (total (if graded
                            (number-to-string
                             (grading-session-total-score session username))
                          "--"))
                 (marker (if graded "✓" " "))
                 (face (if graded 'grading-overview-graded-face
                         'grading-overview-ungraded-face))
                 (no-sub (if (grading-student-has-submission student) "" " [no submission]")))
            (insert (propertize
                     (format "  [%s] %-30s %s/%d%s\n"
                             marker
                             (format "%s %s"
                                     (grading-student-first-name student)
                                     (grading-student-last-name student))
                             total
                             (grading-rubric-total rubric)
                             no-sub)
                     'face face
                     'grading-student-index idx)))
          (cl-incf idx)))
      ;; Help
      (insert (propertize
               "\nRET: jump to student  r: retroactive rubric change  q: close overview\n"
               'face 'grading-panel-help-face))
      (goto-char (point-min)))))

;; Commands

(defun grading-overview-jump-to-student ()
  "Jump to the student on the current line."
  (interactive)
  (let ((idx (get-text-property (point) 'grading-student-index)))
    (if idx
        (let ((session grading-session--current))
          (setf (grading-session-current-student-index session) idx)
          (setf (grading-session-current-question session)
                (grading-question-number
                 (car (grading-rubric-questions (grading-session-rubric session)))))
          (kill-buffer grading-overview--buffer)
          (grading-panel-setup session))
      (message "No student on this line"))))

(defun grading-overview-retroactive-change ()
  "Modify a rubric item's points retroactively."
  (interactive)
  (let* ((session grading-session--current)
         (rubric (grading-session-rubric session))
         ;; Build completion candidates
         (candidates
          (cl-loop for q in (grading-rubric-questions rubric)
                   append (cl-loop for item in (grading-question-items q)
                                   collect (cons (format "Q%d: %s [%s] (%+d)"
                                                         (grading-question-number q)
                                                         (grading-rubric-item-description item)
                                                         (grading-rubric-item-id item)
                                                         (grading-rubric-item-points item))
                                                 (grading-rubric-item-id item)))))
         (choice (completing-read "Modify rubric item: " candidates nil t))
         (item-id (cdr (assoc choice candidates)))
         (found (grading-rubric-find-item rubric item-id))
         (current-points (grading-rubric-item-points (cdr found)))
         (new-points (read-number
                      (format "New points for %s (currently %+d): " item-id current-points))))
    (when (= new-points current-points)
      (message "No change.")
      (cl-return-from grading-overview-retroactive-change))
    (let ((affected (grading-session-retroactive-change session item-id new-points)))
      (if (null affected)
          (message "Changed %s to %+d (no students affected)" item-id new-points)
        ;; Show preview
        (with-output-to-temp-buffer "*Retroactive Change Preview*"
          (princ (format "Changed %s: %+d → %+d\n\nAffected students:\n"
                         item-id current-points new-points))
          (dolist (entry affected)
            (princ (format "  %s: %d → %d\n"
                           (car entry) (cadr entry) (caddr entry)))))
        (message "Changed %s to %+d (%d students affected)"
                 item-id new-points (length affected))))
    (grading-session-save session)
    (grading-overview--render)))

(defun grading-overview-close ()
  "Close the overview buffer and return to the grading panel."
  (interactive)
  (kill-buffer grading-overview--buffer)
  (grading-panel-setup grading-session--current))

;; Keymap

(defvar grading-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'grading-overview-jump-to-student)
    (define-key map "r" #'grading-overview-retroactive-change)
    (define-key map "q" #'grading-overview-close)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    map)
  "Keymap for `grading-overview-mode'.")

(define-derived-mode grading-overview-mode special-mode "Grading-Overview"
  "Major mode for the grading overview."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun grading-overview-show ()
  "Show the grading overview buffer."
  (interactive)
  (setq grading-overview--buffer (get-buffer-create "*Grading Overview*"))
  (delete-other-windows)
  (switch-to-buffer grading-overview--buffer)
  (grading-overview-mode)
  (grading-overview--render))

(provide 'grading-overview)
;;; grading-overview.el ends here
