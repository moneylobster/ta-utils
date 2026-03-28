;;; grading-panel.el --- Grading panel buffer and UI -*- lexical-binding: t; -*-

;;; Commentary:
;; The grading panel is the main interaction buffer during grading.
;; It displays the current student's grades and provides keybindings
;; for toggling rubric items, adding notes, and navigating.

;;; Code:

(require 'cl-lib)
(require 'grading-rubric)
(require 'grading-session)
(require 'grading-moodle)

(defvar grading-panel--pdf-window nil
  "Window displaying the PDF.")
(defvar grading-panel--panel-window nil
  "Window displaying the grading panel.")
(defvar grading-panel--panel-buffer nil
  "The grading panel buffer.")

(defface grading-panel-header-face
  '((t :weight bold :height 1.2))
  "Face for the student header in the grading panel.")

(defface grading-panel-question-face
  '((t :weight bold :underline t))
  "Face for question headers in the grading panel.")

(defface grading-panel-score-face
  '((t :weight bold))
  "Face for scores in the grading panel.")

(defface grading-panel-applied-face
  '((t :foreground "red"))
  "Face for applied mistake items.")

(defface grading-panel-milestone-face
  '((t :foreground "green"))
  "Face for applied milestone items.")

(defface grading-panel-note-face
  '((t :foreground "cyan" :slant italic))
  "Face for notes.")

(defface grading-panel-help-face
  '((t :foreground "gray60"))
  "Face for the help line at the bottom.")

(defface grading-panel-no-submission-face
  '((t :foreground "orange" :weight bold))
  "Face for the no-submission warning.")

;; Panel rendering

(defun grading-panel--render ()
  "Render the grading panel for the current student."
  (let* ((session grading-session--current)
         (student (grading-session-current-student session))
         (username (grading-student-username student))
         (rubric (grading-session-rubric session))
         (idx (grading-session-current-student-index session))
         (total-students (length (grading-session-students session)))
         (current-qnum (grading-session-current-question session))
         (inhibit-read-only t))
    (with-current-buffer grading-panel--panel-buffer
      (erase-buffer)
      ;; Header
      (insert (propertize
               (format "══ %s %s (%s) ══ [%d/%d]\n"
                       (grading-student-first-name student)
                       (grading-student-last-name student)
                       (or username "?")
                       (1+ idx) total-students)
               'face 'grading-panel-header-face))
      ;; No-submission warning
      (unless (grading-student-has-submission student)
        (insert (propertize "\n  ⚠ No submission found\n"
                            'face 'grading-panel-no-submission-face)))
      ;; Questions
      (let ((grand-total 0))
        (dolist (q (grading-rubric-questions rubric))
          (let* ((qnum (grading-question-number q))
                 (qg (grading-session-get-question-grade session username qnum))
                 (score (grading-session-question-score session username qnum))
                 (is-current (= qnum current-qnum)))
            (cl-incf grand-total score)
            (insert "\n")
            ;; Question header
            (insert (propertize
                     (format "%s Q%d: %s ── [%d/%d]\n"
                             (if is-current "▶" "──")
                             qnum
                             (grading-question-title q)
                             score
                             (grading-question-max-points q))
                     'face 'grading-panel-question-face))
            ;; Rubric items
            (dolist (item (grading-question-items q))
              (let* ((applied (memq (grading-rubric-item-id item)
                                    (grading-question-grade-applied-items qg)))
                     (marker (cond
                              ((not applied) " ")
                              ((eq (grading-rubric-item-type item) 'mistake) "X")
                              (t "+")))
                     (face (cond
                            ((not applied) 'default)
                            ((eq (grading-rubric-item-type item) 'mistake)
                             'grading-panel-applied-face)
                            (t 'grading-panel-milestone-face))))
                (insert (propertize
                         (format "  [%s] (%c) %-35s %+d\n"
                                 marker
                                 (grading-rubric-item-letter item)
                                 (grading-rubric-item-description item)
                                 (grading-rubric-item-points item))
                         'face face))))
            ;; Custom adjustment
            (let ((adj (grading-question-grade-custom-adjustment qg)))
              (when (/= adj 0)
                (insert (propertize
                         (format "  [*] Custom adjustment: %+d%s\n"
                                 adj
                                 (let ((reason (grading-question-grade-custom-reason qg)))
                                   (if reason (format " (%s)" reason) "")))
                         'face 'grading-panel-applied-face))))
            ;; Note
            (let ((note (grading-question-grade-note qg)))
              (when (and note (not (string-empty-p note)))
                (insert (propertize
                         (format "  Note: \"%s\"\n" note)
                         'face 'grading-panel-note-face))))))
        ;; Total
        (insert (propertize
                 (format "\n%40s %d/%d\n"
                         "Total:"
                         grand-total
                         (grading-rubric-total rubric))
                 'face 'grading-panel-score-face)))
      ;; Help line
      (insert (propertize
               "\n[a-z]toggle [1-9]question [TAB]next-q [SPC/DEL]scroll\nC-c C-n/C-p next/prev  C-c n note  C-c c custom\nC-c j jump  C-c o overview  C-c f finalize\nC-c C-s save  C-c q quit\n"
               'face 'grading-panel-help-face))
      (goto-char (point-min)))))

;; PDF display

(defun grading-panel--show-pdf (student)
  "Display the PDF for STUDENT in the PDF window."
  (let ((file (grading-student-submission-file student)))
    (when (and file (file-exists-p file))
      (when (and grading-panel--pdf-window
                 (window-live-p grading-panel--pdf-window))
        (with-selected-window grading-panel--pdf-window
          (find-file file))))))

;; Commands

(defun grading-panel-next-student ()
  "Move to the next student."
  (interactive)
  (let* ((session grading-session--current)
         (max-idx (1- (length (grading-session-students session))))
         (new-idx (min (1+ (grading-session-current-student-index session)) max-idx)))
    (setf (grading-session-current-student-index session) new-idx)
    (setf (grading-session-current-question session)
          (grading-question-number
           (car (grading-rubric-questions (grading-session-rubric session)))))
    (grading-session-save session)
    (grading-panel--show-pdf (grading-session-current-student session))
    (grading-panel--render)))

(defun grading-panel-prev-student ()
  "Move to the previous student."
  (interactive)
  (let* ((session grading-session--current)
         (new-idx (max 0 (1- (grading-session-current-student-index session)))))
    (setf (grading-session-current-student-index session) new-idx)
    (setf (grading-session-current-question session)
          (grading-question-number
           (car (grading-rubric-questions (grading-session-rubric session)))))
    (grading-session-save session)
    (grading-panel--show-pdf (grading-session-current-student session))
    (grading-panel--render)))

(defun grading-panel-jump-to-question (n)
  "Jump to question number N."
  (interactive "nQuestion number: ")
  (let* ((session grading-session--current)
         (rubric (grading-session-rubric session)))
    (if (grading-rubric-question-by-number rubric n)
        (progn
          (setf (grading-session-current-question session) n)
          (grading-panel--render))
      (message "No question %d" n))))

(defun grading-panel-next-question ()
  "Cycle to the next question."
  (interactive)
  (let* ((session grading-session--current)
         (rubric (grading-session-rubric session))
         (questions (grading-rubric-questions rubric))
         (current (grading-session-current-question session))
         (numbers (mapcar #'grading-question-number questions))
         (pos (cl-position current numbers))
         (next-pos (if pos (mod (1+ pos) (length numbers)) 0)))
    (setf (grading-session-current-question session) (nth next-pos numbers))
    (grading-panel--render)))

(defun grading-panel-toggle-item (letter)
  "Toggle the rubric item with LETTER for the current student/question."
  (interactive (list last-command-event))
  (let* ((session grading-session--current)
         (student (grading-session-current-student session))
         (username (grading-student-username student))
         (qnum (grading-session-current-question session))
         (rubric (grading-session-rubric session))
         (question (grading-rubric-question-by-number rubric qnum))
         (item (cl-find letter (grading-question-items question)
                        :key #'grading-rubric-item-letter)))
    (if item
        (progn
          (grading-session-toggle-item session username qnum
                                       (grading-rubric-item-id item))
          (grading-panel--render))
      (message "No item '%c' for current question" letter))))

(defun grading-panel-add-note ()
  "Add or edit a note for the current question."
  (interactive)
  (let* ((session grading-session--current)
         (student (grading-session-current-student session))
         (username (grading-student-username student))
         (qnum (grading-session-current-question session))
         (qg (grading-session-get-question-grade session username qnum))
         (current-note (or (grading-question-grade-note qg) ""))
         (note (read-string (format "Note for Q%d: " qnum) current-note)))
    (grading-session-set-note session username qnum
                              (if (string-empty-p note) nil note))
    (grading-panel--render)))

(defun grading-panel-custom-adjustment ()
  "Set a custom point adjustment for the current question."
  (interactive)
  (let* ((session grading-session--current)
         (student (grading-session-current-student session))
         (username (grading-student-username student))
         (qnum (grading-session-current-question session))
         (points (read-number (format "Custom adjustment for Q%d (e.g. -2 or +3): " qnum)))
         (reason (read-string "Reason (optional): ")))
    (grading-session-set-custom-adjustment
     session username qnum points
     (if (string-empty-p reason) nil reason))
    (grading-panel--render)))

(defun grading-panel-jump-to-student ()
  "Jump to a student by name."
  (interactive)
  (let* ((session grading-session--current)
         (students (grading-session-students session))
         (names (mapcar (lambda (s)
                          (format "%s %s (%s)"
                                  (grading-student-first-name s)
                                  (grading-student-last-name s)
                                  (grading-student-username s)))
                        students))
         (choice (completing-read "Jump to student: " names nil t))
         (idx (cl-position choice names :test #'string=)))
    (when idx
      (setf (grading-session-current-student-index session) idx)
      (setf (grading-session-current-question session)
            (grading-question-number
             (car (grading-rubric-questions (grading-session-rubric session)))))
      (grading-panel--show-pdf (grading-session-current-student session))
      (grading-panel--render))))

(defun grading-panel-save ()
  "Save the current session."
  (interactive)
  (grading-session-save grading-session--current)
  (message "Session saved."))

(defun grading-panel-scroll-pdf-down ()
  "Scroll the PDF window down."
  (interactive)
  (when (and grading-panel--pdf-window
             (window-live-p grading-panel--pdf-window))
    (with-selected-window grading-panel--pdf-window
      (if (fboundp 'pdf-view-scroll-up-or-next-page)
          (pdf-view-scroll-up-or-next-page)
        (scroll-up-command)))))

(defun grading-panel-scroll-pdf-up ()
  "Scroll the PDF window up."
  (interactive)
  (when (and grading-panel--pdf-window
             (window-live-p grading-panel--pdf-window))
    (with-selected-window grading-panel--pdf-window
      (if (fboundp 'pdf-view-scroll-down-or-previous-page)
          (pdf-view-scroll-down-or-previous-page)
        (scroll-down-command)))))

(defun grading-panel-quit ()
  "Save and quit the grading session."
  (interactive)
  (grading-session-save grading-session--current)
  (when (and grading-panel--panel-buffer (buffer-live-p grading-panel--panel-buffer))
    (kill-buffer grading-panel--panel-buffer))
  (setq grading-session--current nil)
  (delete-other-windows)
  (message "Grading session saved and closed."))

;; Keymap

(defvar grading-panel-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Letter keys for toggling rubric items — all of a-z available
    (dolist (c (number-sequence ?a ?z))
      (define-key map (string c)
                  (lambda () (interactive) (grading-panel-toggle-item c))))
    ;; Question number keys
    (dolist (n '(1 2 3 4 5 6 7 8 9))
      (define-key map (kbd (number-to-string n))
                  (lambda () (interactive) (grading-panel-jump-to-question n))))
    ;; Navigation and commands under C-c prefix
    (define-key map (kbd "C-c C-n") #'grading-panel-next-student)
    (define-key map (kbd "C-c C-p") #'grading-panel-prev-student)
    (define-key map (kbd "C-c C-f") #'grading-panel-next-question)
    (define-key map (kbd "TAB")     #'grading-panel-next-question)
    (define-key map (kbd "C-c n")   #'grading-panel-add-note)
    (define-key map (kbd "C-c c")   #'grading-panel-custom-adjustment)
    (define-key map (kbd "C-c j")   #'grading-panel-jump-to-student)
    (define-key map (kbd "C-c C-s") #'grading-panel-save)
    (define-key map (kbd "C-c o")   #'grading-overview-show)
    (define-key map (kbd "C-c f")   #'grading-finalize)
    (define-key map (kbd "C-c q")   #'grading-panel-quit)
    ;; PDF scrolling — no conflict
    (define-key map (kbd "SPC") #'grading-panel-scroll-pdf-down)
    (define-key map (kbd "DEL") #'grading-panel-scroll-pdf-up)
    map)
  "Keymap for `grading-panel-mode'.")

(define-derived-mode grading-panel-mode special-mode "Grading"
  "Major mode for the grading panel."
  (setq buffer-read-only t)
  (setq truncate-lines t))

;; Setup

(defun grading-panel-setup (session)
  "Set up the grading panel windows for SESSION."
  (setq grading-session--current session)
  (delete-other-windows)
  (let* ((student (grading-session-current-student session))
         (pdf-file (grading-student-submission-file student)))
    ;; Open PDF in left window
    (when (and pdf-file (file-exists-p pdf-file))
      (find-file pdf-file))
    (setq grading-panel--pdf-window (selected-window))
    ;; Create panel in right window
    (setq grading-panel--panel-buffer
          (get-buffer-create "*Grading Panel*"))
    (setq grading-panel--panel-window
          (split-window-right (floor (* 0.65 (frame-width)))))
    (set-window-buffer grading-panel--panel-window grading-panel--panel-buffer)
    (select-window grading-panel--panel-window)
    (with-current-buffer grading-panel--panel-buffer
      (grading-panel-mode))
    (grading-panel--render)))

(provide 'grading-panel)
;;; grading-panel.el ends here
