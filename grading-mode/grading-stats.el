;;; grading-stats.el --- Statistics calculations -*- lexical-binding: t; -*-

;;; Commentary:
;; Statistical functions for grading data: mean, stdev, distributions.

;;; Code:

(require 'cl-lib)
(require 'grading-rubric)
(require 'grading-session)

(defun grading-stats-mean (values)
  "Compute the arithmetic mean of VALUES (list of numbers)."
  (if (null values)
      0.0
    (/ (float (apply #'+ values)) (length values))))

(defun grading-stats-stdev (values)
  "Compute the population standard deviation of VALUES."
  (if (or (null values) (= (length values) 1))
      0.0
    (let* ((mean (grading-stats-mean values))
           (squared-diffs (mapcar (lambda (v) (expt (- v mean) 2)) values)))
      (sqrt (grading-stats-mean squared-diffs)))))

(defun grading-stats--graded-usernames (session)
  "Return list of usernames that have been graded in SESSION."
  (let ((result nil))
    (dolist (student (grading-session-students session))
      (let ((username (grading-student-username student)))
        (when (grading-session-student-graded-p session username)
          (push username result))))
    (nreverse result)))

(defun grading-stats-question-scores (session question-number)
  "Get list of scores for QUESTION-NUMBER across all graded students in SESSION."
  (let ((usernames (grading-stats--graded-usernames session)))
    (mapcar (lambda (u)
              (grading-session-question-score session u question-number))
            usernames)))

(defun grading-stats-total-scores (session)
  "Get list of total scores across all graded students in SESSION."
  (let ((usernames (grading-stats--graded-usernames session)))
    (mapcar (lambda (u) (grading-session-total-score session u)) usernames)))

(defun grading-stats-question-summary (session question-number)
  "Return (mean . stdev) for QUESTION-NUMBER in SESSION."
  (let ((scores (grading-stats-question-scores session question-number)))
    (if scores
        (cons (grading-stats-mean scores) (grading-stats-stdev scores))
      (cons 0.0 0.0))))

(defun grading-stats-overall-summary (session)
  "Return (mean . stdev) for total scores in SESSION."
  (let ((scores (grading-stats-total-scores session)))
    (if scores
        (cons (grading-stats-mean scores) (grading-stats-stdev scores))
      (cons 0.0 0.0))))

(defun grading-stats-distribution (session num-bins)
  "Compute grade distribution for SESSION with NUM-BINS bins.
Returns list of (range-label . count)."
  (let* ((total (grading-rubric-total (grading-session-rubric session)))
         (scores (grading-stats-total-scores session))
         (bin-size (ceiling (/ (float total) num-bins)))
         (bins nil))
    (dotimes (i num-bins)
      (let* ((low (* i bin-size))
             (high (min (1- (* (1+ i) bin-size)) total))
             (label (if (= i (1- num-bins))
                        (format "%d-%d" low total)
                      (format "%d-%d" low high)))
             (count (cl-count-if (lambda (s) (and (>= s low) (<= s high))) scores)))
        (push (cons label count) bins)))
    (nreverse bins)))

(defun grading-stats-graded-count (session)
  "Return the number of graded students in SESSION."
  (length (grading-stats--graded-usernames session)))

(provide 'grading-stats)
;;; grading-stats.el ends here
