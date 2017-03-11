;; helper functions
(defun rmd-search-backward-r-chunk (count)
  "Helper function to move cursor to beginning of previous R chunk"
  (if  (search-backward-regexp "```{[r].*}" nil t count)
      (next-line)
    (message "No more R chunks")))


;; checking 
(defun rmd-point-in-chunk ()
  "Check if the current point is in an R code chunk"
  (interactive)
  (save-excursion
    (let ((cur (point)))
      (and
       (< (search-backward-regexp "```{[r].*}") cur)
       (> (search-forward-regexp "```\n") cur)))))


;; movement
(defun rmd-goto-next-r-chunk ()
  "Move the pointer to the first line of next R chunk"
  (interactive)
  (if  (search-forward-regexp "```{[r].*}" nil t 1)
      (next-line)
    (message "No more R chunks")))

(defun rmd-goto-previous-r-chunk ()
  "Move the pointer to the first line of previous R chunk"
  (interactive)
  (if (rmd-point-in-chunk)
      (rmd-search-backward-r-chunk 2)
    (rmd-search-backward-r-chunk 1)))


;; selection
(defun rmd-select-r-chunk ()
  "Select all text in an R chunk"
  (interactive)
  (if (rmd-point-in-chunk)
      (progn
        (rmd-search-backward-r-chunk 1)
        (set-mark-command nil)
        (search-forward-regexp "```")
        (beginning-of-line)
        (setq deactivate-mark nil))
    (message "Not in R chunk")))


;; demo keybindings
(global-set-key (kbd "C-c d") (lambda () (interactive) (print (rmd-point-in-chunk))))
(global-set-key (kbd "C-c v") (lambda () (interactive) (print (rmd-select-r-chunk))))

