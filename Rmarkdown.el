(defun rmd-select-r-chunk ()
  "Select all text in an R chunk"
  (interactive)
  (progn
    (search-backward-regexp "```{[r].*}")
    (next-line)
    (set-mark-command nil)
    (search-forward-regexp "```")
    (beginning-of-line)
    (setq deactivate-mark nil)))


(defun rmd-point-in-chunk ()
  "Check if the current point is in an R code chunk"
  (interactive)
  (save-excursion
    (let ((cur (point)))
      (and
       (< (search-backward-regexp "```{[r].*}") cur)
       (> (search-forward-regexp "```\n") cur)))))


(global-set-key (kbd "C-c d") (lambda () (interactive) (print (rmd-point-in-chunk))))
(global-set-key (kbd "C-c v") (lambda () (interactive) (print (rmd-select-r-chunk))))

