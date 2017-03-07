
(message "hello world")

```{r test}

a <- 10
b <- 20

```


(defun select-r-chunk ()
  (interactive)
  (search-backward-regexp "```{[r].*}")
  (next-line)
  (set-mark-command nil)
  (search-forward-regexp "```")
  (previous-line)
  (setq deactivate-mark nil))


(defun rmd-point-in-chunk ()
  "Check if the current point is in an R code chunk"
  (interactive)
  (save-excursion
    (let ((cur (point)))
      (and
       (< (search-backward-regexp "```{[r].*}") cur)
       (> (search-forward-regexp "```\n") cur)))))


(global-set-key (kbd "C-c C-d") (lambda () (interactive) (print (rmd-point-in-chunk))))


```{r test2}


```
