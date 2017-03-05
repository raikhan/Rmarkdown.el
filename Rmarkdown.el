
(message "hello world")

```{r test}

a <- 10
b <- 20

```


(defun select-r-cell ()
  (interactive)
  (search-backward-regexp "```{[r].*}") 
  (next-line)
  (set-mark-command nil)
  (search-forward-regexp "```")
  (previous-line)
  (setq deactivate-mark nil)) 

(global-set-key (kbd "C-c C-d") 'select-r-cell)


```{r test2}


```
