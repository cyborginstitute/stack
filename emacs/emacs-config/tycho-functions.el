(provide 'tycho-functions)

(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window  (selected-window))
             (this-buffer  (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start   (window-start this-window))
             (other-start  (window-start other-window)))
        (set-window-buffer this-window  other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start  this-window  other-start)
        (set-window-start  other-window this-start)))))

(defun tychoish-electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun tychoish-todo-compile ()
  (interactive)
  (if (get-buffer "*todo-compile*")
      (progn
        (switch-to-buffer-other-window (get-buffer "*todo-compile*"))
        (recompile))
    (progn
      (compile "make -j -k -C ~/wiki")
      (switch-to-buffer-other-window "*compilation*")
      (rename-buffer "*todo-compile*")))
  (revbufs))
(defalias 'todo-compile 'tychoish-todo-compile)

(defun sudo-find-file (file-name)
  (interactive "Find file (sudo): ")
  (find-file (concat "/sudo::" file-name)))

(defun ssh-reagent ()
  (interactive)
  (let* ((sshdir (car (directory-files "/tmp" nil "ssh-*")))
         (agent (car (directory-files (concat "/tmp/" sshdir) nil "agent.*"))))
    (setenv "SSH_AUTH_SOCK" (concat "/tmp/" sshdir "/" agent)))
  (message "Attached to SSH Session"))
(defalias 'sshra 'ssh-reagent)

(defun tychoish-oa-wiki-twitter ()
  (interactive)
  (while (re-search-forward "\\(.\\) \\[\\@\\(.*\\)\\]$" nil t)
    (replace-match "\\1: [@\\2](http://twitter.com/\\2)" nil nil)))

(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs,
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(defun remove-dupes (list)
  (let (tmp-list head)
    (while list
      (setq head (pop list))
      (unless (equal head (car list))
        (push head tmp-list)))
    (reverse tmp-list)))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
  If there's no region, the current line will be duplicated.
  However, if there's a region, all lines that region covers
  will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defvar th-shell-popup-buffer nil)

(defun th-shell-popup ()
  "Toggle a shell popup buffer with the current file's directory as cwd."
  (interactive)
  (unless (buffer-live-p th-shell-popup-buffer)
    (save-window-excursion (shell "*Popup Shell*"))
    (setq th-shell-popup-buffer (get-buffer "*Popup Shell*")))
  (let ((win (get-buffer-window th-shell-popup-buffer))
        (dir (file-name-directory (or (buffer-file-name)
                                      ;; dired
                                      dired-directory
                                      ;; use HOME
                                      "~/"))))
    (if win
        (delete-window win)
      (pop-to-buffer th-shell-popup-buffer nil t)
      (comint-send-string nil (concat "cd " dir "\n")))))

(defun my-occur (&optional arg)
 "Make sure to always put occur in a vertical split, into a narrower buffer at the side.
I didn't like the default horizontal split, nor the way it messes up the arrangement of windows in the frame or
the way in which the standard way uses a neighbor window."
  (interactive "P")
  (window-configuration-to-register ?y) ; store whatever frame configuratin we are currently in
  (occur (read-from-minibuffer "Regexp: "))
  (if (occur-check-existence)
      (progn
        (delete-other-windows)
        (split-window-horizontally)
        (enlarge-window-horizontally -10)
        (set-cursor-color "black")))
  (occur-procede-accordingly))

(defun occur-procede-accordingly ()
  "Switch to occur buffer or prevent opening of the occur window if no matches occured."
  (interactive "P")
  (if (not(get-buffer "*Occur*"))
      (message "There are no results.")
    (switch-to-buffer "*Occur*")))

(defun occur-check-existence()
  "Signal the existance of an occur buffer depending on the number of matches."
  (interactive)
  (if (not (get-buffer "*Occur*")) nil t))

(defun tychoish-mark-task-done ()
  (interactive)
  (beginning-of-line)
  (re-search-forward "^[A-Z]+ \\(-*.*\\) \\(.*\\)$" nil nil)
    (replace-match "- DONE \\1 \\2"))

(defun occur-mode-quit ()
  "Quit and close occur window. I want to press 'q' and leave things as they were before in regard of the split of windows in the frame.
This is the equivalent of pressing C-x 0 and reset windows in the frame, in whatever way they were,
plus jumping to the latest position of the cursor which might have been changed by using the links out
of any of the matches found in occur."
  (interactive)
  (switch-to-buffer "*Occur*")
  ;; in order to know where we put the cursor thay might have jumped from qoccur
  (other-window 1)                  ;; go to the main window
  (point-to-register ?1)            ;; store the latest cursor position
  (switch-to-buffer "*Occur*")      ;; go back to the occur window
  (kill-buffer "*Occur*")           ;; delete it
  (jump-to-register ?y)             ;; reset the original frame state
  (register-to-point ?1))           ;; re-position cursor

;; some key bindings defined below. Use "p" ans "n" as in dired mode (without Cntrl key) for previous and next line; just show occurrence without leaving the "occur" buffer; use RET to display the line of the given occurrence, instead of jumping to i,t which you do clicking instead;  also quit mode with Ctrl-g.
(define-key occur-mode-map (kbd "q") 'occur-mode-quit)
(define-key occur-mode-map (kbd "C-q") 'occur-mode-quit)
(define-key occur-mode-map (kbd "C-g") 'occur-mode-quit)
(define-key occur-mode-map (kbd "C-RET") 'occur-mode-goto-occurrence-other-window)
(define-key occur-mode-map (kbd "C-<up>") 'occur-mode-goto-occurrence-other-window)
(define-key occur-mode-map (kbd "RET") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "p") 'previous-line)
(define-key occur-mode-map (kbd "n") 'next-line)
