;; tycho-ikiwiki.el
;;
;; Emacs support for ikiwiki blogs.
;;
;; To use, just put this file somewhere in the load path and
;; (require 'tycho-ikiwiki)
;;
;; TODO improve documentation here.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global Settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tycho-ikiwiki)

;;
;; Variables
;;

(defvar ikiwiki-post-ext ".mdwn"
  "File extension of Jekyll (tychoish) posts.")

(defvar ikiwiki-directory "~/sites/wikish/"
  "Path to ikiwiki blog.")
(defvar ikiwiki-drafts-dir "~/wiki/rhizome/"
  "Path to ikiwiki blog.")
(defvar ikiwiki-tychoish-rhizome-dir "~/wikish/rhizome/"
  "Path to ikiwiki blog.")
(defvar ikiwiki-tychoish-knitting-dir "~/wikish/knitting/"
  "Path to ikiwiki blog.")
(defvar ikiwiki-tychoish-dir "~/wikish/"
  "Path to ikiwiki blog.")

(defvar ikiwiki-blog-template
  "[[!meta title=\"%s\"]]\n[[!meta author=\"tychoish\"]]\n[[!meta date=\"\"]]\n[[!tag ]]\n"
  "Default template for ikiwiki-blog. %s will be replace by the post title.")

;;
;; Helper Functions
;;

(defun ikiwiki-file-make-slug (s)
  "Turn a string into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun ikiwiki-file-escape (s)
  "Escape a string for ikiwiki title."
  (if (or (string-match ":" s)
          (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Working Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ikiwiki-create-blog-post (title)
  "Create a new rhizome post."
  (interactive "sPost Title: ")
  (let ((draft-file (concat ikiwiki-drafts-dir
                            (ikiwiki-file-make-slug title)
                            ikiwiki-post-ext)))
    (if (file-exists-p draft-file)
        (find-file draft-file)
      (find-file draft-file)
      (insert (format ikiwiki-blog-template (ikiwiki-file-escape title))))))

(defun ikiwiki-tychoish-rhizome-publish-post ()
  "Move a draft post to the rhizome directory,"
  (interactive)
  (cond
   ((not (equal
          (file-name-directory (buffer-file-name (current-buffer)))
          (eval ikiwiki-drafts-dir)))
    (message "This is not a draft post.")
    (insert (file-name-directory (buffer-file-name (current-buffer))) "\n"
            (eval ikiwiki-drafts-dir)))
   ((buffer-modified-p)
    (message "Can't publish post; buffer has modifications."))
   (t
    (let ((filename
           (concat ikiwiki-tychoish-rhizome-dir
                   (file-name-nondirectory
                    (buffer-file-name (current-buffer)))))
          (old-point (point)))
      (rename-file (buffer-file-name (current-buffer))
                   filename)
      (kill-buffer nil)
      (find-file filename)
      (set-window-point (selected-window) old-point)
      (pwd)))))

(defun ikiwiki-tychoish-knitting-publish-post ()
  "Move a draft post to the rhizome directory,"
  (interactive)
  (cond
   ((not (equal
          (file-name-directory (buffer-file-name (current-buffer)))
          (eval ikiwiki-drafts-dir)))
    (message "This is not a draft post.")
    (insert (file-name-directory (buffer-file-name (current-buffer))) "\n"
            (eval ikiwiki-drafts-dir)))
   ((buffer-modified-p)
    (message "Can't publish post; buffer has modifications."))
   (t
    (let ((filename
           (concat ikiwiki-tychoish-knitting-dir
                   (file-name-nondirectory
                    (buffer-file-name (current-buffer)))))
          (old-point (point)))
      (rename-file (buffer-file-name (current-buffer))
                   filename)
      (kill-buffer nil)
      (find-file filename)
      (set-window-point (selected-window) old-point)
      (pwd)))))


(defun ikiwiki-tychoish-publish-page ()
  "Move a draft post to the rhizome directory,"
  (interactive)
  (let ((filename
          (concat ikiwiki-tychoish-dir
                  (file-name-nondirectory
                   (buffer-file-name (current-buffer)))))
         (old-point (point)))
     (rename-file (buffer-file-name (current-buffer))
                  filename)
     (kill-buffer nil)
     (find-file filename)
     (set-window-point (selected-window) old-point)
     (pwd)))

 (defun ikiwiki-meta-date () "Insert Ikiwiki Meta Date String" (interactive)
   (shell-command "~/scripts/wiki-date" 1 nil))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;
 ;; Key Bindings
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'revbufs)

(global-set-key (kbd "C-c t n") 'ikiwiki-create-blog-post)
(global-set-key (kbd "C-c t m") 'ikiwiki-meta-date)
(global-set-key (kbd "C-c t p") 'ikiwiki-tychoish-rhizome-publish-post)
(global-set-key (kbd "C-c t k") 'ikiwiki-tychoish-knitting-publish-post)
(global-set-key (kbd "C-c t w") 'ikiwiki-tychish-publish-page)
(global-set-key (kbd "C-c t a") (lambda ()
                                  (interactive)
                                  (find-file "~/wiki/rhizome/")))
(global-set-key (kbd "C-c t r") (lambda ()
                                  (interactive)
                                  (find-file "~/sites/wikish/rhizome/")))

