;;; helm-bookmark.el --- Helm for Emacs regular Bookmarks.

;; Copyright (C) 2012 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(eval-when-compile (require 'cl))
(eval-when-compile (require 'bookmark))
(require 'helm)
(require 'helm-utils)


(defgroup helm-bookmark nil
  "Predefined configurations for `helm.el'."
  :group 'helm)

(defface helm-bookmarks-su-face '((t (:foreground "red")))
  "Face for su/sudo bookmarks."
  :group 'helm-bookmark)



(defvar helm-c-bookmarks-face1 'helm-ff-directory)
(defvar helm-c-bookmarks-face2 'helm-ff-file)
(defvar helm-c-bookmarks-face3 'helm-bookmarks-su-face)

(defvar helm-c-bookmark-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-c-bookmark-run-jump-other-window)
    (define-key map (kbd "C-d")   'helm-c-bookmark-run-delete)
    (when (locate-library "bookmark-extensions")
      (define-key map (kbd "M-e") 'helm-c-bmkext-run-edit))
    (define-key map (kbd "C-c ?") 'helm-c-bookmark-help)
    (delq nil map))
  "Generic Keymap for emacs bookmark sources.")

(defvar helm-c-source-bookmarks
  `((name . "Bookmarks")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . bookmark-all-names)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

;;; bookmark-set
(defvar helm-c-source-bookmark-set
  '((name . "Set Bookmark")
    (dummy)
    (action . bookmark-set))
  "See (info \"(emacs)Bookmarks\").")

;;; Special bookmarks
(defvar helm-c-source-bookmarks-ssh
  '((name . "Bookmarks-ssh")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (helm-c-collect-bookmarks :ssh t)))
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defvar helm-c-source-bookmarks-su
  '((name . "Bookmarks-root")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (helm-c-collect-bookmarks :su t)))
    (filtered-candidate-transformer helm-c-highlight-bookmark-su)

    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defvar helm-c-source-bookmarks-local
  '((name . "Bookmarks-Local")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (helm-c-collect-bookmarks :local t)))
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defun* helm-c-collect-bookmarks (&key local su sudo ssh)
  (let* ((lis-all (bookmark-all-names))
         (lis-loc (cond (local (loop for i in lis-all
                                     unless (string-match "^(ssh)\\|^(su)" i)
                                     collect i))
                        (su (loop for i in lis-all
                                  when (string-match "^(su)" i)
                                  collect i))
                        (sudo (loop for i in lis-all
                                    when (string-match "^(sudo)" i)
                                    collect i))
                        (ssh (loop for i in lis-all
                                   when (string-match "^(ssh)" i)
                                   collect i)))))
    (sort lis-loc 'string-lessp)))

(defun helm-c-bookmark-root-logged-p ()
  (catch 'break
    (dolist (i (mapcar #'buffer-name (buffer-list)))
      (when (string-match (format "*tramp/%s ." helm-su-or-sudo) i)
        (throw 'break t)))))

(defun helm-c-highlight-bookmark-su (files source)
  (if (helm-c-bookmark-root-logged-p)
      (helm-c-highlight-bookmark files source)
      (helm-c-highlight-not-logged files source)))

(defun helm-c-highlight-not-logged (files source)
  (loop for i in files
        collect (propertize i 'face helm-c-bookmarks-face3)))

(defun helm-c-highlight-bookmark (bookmarks source)
  "Used as `candidate-transformer' to colorize bookmarks.
Work both with standard Emacs bookmarks and bookmark-extensions.el."
  (loop for i in bookmarks
        for isfile        = (bookmark-get-filename i)
        for bufp          = (and (fboundp 'bmkext-get-buffer-name)
                                 (bmkext-get-buffer-name i))
        for handlerp      = (and (fboundp 'bookmark-get-handler)
                                 (bookmark-get-handler i))
        for isw3m         = (and (fboundp 'bmkext-w3m-bookmark-p)
                                 (bmkext-w3m-bookmark-p i))
        for isgnus        = (and (fboundp 'bmkext-gnus-bookmark-p)
                                 (bmkext-gnus-bookmark-p i))
        for isman         = (and (fboundp 'bmkext-man-bookmark-p) ; Man
                                 (bmkext-man-bookmark-p i))
        for iswoman       = (and (fboundp 'bmkext-woman-bookmark-p) ; Woman
                                 (bmkext-woman-bookmark-p i))
        for handlerp      = (bookmark-get-handler i)
        for isannotation  = (bookmark-get-annotation i)
        for isabook       = (string= (bookmark-prop-get i 'type) "addressbook")
        for isinfo        = (eq handlerp 'Info-bookmark-jump)
        ;; Add a * if bookmark have annotation
        if (and isannotation (not (string-equal isannotation "")))
        do (setq i (concat "*" i))
        collect (cond (;; info buffers
                       isinfo
                       (propertize i 'face 'helm-bmkext-info 'help-echo isfile))
                      (;; w3m buffers
                       isw3m
                       (propertize i 'face 'helm-bmkext-w3m 'help-echo isfile))
                      (;; gnus buffers
                       isgnus
                       (propertize i 'face 'helm-bmkext-gnus 'help-echo isfile))
                      (;; Man Woman
                       (or iswoman isman)
                       (propertize i 'face 'helm-bmkext-man 'help-echo isfile))
                      (;; Addressbook
                       isabook
                       (propertize i 'face '((:foreground "Tomato"))))
                      (;; directories
                       (and isfile (file-directory-p isfile))
                       (propertize i 'face helm-c-bookmarks-face1 'help-echo isfile))
                      (;; regular files
                       t
                       (propertize i 'face 'helm-bmkext-file 'help-echo isfile)))))

(defun helm-c-bookmark-jump (candidate)
  "Jump to bookmark from keyboard."
  (let ((current-prefix-arg helm-current-prefix-arg))
    (bookmark-jump candidate)))

;;;###autoload
(defun helm-c-bookmark-run-jump-other-window ()
  "Jump to bookmark from keyboard."
  (interactive)
  (helm-c-quit-and-execute-action 'bookmark-jump-other-window))

;;;###autoload
(defun helm-c-bookmark-run-delete ()
  "Delete bookmark from keyboard."
  (interactive)
  (when (y-or-n-p "Delete bookmark?")
    (helm-c-quit-and-execute-action 'helm-delete-marked-bookmarks)))

(defun helm-bookmark-get-bookmark-from-name (bmk)
  "Return bookmark name even if it is a bookmark with annotation.
e.g prepended with *.
Return nil if bmk is not a valid bookmark."
  (let ((bookmark (replace-regexp-in-string "\*" "" bmk)))
    (if (assoc bookmark bookmark-alist)
        bookmark
        (when (assoc bmk bookmark-alist)
          bmk))))

(defun helm-delete-marked-bookmarks (ignore)
  "Delete this bookmark or all marked bookmarks."
  (dolist (i (helm-marked-candidates))
    (bookmark-delete (helm-bookmark-get-bookmark-from-name i)
                     'batch)))


;;;###autoload
(defun helm-bookmarks ()
  "Preconfigured `helm' for bookmarks."
  (interactive)
  (helm-other-buffer 'helm-c-source-bookmarks "*helm bookmarks*"))

;;;###autoload
(defun helm-c-pp-bookmarks ()
  "Preconfigured `helm' for bookmarks (pretty-printed)."
  (interactive)
  (helm-other-buffer '(helm-c-source-bookmarks-local
                       helm-c-source-bookmarks-su
                       helm-c-source-bookmarks-ssh)
                     "*helm pp bookmarks*"))


(provide 'helm-bookmark)

;;; helm-bookmark.el ends here
