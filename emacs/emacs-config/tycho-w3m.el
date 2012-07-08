(provide 'tycho-w3m)
(require 'w3m-load)
;(require 'w3m-search)
(require 'w3m-cookie)
(require 'w3m-session)

;; Make the previous search engine the default for the next
;; search.

(defadvice w3m-search (after change-default activate)
  (let ((engine (nth 1 minibuffer-history)))
    (when (assoc engine w3m-search-engine-alist)
      (setq w3m-search-default-engine engine))))

(defun tychoish-w3m-google (what)
  "Use google to search for WHAT."
  (interactive "sSearch: ")
  (save-window-excursion
    (delete-other-windows)
    (let ((dir default-directory))
      (w3m-browse-url (concat "http://www.google.com/search?q="
                              (w3m-url-encode-string what)))
      (cd dir)
      (recursive-edit))))

(defun w3m-browse-current-buffer ()
  "Look at the current buffer as rendered by w3m."
  (interactive)
  (let ((filename (concat (make-temp-file "w3m-") ".html")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) filename)
          (w3m-find-file filename))
      (delete-file filename))))

(setq browse-url-browser-function 'w3m-browse-url-other-window)

(defun w3m-browse-url-other-window (url &optional newwin)
  (let ((w3m-pop-up-windows t))
    (if (one-window-p) (split-window))
    (other-window 1)
    (w3m-browse-url url newwin)))

(add-hook 'w3m-display-hook
          (lambda (url)
            (let ((buffer-read-only nil))
              (delete-trailing-whitespace))))

(setq browse-url-browser-function 'w3m-browse-url
      browse-url-new-window-flag t)
; (setq w3m-search-default-engine "google")
; (add-to-list 'w3m-search-engine-alist '("google" "http://www.google.com/search?hl=en&q=%s"))
; (add-to-list 'w3m-search-engine-alist '("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s"))
; (add-to-list 'w3m-search-engine-alist '("wikipedia-en" "http://en.wikipedia.org/wiki/Special:Search?search=%s"))

(setq apropos-url-alist
      '(("^gw?:? +\\(.*\\)" . ;; Google Web
         "http://www.google.com/search?q=\\1")
        ("^g!:? +\\(.*\\)" . ;; Google Lucky
         "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=\\1")
        ("^gl:? +\\(.*\\)" .  ;; Google Linux
         "http://www.google.com/linux?q=\\1")
        ("^gi:? +\\(.*\\)" . ;; Google Images
         "http://images.google.com/images?sa=N&tab=wi&q=\\1")
        ("^gg:? +\\(.*\\)" . ;; Google Groups
         "http://groups.google.com/groups?q=\\1")
        ("^gd:? +\\(.*\\)" . ;; Google Directory
         "http://www.google.com/search?&sa=N&cat=gwd/Top&tab=gd&q=\\1")
        ("^gn:? +\\(.*\\)" . ;; Google News
         "http://news.google.com/news?sa=N&tab=dn&q=\\1")
        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(\\w+://.*\\)" . ;; Google Translate URL
         "http://translate.google.com/translate?langpair=\\1|\\2&u=\\3")
        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ;; Google Translate Text
         "http://translate.google.com/translate_t?langpair=\\1|\\2&text=\\3")
        ("^/\\.$" . ;; Slashdot
         "http://www.slashdot.org")
        ("^/\\.:? +\\(.*\\)" . ;; Slashdot search
         "http://www.osdn.com/osdnsearch.pl?site=Slashdot&query=\\1")
        ("^fm$" . ;; Freshmeat
         "http://www.freshmeat.net")
        ("^ewiki:? +\\(.*\\)" . ;; Emacs Wiki Search
         "http://www.emacswiki.org/cgi-bin/wiki?search=\\1")
        ("^ewiki$" . ;; Emacs Wiki
         "http://www.emacswiki.org")
        ))


(defvar w3m-isearch-links-do-wrap nil
    "Used internally for fast search wrapping.")

  (defun w3m-isearch-links (&optional regexp)
    (interactive "P")
    (let ((isearch-wrap-function
           #'(lambda ()
               (setq w3m-isearch-links-do-wrap nil)
               (if isearch-forward
                   (goto-char (window-start))
                 (goto-char (window-end)))))
          (isearch-search-fun-function
           #'(lambda () 'w3m-isearch-links-search-fun))
          post-command-hook		;inhibit link echoing
          do-follow-link
          (isearch-mode-end-hook
           (list  #'(lambda nil
                      (when (and (not isearch-mode-end-hook-quit)
                                 (w3m-anchor))
                        (setq do-follow-link t))))))
      (setq w3m-isearch-links-do-wrap t)
      (isearch-mode t
                    regexp
                    ;; fast wrap
                    #'(lambda nil
                        (if isearch-success
                            (setq w3m-isearch-links-do-wrap t)
                          (when w3m-isearch-links-do-wrap
                            (setq w3m-isearch-links-do-wrap nil)
                            (setq isearch-forward
                                  (not isearch-forward))
                            (isearch-repeat isearch-forward))))
                    t)
      (when do-follow-link
        (w3m-view-this-url))))

  (defun w3m-isearch-links-search-fun (string &optional bound no-error)
    (let* (isearch-search-fun-function
           (search-fun  (isearch-search-fun))
           error
           (bound  (if isearch-forward
                       (max (or bound 0)
                            (window-end))
                     (min (or bound (window-start))
                          (window-start)))))
      (condition-case err
          (while (and (apply search-fun (list string bound))
                      (not (w3m-anchor (point)))))
        (error (setq error err)))
      (if error
          (if (not no-error)
              (signal (car error) (cadr error)))
        (point))))

(define-key w3m-mode-map [?v] 'w3m-isearch-links)

(add-hook 'w3m-display-hook
              (lambda (url)
                (let ((buffer-read-only nil))
                  (delete-trailing-whitespace))))

;(w3m-lnum-mode 1)
(add-hook 'w3m-mode-hook 'w3m-lnum-mode)

(global-set-key (kbd "C-c w S") 'w3m-search)
(global-set-key (kbd "C-c w O") 'w3m-goto-url)
(global-set-key (kbd "C-c w s") 'w3m-search-new-session)
(global-set-key (kbd "C-c w o") 'w3m-goto-url-new-session)

(global-set-key (kbd "C-c w n") 'w3m-next-buffer)
(global-set-key (kbd "C-c w p") 'w3m-previous-buffer)

(global-set-key (kbd "C-c w r s") 'w3m-session-save)
(global-set-key (kbd "C-c w r l") 'w3m-session-select)

(global-set-key (kbd "C-c w g") 'tychoish-w3m-google)
(global-set-key (kbd "C-c w e") 'browse-url-firefox)
(global-set-key (kbd "C-c w f") 'browse-url-at-point)
(global-set-key (kbd "C-c w y") 'w3m-print-this-url)
(global-set-key (kbd "C-c w l") 'w3m-print-current-url)
(global-set-key (kbd "C-c w i") 'instapaper-add-at-point)

(define-key w3m-mode-map (kbd "C-c w i") 'instapaper-add-from-w3m)

(add-hook 'w3m-mode-hook
          (lambda ()
            (local-set-key (kbd "C-p") 'previous-line)
            (local-set-key (kbd "C-n") 'next-line)
            (local-set-key (kbd "M-p") 'w3m-previous-anchor)
            (local-set-key (kbd "M-n") 'w3m-next-anchor)
            (local-set-key (kbd "M-<up>") 'w3m-previous-anchor)
            (local-set-key (kbd "M-<down>") 'w3m-next-anchor)
            (local-set-key (kbd "<up>") 'previous-line)
            (local-set-key (kbd "<down>") 'next-line)))

(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)
(setq w3m-language "en_US")
(setq w3m-session-save-always t)
(setq w3m-session-load-always t)
(setq w3m-session-show-titles t)
(setq w3m-session-autosave t)
(setq w3m-session-load-last-sessions t)
(setq w3m-session-duplicate-tabs 'ask) ;  'never, 'always, 'ask
(setq w3m-use-japanese-menu nil)
(setq w3m-use-title-buffer-name t)
(setq browse-url-browser-function 'w3m-browse-url
      browse-url-new-window-flag t)
(setq w3m-toggle-inline-images t)
(setq w3m-toggle-inline-images-permanently t)

(setq browse-url-browser-function 'w3m-browse-url)
(setq browse-url-new-window-flag nil)
; (setq browse-url-browser-function 'browse-url-firefox)

(standard-display-ascii ?\200 [15])
(standard-display-ascii ?\201 [21])
(standard-display-ascii ?\202 [24])
(standard-display-ascii ?\203 [13])
(standard-display-ascii ?\204 [22])
(standard-display-ascii ?\205 [25])
(standard-display-ascii ?\206 [12])
(standard-display-ascii ?\210 [23])
(standard-display-ascii ?\211 [14])
(standard-display-ascii ?\212 [18])
(standard-display-ascii ?\214 [11])
(standard-display-ascii ?\222 [?\'])
(standard-display-ascii ?\223 [?\"])
(standard-display-ascii ?\224 [?\"])
(standard-display-ascii ?\227 " -- ")

(defalias 'wku 'w3m-print-this-url)
(defalias 'wkl 'w3m-print-current-url)

(defadvice w3m-modeline-title (around my-w3m-modeline-title)
  "prevent original function from running; cleanup remnants"
  (setq w3m-modeline-separator ""
        w3m-modeline-title-string ""))
(ad-activate 'w3m-modeline-title)

(define-key w3m-mode-map (kbd "i") 'instapaper-add-from-w3m)
