(provide 'tycho-writing)
(require 'markdown-mode)
(require 'screenplay)
(require 'rst)

(defun tychoish-rst-faces()
  (interactive)
  (setq rst-level-face-max 0)
  (set-face-background 'rst-level-1-face nil)
  (set-face-background 'rst-level-2-face nil)
  (set-face-background 'rst-level-3-face nil)
  (set-face-background 'rst-level-4-face nil)
  (set-face-background 'rst-level-5-face nil)
  (set-face-background 'rst-level-6-face nil))

(setq auto-mode-alist (cons '("\\.mdwn" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.script" . screenplay-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tex" . latex-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("\\.txt" . rst-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rst" . rst-mode)auto-mode-alist))

(add-hook 'rst-mode-hook
     (lambda ()
      (define-key rst-mode-map "\"" 'tychoish-electric-pair)
      (define-key rst-mode-map "\'" 'tychoish-electric-pair)
      (define-key rst-mode-map "\*" 'tychoish-electric-pair)
      (define-key rst-mode-map "\_" 'tychoish-electric-pair)
      (define-key rst-mode-map "(" 'tychoish-electric-pair)
      (define-key rst-mode-map "[" 'tychoish-electric-pair)
      (define-key rst-mode-map "{" 'tychoish-electric-pair)
))

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'orgstruct-mode)
(add-hook 'rst-mode-hook 'flyspell-mode)
(add-hook 'rst-mode-hook 'turn-on-auto-fill)
(add-hook 'rst-mode-hook 'orgstruct-mode)
(add-hook 'rst-mode-hook 'tychoish-rst-faces)
(add-hook 'screenplay-mode-hook 'flyspell-mode)
(add-hook 'screenplay-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'rst-adjust-hook 'rst-toc-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; artbollocks
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'artbollocks-mode)
(add-hook 'text-mode-hook 'turn-on-artbollocks-mode)
(add-hook 'rst-mode-hook 'turn-on-artbollocks-mode)
(add-hook 'markdown-mode-hook 'turn-on-artbollocks-mode)
(add-hook 'message-mode-hook 'turn-on-artbollocks-mode)
(add-hook 'mail-mode-hook 'turn-on-artbollocks-mode)
(defalias 'ab 'artbollocks-mode)
(setq weasel-words-regex
      (concat "\\b" (regexp-opt
                     '("one of the"
                       "very"
                       "sort of"
                       "a lot"
                       "probably"
                       "maybe"
                       "perhaps"
                       "I think"
                       "really"
                       "nice"
                       "utilize"
                       "leverage") t) "\\b"))
;; Fix a bug in the regular expression to catch repeated words
; (setq lexical-illusions-regex "\\b\\(\\w+\\)\\W+\\(\\1\\)\\b")
;; Don't show the art critic words, or at least until I figure
;; out my own jargon
(setq artbollocks t)
(setq lexical-illusions nil)
(setq weasl-words t)
(setq passive-voice t)
;; Make sure keywords are case-insensitive
(defadvice search-for-keyword (around sacha activate)
  "Match in a case-insensitive way."
  (let ((case-fold-search t))
    ad-do-it))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Word Count and other old functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wc (&optional start end)
  "Prints number of lines, words and characters in region or whole buffer."
  (interactive)
  (let ((n 0)
        (start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (message "Lines: %3d; Words: %3d; Characters: %3d" (count-lines start end) n (- end start))))
(defalias 'word-count 'wc)

;; sentence hilighting

;; (setq sentence-end "[^.].[.?!]+\\([]\"')}]*\\|<[^>]+>\\)\\($\\| $\\|\t\\| \\)[ \t\n]*")

;; ; (setq sentence-color "#adadad") ; foreground  color  for the current sentence; adjust as needed
;; (setq sentence-face (make-face 'sentence-face-background))
;; (set-face-foreground sentence-face sentence-color)

;; (defun sentence-begin-pos ()
;;   (save-excursion (unless (= (point) (point-max)) (forward-char)) (backward-sentence) (point)))
;; (defun sentence-end-pos ()
;;   (save-excursion (unless (= (point) (point-max)) (forward-char)) (backward-sentence) (forward-sentence) (point)))

;; (setq sentence-highlight-mode nil)
;; (setq sentence-end-double-space nil)
;; (defun sentence-highlight-current (&rest ignore)
;;   "Highlight current sentence."
;;     (and sentence-highlight-mode (> (buffer-size) 0)
;;     (progn
;;       (and  (boundp 'sentence-extent)
;;         sentence-extent
;;         (move-overlay sentence-extent (sentence-begin-pos) (sentence-end-pos) (current-buffer))))))

;; (defun tychoish-sentence-highlight ()
;;   (lambda ()
;;     (make-local-variable 'sentence-highlight-mode)
;;     (setq sentence-highlight-mode t)
;;     (add-hook 'post-command-hook   'sentence-highlight-current)))
