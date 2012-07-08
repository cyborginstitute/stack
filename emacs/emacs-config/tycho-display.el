(provide 'tycho-display)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Font and Appearance Related Configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tychoish-font-tiny ()
  (interactive)
  (setq default-frame-alist '((font-backend . "xft")
                              (font . "Inconsolata-11")
                              (vertical-scroll-bars . 0)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (alpha 86 84)))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defun tychoish-font-smaller ()
  (interactive)
  (setq default-frame-alist '((font-backend . "xft")
                              (font . "Inconsolata-12")
                              (vertical-scroll-bars . 0)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (alpha 86 84)))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defun tychoish-font-small ()
  (interactive)
  (setq default-frame-alist '((font-backend . "xft")
                              (font . "Inconsolata-g-13")
                              (vertical-scroll-bars . 0)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (alpha 86 84)))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))


(defun tychoish-display ()
  (interactive)
  (setq default-frame-alist '((font-backend . "xft")
                              (vertical-scroll-bars . 0)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (alpha 97 97)))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))


(defun tychoish-font-medium ()
  (interactive)
  (setq default-frame-alist '((font-backend . "xft")
                              (font . "Inconsolata-14")
                              (vertical-scroll-bars . 0)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (alpha 86 84)))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defun tychoish-font-big ()
  (interactive)
  (setq default-frame-alist '((font-backend . "xft")
                              (font . "Inconsolata-15")
                              (vertical-scroll-bars . 0)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (alpha 86 84)))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defun tychoish-font-bigger ()
  (interactive)
  (setq default-frame-alist '((font-backend . "xft")
                              (font . "Inconsolata-16")
                              (vertical-scroll-bars . 0)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (alpha 86 84)))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defun tychoish-font-huge ()
  (interactive)
  (setq default-frame-alist '((font-backend . "xft")
                              (font . "Inconsolata-17")
                              (vertical-scroll-bars . 0)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (alpha 86 84)))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defun tychoish-console ()
  (interactive)
  (setq default-frame-alist '((vertical-scroll-bars . 0)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)))
  (setq-default tool-bar-mode nil
                menu-bar-mode nil))

(setq-default inhibit-startup-message 't
              initial-scratch-message 'nil
              save-place t
              scroll-bar-mode nil
              tool-bar-mode nil
              menu-bar-mode nil
              scroll-margin 0
              indent-tabs-mode nil
              flyspell-issue-message-flag 'nil
              size-indication-mode t
              scroll-conservatively 25
              scroll-preserve-screen-position 1
              cursor-in-non-selected-windows nil)

(defalias 'big 'text-scale-increase)
(defalias 'small 'text-scale-decrease)

;; (global-set-key (kbd "C-c f s") 'tychoish-font-small)
;; (global-set-key (kbd "C-c f m") 'tychoish-font-medium)
;; (global-set-key (kbd "C-c f b") 'tychoish-font-big)
;; (global-set-key (kbd "C-c f h") 'tychoish-font-huge)

(global-set-key (kbd "C-c f i") 'text-scale-increase)
(global-set-key (kbd "C-c f d") 'text-scale-decrease)
(global-set-key (kbd "C-c f +") 'text-scale-increase)
(global-set-key (kbd "C-c f -") 'text-scale-decrease)

(setq swapping-buffer nil)
(setq swapping-window nil)

(defun swap-buffers-in-windows ()
  "Swap buffers between two windows"
  (interactive)
  (if (and swapping-window
           swapping-buffer)
      (let ((this-buffer (current-buffer))
            (this-window (selected-window)))
        (if (and (window-live-p swapping-window)
                 (buffer-live-p swapping-buffer))
            (progn (switch-to-buffer swapping-buffer)
                   (select-window swapping-window)
                   (switch-to-buffer this-buffer)
                   (select-window this-window)
                   (message "Swapped buffers."))
          (message "Old buffer/window killed.  Aborting."))
        (setq swapping-buffer nil)
        (setq swapping-window nil))
    (progn
      (setq swapping-buffer (current-buffer))
      (setq swapping-window (selected-window))
      (message "Buffer and window marked for swapping."))))

(global-set-key (kbd "C-c p") 'swap-buffers-in-windows)
