(in-package :stumpwm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; It rubs the modules into the configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-module "tycho-system")
;; (load-module "mpd")
(load-module "tycho-keybindings")
(load-module "tycho-windowing")
;; (load-module "battery-portable")

; (load-module "mem")
; (load-module "notifications")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Apperance and Generic Settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-font "-b&h-lucidatypewriter-medium-r-normal-sans-11-80-100-100-m-70-iso10646-1")
;; (set-font "-artwiz-snap-normal-*-*-*-*-*-*-*-*-*-iso10646-*")
;; (set-font "-*-aqui-medium-r-*-*-11-*-*-*-*-*-*-*")
;; (set-font "-*-wenquanyi bitmap song-medium-r-normal-*-12-*-*-*-*-*-iso10646-*")
;; (set-font "-xos4-*-medium-r-normal-*-13-*-*-*-*-*-iso10646-*")
;; (set-font "-*-profont-medium-r-normal-*-14-*-*-*-*-*-iso8859-*")
;; (set-font "-*-unifont-medium-r-normal-*-14-*-*-*-*-*-iso10646-*")
;; (set-font "-*-comic sans ms-medium-r-normal-*-12-*-*-*-*-*-*-*")
;; (set-font "-adobe-helvetica-medium-r-normal-*-11-*-*-*-*-56-*-*")
;; (set-font "-*-inconsolata-medium-r-normal-*-*-*-*-*-*-*-*-*")
;; (set-font "-*-terminal-*-*-normal-*-14-*-*-*-*-*-*-*")
;; (set-font "-*-courier-medium-r-normal-*-12-*-*-*-*-*-*-*")

(defparameter *normal-border-width* 1)
(set-focus-color "green")
(set-unfocus-color "blue")

(setf *window-border-style* :thin)
(setf *mpd-timeout* nil)
(setf *mouse-focus-policy* :click) ;; :click, :ignore, :sloppy
(setf *message-window-gravity* :top-right)

(setf *input-window-gravity* :top-right)
(setf *suppress-frame-indicator* t)
(setf *frame-indicator-timer* t)
(setf *timeout-frame-indicator-wait* 1)
(setf *frame-number-map* "1234567890")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modeline Configuration and Setntings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *mode-line-position* :top)
(setf *mode-line-background-color* "black")
(setf *mode-line-foreground-color* "gray")
(setf *mode-line-border-width* 0)
(setf *mode-line-border-color* "red")
(setf *mode-line-pad-x* 4)
(setf *mode-line-pad-y* 0)
(setf *mode-line-timeout* 1)
(setf *time-modeline-string* "%m-%d ^B%l:%M^b")

(setf *screen-mode-line-format*
      (list "[%d] " "B: %B "
       "[^B%n^b]%W"))


(if (not (head-mode-line (current-head)))
     (toggle-mode-line (current-screen) (current-head)))
