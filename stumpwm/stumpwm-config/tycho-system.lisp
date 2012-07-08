(in-package :stumpwm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Toggling and Enabling the Mouse
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcommand toggle-mouse ()() (run-shell-command "toggle-mouse"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic Appplication Triggers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcommand firefox ()() (run-or-raise "firefox" '(:class "Firefox")))
(defcommand dmenu ()() (run-shell-command "dmenu_run -nb '#000000' -nf '#ffffff'"))
(defcommand dmpc ()() (run-shell-command "dmpc"))
(defcommand dmenu-work ()() (run-shell-command "dmenu-work"))
(defcommand dmenu-tychoish ()() (run-shell-command "dmenu-tychoish"))
(defcommand dmenu-stl ()() (run-shell-command "dmenu-stl"))

(defcommand emacs ()() (run-shell-command "emacsclient -a emacs -n -c"))
(defcommand emacs-tychoish ()() (run-shell-command "emacsclient -a emacs -n -c"))
(defcommand emacs-work ()() (run-shell-command "emacsclient -a emacs -n -c"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Terminals and Screen Sessions, elswhere
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcommand term ()() (run-shell-command "urxvtcd -title local-term"))
(defcommand sterm ()() (run-shell-command "urxvtcd -title local-screen -e screen -x tychoish"))

(defcommand remote-term ()() (run-shell-command "urxvtcd -title remote-term -e ssh -t whoami@remote.example.net"))
(defcommand remote-screen ()() (run-shell-command "urxvtcd -title remote-screen -e ssh -t whoami@remote.example.net screen -DRR"))
(defcommand remote-screen-second ()() (run-shell-command "urxvtcd -title remote-screen -e ssh -t whoami@remote.example.net screen -x"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Monitor Kickstarts and Network Manipulation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defcommand engage-dual ()()
;;             (run-shell-command "xrandr --output VGA1 --auto --right-of LVDS1 --output LVDS1 --auto")
;;             (run-shell-command "feh --bg-scale ~/.desktop.jpg"))
;; (defcommand engage-laptop ()()
;;             (restore-desktop-laptop)
;;             (run-shell-command "xrandr --output VGA1 --off --output LVDS1 --auto")
;;             (run-shell-command "feh --bg-scale ~/.desktop.jpg"))


;; Macros for Keybindings
;;

;; (defmacro defkey-top (key cmd)
;;   `(define-key *top-map* (kbd ,key) ,cmd))
;; (defmacro defkeys-top (&rest keys)
;;   (let ((ks (mapcar #'(lambda (k) (cons 'defkey-top k)) keys)))
;;     `(progn ,@ks)))

;; (defmacro defkey-root (key cmd)
;;   `(define-key *root-map* (kbd ,key) ,cmd))
;; (defmacro defkeys-root (&rest keys)
;;   (let ((ks (mapcar #'(lambda (k) (cons 'defkey-root k)) keys)))
;;     `(progn ,@ks)))
