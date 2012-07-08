(in-package :stumpwm)

;;
;; One Key to rule them all
;;

(set-prefix-key (kbd "C-q"))

;;
;; Insertions into top level bindings.
;;

(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec amixer -q set Master 5- unmute")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec amixer -q set Master 5+ unmute")
(define-key *top-map* (kbd "XF86AudioMute") "exec amixer -q sset Master toggle")

;;
;; Insertions into root level bindings.
;;

(define-key *root-map* (kbd "d") "dmenu")
(define-key *root-map* (kbd "C-r") "emacs-capture")
(define-key *root-map* (kbd "C-d") "dmenu-tychoish")
(define-key *root-map* (kbd "b") "firefox")

(define-key *root-map* (kbd "c") "term")
(define-key *root-map* (kbd "C-c") "sterm")

(define-key *root-map* (kbd "l") "mode-line")
(define-key *root-map* (kbd "C-.") "gnext")
(define-key *root-map* (kbd "C-,") "gprev")
(define-key *root-map* (kbd "p") "pull-hidden-next")

(define-key *root-map* (kbd "C-q") "pull-hidden-other")
(define-key *root-map* (kbd "q") "send-escape")
(define-key *root-map* (kbd "i") "remote-screen")

(define-key *root-map* (kbd "TAB") "fselect")
(define-key *root-map* (kbd "C-TAB") "fother")
(define-key *root-map* (kbd "C-Up") "move window up")
(define-key *root-map* (kbd "C-Down") "move window down")
(define-key *root-map* (kbd "C-Right") "move window right")
(define-key *root-map* (kbd "C-Left") "move window left")


;;
;; General group manipulation bindings.
;;


(define-key *groups-map* (kbd "q") "gselect 6")
(define-key *groups-map* (kbd "w") "gselect 7")
(define-key *groups-map* (kbd "e") "gselect 8")
(define-key *groups-map* (kbd "r") "gselect 9")
(define-key *groups-map* (kbd "t") "gselect 10")

(define-key *groups-map* (kbd "a") "gselect 1")
(define-key *groups-map* (kbd "s") "gselect 2")
(define-key *groups-map* (kbd "d") "gselect 3")
(define-key *groups-map* (kbd "f") "gselect 4")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; tycho "catchall" keybinding for personal functionality, like C-c in emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tycho-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "i")      "remote-term")
    (define-key m (kbd "f")      "remote-screen-second")
    (define-key m (kbd "g")      "gramsci-screen-second")
    (define-key m (kbd "w")      "emacs-work")
    (define-key m (kbd "e")      "emacs-tychoish")
    (define-key m (kbd "m")      "dmenu-tychoish")
    (define-key m (kbd "l")      "dmenu-stl")
    (define-key m (kbd "x")      "dmenu-work")
    m))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; System bindings for custom manipulation of underlying features
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *system-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-s")   "swank")
    (define-key m (kbd "C-l")   "engage-laptop")
    (define-key m (kbd "C-d")   "engage-dual")
    (define-key m (kbd "b")     "banish")
    (define-key m (kbd "C-b")   "banish")
    m))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Windowing and Display Related Manipulations/Keybindings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *dump-layout-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "d")     "dump-desktop-dual")
    (define-key m (kbd "h")     "dump-desktop-home")
    (define-key m (kbd "l")     "dump-desktop-laptop")
    (define-key m (kbd "o")     "dump-desktop-office")
    (define-key m (kbd "D")     "dump-desktop-dual-alt")
    (define-key m (kbd "H")     "dump-desktop-home-alt")
    (define-key m (kbd "L")     "dump-desktop-laptop-alt")
    (define-key m (kbd "O")     "dump-desktop-office-alt")
    m))

(defvar *window-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "0")     "remove")
    (define-key m (kbd "1")     "only")
    (define-key m (kbd "2")     "vsplit")
    (define-key m (kbd "3")     "hsplit")
    (define-key m (kbd "5")     "fclear")
    (define-key m (kbd "6")     "resize")
    (define-key m (kbd "c")     "repack-window-numbers")
    (define-key m (kbd "i")     "info")
    (define-key m (kbd "h")     "restore-desktop-home")
    (define-key m (kbd "l")     "restore-desktop-laptop")
    (define-key m (kbd "d")     "restore-desktop-dual")
    (define-key m (kbd "o")     "restore-desktop-office")
    (define-key m (kbd "H")     "restore-desktop-home-alt")
    (define-key m (kbd "L")     "restore-desktop-laptop-alt")
    (define-key m (kbd "D")     "restore-desktop-home-alt")
    (define-key m (kbd "O")     "restore-desktop-office-alt")
    (define-key m (kbd "p")     "place-existing-windows")
    (define-key m (kbd "s")     "swap-window")
    (define-key m (kbd "-")     "fskip")
    (define-key m (kbd "+")     "funskip")
    (define-key m (kbd "d")     '*dump-layout-map*)
    (define-key m (kbd "C-d")   '*dump-layout-map*)
    m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MPD and Music/Audio Related Keybindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *volume-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "l")     "volume-loud")
    (define-key m (kbd "q")     "volume-quiet")
    (define-key m (kbd "m")     "volume-toggle")
    m))

(define-key *mpd-map* (kbd "d") "dmpc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Key binding namespaces
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key *root-map* (kbd "C-s") '*system-map*)
(define-key *root-map* (kbd "C-t") '*tycho-map*)
(define-key *root-map* (kbd "t") '*tycho-map*)
(define-key *root-map* (kbd "j") '*mpd-map*)
(define-key *root-map* (kbd "C-x") '*window-map*)
(define-key *root-map* (kbd "x") '*window-map*)
(define-key *window-map* (kbd "C-d") '*dump-layout-map*)
(define-key *window-map* (kbd "d") '*dump-layout-map*)
(define-key *root-map* (kbd "v") '*volume-map*)
