(in-package :stumpwm)

(set-contrib-dir "~/stumpwm")

(load-module "tycho-settings")

;; Startup Initialization
(run-commands "gselect 1" "grename cyborg")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Deleuze Layout Commands
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcommand dump-desktop-home ()()
  (dump-desktop-to-file "~/stumpwm/layout/deleuze-home"))
(defcommand dump-desktop-dual ()()
  (dump-desktop-to-file "~/stumpwm/layout/deleuze-dual"))
(defcommand dump-desktop-laptop ()()
  (dump-desktop-to-file "~/stumpwm/layout/deleuze-laptop"))
(defcommand dump-desktop-office ()()
  (dump-desktop-to-file "~/stumpwm/layout/deleuze-office"))

(defcommand dump-desktop-home-alt ()()
  (dump-desktop-to-file "~/stumpwm/layout/deleuze-home-alt"))
(defcommand dump-desktop-dual-alt ()()
  (dump-desktop-to-file "~/stumpwm/layout/deleuze-dual-alt"))
(defcommand dump-desktop-laptop-alt ()()
  (dump-desktop-to-file "~/stumpwm/layout/deleuze-laptop-alt"))
(defcommand dump-desktop-office-alt ()()
  (dump-desktop-to-file "~/stumpwm/layout/deleuze-office-alt"))

(defcommand restore-desktop-home ()()
  (restore-from-file "~/stumpwm/layout/deleuze-home")
  (place-existing-windows))
(defcommand restore-desktop-dual ()()
  (restore-from-file "~/stumpwm/layout/deleuze-home")
  (place-existing-windows))
(defcommand restore-desktop-laptop ()()
  (restore-from-file "~/stumpwm/layout/deleuze-laptop")
  (place-existing-windows))
(defcommand restore-desktop-office ()()
  (restore-from-file "~/stumpwm/layout/deleuze-office")
  (place-existing-windows))

(defcommand restore-desktop-dual-home-alt ()()
  (restore-from-file "~/stumpwm/layout/deleuze-home-alt")
  (place-existing-windows))
(defcommand restore-desktop-dual-dual-alt ()()
  (restore-from-file "~/stumpwm/layout/deleuze-daul-alt")
  (place-existing-windows))
(defcommand restore-desktop-laptop-alt ()()
  (restore-from-file "~/stumpwm/layout/deleuze-laptop-alt")
  (place-existing-windows))
(defcommand restore-desktop-office-alt ()()
  (restore-from-file "~/stumpwm/layout/deleuze-office-alt")
  (place-existing-windows))

(defcommand restore-deleuze-base ()()
  (restore-from-file "~/stumpwm/layout/group-deleuze-base")
  (place-existing-windows))

(defcommand restore-thirds ()()
  (restore-from-file "~/stumpwm/layout/group-thirds")
  (place-existing-windows))

(run-commands "restore-desktop-laptop" "gselect 1")
