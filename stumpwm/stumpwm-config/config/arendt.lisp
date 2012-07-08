(in-package :stumpwm)

(set-contrib-dir "~/stumpwm")

(load-module "tycho-settings")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; It makes the emacs-slime go.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf stumpwm:*top-level-error-action* :abort)

;; Startup Initialization

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Arendt Layout Commands
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcommand dump-desktop-home ()()
  (dump-desktop-to-file "~/stumpwm/layout/arendt-home")
  (dump-window-placement-rules "~/stumpwm/layout/arendt-home.wplace"))
(defcommand dump-desktop-laptop ()()
  (dump-desktop-to-file "~/stumpwm/layout/arendt-laptop")
  (dump-window-placement-rules "~/stumpwm/layout/arendt-laptop.wplace"))
(defcommand dump-desktop-office ()()
  (dump-desktop-to-file "~/stumpwm/layout/arendt-office")
  (dump-window-placement-rules "~/stumpwm/layout/arendt-office.wplace"))

(defcommand dump-desktop-home-alt ()()
  (dump-desktop-to-file "~/stumpwm/layout/arendt-home-alt")
  (dump-window-placement-rules "~/stumpwm/layout/arendt-home-alt.wplace"))
(defcommand dump-desktop-dual-alt ()()
  (dump-desktop-to-file "~/stumpwm/layout/arendt-dual-alt")
  (dump-window-placement-rules "~/stumpwm/layout/arendt-dual-alt.wplace"))
(defcommand dump-desktop-laptop-alt ()()
  (dump-desktop-to-file "~/stumpwm/layout/arendt-laptop-alt")
  (dump-window-placement-rules "~/stumpwm/layout/arendt-laptop-alt.wplace"))
(defcommand dump-desktop-office-alt ()()
  (dump-desktop-to-file "~/stumpwm/layout/arendt-office-alt")
  (dump-window-placement-rules "~/stumpwm/layout/arendt-office-alt.wplace"))

(defcommand restore-desktop-home ()()
  (restore-from-file "~/stumpwm/layout/arendt-home")
  (restore-window-placement-rules "~/stumpwm/layout/arendt-home.wplace")
  (run-commands "fselect 1" "place-existing-windows"))
(defcommand restore-desktop-laptop ()()
  (restore-from-file "~/stumpwm/layout/arendt-laptop")
  (restore-window-placement-rules "~/stumpwm/layout/arendt-laptop.wplace")
  (run-commands "fselect 1" "place-existing-windows"))
(defcommand restore-desktop-office ()()
  (restore-from-file "~/stumpwm/layout/arendt-office")
  (restore-window-placement-rules "~/stumpwm/layout/arendt-office.wplace")
  (run-commands "fselect 1" "place-existing-windows"))
(defcommand restore-desktop-dual ()()
  (restore-from-file "~/stumpwm/layout/arendt-dual")
  (restore-window-placement-rules "~/stumpwm/layout/arendt-dual.wplace")
  (run-commands "fselect 1" "place-existing-windows"))

(defcommand restore-desktop-dual-home-alt ()()
  (restore-from-file "~/stumpwm/layout/arendt-home-alt")
  (restore-window-placement-rules "~/stumpwm/layout/arendt-home-alt.wplace")
  (run-commands "fselect 1" "place-existing-windows"))
(defcommand restore-desktop-dual-dual-alt ()()
  (restore-from-file "~/stumpwm/layout/arendt-daul-alt")
  (restore-window-placement-rules "~/stumpwm/layout/arendt-dual-alt.wplace")
  (run-commands "fselect 1" "place-existing-windows"))
(defcommand restore-desktop-laptop-alt ()()
  (restore-from-file "~/stumpwm/layout/arendt-laptop-alt")
  (restore-window-placement-rules "~/stumpwm/layout/arendt-laptop-alt.wplace")
  (run-commands "fselect 1" "place-existing-windows"))
(defcommand restore-desktop-office-alt ()()
  (restore-from-file "~/stumpwm/layout/arendt-office-alt")
  (restore-window-placement-rules "~/stumpwm/layout/arendt-office-alt.wplace")
  (run-commands "fselect 1" "place-existing-windows"))

(defcommand restore-arendt-base ()()
  (restore-from-file "~/stumpwm/layout/group-arendt-base")
  (run-commands "fselect 1" "place-existing-windows"))
(defcommand restore-thirds ()()
  (restore-from-file "~/stumpwm/layout/group-thirds")
  (run-commands "fselect 1" "place-existing-windows"))

(run-commands "gselect 1" "grename cyborg" "restore-desktop-laptop" "gselect 1" "fselect 1")
