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
;; Derrida Layout Commands
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcommand dump-desktop-office ()()
  (dump-desktop-to-file "~/stumpwm/layout/derrida-office")
  (dump-window-placement-rules "~/stumpwm/layout/derrida-office.wplace"))
(defcommand dump-desktop-office-alt ()()
  (dump-desktop-to-file "~/stumpwm/layout/derrida-office-alt")
  (dump-window-placement-rules "~/stumpwm/layout/derrida-office-alt.wplace"))

(defcommand restore-desktop-office ()()
  (restore-from-file "~/stumpwm/layout/derrida-office")
  (restore-window-placement-rules "~/stumpwm/layout/derrida-office.wplace")
  (run-commands "fselect 1" "place-existing-windows"))
(defcommand restore-desktop-office-alt ()()
  (restore-from-file "~/stumpwm/layout/derrida-office-alt")
  (restore-window-placement-rules "~/stumpwm/layout/derrida-office-alt.wplace")
  (run-commands "fselect 1" "place-existing-windows"))

(defcommand restore-derrida-base ()()
  (restore-from-file "~/stumpwm/layout/group-derrida-base")
  (run-commands "fselect 1" "place-existing-windows"))
(defcommand restore-thirds ()()
  (restore-from-file "~/stumpwm/layout/group-thirds")
  (run-commands "fselect 1" "place-existing-windows"))


(add-hook *focus-frame-hook*
	  (lambda (cframe lframe)
	    (declare (ignore lframe))
	    (multiple-value-bind (x y)
		(xlib:global-pointer-position *display*)
	      (unless (eq cframe (find-frame (current-group) x y))
		(banish)))))

(run-commands "gselect 1" "grename cyborg" "restore-desktop-office" "gselect 1" "fselect 1")
