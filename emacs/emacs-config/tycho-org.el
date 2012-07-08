(provide 'tycho-org)
; (require 'tycho-org-setup)

(require 'org-install)
(require 'org-annotate-file)
;(require 'org-w3m)
;(require 'org-velocity)
(require 'org-inlinetask)

(setq auto-mode-alist (cons '("\\.org" . org-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To use the org-agenda, edit the org-setup file and uncomment the
;; following  expression
;;
;; (require 'org-setup)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun org-set-weekday-of-timestamp ()
  "Check if cursor is within a timestamp and compute weekday from numeric
date"
  (interactive)
  (when (org-at-timestamp-p t)
    (org-timestamp-change 0 'year)
    (message "Weekday of timestamp has been adjusted.")
    t
  ))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-set-weekday-of-timestamp)

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; (global-set-key (kbd "C-c o m i") 'org-mobile-pull)
;; (global-set-key (kbd "C-c o m e") 'org-mobile-push)


(define-key org-mode-map (kbd "C-c o v") 'org-velocity-read)
(define-key org-mode-map (kbd "C-c o l s") 'org-store-link)
(define-key org-mode-map (kbd "C-c o l i") 'org-insert-link)
(define-key org-mode-map (kbd "C-c o l o") 'org-open-link-from-string)
(define-key org-mode-map (kbd "C-c o l a o") 'org-agenda-open-link)
;; (define-key org-mode-map (kbd "C-c o t") 'org-set-tags-to)
;; (define-key org-mode-map (kbd "C-c o p") 'org-insert-property-drawer)
;; (define-key org-mode-map (kbd "C-c o d") 'org-date)
 (define-key org-mode-map (kbd "C-c o s s") 'org-archive-to-archive-sibling)
(define-key org-mode-map (kbd "C-c o s t") 'org-archive-set-tag)
(define-key org-mode-map (kbd "C-c o n") 'org-narrow-to-subtree)

(define-key org-mode-map (kbd "C-c o r c") 'org-bibtex-create)
(define-key org-mode-map (kbd "C-c o r r") 'org-bibtex-create-in-current-entry)
(define-key org-mode-map (kbd "C-c o r k") 'org-bibtex-export-to-kill-ring)
(define-key org-mode-map (kbd "C-c o r v v") 'org-bibtex-check)
(define-key org-mode-map (kbd "C-c o r v a") 'org-bibtex-check-all)
(define-key org-mode-map (kbd "C-c o r s") 'org-bibtex-search)
(define-key org-mode-map (kbd "C-c o r e") 'org-bibtex)

;; (global-set-key (kbd "C-c o t") 'org-todo-list)
;; (global-set-key (kbd "C-c o a") 'org-agenda)
;; (global-set-key (kbd "C-c o j") 'org-capture)
;; (global-set-key (kbd "C-c o c") 'org-capture)
;; (global-set-key (kbd "C-c o k l") 'org-capture-goto-last-stored)
;; (global-set-key (kbd "C-c o k t") 'org-capture-goto-target)
;; (global-set-key (kbd "C-c o k w") 'org-capture-refile)
;; (global-set-key (kbd "C-c o l a") 'org-annotate-file)

(defalias 'oa 'org-agenda)
(defalias 'org-tag 'org-set-tags)

;; org mode stuff
(setq org-CUA-compatible t)
(setq org-agenda-columns-add-appointments-to-effort-sum t)
(setq org-agenda-default-appointment-duration 60)
; (setq org-agenda-include-all-todo nil)
(setq org-agenda-include-diary t)
(setq org-agenda-mouse-1-follows-link t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-unavailable-files t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown nil)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-todo-ignore-deadlines nil)
(setq org-agenda-todo-ignore-scheduled nil)
(setq org-agenda-todo-ignore-with-date nil)
(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-agenda-use-time-grid nil)
(setq org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "CITATION" "NOTES"))
(setq org-goto-interface 'outline-path-completion)
(setq org-hide-leading-stars t)
(setq org-outline-path-complete-in-steps t)
(setq org-provide-todo-statistics t)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
(setq org-refile-use-outline-path 'file)
(setq org-replace-disputed-keys t)
(setq org-reverse-note-order t)
(setq org-use-fast-tag-selection t)
(setq org-use-fast-todo-selection t)
(setq org-use-speed-commands t)
(setq org-return-follows-link t)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-track-ordered-property-with-tag nil)

(add-to-list 'org-speed-commands-user '("-" . org-narrow-to-subtree))
(add-to-list 'org-speed-commands-user '("+" . widen))

(setq org-export-email-info t)

(setq org-agenda-custom-commands
      '(("c" "Combined Tasks"
         ((agenda "" nil)
          (tags-todo "+BLOCKED<>\"t\"/!-EDIT-SEND-PUBLISH"
                     ((org-agenda-overriding-header "Simple Tasks")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (todo "EDIT"
                ((org-agenda-overriding-header "Editoral Tasks")
                 (org-agenda-dim-blocked-tasks 'invisible)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
          (todo "SEND|PUBLISH"
                ((org-agenda-overriding-header "Processing Tasks")
                 (org-agenda-dim-blocked-tasks 'invisible)))))
        ("r" "Recurring Tasks" agenda ""
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":ONGOING:"))))
        ("o" "One-Time Tasks" agenda ""
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":ONGOING:"))))
        ("w" "Writing Tasks, without Editing" tags-todo "/!-EDIT-SEND-PUBLISH"
         ((org-agenda-skip-function '(org-agenda-skip-subtree-if 'regexp ":ONGOING:"))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
          (org-agenda-sorting-strategy '(category-keep))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
        ("e" "Editing, Sending and Publishign" todo "EDIT|SEND|PUBLISH"
         ((org-agenda-skip-function '(org-agenda-skip-subtree-if 'regexp ":ONGOING:"))))))

;; remember and org mode config custtom

(defun org-add-eventually()
  "Adding a file to org-agenda when saved"
  (interactive)
  (if (string= major-mode "org-mode")
      (org-agenda-file-to-front)))

(defadvice org-capture-finalize (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame if it is the capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (org-capture)
  )

(defvar org-capture-anything
  '((name . "Org Capture")
    (candidates . (lambda () (mapcar 'car org-capture-templates)))
    (action . (lambda (name)
                (let* ((orig-template org-capture-templates)
                       (org-capture-templates
                        (list (assoc name orig-template))))
                  (call-interactively 'org-capture))))))

(defun org-agenda-reschedule-to-today ()
  (interactive)
  (flet ((org-read-date (&rest rest) (current-time)))
    (call-interactively 'org-agenda-schedule)))
