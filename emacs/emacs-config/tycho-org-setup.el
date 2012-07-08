(provide 'tycho-org-setup)

(setq diary-file "~/org/archive/diary")
(setq org-directory "~/org/")
;; (setq org-mobile-inbox-for-pull "~/org/mobile.org")
;; (setq org-mobile-directory "~/Dropbox/org-mobile")

(setq org-default-notes-file (concat org-directory "codex.org"))
(setq org-annotate-file-storage-file "~/org/annotations.org")
;; (setq org-agenda-files '("~/org/"
;;                          "~/org/data/"
;;                          ))

;; (global-set-key (kbd "C-c o f o")
;;                 (lambda () (interactive) (find-file "~/org/codex.org")))
;; (global-set-key (kbd "C-c o f j")
;;                 (lambda () (interactive) (find-file "~/org/journal.org")))
;; (global-set-key (kbd "C-c o f b")
;;                 (lambda () (interactive) (find-file "~/org/tychoish.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org-capture-init
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-capture-templates
;;       '(("t" "Flow-Task" plain (file "~/wiki/flow.mdwn")
;;          "TODO %?")

;;         ("l" "Add Long Term/Ongoing Task" plain
;;          (file+regexp "~/wiki/flow.mdwn" "^## Recurring Tasks[[:ascii:][:nonascii:]]\\{2\\}")
;;          "TODO  %?")
;;         ("s" "Add Scheduled Item" plain
;;          (file+regexp "~/wiki/flow.mdwn" "^## Schedule[[:ascii:][:nonascii:]]\\{2\\}")
;;          "SCHEDULED %?")
;;         ("p" "Add Project to Flow" plain
;;          (file+regexp "~/wiki/flow.mdwn" "^## Projects[[:ascii:][:nonascii:]]\\{2\\}")
;;          "### %?")
;;         ("r" "Add New Rhizome Task" plain
;;          (file "~/wiki/rhizome.mdwn")
;;          "WRITE %?")
;;         ("u" "Update Rhizome Item" plain
;;          (file "~/wiki/rhizome/update.mdwn")
;;          "WRITE %?")


;;         ("o" "org codex, origin file")
;;         ("oc" "codex" entry (file+headline "~/org/codex.org" "Inbox")
;;          "* %?\n%i" :prepend t)
;;         ("ot" "todo" entry (file+headline "~/org/codex.org" "Tasks")
;;          "* TODO %?" :prepend t)
;;         ("on" "notes" entry (file+headline "~/org/codex.org" "Notes")
;;          "* %?\n%i\n\n%x" :prepend t)
;;         ("ol" "shopping list item" entry (file+headline "~/org/codex.org" "Shopping")
;;          "* %?" :prepend t)
;;         ("oe" "email" entry (file+headline "~/org/codex.org" "Email")
;;          "* TODO %?" :prepend t)

;;         ("j" "journaling and task logging (org)")
;;         ("jl" "log" entry (file "~/org/journal.org")
;;          "* Activity Log %t\n%?":prepend nil)
;;         ("jj" "journal" entry (file "~/org/journal.org")
;;          "* Personal %t\n%?":prepend nil)

;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;         ("d" "data, records, citation")
;;         ("dc" "clippings" entry (file "~/org/data/clippings.org")
;; "* PROCESS %^{Title} %^g
;; :CITATION:
;; :date: %^t
;; :link: %^{link}
;; :END:
;; %?%x"
;; :prepend t)

;;         ("db" "bookmarks/links" entry (file "~/org/data/links.org")
;; "* PROCESS %^{Link Destination} %^g
;; :CITATION:
;; :date: %^t
;; :END:
;; %?
;; %x"
;; :prepend t)

;;         ("df" "fact-file" entry (file "~/org/data/facts.org")
;; "* %^{Title} %^g
;; :CITATION:
;; :date: %^t
;; :link: %^{link}
;; :END:

;; %x %?"
;; :prepend t)

;;         ("da" "annotate" entry (file "~/org/data/annotations.org")
;; "* %^{Title} %^g
;; :CITATION:
;; :date: %^t
;; :cite-key: %^{key}
;; :link: %^{link}
;; :END:

;; %?"
;; :prepend t)

;; ))
