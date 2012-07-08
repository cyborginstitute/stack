;;; -*- Mode: Emacs-Lisp -*-
;;; Copyright © 2002-2009 Jamie Zawinski <jwz@jwz.org>.
;;;
;;; Permission to use, copy, modify, distribute, and sell this software and its
;;; documentation for any purpose is hereby granted without fee, provided that
;;; the above copyright notice appear in all copies and that both that
;;; copyright notice and this permission notice appear in supporting
;;; documentation.  No representations are made about the suitability of this
;;; software for any purpose.  It is provided "as is" without express or 
;;; implied warranty.
;;;
;;; Created: 27-May-2002.
;;;
;;; This posts to LiveJournal.
;;; Really all it does is let you edit some text, then constructs
;;; a proper URL and does a HTTP POST to it.
;;; It's clever about figuring out what your password is by
;;; digging around in your Netscape/Mozilla cookies file.
;;; But make sure `jwz-lj-lj-user-name' is set properly.
;;;
;;; This package expects you to type in HTML directly.  Which I do.
;;;
;;; See monkeybutter.el for something that has facilities for
;;; auto-converting plain text to HTML.
;;;
;;; Interesting commands:
;;;
;;;    M-x livejournal  Fill in the fields, edit the body.
;;;    M-x ljpreview    Save the HTML to a temp file and send it to a
;;;                     web browser to see what it will look like.
;;;                     Converts the <LJ> tags to something readable.
;;;    C-c C-c          Submit it.
;;;
;;; Useful non-interactive function:
;;;
;;;    jwz-lj-post      Useful for letting other programs submit entries.
;;;

(defvar jwz-lj-lj-user-name (user-login-name)
  "*Your LiveJournal ID.")

(defvar jwz-lj-fcc-file nil
  "*Where to save a copy of your LJ posts.")

(defvar jwz-lj-bcc-address nil
  "*Where to mail a copy of your LJ posts.")

(defvar jwz-lj-validator "validate.pl -"
  "*If non-nil, a program to run on a region to check for HTML errors.")

(defvar jwz-lj-known-tags '()
  "Cache of the tags used on any of your posts.
Used to warn you when you make a typo in the Tags: field.")

(defvar jwz-lj-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'jwz-lj-mode-map)
    (define-key map "\C-c\C-c" 'jwz-lj-submit)
    map))


(defconst jwz-lj-entity-table
  '(
    ;("quot"   . ?\") ("amp"    . ?\&) ("lt"     . ?\<) ("gt"     . ?\>)
    ;("nbsp"   . ?\ )
    ("iexcl"  . ?\¡) ("cent"   . ?\¢) ("pound"  . ?\£)
    ("curren" . ?\¤) ("yen"    . ?\¥) ("brvbar" . ?\¦) ("sect"   . ?\§)
    ("uml"    . ?\¨) ("copy"   . ?\©) ("ordf"   . ?\ª) ("laquo"  . ?\«)
    ("not"    . ?\¬) ("shy"    . ?\­) ("reg"    . ?\®) ("macr"   . ?\¯)
    ("deg"    . ?\°) ("plusmn" . ?\±) ("sup2"   . ?\²) ("sup3"   . ?\³)
    ("acute"  . ?\´) ("micro"  . ?\µ) ("para"   . ?\¶) ("middot" . ?\·)
    ("cedil"  . ?\¸) ("sup1"   . ?\¹) ("ordm"   . ?\º) ("raquo"  . ?\»)
    ("frac14" . ?\¼) ("frac12" . ?\½) ("frac34" . ?\¾) ("iquest" . ?\¿)
    ("Agrave" . ?\À) ("Aacute" . ?\Á) ("Acirc"  . ?\Â) ("Atilde" . ?\Ã)
    ("Auml"   . ?\Ä) ("Aring"  . ?\Å) ("AElig"  . ?\Æ) ("Ccedil" . ?\Ç)
    ("Egrave" . ?\È) ("Eacute" . ?\É) ("Ecirc"  . ?\Ê) ("Euml"   . ?\Ë)
    ("Igrave" . ?\Ì) ("Iacute" . ?\Í) ("Icirc"  . ?\Î) ("Iuml"   . ?\Ï)
    ("ETH"    . ?\Ð) ("Ntilde" . ?\Ñ) ("Ograve" . ?\Ò) ("Oacute" . ?\Ó)
    ("Ocirc"  . ?\Ô) ("Otilde" . ?\Õ) ("Ouml"   . ?\Ö) ("times"  . ?\×)
    ("Oslash" . ?\Ø) ("Ugrave" . ?\Ù) ("Uacute" . ?\Ú) ("Ucirc"  . ?\Û)
    ("Uuml"   . ?\Ü) ("Yacute" . ?\Ý) ("THORN"  . ?\Þ) ("szlig"  . ?\ß)
    ("agrave" . ?\à) ("aacute" . ?\á) ("acirc"  . ?\â) ("atilde" . ?\ã)
    ("auml"   . ?\ä) ("aring"  . ?\å) ("aelig"  . ?\æ) ("ccedil" . ?\ç)
    ("egrave" . ?\è) ("eacute" . ?\é) ("ecirc"  . ?\ê) ("euml"   . ?\ë)
    ("igrave" . ?\ì) ("iacute" . ?\í) ("icirc"  . ?\î) ("iuml"   . ?\ï)
    ("eth"    . ?\ð) ("ntilde" . ?\ñ) ("ograve" . ?\ò) ("oacute" . ?\ó)
    ("ocirc"  . ?\ô) ("otilde" . ?\õ) ("ouml"   . ?\ö) ("divide" . ?\÷)
    ("oslash" . ?\ø) ("ugrave" . ?\ù) ("uacute" . ?\ú) ("ucirc"  . ?\û)
    ("uuml"   . ?\ü) ("yacute" . ?\ý) ("thorn"  . ?\þ) ("yuml"   . ?\ÿ))
  "HTML entities to Latin1 characters.")


(defun jwz-lj-entify (&optional start end)
  "Convert any non-ASCII characters in the region to HTML entities.
If there is no region, use the whole buffer."
  (interactive)
  (let ((re (concat "["
                    (mapconcat #'(lambda (x) (make-string 1 (cdr x)))
                               jwz-lj-entity-table nil)
                    "]"))
        (case-fold-search nil))
    (cond ((or start end)
           (or start (setq start (point-min)))
           (or end   (setq end   (point-max))))
          (t
           (setq start (point-min))
           (setq end (point-max)))
          (if (region-active-p)
              (setq start (if (< (point) (mark)) (point) (mark))
                    end   (if (< (point) (mark)) (mark) (point)))))
    (save-excursion
      (goto-char start)
      (setq end (copy-marker end))
      (while (search-forward-regexp re end t)
        (let* ((ch (preceding-char))
               (entity (or (car (rassq ch jwz-lj-entity-table))
                           (error "no entity %c" ch))))
          (delete-char -1)
          (insert-before-markers "&" entity ";")))))

; (if (and (fboundp 'find-non-ascii-charset-region)
;          (find-non-ascii-charset-region start end))
  (if (and (fboundp 'charsets-in-region)
           (delq 'ascii (charsets-in-region start end)))
      (error "non-ascii characters exist in this buffer!"))
  )


(defun jwz-lj-http-encode (string &optional convert-latin1-p)
  "Encodes the string as per the URL quoting conventions (%XX)."
  (let (b)
    (save-excursion
      (unwind-protect
          (progn
            (setq b (get-buffer-create " *jwz-lj-encode*"))
            (set-buffer b)
            (erase-buffer)
            (insert string)

            (if convert-latin1-p
                (jwz-lj-entify (point-min) (point-max)))

            (goto-char (point-min))
            (while (re-search-forward "[^-a-zA-Z0-9_]" nil t)
              (let ((c (preceding-char)))
                (delete-char -1)
                (insert "%" (format "%02X" c))))

            (buffer-string))
        (if b (kill-buffer b))))))


(defun jwz-lj-make-url (subject body user
                                &optional security-level tags community
                                auto-format-p disallow-comments-p
                                current-mood current-music)
  "Creates a URL for making a post to LiveJournal."

  (let ((friends-mask nil))
    (setq subject   (jwz-lj-http-encode subject t))
    (setq body      (jwz-lj-http-encode body t))

    (or security-level (setq security-level "public"))

    (cond ((member security-level '("public" "private"))
           nil)
          ((equal security-level "friends")
           (setq security-level "usemask"
                 friends-mask 1))
          (t
           (let* ((groups (jwz-lj-get-friends-groups))
                  (n (cdr (assoc security-level groups))))
             (cond (n
                    (setq security-level "usemask"
                          friends-mask (lsh 1 n)))
                   (t
                    (error "unknown friends group: %s" security-level))))))

    (setq security-level (jwz-lj-http-encode security-level t))

    (if (and tags (not (equal tags "")))
        (jwz-lj-check-tags tags))

    (if (stringp current-mood)
        (setq current-mood  (jwz-lj-http-encode current-mood t)))

    (if current-music
        (setq current-music (jwz-lj-http-encode current-music t)))

    (let* ((timestr (current-time-string))
           (year (substring timestr 20 24))
           (mon  (format "%02d"
                         (position (intern (substring timestr  4  7))
                                   '(nil Jan Feb Mar Apr May Jun
                                         Jul Aug Sep Oct Nov Dec))))
           (day  (format "%02d" (string-to-int (substring timestr  8 10))))
           (hour (format "%02d" (string-to-int (substring timestr 11 13))))
           (min  (format "%02d" (string-to-int (substring timestr 14 16))))
           (url-base "http://www.livejournal.com/interface/flat")
           (url (concat
                 url-base
                 "?mode=postevent"
                 "&user=" user
                 "&auth_method=cookie"
                 "&ver=0"
                 "&subject=" subject
                 "&security=" security-level
                 (if friends-mask
                     (format "&allowmask=%d" friends-mask)
                   "")
                 (if tags
                     (concat "&prop_taglist=" (jwz-lj-http-encode tags t))
                   "")
                 (if (and community (not (equal community "")))
                     (concat "&usejournal=" (jwz-lj-http-encode community t))
                   "")
                 "&year=" year
                 "&mon=" mon
                 "&day=" day
                 "&hour=" hour
                 "&min=" min
                 (cond ((integerp current-mood)
                        (concat "&prop_current_moodid=" current-mood))
                       (current-mood
                        (concat "&prop_current_mood=" current-mood))
                       (t ""))
                 (if current-music
                     (concat "&prop_current_music=" current-music)
                   "")
                 "&prop_opt_preformatted=" (if auto-format-p "0" "1")
                 "&prop_opt_nocomments=" (if disallow-comments-p "1" "0")
                 "&event=" body
                 )))
      url)))


(defun jwz-lj-post-1 (url cookies)
  "Does an HTTP POST to the given URL, with the given cookie alist.
Signals an error if the post is unsuccessful.
Returns the post ID number if successful."

  (or (string-match "\\`http://\\([^/]+\\)\\([^?&]+\\)\\?\\(.*\\)\\'" url)
      (error "unparsable url: %s" url))

  ;; convert alist entries to an http header.
  (setq cookies (concat "Cookie: "
                        (mapconcat #'(lambda (c) (concat (car c) "=" (cdr c)))
                                   cookies
                                   "; ")
                        "\r\n"))

  (let* ((timeout 180)  ; seconds to wait
         (host (match-string 1 url))
         (port 80)  ; sue me
         (path (match-string 2 url))
         (args (match-string 3 url))
         (post-id nil)

         (post-cmd
          (concat "POST " path " HTTP/1.0\r\n"
                  "Content-Type: application/x-www-form-urlencoded\r\n"
                  "Content-Length: " (int-to-string (length args)) "\r\n"
                  "Host: " host "\r\n"
                  "X-LJ-Auth: cookie\r\n"
                  cookies
                  "\r\n"
                  args))
         proc buf)

    (unwind-protect
        (progn
          (setq proc (open-network-stream "LiveJournal"
                                          "*LiveJournal-Server-Response*"
                                          host
                                          port)
                buf (process-buffer proc))

          (process-send-string proc post-cmd)
          (message "Posted to %s; waiting for response..." host)

          (while (equal (process-status proc) 'open)
            (unless (accept-process-output proc timeout)
              (delete-process proc)
              (error "Server error: timed out while waiting!")))

          (message "Response received; processing...")

          (with-current-buffer buf
            (setq post-id (jwz-lj-parse-response)))
          (message "Posted!")
          )
      ;; unwind-protect
      (if buf (kill-buffer buf)))
    post-id))


(defun jwz-lj-parse-response ()
  "Parses an LJ post response (success, errors) out of the current buffer."
  (let ((ok nil)
        (anum nil)
        (itemid nil)
        (friendgroups nil)
        (friendarray nil)
        (tagarray nil)
        )
    (save-excursion
      (goto-char (point-min))
      (or (re-search-forward "\n\r?\n" nil t)
          (error "LJ Error: couldn't find end of HTTP headers in response"))
      (while (not (eobp))
        (let ((key (buffer-substring (point)
                                     (progn (forward-line 1) (1- (point)))))
              (val (buffer-substring (point)
                                     (progn (forward-line 1) (1- (point))))))
          (cond ((equal key "success")
                 (setq ok val))
                ((equal key "errmsg")
                 (error (concat "LJ Error: " val)))
                ((equal key "anum")
                 (setq anum val))
                ((equal key "itemid")
                 (setq itemid val))
                ((string-match "^frgrp_\\([0-9]+\\)_name$" key)
                 (setq friendgroups
                       (cons (cons val (string-to-int (match-string 1 key)))
                             friendgroups))
                 (setq ok t))
                ((string-match "^tag_\\([0-9]+\\)_\\([a-z_]+\\)$" key)
                 (or tagarray (setq tagarray (make-vector 1000 nil)))
                 (let ((n (string-to-int (match-string 1 key)))
                       (sub (match-string 2 key)))
                   (if (>= n (length tagarray))
                       (error "more than %s many tags: %s" 
                              (length tagarray) n))
                   (or (aref tagarray n) (aset tagarray n (cons nil nil)))
                   (cond ((equal sub "name")
                          (setcar (aref tagarray n) val))
                         ((equal sub "uses")
                          (setcdr (aref tagarray n) (string-to-int val)))
                         (t nil)) ; don't care about rest
                   ))
                ((string-match "^friend_\\([0-9]+\\)_\\([a-z]+\\)$" key)
                 (let ((n (match-string 1 key))
                       (sub (match-string 2 key))
                       sym
                       fd)
                   (or friendarray (setq friendarray (make-vector 1000 nil)))
                   (setq sym (intern n friendarray)
                         fd (cons (cons sub val) 
                                  (and (boundp sym) (symbol-value sym))))
                   (set sym fd))
                 (setq ok t))
                ))))
    (or ok
        (error "LJ Error: neither 'ok' nor 'errmsg' response?"))

    (cond (friendgroups
           (nreverse friendgroups))
          (friendarray
           (let ((result '()))
             (mapatoms
              #'(lambda (s)
                  (setq s (symbol-value s))
                  (let ((user (cdr (assoc "user" s)))
                        (name (cdr (assoc "name" s)))
                        (bd   (cdr (assoc "birthday" s)))
                        (type (cdr (assoc "type" s)))
                        (grp  (or (cdr (assoc "groupmask" s)) "0")))
                    (if (and user (not type))
                        (setq result (cons (list user name (string-to-int grp)
                                                 bd)
                                           result)))))
              friendarray)
             (setq result
                   (sort result
                         #'(lambda (a b) (string-lessp (car a) (car b)))))
             result))
          (tagarray
           (sort (delq nil (append tagarray nil))
                 #'(lambda (a b) (> (or (cdr a) 0) (or (cdr b) 0)))))
          ((not (and anum itemid))
           (error "no 'anum' or 'itemid' in response?"))
          ((not anum) (error "no 'anum' in response?"))
          ((not itemid) (error "no 'anum' in response?"))
          (t
           (setq anum   (string-to-int anum)
                 itemid (string-to-int itemid))
           ;; return the post ID
           (logior (lsh itemid 8) anum)
           ))))

(defun jwz-lj-get-friends-groups ()
  "Retrieves the alist of defined friends groups from livejournal."
  (let* ((cookies (or (cdr (jwz-lj-get-cookies))
                      (error "no LJ cookies found")))
         (url-base "http://www.livejournal.com/interface/flat")
         (url (concat url-base
                      "?mode=getfriendgroups"
                      "&user=" jwz-lj-lj-user-name
                      "&auth_method=cookie"
                      "&ver=0"
                      )))
    (jwz-lj-post-1 url cookies)))

(defun jwz-lj-get-friends ()
  "Retrieves the alist of defined friends from livejournal.
Elements are: (\"username\" \"realname\" (\"groups\") \"birthday\")."
  (let* ((cookies (or (cdr (jwz-lj-get-cookies))
                      (error "no LJ cookies found")))
         (url-base "http://www.livejournal.com/interface/flat")
         (url (concat url-base
                      "?mode=getfriends"
                      "&includebdays=1"
                      "&user=" jwz-lj-lj-user-name
                      "&auth_method=cookie"
                      "&ver=0"
                      ))
         (friends (jwz-lj-post-1 url cookies))
         (groups (jwz-lj-get-friends-groups)))
    (dolist (friend friends)
      (let ((g0 (nth 2 friend))
            (g1 '()))
        (dolist (ga groups)
          (if (not (equal (car ga) "Default View"))
              (if (/= 0 (logand g0 (lsh 1 (cdr ga))))
                  (setq g1 (cons (car ga) g1)))))
        (setf (nth 2 friend) g1)))
    friends))

(defun jwz-lj-get-tags ()
  "Retrieves the alist of defined tags from livejournal."
  (let* ((cookies (or (cdr (jwz-lj-get-cookies))
                      (error "no LJ cookies found")))
         (url-base "http://www.livejournal.com/interface/flat")
         (url (concat url-base
                      "?mode=getusertags"
                      "&user=" jwz-lj-lj-user-name
                      "&auth_method=cookie"
                      "&ver=0"
                      )))
    (jwz-lj-post-1 url cookies)))


(defun jwz-lj-check-tags (tags)
  "Warn if any of the tags in the string have not been used before."
  (or jwz-lj-known-tags
      (setq jwz-lj-known-tags (jwz-lj-get-tags)))
  (let (b bad)
    (save-excursion
      (unwind-protect
          (let (p)
            (setq b (get-buffer-create " *jwz-lj-tags*"))
            (set-buffer b)
            (erase-buffer)
            (insert tags)
            (goto-char (point-min))
            (while (re-search-forward "[ \t\r\n]*,[ \t\r\n]*" nil t)
              (delete-region (match-beginning 0) (match-end 0))
              (insert "\n"))
            (goto-char (point-min))
            (while (not (eobp))
              (setq p (point))
              (end-of-line)
              (or (= p (point))
                  (let ((tag (buffer-substring p (point))))
                    (if (not (assoc tag jwz-lj-known-tags))
                        (setq bad (if bad (concat bad ", " tag) tag)))))
              (forward-line 1)))
        (if b (kill-buffer b))))
    (cond (bad
           (or (yes-or-no-p (concat "Post with new tags " bad "? "))
               (error "abort.")))))
  nil)


(defun jwz-lj-get-cookies ()
  "Looks in the Netscape and/or Mozilla cookie files to find LiveJournal data.
Returns an alist of matching cookies."
  (jwz-lj-get-cookies-1 "livejournal.com"))

;(defun jwz-lj-get-twitter-cookies ()
;  "Looks in the Netscape and/or Mozilla cookie files to find Twitter data.
;Returns an alist of matching cookies."
;  (jwz-lj-get-cookies-1 "twitter.com"))

(defun jwz-lj-get-cookies-1 (domain)
  "Looks in the Netscape and/or Mozilla cookie files to find LiveJournal data.
Returns an alist of matching cookies."

  (let ((files '())
        (cookies '())
        (host-match (concat "\\b" (regexp-quote domain) "$"))
        file)

    ;; Safari
    (if (file-exists-p "~/Library/Cookies/Cookies.plist")
        (setq files (append files (list "~/Library/Cookies/Cookies.plist"))))

    ;; Netscape
    (if (file-directory-p "~/.netscape")
        (setq files (append files (list "~/.netscape/cookies"))))

    ;; Firefox 1-3
    (cond
     ((file-directory-p "~/Library/Application Support/Firefox/Profiles")
      (let ((dir (car (directory-files
                       "~/Library/Application Support/Firefox/Profiles"
                       t "\\.default$" nil 'dirs))))
        (or dir (error "couldn't figure firefox salt directory"))
        (let ((f1 (concat dir "/cookies.txt"))
              (f2 (concat dir "/cookies.sqlite")))
          (if (file-exists-p f1) (setq files (append files (list f1))))
          (if (file-exists-p f2) (setq files (append files (list f2)))))))

     ((file-directory-p "~/.mozilla/firefox")
      (let ((dir (car (directory-files "~/.mozilla/firefox"
                                       t "\\.default$" nil 'dirs))))
        (or dir (error "couldn't figure firefox salt directory"))
        (let ((f1 (concat dir "/cookies.txt"))
              (f2 (concat dir "/cookies.sqlite")))
          (if (file-exists-p f1) (setq files (append files (list f1))))
          (if (file-exists-p f2) (setq files (append files (list f2)))))))

     ;; Don't check Mozilla cookies if a Firefox profile exists,
     ;; since the Mozilla cookies are probably out of date.
     ((file-directory-p "~/.mozilla")
      (let ((dir
             (let (d)
               (cond ((file-directory-p (setq d "~/.mozilla/default"))
                      d)
                     ((file-directory-p
                       (setq d (concat "~/.mozilla/" (user-login-name))))
                      d)
                     (t (error "can't figure out your .mozilla profile"))))))
        (setq dir (car (directory-files dir t "\\.slt$" nil 'dirs)))
        (or dir (error "couldn't figure mozilla salt directory"))
        (setq files (cons (concat dir "/cookies.txt") files)))))

    ;; Galeon
    (if (file-directory-p "~/.galeon/mozilla/galeon")
        (setq files (cons "~/.galeon/mozilla/galeon/cookies.txt"
                          files)))

    ;; Emacs-W3 URL
    (if (file-directory-p "~/.url")
	(if (file-exists-p "~/.url/cookies")
	    (setq files (cons "~/.url/cookies" files))))

    (while files
      (save-excursion
        (let (b)
          (unwind-protect
              (progn
                (setq b (get-buffer-create " *lj-cookie-tmp*"))
                (set-buffer b)
                (cond ((string-match "\\.sqlite$" (car files))
                       (jwz-lj-extract-sql-cookies (car files) domain))
                      ((string-match "\\.plist$" (car files))
                       (jwz-lj-extract-xml-cookies (car files) domain))
                      (t 
                       (insert-file-contents (car files) nil nil nil t)))
                (goto-char (point-min))
                (while (not (eobp))
                  (if (looking-at (concat "^\\([^\t\r\n]+\\)\t"  ; 1 host
                                           "\\([^\t\r\n]+\\)\t"  ; 2 bool
                                           "\\([^\t\r\n]+\\)\t"  ; 3 path
                                           "\\([^\t\r\n]+\\)\t"  ; 4 bool
                                           "\\([^\t\r\n]+\\)\t"  ; 5 time_t
                                           "\\([^\t\r\n]+\\)\t"  ; 6 key
                                           "\\([^\t\r\n]+\\)$")) ; 7 val
                      (let ((host (match-string 1))
                            (key (match-string 6))
                            (val (match-string 7)))
                        (if (and (string-match host-match host)
                                 (not (assoc key cookies)))
                            (setq cookies (cons (cons key val) cookies)))
                        ))
                  (forward-line 1))
                )
            ;; unwind-protected
            (if b (kill-buffer b)))))
      (if cookies
          (setq file (car files)
                files nil)
        (setq files (cdr files))))
    (cons file (nreverse cookies))))


;; For Firefox 3
;;
(defun jwz-lj-extract-sql-cookies (file domain)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point))
      (shell-command-on-region
       (point-min) (point-max)
       (concat "sqlite3"
               " -list -separator '\t'"
               " '" file "'"
               " 'SELECT host,isHttpOnly,path,expiry,isSecure,name,value" 
               "  FROM moz_cookies"
               "  WHERE host=\".www." domain "\""
               "  OR    host=\"." domain "\""
               "  ;'")
       t t))))


;; For Safari 3
;;
(defun jwz-lj-extract-xml-cookies (file match-domain)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point))
      (insert-file-contents file nil nil nil t)
      (goto-char (point-min))
      (search-forward "<dict>")
      (let ((result "")
            (start (point))
            end
            domain path name value)
        (while (search-forward "</dict>" nil t)
          (setq end (point))
          (goto-char start)
          (cond
           ((search-forward match-domain end t)  ; bail fast

            (goto-char start)
            (re-search-forward (concat "<key>Domain</key>[ \t\n\r]*"
                                       "<string>\\([^<>]+\\)</string>")
                               end)
            (setq domain (match-string 1))
            (goto-char start)
            (re-search-forward (concat "<key>Path</key>[ \t\n\r]*"
                                       "<string>\\([^<>]+\\)</string>")
                               end)
            (setq path (match-string 1))
            (goto-char start)
            (re-search-forward (concat "<key>Name</key>[ \t\n\r]*"
                                       "<string>\\([^<>]+\\)</string>")
                               end)
            (setq name (match-string 1))
            (goto-char start)
            (re-search-forward (concat "<key>Value</key>[ \t\n\r]*"
                                       "<string>\\([^<>]+\\)</string>")
                               end)
            (setq value (match-string 1))
            (if (string-match (concat "\\b" (regexp-quote match-domain) "$") 
                              domain)
                (setq result
                      (concat domain "\tTRUE\t" path "\tFALSE\t0\t"
                              name "\t" value
                              "\n" result)))))
          (goto-char end)
          (setq start end))
        (delete-region (point-min) (point-max))
        (insert result))))
  nil)


(defun jwz-lj-post (subject body
                    &optional security-level tags community
                              auto-format-p disallow-comments-p
                              current-mood current-music)
  "Post to LiveJournal.
Determines the user and hpassword from the Netscape/Mozilla cookies,
if they are not provided.
Signals an error if the post is unsuccessful.
Returns the post ID number if successful."

  (let* ((cookies (or (cdr (jwz-lj-get-cookies))
                      (error "no LJ cookies found")))
         (url (jwz-lj-make-url subject body jwz-lj-lj-user-name
                               security-level tags community
                               auto-format-p disallow-comments-p
                               current-mood current-music)))
    (jwz-lj-post-1 url cookies)))


(defun jwz-lj-mode ()
  (interactive)
  (html-mode)
  (use-local-map jwz-lj-mode-map)
  (setq mode-name "jwz-LJ")
  (setq major-mode 'jwz-lj-mode)
  (auto-save-mode auto-save-default)
  (run-hooks 'jwz-lj-mode-hook))

(defun jwz-lj ()
  "*Compose a post to LiveJournal."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*livejournal*"))
  (erase-buffer)
  (goto-char (point-min))
  (insert "Subject: \n"
          "Music: \n"
;         "Mood: \n"
          "Security: public\n"
          "Tags: \n"
;         "Community: \n"
          (cond (jwz-lj-fcc-file
                 (concat "FCC: " jwz-lj-fcc-file "\n"))
                (jwz-lj-bcc-address
                 (concat "BCC: " jwz-lj-bcc-address "\n"))
                (t "FCC: \nBCC: \n"))
          "\n")
  (goto-char (point-min))
  (end-of-line)
  (jwz-lj-mode)

  (cond ((and buffer-auto-save-file-name
              (file-exists-p buffer-auto-save-file-name)
              (yes-or-no-p (format "Recover auto save file %s? "
                                   buffer-auto-save-file-name)))
         (erase-buffer)
         (insert-file-contents buffer-auto-save-file-name nil)))
  nil)


(defun jwz-lj-validate ()
  "Validates the HTML in the current buffer via jwz-lj-validator."
  (if jwz-lj-validator
      (let ((ob (current-buffer))
            b err line)
        (save-excursion
          (unwind-protect
              (progn
                (setq b (get-buffer-create " *jwz-lj-validate*"))
                (set-buffer b)
                (erase-buffer)
                (insert-buffer ob)
                (shell-command-on-region (point-min) (point-max)
                                         jwz-lj-validator
                                         t t)
                (goto-char (point-min))
                (if (eobp)
                    nil
                  (goto-char (point-min))
                  (cond ((looking-at "^[^ \t\r\n]+:[ \t]+-?:?[ \t]+")
                         (delete-region (point) (match-end 0))))
                  (cond ((looking-at "^\\([0-9]+\\):[ \t]+")
                         (setq line (string-to-int (match-string 1)))
                         (delete-region (point) (match-end 0))))
                  (setq err (buffer-string))
                  ))
            (if b (kill-buffer b))))
        (if line (goto-line line))
        (if err (error err))))
  nil)

(defun jwz-lj-html-clean (string &optional unfold-lines-p show-lj-tags-p)
  ;; basically just replaces newlines with spaces.
  ;; if unfold-lines-p is true, turns newlines into spaces.
  ;; if show-lj-tags is true, then makes any <lj*> tags be visible.
  (let (b)
    (save-excursion
      (unwind-protect
          (progn
            (setq b (get-buffer-create " *jwz-lj-clean*"))
            (set-buffer b)
            (erase-buffer)
            (insert string)
            (let ((case-fold-search t))
              (if (re-search-forward "<PRE\b" nil t)
                  (error "you really don't want to be using PRE.")))

            (cond (unfold-lines-p
                   (goto-char (point-min))
                   (while (search-forward "\n" nil t)
                     (delete-char -1)
                     (unless (or (= (preceding-char) ?\ )
                                 (= (following-char) ?\ ))
                       (insert " ")))))
            (cond (show-lj-tags-p
                   (goto-char (point-min))
                   (let ((case-fold-search t))
                     (while (re-search-forward "<\\(/?lj[^>]*\\)+>" nil t)
                       (let ((tt (match-string 1))
                             (u nil)
                             (ins nil)
                             (e (match-end 0)))
                         (goto-char (match-beginning 0))

                         (cond ((looking-at "</?lj[ \t]*user=\"\\([^\"]+\\)\"")
                                (setq u (match-string 1)))
                               ((looking-at "<lj-pq\\b")
                                (setq ins "<P>")
                                (save-excursion
                                  (goto-char e)
                                  (search-forward "<" nil t)
                                  (forward-char -1)
                                  (insert "<UL>")))
                               ((looking-at "</lj-pq\\b") (setq ins "</UL>"))
                               ((looking-at "<lj-pi\\b")  (setq ins "<LI>"))
                               )

                         (delete-region (point) e)
                         (if ins (insert ins))
                         (cond (u
                                (insert
                                 "<A HREF=\"http://www.livejournal.com/"
                                 "userinfo.bml?user=" u "\">"
                                 "<IMG SRC=\"http://www.livejournal.com/"
                                 "img/userinfo.gif\" WIDTH=17 HEIGHT=17"
                                 " BORDER=0 ALIGN=absmiddle VSPACE=0 HSPACE=0>"
                                 "</A>"
                                 "<A HREF=\"http://www.livejournal.com"
                                 "/users/" u "\">" u "</A>"))
                               (tt
                                (insert "<B><U>&lt;" tt "&gt;</U></B>")))
                         )))))

            ;; compress spaces
            (cond (unfold-lines-p
                   (goto-char (point-min))
                   (while (re-search-forward "[ \t][ \t]+" nil t)
                     (replace-match " "))))

            (let ((case-fold-search t)
                  (clear-p nil))
              ;; put some para breaks in
              (goto-char (point-min))
              (insert "<P>")
              (if (looking-at "\\([ \t]*<P>\\)+")
                  (replace-match ""))

              (goto-char (point-max))
              (while (and (search-backward "<" nil t)
                          (looking-at "<P>[ \t]*\\'"))
                (replace-match ""))

              (goto-char (point-min))
              (if (re-search-forward   ;; any floaters?
            "<\\(TABLE\\|P\\|DIV\\|IMG\\)\\b[^<>]*\\bALIGN=\\(LEFT\\|RIGHT\\)"
                   nil t)
                  (setq clear-p t))

              (goto-char (point-max))
              (if clear-p (insert "<BR CLEAR=ALL>"))
              )

            (buffer-string))
        (if b (kill-buffer b))))))


(defun jwz-lj-submit ()
  "*Compose a post to LiveJournal."
  (interactive)
  (let ((case-fold-search t)
        (auto-p nil)
        (no-comments-p nil)
        subj music mood sec tags community fcc bcc body
        new-url)

    (jwz-lj-validate)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (search-forward "\n\n")
        (narrow-to-region (point-min) (point))

        (goto-char (point-min))
        (if (re-search-forward "^Subject:[ \t]*\\(.*\\)$" nil t)
            (setq subj (match-string 1)))

        (goto-char (point-min))
        (if (re-search-forward "^Music:[ \t]*\\(.*\\)$" nil t)
            (setq music (match-string 1)))

        (goto-char (point-min))
        (if (re-search-forward "^Mood:[ \t]*\\(.*\\)$" nil t)
            (setq mood (match-string 1)))

        (goto-char (point-min))
        (if (re-search-forward "^FCC:[ \t]*\\(.*\\)$" nil t)
            (setq fcc (match-string 1)))

        (goto-char (point-min))
        (if (re-search-forward "^BCC:[ \t]*\\(.*\\)$" nil t)
            (setq bcc (match-string 1)))

        (goto-char (point-min))
        (if (re-search-forward "^Security:[ \t]*\\(.*\\)$" nil t)
            (setq sec (match-string 1)))

        (goto-char (point-min))
        (if (re-search-forward "^Tags:[ \t]*\\(.*\\)$" nil t)
            (setq tags (match-string 1)))

        (goto-char (point-min))
        (if (re-search-forward "^Community:[ \t]*\\(.*\\)$" nil t)
            (setq community (match-string 1)))

        (goto-char (point-max))
        (widen)

        (setq body (buffer-substring (point) (point-max)))))

    ;; First post it, to get the URL.
    ;; Then FCC it, so we can include the URL in the mbox.
    
    (let ((id (jwz-lj-post
               subj
               (jwz-lj-html-clean body t nil)
               sec tags community auto-p no-comments-p mood music)))
      (or id (error "no ID for post"))
      (setq new-url (format "http://%s.livejournal.com/%s.html" 
                            jwz-lj-lj-user-name id)))

    (if (and fcc (not (equal fcc "")))
        (jwz-lj-do-fcc fcc
                       (jwz-lj-html-clean body nil t)
                       subj new-url))

    (if (and bcc (not (equal bcc "")))
        (jwz-lj-do-bcc bcc
                       (jwz-lj-html-clean body nil t)
                       subj new-url))
    )

  ;; If buffer has no file, mark it as unmodified and delete autosave.
  (cond ((or (not buffer-file-name)
             (not (buffer-modified-p)))
         (set-buffer-modified-p nil)
         (delete-auto-save-file-if-necessary t))
        ((or noninteractive
             (y-or-n-p (format "Save file %s? " buffer-file-name)))
         (save-buffer)))

  (kill-buffer (current-buffer)))


(defun jwz-lj-do-fcc (file html subj &optional url)
  (let (b)
    (save-excursion
      (unwind-protect
          (progn
            (setq b (get-buffer-create " *jwz-lj-fcc*"))
            (set-buffer b)
            (erase-buffer)
            (insert "From " (user-login-name) " -\n"
                    "From: " (user-full-name) " <" (user-mail-address) ">\n"
                    "Date: " (current-time-string) "\n"
                    "Subject: " (or subj "") "\n"
                    "MIME-Version: 1.0\n"
                    "Content-Type: text/html\n"
                    "X-Mozilla-Status: 0000\n"
                    (cond (url
                           (concat "X-URL: " url "\n"
                                   "\n"
                                   "<A HREF=\"" url "\">" url "</A><P>\n"))
                          (t ""))
                    "\n"
                    html
                    "\n\n")
            (write-region (point-min) (point-max) file t nil))
        (if b (kill-buffer b)))))
  nil)


(defun jwz-lj-do-bcc (to html subj &optional url)
  (save-excursion
    (message (format "Composing mail (%s)..." subj))
    (mail)
    (mail-to)      (insert to)
    (mail-subject) (insert subj)

    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n"))
    (delete-region (point) (point-max))

    (save-excursion
      (goto-char (match-beginning 0))
      (insert "\nMIME-Version: 1.0\n"
              "Content-Type: text/html\n")

      (goto-char (point-min))
      (search-forward (concat "\n" mail-header-separator "\n"))
      (delete-region (point) (point-max))
      (save-excursion
        (goto-char (match-beginning 0))
        (insert "\nMIME-Version: 1.0\n"
                "Content-Type: text/html\n")
        (if url (insert "X-URL: " url "\n"))
        (insert "\n"))

      (if url (insert "<A HREF=\"" url "\">" url "</A><P>\n"))
      (insert html "<P>\n\n")
      (message (format "Sending mail (%s)..." subj))
      (mail-send-and-exit nil)
      ))
  nil)


(defun jwz-lj-preview ()
  "Sends the body of the current post to Mozilla to view it."
  (interactive)

  (jwz-lj-validate)
  (let ((file "/tmp/ljtmp.html")
        body)
    (let (subject music tags community)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (search-forward "\n\n")
          (narrow-to-region (point-min) (point))

          (goto-char (point-min))
          (if (re-search-forward "^Subject:[ \t]*\\(.*\\)$" nil t)
              (setq subject (match-string 1)))

          (goto-char (point-min))
          (if (re-search-forward "^Music:[ \t]*\\(.*\\)$" nil t)
              (setq music (match-string 1)))

          (goto-char (point-min))
          (if (re-search-forward "^Tags:[ \t]*\\(.*\\)$" nil t)
              (setq tags (match-string 1)))

          (goto-char (point-min))
          (if (re-search-forward "^Community:[ \t]*\\(.*\\)$" nil t)
              (setq community (match-string 1)))

          (goto-char (point-max))
          (widen)

          (setq body (buffer-substring (point) (point-max)))
          ))
      (setq subject (jwz-lj-html-clean (or subject "") nil t))
      (if (string-match "\\`<p>[ \t\n]*" subject)
          (setq subject (substring subject (match-end 0))))
      (if (string-match "[ \t\n]*<p>[ \t\n]*\\'" subject)
          (setq subject (substring subject 0 (match-beginning 0))))

      (setq body
            (concat "<TITLE>LiveJournal Preview</TITLE>\n"
                    "<body bgcolor=\"#C0C0C0\">\n"
                    "<P><B>Subject: " subject "<BR>"
                    "Music: " (or music "") "</B><BR>"
                    "<B>Tags: " (if tags (jwz-lj-tags-to-html tags) "")
                    "</B><BR>"
                    (if community
                        (concat "<B>Community: " community "</B><BR>")
                      "")
                    "<P>"
                    (jwz-lj-html-clean body t t))))

    (let (b)
      (save-excursion
        (unwind-protect
            (progn
              (setq b (get-buffer-create " *jwz-lj-preview*"))
              (set-buffer b)
              (erase-buffer)
              (insert body)
              (write-region (point-min) (point-max) file))
          (if b (kill-buffer b)))))

    (browse-url file)))

(defun jwz-lj-tags-to-html (tags)
  (let (b)
    (save-excursion
      (unwind-protect
          (progn
            (setq b (get-buffer-create " *jwz-lj-tags*"))
            (set-buffer b)
            (erase-buffer)
            (insert tags)
            (goto-char (point-min))
            (while (not (eobp))
              (skip-chars-forward " \t\r\n")
              (let ((p (point))
                    tag)
                (skip-chars-forward "^,\r\n")
                (setq tag (buffer-substring p (point)))
                (save-excursion
                  (goto-char p)
                  (insert "<A HREF=\"http://" jwz-lj-lj-user-name
                          ".livejournal.com/tag/" tag "\">"))
                (insert "</A>")
                (skip-chars-forward " \t\r\n,")))
            (buffer-string))
        (if b (kill-buffer b))))))

(defun jwz-lj-cut ()
  (interactive)
  (let* ((pct (round (/ (* 100.0 (point)) (point-max))))
         (txt (format " --More--(%2d%%) " pct)))
    (insert "<P><LJ-CUT text=\"" txt "\">\n"))
  nil)

(defun jwz-lj-youtube (url)
  (interactive "sVideo URL: ")
  (save-restriction
    (narrow-to-region (point) (point))
    (insert url)
    (goto-char (point-min))
    (let ((ct "application/x-shockwave-flash")
          (autoplay "TRUE")
          w h)
      (cond ((looking-at "http://[^.]*\\.?youtube\\.com/")
             (cond ((re-search-forward "\\(/watch\\)?\\?v=" nil t)
                    (delete-region (match-beginning 0) (match-end 0))
                    (insert "/v/")))
             (cond ((re-search-forward "&.*$" nil t)
                    (delete-region (match-beginning 0) (match-end 0))))
             (goto-char (point-min))
             (or (looking-at "^http://www\\.youtube\\.com/v/")
                 (error "bogus youtube url: %s" (buffer-string)))
             (goto-char (point-max))
             (insert "&amp;color1=0"
                     "&amp;color2=0x004400"
                     "&amp;fs=1"
                     "&amp;showsearch=0"
                     "&amp;showinfo=0"
                     "&amp;iv_load_policy=3"
                     "&amp;ap=%2526fmt=18")
             (setq w 425
                   h 344))
            ((looking-at "http://video\\.google\\.com/")
             (cond ((re-search-forward "/videoplay\\?" nil t)
                    (delete-region (match-beginning 0) (match-end 0))
                    (insert "/googleplayer.swf?")))
             (goto-char (point-min))
             (or (looking-at
                  "^http://video\\.google\\.com/googleplayer\\.swf\\?docId=")
                 (error "bogus google url: %s" (buffer-string)))
             (goto-char (point-min))
             (cond ((re-search-forward "&.*$" nil t)
                    (delete-region (match-beginning 0) (match-end 0))))
             (setq w 420
                   h 352))
            ((looking-at "[^ \t\r\n\"\']+\\.\\(mov\\|mp4\\|mpe?g\\)$")
             (setq ct "video/quicktime"
                   autoplay "FALSE"
                   w 320
                   h 280))
            (t (error "not a Youtube, Google, or MPEG URL: %s" url)))
      (setq url (buffer-substring (point-min) (point-max)))
      (delete-region (point-min) (point-max))
      (insert "<EMBED\n"
              " SRC=\"" url "\"\n"
              " TYPE=\"" ct "\"\n"
              " WIDTH=" (int-to-string w) " HEIGHT=" (int-to-string h) "\n"
              " AUTOPLAY=" autoplay
              " SCALE=ASPECT"
              " ALLOWFULLSCREEN=TRUE\n"
              " BGCOLOR=\"#002200\""
              " BORDER=1>"
              "</EMBED>\n"))))

(fset 'lj 'jwz-lj)
(fset 'livejournal 'jwz-lj)
(fset 'ljpreview 'jwz-lj-preview)
(fset 'entify 'jwz-lj-entify)
(fset 'ljcut 'jwz-lj-cut)

(provide 'jwz-lj)
