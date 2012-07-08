;;; muse-markdown.el --- support markdown syntax

;; Copyright (C) 2006 Your Name <email@address.com>

;; Author: Your Name
;; Date: Thu 16-Mar-2006

;; This file is not part of Emacs Muse or GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs Muse; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file changes the markup that Muse uses so that it supports
;; some of the markdown syntax as defined by
;; http://daringfireball.net/projects/markdown/syntax.

;;; Contributors:

;;; Code:

(require 'muse)
(require 'assoc)                        ; use the `aput' function

(defvar muse-markdown-heading-regexp "^#\\s-+"
  "Regexp for markdown-style headings.")

;; Change rule 1700 of `muse-publish-markup-regexps' to use our regexp
(eval-after-load "muse-publish"
    '(progn
       (aput 'muse-publish-markup-regexps 1700
             muse-markdown-heading-regexp)))

;; Change some values in muse-regexps.el
(setq muse-explicit-link-regexp
      "\\[\\([^][\t\n]+\\)\\](\\([^][\n]+\\))")

;; Redefine `muse-get-link' and `muse-get-link-desc' to work with
;; markdown links.  In this case, description is specified first, then
;; the link.  For Muse's native markup, it is the opposite.

(defun muse-get-link (&optional target)
  "Based on the match data, retrieve the link.
Use TARGET to get the string, if it is specified."
  (muse-match-string-no-properties 2 target))

(defun muse-get-link-desc (&optional target)
  "Based on the match data, retrieve the link description.
Use TARGET to get the string, if it is specified."
  (match-match-string-no-properties 1 target))

;; Change muse-make-link function
(eval-after-load "muse-mode"
  '(progn
     (defun muse-make-link (link &optional desc)
       "Return a link to LINK with DESC as the description."
       (when (string-match muse-explicit-link-regexp link)
         (unless desc (setq desc (muse-get-link-desc link)))
         (setq link (muse-get-link link)))
       (if (and desc
                link
                (not (string= desc ""))
           (not (string= link desc)))
           (concat "[" (muse-link-escape desc) "]("
                   (muse-link-escape link) ")")
         ;; does markdown do explicit links without descriptions?
         ;; here's an example that would work if the syntax for that
         ;; was (link)
         (concat "(" (muse-link-escape link) ")")))))

(provide 'muse-markdown)

;;; muse-markdown.el ends here
