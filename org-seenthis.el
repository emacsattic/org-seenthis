;; org-seenthis.el --- Provides Seenthis.net integration for Org-mode.
     
;; Author: Julien Barnier <julien@nozav.org>
;; Created: 27 May 2011
;; Keywords: seenthis orgmode

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;; Part of this codes come from org-ghi.el by Puneeth Chaganti :
;; https://github.com/punchagan/org-ghi

;;; Commentary:


;;; Code:

(require 'xml)

(defvar seenthis-user nil
  "If non-nil, will be used as your SeenThis username.")

(defvar seenthis-password nil
  "If non-nil, will be used as your SeenThis password.")

(defun seenthis-get-auth-info ()
  (unless seenthis-user
    (setq seenthis-user (read-string "SeenThis username: ")))
  (unless seenthis-password
    (setq seenthis-password (read-passwd "SeenThis password: "))))


(defun seenthis-request (method url &optional params)
  "Makes a request to `url' synchronously, notifying `callback'
when complete. Optionally accepts additional POST `params' as a
list of (key . value) conses."
  (seenthis-get-auth-info)
  (let ((url-request-data params)
        (url-request-extra-headers
         `(("Content-Type" . "application/x-www-form-urlencoded")
           ("Authorization" . ,(concat "Basic "
                                       (base64-encode-string
                                        (concat seenthis-user ":"
                                                seenthis-password))))))
          (url-max-redirecton -1)
          (url-request-method method))
      (url-retrieve-synchronously url)))


(defun seenthis-walk (node)
  (when (listp node)
    (cond ((string= "id" (xml-node-name node))
	   (setq id (first (xml-node-children node))))
	  ((string= "title" (xml-node-name node))
	   (setq title (first (xml-node-children node))))
	  ((string= "published" (xml-node-name node))
	   (setq published (first (xml-node-children node))))
	  ((string= "updated" (xml-node-name node))
	   (setq updated (first (xml-node-children node))))
	  ((string= "link" (xml-node-name node))
	   (unless (string= (xml-get-attribute node 'rel) "edit")
	     (setq link (xml-get-attribute node 'href))))
	  ((string= "summary" (xml-node-name node))
	   (setq summary (first (xml-node-children node)))
	   (setq result (cons (list 
			       (if (and (boundp 'id) id) id "")
			       (if (and (boundp 'title) title) title "")
			       (if (and (boundp 'published) published) published "")
			       (if (and (boundp 'updated) updated) updated "")
			       (if (and (boundp 'link) link) link "")
			       (if (and (boundp 'summary) summary) summary "")) result)))
	  (t
	   (mapc 'seenthis-walk (xml-node-children node))))))

(defun seenthis-parse-entries ()
  "Parse an xml buffer for seenthis entries"
  (goto-char (point-min))
  (when (search-forward "<feed" nil t)
    (forward-char -5)
    (let (id title published updated link summary result)
      (seenthis-walk (car (xml-parse-region (point) (point-max))))
      (nreverse result))))

(defun seenthis-get-entries (user &optional index)
  "Get the entries for the specified user"
  (let ((url (format "https://seenthis.net/api/people/%s/messages/%s" user index)))
   (with-current-buffer
       (seenthis-request "GET" url) 
       (seenthis-parse-entries))))

(defun seenthis-convert-summary (summary)
  (setq summary (replace-regexp-in-string "#" "" summary))
  (setq summary (replace-regexp-in-string "❝" "#+BEGIN_QUOTE\n" summary))
  (replace-regexp-in-string "❞" "\n#+END_QUOTE" summary))

(defun seenthis-extract-tag (summary &optional pos)
    (when (string-match "\\([#@][-'_[:word:]]+\\)" summary pos)
      (setq tags (cons (match-string 1 summary) tags))
      (seenthis-extract-tag summary (match-end 1))))

(defun seenthis-extract-all-tags (summary)
  (let (tags)
    (seenthis-extract-tag summary)
    (concat ":" (mapconcat 'identity tags ":") ":")))

(defun seenthis-insert-entry (entry)
  (let ((id (pop entry))
	(title (pop entry))
	(published (pop entry))
	(updated (pop entry))
	(link (pop entry))
	(summary (decode-coding-string (pop entry) 'utf-8)))
    (save-excursion
      (org-insert-heading-after-current)
      (insert (concat title "\n\n"))
      (org-set-property "seenthis-id" id)
      (org-set-property "seenthis-published" published)
      (org-set-property "seenthis-updated" updated)
      (org-set-property "seenthis-link" link)
      (let ((beg (point)))
	(insert-before-markers 
	 (concat (seenthis-convert-summary summary) "\n\n"))	
	(fill-region beg (point)))
      (message "%s" (seenthis-extract-all-tags summary))
      (org-set-tags-to  (seenthis-extract-all-tags summary))
      )))

(defun seenthis-insert-entries (&optional user index)
  (interactive)
  (let ((index (or index 0))
	(user (or user seenthis-user)))
    (mapconcat 'seenthis-insert-entry (seenthis-get-entries user index) "\n")))

(defun seenthis-insert-my-entries (&optional index)
  (interactive)
  (seenthis-insert-entries seenthis-user index))


(defun seenthis-create-message-clean-body (body)
  ""					   
  (let ((case-fold-search t))
    (setq body (replace-regexp-in-string "^[ \\t]+" "" body))
    (message "%s" body)
    (setq body (replace-regexp-in-string "^#\\+BEGIN_QUOTE\n" "❝" body))
    (message "%s" body)
    (setq body (replace-regexp-in-string "\n#\\+END_QUOTE" "❞" body))    
    (setq body (replace-regexp-in-string "\\[\\[" "" body))
    (replace-regexp-in-string "\\]\\]" "" body)))

(defun seenthis-create-message-from-subtree ()
  (interactive)
  (let ((title (org-get-heading t))
	(body (org-get-entry))
	(tags (org-get-tags-at)))
    (setq body (seenthis-create-message-clean-body body))
    (setq tags (mapconcat (function (lambda (s) (concat "#" s))) tags " "))
    (message "%s" (concat title "\n\n" body "\n\n" tags))))


(provide 'org-seenthis)
