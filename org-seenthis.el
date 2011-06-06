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

(defvar org-seenthis-user nil
  "If non-nil, will be used as your SeenThis username.")

(defvar org-seenthis-password nil
  "If non-nil, will be used as your SeenThis password.")

(defun org-seenthis-get-auth-info ()
  (unless org-seenthis-user
    (setq org-seenthis-user (read-string "SeenThis username: ")))
  (unless org-seenthis-password
    (setq org-seenthis-password (read-passwd "SeenThis password: "))))


(defun org-seenthis-request (method url content-type &optional params)
  "Makes a request to `url' synchronously, notifying `callback'
when complete. Optionally accepts additional POST `params' as a
list of (key . value) conses."
  (org-seenthis-get-auth-info)
  (let ((url-request-data params)
        (url-request-extra-headers
         `(("Content-Type" . content-type)
           ("Authorization" . ,(concat "Basic "
                                       (base64-encode-string
                                        (concat org-seenthis-user ":"
                                                org-seenthis-password))))))
          (url-max-redirecton -1)
          (url-request-method method))
      (url-retrieve-synchronously url)))

(defun org-seenthis-walk (node)
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
	   (mapc 'org-seenthis-walk (xml-node-children node))))))

(defun org-seenthis-parse-entries ()
  "Parse an xml buffer for seenthis entries"
  (goto-char (point-min))
  (when (search-forward "<feed" nil t)
    (forward-char -5)
    (let (id title published updated link summary result)
      (org-seenthis-walk (car (xml-parse-region (point) (point-max))))
      (nreverse result))))

(defun org-seenthis-get-entries (user &optional index)
  "Get the entries for the specified user"
  (let ((url (format "https://seenthis.net/api/people/%s/messages/%s" user index)))
   (with-current-buffer
       (org-seenthis-request "GET" url "application/x-www-form-urlencoded") 
       (org-seenthis-parse-entries))))

(defun org-seenthis-convert-summary (summary)
  (setq summary (replace-regexp-in-string "#" "" summary))
  (setq summary (replace-regexp-in-string "❝" "#+BEGIN_QUOTE\n" summary))
  (replace-regexp-in-string "❞" "\n#+END_QUOTE" summary))

(defun org-seenthis-extract-tag (summary &optional pos)
    (when (string-match "\\([#@][-'_[:word:]]+\\)" summary pos)
      (setq tags (cons (match-string 1 summary) tags))
      (org-seenthis-extract-tag summary (match-end 1))))

(defun org-seenthis-extract-all-tags (summary)
  (let (tags)
    (org-seenthis-extract-tag summary)
    (concat ":" (mapconcat 'identity tags ":") ":")))

(defun org-seenthis-insert-entry (entry)
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
	 (concat (org-seenthis-convert-summary summary) "\n\n"))	
	(fill-region beg (point)))
      (message "%s" (org-seenthis-extract-all-tags summary))
      (org-set-tags-to  (org-seenthis-extract-all-tags summary))
      )))

(defun org-seenthis-insert-entries (&optional user index)
  (interactive)
  (let ((index (or index 0))
	(user (or user org-seenthis-user)))
    (mapconcat 'org-seenthis-insert-entry (org-seenthis-get-entries user index) "\n")))

(defun org-seenthis-insert-my-entries (&optional index)
  (interactive)
  (org-seenthis-insert-entries org-seenthis-user index))


(defun org-seenthis-create-message-clean-body (body)
  "Cleans the text of an entry before publishing"					   
  (let ((case-fold-search t))
    (setq body (replace-regexp-in-string "^[ \\t]+" "" body))
    (message "%s" body)
    (setq body (replace-regexp-in-string "^#\\+BEGIN_QUOTE\n" "❝" body))
    (message "%s" body)
    (setq body (replace-regexp-in-string "\n#\\+END_QUOTE" "❞" body))    
    (setq body (replace-regexp-in-string "\\[\\[" "" body))
    (replace-regexp-in-string "\\]\\]" "" body)))

(defun org-seenthis-create-message-from-subtree ()
  "Generate an atom xml string from the current subtree to be published via the SeenThis API."
  (interactive)
  (let ((title (org-get-heading t))
	(body (org-get-entry))
	(tags (org-get-tags-at)))
    (setq body (org-seenthis-create-message-clean-body body))
    (setq tags (mapconcat (function (lambda (s) (concat "#" s))) tags " "))
    (concat 
     "<?xml version='1.0' encoding='UTF-8'?>
<entry xmlns='http://www.w3.org/2005/Atom' xmlns:thr='http://purl.org/syndication/thread/1.0'>"
title
"\n\n"
body
"\n\n"
tags
"</entry>")))

(defun org-seenthis-post-message-from-subtree ()
  "Post a new entry to SeenThis based on the content of the current subtree."
  (interactive)

  )
  

(provide 'org-seenthis)
