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

;; Part of this code come from org-ghi.el by Puneeth Chaganti :
;; https://github.com/punchagan/org-ghi

;; Part of this code come from atom-api.el by Erik Hetzner :
;; https://launchpad.net/atompub.el


;;; Commentary:

;;; For installation and usage, see README.org

;;; Code:

(require 'xml)

(defvar org-seenthis-user nil
  "If non-nil, will be used as your SeenThis username.")

(defvar org-seenthis-password nil
  "If non-nil, will be used as your SeenThis password.")

(defun org-seenthis-get-auth-info ()
  "Get SeenThis authentication informations from variables or prompt."
  (unless org-seenthis-user
    (setq org-seenthis-user (read-string "SeenThis username: ")))
  (unless org-seenthis-password
    (setq org-seenthis-password (read-passwd "SeenThis password: "))))


(defun org-seenthis-request (method url content-type &optional params)
  "Makes a request to `url' synchronously, with the `method' and
`content-type' specified. Optionally accepts additional POST
`params' as a list of (key . value) conses."
  (org-seenthis-get-auth-info)
  (let ((url-request-data params)
        (url-request-extra-headers
         `(("Content-Type" . ,content-type)
           ("Authorization" . ,(concat "Basic "
                                       (base64-encode-string
                                        (concat org-seenthis-user ":"
                                                org-seenthis-password))))))
          (url-max-redirecton -1)
          (url-request-method method))
      (url-retrieve-synchronously url)))

(defun org-seenthis-encode-string-to-utf (string)
  "Encode `string' to utf-8 encoding."
  (let ((temp-file-name (make-temp-file "atom-api"))
 	(coding-system-for-write 'utf-8)
 	(coding-system-for-read 'binary))
     (with-temp-file temp-file-name
       (insert string))
     (with-temp-buffer
       (insert-file-contents-literally temp-file-name)
       (buffer-string))))

(defun org-seenthis-walk (node)
  "Walks an xml parsed structure to extract values."
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

(defun org-seenthis-parse (start-string)
  "Parse an xml buffer for seenthis entries. `start-string'
indicates the beginning of the first tag for parsing."
  (goto-char (point-min))
  (when (search-forward start-string nil t)
    (forward-char (- (length start-string)))
    (let (id title published updated link summary result)
      (org-seenthis-walk (car (xml-parse-region (point) (point-max))))
      (nreverse result))))

(defun org-seenthis-get-entries-from-user-feed (user &optional index)
  "Get the entries for the specified `user'."
  (let ((url (format "https://seenthis.net/api/people/%s/messages/%s" user index)))
   (with-current-buffer
       (org-seenthis-request "GET" url "application/x-www-form-urlencoded") 
       (org-seenthis-parse "<feed"))))

(defun org-seenthis-convert-summary (summary)
  "Convert SeenThis markup into org-mode markup."
  (setq summary (replace-regexp-in-string "#" "" summary))
  (setq summary (replace-regexp-in-string "❝" "#+BEGIN_QUOTE\n" summary))
  (replace-regexp-in-string "❞" "\n#+END_QUOTE" summary))

(defun org-seenthis-extract-tags (summary &optional pos)
  "Extract SeenThis tags from the given `summary' optionnaly
starting at position `pos'."
  (setq pos (if pos pos 0))
  (when (string-match "\\([#@][-'_[:word:]]+\\)" summary pos)
    (let ((tag (match-string 1 summary)))
      (setq tag (replace-regexp-in-string "#" "" tag))
      (setq tags (cons tag tags))
      (org-seenthis-extract-tags summary (match-end 1)))))

(defun org-seenthis-extract-all-tags (summary)
  "Extract all SeenThis tags from `summary'."
  (let (tags (tag nil))
    (setq tag (org-seenthis-extract-tag summary))
    (while tag
    (concat ":" (mapconcat 'identity tags ":") ":"))))

(defun org-seenthis-insert-entry (entry)
  "Inserts SeenThis parsed `entry' as an org subtree."
  (let (tags
	(id (pop entry))
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
      (org-set-property "seenthis-url" link)
      (let ((beg (point)))
	(insert-before-markers 
	 (concat (org-seenthis-convert-summary summary) "\n\n"))	
	(fill-region beg (point))
	(indent-region beg (point)))
      (org-seenthis-extract-tags summary)
      (message "%s" tags)
      (org-set-tags-to (concat ":" (mapconcat 'identity tags ":") ":"))
      (org-back-to-heading)
      (org-cycle "FOLDED"))))

(defun org-seenthis-get-entries (&optional user index)
  "Get SeenThis entries from `user', optionaly starting at
`index', and insert them as a set of folded org subtrees.

If `user' is not specified, get entries from
`org-seenthis-user'."
  (interactive "sSeenThis user (default to current user): \nP")
  (let ((index (if index index 0))
	(user (if (string= user "") org-seenthis-user user)))
    (mapconcat 'org-seenthis-insert-entry (org-seenthis-get-entries-from-user-feed user index) "\n")))


(defun org-seenthis-clean-body (body)
  "Cleans the text of an entry before publishing"					   
  (let ((case-fold-search t))
    (setq body (replace-regexp-in-string "^[ \\t]+" "" body))
    (setq body (replace-regexp-in-string ":PROPERTIES:\\(.\\|\n\\)+?:END:" "" body))
    (setq body (replace-regexp-in-string "^#\\+BEGIN_QUOTE\n" "❝" body))
    (setq body (replace-regexp-in-string "\n#\\+END_QUOTE" "❞" body))    
    (setq body (replace-regexp-in-string "\\[\\[" "" body))
    (replace-regexp-in-string "\\]\\]" "" body)))

(defun org-seenthis-create-entry-from-subtree ()
  "Generate an atom xml string from the current subtree to be published via the SeenThis API."
  (interactive)
  (let ((title (org-get-heading t))
	(body (org-get-entry))
	(tags (org-get-tags-at))
	(id (org-entry-get (point) "seenthis-id")))
    (setq body (org-seenthis-clean-body body))
    (setq tags (delete "ATTACH" tags))
    (setq tags (mapconcat (function (lambda (s) (concat "#" s))) tags " "))
    (setq id (if id (format "<id>%s</id>" id) ""))
    (org-seenthis-encode-string-to-utf 
     (format "<?xml version='1.0' encoding='UTF-8'?>
<entry xmlns='http://www.w3.org/2005/Atom' xmlns:thr='http://purl.org/syndication/thread/1.0'>
%s
<summary><![CDATA[%s

%s

%s]]></summary>
</entry>" id title body tags))))

(defun org-seenthis-post-entry ()
  "Post a new entry to SeenThis based on the content of the current subtree."
  (interactive)
  (let (result-buffer result-ok id title published updated link
		      (xml (org-seenthis-create-entry-from-subtree))
		      (is-edit (org-entry-get (point) "seenthis-id")))
    (setq result-buffer 
	  (org-seenthis-request 
	   (if is-edit "PUT" "POST")
	   "https://seenthis.net/api/messages" 
	   "application/atom+xml;type=entry;charset=utf-8" xml))
    (save-excursion
      (set-buffer result-buffer)
      (goto-char (point-min))
      (setq result-ok (search-forward "<id>message:" nil t)))
    (if result-ok
	(progn
	  (save-excursion
	    (set-buffer result-buffer)
	    (setq result (car (org-seenthis-parse "<entry"))))
	  (setq id (pop result)
		title (pop result)
		published (pop result)
		updated (pop result)
		link (pop result))
	  (org-set-property "seenthis-id" id)
	  (org-set-property "seenthis-published" published)
	  (org-set-property "seenthis-updated" updated)
	  (org-set-property "seenthis-url" link)
	  (message "%s" "Entry published succesfully !"))
      (progn 
	(pop-to-buffer result-buffer)
	(error "Entry has not been published !")))
    ))


(provide 'org-seenthis)

;;; org-seenthis.el ends here
