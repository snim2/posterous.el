;; posterous.el --- Emacs integration for posterous.com

(defvar posterous-version "0.1")

;; Currently only posts to a blog.

;; Maintainer: Sarah Mount snim2@snim2.org
;; Author: Sarah Mount snim2@snim2.org
;; Version: 0.1
;; Created: 14 May 2010
;; Keywords: posterous blog 

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.


(require 'url)
(require 'xml)


(defcustom posterous-email nil
  "Email address or username on posterous.com."
  :type 'string
  :group 'posterous)


(defcustom posterous-password nil
  "User password for posterous.com."
  :type 'string
  :group 'posterous)


(defcustom posterous-default-siteid nil
  "Siteid for default posterous blog to post to. If this variable
   is null the user's default site will be used."
  :type 'string
  :group 'posterous)


(defcustom posterous-suppress-autopost nil
  "Do NOT automatically send posterous posts to social networks."
  :type 'boolean
  :group 'posterous)


(defconst posterous-base-url "http://posterous.com/api/"
  "Base posterous URL.")


(defconst posterous-getsites-url (concat posterous-base-url "getsites")
  "Posterous URL for retrieving a list of posterous blogs.")


(defconst posterous-post-url (concat posterous-base-url "newpost")
  "Posterous URL for posting new content.")


(defconst posterous-update-url (concat posterous-base-url "updatepost")
  "Posterous URL for updating an existing post.")


(defconst posterous-comment-url (concat posterous-base-url "newcomment")
  "Posterous URL for posting a new comment on an existing post.")


(defconst posterous-result-buffer "*posterous*"
  "Buffer name for copying responses from posterous.com.")


(defun posterous-getsites ()
  "Get a list of sites owned by this user."
  (interactive)
  (let ((url-request-method "GET")
		(url-request-extra-headers
		 `(("Authorization" . ,(base64-encode-string 
								(format "%s:%s" posterous-email posterous-password))))))
	(save-excursion
	  (if (null posterous-email)
		  (setq posterous-email (read-from-minibuffer "Email address: ")))
	  (if (null posterous-password)
		  (setq posterous-password (read-from-minibuffer "Posterous password: ")))
	  (url-retrieve posterous-getsites-url 'posterous-getsites-callback))))


(defun posterous-post-base (post private media title tags autopost)
  "Generic post to posterous function. Called by posterous-post-generic."
	(let ((url-request-method "POST")
		  (url-request-extra-headers
		   `(("Content-Type" . "application/x-www-form-urlencoded")
			 ("Authorization" . ,(base64-encode-string 
								  (format "%s:%s" posterous-email posterous-password)))))
		  (url-request-data
		   (concat "site_id="   (url-hexify-string posterous-default-siteid) ;; site_id
				   "&title="    (url-hexify-string title)                    ;; title
				   "&body="     (url-hexify-string post)                     ;; body
				   "&autopost=" (url-hexify-string autopost)                 ;; autopost
				   "&private="  (url-hexify-string private)                  ;; private
				   "&tags="     (url-hexify-string tags)                     ;; tags
				   "&media="   (url-hexify-string media)                     ;; media
				   )))
		(url-retrieve posterous-post-url 'posterous-post-callback)))


(defun posterous-post-generic (start end private media)
  "Generic post function called by all interactive functions."
  (let (post title tags autopost)
	(setq post (buffer-substring start end))
	(setq title (read-from-minibuffer "Post title: "))
	(setq tags (read-from-minibuffer "Tags (comma-separated): "))
	(if (null posterous-email)
		(setq posterous-email (read-from-minibuffer "Email address: ")))
	(if (null posterous-password)
		(setq posterous-password (read-from-minibuffer "Posterous password: ")))
	(if posterous-suppress-autopost
		(setq autopost "1")
	  (setq autopost "0"))
	(posterous-post-base post private media title tags autopost)))


(defun posterous-region (start end)
  "Post a region of text to posterous."
  (interactive "r")
  (save-excursion
	(posterous-post-generic start end "0" "")))


(defun posterous-region-private (start end)
  "Post a region of text to posterous as a private post."
  (interactive "r")
  (save-excursion
	(posterous-post-generic start end "1" "")))


(defun posterous-buffer ()
  "Post a whole buffer to posterous."
  (interactive)
  (save-excursion
	(posterous-region (point-min) (point-max) "0" "")))


(defun posterous-buffer-private ()
  "Post a whole buffer to posterous as a private post."
  (interactive)
  (save-excursion
	(posterous-region (point-min) (point-max) "1" "emacs,posterous" "")))


(defun posterous-getsites-callback (response)
   "Deal with an HTTP response from posterous.com.
    Remove the HTTP header, parse the XML returned from posterous.
   "
   (rename-buffer posterous-result-buffer)
   ;;; Delete HTTP header
   (goto-char (point-min))
   (search-forward-regexp "\n\n")
   (delete-region (point-min) (point))
   (set-buffer-modified-p nil)
   (switch-to-buffer (current-buffer)))
   ;;; Parse XML returned by posterous.com
;   (let* ((root (xml-parse-region (point-min) (point-max)))
;		  (rsp (car root)))))


 (defun posterous-post-callback (response)
   "Deal with an HTTP response from posterous.com.
    Remove the HTTP header, parse the XML returned from posterous.
   "
   (rename-buffer posterous-result-buffer)
   ;;; Delete HTTP header
   (goto-char (point-min))
   (search-forward-regexp "\n\n")
   (delete-region (point-min) (point))
   (set-buffer-modified-p nil)
   (switch-to-buffer (current-buffer))
   ;;; Parse XML returned by posterous.com
   (let* ((root (xml-parse-region (point-min) (point-max)))
		  (rsp (car root))
		  (attrs (xml-node-attributes rsp))
		  (stat (cdr (assq 'stat attrs))))
	 (cond ((null stat)
			(message "No status returned from posterous.com. Unknown error."))
		   ;;; Posting was successful.
		   ((string-equal stat "ok")
			(let* ((post (car (xml-get-children rsp 'post)))
				   (id (car (xml-get-children post 'id)))
				   (text-id (car (xml-node-children id)))
				   (url (car (xml-get-children post 'url)))
				   (text-url (car (xml-node-children url))))
			  (message "New post at has id %s at: %s"  text-id text-url)))
		   ;;; Posting failed.
		   ((string-equal stat "fail")
			(let* ((err (car (xml-get-children rsp 'err)))
				   (err-attrs (xml-node-attributes err))
				   (msg (cdr (assq 'msg err-attrs))))
			  (message "Posting to posterous failed: %s." msg)))))
   (kill-buffer (current-buffer)))


(provide 'posterous)

;; posterous.el end.