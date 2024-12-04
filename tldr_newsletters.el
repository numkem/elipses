;;; tldr-newsletters.el --- Display TLDR newsletters -*- lexical-binding: t -*-

;; Author: numkem
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (json "1.5"))
;; Keywords: mail, news
;; URL: https://github.com/numkem/elipses/tldr_newsletters.el

;;; Commentary:
;; Display TLDR newsletters with a date and subject fields
;; Once selected, creates a new buffer with all the content of the email in it rendered in html

;;; Code:
(require 'json)
(require 'url)

(defgroup tldr-newsletters nil
  "Settings for tldr-newsletters."
  :group 'mail)

(defcustom tldr-newsletters-opentrashmail-base-url "http://trashmail.svc.numkem.int"
  "URL for the TLDR newsletters API."
  :type 'string
  :group 'tldr-newsletters)

(defconst tldr-newsletters-opentrashmail-all-mail-endpoint "/json/")
(defconst tldr-newsletters-opentrashmail-delete-endpoint "/api/delete/")

(defcustom tldr-newsletters-opentrashmail-email-address "tldr@news.numkem.org"
  "Email address that contains all the newsletters in OpenTrashMail"
  :type 'string
  :group 'tldr-newsletters)

(defcustom tldr-newsletters-buffer-name "*TLDR Newsletters*"
  "Name of the buffer for displaying newsletter details."
  :type 'string
  :group 'tldr-newsletters)

(defvar tldr-newsletters-mode-map (make-sparse-keymap)
  "Keymap for tldr-newsletter-mode.")

(defun tldr-newsletters-opentrashmail--api-url ()
  (format "%s%s%s" tldr-newsletters-opentrashmail-base-url tldr-newsletters-opentrashmail-all-mail-endpoint tldr-newsletters-opentrashmail-email-address))

(defun tldr-newsletters-opentrashmail--delete-url (email-id)
  (format "%s%s%s/%s" tldr-newsletters-opentrashmail-base-url tldr-newsletters-opentrashmail-delete-endpoint tldr-newsletters-opentrashmail-email-address email-id))

(defvar tldr-newsletters-data nil
  "Holds the parsed newsletter JSON data.")

(defclass tldr-newsletters-email ()
  ((id :initarg :id :accessor tldr-email-id)
   (timestamp :initarg :timestamp :accessor tldr-email-timestamp)
   (from :initarg :from :accessor tldr-email-from)
   (subject :initarg :subject :accessor tldr-email-subject)
   (body :initarg :body :accessor tldr-email-body)))

(defun tldr-newsletters--api-callback (_status)
  "Handle the API response with STATUS."
  (goto-char (point-min))
  (re-search-forward "^$")
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string)
        (json-response (buffer-substring (point) (point-max))))
    (kill-buffer)
    (setq tldr-newsletters-data (tldr-newsletters-parse-json (json-read-from-string json-response)))))

(defun tldr-newsletters-menu ()
  (let* ((choices (mapcar (lambda (email)
                            (cons (format "%s: %s"
                                          (tldr-email-timestamp email)
                                          (tldr-email-subject email))
                                  (tldr-email-id email)))
                          (hash-table-values tldr-newsletters-data)))
         (selection (assoc (completing-read "Pick a date: " choices) choices))
         (selection-id (cdr selection)))
    (gethash selection-id tldr-newsletters-data)))

(defun tldr-newsletters-fetch ()
  "Fetch TLDR newsletters from the API."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "application/json"))))
    (url-retrieve (tldr-newsletters-opentrashmail--api-url) #'tldr-newsletters--api-callback nil t t)))

(defun tldr-newsletters-parse-json (json-hash)
  "Reads the JSON hash-table and converts it's values into classes."
  (let ((emails (make-hash-table :test 'equal)))
    (maphash (lambda (key val)
               (let ((date (tldr-newsletters-parse-date (gethash "id" val))))
                 (puthash key (tldr-newsletters-parse-email val date) emails))) json-hash)
    emails))

(defun tldr-newsletters-parse-date (date)
  (format-time-string "%F" (seconds-to-time (/ (string-to-number date) 1000))))

(defun tldr-newsletters-parse-email (raw-email timestamp)
  "Received an email representation as an hashtable and returns an email object"
  (tldr-newsletters-email :id (gethash "id" raw-email)
                          :body (gethash "body" raw-email)
                          :from (gethash "from" raw-email)
                          :subject (gethash "subject" raw-email)
                          :timestamp timestamp))

(defun tldr-newsletters-display-email (email)
  "Fill in the buffer with EMAIL content.
   Sets the buffer to readonly and activate the minor mode"
  (switch-to-buffer (get-buffer-create (format "*TLDR Newsletter %s*" (tldr-email-timestamp email))))
  (tldr-newsletters-mode 1)
  (insert (format "ID: %s\n" (tldr-email-id email)))
  (insert (format "Date: %s\n" (tldr-email-timestamp email)))
  (insert (format "Subject: %s\n" (tldr-email-subject email)))
  (insert "-----\n")
  (insert (tldr-email-body email))
  (goto-char (point-min))
  (while (re-search-forward "\r" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (read-only-mode 1))

(defun tldr-newsletters--delete-callback (_status)
  (message "Email deleted"))

(defun tldr-newsletters-delete-current-email ()
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward "ID: \\([^\n]+\\)" nil t)
    (let ((id (match-string 1)))
      (tldr-newsletters-delete-email (make-instance 'tldr-newsletters-email :id id)))))

(defun tldr-newsletters-delete-email (email)
  (let ((email-id (tldr-email-id email))
        (url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "application/json"))))
    (url-retrieve (tldr-newsletters-opentrashmail--delete-url email-id) #'tldr-newsletters--delete-callback nil t t)))

;;;###autoload
(defun tldr-newsletters ()
  "Display TLDR newsletters."
  (interactive)
  (tldr-newsletters-fetch)
  (tldr-newsletters-display-email (tldr-newsletters-menu)))

(defun tldr-newsletters-pick-delete-email()
  "Display all the newsletters and pick one to delete"
  (interactive)
  (tldr-newsletters-fetch)
  (tldr-newsletters-delete-email (tldr-newsletters-menu)))

(define-minor-mode tldr-newsletters-mode
  "Toggle tldr-newsletters mode.
  This is essentially to allow binding of local leader keys"
  :lighter "tldr-newsletters-mode"
  :keymap tldr-newsletters-mode-map)

(map! :map tldr-newsletters-mode-map
      :localleader
      :desc "Delete current email" :n "d" #'tldr-newsletters-delete-current-email)

(provide 'tldr-newsletters)
;;; tldr-newsletters.el ends here
