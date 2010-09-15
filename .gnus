(require 'nnir)

(setq gnus-always-read-dribble-file t
      gnus-novice-user nil
      user-mail-address "masse@klarna.com"
      user-full-name "mats cronqvist")

(setq gnus-select-method 
      '(nntp 
	"news" 
	(nntp-address "news.gmane.org")
	(nnir-search-engine nntp)))

(setq gnus-secondary-select-methods 
      '(
; 	(nnimap "klarna"
; 		(nnimap-address "imap.gmail.com")
; 		(nnimap-server-port 993)
; 		(nnir-search-engine imap)
; 		(nnimap-authinfo-file "~/.gnus-authinfo")
; 		(nnimap-stream ssl))
	(nnimap "cronqvi.st"
		(nnimap-address "mailcluster.loopia.se")
		(nnimap-server-port 993)
		(nnir-search-engine imap)
		(nnimap-authinfo-file "~/.gnus-authinfo-priv")
 		(nnimap-stream ssl))
 	(nnimap "gmail"
 		(nnimap-address "imap.gmail.com")
 		(nnimap-server-port 993)
 		(nnir-search-engine imap)
 		(nnimap-authinfo-file "~/.gnus-authinfo-priv")
 		(nnimap-stream ssl))))

;;
;; sending mail.
;;

; klarna's (i.e. google's) smtp.
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials "~/.gnus-authinfo"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-debug-info t
      smtpmail-smtp-service 587)

; inside kreditor, use unencrypted smtp.
;
;(setq smtpmail-starttls-credentials '(("mail.kreditor.se" 25 nil nil))
;      smtpmail-smtp-server "mail.kreditor.se"
;      smtpmail-default-smtp-server "mail.kreditor.se"
;      send-mail-function 'smtpmail-send-it
;      message-send-mail-function 'smtpmail-send-it
;      smtpmail-smtp-service 25)
;
;  outside kreditor, use encrypted connection to loopia

;(setq message-send-mail-function 'smtpmail-send-it
;      smtpmail-starttls-credentials '(("mailcluster.loopia.se" 587 nil nil))
;      smtpmail-auth-credentials "~/.gnus-authinfo"
;      smtpmail-default-smtp-server "mailcluster.loopia.se"
;      smtpmail-smtp-server "mailcluster.loopia.se"
;      smtpmail-smtp-service 587
;      smtpmail-debug-info t)

;;;
;;; Setup posting styles.
;;;
(setq gnus-posting-styles
      '((".*" 
	 (address "mats.cronqvist@klarna.com"))
	(".*erlang.*"
	 (address "masse@klarna.com"))
	(".*gmail.*"
	 (address "mats.cronqvist@gmail.se"))
	(".*cronqvi.st"
	 (address "masse@cronqvi.st"))))

(setq nnimap-split-inbox '("INBOX" ))
(setq nnimap-split-predicate "UNDELETED")
(setq nnimap-split-crosspost nil)

(setq nnimap-split-rule
      '(("cronqvi.st" ("INBOX" (('junk          "allofmp3")
				('junk          "noreply@tradera.com")
                                ('junk          "Pumpkin Patch")
                                ('junk          "Pixmania")
				("INBOX.Spam"   "\\*SPAM\\*")
				("INBOX.Spam"   "\\[SPAM\\]")
				("INBOX.mail"   ""))))
        ("gmail"      ("INBOX" (("INBOX.mail"   ""))))))

(defun despam()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (if (search-forward-regexp "^\\$" (point-max) t)
               (progn
                 (gnus-summary-delete-article)
                  t)
             nil))))

(add-hook 'gnus-summary-generate-hook 'my-summary)

(defun my-summary ()
  (interactive)
  (local-set-key (kbd "$") 'gnus-summary-mark-as-spam)
  (local-set-key (kbd "C") 'despam))


(defun my-bbdb ()
  (require 'bbdb)
  (bbdb-initialize 'gnus 'message)
  (autoload 'bbdb/send-hook "moy-bbdb" "" t)
  (add-hook 'message-send-hook 'bbdb/send-hook)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus) 
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-message) 
  (add-hook 'message-setup-hook 'bbdb-define-all-aliases)
  (setq bbdb-offer-save (quote savenoprompt))
  (setq bbdb/send-auto-create-p t)
  (setq bbdb/send-prompt-for-create-p t)
  (setq  bbdb-notice-hook '(bbdb-auto-notes-hook)
         bbdb-north-american-phone-numbers-p nil
         bbdb-offer-save 'savenoprompt
         ;; Notice only messages addressed to me or the erlang newsgroups
         bbdb-ignore-most-messages-alist '(("to" . "[Mm]ats")
                                           ("to" . "[Mm]asse")
                                           ("newsgroups" . "distel")
                                           ("newsgroups" . "erlang"))
         bbdb/mail-auto-create-p 'bbdb-ignore-most-messages-hook
         bbdb/news-auto-create-p 'bbdb-ignore-most-messages-hook
         bbdb/prompt-for-create-p t ))

(if (locate-library "bbdb")
    (my-bbdb))
