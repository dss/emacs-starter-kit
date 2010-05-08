;; TODO can't I put these somewhere else, env?
(push "~/bin" exec-path)
(push "/opt/local/bin" exec-path)
(push "/usr/local/share/emacs/site-lisp" load-path)
;; TODO load everything in my dir
(push "~/.emacs.d/dss/jabber" load-path)

(menu-bar-mode -1)
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (setq visible-bell nil)
  (transient-mark-mode t)
  (pc-selection-mode t)
  (server-start)
  (when (eq system-type 'darwin)
    (menu-bar-mode t)
    (defconst font
      "-apple-Terminus-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")
    (set-default-font font)
    (add-to-list 'default-frame-alist (cons 'font font))
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)
    (setq delete-by-moving-to-trash 1)
    (defun ns-raise-emacs ()
      (ns-do-applescript "tell application \"Emacs\" to activate"))
    ;; (add-hook 'server-visit-hook 'ns-raise-emacs)
    (set-frame-parameter nil 'fullscreen 'fullboth)
    )
  (when (eq system-type 'gnu/linux)
    (defconst font
      "-xos4-terminus-medium-r-normal--12-*-72-72-c-60-iso8859-1")
    (set-default-font font)
    (add-to-list 'default-frame-alist (cons 'font font)))
  (when (eq system-type 'windows-nt)
    (defconst font
      "-*-Lucida Console-normal-r-*-*-12-112-96-96-c-*-iso8859-1")
    (set-default-font font)
    (add-to-list 'default-frame-alist (cons 'font font))))

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c c") 'calc)
(global-set-key (kbd "C-c k") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c n") 'notmuch-folder)
(global-set-key (kbd "C-c m") 'notmuch-mua-mail)

(add-hook 'shell-mode-hook 'n-shell-mode-hook)
(defun n-shell-mode-hook ()
  "12Jan2002 - sailor, shell mode customizations."
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input))

;; ack (grep replacement)
(load-file "~/.emacs.d/dss/full-ack.el")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; Gnus
(setq mail-user-agent 'message-user-agent) 
(setq user-mail-address "dss@orst.edu")
(setq user-full-name "Darren Shepard")
(setq gnus-select-method '(nntp "news.free.fr"))

(add-hook 'gnus-topic-mode-hook 'gnus-topic-mode)
(setq mm-attachment-override-types '("image/.*"))
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(setq gnus-ignored-newsgroups "") ;; show [Gmail]/* folders

(require 'epa-file)
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; SMTP
;; (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it
;;       smtpmail-smtp-service 587
;;       smtpmail-auth-credentials (expand-file-name "~/.authinfo"))

;; notmuch
(require 'notmuch)
(setq notmuch-folders '(("inbox"    . "tag:inbox and not tag:delete")
                        ("starred"  . "tag:starred and not tag:delete")
                        ("sent"     . "tag:sent and not tag:delete")
                        ("spam"     . "tag:spam and not tag:delete")
                        ("trash"    . "tag:delete")
                        ("blowfish" . "tag:blowfish and not tag:delete")
                        ("bulk"     . "tag:bulk and not tag:delete")
                        ("notmuch"  . "tag:notmuch and not tag:delete")))
(setq notmuch-search-oldest-first nil)
(define-key notmuch-folder-mode-map "j" 'notmuch-folder-next)
(define-key notmuch-folder-mode-map "k" 'notmuch-folder-previous)
(define-key notmuch-search-mode-map "j" 'notmuch-search-next-thread)
(define-key notmuch-search-mode-map "k" 'notmuch-search-previous-thread)
(define-key notmuch-show-mode-map "j" 'notmuch-show-next-message)
(define-key notmuch-show-mode-map "k" 'notmuch-show-previous-message)

(setq message-cite-function 'message-cite-original-without-signature)
(setq message-kill-buffer-on-exit t)  ;; kill mail buffers after sending
(setq message-default-mail-headers "Bcc: darren.shepard@gmail.com\n")
(setq mail-header-separator "")

;; Little brother database
(autoload 'lbdb "lbdb" "Query the Little Brother's Database" t)
(autoload 'lbdb-region "lbdb" "Query the Little Brother's Database" t)
(autoload 'lbdb-maybe-region "lbdb" "Query the Little Brother's Database" t)

;; erc
(setq erc-fill-column 68)
(add-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)

;; Jabber
(require 'jabber-autoloads)
(setq jabber-account-list
      '(("darrenshepard@chat.facebook.com")
        ("darren.shepard@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))
(setq jabber-vcard-avatars-retrieve nil)
(setq jabber-roster-show-bindings nil)
(setq jabber-activity-make-strings 'jabber-activity-make-strings-shorten)
(setq jabber-alert-presence-hooks nil)
(setq jabber-alert-muc-hooks nil)
(add-hook 'jabber-chat-mode-hook 'flyspell-mode)
(add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)
(add-hook 'jabber-mode-hook 'erc-add-scroll-to-bottom)

;; Do this last since pc-select changes region color
(require 'color-theme)
(load-file "~/.emacs.d/dss/twilight.el")
(color-theme-twilight)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
