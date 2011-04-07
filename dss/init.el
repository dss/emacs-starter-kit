;; keys
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c k") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c c") 'calc)

(push "/opt/local/bin" exec-path)

(setq default-tab-width 4)

;; gui mode
(menu-bar-mode -1)
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (setq visible-bell nil)
  (transient-mark-mode t)
  (pc-selection-mode t)
;;  (server-start)
  (when (eq system-type 'darwin)
    (menu-bar-mode t)
;;    (defconst font "-apple-Terminus-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
    (defconst font "-apple-Meslo_LG_S_DZ-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
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

;; force compile buffer to scroll
(defadvice compile-internal (after my-scroll act comp)
  "Forces compile buffer to scroll. See around line 363 in compile.el"
  (let* ((ob (current-buffer)))
    (save-excursion
      (select-window (get-buffer-window ad-return-value))
      (goto-char (point-max))
      (select-window (get-buffer-window ob)))))
(setq compilation-scroll-output t)

;; make up/down work in *shell*
(add-hook 'shell-mode-hook 'n-shell-mode-hook)
(defun n-shell-mode-hook ()
  "12Jan2002 - sailor, shell mode customizations."
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input))

;; mail-related
(setq message-cite-function 'message-cite-original-without-signature)
(setq message-kill-buffer-on-exit t)  ;; kill mail buffers after sending
(setq mail-header-separator "")

;; mutt specific
(defun dss-message-mode-hook ()
  (flush-lines "^Reply-To:")
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*")
  (not-modified)
  (message-goto-body))
(or (assoc "mutt-" auto-mode-alist)
    (setq auto-mode-alist (cons '("mutt-" . message-mode) auto-mode-alist)))
(add-hook 'message-mode-hook 'dss-message-mode-hook)

;; little brother db
(autoload 'lbdb "lbdb" "Query the Little Brother's Database" t)
(autoload 'lbdb-region "lbdb" "Query the Little Brother's Database" t)
(autoload 'lbdb-maybe-region "lbdb" "Query the Little Brother's Database" t)

;; golang
(add-to-list 'load-path "~/tools/go/misc/emacs" t)
(require 'go-mode-load)

;; org-mode
(setq org-agenda-files (list "~/Documents/notes.org"
                             "~/Documents/obvi.org"))

;; ack (grep replacement)
(load-file "~/.emacs.d/dss/full-ack.el")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

(require 'parenface)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-resize-window t)

(require 'php-mode)

;; do this last since pc-select changes region color
(require 'color-theme)
(load-file "~/.emacs.d/dss/twilight.el")
(color-theme-twilight)
