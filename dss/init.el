;; keys
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c k") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c c") 'calc)
(global-set-key (kbd "C-c g") 'ack)
(global-set-key (kbd "C-x g") 'ack)

(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-vertically)
(global-set-key (kbd "s-3") 'split-window-horizontally)
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "s-k") 'kill-buffer)
(global-set-key (kbd "s-g") 'magit-status)

(setq default-tab-width 4)

(load-file "~/.emacs.d/dss/dss.el")

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
    (defconst font "-apple-Meslo_LG_S_DZ-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
    (set-default-font font)
    (add-to-list 'default-frame-alist (cons 'font font))
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)
    (setq delete-by-moving-to-trash 1)
    (defun ns-raise-emacs ()
      (ns-do-applescript "tell application \"Emacs\" to activate"))
    ;; (add-hook 'server-visit-hook 'ns-raise-emacs)
    (set-frame-parameter nil 'fullscreen 'fullboth)
    (global-set-key (kbd "s-f") 'ns-toggle-fullscreen)
    )
  (when (eq system-type 'gnu/linux)
    (defconst font "-xos4-terminus-medium-r-normal--12-*-72-72-c-60-iso8859-1")
    ;(defconst font "-bitstream-Meslo LG S DZ-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
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
;(add-to-list 'load-path "~/tools/go/misc/emacs" t)
;(require 'go-mode-load)

;; ack (grep replacement)
(load-file "~/.emacs.d/dss/full-ack.el")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
(setq ack-prompt-for-directory t)

(require 'parenface)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-resize-window t)

(require 'php-mode)
(add-to-list 'auto-mode-alist '("Makefile.inc" . makefile-mode))

(setq-default c-indent-tabs-mode t
              c-indent-level 4
              c-argdecl-indent 0
              c-tab-always-indent t
              backward-delete-function nil)
(c-add-style "my-c-style" '((c-continued-statement-offset 4)))

(defun my-c-mode-hook ()
  (c-set-style "my-c-style")
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement 'c-lineup-runin-statements))
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(defun lconfig-c-mode ()
  (progn (define-key c-mode-base-map "\C-m" 'newline-and-indent)
         (define-key c-mode-base-map [f5] 'next-error)
         (define-key c-mode-base-map [f6] 'compile)
         (define-key c-mode-base-map (kbd "C-]") 'find-tag-other-window)
         (define-key c-mode-base-map (kbd "C-t") 'pop-tag-mark)
         ))
(add-hook 'c-mode-common-hook 'lconfig-c-mode)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; do this last since pc-select changes region color
(require 'color-theme)
(load-file "~/.emacs.d/dss/twilight.el")
(color-theme-twilight)

;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "#27292A"))))

;(require 'show-wspace)
;(add-hook 'c-mode-common-hook 'show-ws-highlight-tabs)

(hl-line-mode 1)

(defun unhtml (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;"))))

(defun split-into-three-horizontally-and-follow ()
  "Splits current window into 4 windows horizontally and switchs to follow-mode"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode 1)
  ;; Turn on line highlighting to make it easier to
  ;; track cursor
  (hl-line-mode 1))

(defun split-into-four-horizontally-and-follow ()
  "Splits current window into 4 windows horizontally and switchs to follow-mode"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode 1)
  ;; Turn on line highlighting to make it easier to
  ;; track cursor
  (hl-line-mode 1))

(global-set-key (kbd "C-x 8") 'split-into-three-horizontally-and-follow)
(global-set-key (kbd "C-x 9") 'split-into-four-horizontally-and-follow)

(require 'yasnippet-bundle)
