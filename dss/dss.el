(defun indent-untabify-region ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (region-beginning) (region-end))
  (untabify (region-beginning) (region-end)))

(defun toggle-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines)))

(defun toggle-truncate-partial-width-windows ()
  (interactive)
  (setq truncate-partial-width-windows (not truncate-partial-width-windows)))

(defalias 'qrr 'query-replace-regexp)

(defun dss-toggle-shell ()
  (interactive)
  (if (get-buffer-window "*shell*" 'visible)
      (progn
        (delete-window (get-buffer-window "*shell*" 'visible)))
    (split-window-vertically)
    (shell)))

(define-key global-map "\M-`" 'dss-toggle-shell)

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(require 'show-wspace)
(add-hook 'c-mode-common-hook 'show-ws-highlight-tabs)

                                        ; Tab Completion Everywhere
                                        ; http://www.emacsblog.org/2007/03/12/tab-completion-everywhere/
(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

(defun my-tab-fix ()
  (local-set-key [tab] 'indent-or-expand))

(add-hook 'c++-mode-hook        'my-tab-fix)
(add-hook 'c-mode-hook          'my-tab-fix)
(add-hook 'sh-mode-hook         'my-tab-fix)
(add-hook 'emacs-lisp-mode-hook 'my-tab-fix)

(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun progmodes-hooks ()
  "Hooks for programming modes"
  (yas/minor-mode-on)
  (add-hook 'before-save-hook 'progmodes-write-hooks))

(defun progmodes-write-hooks ()
  "Hooks which run on file write for programming modes"
  (prog1 nil
    (set-buffer-file-coding-system 'utf-8-unix)
    (untabify-buffer)
    (delete-trailing-whitespace)))

(defun delete-trailing-whitespacep ()
  "Should we delete trailing whitespace when saving this file?"
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (next-line 25))
    (let ((pos (point)))
      (goto-char (point-min))
      (and (re-search-forward (concat "@author +" user-full-name) pos t) t))))

(defun maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace if I am the author of this file."
  (interactive)
  (and (delete-trailing-whitespacep) (delete-trailing-whitespace)))

(add-hook 'c++-mode-hook 'progmodes-hooks)
(add-hook 'c-mode-hook 'progmodes-hooks)
