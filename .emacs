(add-to-list 'load-path "~/src/emacs")
(dolist (f (file-expand-wildcards "~/src/emacs/*"))
  (add-to-list 'load-path f))
(load-file "~/.emacs.d/init.el")

(require 'auto-complete)
(require 'paredit)
(require 'clojure-mode)
(require 'refheap)
(require 'ido-ubiquitous)

(delete-selection-mode 1)
(ido-mode 1)
(ido-ubiquitous 1)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(auto-complete-mode 1)
(column-number-mode 1)
(setq font-lock-verbose nil)

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(global-set-key "\C-a" 'smart-line-beginning)
(global-set-key (kbd "C-c s") (lambda ()
                                (interactive)
                                (slime-connect "localhost" 4005)))
(global-set-key (kbd "C-c z") 'shell)
(global-set-key (kbd "C-x =") (lambda ()
                                (interactive)
                                (diff-buffer-with-file (current-buffer))))

(global-set-key (kbd "C-x !")
                (lookup-key (current-global-map) (kbd "C-x 1")))
(global-set-key (kbd "C-x @")
                (lookup-key (current-global-map) (kbd "C-x 2")))
(global-set-key (kbd "C-x #")
                (lookup-key (current-global-map) (kbd "C-x 3")))
(global-set-key (kbd "C-x $")
                (lookup-key (current-global-map) (kbd "C-x 4")))

(global-set-key (kbd "C-c k") 'bury-buffer)
(global-set-key (kbd "C-c j") 'clojure-jack-in)
(global-set-key (kbd "C-c b") (lambda ()
                                (interactive)
                                (switch-to-buffer "*scratch*")))

(global-set-key (kbd "C-c g") (lambda ()
                                (interactive)
                                (if (use-region-p)
                                    (refheap-paste-region-private (region-beginning) (region-end))
                                  (refheap-paste-buffer-private))))
(global-set-key (kbd "C-c G") (lambda ()
                                (interactive)
                                (if (use-region-p)
                                    (progn (message "active")
                                           (refheap-paste-region (region-beginning) (region-end)))
                                  (refheap-paste-buffer))))

(global-set-key (kbd "C-c a") (lambda ()
                                (interactive)
                                (when (equal ".clj" (substring (buffer-file-name) -4))
                                  (paredit-mode t))
                                (let ((pt (point)))
                                  (beginning-of-buffer)
                                  (if (re-search-forward "^\\([-+=]\\)\\1\\{5,\\}" nil t)
                                      (error "%s" "Found git conflict markers")
                                    (goto-char pt)))
                                (save-buffer)
                                (shell-command (format "git add %s" (buffer-file-name)))))

(global-set-key (kbd "C-t") 'transpose-sexps)

(global-unset-key "\M-c")

(when (window-system)
  (global-unset-key "\C-z")
  (dolist (key (list (kbd "C-x C-c") (kbd "s-q")))
    (global-set-key key (lambda ()
                          (interactive)
                          (message "Why would you ever leave?")))))

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path))

(add-hook 'slime-repl-mode-hook (lambda ()
                                  (define-key slime-repl-mode-map (kbd "<C-return>") nil)
                                  (setq lisp-indent-function 'clojure-indent-function)
                                  (set-syntax-table clojure-mode-syntax-table)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(backup-directory-alist (quote (("." . "~/.emacs-backups"))))
 '(clojure-always-indent nil)
 '(haskell-mode-hook '(turn-on-haskell-indentation))
 '(clojure-defun-indents (quote (at-revision build-protocol)))
 '(clojure-mode-use-backtracking-indent t)
 '(initial-major-mode (quote clojure-mode))
 '(only-global-abbrevs t)
 '(open-paren-in-column-0-is-defun-start nil)
 '(refheap-token "dfcca00a-5a73-4ed0-bc34-b37910864098")
 '(refheap-user "amalloy")
 '(safe-local-variable-values (quote ((clojure-always-indent . defun) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby") (clojure-defun-indents quote (at-revision)))))
 '(set-mark-command-repeat-pop t)
 '(tab-width 8)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-strip-common-suffix t)
 '(visible-bell t)
 '(window-min-height 10)
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
