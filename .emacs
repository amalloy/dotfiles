(add-to-list 'load-path "~/src/emacs")
(dolist (f (file-expand-wildcards "~/src/emacs/*"))
  (add-to-list 'load-path f))
(load-file "~/.emacs.d/init.el")

(require 'ido-ubiquitous)
(require 'auto-complete)
(require 'paredit)
(require 'clojure-mode)
(require 'gist)

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

(defun useful-debug (&optional remove)
  (interactive "P")
  (let ((import-string "\n  (:use flatland.useful.debug)"))
    (if remove
        (save-excursion
          (beginning-of-buffer)
          (while (search-forward import-string nil t)
            (replace-match ""))
          (beginning-of-buffer)
          (while (search-forward-regexp "\n\\s-*\\(\\?!?\\|(\\?!?)\\)" nil t)
            (replace-match ""))
          (beginning-of-buffer)
          (while (search-forward-regexp "(\\?)\\s-*" nil t)
            (replace-match ""))
          (beginning-of-buffer)
          (while (search-forward-regexp "(\\?!?\\s-+" nil t)
            ;;(forward-sexp)
            ;;(backward-sexp)
            (paredit-raise-sexp)))
      (save-excursion
        (beginning-of-buffer)
        (forward-sexp)
        (paredit-backward-down)
        (insert import-string)))))

(global-set-key "\C-a" 'smart-line-beginning)
(global-set-key (kbd "C-c s") (lambda ()
                                (interactive)
                                (slime-connect "localhost" 4005)))
(global-set-key (kbd "C-c z") 'shell)
(global-set-key (kbd "C-x =") (lambda ()
                                (interactive)
                                (diff-buffer-with-file (current-buffer))))
(global-set-key (kbd "C-c r") (lambda ()
                                (interactive)
                                (revert-buffer nil t)))
(global-set-key (kbd "C-c d") 'useful-debug)


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
(global-set-key (kbd "C-c f") 'find-grep)
(global-set-key (kbd "C-c F") 'toggle-frame-fullscreen)

(global-set-key (kbd "C-c g") 'gist-region-or-buffer-private)
(global-set-key (kbd "C-c G") 'gist-region-or-buffer)

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

(global-set-key (kbd "C-c p")
                (lambda (&optional arg)
                  "Switch the repl to the ns of the current .clj file, and switch to repl buffer."
                  (interactive "p")
                  (kmacro-exec-ring-item (quote ([3 134217840 return 3 26] 0 "%d")) arg)))

(global-set-key (kbd "C-t") 'transpose-sexps)

(global-unset-key "\M-c")

(when (window-system)
  (global-unset-key "\C-z")
  (dolist (key (list (kbd "C-x C-c")))
    (global-set-key key (lambda ()
                          (interactive)
                          (message "Why would you ever leave?")))))

(if (equal system-type 'darwin)
    (progn
      ;; visible-bell seems broken on osx?
      (setenv "PATH" (concat "~/bin:/usr/local/bin:" (getenv "PATH")))
      (push "/usr/local/bin" exec-path))
  (setq visible-bell t))

(add-hook 'slime-repl-mode-hook (lambda ()
                                  (define-key slime-repl-mode-map (kbd "<C-return>") nil)
                                  (setq lisp-indent-function 'clojure-indent-function)
                                  (set-syntax-table clojure-mode-syntax-table)))
(add-hook 'c-mode-common-hook (lambda ()
                                (local-set-key (kbd "M-,")
                                               #'pop-tag-mark)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(backup-directory-alist (quote (("." . "~/.emacs-backups"))))
 '(c-basic-offset 2)
 '(clojure-always-indent nil)
 '(clojure-defun-indents (quote (at-revision build-protocol prop/for-all gen/bind)))
 '(clojure-mode-use-backtracking-indent t)
 '(clojure-swank-command
   "LEIN_FAST_TRAMPOLINE=y lein trampoline with-profile +swank jack-in %s")
 '(column-number-mode t)
 '(diff-switches "-u")
 '(grep-find-command
   (quote
    ("find . -type f -not -name '*.o' -exec grep -nH -Pie '' {} +" . 54)))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(initial-major-mode (quote clojure-mode))
 '(menu-bar-mode nil)
 '(only-global-abbrevs t)
 '(open-paren-in-column-0-is-defun-start nil)
 '(refheap-token "dfcca00a-5a73-4ed0-bc34-b37910864098")
 '(refheap-user "amalloy")
 '(safe-local-variable-values
   (quote
    ((clojure-always-indent . defun)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby"))))
 '(set-mark-command-repeat-pop t)
 '(tab-width 8)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-strip-common-suffix t)
 '(window-min-height 10)
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "unknown" :family "Courier New")))))
(put 'narrow-to-region 'disabled nil)
