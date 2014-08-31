(setq inhibit-startup-message t)

(setq default-tab-width 8)
(setq-default indent-tabs-mode nil)

;; add all subdirs of ~/.emacs.d to your load-path
(dolist (path '("~/.emacs.d/elpa/*" "~/.emacs.d/*"))
  (dolist (f (file-expand-wildcards path))
    (add-to-list 'load-path f)))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; load color-theme
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

(load-file "~/.emacs.d/color-theme/themes/tty-dark.el")
(color-theme-tty-dark)

;; default frame size
(add-to-list 'default-frame-alist (cons 'height 42))
(add-to-list 'default-frame-alist (cons 'width 100))

;; load clojure mode
(require 'clojure-mode)

;; load slime
(eval-after-load "slime-repl"
  '(progn (slime-setup '(slime-repl))
          (setq slime-protocol-version 'ignore)))

(require 'slime)
(require 'slime-repl)

;; load paredit and auto-complete
(require 'paredit)
(require 'auto-complete)
(dolist (mode '(clojure emacs-lisp lisp scheme lisp-interaction))
  (add-hook (first (read-from-string (concat (symbol-name mode)
               "-mode-hook")))
      (lambda ()
        (paredit-mode 1)
        (local-set-key (kbd "<M-left>") 'paredit-convolute-sexp)
        (local-set-key (kbd "M-k") 'paredit-forward-kill-word)
        (local-set-key (kbd "C-]") 'paredit-forward-barf-sexp)
        (local-set-key (kbd "C-c [") 'paredit-wrap-square)
        (local-set-key (kbd "C-M-w") 'paredit-backward-up)
        (local-set-key (kbd "C-M-e") 'paredit-forward-up)
        (local-set-key (kbd "C-M-s") 'paredit-backward-down)
        (local-set-key (kbd "C-M-d") 'paredit-forward-down)
        (auto-complete-mode 1))))

;; correctly tab defprotocols, etc
(custom-set-variables
 '(clojure-mode-use-backtracking-indent t))

;; macros
(global-set-key (kbd "C-,")        'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.")        'kmacro-end-or-call-macro)
(global-set-key (kbd "<C-return>") 'apply-macro-to-region-lines)

(add-hook 'before-save-hook 'whitespace-cleanup)
;; (remove-hook 'before-save-hook 'whitespace-cleanup)

(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))

(add-hook 'makefile-mode-hook (lambda ()
                                (setq indent-tabs-mode t)))
