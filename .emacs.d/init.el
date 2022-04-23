;; Made by Julien Palard <julien@palard.fr>
;;
;; Started on  Sun Nov 16 12:00:18 2008 Julien Palard
;;
;; Packages I typically use can be reinstalled by using:
;; M-x package-install-selected-packages
;;
;; To gather tags, according to emacs doc:
;;
;;    find . -name "*.[chCH]" -print | etags -


(setq user-full-name "Julien Palard"
      user-mail-address "julien@palard.fr")
(require 'use-package)
(require 'package)
(package-initialize)
(server-mode)
(setq exec-path (append exec-path '("/home/mdk/.local/bin")))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

(use-package ido
  :ensure t
  :init
  (ido-mode t))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package flycheck-grammalecte
  :ensure t
  :config (flycheck-grammalecte-setup))

(use-package diminish
  :ensure t)

(use-package magit
  :ensure t)

(tool-bar-mode -1)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c p")
  :custom
  (lsp-jedi-hover-disable-keyword-all t)
  (lsp-signature-doc-lines 1)
  (lsp-diagnostics-provider :none)
  (lsp-jedi-pylsp-extra-paths [])
  (lsp-keymap-prefix "C-c x")
  :config
  (set-face-attribute 'lsp-face-highlight-textual nil
                      :background "#666" :foreground "#ffffff"
                      )
  :hook ((python-mode) . lsp)
  :commands lsp-mode
)

(use-package python
  :custom
  (python-indent-guess-indent-offset nil)
)

(use-package lsp-ui
  :ensure t)

(use-package lsp-jedi
  :ensure t
  :after lsp-mode
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
))

;; Test using flycheck-verify-setup
(use-package flycheck
  :ensure t
  :after lsp-mode
  :config
  (global-flycheck-mode t))

(use-package blacken
  :ensure t
  :commands (blacken-mode)
  :hook (python-mode . blacken-mode))

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(add-hook 'org-mode-hook
          (lambda() (setq header-line-format
                                 '(:eval (org-display-outline-path nil t " > " t)))))


;; Disable transient mark mode, I don't like it:
(transient-mark-mode nil)

;; Coding style
(setq-default indent-tabs-mode nil
              tab-width 4
              py-indent-offset 4
              )

;; Don't show trailing whitespaces in term-mode
(add-hook 'term-mode-hook
      (lambda() (make-local-variable 'show-trailing-whitespace)
        (setq show-trailing-whitespace nil)))

(global-font-lock-mode t)
(column-number-mode t)
(show-paren-mode t)

(global-set-key "\C-cc" 'compile)
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\C-xrv" 'list-registers)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key "\C-cj" 'windmove-left)
(global-set-key "\C-ck" 'windmove-down)
(global-set-key "\C-cl" 'windmove-up)
(global-set-key "\C-c;" 'windmove-right)
(global-set-key "\C-x\M-%" 'query-replace-regexp) ;; As C-M-% is ~impossible to type in a terminal emulator:
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(global-set-key "\C-\M-v" 'clipboard-yank)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook 'global-company-mode)


(setq version-control t  ; (for backup files)
      mouse-yank-at-point t
      solarized-high-contrast-mode-line t
      inhibit-startup-message t
      make-backup-files t
      backup-directory-alist (quote ((".*" . "~/.emacs.d/backup/")))
      )

;; Save all backup file in this directory.
(setq-default delete-old-versions t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default truncate-partial-width-windows nil)

(menu-bar-mode -1)

(use-package whitespace
  :diminish (whitespace-mode global-whitespace-mode whitespace-newline-mode)
  :hook ((python-mode) . whitespace-mode)
  :config
  (setq show-trailing-whitespace t)
  (setq whitespace-line-column 88)
  (setq whitespace-style '(face empty tabs lines-tail trailing))
)


;; hex color
(defvar hexcolour-keywords
  '(("#[a-fA-F[:digit:]]\\{3,6\\}"
     (0 (let ((colour (match-string-no-properties 0)))
          (if (or (= (length colour) 4)
                  (= (length colour) 7))
              (put-text-property
               (match-beginning 0)
               (match-end 0)
               'face (list :background (match-string-no-properties 0)
                           :foreground (if (>= (apply '+ (x-color-values
                                                          (match-string-no-properties 0)))
                                               (* (apply '+ (x-color-values "white")) .6))
                                           "black" ;; light bg, dark text
                                         "white" ;; dark bg, light text
                                         )))))
        append))))


(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolour-keywords t))

(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'sass-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'emacs-lisp-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'conf-xdefaults-mode-hook 'hexcolour-add-to-font-lock)

(yas-global-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(frame-background-mode 'dark)
 '(package-selected-packages
   '(flycheck-grammalecte blacken spacemacs-theme company yasnippet-snippets use-package zenburn-theme markdown-mode org po-mode yaml-mode)))

(load-theme 'spacemacs-light t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'default-frame-alist '(font . "LiberationMono:size=18"))
(set-face-attribute 'default t :font "LiberationMono:size=18")
