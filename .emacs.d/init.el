;; Made by Julien Palard <julien@palard.fr>
;;
;; Started on  Sun Nov 16 12:00:18 2008 Julien Palard
;;
;; Packages I typically use can be reinstalled by using:
;; M-x package-install-selected-packages

(setq user-full-name "Julien Palard")
(setq user-mail-address "julien@palard.fr")
(require 'use-package)
(require 'package)
(package-initialize)
(server-mode)
(setq exec-path (append exec-path '("/home/mdk/.local/bin")))

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

(use-package ido
  :ensure t
  :init
  (ido-mode t))

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
  :custom
  (lsp-jedi-hover-disable-keyword-all t)
;;  (lsp-ui-sideline-show-code-actions nil)
;;  (lsp-ui-sideline-show-hover nil)
  (lsp-signature-doc-lines 1)
  (lsp-diagnostics-provider :none)
  (lsp-jedi-pylsp-extra-paths [])
  (lsp-keymap-prefix "C-c x")
  :config
  (set-face-attribute 'lsp-face-highlight-textual nil
                      :background "#666" :foreground "#ffffff"
                      )
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "po-language-server")
    :activation-fn (lsp-activate-on "gettext" "plaintext")
    :priority -1
    :server-id 'po
  ))
  (add-to-list 'lsp-language-id-configuration '(po-mode . "gettext"))
  :hook ((po-mode python-mode) . lsp)
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

;; lsp-mode can only work on named buffers
(defun po-mode-name-buffer ()
  (setq-local buffer-file-name "msgstr.po")
  (lsp))

(defun po-mode-unname-buffer ()
  (setq-local buffer-file-name nil))

(add-hook 'po-mode-hook
 (lambda ()
   (advice-add 'po-edit-msgstr :after 'po-mode-name-buffer)
   (advice-add 'po-subedit-exit :before 'po-mode-unname-buffer)))


;; Disable all version control backends (Start faster):
(setq vc-handled-backends ())

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

(setq inhibit-startup-message t)

(global-font-lock-mode t)
(column-number-mode t)
(show-paren-mode t)

(global-set-key "\C-cc" 'compile)
;(global-set-key "\M-n" 'forward-paragraph)
;(global-set-key "\M-p" 'backward-paragraph)
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


(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(add-hook 'after-init-hook 'global-company-mode)

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning of backup files.
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backup/"))))
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

(defun konix/find-file-hook ()
  "Permits file opening with line suffix like foo.py:21."
  (if (and
       (string-match "^\\(.+\\):\\([0-9]+\\)$" buffer-file-name)
       (not
        (file-exists-p buffer-file-name)))
      ;; the given file does not exist and is of the form file_name:number, I
      ;; most likely wants to open file_name at line number
      (progn
        (let (
              (old_buffer (current-buffer))
              (file_name (match-string-no-properties 1 buffer-file-name))
              (line (match-string-no-properties 2 buffer-file-name))
              )
          (if (file-exists-p file_name)
              (progn
                (find-file file_name)
                (forward-line (string-to-number line))
                (kill-buffer old_buffer)
                nil)
            nil)))
    nil))

(add-to-list 'find-file-hook 'konix/find-file-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(frame-background-mode 'dark)
 '(package-selected-packages
   '(flycheck-grammalecte blacken spacemacs-theme company yasnippet-snippets use-package zenburn-theme markdown-mode org po-mode yaml-mode)))

; (load-theme 'zenburn t)
(load-theme 'spacemacs-light t)
;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;(load-theme 'github-modern t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;(custom-set-faces
; '(default ((t (:family "Terminus" :foundry "UNKN" :slant normal :weight normal :height 90 :width normal)))))
;
(add-to-list 'default-frame-alist '(font . "LiberationMono:size=18"))
(set-face-attribute 'default t :font "LiberationMono:size=18")
(setq solarized-high-contrast-mode-line t)
