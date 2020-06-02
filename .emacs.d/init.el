;; Made by Julien Palard <julien@palard.fr>
;;
;; Started on  Sun Nov 16 12:00:18 2008 Julien Palard
;;
;; Packages I typically use can be reinstalled by using:
;; M-x package-refresh-contents
;; M-x package-install-selected-packages

(setq user-full-name "Julien Palard")
(setq user-mail-address "julien@palard.fr")
(setq default-frame-alist '((tty-color-mode . 8)))
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))
(elpy-enable)

;; Ido - interactive do - switches between buffers and opens files and
;; directories with a minimum of keystrokes.
(require 'ido)
(ido-mode t)

;; Disable all version control backends (Start faster):
(setq vc-handled-backends ())

;; Disable transient mark mode, I don't like it:
(transient-mark-mode nil)

;; PHP
(autoload 'php-mode "php-mode" "Mode for editing PHP source files")
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)

;; Coding style
(setq-default indent-tabs-mode nil
              tab-width 4
              py-indent-offset 4
              show-trailing-whitespace t)

;; Don't show trailing whitespaces in term-mode
(add-hook 'term-mode-hook
      (lambda() (make-local-variable 'show-trailing-whitespace)
        (setq show-trailing-whitespace nil)))

(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

(setq inhibit-startup-message t)

(global-font-lock-mode t)
(column-number-mode t)
(show-paren-mode t)

(global-set-key "\C-cc" 'compile)
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\C-xrv" 'list-registers)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key [f12] 'iwb)
(global-set-key "\C-cj" 'windmove-left)
(global-set-key "\C-ck" 'windmove-down)
(global-set-key "\C-cl" 'windmove-up)
(global-set-key "\C-c;" 'windmove-right)
(global-set-key "\C-x\M-%" 'query-replace-regexp) ;; As C-M-% is ~impossible to type in a terminal emulator:
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

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

(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


;; Highlight 80th column
(require 'whitespace)
(setq whitespace-line-column 88)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)


;; hex color
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{3,6\\}"
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

(add-hook 'python-mode-hook 'blacken-mode)

(add-hook 'php-mode-hook '(lambda ()
                           (auto-complete-mode t)
                           (require 'ac-php)
                           (setq ac-sources  '(ac-source-php ) )
                           (yas-global-mode 1)

                           (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
                           ))


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
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(frame-background-mode (quote dark))
 '(package-selected-packages (quote (markdown-mode org po-mode elpy blacken yaml-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )