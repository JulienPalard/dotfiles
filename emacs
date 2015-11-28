;;
;; .emacs for conf in /home/mandark
;;
;; Made by Palard Julien
;; Login   <julien@palard.fr>
;;
;; Started on  Sun Nov 16 12:00:18 2008 Julien Palard
;;

;;; Code:
(setq user-full-name "Julien Palard")
(setq user-mail-address "julien@palard.fr")

(eval-when-compile (require 'cl))

(defun load-file-if-exists (file)
  "Load a file only if it exists."
  (if (file-exists-p file)
      (load-file file)))

;; Disable all version control backends (Start faster if don't use them) :
(setq vc-handled-backends ())

;; Disable transient mark mode, I don't like it :
(transient-mark-mode nil)

(autoload 'php-mode "php-mode" "Mode for editing PHP source files")

;; Never use tabs to indent :
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default py-indent-offset 4)
(setq-default show-trailing-whitespace t)
(add-hook 'term-mode-hook
      (lambda() (make-local-variable 'show-trailing-whitespace)
        (setq show-trailing-whitespace nil)))

(setq auto-mode-alist (cons '("\\.html" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.li" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tpl" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.php" . php-mode) auto-mode-alist))

(setq inhibit-startup-message t)
(setq european-calendar-style t)

(global-font-lock-mode t)
(column-number-mode t)

(global-set-key "\C-cc" 'compile)
(global-set-key "\C-c\C-g" 'goto-line)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\C-xrv" 'list-registers)
(global-set-key (kbd "M-h") 'backward-kill-word)

(add-to-list 'load-path "~/.emacs.d/geben-0.26")
(add-to-list 'load-path "~/.emacs.d/geben-0.26/tree-widget")
(autoload 'geben "geben" "PHP Debugger on Emacs" t)

(require 'footnote)
(add-hook 'message-mode-hook 'footnote-mode)
(show-paren-mode t)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(put 'upcase-region 'disabled nil)

;; ========== Place Backup Files in Specific Directory ==========

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

(global-set-key (read-kbd-macro "C-M-[") 'shrink-window-horizontally)
(global-set-key (read-kbd-macro "C-M-]") 'enlarge-window-horizontally)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key "\M-/" 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill)
      )

(setq-default truncate-partial-width-windows nil)

(setq-default delete-old-versions t)

;; /*
;; ** Eeple Indent Style
;; ** - Based on the BSD Allman Style, but with an indent of 4 instead of 8.
;; ** - Tab size is set to 4, so each 4 spaces are a tab
;; ** - Opening AND Closing Bracket have to be on the same column
;; ** -
;; **
;; */
;; int_main(int_ac,_char_**av)
;; {
;; --->if_(ac_==_ac)
;; --->{
;; --->--->ac_=_ac;
;; --->}
;; --->else
;; --->{
;; --->--->av_=_av;
;; --->}
;; }

(defun eeple-indent-style ()
  (interactive)
  (c-set-style "bsd")
  (c-set-offset 'case-label 4)
  (setq c-basic-offset 4))

(add-hook 'php-mode-hook 'eeple-indent-style)
(add-hook 'c-mode-hook 'eeple-indent-style)

(menu-bar-mode -1)
(setq font-lock-maximum-size nil)

(load-file-if-exists "~/.emacs.d/rcirc-julien.el")

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key [f12] 'iwb)
(global-set-key [left] 'windmove-left)
(global-set-key [right] 'windmove-right)
(global-set-key [up] 'windmove-up)
(global-set-key [down] 'windmove-down)

;; This one causes by IRC to bug with double quotes:
;; ;; Highlight 80th column
;; (require 'whitespace)
;; (setq whitespace-style '(face empty tabs lines-tail trailing))
;; (global-whitespace-mode t)

(defun konix/find-file-hook ()
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
                (goto-line (string-to-int line))
                (kill-buffer old_buffer)
                nil)
            nil)))
    nil))
(add-to-list 'find-file-hook 'konix/find-file-hook)

;; As C-M-% is almost impossible to type in a terminal emulator:
(global-set-key "\C-x\M-%" 'query-replace-regexp)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
    '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

  )

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

(icomplete-mode 99)

;; pip install flake8
;; pip install pylint
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck)
(flycheck-add-next-checker 'python-flake8 'python-pylint)
(setq flycheck-phpcs-standard "Shape")

(add-hook 'python-mode-hook 'jedi:setup)

;; $ cd ~/.emacs.d/
;; $ mkdir themes
;; $ cd themes/
;; $ git clone https://github.com/sellout/emacs-color-theme-solarized.git
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes/emacs-color-theme-solarized/")
(load-theme 'solarized t)

(defun xah-syntax-color-hex ()
  "Syntax color hex color spec such as 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer)
  )

(require 'company)
(require 'company-etags)
(setq company-etags-use-main-table-list "off")
(add-to-list 'company-etags-modes 'php-mode)
(add-to-list 'company-backends 'company-etags)
(add-to-list 'company-backends 'company-jedi)
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'php-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-my-php-backend)))


(defun company-my-php-backend (command &optional arg &rest ignored)
  (case command
    (prefix (and (eq major-mode 'php-mode)
                 (company-grab-symbol)))
    (sorted t)
    (candidates (all-completions
                 arg
                 (if (and (boundp 'my-php-symbol-hash)
                          my-php-symbol-hash)
                     my-php-symbol-hash

                   (with-temp-buffer
                     (call-process-shell-command "php -r '$all=get_defined_functions();foreach ($all[\"internal\"] as $fun) { echo $fun . \";\";};'"
                                                 nil t)
                     (goto-char (point-min))
                     (let ((hash (make-hash-table)))
                       (while (re-search-forward "\\([^;]+\\);" (point-max) t)
                         (puthash (match-string 1) t hash))
                       (setq my-php-symbol-hash hash))))))))


(require 'weechat)

(provide `.emacs)
;;; .emacs ends here
