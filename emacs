;;
;; .emacs for conf in /home/mandark
;;
;; Made by Palard Julien
;; Login   <julien@palard.fr>
;;
;; Started on  Sun Nov 16 12:00:18 2008 Julien Palard
;;

(setq user-full-name "Julien Palard")
(setq user-mail-address "julien@palard.fr")

(require 'cl)

(defun load-file-if-exists (file)
  "Load a file only if it exists."
  (if (file-exists-p file)
      (load-file file)))

(add-to-list 'load-path "~/.emacs.d/")

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
                                        ; from http://www.emacswiki.org/emacs/BackspaceKey
;;(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
;;(global-set-key (kbd "C-?") 'help-command)

(add-to-list 'load-path "~/.emacs.d/geben-0.26")
(add-to-list 'load-path "~/.emacs.d/geben-0.26/tree-widget")
(autoload 'geben "geben" "PHP Debugger on Emacs" t)

(pc-bindings-mode)
                                        ;(display-time-mode t) Floods my GNU screen's monitoring
(require 'footnote)
(add-hook 'message-mode-hook 'footnote-mode)
(iswitchb-mode t)
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

(icomplete-mode 99)

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

(menu-bar-mode nil)
(setq font-lock-maximum-size nil)

(load-file-if-exists "~/.emacs.d/rcirc-julien.el")

;; flymake-cursor (require 'cl)
;; wget http://www.emacswiki.org/emacs/download/flymake-cursor.el
;; aptitude install pyflakes pep8 to check python code
(require 'flymake-cursor nil 'noerror)
(global-set-key "\C-cn" 'flymake-goto-next-error)

(when (load "flymake" t)
  (defun flymake-python-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name temp-file
                        (file-name-directory buffer-file-name))))
      (list "flymake-python" (list local-file))))

   (defun flymake-php-init ()
     (let* ((temp-file (flymake-init-create-temp-buffer-copy
                        'flymake-create-temp-inplace))
            (local-file (file-relative-name temp-file
                         (file-name-directory buffer-file-name))))
       (list "flymake-php" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-python-init))
)

(add-hook 'find-file-hook 'flymake-find-file-hook)
(setq flymake-start-syntax-check-on-find-file nil)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key [f12] 'iwb)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((nil (:inverse-video t))))
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face)) (((class color) (min-colors 8) (background light)) nil)))
 '(font-lock-comment-face ((nil (:weight bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun terminal-init-screen ()
  "Terminal initialization function for screen-256color."
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))


(global-set-key [left] 'windmove-left)
(global-set-key [right] 'windmove-right)
(global-set-key [up] 'windmove-up)
(global-set-key [down] 'windmove-down)

;; Highlight 80th column
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

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

(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (char-equal (char-syntax (char-before (point))) ?\))
    (let ((matching-text (blink-matching-open)))
      (when matching-text
        (message matching-text)))))
