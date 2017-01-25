(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")   t)
(package-initialize)

;(require 'ergoemacs-mode)

;(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
;(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
;(setq ergoemacs-keyboard-layout "dv")
;(ergoemacs-mode 1)


(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key ivy-minibuffer-map (kbd "M-t") 'ivy-next-line) ;
    (define-key ivy-minibuffer-map (kbd "M-c") 'ivy-previous-line)
    (global-set-key (kbd "M-f") 'swiper)
    
		  ;; (add-hook 'ivy-mode-hook
	      ;; (lambda () (local-set-key (kbd "M-c") 'ivy-previous-line )))
    (global-set-key (kbd "C-1") 'ivy-dispatching-done)
                                        ;(global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-a") 'counsel-M-x)))


(defun my-minibuffer-setup-hook ()
  (xah-fly-keys 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(add-to-list 'load-path "~/.emacs.d/config")
(require 'xah-fly-keys)
(xah-fly-keys 1)

(require 'key-chord)
(key-chord-mode 1)

(key-chord-define xah-fly-key-map "dd" 'backward-word)

;;; Global settings
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1) ; relegate tooltips to echo area only
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines t)
(delete-selection-mode 1)
(setq inhibit-startup-screen t)
(prefer-coding-system 'utf-8)
                                        ;(set-default-font "Menlo 10")
                                        ; (setq default-frame-alist '((font . "Meslo LG S 10")))
                                        ;(set-frame-font "Inconsolata 12")

;(setq default-frame-alist '((font . "Fira Mono 10")))
(setq icicle-ido-like-mode nil)
(setq icicle-mode nil)
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Fira Mono 10"))
;; (add-to-list 'default-frame-alist '(font . "Fira Mono 10"))


(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

;;; Global settings
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1) ; relegate tooltips to echo area only
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines t)
(setq inhibit-startup-screen t)
(prefer-coding-system 'utf-8)
                                        ;(set-default-font "Menlo 10")
                                        ; (setq default-frame-alist '((font . "Meslo LG S 10")))
                                        ;(set-frame-font "Inconsolata 12")

;(setq default-frame-alist '((font . "Fira Mono 10")))
(setq icicle-ido-like-mode nil)
(setq icicle-mode nil)
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Fira Mono 10"))
(add-to-list 'default-frame-alist '(font . "Fira Mono 10"))


(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

(global-nlinum-mode 1)
(setq nlinum-format " %3d ")


(when window-system
  (global-hl-line-mode))


;; enable paren mode

(show-paren-mode 1)

;(load-theme 'material t)
;;;


(setq solarized-use-variable-pitch nil)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-dark t)

(setq scroll-conservatively 100)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package try
  :ensure t)


(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)



(require 'winner)
(winner-mode 1)
;(global-set-key (kbd "<apps> h") ')
;(global-set-key (kbd "<apps> t") 'winner-redo)


(global-set-key (kbd "s-;") 'winner-undo)
(global-set-key (kbd "s-:") 'winner-redo)

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key (kbd "s-<") 'move-text-up)
(global-set-key (kbd "s->") 'move-text-down)

(use-package multiple-cursors
  :ensure t
  :config
  (progn
    (global-set-key (kbd "M-8") 'mc/mark-next-like-this)
					; (global-set-key (kbd "C-") 'mc/mark-previous-like-this)
    (global-set-key (kbd "M-<f3>") 'mc/mark-all-like-this)))


;;ivy actions 
(defun ivy-test (x)
  (kill-new x))

(ivy-set-actions
 t
'(("i" ivy-test "insert")))

;; XML

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(defun custom-folding ()
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))

(add-hook 'nxml-mode-hook
	  (lambda ()
	    (progn 
	      (local-set-key (kbd "M-[") #'hs-hide-level)
	      (local-set-key (kbd "M-]") #'hs-toggle-hiding)
	      (local-set-key (kbd "M-0") #'hs-show-all))))

(defun indent-xml()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "><" nil t)
    (replace-match ">\n<"))
  (nxml-mode)
  (indent-region (point-min) (point-max) nil))
(global-set-key (kbd "M-<f12>") 'indent-xml)

;; Large file hack

(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 200)

(defun my--is-file-large ()
  "If buffer too large and my cause performance issue."
  (< large-file-warning-threshold (buffer-size)))

(define-derived-mode my-large-file-mode fundamental-mode "LargeFile"
  "Fixes performance issues in Emacs for large files."
  ;; (setq buffer-read-only t)
  (setq bidi-display-reordering nil)
  (jit-lock-mode nil)
  (buffer-disable-undo)
  (set (make-variable-buffer-local 'global-hl-line-mode) nil)
  (set (make-variable-buffer-local 'line-number-mode) nil)
  (set (make-variable-buffer-local 'column-number-mode) nil) )

(add-to-list 'magic-mode-alist (cons #'my--is-file-large #'my-large-file-mode))
(global-set-key (kbd "M-<f11>") 'my-large-file-mode)
;; Company

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-b") nil)
  (define-key company-active-map (kbd "M-l") nil)
  (define-key company-active-map (kbd "C-o") nil)
  (define-key company-active-map (kbd "M-t") #'company-select-next)
  (define-key company-active-map (kbd "M-c") #'company-select-previous)
  (define-key company-active-map (kbd "M-f") #'company-search-candidates))
(global-set-key (kbd "M-y") 'company-complete)

;(global-set-key (kbd "M-8") nil)
 
(use-package expand-region
   :ensure t
   :config
   (global-set-key (kbd "M-8") 'er/expand-region))



(global-set-key (kbd "C-.") 'eval-region)

(use-package help-fns+ ; Improved help commands
  :commands (describe-buffer describe-command describe-file
             describe-keymap describe-option describe-option-of-type))

(use-package ace-jump-mode
  :ensure t
  :config
  (define-key global-map (kbd "M-i") 'ace-jump-mode))


;; org mode

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))
;; (setq org-ellipsis "⤵")
(setq org-src-tab-acts-natively t)
(setq org-agenda-files '("f:/datalex/org/"))
(setq org-log-done 'time)

(global-set-key (kbd "M-<f9>") 'fill-paragraph)
(global-set-key (kbd "M-<f10>") 'toggle-truncate-lines)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("f:/datalex/org/tasks.org_archive" "f:/datalex/org/tasks.org")) t)
 '(package-selected-packages
   (quote
    (key-chord evil-dvorak org-bullets solarized-theme ace-jump-mode ergoemacs-mode which-key web-mode vlf use-package try tidy tide spacemacs-theme smex rainbow-mode rainbow-delimiters projectile project-root powerline persistent-soft nlinum neotree multiple-cursors material-theme logview highlight-quoted highlight-numbers help-fns+ helm f evil-multiedit evil-anzu eldoc-eval doom-themes counsel company ac-capf 0blayout))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
