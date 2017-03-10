;; Package settings
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")   t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; General settings
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1) ; relegate tooltips to echo area only
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines t)
(delete-selection-mode 1)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(prefer-coding-system 'utf-8)
(setq icicle-ido-like-mode nil)
(setq icicle-mode nil)
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(font . "Fira Mono 10"))
					;(set-face-font 'default "Fira Mono-11:antialias=none")
					;(set-face-font 'default "Fira Mono-11:antialias=subpixel")
					;(set-face-attribute 'default nil :height 115 :family "Consolas")
					;(set-face-font 'default "Fira Mono-11:antialias=natural")
					;(set-face-font 'default "Fira Mono-11:antialias=standard")
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)

(show-paren-mode 1)
(global-hl-line-mode)
(setq case-fold-search nil)
(setq scroll-conservatively 100)

;; Theme settings

(use-package solarized-theme
  :ensure t)
(setq solarized-use-variable-pitch nil)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-dark t)

;; Load custom modules from congig dir 
(add-to-list 'load-path "~/.emacs.d/config")
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

(require 'xah-fly-keys)
(xah-fly-keys 1)

;; Define my custom modal keys
(define-key xah-fly--tab-key-map (kbd "x") 'indent-xml)
(define-key xah-fly-e-keymap (kbd "k") 'paste-xml)
(define-key xah-fly-leader-key-map (kbd "u") 'helm-mini)

(define-key xah-fly-key-map (kbd "C-r") 'nil)
(define-key xah-fly-key-map (kbd "C-r") 'find-file)
(define-key xah-fly-key-map (kbd "C-k") 'yank)

(global-set-key (kbd "M-]") 'text-scale-increase)
(global-set-key (kbd "M-[") 'text-scale-decrease)

(define-key isearch-mode-map (kbd "M-c") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "M-t") 'isearch-repeat-forward)


(define-key minibuffer-local-map (kbd "M-n") 'nil)
(define-key minibuffer-local-map (kbd "M-n") 'forward-char)
(define-key minibuffer-local-map (kbd "M-H") 'previous-history-element)
(define-key minibuffer-local-map (kbd "M-N") 'next-history-element)

(eval-after-load "nxml-mode"
  '(progn
     (define-key nxml-mode-map (kbd "M-H") 'my-hs-hide-level)
     (define-key nxml-mode-map (kbd "M-N") 'my-hs-toggle-hiding)
     (define-key nxml-mode-map (kbd "M-0") 'hs-show-all)))

;;; test stuff
(defun custom-commands ()
  (interactive)
  (if (equal major-mode 'dired-mode)
      (progn (define-key xah-fly-key-map (kbd "r") 'helm-find-files))
    (message "dired disable")))

;; (add-hook 'xah-fly-command-mode-activate-hook 'custom-commands)

;; Ergoemacs like keys
(global-set-key (kbd "M-c") 'previous-line)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-t") 'next-line)
(global-set-key (kbd "M-n") 'forward-char)
(global-set-key (kbd "M-g") 'backward-word)
(global-set-key (kbd "M-r") 'forward-word)
(global-set-key (kbd "M-d") 'xah-beginning-of-line-or-block)
(global-set-key (kbd "M-s") 'xah-end-of-line-or-block)
(global-set-key (kbd "M-e") 'delete-backward-char)
(global-set-key (kbd "M-u") 'delete-forward-char)
(global-set-key (kbd "M-.") 'backward-kill-word)
(global-set-key (kbd "M-p") 'kill-word)
(global-set-key (kbd "M-C") 'move-text-up)
(global-set-key (kbd "M-T") 'move-text-down)
(global-set-key (kbd "M-k") 'xah-paste-or-paste-previous)
(global-set-key (kbd "M-j") 'xah-copy-line-or-region)
(global-set-key (kbd "M-;") 'undo)
(global-set-key (kbd "M-:") 'undo-tree-redo)
(global-set-key (kbd "M-z") 'xah-comment-dwim)

(require 'winner)
(winner-mode 1)

;; Define custom function profix key 
(xah-fly--define-keys
 (define-prefix-command 'kde-function-keymap)
 '(
   ("9" . winner-undo)
   ("0" . winner-redo)
   ))
(global-set-key (kbd "<f13>") kde-function-keymap)

;; Custom folding overlay
(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])
(defcustom hs-fringe-face 'hs-fringe-face
  "*Specify face used to highlight the fringe on hidden regions."
  :type 'face
  :group 'hideshow)
(defface hs-fringe-face
  '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
  "Face used to highlight the fringe on folded regions"
  :group 'hideshow)
(defcustom hs-face 'hs-face
  "*Specify the face to to use for the hidden region indicator."
  :type 'face
  :group 'hideshow)
(defface hs-face
  '((t (:background "#93a1a1" :foreground "#002b36" :box t)))
  "Face to hightlight the ... area of hidden regions"
  :group 'hideshow)
(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov))))
           )
      ;; On hover over the overlay display the hidden text.
      (overlay-put ov 'help-echo (buffer-substring (overlay-start ov)
						   (overlay-end ov)))
      (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 0 (length display-string) 'face 'hs-face display-string)
      (overlay-put ov 'display display-string)
      )))

(setq hs-set-up-overlay 'display-code-line-counts)

(defun my-hs-toggle-hiding (arg)
  (interactive "p")
  (save-excursion (hs-toggle-hiding))
  )

(defun my-hs-hide-level (arg)
  (interactive "p")
  (hs-hide-level 1)
  )

;; Disable modal keys in minibuffer
(defun my-minibuffer-setup-hook ()
  (xah-fly-keys 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key xah-fly-key-map (kbd "<escape>") 'quit-command)
;; Add supporting of yank to minibuffer
(defun my/paste-in-minibuffer ()
  (local-set-key (kbd "M-k") 'yank)
  (local-set-key (kbd "C-k") 'yank)
  (local-set-key (kbd "C-a") 'mark-whole-buffer))
(add-hook 'minibuffer-setup-hook 'my/paste-in-minibuffer)

(defun quit-command()
  (interactive)
  (if (bound-and-true-p multiple-cursors-mode)
      (mc/keyboard-quit))
  (xah-fly-command-mode-activate)
  (keyboard-quit))

(use-package nlinum
  :ensure t
  :config
  (progn
    (setq nlinum-format " %3d ")
    (add-hook 'prog-mode-hook 'nlinum-mode)
    ;; (add-hook 'text-mode-hook 'nlinum-mode)
    ))

(use-package delight
  :ensure t
  :demand t)



(use-package undo-tree
  :ensure t)  
(global-undo-tree-mode 1) 


;; ;; Install code-folding
;; (use-package hideshowvis
;;   :ensure t
;;   :config (progn 
;;             (hideshowvis-symbols)
;;             (bind-key "C-c h" 'hs-toggle-hiding)
;;             (set-face-attribute 'hs-face nil
;;                                 :box nil
;;                                 :background (face-foreground 'default)
;;                                 :foreground (face-background 'default))
;;             (add-hook 'nxml-mode-hook 'hideshowvis-minor-mode)))

(use-package try
  :ensure t)

;; Swirer settings
(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key ivy-minibuffer-map (kbd "M-t") 'ivy-next-line) ;
    (define-key ivy-minibuffer-map (kbd "M-c") 'ivy-previous-line)
    ;; (global-set-key (kbd "M-f") 'swiper)
    ))

(use-package helm
  :ensure t
  :config
  (setq helm-split-window-in-side-p t)
  ;(setq helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 40)
  (helm-autoresize-mode 1)
  :bind (("M-f" . helm-occur) 
	 :map helm-map
	 ("M-c" . helm-previous-line)
	 ("M-t" . helm-next-line)
	 :map xah-fly-key-map
	 ("C-r" . helm-find-files)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  :bind (:map paredit-mode-map
	      (";" . nil)
	      (":" . nil)
	      ("M-;" . nil)))

(use-package multiple-cursors
  :ensure t
  :config
  (progn
    (define-key mc/keymap (kbd "<escape>") 'mc/keyboard-quit)
    (global-set-key (kbd "C-8") 'mc/mark-all-like-this)
    (global-set-key (kbd "M-8") 'vr/mc-mark)))

(global-set-key (kbd "C-f") 'phi-search)
;; (global-set-key (kbd "C-F") 'phi-search-backward)
;; (global-set-key (kbd "C-F") 'phi-search-again-or-next)

;; Custom defuns
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

(defun indent-xml()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "><" nil t)
    (replace-match ">\n<"))
  (nxml-mode)
  (indent-region (point-min) (point-max) nil)
  (goto-char (point-min)))
(global-set-key (kbd "M-<f12>") 'indent-xml)

(defun paste-xml ()
  (interactive)
  (my-large-file-mode)
  (xah-paste-or-paste-previous)
  (indent-xml))

;; Large file performance improvement
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
  ;; (buffer-disable-undo)
  (set (make-variable-buffer-local 'global-hl-line-mode) nil)
  (set (make-variable-buffer-local 'line-number-mode) nil)
  (set (make-variable-buffer-local 'column-number-mode) nil) )

(add-to-list 'magic-mode-alist (cons #'my--is-file-large #'my-large-file-mode))

(defadvice xah-paste-or-paste-previous (before large-file-paste activate)
  (large-file-paste))

(defun large-file-paste ()
  (interactive)
  (let (text len)
    (setq text (car kill-ring))
    (setq len (length text))
    (message "length %d" len)
    (if (> len 10000)
	(my-large-file-mode))))

;; Company
(use-package company
  :ensure t
  :config
  (progn
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "M-b") nil)
      (define-key company-active-map (kbd "M-l") nil)
      (define-key company-active-map (kbd "C-o") nil)
      (define-key company-active-map (kbd "M-t") #'company-select-next)
      (define-key company-active-map (kbd "M-c") #'company-select-previous)
      (define-key company-active-map (kbd "M-f") #'company-search-candidates))
    (global-set-key (kbd "M-y") 'company-complete)

    (add-hook 'after-init-hook 'global-company-mode)
    ))


(use-package expand-region
  :ensure t)

;; (use-package help-fns+ ; Improved help commands
;;   :commands (describe-buffer describe-command describe-file
;; 			     describe-keymap describe-option describe-option-of-type))

(use-package help-fns+
  :ensure t)

;; (use-package ace-jump-mode
;;   :ensure t
;;   :config
;;   (define-key global-map (kbd "M-i") 'ace-jump-mode))

;; org mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (progn
	      (org-bullets-mode t)
	      (define-key org-mode-map (kbd "M-H") 'org-metaleft)
	      (define-key org-mode-map (kbd "M-N") 'org-metaright))))
(setq org-src-tab-acts-natively t)
(setq org-agenda-files '("f:/datalex/org/"))
(setq org-log-done 'time)
(setq org-src-fontify-natively t)

(xah-fly--define-keys
 (define-prefix-command 'kde-org-keymap)
 '(
   ("e" . org-edit-special)
   ("a" . org-agenda)
   ))

(defun replace-char (arg)
  (interactive
   (list
    (read-char-exclusive)))
  (delete-char 1)
  (insert arg))

;; Python config
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))


(use-package elpy
  :ensure t
  :config
  (progn 
    (elpy-enable)
    (setq Exec-path (append exec-path '("c:/Program Files (x86)/Python/Python36-32/Scripts")))
    ;; (elpy-use-ipython)
    (setq elpy-rpc-backend "jedi")
    ))



(defun prelude-personal-python-mode-defaults ()
  "Personal defaults for Python programming."
  ;; Enable elpy mode
  (elpy-mode)
  ;; Jedi backend
  ;; (jedi:setup)
  ;; (setq jedi:complete-on-dot t) ;optional
  ;; (auto-complete-mode)
  ;; (jedi:ac-setup)
					;(setq elpy-rpc-python-command "python3")
  ;; (python-shell-interpreter "ipython3")
  (company-quickhelp-mode))

(setq prelude-personal-python-mode-hook 'prelude-personal-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'prelude-personal-python-mode-hook)))

(defun xah-display-minor-mode-key-priority  ()
  "Print out minor mode's key priority.
URL `http://ergoemacs.org/emacs/minor_mode_key_priority.html'
Version 2017-01-27"
  (interactive)
  (mapc
   (lambda (x) (prin1 (car x)) (terpri))
   minor-mode-map-alist))

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.
Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'xah-fly-keys)
    (let ((mykeys (assq 'xah-fly-keys minor-mode-map-alist)))
      (assq-delete-all 'xah-fly-keys minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(add-hook 'after-load-functions 'my-keys-have-priority)

;; Resolve magit conflicts
(use-package magit
  :ensure t
  :bind (:map magit-file-section-map
	      ("u" . nil)
	      ("a" . nil)))
(xah-fly--define-keys
 (define-prefix-command 'kde-git-keymap)
 '(
   ("s" . magit-status)
   ("r" . magit-refresh)
   ("c" . magit-commit)
   ("p" . magit-push)
   ))

(setq exec-path (append exec-path '("f:/app/cygwin/bin")))

;; Add yasnippet support
(yas-global-mode 1)

;; ediff settings
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")

(defun ora-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "t" 'ediff-next-difference)
  (define-key ediff-mode-map "c" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'ora-ediff-hook)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(require 're-builder)
(setq reb-re-syntax 'string)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

;; (defun my-command-error-function (data context caller)
;;   "Ignore the buffer-read-only, beginning-of-buffer,
;; end-of-buffer signals; pass the rest to the default handler."
;;   (when (not (memq (car data) '(buffer-read-only
;;                                 beginning-of-buffer
;;                                 end-of-buffer)))
;;     (command-error-default-function data context caller)))

;; (setq command-error-function #'my-command-error-function)
(put 'narrow-to-region 'disabled nil)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(require 'ob-sh)
;; (setq org-babel-sh-command "f:\\app\\emacs24\\libexec\\emacs\\24.5\\i686-pc-mingw32\\cmdproxy.exe")


(use-package visual-regexp
  :ensure t)
(use-package visual-regexp-steroids
  :ensure t)

;; (use-package spaceline
;;   :ensure t
;;   :config
;;   (progn 
;;     (require 'spaceline-config)
;;     (spaceline-emacs-theme)
;;     (spaceline-helm-mode)
;;     (setq powerline-default-separator 'wave)
;;     (spaceline-compile)
;;     ))

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))

(use-package diminish
  :ensure t
  :config
  (progn 
    (diminish 'ivy-mode)
    (diminish 'which-key-mode)
    (diminish 'undo-tree-mode)
    (diminish 'xah-fly-keys "xah")
    (diminish 'all-the-icons-dired-mode)
    (diminish-major-mode 'emacs-lisp-mode-hook "ξλ")
    (diminish-major-mode 'lisp-interaction-mode-hook "λ")
    ))

(use-package all-the-icons
  :ensure t)

(use-package dired+
  :ensure t
  :config
  (setq ls-lisp-dirs-first t))

(use-package tramp-hdfs
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

; Modeline - config

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'slant))

(setq-default mode-line-format
	      '(
		(:eval
		 (let* ((active (powerline-selected-window-active))
			(mode-line-buffer-id (if active 'mode-line-buffer-id-inactive 'mode-line-buffer-id))
			(mode-line (if active 'mode-line-inactive 'mode-line))
			(face1 (if active 'powerline-inactive2 'powerline-active1))
			(face2 (if active 'powerline-inactive1 'powerline-active2))
			(face-bold1 (if active 'powerline-inactive-bold-2 'powerline-active-bold-1))
			(face-bold2 (if active 'powerline-inactive-bold-1 'powerline-active-bold-2))
			(separator-left (intern (format "powerline-%s-%s"
							(powerline-current-separator)
							(car powerline-default-separator-dir))))
			(separator-right (intern (format "powerline-%s-%s"
							 (powerline-current-separator)
							 (cdr powerline-default-separator-dir))))
			(ths (list
			      (powerline-raw " test" 'face1 'r)
			      ))
			(lhs (list
			      (powerline-raw (xah-get-current-mode-str) face-bold1 'l)
			      (powerline-raw "%*  " face1 'l)
			      (mode-icon face1)
			      (powerline-buffer-id face-bold1 'l)
			      (powerline-raw " " face1 'l)
			      (powerline-narrow face1 'l)
			      (powerline-raw (custom-modeline-icon-vc face1) face1 'l)))
			(rhs (list (powerline-raw global-mode-string face1 'r)
				   (custom-modeline-region-info face1)
				   (powerline-raw "%4l" face1 'r)
				   (powerline-raw ":" face1)
				   (powerline-raw "%3c" face1 'r)
				   (funcall separator-right face1 mode-line)
				   (powerline-raw " " mode-line)
				   (powerline-raw "%6p" mode-line 'r)
				   (powerline-buffer-size face1 'l)
				   (powerline-hud face2 face1)))
			(center (list (powerline-raw " " face1)
				      (funcall separator-left face1 face2)
				      (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
					(powerline-raw erc-modified-channels-object face2 'l))
				      (powerline-major-mode face2 'l)
				      (powerline-process face2)
				      (powerline-raw " :" face2)
				      (powerline-minor-modes face2 'l)
				      (powerline-raw " " face2)
				      (funcall separator-right face2 face1))))
		   (concat 
		    (powerline-render lhs)
		  ;   (powerline-render ths)
			    (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			    (powerline-render center)
			    (powerline-fill face1 (powerline-width rhs))
		    (powerline-render rhs)
		    )))))

(face-spec-set
 'mode-line
 '((t
      :box (:line-width 1 :color "#002b36" :style unspecified)
      :overline "#002b36"
      )))

(defface powerline-active-bold-1
  '((t
     :weight bold
     :inherit powerline-active1
     ))
  "face"
  :group 'powerline)

(defface powerline-active-bold-2
  '((t
     :weight bold
     :inherit powerline-active2
     ))
  "face"
  :group 'powerline)

(defface powerline-inactive-bold-1
  '((t
     :weight bold
     :inherit powerline-inactive1
     ))
  "face"
  :group 'powerline)

(defface powerline-inactive-bold-2
  '((t
     :weight bold
     :inherit powerline-inactive2
     ))
  "face"
  :group 'powerline)

(defface my-xah-info
  '(( t
      :foreground "#839496"
      :background "#0e5994"
      :weight bold
      ))
  "Face for global variables."
  :group 'my-lang-mode )


(defun mode-icon (face-value)
  (let ((family (all-the-icons-icon-family-for-buffer))
	(icon   (all-the-icons-icon-for-buffer)))
    (if (symbolp icon)
        (propertize (symbol-name icon)
                    'face `(:height 0.8 :inherit package-status-external)
                    'display '(raise 0.1))
      (propertize icon
		  'face `(:height 1.1 :family ,family :inherit ,face-value)
		  'display '(raise 0.0)
		  ))))

(defun custom-modeline-region-info (face)
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
          (chars (count-words (region-end) (region-beginning))))
      (concat
       (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
                   'face `(:family ,(all-the-icons-octicon-family) :inherit ,face)
                   'display '(raise -0.0))
       (propertize (format " (%s, %s)" words chars)
                   'face `(:height 0.9 :inherit ,face))))))

(defun -custom-modeline-github-vc (face)
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-alltheicon "git" :height 1.0 :v-adjust 0.1 :face face))
                 'display '(raise 0.1))
     (propertize " • ")
     (propertize (format "%s" (all-the-icons-octicon "git-branch" :face face))
                 'display '(raise 0.1))
     (propertize (format " %s  " branch)
                 'display '(raise 0.1) 'face `(:inherit ,face)))))

(defun custom-modeline-icon-vc (face)
  (when vc-mode
    (cond
      ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc face))
      (t (format "%s" vc-mode)))))

(defun xah-get-current-mode-str ()
  (if xah-fly-insert-state-q "INSERT  " "COMMAND "))


