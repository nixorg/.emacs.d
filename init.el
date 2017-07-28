(require 'package)
(server-start)

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

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-by-copying t)

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

(global-set-key (kbd "C-0") 'text-scale-increase)
(global-set-key (kbd "C-9") 'text-scale-decrease)

(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
  (define-key isearch-mode-map (kbd "<left>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<right>") 'isearch-ring-advance )

  (define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)

  (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer))


(define-key minibuffer-local-map (kbd "M-n") 'nil)
(define-key minibuffer-local-map (kbd "M-n") 'forward-char)
(define-key minibuffer-local-map (kbd "M-H") 'previous-history-element)
(define-key minibuffer-local-map (kbd "M-N") 'next-history-element)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(eval-after-load "nxml-mode"
  '(progn
     (define-key nxml-mode-map (kbd "H") 'my-hs-hide-level)
     (define-key nxml-mode-map (kbd "N") 'my-hs-toggle-hiding)
     (define-key nxml-mode-map (kbd "C-0") 'hs-show-all)))

;;; test stuff
(defun custom-commands ()
  (interactive)
  (if (equal major-mode 'dired-mode)
      (progn (define-key xah-fly-key-map (kbd "r") 'helm-find-files))
    (message "dired disable")))

;; (add-hook 'xah-fly-command-mode-activate-hook 'custom-commands)

;; Ergoemacs like keys
;; (global-set-key (kbd "M-c") 'previous-line)
;; (global-set-key (kbd "M-h") 'backward-char)
;; (global-set-key (kbd "M-t") 'next-line)
(global-set-key (kbd "M-n") 'forward-char)
(global-set-key (kbd "M-g") 'backward-word)
(global-set-key (kbd "M-r") 'forward-word)
(global-set-key (kbd "M-d") 'xah-beginning-of-line-or-block)
(global-set-key (kbd "M-s") 'xah-end-of-line-or-block)
(global-set-key (kbd "M-e") 'delete-backward-char)
(global-set-key (kbd "M-u") 'delete-forward-char)
(global-set-key (kbd "M-.") 'backward-kill-word)
(global-set-key (kbd "M-p") 'kill-word)
(global-set-key (kbd "M-c") 'move-line-up)
(global-set-key (kbd "M-t") 'move-line-down)
(global-set-key (kbd "M-k") 'xah-paste-or-paste-previous)
(global-set-key (kbd "M-j") 'xah-copy-line-or-region)
(global-set-key (kbd "M-;") 'undo)
(global-set-key (kbd "M-z") 'xah-comment-dwim)
(global-set-key (kbd "M-:") 'undo-tree-redo)

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

  (if xah-fly-insert-state-q
      (xah-fly-command-mode-activate)
    (progn
      (if (bound-and-true-p multiple-cursors-mode)
	  (mc/keyboard-quit)
	(keyboard-quit)))
    ))

(use-package nlinum
  :ensure t
  :config
  (progn
    (setq nlinum-format " %3d ")
    ;; (add-hook 'prog-mode-hook 'nlinum-mode)
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
  (helm-mode)
  (setq helm-split-window-in-side-p t)
  ;(setq helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 40)
  (helm-autoresize-mode 1)
  :bind (("C-f" . helm-occur)
	 :map helm-map
	 ("M-c" . helm-previous-line)
	 ("M-t" . helm-next-line)
	 :map xah-fly-key-map
	 ("C-r" . helm-find-files)))

(use-package helm-descbinds
  :ensure t
  :config (helm-descbinds-mode))

(use-package helm-describe-modes
  :ensure t
  :config (global-set-key [remap describe-mode] #'helm-describe-modes))

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
	      ("M-;" . nil))
  :config
  (define-key paredit-mode-map (kbd "C-,") 'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "C-<") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C->") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-p") 'paredit-raise-sexp)
  (define-key paredit-mode-map (kbd "C-S-r") 'paredit-forward)
  (define-key paredit-mode-map (kbd "C-S-g") 'paredit-backward)
  ;; (define-key paredit-mode-map (kbd "C-S-t") 'paredit-forward-up)
  ;; (define-key paredit-mode-map (kbd "C-S-c") 'paredit-backward-up)
  (define-key paredit-mode-map (kbd "C-<return>") 'paredit-close-new-line-custom)

  (defun paredit-close-new-line-custom ()
    (interactive)
    (paredit-close-round)
    (newline-and-indent)))

(use-package multiple-cursors
  :ensure t
  :config
  (progn
    (setq mc/always-run-for-all t)
    (define-key mc/keymap (kbd "<escape>") 'mc/keyboard-quit)
    (global-set-key (kbd "C-8") 'mc/mark-all-like-this)
    (global-set-key (kbd "M-8") 'vr/mc-mark)))

;; (global-set-key (kbd "C-f") 'phi-search)
;; (global-set-key (kbd "C-F") 'phi-search-backward)
;; (global-set-key (kbd "C-F") 'phi-search-again-or-next)

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))


;; ;; Custom defuns
;; (defun move-text-internal (arg)
;;   (cond
;;    ((and mark-active transient-mark-mode)
;;     (if (> (point) (mark))
;; 	(exchange-point-and-mark))
;;     (let ((column (current-column))
;; 	  (text (delete-and-extract-region (point) (mark))))
;;       (forward-line arg)
;;       (move-to-column column t)
;;       (set-mark (point))
;;       (insert text)
;;       (exchange-point-and-mark)
;;       (setq deactivate-mark nil)))
;;    (t
;;     (beginning-of-line)
;;     (when (or (> arg 0) (not (bobp)))
;;       (message "forward %s" arg)
;;       (forward-line)
;;       (when (or (< arg 0) (not (eobp)))
;; 	(transpose-lines arg))
;;       (forward-line -1)))))

;; (defun move-text-down (arg)
;;   "Move region (transient-mark-mode active) or current line
;;   arg lines down."
;;   (interactive "*p")
;;   (move-text-internal arg))

;; (defun move-text-up (arg)
;;   "Move region (transient-mark-mode active) or current line
;;   arg lines up."
;;   (interactive "*p")
;;   (move-text-internal (- arg)))

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
  (large-file-mode)
  (xah-paste-or-paste-previous)
  (indent-xml))

;; Large file performance improvement
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 200)

(defun my--is-file-large ()
  "If buffer too large and my cause performance issue."
  (< large-file-warning-threshold (buffer-size)))

(define-derived-mode large-file-mode fundamental-mode "LargeFile"
  "Fixes performance issues in Emacs for large files."
  ;; (setq buffer-read-only t)
  (setq bidi-display-reordering nil)
  (jit-lock-mode nil)
  ;; (buffer-disable-undo)
  (set (make-variable-buffer-local 'global-hl-line-mode) nil)
  (set (make-variable-buffer-local 'line-number-mode) nil)
  (set (make-variable-buffer-local 'column-number-mode) nil))

(add-to-list 'magic-mode-alist (cons #'my--is-file-large #'large-file-mode))

(defadvice xah-paste-or-paste-previous (before large-file-paste activate)
  (large-file-paste))

(defun large-file-paste ()
  (interactive)
  (let (text len)
    (setq text (car kill-ring))
    (setq len (length text))
    (message "length %d" len)
    (if (> len 10000)
	(large-file-mode))))

;; Company
(use-package company
  :ensure t
  :config
  (progn
    (with-eval-after-load 'company
      ;; (company-quickhelp-mode)
      ;; (setq company-quickhelp-delay 1.0)
      (define-key company-active-map (kbd "M-b") nil)
      (define-key company-active-map (kbd "M-l") nil)
      (define-key company-active-map (kbd "C-o") nil)
      (define-key company-active-map (kbd "M-t") #'company-select-next)
      (define-key company-active-map (kbd "M-c") #'company-select-previous)
      (define-key company-active-map (kbd "M-f") #'company-search-candidates))
    (global-set-key (kbd "C-y") 'company-complete)

    (add-hook 'after-init-hook 'global-company-mode)
    ))


(use-package expand-region
  :ensure t)

;; (use-package help-fns+ ; Improved help commands
;;   :commands (describe-buffer describe-command describe-file
;; 			     describe-keymap describe-option describe-option-of-type))

(use-package help-fns+
  :ensure t
  :disabled)

;; (use-package ace-jump-mode
;;   :ensure t
;;   :config
;;   (define-key global-map (kbd "M-i") 'ace-jump-mode))

;; org mode
(use-package ob-ipython
  :ensure t)
(setq org-confirm-babel-evaluate nil)   ;don't prompt me to confirm everytime I want to evaluate a block
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(setq org-startup-with-inline-images t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (ipython . t)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (progn
	      (org-bullets-mode t)
	      (define-key org-mode-map (kbd "M-H") 'org-metaleft)
	      (define-key org-mode-map (kbd "M-N") 'org-metaright))))
(setq org-src-tab-acts-natively t)
(setq org-agenda-files '("d:/datalex/doc/org/"))
(setq org-log-done 'time)
(setq org-src-fontify-natively t)

(xah-fly--define-keys
 (define-prefix-command 'kde-org-keymap)
 '(
   ("e" . org-edit-special)
   ("a" . org-agenda)
   ("t" . org-toggle-checkbox)
   ))

(defun replace-char (arg)
  (interactive
   (list
    (read-char-exclusive)))
  (delete-char 1)
  (insert arg))

(use-package smartparens
  :ensure t
  :config
  (add-hook 'python-mode-hook #'smartparens-mode)
  (add-hook 'org-mode-hook #'smartparens-mode))


;; Python config
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))


(use-package elpy
  :ensure t
  :config
  (progn
    (elpy-enable)
    ;; (setq Exec-path (append exec-path '("c:/Program Files (x86)/Python3/Scripts")))
    (setq Exec-path (append exec-path '("c:/Users/Yauheni_Kuzmianok/.virtualenv/Scripts")))
    (pyvenv-activate "~/.virtualenv")
    (elpy-use-ipython)
    (setenv "PYTHONIOENCODING" "UTF-8")
    (setq elpy-rpc-backend "jedi")
    (setq jedi:complete-on-dot t)
    (setq jedi:setup-keys t)
    ))

(use-package realgud
  :ensure t)

(require 'cl)

(use-package ein
  :ensure t
  :config
  (progn
    (require 'websocket)
    ;; Use Jedi with EIN
    (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
    (setq ein:default-url-or-port "http://localhost:8888"
          ein:output-type-preference '(emacs-lisp svg png jpeg
                                                  html text latex javascript))
    )
  )

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenv/"))

(defun prelude-personal-python-mode-defaults ()
  "Personal defaults for Python programming."
  ;; Enable elpy mode
  (elpy-mode)
  (smartparens-mode)
  ;; Jedi backend
  ;; (jedi:setup)
  ;; (setq jedi:complete-on-dot t) ;optional
  ;; (auto-complete-mode)
  ;; (jedi:ac-setup)
					;(setq elpy-rpc-python-command "python3")
  ;; (python-shell-interpreter "ipython3")
  )

(setq prelude-personal-python-mode-hook 'prelude-personal-python-mode-defaults)


(add-hook 'python-mode-hook (lambda ()
			      ;(electric-pair-mode 1)
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

;(setq exec-path (append exec-path '("d:/app/cygwin/bin")))

;; Add yasnippet support
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(define-key yas-keymap (kbd "C-d") 'yas-skip-and-clear-or-delete-char)

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
  :ensure t
  :config
  (setq vr/engine 'pcre2el))
(use-package pcre2el
  :ensure t)

(use-package spaceline
  :ensure t
  :config
  (progn
    (require 'spaceline-config)
    ;; (spaceline-emacs-theme)
    (spaceline-helm-mode)
    ;; (setq powerline-default-separator 'wave)
    (spaceline-compile)
    ))

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
    (if (not (symbolp icon))
        ;; (propertize (symbol-name icon)
                    ;; 'face `(:height 0.8 :inherit ,face-value)
                    ;; 'display '(raise 0.1))
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

(use-package auto-highlight-symbol
  :ensure t
  :config
  (global-auto-highlight-symbol-mode t))

(use-package highlight-symbol
  :ensure t)

(use-package google-translate
  :ensure t
  :config
  (setq google-translate-default-source-language "en")
 (setq google-translate-default-target-language "ru"))

(use-package multitran
  :ensure t)

(defun multitran-custom ()
  (interactive)
  (multitran--word (thing-at-point 'word)))

(use-package thesaurus
  :ensure t
  :config
  (setq thesaurus-bhl-api-key "72dd7311ba167ef0ae7d2c1585959e6b")

  (defun thesaurus-fetch-synonyms (word)
    "fetch synonyms for the given word, from a remote source."
    (let ((synonym-list nil)
	  (buf (thesaurus-get-buffer-for-word word)))
      (if buf
	  (progn
	    (with-current-buffer buf
	      (rename-buffer (concat "*thesaurus* - " word) t)
	      (goto-char (point-min))
	      (thesaurus-process-http-headers)
	      (while (not (= (point-min) (point-max)))
		(let ((elt (thesaurus-parse-one-line)))
		  (if elt
		      (add-to-list 'synonym-list elt)))))
	    (kill-buffer buf)
	    (nreverse synonym-list)
	    )))))

(defun my-new-line-and-indent ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun my-new-line-and-indent-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))


(defun custom-eval-single ()
  (interactive)
  (if (or
       (string-equal major-mode "xah-elisp-mode")
       (string-equal major-mode "emacs-lisp-mode")
       (string-equal major-mode "lisp-mode")
       (string-equal major-mode "lisp-interaction-mode")
       (string-equal major-mode "common-lisp-mode")
       (string-equal major-mode "clojure-mode")
       (string-equal major-mode "xah-clojure-mode")
       (string-equal major-mode "scheme-mode"))
      (eval-defun nil)
    (if (or (eq major-mode 'js2-mode)
	    (eq major-mode 'html-mode)
	    (eq major-mode 'css-mode))
	(skewer-html-eval-tag))
    (if (eq major-mode 'python-mode)
	(elpy-shell-send-region-or-buffer))))

(defun custom-eval-double ()
  (interactive)
  (if (or
       (string-equal major-mode "xah-elisp-mode")
       (string-equal major-mode "emacs-lisp-mode")
       (string-equal major-mode "lisp-mode")
       (string-equal major-mode "lisp-interaction-mode")
       (string-equal major-mode "common-lisp-mode")
       (string-equal major-mode "clojure-mode")
       (string-equal major-mode "xah-clojure-mode")
       (string-equal major-mode "scheme-mode"))
      (eval-region (region-beginning) (region-end) t)
    (if (eq major-mode 'js2-mode)
	(skewer-eval-last-expression))
    (if (eq major-mode 'python-mode)
	(elpy-shell-send-current-statement))))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define xah-fly-key-map "``" 'custom-eval-double))

(use-package flycheck
  :ensure t
  :config
  ;; (global-flycheck-mode)
  )

(use-package corral
  :ensure t)

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (push "*multitran*" popwin:special-display-config))

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
d  (next-line 1)
  (yank)>
)
(global-set-key (kbd "C-d") 'duplicate-line)

(use-package dumb-jump
  :ensure t)

(use-package buffer-move
  :ensure t
  :config
  (define-key xah-fly-key-map (kbd "C-S-c") 'buf-move-up)
  (define-key xah-fly-key-map (kbd "C-S-t") 'buf-move-down)
  (define-key xah-fly-key-map (kbd "C-S-h") 'buf-move-left)
  (define-key xah-fly-key-map (kbd "C-S-n") 'buf-move-right)
  )

(use-package switch-window
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

;;; Web dev configuration

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode))

(use-package js2-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'js2-mode-hook 'smartparens-mode))

(use-package skewer-mode
  :ensure t
  :config
  (add-hook 'html-mode-hook 'skewer-html-mode)
  (add-hook 'js2-mode 'skewer-mode)
  (add-hook 'css-mode 'skewer-mode)
  (define-key html-mode-map (kbd "C-c C-c") 'skewer-html-eval-tag))

(use-package web-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'smartparens-mode)
  (sp-with-modes '(web-mode)
    (sp-local-pair "{% "  " %}")
    (sp-local-pair "<p> "  " </p>")
    (sp-local-pair "{% "  " %}")
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>")))

