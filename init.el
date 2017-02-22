;; Package settings
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")   t)
(package-initialize)

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
(add-to-list 'default-frame-alist '(font . "Fira Mono 10"))
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)
(global-undo-tree-mode 1)
(show-paren-mode 1)
(global-hl-line-mode)
(setq scroll-conservatively 100)

(global-nlinum-mode 1)
(setq nlinum-format " %3d ")

;; Theme settings
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

(define-key xah-fly-key-map (kbd "C--") 'text-scale-increase)
(define-key xah-fly-key-map (kbd "C-+") 'text-scale-decrease)

(define-key xah-fly-key-map (kbd "M-;") 'xah-comment-dwim)

(define-key isearch-mode-map (kbd "M-c") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "M-t") 'isearch-repeat-forward)

(eval-after-load "nxml-mode"
  '(progn
     (define-key nxml-mode-map (kbd "M-H") 'hs-hide-level)
     (define-key nxml-mode-map (kbd "M-N") 'hs-toggle-hiding)
     (define-key nxml-mode-map (kbd "M-0") 'hs-show-all)))

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


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
  :diminish paredit-mode
  :init
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  :bind (:map paredit-mode-map
	(";" . nil)))

(use-package multiple-cursors
  :ensure t
  :config
  (progn
    (define-key mc/keymap (kbd "<escape>") 'mc/keyboard-quit)
    (global-set-key (kbd "C-8") 'mc/mark-all-like-this)))

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
  (indent-region (point-min) (point-max) nil))
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
  (buffer-disable-undo)
  (set (make-variable-buffer-local 'global-hl-line-mode) nil)
  (set (make-variable-buffer-local 'line-number-mode) nil)
  (set (make-variable-buffer-local 'column-number-mode) nil) )

(add-to-list 'magic-mode-alist (cons #'my--is-file-large #'my-large-file-mode))

;; Company
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-b") nil)
  (define-key company-active-map (kbd "M-l") nil)
  (define-key company-active-map (kbd "C-o") nil)
  (define-key company-active-map (kbd "M-t") #'company-select-next)
  (define-key company-active-map (kbd "M-c") #'company-select-previous)
  (define-key company-active-map (kbd "M-f") #'company-search-candidates))
(global-set-key (kbd "M-y") 'company-complete)

(add-hook 'after-init-hook 'global-company-mode)

(use-package expand-region
   :ensure t
   :config
   (global-set-key (kbd "M-8") 'er/expand-region))

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
		     (progn
		       (org-bullets-mode t)
		       (define-key org-mode-map (kbd "M-H") 'org-metaleft)
		       (define-key org-mode-map (kbd "M-N") 'org-metaright))))
(setq org-src-tab-acts-natively t)
(setq org-agenda-files '("f:/datalex/org/"))
(setq org-log-done 'time)

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

(require 'elpy)
(elpy-enable)
(setq Exec-path (append exec-path '("c:/Program Files (x86)/Python/Python36-32/Scripts")))
;; (elpy-use-ipython)
(setq elpy-rpc-backend "jedi")

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
))



(setq exec-path (append exec-path '("f:/app/cygwin/bin")))

(yas-global-mode 1)

(global-set-key (kbd "C-p") 'yas-expand)




