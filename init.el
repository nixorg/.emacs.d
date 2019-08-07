
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(defconst emacs-start-time (current-time))
(defvar file-name-handler-alist-old file-name-handler-alist)

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Fira Mono 13"))

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(package-initialize)

(org-babel-load-file "~/.emacs.d/emacs.org")

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(backup-by-copying t)
 '(bookmark-save-flag t)
 '(case-fold-search t)
 '(delete-selection-mode t)
 '(global-hl-line-mode t)
 '(icicle-ido-like-mode nil)
 '(icicle-mode nil)
 '(inhibit-startup-screen t)
 '(large-file-warning-threshold 100000000)
 '(make-backup-files nil)
 '(mc/always-run-for-all t)
 '(mc/cmds-to-run-once 'nil t)
 '(menu-bar-mode nil)
 '(ns-command-modifier 'control)
 '(ns-pop-up-frames nil)
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-wikinodes))
 '(org-wikinodes-active t)
 '(org-wikinodes-scope 'directory)
 '(package-selected-packages
   '(edit-indirect ox-pandoc htmlize pandoc-mode anki-editor lsp-intellij yaml-mode org-pdfview pdf-tools ob-ruby ob-http ob-restclient restclient vue-mode lsp-vue company-lsp lsp-ui org-jira lsp-mode flymake-python-pyflakes flymake-diagnostic-at-point osx-dictionary speed-type multi-term org-mind-map org-gcal calfw-ical calfw-org calfw org-caldav org-brain org-download org-journal org-super-agenda hydra editorconfig company-tern tern ace-window which-key web-mode visual-regexp-steroids virtualenvwrapper use-package undo-tree tramp-hdfs tide thesaurus switch-window spaceline solarized-theme smartparens realgud rainbow-mode popwin pcre2el paredit ox-hugo osx-pseudo-daemon org-bullets ob-ipython nlinum neotree multitran multiple-cursors markdown-mode magit mac-pseudo-daemon key-chord jedi highlight-symbol helm-projectile helm-describe-modes helm-descbinds helm-dash helm-ag google-translate fzf font-lock+ expand-region exec-path-from-shell emmet-mode ein dumb-jump dired+ diminish delight dash-at-point corral company-jedi cider buffer-move auto-highlight-symbol all-the-icons-dired ag))
 '(powerline-image-apple-rgb t t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values '((org-image-actual-width)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 100)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t)
 '(visible-bell t)
 '(which-key-mode t)
 '(winner-mode t))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-selection ((t (:background "#839496" :foreground "#002b36" :weight bold)))))
