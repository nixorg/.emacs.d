;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
		     '(("jira" "** $1\n   :PROPERTIES:\n   :DESCRIPTION: $2\n   :link:     jira:$1 \n   :ID: 	   $1 \n   :END:\n$0" "jira" nil nil nil "/Users/nixorg/.emacs.d/snippets/org-mode/jira" nil nil)
		       ("el" "#+BEGIN_SRC emacs-lisp\n$0\n#+END_SRC\n" "el" nil nil nil "/Users/nixorg/.emacs.d/snippets/org-mode/el" nil nil)))


;;; Do not edit! File generated at Sat May  4 10:24:33 2019
