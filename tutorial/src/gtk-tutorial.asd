;;;; gtk-tutorial.lisp

(asdf:defsystem :gtk-tutorial
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk :split-sequence)
  :components ((:file "gtk-tutorial")
               (:file "text-view-attributes")
               (:file "text-view-find-next")
               (:file "text-view-insert")
               (:file "text-view-insert-image")
               (:file "text-view-insert-widget")
               (:file "text-view-search")
               (:file "text-view-simple")
               (:file "text-view-tags")
               (:file "text-view-tooltip")
               (:file "tree-view-dump-model")
               (:file "tree-view-path")
               (:file "tree-view-simple")
              ))

;;; 2021-2-21
