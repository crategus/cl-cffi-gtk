;;;; gtk-tutorial.lisp

(asdf:defsystem :gtk-tutorial
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk :split-sequence)
  :components ((:file "gtk-tutorial")
               (:file "cell-renderer-properties")
               (:file "color-button-label")
               (:file "color-chooser-dialog")
               (:file "file-chooser-button")
               (:file "font-button-label")
               (:file "text-view-attributes")
               (:file "text-view-find-next")
               (:file "text-view-insert")
               (:file "text-view-insert-image")
               (:file "text-view-insert-widget")
               (:file "text-view-search")
               (:file "text-view-simple")
               (:file "text-view-tags")
               (:file "text-view-tooltip")
               (:file "tree-view-column-data-func")
               (:file "tree-view-dump-model")
               (:file "tree-view-path")
               (:file "tree-view-simple")
              ))

;;; 2021-2-23
