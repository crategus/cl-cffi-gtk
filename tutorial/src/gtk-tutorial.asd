;;;; gtk-tutorial.lisp

(asdf:defsystem :gtk-tutorial
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk :split-sequence)
  :components ((:file "gtk-tutorial")
               ;; Chapter Multiline Text Widget
               (:file "text-view-attributes")
               (:file "text-view-find-next")
               (:file "text-view-insert")
               (:file "text-view-insert-image")
               (:file "text-view-insert-widget")
               (:file "text-view-search")
               (:file "text-view-simple")
               (:file "text-view-tags")
               (:file "text-view-tooltip")
               ;; Chapter Tree and List Widgets
               (:file "cell-renderer-properties")
               (:file "tree-view-content-type")
               (:file "tree-view-context-menu")
               (:file "tree-view-drag-and-drop")
               (:file "tree-view-dump-model")
               (:file "tree-view-editable")
               (:file "tree-view-example")
               (:file "tree-view-path")
               (:file "tree-view-simple")
               (:file "tree-view-sortable")
               (:file "icon-view-example")
               ;; Chapter Selecting Colors, Files and Fonts
               (:file "color-button-label")
               (:file "color-chooser-dialog")
               (:file "file-chooser-button")
               (:file "font-button-label")
              ))

;;; 2021-3-19
