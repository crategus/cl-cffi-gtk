(defpackage :gtk
  (:use :cl :cffi :gobject :gdk :glib :iter :pango)
  (:export #:get-major-version
           #:get-minor-version
           #:get-micro-version
           #:get-binary-age
           #:get-interface-age
           #:check-version
           
           #:gtk-main
           #:dialog-run
           #:object-destroy
           #:text-buffer-insert
           #:define-child-property
           #:container-class-child-properties
           #:generate-child-properties
           #:tree-lisp-store
           #:tree-lisp-store-root
           #:tree-node
           #:make-tree-node
           #:tree-node-tree
           #:tree-node-parent
           #:tree-node-id
           #:tree-node-item
           #:tree-node-children
           #:tree-node-insert-at
           #:tree-node-remove-at
           #:tree-node-child-at
           #:tree-lisp-store-add-column
           #:gtk-main-add-timeout
           #:gtk-call-aborted
           #:gtk-call-aborted-condition
           #:let-ui))

(defpackage :gtk-examples
  (:use :cl :gtk :gdk :gobject)
  (:export #:test-dialog))

(in-package :gtk)

#+sbcl (when (and (find-package "SB-EXT")
                  (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
         (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")) :traps nil))
