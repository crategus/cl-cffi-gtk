;;; ----------------------------------------------------------------------------
;;; rtest-gtk-tree-model.lisp
;;;
;;; Copyright (C) 2012 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(in-package :gtk-tests)

;;; ----------------------------------------------------------------------------

(define-test gtk-tree-path
  (let ((path  (gtk-tree-path-new))
        (path1 (gtk-tree-path-new-first))
        (path2 (gtk-tree-path-new-from-string "1:2:3"))
        (path3 (gtk-tree-path-new-from-indices 1 2 3)))
    (assert-false (gtk-tree-path-to-string path))
    (assert-equal "0" (gtk-tree-path-to-string path1))
    (assert-equal "1:2:3" (gtk-tree-path-to-string path2))
    (assert-equal "1:2:3" (gtk-tree-path-to-string path3))
    (assert-equal "1:2:3:4"
                  (gtk-tree-path-to-string (gtk-tree-path-append-index path2 4)))
    (assert-equal "0:1:2:3"
                  (gtk-tree-path-to-string (gtk-tree-path-prepend-index path2 0)))
    (assert-eql 1 (gtk-tree-path-get-depth path1))
    (assert-eql 3 (gtk-tree-path-get-depth path2))
    (assert-eql 3 (gtk-tree-path-get-depth path3))
    (assert-equal '(0) (gtk-tree-path-get-indices path1))
    (assert-equal '(1 2 3) (gtk-tree-path-get-indices path2))
    (assert-eql 0 (gtk-tree-path-compare path2 path3))
    (assert-eql -1 (gtk-tree-path-compare path1 path2))
    (assert-eql 1 (gtk-tree-path-compare path2 path1))
    (assert-equal "1:2:4"
                  (gtk-tree-path-to-string (gtk-tree-path-next path2)))

    (assert-equal "1:2:3"
                  (gtk-tree-path-to-string (gtk-tree-path-prev path2)))
    (assert-equal "1:2"
                  (gtk-tree-path-to-string (gtk-tree-path-up path2)))
    (assert-equal "1:2:0"
                  (gtk-tree-path-to-string (gtk-tree-path-down path2)))
    ;; TODO: Check the following tests
    (assert-false (gtk-tree-path-is-ancestor path2
                                             (gtk-tree-path-next path3)))
    (assert-false (gtk-tree-path-is-ancestor path2
                                             (gtk-tree-path-prev path3)))
    (assert-false (gtk-tree-path-is-descendant path2
                                               (gtk-tree-path-next path3)))
    (assert-false (gtk-tree-path-is-descendant path2
                                               (gtk-tree-path-prev path3)))

  ))


(define-test gtk-tree-model
  ;; Type checks
  (assert-false (g-type-is-object "GtkTreeModel"))
  (assert-false (g-type-is-abstract "GtkTreeModel"))
  (assert-true  (g-type-is-derived "GtkTreeModel"))
  (assert-false (g-type-is-fundamental "GtkTreeModel"))
  (assert-true  (g-type-is-value-type "GtkTreeModel"))
  (assert-true  (g-type-has-value-table "GtkTreeModel"))
  (assert-false (g-type-is-classed "GtkTreeModel"))
  (assert-false (g-type-is-instantiatable "GtkTreeModel"))
  (assert-true  (g-type-is-derivable "GtkTreeModel"))
  (assert-false (g-type-is-deep-derivable "GtkTreeModel"))
  (assert-true  (g-type-is-interface "GtkTreeModel"))

  ;; Check the registered name
  (assert-eq 'gtk-tree-model
             (registered-object-type-by-name "GtkTreeModel"))
  
  ;; Check infos about the interface
  (let ((class (find-class 'gtk-tree-model)))
    ;; Check the interface name and type of the interface
    (assert-eq 'gtk-tree-model (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkTreeModel" (gobject-class-g-type-name class))
    (assert-equal "GtkTreeModel" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_tree_model_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-true  (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GInterface") (g-type-parent "GtkTreeModel"))
  (assert-eql 2 (g-type-depth "GtkTreeModel"))
  (assert-eql   nil
                (g-type-next-base "GtkTreeModel" "GObject"))
  (assert-true  (g-type-is-a "GtkTreeModel" "GObject"))
  (assert-false (g-type-is-a "GtkTreeModel" "gboolean"))
  (assert-false (g-type-is-a "GtkTreeModel" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkTreeModel")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GtkTreeModel")))
  
  ;; Get the names of the interface properties
  (assert-equal
       '()
     (mapcar #'param-spec-name
             (g-object-interface-list-properties (gtype "GtkTreeModel"))))

  ;; Get the interface definition
  (assert-equal
     '(DEFINE-G-INTERFACE "GtkTreeModel"
            GTK-TREE-MODEL
            (:EXPORT T :TYPE-INITIALIZER "gtk_tree_model_get_type"))
     (get-g-type-definition (gtype "GtkTreeModel")))

  ;; Create an instance of GtkListStore
  (let ((model (make-instance 'gtk-list-store
                              :column-types '("gint" "gdouble" "gchararray"))))
    ;; Add some data to the list store
    (gtk-list-store-set model (gtk-list-store-append model) 0 0.0d0 "null")
    (gtk-list-store-set model (gtk-list-store-append model) 1 1.0d0 "eins")
    (gtk-list-store-set model (gtk-list-store-append model) 2 2.0d0 "zwei")
    ;; Check functions
    (assert-equal '(:ITERS-PERSIST :LIST-ONLY) (gtk-tree-model-get-flags model))
    (assert-eql 3 (gtk-tree-model-get-n-columns model))
    (assert-equal "gint" (gtype-name (gtk-tree-model-get-column-type model 0)))
    (assert-equal "gdouble" (gtype-name (gtk-tree-model-get-column-type model 1)))
    (assert-equal "gchararray" (gtype-name (gtk-tree-model-get-column-type model 2)))
    (assert-eq 'gtk-tree-iter (type-of (gtk-tree-model-get-iter model (gtk-tree-path-new-first))))
    (assert-eq 'gtk-tree-iter (type-of (gtk-tree-model-get-iter-first model)))
    (assert-eq 'gtk-tree-iter (type-of (gtk-tree-model-get-iter-from-string model "0")))
    (let ((iter (gtk-tree-model-get-iter-first model)))
      (assert-equal "0"
                    (gtk-tree-path-to-string (gtk-tree-model-get-path model iter)))
      (assert-equal "0" (gtk-tree-model-get-string-from-iter model iter))
      (assert-eql 0 (gtk-tree-model-get-value model iter 0))
      (assert-eql 0.0d0 (gtk-tree-model-get-value model iter 1))
      (assert-equal "null" (gtk-tree-model-get-value model iter 2)))
    (let ((iter (gtk-tree-model-iter-next model (gtk-tree-model-get-iter-first model))))
      (assert-equal "1"
                    (gtk-tree-path-to-string (gtk-tree-model-get-path model iter)))
      (assert-equal "1" (gtk-tree-model-get-string-from-iter model iter))
      (assert-eql 1 (gtk-tree-model-get-value model iter 0))
      (assert-eql 1.0d0 (gtk-tree-model-get-value model iter 1))
      (assert-equal "eins" (gtk-tree-model-get-value model iter 2)))
    (let ((iter (gtk-tree-model-iter-previous model (gtk-tree-model-get-iter-from-string model "1"))))
      (assert-equal "0"
                    (gtk-tree-path-to-string (gtk-tree-model-get-path model iter)))
      (assert-equal "0" (gtk-tree-model-get-string-from-iter model iter))
      (assert-eql 0 (gtk-tree-model-get-value model iter 0))
      (assert-eql 0.0d0 (gtk-tree-model-get-value model iter 1))
      (assert-equal "null" (gtk-tree-model-get-value model iter 2)))
  )
)

