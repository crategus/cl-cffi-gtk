;;; ----------------------------------------------------------------------------
;;; rtest-gtk-action-group.lisp
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

(define-test gtk-action-group
  ;; Type checks
  (assert-true  (g-type-is-object "GtkActionGroup"))
  (assert-false (g-type-is-abstract "GtkActionGroup"))
  (assert-true  (g-type-is-derived "GtkActionGroup"))
  (assert-false (g-type-is-fundamental "GtkActionGroup"))
  (assert-true  (g-type-is-value-type "GtkActionGroup"))
  (assert-true  (g-type-has-value-table "GtkActionGroup"))
  (assert-true  (g-type-is-classed "GtkActionGroup"))
  (assert-true  (g-type-is-instantiatable "GtkActionGroup"))
  (assert-true  (g-type-is-derivable "GtkActionGroup"))
  (assert-true  (g-type-is-deep-derivable "GtkActionGroup"))
  (assert-false (g-type-is-interface "GtkActionGroup"))

  ;; Check the registered name
  (assert-eq 'gtk-action-group
             (registered-object-type-by-name "GtkActionGroup"))
  
  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkActionGroup"))))
    (assert-equal (gtype "GtkActionGroup") (g-type-from-class class))
    (assert-equal (gtype "GtkActionGroup") (g-object-class-type class))
    (assert-equal "GtkActionGroup" (g-object-class-name class))
    (assert-equal (gtype "GtkActionGroup")
                  (g-type-from-class  (g-type-class-peek "GtkActionGroup")))
    (assert-equal (gtype "GtkActionGroup")
                  (g-type-from-class  (g-type-class-peek-static "GtkActionGroup")))
    (g-type-class-unref class))
  
  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-action-group)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-action-group (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkActionGroup" (gobject-class-g-type-name class))
    (assert-equal "GtkActionGroup" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_action_group_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  ;; Check some more GType information
  (assert-equal (gtype "GObject") (g-type-parent "GtkActionGroup"))
  (assert-eql 2 (g-type-depth "GtkActionGroup"))
  (assert-eql   (gtype "GtkActionGroup")
                (g-type-next-base "GtkActionGroup" "GObject"))
  (assert-false (g-type-is-a "GtkActionGroup" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkActionGroup" "gboolean"))
  (assert-false (g-type-is-a "GtkActionGroup" "GtkWindow"))

  ;; Check the children
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkActionGroup")))
  ;; Check the interfaces
  (assert-equal '("GtkBuildable")
                (mapcar #'gtype-name (g-type-interfaces "GtkActionGroup")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkActionGroup" query)
    (assert-equal (gtype "GtkActionGroup")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkActionGroup"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 88 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql 16 (foreign-slot-value query 'g-type-query :instance-size)))
    
  ;; Get the names of the class properties.
  (assert-equal
      '("name" "sensitive" "visible" "accel-group")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkActionGroup"))))
    
  ;; Get the names of the style properties.
  (assert-equal
      '()
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkActionGroup"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GtkActionGroup" GTK-ACTION-GROUP
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GtkBuildable") :TYPE-INITIALIZER
                                "gtk_action_group_get_type")
                               ((ACCEL-GROUP GTK-ACTION-GROUP-ACCEL-GROUP
                                 "accel-group" "GtkAccelGroup" T T)
                                (NAME GTK-ACTION-GROUP-NAME "name" "gchararray"
                                 T NIL)
                                (SENSITIVE GTK-ACTION-GROUP-SENSITIVE
                                 "sensitive" "gboolean" T T)
                                (VISIBLE GTK-ACTION-GROUP-VISIBLE "visible"
                                 "gboolean" T T)))
     (get-g-type-definition (gtype "GtkActionGroup")))

  (let ((group (gtk-action-group-new "ActionGroup")))
    ;; Access the properties
    (assert-eql nil (gtk-action-group-accel-group group))
    (assert-equal "ActionGroup" (gtk-action-group-name group))
    (assert-true (gtk-action-group-sensitive group))
    (assert-true (gtk-action-group-visible group)))

  ;; Check functions
  (let ((group (gtk-action-group-new "ActionGroup")))
    ;; gtk-action-group-get-name
    (assert-equal "ActionGroup" (gtk-action-group-get-name group))
    ;; gtk-action-group-get-sensitive
    (assert-true (gtk-action-group-get-sensitive group))
    ;; gtk-action-group-set-sensitive
    (gtk-action-group-set-sensitive group nil)
    (assert-false (gtk-action-group-get-sensitive group))
    (gtk-action-group-set-sensitive group t)
    (assert-true (gtk-action-group-get-sensitive group))
    ;; gtk-action-group-get-visible
    (assert-true (gtk-action-group-get-visible group))
    ;; gtk-action-group-set-visible
    (gtk-action-group-set-visible group nil)
    (assert-false (gtk-action-group-get-visible group))
    (gtk-action-group-set-visible group t)
    (assert-true (gtk-action-group-get-visible group))
    ;; gtk-action-group-get-action
    ;; gtk-action-group-list-actions
    ;; gtk-action-group-add-action
    ;; gtk-action-group-remove-action
    (let ((action1 (gtk-action-new "action1"))
          (action2 (gtk-action-new "action2"))
          (action3 (gtk-action-new "action3")))
      (gtk-action-group-add-action group action1)
      (assert-equal action1 (gtk-action-group-get-action group "action1"))
      (assert-false (gtk-action-group-get-action group "action2"))
      (assert-equal (list action1)
                    (gtk-action-group-list-actions group))
      (gtk-action-group-add-action group action2)
      (assert-equal action2 (gtk-action-group-get-action group "action2"))
      (assert-equal (list action1 action2)
                    (gtk-action-group-list-actions group))
      (gtk-action-group-add-action group action3)
      (assert-equal action3 (gtk-action-group-get-action group "action3"))
      (assert-equal (list action1 action2 action3)
                    (gtk-action-group-list-actions group))
      (gtk-action-group-remove-action group action2)
      (assert-equal (list action1 action3)
                    (gtk-action-group-list-actions group))
      (gtk-action-group-remove-action group action3)
      (assert-equal (list action1)
                    (gtk-action-group-list-actions group))
      (gtk-action-group-remove-action group action1)
      (assert-equal (list)
                    (gtk-action-group-list-actions group))
    )
    ;; gtk-action-group-add-actions
    (let ((entries (list (list "action1" nil nil nil nil nil)
                         (list "action2" nil nil nil nil nil)
                         (list "action3" nil nil nil nil nil))))
      (gtk-action-group-add-actions group entries)
      (assert-equal "action1"
                    (gtk-action-name (gtk-action-group-get-action group "action1")))
      (assert-equal "action2"
                    (gtk-action-name (gtk-action-group-get-action group "action2")))
      (assert-equal "action3"
                    (gtk-action-name (gtk-action-group-get-action group "action3")))
    )
  )
)

;;; --- End of file rtest-gtk-action-group.lisp --------------------------------
