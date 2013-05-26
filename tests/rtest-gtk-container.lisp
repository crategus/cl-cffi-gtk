;;; ----------------------------------------------------------------------------
;;; rtest-container.lisp
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

(define-test gtk-container
  ;; Type checks
  (assert-true  (g-type-is-object "GtkContainer"))
  (assert-true  (g-type-is-abstract "GtkContainer"))
  (assert-true  (g-type-is-derived "GtkContainer"))
  (assert-false (g-type-is-fundamental "GtkContainer"))
  (assert-true  (g-type-is-value-type "GtkContainer"))
  (assert-true  (g-type-has-value-table "GtkContainer"))
  (assert-true  (g-type-is-classed "GtkContainer"))
  (assert-true  (g-type-is-instantiatable "GtkContainer"))
  (assert-true  (g-type-is-derivable "GtkContainer"))
  (assert-true  (g-type-is-deep-derivable "GtkContainer"))
  (assert-false (g-type-is-interface "GtkContainer"))

  ;; Check the registered name
  (assert-eq 'gtk-container
             (registered-object-type-by-name "GtkContainer"))
  
  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkContainer"))))
    (assert-equal (gtype "GtkContainer") (g-type-from-class class))
    (assert-equal (gtype "GtkContainer") (g-object-class-type class))
    (assert-equal "GtkContainer" (g-object-class-name class))
    (assert-equal (gtype "GtkContainer")
                  (g-type-from-class  (g-type-class-peek "GtkContainer")))
    (assert-equal (gtype "GtkContainer")
                  (g-type-from-class  (g-type-class-peek-static "GtkContainer")))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-container)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-container (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkContainer" (gobject-class-g-type-name class))
    (assert-equal "GtkContainer" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_container_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  ;; Check some more GType information
  (assert-equal (gtype "GtkWidget") (g-type-parent "GtkContainer"))
  (assert-eql 4 (g-type-depth "GtkContainer"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkContainer" "GObject"))
  (assert-true  (g-type-is-a "GtkContainer" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkContainer" "gboolean"))
  (assert-false (g-type-is-a "GtkContainer" "GtkWindow"))

  ;; Check the children
  (assert-equal 
       '("GtkBin" "GtkMenuShell" "GtkBox" "GtkTable" "GtkGrid" "GtkLayout"
         "GtkFixed" "GtkNotebook" "GtkPaned" "GtkTextView" "GtkTreeView"
         "GtkIconView" "GtkToolItemGroup" "GtkToolbar" "GtkToolPalette"
         "GtkSocket")
                (mapcar #'gtype-name (g-type-children "GtkContainer")))
  ;; Check the interfaces
  (assert-equal '("AtkImplementorIface" "GtkBuildable")
                (mapcar #'gtype-name (g-type-interfaces "GtkContainer")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkContainer" query)
    (assert-equal (gtype "GtkContainer")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkContainer"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 488 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  20 (foreign-slot-value query 'g-type-query :instance-size)))
    
  ;; Get the names of the class properties.
  (assert-equal
      '("name" "parent" "width-request" "height-request" "visible" "sensitive"
         "app-paintable" "can-focus" "has-focus" "is-focus" "can-default"
         "has-default" "receives-default" "composite-child" "style" "events"
         "no-show-all" "has-tooltip" "tooltip-markup" "tooltip-text" "window"
         "double-buffered" "halign" "valign" "margin-left" "margin-right"
         "margin-top" "margin-bottom" "margin" "hexpand" "vexpand"
         "hexpand-set" "vexpand-set" "expand" "border-width" "resize-mode"
         "child")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkContainer"))))
    
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "text-handle-height"
         "text-handle-width" "visited-link-color" "wide-separators"
         "window-dragging")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkContainer"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GtkContainer" GTK-CONTAINER
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable")
                                :TYPE-INITIALIZER "gtk_container_get_type")
                               ((BORDER-WIDTH GTK-CONTAINER-BORDER-WIDTH
                                 "border-width" "guint" T T)
                                (CHILD GTK-CONTAINER-CHILD "child" "GtkWidget"
                                 NIL T)
                                (RESIZE-MODE GTK-CONTAINER-RESIZE-MODE
                                 "resize-mode" "GtkResizeMode" T T)))
     (get-g-type-definition (gtype "GtkContainer")))
  
  ;; Because GtkContainer is abstract, we create a GtkBox
  (let ((widget (gtk-box-new :horizontal 6)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GtkBox") (g-object-type widget))
    (assert-equal "GtkBox" (g-object-type-name widget))
    (assert-true (g-type-is-a "GtkBox" (g-type-from-instance widget)))

    ;; Access the properties
    (assert-eql 0 (gtk-container-border-width widget))
    (assert-error 'error (gtk-container-child widget)) ; not readable
    (assert-eql :parent (gtk-container-resize-mode widget))

    ;; Get the values of style properties
    (assert-eql 0.04 (gtk-widget-style-get-property widget "cursor-aspect-ratio"))
    (assert-false (gtk-widget-style-get-property widget "cursor-color"))
    (assert-equal "" (gtk-widget-style-get-property widget "focus-line-pattern"))
    (assert-eql 1 (gtk-widget-style-get-property widget "focus-line-width"))
    (assert-eql 0 (gtk-widget-style-get-property widget "focus-padding"))
    (assert-true (gtk-widget-style-get-property widget "interior-focus"))
    (assert-eq 'gdk-color (type-of (gtk-widget-style-get-property widget "link-color")))
    (assert-eql 16 (gtk-widget-style-get-property widget "scroll-arrow-hlength"))
    (assert-eql 16 (gtk-widget-style-get-property widget "scroll-arrow-vlength"))
    (assert-false (gtk-widget-style-get-property widget "secondary-cursor-color"))
    (assert-eql 2  (gtk-widget-style-get-property widget "separator-height"))
    (assert-eql 2  (gtk-widget-style-get-property widget "separator-width"))
    (assert-eql 20 (gtk-widget-style-get-property widget "text-handle-height"))
    (assert-eql 16 (gtk-widget-style-get-property widget "text-handle-width"))
    (assert-eq 'gdk-color (type-of (gtk-widget-style-get-property widget "visited-link-color")))
    (assert-false  (gtk-widget-style-get-property widget "wide-separators"))
    (assert-false (gtk-widget-style-get-property widget "window-dragging"))
  )
  

  ;; Emit Signals

  (let ((widget (gtk-box-new :horizontal 6))
        (child (make-instance 'gtk-button))
        (result nil))
    (g-signal-connect widget "add"
      (lambda (container widget)
        (setf result "add")
        t))

    (g-signal-connect widget "check-resize"
      (lambda (widget)
        (setf result "check-resize")
        t))

    (g-signal-connect widget "remove"
      (lambda (container widget)
        (setf result "remove")
        t))

    (g-signal-connect widget "set-focus-child"
      (lambda (container widget)
        (setf result "set-focus-child")
        t))

    ;; Add the child
    (g-signal-emit widget "add" child)
    (assert-equal "add" result)
    (assert-equal (list child) (gtk-container-get-children widget))

    ;; Resize
    (g-signal-emit widget "check-resize")
    (assert-equal "check-resize" result)

    ;; Set focus child
    (g-signal-emit widget "set-focus-child" child)
    (assert-equal "set-focus-child" result)

    ;; Remove the child
    (g-signal-emit widget "remove" child)
    (assert-equal "remove" result)
    (assert-equal (list) (gtk-container-get-children widget))
  )

  ;; Call functions

  ;; gtk-container-add
  ;; gtk-container-remove
  (let ((widget (gtk-box-new :horizontal 6))
        (button1 (make-instance 'gtk-button))
        (button2 (make-instance 'gtk-button))
        (button3 (make-instance 'gtk-button)))
    (gtk-container-add widget button1)
    (assert-equal (list button1) (gtk-container-get-children widget))
    (gtk-container-add widget button2)
    (assert-equal (list button1 button2) (gtk-container-get-children widget))
    (gtk-container-add widget button3)
    (assert-equal (list button1 button2 button3)
                  (gtk-container-get-children widget))
    (gtk-container-remove widget button1)
    (assert-equal (list button2 button3) (gtk-container-get-children widget))
    (gtk-container-remove widget button2)
    (assert-equal (list button3) (gtk-container-get-children widget))
    (gtk-container-remove widget button3)
    (assert-equal (list) (gtk-container-get-children widget)))

;;;     gtk_container_add_with_properties

  ;; gtk-container-get-resize-mode and gtk-container-set-resize-mode
  (let ((widget (gtk-box-new :horizontal 6)))
    (assert-eq :parent (gtk-container-get-resize-mode widget))
    (gtk-container-set-resize-mode widget :queue)
    (assert-eq :queue (gtk-container-get-resize-mode widget)))

  ;; gtk_container_check_resize
  (let ((widget (gtk-box-new :horizontal 6))
        (result nil))
    (g-signal-connect widget "check-resize"
      (lambda (widget)
        (setf result "check-resize")))
    (gtk-container-check-resize widget)
    (assert-equal "check-resize" result))

  ;; gtk_container_foreach
  (let ((widget (gtk-box-new :horizontal 6))
        (button1 (make-instance 'gtk-button))
        (button2 (make-instance 'gtk-button))
        (button3 (make-instance 'gtk-button)))
    (gtk-container-add widget button1)
    (gtk-container-add widget button2)
    (gtk-container-add widget button3)
    (assert-equal (list button1 button2 button3)
                  (gtk-container-get-children widget))
    ;; Set the label to "text" for all buttons in the container
    (gtk-container-foreach widget
                           (lambda (child)
                             (gtk-button-set-label child "text")))

    (assert-equal "text" (gtk-button-get-label button1))
    (assert-equal "text" (gtk-button-get-label button2))
    (assert-equal "text" (gtk-button-get-label button3)))

  ;; gtk_container_get_children
  ;; See the tests for gtk-container-add and gtk-container-remove above

  ;; gtk_container_get_path_for_child
  (let ((widget (gtk-box-new :horizontal 6))
        (child (make-instance 'gtk-button))
        (path nil))
    (gtk-container-add widget child)
    (setf path  (gtk-container-get-path-for-child widget child))
    (assert-equal 'gtk-widget-path (type-of path))
    (assert-equal "GtkBox.horizontal GtkButton.button"
                  (gtk-widget-path-to-string path)))

  ;; gtk_container_set_reallocate_redraws
  (let ((widget (gtk-box-new :horizontal 6)))
    (gtk-container-set-reallocate-redraws widget t))

  ;; gtk_container_get_focus_child
  ;; gtk_container_set_focus_child
  ;; gtk_container_get_focus_vadjustment
  ;; gtk_container_set_focus_vadjustment
  ;; gtk_container_get_focus_hadjustment
  ;; gtk_container_set_focus_hadjustment
  (let ((widget (gtk-box-new :horizontal 6))
        (adjustment (make-instance 'gtk-adjustment))
        (button1 (make-instance 'gtk-button))
        (button2 (make-instance 'gtk-button))
        (button3 (make-instance 'gtk-button)))
    (gtk-container-add widget button1)
    (gtk-container-add widget button2)
    (gtk-container-add widget button3)

    (assert-false (gtk-container-get-focus-child widget))
    (gtk-container-set-focus-child widget button2)
    (assert-equal button2 (gtk-container-get-focus-child widget))

    (assert-false (gtk-container-get-focus-vadjustment widget))
    (gtk-container-set-focus-vadjustment widget adjustment)
    (assert-equal adjustment (gtk-container-get-focus-vadjustment widget))

    (assert-false (gtk-container-get-focus-hadjustment widget))
    (gtk-container-set-focus-hadjustment widget adjustment)
    (assert-equal adjustment (gtk-container-get-focus-hadjustment widget)))

;;;     gtk_container_resize_children

  ;; gtk_container_child_type
  (let ((widget (gtk-box-new :horizontal 6)))
    (assert-equal (gtype "GtkWidget") (gtk-container-child-type widget))
  )

;;;     gtk_container_child_get
;;;     gtk_container_child_set
;;;     gtk_container_child_get_property
;;;     gtk_container_child_set_property
  (let ((widget (gtk-box-new :horizontal 6))
        (child (make-instance 'gtk-button)))
    (gtk-container-add widget child)
    (assert-false (gtk-container-child-get-property widget child "expand"))
    (assert-true (gtk-container-child-get-property widget child "fill"))
    (assert-eq :start (gtk-container-child-get-property widget child "pack-type"))
    (assert-eql 0 (gtk-container-child-get-property widget child "padding"))
    (assert-eql 0 (gtk-container-child-get-property widget child "position"))

    (gtk-container-child-set-property widget child "expand" t)
    (gtk-container-child-set-property widget child "fill" nil)
    (gtk-container-child-set-property widget child "pack-type" :end)
    (gtk-container-child-set-property widget child "padding" 10)
    (gtk-container-child-set-property widget child "position" 4)

    (assert-true (gtk-container-child-get-property widget child "expand"))
    (assert-false (gtk-container-child-get-property widget child "fill"))
    (assert-eq :end (gtk-container-child-get-property widget child "pack-type"))
    (assert-eql 10 (gtk-container-child-get-property widget child "padding"))
    (assert-eql 0 (gtk-container-child-get-property widget child "position"))

    (assert-equal (list t nil :end 10 0)
                  (gtk-container-child-get widget child
                                           "expand" "fill" "pack-type" "padding" "position"))

    (gtk-container-child-set widget child "expand" nil
                                          "fill" t
                                          "pack-type" :start
                                          "padding" 0
                                          "position" 0)

    (assert-false (gtk-container-child-get-property widget child "expand"))
    (assert-true (gtk-container-child-get-property widget child "fill"))
    (assert-eq :start (gtk-container-child-get-property widget child "pack-type"))
    (assert-eql 0 (gtk-container-child-get-property widget child "padding"))
    (assert-eql 0 (gtk-container-child-get-property widget child "position"))

  )


;;;     gtk_container_child_get_valist
;;;     gtk_container_child_set_valist


;;;     gtk_container_child_notify
;;;     gtk_container_forall
;;;     gtk_container_get_border_width
;;;     gtk_container_set_border_width
;;;     gtk_container_propagate_draw
;;;     gtk_container_get_focus_chain
;;;     gtk_container_set_focus_chain
;;;     gtk_container_unset_focus_chain
;;;     gtk_container_class_find_child_property
;;;     gtk_container_class_install_child_property
;;;     gtk_container_class_list_child_properties
;;;     gtk_container_class_handle_border_width

)
