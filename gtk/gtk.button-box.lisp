;;; ----------------------------------------------------------------------------
;;; gtk.button-box.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;;
;;; GtkButtonBox
;;;
;;; A container for arranging buttons
;;;
;;; Synopsis
;;;
;;;     GtkButtonBox
;;;
;;;     gtk_button_box_new
;;;     gtk_button_box_get_layout
;;;     gtk_button_box_get_child_secondary
;;;     gtk_button_box_get_child_non_homogeneous
;;;     gtk_button_box_set_layout
;;;     gtk_button_box_set_child_secondary
;;;     gtk_button_box_set_child_non_homogeneous
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkButtonBox
;;;                                  +----GtkHButtonBox
;;;                                  +----GtkVButtonBox
;;;
;;; Implemented Interfaces
;;;
;;; GtkButtonBox implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; Properties
;;;
;;;   "layout-style"             GtkButtonBoxStyle     : Read / Write
;;;
;;; Child Properties
;;;
;;;   "non-homogeneous"          gboolean              : Read / Write
;;;   "secondary"                gboolean              : Read / Write
;;;
;;; Style Properties
;;;
;;;   "child-internal-pad-x"     gint                  : Read
;;;   "child-internal-pad-y"     gint                  : Read
;;;   "child-min-height"         gint                  : Read
;;;   "child-min-width"          gint                  : Read
;;;
;;; Description
;;;
;;; A button box should be used to provide a consistent layout of buttons
;;; throughout your application. The layout/spacing can be altered by the
;;; programmer, or if desired, by the user to alter the 'feel' of a program to a
;;; small degree.
;;;
;;; gtk_button_box_get_layout() and gtk_button_box_set_layout() retrieve and
;;; alter the method used to spread the buttons in a button box across the
;;; container, respectively.
;;;
;;; The main purpose of GtkButtonBox is to make sure the children have all the
;;; same size. GtkButtonBox gives all children the same size, but it does allow
;;; 'outliers' to keep their own larger size. To force all children to be
;;; strictly the same size without exceptions, you can set the "homogeneous"
;;; property to TRUE.
;;;
;;; To excempt individual children from homogeneous sizing regardless of their
;;; 'outlier' status, you can set the "non-homogeneous" child property.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "layout-style" property
;;;
;;;   "layout-style"             GtkButtonBoxStyle     : Read / Write
;;;
;;; How to lay out the buttons in the box. Possible values are: spread, edge,
;;; start and end.
;;;
;;; Default value: GTK_BUTTONBOX_EDGE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Child Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "non-homogeneous" child property
;;;
;;;   "non-homogeneous"          gboolean              : Read / Write
;;;
;;; If TRUE, the child will not be subject to homogeneous sizing.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary" child property
;;;
;;;   "secondary"                gboolean              : Read / Write
;;;
;;; If TRUE, the child appears in a secondary group of children, suitable for,
;;; e.g., help buttons.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "child-internal-pad-x" style property
;;;
;;;   "child-internal-pad-x"     gint                  : Read
;;;
;;; Amount to increase child's size on either side.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 4
;;;
;;; ----------------------------------------------------------------------------
;;; The "child-internal-pad-y" style property
;;;
;;;   "child-internal-pad-y"     gint                  : Read
;;;
;;; Amount to increase child's size on the top and bottom.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "child-min-height" style property
;;;
;;;   "child-min-height"         gint                  : Read
;;;
;;; Minimum height of buttons inside the box.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 27
;;;
;;; ----------------------------------------------------------------------------
;;; The "child-min-width" style property
;;;
;;;   "child-min-width"          gint                  : Read
;;;
;;; Minimum width of buttons inside the box.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 85
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkButtonBox
;;;
;;; struct GtkButtonBox;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkButtonBox" 'gtk-button-box))

(define-g-object-class "GtkButtonBox" gtk-button-box
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_button_box_get_type")
  ((layout-style
    gtk-button-box-layout-style
    "layout-style" "GtkButtonBoxStyle" t t)))

;;; ----------------------------------------------------------------------------

(define-child-property "GtkButtonBox"
                       gtk-button-box-child-non-homogeneous
                       "non-homogeneous" "gboolean" t t t)

(define-child-property "GtkButtonBox"
                       gtk-button-box-child-secondary
                       "secondary" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_new ()
;;;
;;; GtkWidget * gtk_button_box_new (GtkOrientation orientation);
;;;
;;; Creates a new GtkButtonBox.
;;;
;;; orientation :
;;;     the box' orientation.
;;;
;;; Returns :
;;;     a new GtkButtonBox.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-new))

(defun gtk-button-box-new (orientation)
  (make-instance 'gtk-button-box
                 :orientation orientation))

(export 'gtk-button-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_get_layout ()
;;;
;;; GtkButtonBoxStyle gtk_button_box_get_layout (GtkButtonBox *widget);
;;;
;;; Retrieves the method being used to arrange the buttons in a button box.
;;;
;;; widget :
;;;     a GtkButtonBox
;;;
;;; Returns :
;;;     the method used to lay out buttons in widget.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-get-layout))

(defun gtk-button-box-get-layout (widget)
  (gtk-button-box-layout-style widget))

(export 'gtk-button-box-get-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_get_child_secondary ()
;;;
;;; gboolean gtk_button_box_get_child_secondary (GtkButtonBox *widget,
;;;                                              GtkWidget *child);
;;;
;;; Returns whether child should appear in a secondary group of children.
;;;
;;; widget :
;;;     a GtkButtonBox
;;;
;;; child :
;;;     a child of widget
;;;
;;; Returns :
;;;     whether child should appear in a secondary group of children.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-get-child-secondary))

(defun gtk-button-box-get-child-secondary (widget child)
  (gtk-button-box-child-secondary widget child))

(export 'gtk-button-box-get-child-secondary)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_get_child_non_homogeneous ()
;;;
;;; gboolean gtk_button_box_get_child_non_homogeneous (GtkButtonBox *widget,
;;;                                                    GtkWidget *child);
;;;
;;; Returns whether the child is exempted from homogenous sizing.
;;;
;;; widget :
;;;     a GtkButtonBox
;;;
;;; child :
;;;     a child of widget
;;;
;;; Returns :
;;;     TRUE if the child is not subject to homogenous sizing
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-get-child-non-homogeneous))

(defun gtk-button-box-get-child-non-homogeneous (widget child)
  (gtk-button-box-child-non-homogeneous widget child))

(export 'gtk-button-box-get-child-non-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_set_layout ()
;;;
;;; void gtk_button_box_set_layout (GtkButtonBox *widget,
;;;                                 GtkButtonBoxStyle layout_style);
;;;
;;; Changes the way buttons are arranged in their container.
;;;
;;; widget :
;;;     a GtkButtonBox
;;;
;;; layout_style :
;;;     the new layout style
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-set-layout))

(defun gtk-button-box-set-layout (widget layout-style)
  (setf (gtk-button-box-layout-style widget) layout-style))

(export 'gtk-button-box-set-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_set_child_secondary ()
;;;
;;; void gtk_button_box_set_child_secondary (GtkButtonBox *widget,
;;;                                          GtkWidget *child,
;;;                                          gboolean is_secondary);
;;;
;;; Sets whether child should appear in a secondary group of children. A
;;; typical use of a secondary child is the help button in a dialog.
;;;
;;; This group appears after the other children if the style is
;;; GTK_BUTTONBOX_START, GTK_BUTTONBOX_SPREAD or GTK_BUTTONBOX_EDGE, and before
;;; the other children if the style is GTK_BUTTONBOX_END. For horizontal button
;;; boxes, the definition of before/after depends on direction of the widget
;;; (see gtk_widget_set_direction()). If the style is GTK_BUTTONBOX_START or
;;; GTK_BUTTONBOX_END, then the secondary children are aligned at the other end
;;; of the button box from the main children. For the other styles, they appear
;;; immediately next to the main children.
;;;
;;; widget :
;;;     a GtkButtonBox
;;;
;;; child :
;;;     a child of widget
;;;
;;; is_secondary :
;;;     if TRUE, the child appears in a secondary group of the button box.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-set-child-secondary))

(defun gtk-button-box-set-child-secondary (widget child is-secondary)
  (setf (gtk-button-box-child-secondary widget child) is-secondary))

(export 'gtk-button-box-set-child-secondary)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_set_child_non_homogeneous ()
;;;
;;; void gtk_button_box_set_child_non_homogeneous (GtkButtonBox *widget,
;;;                                                GtkWidget *child,
;;;                                                gboolean non_homogeneous);
;;;
;;; Sets whether the child is exempted from homogeous sizing.
;;;
;;; widget :
;;;     a GtkButtonBox
;;;
;;; child :
;;;     a child of widget
;;;
;;; non_homogeneous :
;;;     the new value
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-set-child-non-homogeneous))

(defun gtk-button-box-set-child-non-homogeneous (widget child non-homogeneous)
  (setf (gtk-button-box-child-non-homogeneous widget child) non-homogeneous))

(export 'gtk-button-box-set-child-non-homogeneous)

;;; ----------------------------------------------------------------------------
;;; GtkHButtonBox
;;;
;;; A container for arranging buttons horizontally
;;;
;;; Synopsis
;;;
;;;     GtkHButtonBox
;;;
;;;     gtk_hbutton_box_new
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkButtonBox
;;;                                  +----GtkHButtonBox
;;;
;;; Implemented Interfaces
;;;
;;; GtkHButtonBox implements AtkImplementorIface, GtkBuildable and
;;; GtkOrientable.
;;;
;;; Description
;;;
;;; A button box should be used to provide a consistent layout of buttons
;;; throughout your application. The layout/spacing can be altered by the
;;; programmer, or if desired, by the user to alter the 'feel' of a program to a
;;; small degree.
;;;
;;; A GtkHButtonBox is created with gtk_hbutton_box_new(). Buttons are packed
;;; into a button box the same way widgets are added to any other container,
;;; using gtk_container_add(). You can also use gtk_box_pack_start() or
;;; gtk_box_pack_end(), but for button boxes both these functions work just like
;;; gtk_container_add(), ie., they pack the button in a way that depends on the
;;; current layout style and on whether the button has had
;;; gtk_button_box_set_child_secondary() called on it.
;;;
;;; The spacing between buttons can be set with gtk_box_set_spacing(). The
;;; arrangement and layout of the buttons can be changed with
;;; gtk_button_box_set_layout().
;;;
;;; GtkHButtonBox has been deprecated, use GtkButtonBox instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHButtonBox
;;;
;;; struct GtkHButtonBox;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkHButtonBox" 'gtk-hbutton-box))

(define-g-object-class "GtkHButtonBox" gtk-hbutton-box
  (:superclass gtk-button-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_hbutton_box_get_type")
  nil)

;;; ----------------------------------------------------------------------------

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-position
                       "position" "gint" t t t)

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-secondary
                       "secondary" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_hbutton_box_new ()
;;;
;;; GtkWidget * gtk_hbutton_box_new (void);
;;;
;;; Warning
;;;
;;; gtk_hbutton_box_new has been deprecated since version 3.2 and should not be
;;; used in newly-written code. Use gtk_button_box_new() with
;;; GTK_ORIENTATION_HORIZONTAL instead
;;;
;;; Creates a new horizontal button box.
;;;
;;; Returns :
;;;     a new button box GtkWidget.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hbutton-box-new))

(defun gtk-hbutton-box-new ()
  (make-instance 'gtk-button-box
                 :orientation :horizontal))

(export 'gtk-hbutton-box-new)

;;; ----------------------------------------------------------------------------
;;; GtkVButtonBox
;;;
;;; A container for arranging buttons vertically
;;;
;;; Synopsis
;;;
;;;     GtkVButtonBox
;;;
;;;     gtk_vbutton_box_new
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkButtonBox
;;;                                  +----GtkVButtonBox
;;;
;;; Implemented Interfaces
;;;
;;; GtkVButtonBox implements AtkImplementorIface, GtkBuildable and
;;; GtkOrientable.
;;;
;;; Description
;;;
;;; A button box should be used to provide a consistent layout of buttons
;;; throughout your application. The layout/spacing can be altered by the
;;; programmer, or if desired, by the user to alter the 'feel' of a program to a
;;; small degree.
;;;
;;; A GtkVButtonBox is created with gtk_vbutton_box_new(). Buttons are packed
;;; into a button box the same way widgets are added to any other container,
;;; using gtk_container_add(). You can also use gtk_box_pack_start() or
;;; gtk_box_pack_end(), but for button boxes both these functions work just like
;;; gtk_container_add(), ie., they pack the button in a way that depends on the
;;; current layout style and on whether the button has had
;;; gtk_button_box_set_child_secondary() called on it.
;;;
;;; The spacing between buttons can be set with gtk_box_set_spacing(). The
;;; arrangement and layout of the buttons can be changed with
;;; gtk_button_box_set_layout().
;;;
;;; GtkVButtonBox has been deprecated, use GtkButtonBox instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVButtonBox
;;;
;;; struct GtkVButtonBox;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkVButtonBox" 'gtk-vbutton-box))

(define-g-object-class "GtkVButtonBox" gtk-vbutton-box
  (:superclass gtk-button-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_vbutton_box_get_type")
  nil)

;;; ----------------------------------------------------------------------------

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-position
                       "position" "gint" t t t)

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-secondary
                       "secondary" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_vbutton_box_new ()
;;;
;;; GtkWidget * gtk_vbutton_box_new (void);
;;;
;;; Warning
;;;
;;; gtk_vbutton_box_new has been deprecated since version 3.2 and should not be
;;; used in newly-written code. Use gtk_button_box_new() with
;;; GTK_ORIENTATION_VERTICAL instead
;;;
;;; Creates a new vertical button box.
;;;
;;; Returns :
;;;     a new button box GtkWidget.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-vbutton-box-new))

(defun gtk-vbutton-box-new ()
  (make-instance 'gtk-button-box
                 :orientation :vertical))

(export 'gtk-vbutton-box-new)

;;; --- End of file gtk.button-box.lisp ----------------------------------------
