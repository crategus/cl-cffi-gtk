;;; ----------------------------------------------------------------------------
;;; gtk.button-box.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkButtonBox
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-button-box 'type)
 "@version{2013-2-15}
  @begin{short}
    A button box should be used to provide a consistent layout of buttons
    throughout your application. The layout/spacing can be altered by the
    programmer, or if desired, by the user to alter the 'feel' of a program to a
    small degree.
  @end{short}

  gtk_button_box_get_layout() and gtk_button_box_set_layout() retrieve and
  alter the method used to spread the buttons in a button box across the
  container, respectively.

  The main purpose of GtkButtonBox is to make sure the children have all the
  same size. GtkButtonBox gives all children the same size, but it does allow
  'outliers' to keep their own larger size. To force all children to be
  strictly the same size without exceptions, you can set the \"homogeneous\"
  property to TRUE.

  To excempt individual children from homogeneous sizing regardless of their
  'outlier' status, you can set the \"non-homogeneous\" child property.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"child-internal-pad-x\" style property}
      @code{\"child-internal-pad-x\"} of type @code{gint} (Read)@br{}
      Amount to increase child's size on either side.@br{}
      Allowed values: @code{>= 0}@br{}
      Default value: @code{4}

    @subheading{The \"child-internal-pad-y\" style property}
      @code{\"child-internal-pad-y\"} of type @code{gint} (Read)@br{}
      Amount to increase child's size on the top and bottom.@br{}
      Allowed values: @code{>= 0}@br{}
      Default value: @code{0}

    @subheading{The \"child-min-height\" style property}
      @code{\"child-min-height\"} of type @code{gint} (Read)@br{}
      Minimum height of buttons inside the box.@br{}
      Allowed values: @code{>= 0}@br{}
      Default value: @code{27}

    @subheading{The \"child-min-width\" style property}
      @code{\"child-min-width\"} of type @code{gint} (Read)@br{}
      Minimum width of buttons inside the box.@br{}
      Allowed values: @code{>= 0}@br{}
      Default value: @code{85}
  @end{dictionary}
  @see-slot{gtk-button-box-layout-style}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "layout-style" 'gtk-button-box) 't)
 "The \"layout-style\" property of type @symbol{gtk-button-box-style}
  (Read / Write)@br{}
  How to lay out the buttons in the box. Possible values are: spread, edge,
  start and end.@br{}
  Default value: @code{:edge}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-box-layout-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-box-layout-style 'function)
 "@version{2013-3-28}
  Accessor of the slot @code{\"layout-style\"} of the @class{gtk-button-box}
  class.")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkButtonBox"
                       gtk-button-box-child-non-homogeneous
                       "non-homogeneous" "gboolean" t t t)

(define-child-property "GtkButtonBox"
                       gtk-button-box-child-secondary
                       "secondary" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-box-child-non-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-box-child-non-homogeneous 'function)
 "@version{2013-2-15}
  The @code{\"non-homogeneous\"} child property of type @code{gboolean}
  (Read / Write)@br{}
  If TRUE, the child will not be subject to homogeneous sizing.@br{}
  Default value: FALSE")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-box-child-secondary atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-box-child-secondary 'function)
 "@version{2013-2-15}
  The @codee{\"secondary\"} child property of type @code{gboolean}
  (Read / Write)@br{}
  If TRUE, the child appears in a secondary group of children, suitable for,
  e.g., help buttons.@br{}
  Default value: FALSE")

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-new))

(defun gtk-button-box-new (orientation)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-15}
  @argument[orientation]{the box' orientation.}
  @return{A new GtkButtonBox.}
  @short{Creates a new GtkButtonBox.}

  Since 3.0"
  (make-instance 'gtk-button-box
                 :orientation orientation))

(export 'gtk-button-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_get_layout ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-get-layout))

(defun gtk-button-box-get-layout (widget)
 "@version{2013-2-15}
  @argument[widget]{a GtkButtonBox}
  @return{the method used to lay out buttons in widget.}
  @begin{short}
    Retrieves the method being used to arrange the buttons in a button box.
  @end{short}"
  (gtk-button-box-layout-style widget))

(export 'gtk-button-box-get-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_get_child_secondary ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-get-child-secondary))

(defun gtk-button-box-get-child-secondary (widget child)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-15}
  @argument[widget]{a GtkButtonBox}
  @argument[child]{a child of widget}
  @return{whether child should appear in a secondary group of children.}
  @begin{short}
    Returns whether child should appear in a secondary group of children.
  @end{short}

  Since 2.4"
  (gtk-button-box-child-secondary widget child))

(export 'gtk-button-box-get-child-secondary)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_get_child_non_homogeneous ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-get-child-non-homogeneous))

(defun gtk-button-box-get-child-non-homogeneous (widget child)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-15}
  @argument[widget]{a GtkButtonBox}
  @argument[child]{a child of widget}
  @return{TRUE if the child is not subject to homogenous sizing}
  @begin{short}
    Returns whether the child is exempted from homogenous sizing.
  @end{short}

  Since 3.2"
  (gtk-button-box-child-non-homogeneous widget child))

(export 'gtk-button-box-get-child-non-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_set_layout ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-set-layout))

(defun gtk-button-box-set-layout (widget layout-style)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-15}
  @argument[widget]{a GtkButtonBox}
  @argument[layout_style]{the new layout style}
  @short{Changes the way buttons are arranged in their container.}"
  (setf (gtk-button-box-layout-style widget) layout-style))

(export 'gtk-button-box-set-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_set_child_secondary ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-set-child-secondary))

(defun gtk-button-box-set-child-secondary (widget child is-secondary)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-15}
  @argument[widget]{a GtkButtonBox}
  @argument[child]{a child of widget}
  @argument[is_secondary]{if TRUE, the child appears in a secondary group of the
    button box.}
  @begin{short}
    Sets whether child should appear in a secondary group of children. A
    typical use of a secondary child is the help button in a dialog.
  @end{short}

  This group appears after the other children if the style is
  GTK_BUTTONBOX_START, GTK_BUTTONBOX_SPREAD or GTK_BUTTONBOX_EDGE, and before
  the other children if the style is GTK_BUTTONBOX_END. For horizontal button
  boxes, the definition of before/after depends on direction of the widget
  (see gtk_widget_set_direction()). If the style is GTK_BUTTONBOX_START or
  GTK_BUTTONBOX_END, then the secondary children are aligned at the other end
  of the button box from the main children. For the other styles, they appear
  immediately next to the main children."
  (setf (gtk-button-box-child-secondary widget child) is-secondary))

(export 'gtk-button-box-set-child-secondary)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_set_child_non_homogeneous ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-set-child-non-homogeneous))

(defun gtk-button-box-set-child-non-homogeneous (widget child non-homogeneous)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-15}
  @argument[widget]{a GtkButtonBox}
  @argument[child]{a child of widget}
  @argument[non_homogeneous]{the new value}
  @begin{short}
    Sets whether the child is exempted from homogeous sizing.
  @end{short}

  Since 3.2"
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
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHButtonBox
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-hbutton-box 'type)
 "@version{2013-2-15}
  @begin{short}
    A button box should be used to provide a consistent layout of buttons
    throughout your application. The layout/spacing can be altered by the
    programmer, or if desired, by the user to alter the 'feel' of a program to a
    small degree.
  @end{short}

  A GtkHButtonBox is created with gtk_hbutton_box_new(). Buttons are packed
  into a button box the same way widgets are added to any other container,
  using gtk_container_add(). You can also use gtk_box_pack_start() or
  gtk_box_pack_end(), but for button boxes both these functions work just like
  gtk_container_add(), ie., they pack the button in a way that depends on the
  current layout style and on whether the button has had
  gtk_button_box_set_child_secondary() called on it.

  The spacing between buttons can be set with gtk_box_set_spacing(). The
  arrangement and layout of the buttons can be changed with
  gtk_button_box_set_layout().

  GtkHButtonBox has been deprecated, use GtkButtonBox instead.
")

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
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-expand 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the child property @code{\"expand\"} of the
    @class{gtk-hbutton-box} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-fill 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the child property @code{\"fill\"} of the
    @class{gtk-hbutton-box} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-padding 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the child property @code{\"padding\"} of the
    @class{gtk-hbutton-box} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-pack-type 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the child property @code{\"pack-type\"} of the
    @class{gtk-hbutton-box} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-position 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the child property @code{\"position\"} of the
    @class{gtk-hbutton-box} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-secondary atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-secondary 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the child property @code{\"secondary\"} of the
    @class{gtk-hbutton-box} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_hbutton_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hbutton-box-new))

(defun gtk-hbutton-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-2-15}
  @return{A new button box GtkWidget.}
  @b{Warning}

  gtk_hbutton_box_new has been deprecated since version 3.2 and should not be
  used in newly-written code. Use gtk_button_box_new() with
  GTK_ORIENTATION_HORIZONTAL instead
  @short{Creates a new horizontal button box.}"
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
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVButtonBox
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-vbutton-box 'type)
 "@version{2013-2-15}
  @begin{short}
    A button box should be used to provide a consistent layout of buttons
    throughout your application. The layout/spacing can be altered by the
    programmer, or if desired, by the user to alter the 'feel' of a program to a
    small degree.
  @end{short}

  A GtkVButtonBox is created with gtk_vbutton_box_new(). Buttons are packed
  into a button box the same way widgets are added to any other container,
  using gtk_container_add(). You can also use gtk_box_pack_start() or
  gtk_box_pack_end(), but for button boxes both these functions work just like
  gtk_container_add(), ie., they pack the button in a way that depends on the
  current layout style and on whether the button has had
  gtk_button_box_set_child_secondary() called on it.

  The spacing between buttons can be set with gtk_box_set_spacing(). The
  arrangement and layout of the buttons can be changed with
  gtk_button_box_set_layout().

  GtkVButtonBox has been deprecated, use GtkButtonBox instead.")

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
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-expand 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the child property @code{\"expand\"} of the
    @class{gtk-vbutton-box} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-fill 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the child property @code{\"fill\"} of the
    @class{gtk-vbutton-box} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-padding 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the child property @code{\"padding\"} of the
    @class{gtk-vbutton-box} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-pack-type 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the child property @code{\"pack-type\"} of the
    @class{gtk-vbutton-box} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-position 'function)
 "@version{2013-3-8}
  Accessor of the child property @code{\"position\"} of the
  @class{gtk-vbutton-box} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-secondary atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-secondary 'function)
 "@version{2013-3-8}
  Accessor of the child property @code{\"secondary\"} of the
  @class{gtk-vbutton-box} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_vbutton_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-vbutton-box-new))

(defun gtk-vbutton-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @return{A new button box widget.}
  @subeading{Warning}
    @sym{gtk-vbutton-box-new} has been deprecated since version 3.2 and should
    not be used in newly-written code. Use @fun{gtk-button-box-new} with
    @code{:vertical} instead.

  @short{Creates a new vertical button box.}
  @see-function{gtk-button-box-new}"
  (make-instance 'gtk-button-box
                 :orientation :vertical))

(export 'gtk-vbutton-box-new)

;;; --- End of file gtk.button-box.lisp ----------------------------------------
