;;; ----------------------------------------------------------------------------
;;; gtk.button-box.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     A container for arranging buttons
;;;
;;; Types and Values
;;;
;;;     GtkButtonBox
;;;     GtkButtonBoxStyle
;;;
;;; Functions
;;;
;;;     gtk_button_box_new
;;;     gtk_button_box_get_layout
;;;     gtk_button_box_get_child_secondary
;;;     gtk_button_box_get_child_non_homogeneous
;;;     gtk_button_box_set_layout
;;;     gtk_button_box_set_child_secondary
;;;     gtk_button_box_set_child_non_homogeneous
;;;
;;; Properties
;;;
;;;     GtkButtonBoxStyle  layout-style          Read / Write
;;;
;;; Child Properties
;;;
;;;              gboolean  non-homogeneous       Read / Write
;;;              gboolean  secondary             Read / Write
;;;
;;; Style Properties
;;;
;;;                  gint  child-internal-pad-x  Read
;;;                  gint  child-internal-pad-y  Read
;;;                  gint  child-min-height      Read
;;;                  gint  child-min-width       Read
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkButtonBox
;;;                         ├── GtkHButtonBox
;;;                         ╰── GtkVButtonBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkButtonBox implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkButtonBoxStyle
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkButtonBoxStyle" gtk-button-box-style
  (:export t
   :type-initializer "gtk_button_box_style_get_type")
  (:spread 1)
  (:edge 2)
  (:start 3)
  (:end 4)
  (:center 5)
  (:expand 6))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-box-style atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-button-box-style atdoc:*external-symbols*)
 "@version{2021-12-9}
  @begin{short}
    Used to dictate the style that a @class{gtk-button-box} widget uses to
    layout the buttons it contains.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkButtonBoxStyle\" gtk-button-box-style
  (:export t
   :type-initializer \"gtk_button_box_style_get_type\")
  (:spread 1)
  (:edge 2)
  (:start 3)
  (:end 4)
  (:center 5)
  (:expand 6))
  @end{pre}
  @begin[code]{table}
    @entry[:spread]{Buttons are evenly spread across the box.}
    @entry[:edge]{Buttons are placed at the edges of the box.}
    @entry[:start]{Buttons are grouped towards the start of the box, on the
      left for a horizontal box, or the top for a vertical box.}
    @entry[:end]{Buttons are grouped towards the end of the box, on the right
      for a horizontal box, or the bottom for a vertical box.}
    @entry[:center]{Buttons are centered in the box.}
    @entry[:expand]{Buttons expand to fill the box. This entails giving buttons
      a \"linked\" appearance, making button sizes homogeneous, and setting
      spacing to 0, same as calling the @fun{gtk-box-homogeneous} and
      @fun{gtk-box-spacing} functions manually.}
  @end{table}
  @see-class{gtk-button-box}
  @see-function{gtk-box-homogeneous}
  @see-function{gtk-box-spacing}")

;;; ----------------------------------------------------------------------------
;;; struct GtkButtonBox
;;; ----------------------------------------------------------------------------

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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-button-box 'type)
 "@version{2021-12-9}
  @begin{short}
    A @sym{gtk-button-box} widget should be used to provide a consistent layout
    of buttons throughout your application.
  @end{short}
  The layout/spacing can be altered by the programmer, or if desired, by the
  user to alter the 'feel' of a program to a small degree.

  The @fun{gtk-button-box-layout-style} slot access function retrieves and
  alters the method used to spread the buttons in a button box across the
  container.

  The main purpose of the @sym{gtk-button-box} widget is to make sure the
  children have all the same size. The @sym{gtk-button-box} widget gives all
  children the same size, but it does allow 'outliers' to keep their own larger
  size.

  To excempt individual children from homogeneous sizing regardless of their
  'outlier' status, you can set the @code{non-homogeneous} child property.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-button-box} widget uses a single CSS node with name
    @code{buttonbox}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[non-homogeneous]{entry}
        The @code{non-homogeneous} child property of type @code{:boolean}
        (Read / Write) @br{}
        If @em{true}, the child widget will not be subject to homogeneous
        sizing. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[secondary]{entry}
        The @code{secondary} child property of type @code{:boolean}
        (Read / Write) @br{}
        If @em{true}, the child widget appears in a secondary group of children,
        suitable for, e.g. help buttons. @br{}
        Default value: @em{false}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[child-internal-pad-x]{entry}
        The @code{child-internal-pad-x} style property of type @code{:int}
        (Read) @br{}
        Amount to increase size of the child widget on either side. @br{}
        @em{Warning:} The @code{child-internal-pad-x} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use CSS padding instead. @br{}
        Allowed values: >= 0 @br{}
        Default value: 4
      @end{entry}
      @begin[child-internal-pad-y]{entry}
        The  @code{child-internal-pad-y} style property of type @code{:int}
        (Read) @br{}
        Amount to increase the size of the child widget on the top and bottom.
        @br{}
        @em{Warning:} The @code{child-internal-pad-y} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use CSS padding instead. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[child-min-height]{entry}
        The @code{child-min-height} style property of type @code{:int} (Read)
        @br{}
        Minimum height of buttons inside the box. @br{}
        @em{Warning:} The @code{child-min-height} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use CSS padding instead. @br{}
        Allowed values: >= 0 @br{}
        Default value: 27
      @end{entry}
      @begin[child-min-width]{entry}
        The @code{child-min-width} style property of type @code{:int} (Read)
        @br{}
        Minimum width of buttons inside the box. @br{}
        @em{Warning:} The @code{child-min-width} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use CSS padding instead. @br{}
        Allowed values: >= 0 @br{}
        Default value: 85
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-slot{gtk-button-box-layout-style}
  @see-class{gtk-box}
  @see-symbol{gtk-button-box-style}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-button-box-layout-style --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "layout-style" 'gtk-button-box)
                     't)
 "The @code{layout-style} property of type @symbol{gtk-button-box-style}
  (Read / Write) @br{}
  How to lay out the buttons in the box. @br{}
  Default value: @code{:edge}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-box-layout-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-box-layout-style 'function)
 "@version{2021-12-9}
  @syntax[]{(gtk-button-box-layout-style object) => style}
  @syntax[]{(setf (gtk-button-box-layout-style object) style)}
  @argument[object]{a @class{gtk-button-box} widget}
  @argument[style]{a value of the @symbol{gtk-button-box-style} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-button-box]{layout-style} slot of the
    @class{gtk-button-box} class.
  @end{short}

  The @sym{gtk-button-box-layout-style} slot access function retrieves the
  method being used to arrange the buttons in a button box. The
  @sym{(setf gtk-button-box-layout-style)} slot access function changes the way
  buttons are arranged.
  @see-class{gtk-button-box}
  @see-symbol{gtk-button-box-style}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkButtonBox" 'gtk-button-box))

;;; --- gtk-button-box-child-non-homogeneous -----------------------------------

(define-child-property "GtkButtonBox"
                       gtk-button-box-child-non-homogeneous
                       "non-homogeneous" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-box-child-non-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-box-child-non-homogeneous 'function)
 "@version{2021-12-9}
  @syntax[]{(gtk-button-box-child-non-homogeneous container child) => setting}
  @syntax[]{(setf (gtk-button-box-child-non-homogeneous container child) setting)}
  @argument[container]{a @class{gtk-button-box} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[setting]{a boolean whether the child widget is not subject to
    homogeneous sizing}
  @begin{short}
    Accessor of the @code{non-homogeneous} child property of the button box.
  @end{short}

  The @sym{gtk-button-box-child-non-homogeneous} function returns whether the
  child widget is exempted from homogeneous sizing. The
  @sym{(setf gtk-button-box-child-non-homogeneous)} function sets whether the
  child widget is exempted.
  @see-class{gtk-button-box}
  @see-class{gtk-widget}")

;;; --- gtk-button-box-child-secondary -----------------------------------------

(define-child-property "GtkButtonBox"
                       gtk-button-box-child-secondary
                       "secondary" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-box-child-secondary atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-box-child-secondary 'function)
 "@version{2021-12-9}
  @syntax[]{(gtk-button-box-child-secondary container child) => setting}
  @syntax[]{(setf (gtk-button-box-child-secondary container child) setting)}
  @argument[container]{a @class{gtk-button-box} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[setting]{if @em{true}, the child widget appears in a secondary
    group of the button box}
  @begin{short}
    Accessor of the @code{secondary} child property of the button box.
  @end{short}

  The @sym{gtk-button-box-child-secondary} function returns whether the child
  widget should appear in a secondary group of children. The
  @sym{(setf gtk-button-box-child-secondary)} function sets whether the child
  widget should appear in a secondary group of children. A typical use of a
  secondary child widget is the help button in a dialog.

  This group appears after the other children if the style is the @code{:start},
  @code{:spread} or @code{:edge} style, and before the other children if the
  style is the @code{:end} style. For horizontal button boxes, the definition
  of before/after depends on the direction of the widget, see the
  @fun{gtk-widget-direction} function. If the style is the @code{:start} or
  @code{:end} style, then the secondary children are aligned at the other end
  of the button box from the main children. For the other styles, they appear
  immediately next to the main children.
  @see-class{gtk-button-box}
  @see-class{gtk-widget}
  @see-function{gtk-widget-direction}")

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-new))

(defun gtk-button-box-new (orientation)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-9}
  @argument[orientation]{a value of the @symbol{gtk-orientation} enumeration}
  @return{A new @class{gtk-button-box} widget.}
  @short{Creates a new button box.}
  @see-class{gtk-button-box}
  @see-symbol{gtk-orientation}"
  (make-instance 'gtk-button-box
                 :orientation orientation))

(export 'gtk-button-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_get_layout ()
;;; gtk_button_box_set_layout () -> gtk-button-box-layout
;;; ----------------------------------------------------------------------------

(defun (setf gtk-button-box-layout) (style buttonbox)
  (setf (gtk-button-box-layout-style buttonbox) style))

(defun gtk-button-box-layout (buttonbox)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-9}
  @syntax[]{(gtk-button-box-layout button-box) => style}
  @syntax[]{(setf (gtk-button-box-layout button-box) style)}
  @argument[buttonbox]{a @class{gtk-button-box} widget}
  @argument[style]{a value of the @symbol{gtk-button-box-style} enumeration}
  @begin{short}
    Accessor of the layout style of the button box.
  @end{short}

  The @sym{gtk-button-box-layout} function retrieves the method being used to
  arrange the buttons in a button box. The @sym{(setf gtk-button-box-layout)}
  function changes the way buttons are arranged.
  @begin[Note]{dictionary}
    The @sym{gtk-button-box-layout} function is an abbreviation for the slot
    access @fun{gtk-button-box-layout-style} function.
  @end{dictionary}
  @see-class{gtk-button-box}
  @see-symbol{gtk-button-box-style}
  @see-function{gtk-button-box-layout-style}"
  (gtk-button-box-layout-style buttonbox))

(export 'gtk-button-box-layout)

;;; --- End of file gtk.button-box.lisp ----------------------------------------
