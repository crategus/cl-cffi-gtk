;;; ----------------------------------------------------------------------------
;;; gtk.button-box.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
  (:default-style 0)            ; TODO: not documented, check the implementation
  (:spread 1)
  (:edge 2)
  (:start 3)
  (:end 4)
  (:center 5)
  (:expand 6))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-box-style atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-button-box-style atdoc:*external-symbols*)
 "@version{2019-3-16}
  @begin{short}
    Used to dictate the style that a @class{gtk-button-box} widget uses to
    layout the buttons it contains.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkButtonBoxStyle\" gtk-button-box-style
  (:export t
   :type-initializer \"gtk_button_box_style_get_type\")
  (:default-style 0)
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
      spacing to 0 (same as calling the functions @fun{gtk-box-homogeneous} and
      @fun{gtk-box-spacing} manually).}
  @end{table}
  @see-class{gtk-button-box}")

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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-button-box 'type)
 "@version{2020-5-3}
  @begin{short}
    A button box should be used to provide a consistent layout of buttons
    throughout your application.
  @end{short}
  The layout/spacing can be altered by the programmer, or if desired, by the
  user to alter the 'feel' of a program to a small degree.

  The slot access function @fun{gtk-button-box-layout-style} retrieve and alter
  the method used to spread the buttons in a button box across the container.

  The main purpose of @sym{gtk-button-box} is to make sure the children have
  all the same size. @sym{gtk-button-box} gives all children the same size,
  but it does allow 'outliers' to keep their own larger size.

  To excempt individual children from homogeneous sizing regardless of their
  'outlier' status, you can set the @code{non-homogeneous} child property.
  @begin[CSS nodes]{dictionary}
    @class{gtk-button-box} uses a single CSS node with name @code{buttonbox}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[non-homogeneous]{entry}
        The @code{non-homogeneous} child property of type @code{:boolean}
        (Read / Write) @br{}
        If @em{true}, the child will not be subject to homogeneous sizing. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[secondary]{entry}
        The @code{secondary} child property of type @code{:boolean}
        (Read / Write) @br{}
        If @em{true}, the child appears in a secondary group of children,
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
        Amount to increase child's size on either side. @br{}
        @em{Warning:} The @code{child-internal-pad-x} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use CSS padding instead. @br{}
        Allowed values: >= 0 @br{}
        Default value: 4
      @end{entry}
      @begin[child-internal-pad-y]{entry}
        The  @code{child-internal-pad-y} style property of type @code{:int}
        (Read) @br{}
        Amount to increase child's size on the top and bottom. @br{}
        @em{Warning:} The @code{child-internal-pad-y} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use CSS padding instead. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[child-min-height]{entry}
        The @code{child-min-height} style property of type @code{:int} (Read)
        @br{}
        Minimum height of buttons inside the box. @br{}
        @em{Warning:} The @code{child-min-height} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use CSS padding instead. @br{}
        Allowed values: >= 0 @br{}
        Default value: 27
      @end{entry}
      @begin[child-min-width]{entry}
        The @code{child-min-width} style property of type @code{:int} (Read)
        @br{}
        Minimum width of buttons inside the box. @br{}
        @em{Warning:} The @code{child-min-width} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use CSS padding instead. @br{}
        Allowed values: >= 0 @br{}
        Default value: 85
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-slot{gtk-button-box-layout-style}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-button-box-layout-style --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "layout-style" 'gtk-button-box)
                     't)
 "The @code{layout-style} property of type @symbol{gtk-button-box-style}
  (Read / Write) @br{}
  How to lay out the buttons in the box. Possible values are @code{:spread},
  @code{:edge}, @code{:start}, @code{:end}, @code{:center}, and @code{:expand}.
  @br{}
  Default value: @code{:edge}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-box-layout-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-box-layout-style 'function)
 "@version{2020-5-3}
  @argument[object]{a @class{gtk-button-box} container}
  @argument[layout-style]{the layout style of type
    @symbol{gtk-button-box-style}}
  @syntax[]{(gtk-button-box-layout-style object) => layout-style}
  @syntax[]{(setf (gtk-button-box-layout-style object) layout-style)}
  @begin{short}
    Accessor of the @slot[gtk-button-box]{layout-style} slot of the
    @class{gtk-button-box} class.
  @end{short}

  The slot access function @sym{gtk-button-box-layout-style} retrieves the
  method being used to arrange the buttons in a button box. The slot access
  function @sym{(setf gtk-button-box-layout-style} changes the way buttons are
  arranged in their container.
  @see-class{gtk-button-box}
  @see-symbol{gtk-button-box-style}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-button-box-child-non-homogeneous -----------------------------------

(define-child-property "GtkButtonBox"
                       gtk-button-box-child-non-homogeneous
                       "non-homogeneous" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-box-child-non-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-box-child-non-homogeneous 'function)
 "@version{2020-5-3}
  @syntax[]{(gtk-button-box-child-non-homogeneous container child) => non-homogeneous}
  @syntax[]{((setf (gtk-button-box-child-non-homogeneous container child) non-homogeneous)}
  @argument[container]{a @class{gtk-button-box} container}
  @argument[child]{a @class{gtk-widget} child}
  @argument[non-homogeneous]{a boolean whether the child is not subject to
    homogeneous sizing}
  @begin{short}
    Accessor of the @code{non-homogeneous} child property of the button box.
  @end{short}

  The function @sym{gtk-button-box-child-non-homogeneous} returns whether the
  child is exempted from homogeneous sizing. The function
  @sym{(setf gtk-button-box-child-non-homogeneous)} sets whether the child
  widget is exempted from homogeous sizing.
  @see-class{gtk-button-box}")

;;; --- gtk-button-box-child-secondary -----------------------------------------

(define-child-property "GtkButtonBox"
                       gtk-button-box-child-secondary
                       "secondary" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-box-child-secondary atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-box-child-secondary 'function)
 "@version{2020-5-3}
  @syntax[]{(gtk-button-box-child-secondary container child) => is-secondary}
  @syntax[]{((setf (gtk-button-box-child-secondary container child) is-secondary)}
  @argument[container]{a @class{gtk-button-box} container}
  @argument[child]{a @class{gtk-widget} child}
  @argument[is-secondary]{if @em{true}, the child appears in a secondary group
    of the button box}
  @begin{short}
    Accessor of the @code{secondary} child property of the button box.
  @end{short}

  The function @sym{gtk-button-box-child-secondary} returns whether child
  widget should appear in a secondary group of children. The function
  @sym{(setf gtk-button-box-child-secondary)} sets whether the child widget
  should appear in a secondary group of children. A typical use of a secondary
  child is the help button in a dialog.

  This group appears after the other children if the style is @code{:start},
  @code{:spread} or @code{:edge}, and before the other children if the style is
  @code{:end}. For horizontal button boxes, the definition of before/after
  depends on the direction of the widget, see the function
  @fun{gtk-widget-direction}. If the style is @code{:start} or @code{:end},
  then the secondary children are aligned at the other end of the button box
  from the main children. For the other styles, they appear immediately next
  to the main children.
  @see-class{gtk-button-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-box-new))

(defun gtk-button-box-new (orientation)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-3}
  @argument[orientation]{the box's orientation of type @symbol{gtk-orientation}}
  @return{A new @class{gtk-button-box} container}
  @short{Creates a new button box container.}
  @see-class{gtk-button-box}
  @see-symbol{gtk-orientation}"
  (make-instance 'gtk-button-box
                 :orientation orientation))

(export 'gtk-button-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_button_box_get_layout ()
;;; gtk_button_box_set_layout () -> gtk-button-box-layout
;;; ----------------------------------------------------------------------------

(defun (setf gtk-button-box-layout) (layout-style button-box)
  (setf (gtk-button-box-layout-style button-box) layout-style))

(defun gtk-button-box-layout (button-box)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-3}
  @syntax[]{(gtk-button-box-layout button-box) => layout-style}
  @syntax[]{(setf (gtk-button-box-layout button-box) layout-style)}
  @argument[button-box]{a @class{gtk-button-box} container}
  @argument[layout-style]{the layout style of type
    @symbol{gtk-button-box-style}}
  @begin{short}
    Accessor of the layout style of the button box.
  @end{short}

  The function @sym{gtk-button-box-layout} retrieves the method being used to
  arrange the buttons in a button box. The function
  @sym{(setf gtk-button-box-layout)} changes the way buttons are arranged in
  their container.

  @begin[Note]{dictionary}
    The function @sym{gtk-button-box-layout} is identical with the slot
    access function @sym{gtk-button-box-layout-style}.
  @end{dictionary}
  @see-class{gtk-button-box}"
  (gtk-button-box-layout-style button-box))

(export 'gtk-button-box-layout)

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
   :export nil
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_hbutton_box_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-hbutton-box 'type)
 "@version{2013-5-18}
  @begin{short}
    A button box should be used to provide a consistent layout of buttons
    throughout your application. The layout/spacing can be altered by the
    programmer, or if desired, by the user to alter the 'feel' of a program to a
    small degree.
  @end{short}

  A @class{gtk-hbutton-box} is created with the function
  @fun{gtk-hbutton-box-new}. Buttons are packed into a button box the same way
  widgets are added to any other container, using the function
  @fun{gtk-container-add}. You can also use the functions
  @fun{gtk-box-pack-start} or @fun{gtk-box-pack-end}, but for button boxes both
  these functions work just like the function @fun{gtk-container-add}, i.e.,
  they pack the button in a way that depends on the current layout style and on
  whether the button has had the function
  @fun{gtk-button-box-set-child-secondary} called on it.

  The spacing between buttons can be set with the @fun{gtk-box-spacing}
  function. The arrangement and layout of the buttons can be
  changed with the function @fun{gtk-button-box-set-layout}.
  @begin[Warning]{dictionary}
    @class{gtk-hbutton-box} has been deprecated, use @class{gtk-button-box}
    instead.
  @end{dictionary}
  @see-class{gtk-button-box}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-expand
                       "expand" "gboolean" t t nil)

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-fill
                       "fill" "gboolean" t t nil)

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-padding
                       "padding" "guint" t t nil)

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-pack-type
                       "pack-type" "GtkPackType" t t nil)

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-position
                       "position" "gint" t t nil)

(define-child-property "GtkHButtonBox"
                       gtk-hbutton-box-child-secondary
                       "secondary" "gboolean" t t nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-expand 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{expand} of the
  @class{gtk-hbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-hbutton-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-fill 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{fill} of the
  @class{gtk-hbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-hbutton-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-padding 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{padding} of the
  @class{gtk-hbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-hbutton-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-pack-type 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{pack-type} of the
  @class{gtk-hbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-hbutton-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-position 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{position} of the
  @class{gtk-hbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-hbutton-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbutton-box-child-secondary atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbutton-box-child-secondary 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{secondary} of the
  @class{gtk-hbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-hbutton-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_hbutton_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hbutton-box-new))

(defun gtk-hbutton-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @return{A new button box container.}
  @subheading{Warning}
    The function @fun{gtk-hbutton-box-new} has been deprecated since version 3.2
    and should not be used in newly-written code. Use the function
    @fun{gtk-button-box-new} with @code{:horizontal} instead.
  @short{Creates a new horizontal button box.}"
  (make-instance 'gtk-button-box
                 :orientation :horizontal))

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
   :export nil
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_vbutton_box_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-vbutton-box 'type)
 "@version{2013-5-18}
  @begin{short}
    A button box should be used to provide a consistent layout of buttons
    throughout your application. The layout/spacing can be altered by the
    programmer, or if desired, by the user to alter the 'feel' of a program to a
    small degree.
  @end{short}

  A @class{gtk-vbutton-box} is created with the function
  @fun{gtk-vbutton-box-new}. Buttons are packed into a button box the same way
  widgets are added to any other container, using the function
  @fun{gtk-container-add}. You can also use the functions
  @fun{gtk-box-pack-start} or @fun{gtk-box-pack-end}, but for button boxes both
  these functions work just like the function @fun{gtk-container-add}, i.e.,
  they pack the button in a way that depends on the current layout style and on
  whether the button has had the function
  @fun{gtk-button-box-set-child-secondary} called on it.

  The spacing between buttons can be set with the @fun{gtk-box-spacing}
  function. The arrangement and layout of the buttons can be
  changed with the function @fun{gtk-button-box-set-layout}.

  @class{gtk-vbutton-box} has been deprecated, use @class{gtk-button-box}
  instead.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-expand
                       "expand" "gboolean" t t nil)

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-fill
                       "fill" "gboolean" t t nil)

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-padding
                       "padding" "guint" t t nil)

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-pack-type
                       "pack-type" "GtkPackType" t t nil)

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-position
                       "position" "gint" t t nil)

(define-child-property "GtkVButtonBox"
                       gtk-vbutton-box-child-secondary
                       "secondary" "gboolean" t t nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-expand 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{expand} of the
  @class{gtk-vbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-vbutton-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-fill 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{fill} of the
  @class{gtk-vbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-vbutton-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-padding 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{padding} of the
  @class{gtk-vbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-vbutton-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-pack-type 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{pack-type} of the
  @class{gtk-vbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-vbutton-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-position 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{position} of the
  @class{gtk-vbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-vbutton-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbutton-box-child-secondary atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbutton-box-child-secondary 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{secondary} of the
  @class{gtk-vbutton-box} class.
  @see-class{gtk-button-box}
  @see-class{gtk-vbutton-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_vbutton_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-vbutton-box-new))

(defun gtk-vbutton-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @return{A new button box container.}
  @subeading{Warning}
    @sym{gtk-vbutton-box-new} has been deprecated since version 3.2 and should
    not be used in newly-written code. Use the function @fun{gtk-button-box-new}
    with @code{:vertical} instead.

  @short{Creates a new vertical button box container.}
  @see-function{gtk-button-box-new}"
  (make-instance 'gtk-button-box
                 :orientation :vertical))

;;; --- End of file gtk.button-box.lisp ----------------------------------------
