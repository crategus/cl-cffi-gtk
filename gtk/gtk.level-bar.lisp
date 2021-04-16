;;; ----------------------------------------------------------------------------
;;; gtk.level-bar.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2014 - 2021 Dieter Kaiser
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
;;; GtkLevelBar
;;;
;;;     A bar that can used as a level indicator.
;;;
;;; Types and Values
;;;
;;;     GTK_LEVEL_BAR_OFFSET_LOW
;;;     GTK_LEVEL_BAR_OFFSET_HIGH
;;;     GTK_LEVEL_BAR_OFFSET_FULL
;;;
;;;     GtkLevelBarMode
;;;     GtkLevelBar
;;;
;;; Functions
;;;
;;;     gtk_level_bar_new
;;;     gtk_level_bar_new_for_interval
;;;     gtk_level_bar_set_mode
;;;     gtk_level_bar_get_mode
;;;     gtk_level_bar_set_value
;;;     gtk_level_bar_get_value
;;;     gtk_level_bar_set_min_value
;;;     gtk_level_bar_get_min_value
;;;     gtk_level_bar_set_max_value
;;;     gtk_level_bar_get_max_value
;;;     gtk_level_bar_set_inverted
;;;     gtk_level_bar_get_inverted
;;;     gtk_level_bar_add_offset_value
;;;     gtk_level_bar_remove_offset_value
;;;     gtk_level_bar_get_offset_value
;;;
;;; Properties
;;;
;;;            gboolean    inverted            Read / Write
;;;             gdouble    max-value           Read / Write
;;;             gdouble    min-value           Read / Write
;;;     GtkLevelBarMode    mode                Read / Write
;;;             gdouble    value               Read / Write
;;;
;;; Style Properties
;;;
;;;                gint    min-block-height    Read / Write
;;;                gint    min-block-width     Read / Write
;;;
;;; Signals
;;;
;;;                void    offset-changed      Has Details
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkLevelBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkLevelBar implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_LEVEL_BAR_OFFSET_LOW
;;;
;;; #define GTK_LEVEL_BAR_OFFSET_LOW  "low"
;;;
;;; The name used for the stock low offset included by GtkLevelBar.
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_LEVEL_BAR_OFFSET_HIGH
;;;
;;; #define GTK_LEVEL_BAR_OFFSET_HIGH "high"
;;;
;;; The name used for the stock high offset included by GtkLevelBar.
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_LEVEL_BAR_OFFSET_FULL
;;;
;;; #define GTK_LEVEL_BAR_OFFSET_FULL "full"
;;;
;;; The name used for the stock full offset included by GtkLevelBar.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkLevelBarMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkLevelBarMode" gtk-level-bar-mode
  (:export t
   :type-initializer "gtk_level_bar_mode_get_type")
  (:continuous 0)
  (:discrete 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-level-bar-mode atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-level-bar-mode atdoc:*external-symbols*)
 "@version{2021-4-16}
  @begin{short}
    Describes how @class{gtk-level-bar} contents should be rendered.
  @end{short}
  Note that this enumeration could be extended with additional modes in the
  future.
  @begin{pre}
(define-g-enum \"GtkLevelBarMode\" gtk-level-bar-mode
  (:export t
   :type-initializer \"gtk_level_bar_mode_get_type\")
  (:continuous 0)
  (:discrete 1))
  @end{pre}
  @begin[code]{table}
    @entry[:continuous]{The level bar has a continuous mode.}
    @entry[:discrete]{The level bar has a discrete mode.}
  @end{table}
  @see-class{gtk-level-bar}")

;;; ----------------------------------------------------------------------------
;;; struct GtkLevelBar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkLevelBar" gtk-level-bar
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_level_bar_get_type")
  ((inverted
    gtk-level-bar-inverted
    "inverted" "gboolean" t t)
   (max-value
    gtk-level-bar-max-value
    "max-value" "gdouble" t t)
   (min-value
    gtk-level-bar-min-value
    "min-value" "gdouble" t t)
   (mode
    gtk-level-bar-mode
    "mode" "GtkLevelBarMode" t t)
   (value
    gtk-level-bar-value
    "value" "gdouble" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-level-bar 'type)
 "@version{*2021-4-16}
  @begin{short}
    The @sym{gtk-level-bar} widget is a bar widget that can be used as a
    level indicator.
  @end{short}
  Typical use cases are displaying the strength of a password, or showing the
  charge level of a battery.

  @image[levelbar]{}

  Use the function @fun{gtk-level-bar-value} to set the current value, and the
  function @fun{gtk-level-bar-add-offset-value} to set the value offsets at
  which the bar will be considered in a different state. GTK will add a few
  offsets by default on the level bar: @code{\"low\"}, @code{\"high\"} and
  @code{\"full\"}, with values 0.25, 0.75 and 1.0 respectively.

  Note that it is your responsibility to update preexisting offsets when
  changing the minimum or maximum value. GTK+ will simply clamp them to the new
  range.

  The default interval of values is between zero and one, but it is possible to
  modify the interval using the functions @fun{gtk-level-bar-min-value} and
  @fun{gtk-level-bar-max-value}. The value will be always drawn in proportion
  to the admissible interval, i.e. a value of 15 with a specified interval
  between 10 and 20 is equivalent to a value of 0.5 with an interval between
  0 and 1. When the @code{:discrete} level bar mode is used, the bar level is
  rendered as a finite number of separated blocks instead of a single one.
  The number of blocks that will be rendered is equal to the number of units
  specified by the admissible interval.

  For instance, to build a bar rendered with five blocks, it is sufficient to
  set the minimum value to 0 and the maximum value to 5 after changing the
  indicator mode to discrete.
  @begin[GtkLevelBar as GtkBuildable]{dictionary}
    The @sym{gtk-level-bar} implementation of the @class{gtk-buildable}
    interface supports a custom @code{<offsets>} element, which can contain any
    number of @code{<offset>} elements, each of which must have name and value
    attributes.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 levelbar[.discrete]
 ╰── trough
     ├── block.filled.level-name
     ┊
     ├── block.empty
     ┊
    @end{pre}
    The @sym{gtk-level-bar} widget has a main CSS node with name @code{levelbar}
    and one of the style classes @code{.discrete} or @code{.continuous} and a
    subnode with name @code{trough}. Below the @code{trough} node are a number
    of nodes with name @code{block} and style class @code{.filled} or
    @code{.empty}. In continuous mode, there is exactly one node of each, in
    discrete mode, the number of filled and unfilled nodes corresponds to blocks
    that are drawn. The @code{block.filled} nodes also get a style class
    @code{.level-name} corresponding to the level for the current value.

    In horizontal orientation, the nodes are always arranged from left to right,
    regardless of text direction.
  @end{dictionary}
  @begin[Example]{dictionary}
    Adding a custom offset on the bar.
    @begin{pre}
(defun create-level-bar (orientation)
  (let* ((levelbar (make-instance 'gtk-level-bar
                                  :orientation orientation)))
    ;; This changes the value of the default low offset
    (gtk-level-bar-add-offset-value levelbar \"low\" 0.10d0)
    ;; This adds a new offset to the bar; the application will
    ;; be able to change its color CSS like this:
    ;;
    ;; levelbar block.my-offset {
    ;;   background-color: magenta;
    ;;   border-style: solid;
    ;;   border-color: black;
    ;;   border-style: 1px;
    ;; @}
    (gtk-level-bar-add-offset-value levelbar \"my-offset\" 0.60d0)
    ;; Return the new level bar
    levelbar))
    @end{pre}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[min-block-height]{entry}
        The @code{min-block-height} style property of type @code{:int}
        (Read / Write) @br{}
        The style property determines the minimum height for blocks filling the
        @sym{gtk-level-bar} widget. @br{}
        @em{Warning:} The @code{min-block-height} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use the standard min-width/min-height CSS properties on the block
        elements. The value of this style property is ignored. @br{}
        Allowed values: >= 1 @br{}
        Default value: 3
      @end{entry}
      @begin[min-block-width]{entry}
        The @code{min-block-width} style property of type @code{:int}
        (Read / Write) @br{}
        The style property determines the minimum width for blocks filling the
        @sym{gtk-level-bar} widget. @br{}
        @em{Warning:} The @code{min-block-height} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use the standard min-width/min-height CSS properties on the block
        elements. The value of this style property is ignored. @br{}
        Allowed values: >= 1 @br{}
        Default value: 3
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"offset-changed\" signal}
      @begin{pre}
 lambda (levelbar name)    :detailed
      @end{pre}
      Emitted when an offset specified on the bar changes value as an effect to
      the function @fun{gtk-level-bar-add-offset-value} being called. The signal
      supports detailed connections. You can connect to the detailed signal
      \"changed::x\" in order to only receive callbacks when the value of
      offset \"x\" changes.
      @begin[code]{table}
        @entry[levelbar]{The @sym{gtk-level-bar} widget which received the
          signal.}
        @entry[name]{A string with the name of the offset that changed value.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-level-bar-inverted}
  @see-slot{gtk-level-bar-max-value}
  @see-slot{gtk-level-bar-min-value}
  @see-slot{gtk-level-bar-mode}
  @see-slot{gtk-level-bar-value}
  @see-class{gtk-progress-bar}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-level-bar-inverted -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inverted" 'gtk-level-bar) 't)
 "The @code{inverted} property of type @code{:boolean} (Read / Write) @br{}
  Level bars normally grow from top to bottom or left to right. Inverted level
  bars grow in the opposite direction. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-level-bar-inverted atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-level-bar-inverted 'function)
 "@version{2021-4-16}
  @syntax[]{(gtk-level-bar-inverted object) => inverted}
  @syntax[]{(setf (gtk-level-bar-inverted object) inverted)}
  @argument[object]{a @class{gtk-level-bar} widget}
  @argument[inverted]{@em{true} to invert the level bar}
  @begin{short}
    Accessor of the @slot[gtk-level-bar]{inverted} slot of the
    @class{gtk-level-bar} class.
  @end{short}

  The slot access function @sym{gtk-level-bar-inverted} returns @em{true}
  if the level bar is inverted. The slot acess function
  @sym{(setf gtk-level-bar-inverted)} sets the value of the
  @slot[gtk-level-bar]{inverted} property.
  @see-class{gtk-level-bar}")

;;; --- gtk-level-bar-max-value ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-value" 'gtk-level-bar) 't)
 "The @code{max-value} property of type @code{:double} (Read / Write) @br{}
  The property determines the maximum value of the interval that can be
  displayed by the bar. @br{}
  Allowed values: >= 0.0d0 @br{}
  Default value: 1.0d0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-level-bar-max-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-level-bar-max-value 'function)
 "@version{*2021-4-16}
  @syntax[]{(gtk-level-bar-max-value object) => value}
  @syntax[]{(setf (gtk-level-bar-max-value object) value)}
  @argument[object]{a @class{gtk-level-bar} widget}
  @argument[value]{a double float with a positive value}
  @begin{short}
    Accessor of the @slot[gtk-level-bar]{max-value} slot of the
    @class{gtk-level-bar} class.
  @end{short}

  The slot access function @sym{gtk-level-bar-max-value} returns the value of
  the @slot[gtk-level-bar]{max-value} property. The slot access function
  @sym{(setf gtk-level-bar-max-value)} sets the value.
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-min-value}")

;;; --- gtk-level-bar-min-value ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-value" 'gtk-level-bar) 't)
 "The @code{min-value} property of type @code{:double} (Read / Write) @br{}
  The property determines the minimum value of the interval that can be
  displayed by the bar. @br{}
  Allowed values: >= 0.0d0 @br{}
  Default value: 0.0d0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-level-bar-min-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-level-bar-min-value 'function)
 "@version{2021-4-16}
  @syntax[]{(gtk-level-bar-min-value object) => value}
  @syntax[]{(setf (gtk-level-bar-min-value object) value)}
  @argument[object]{a @class{gtk-level-bar} widget}
  @argument[value]{a double float with a positive value}
  @begin{short}
    Accessor of the @slot[gtk-level-bar]{min-value} slot of the
    @class{gtk-level-bar} class.
  @end{short}

  The slot access function @sym{gtk-level-bar-min-value} returns the value
  of the @slot[gtk-level-bar]{min-value} property. The slot access function
  @sym{(setf gtk-level-bar-min-value)} sets the value.
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-max-value}")

;;; --- gtk-level-bar-mode -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mode" 'gtk-level-bar) 't)
 "The @code{mode} property of type @symbol{gtk-level-bar-mode}
  (Read / Write) @br{}
  The property determines the way a @sym{gtk-level-bar} widget interprets the
  value properties to draw the level fill area. Specifically, when the value is
  @code{:continuous}, the @sym{gtk-level-bar} widget will draw a single block
  representing the current value in that area. When the value is
  @code{:discrete}, the widget will draw a succession of separate blocks filling
  the draw area, with the number of blocks being equal to the units separating
  the integral roundings of @code{min-value} and @code{max-value}. @br{}
  Default value: @code{:continuous}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-level-bar-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-level-bar-mode 'function)
 "@version{2021-4-16}
  @syntax[]{(gtk-level-bar-mode object) => mode}
  @syntax[]{(setf (gtk-level-bar-mode object) mode)}
  @argument[object]{a @class{gtk-level-bar} widget}
  @argument[mode]{a value of the @symbol{gtk-level-bar-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-level-bar]{mode} slot of the
    @class{gtk-level-bar} class.
  @end{short}

  The slot access function @sym{gtk-level-bar-mode} returns the value of the
  @slot[gtk-level-bar]{mode} property. The slot access function
  @sym{(setf gtk-level-bar-mode)} sets the value.
  @see-class{gtk-level-bar}
  @see-symbol{gtk-level-bar-mode}")

;;; --- gtk-level-bar-value ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value" 'gtk-level-bar) 't)
 "The @code{value} property of type @code{:double} (Read / Write) @br{}
  The property determines the currently filled value of the level bar. @br{}
  Allowed values: >= 0.0d0 @br{}
  Default value: 0.0d0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-level-bar-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-level-bar-value 'function)
 "@version{2021-4-16}
  @syntax[]{(gtk-level-bar-value object) >= value}
  @syntax[]{(setf (gtk-level-bar-value object) value)}
  @argument[object]{a @class{gtk-level-bar} widget}
  @argument[value]{a double float with a value in the interval between
    @slot[gtk-level-bar]{min-value} and @slot[gtk-level-bar]{max-value}}
  @begin{short}
    Accessor of the @slot[gtk-level-bar]{value} slot of the
    @class{gtk-level-bar} class.
  @end{short}

  The slot access function @sym{gtk-level-bar-value} gets the value of the
  level bar in the interval between @slot[gtk-level-bar]{min-value} and
  @slot[gtk-level-bar]{max-value}. The slot access funtion
  @sym{(setf gtk-level-bar-value)} sets the value.
  @see-class{gtk-level-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-level-bar-new))

(defun gtk-level-bar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-4-16}
  @return{A @class{gtk-level-bar} widget.}
  @short{Creates a new level bar.}
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-new-for-interval}"
  (make-instance 'gtk-level-bar))

(export 'gtk-level-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_new_for_interval ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-level-bar-new-for-interval))

(defun gtk-level-bar-new-for-interval (min-value max-value)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-16}
  @argument[min-value]{a double float with a positive value}
  @argument[max-value]{a double float with a positive value}
  @return{A @class{gtk-level-bar} widget.}
  @begin{short}
    Utility constructor that creates a new level bar for the specified interval.
  @end{short}
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-new}"
  (make-instance 'gtk-level-bar
                 :min-value min-value
                 :max-value max-value))

(export 'gtk-level-bar-new-for-interval)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_add_offset_value ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_level_bar_add_offset_value" %gtk-level-bar-add-offset-value)
    :void
  (levelbar (g-object gtk-level-bar))
  (name :string)
  (value :double))

(defun gtk-level-bar-add-offset-value (levelbar name value)
 #+cl-cffi-gtk-documentation
 "@version{*2021-4-16}
  @argument[levelbar]{a @class{gtk-level-bar} widget}
  @argument[name]{a string with the name of the new offset}
  @argument[value]{a double float value for the new offset}
  @begin{short}
    Adds a new offset marker on the level bar at the position specified by
    @arg{value}.
  @end{short}

  When the level bar value is in the interval topped by @arg{value}, or between
  @arg{value} and @slot[gtk-level-bar]{max-value} in case the offset is the last
  one on the bar, a style class named @code{level-name} will be applied when
  rendering the level bar fill. If another offset marker named @arg{name}
  exists, its value will be replaced by @arg{value}.
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-remove-offset-value}"
  (%gtk-level-bar-add-offset-value levelbar
                                   name
                                   (coerce value 'double-float)))

(export 'gtk-level-bar-add-offset-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_remove_offset_value ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_level_bar_remove_offset_value" gtk-level-bar-remove-offset-value)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-16}
  @argument[levelbar]{a @class{gtk-level-bar} widget}
  @argument[name]{a string with the name of an offset in the bar}
  @begin{short}
    Removes an offset marker previously added with the function
    @fun{gtk-level-bar-add-offset-value}.
  @end{short}
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-add-offset-value}"
  (levelbar (g-object gtk-level-bar))
  (name :string))

(export 'gtk-level-bar-remove-offset-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_get_offset_value () -> gtk-level-bar-offset-value
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_level_bar_get_offset_value" %gtk-level-bar-offset-value) :boolean
  (levelbar (g-object gtk-level-bar))
  (name :string)
  (value (:pointer :double)))

(defun gtk-level-bar-offset-value (levelbar name)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-16}
  @argument[levelbar]{a @class{gtk-level-bar} widget}
  @argument[name]{a string with the name of an offset in the level bar}
  @return{The double float value which specified the offset marker.}
  @begin{short}
    Fetches the value specified for the offset marker @arg{name} in the level
    bar, returning @code{nil} in case an offset named @arg{name} was not found.
  @end{short}
  @see-class{gtk-level-bar}"
  (with-foreign-object (value :double)
    (when (%gtk-level-bar-offset-value levelbar name value)
      (mem-ref value :double))))

(export 'gtk-level-bar-offset-value)

;;; --- End of file gtk.level-bar.lisp -----------------------------------------
