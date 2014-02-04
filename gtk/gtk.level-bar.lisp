;;; ----------------------------------------------------------------------------
;;; gtk.level-bar.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.9 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2014 Dieter Kaiser
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
;;; A bar that can used as a level indicator
;;;
;;; Synopsis
;;;
;;;     GTK_LEVEL_BAR_OFFSET_LOW
;;;     GTK_LEVEL_BAR_OFFSET_HIGH
;;;
;;;     GtkLevelBarMode
;;;     GtkLevelBar
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
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkLevelBar
;;;
;;; Implemented Interfaces
;;;
;;; GtkLevelBar implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; Properties
;;;
;;;   "inverted"                 gboolean              : Read / Write
;;;   "max-value"                gdouble               : Read / Write
;;;   "min-value"                gdouble               : Read / Write
;;;   "mode"                     GtkLevelBarMode       : Read / Write
;;;   "value"                    gdouble               : Read / Write
;;;
;;; Style Properties
;;;
;;;   "min-block-height"         gint                  : Read / Write
;;;   "min-block-width"          gint                  : Read / Write
;;;
;;; Signals
;;;
;;;   "offset-changed"                                 : Has Details
;;;
;;; Description
;;;
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
;;; enum GtkLevelBarMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkLevelBarMode" gtk-level-bar-mode
  (:export t
   :type-initializer "gtk_level_bar_mode_get_type")
  (:continuous 0)
  (:discrete 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-level-bar-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-level-bar-mode atdoc:*external-symbols*)
 "@version{2014-2-3}
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
    @entry[:continuous]{The bar has a continuous mode.}
    @entry[:discrete]{The bar has a discrete mode.}
  @end{table}

  Since 3.6
  @see-class{gtk-level-bar}")

;;; ----------------------------------------------------------------------------
;;; struct GtkLevelBar
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(define-g-object-class "GtkLevelBar" gtk-level-bar
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_level_bar_get_type")
  (#+gtk-3-8
   (inverted
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

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation 'gtk-level-bar 'type)
 "@version{2014-2-3}
  @begin{short}
    The @class{gtk-level-bar} is a bar widget that can be used as a level
    indicator. Typical use cases are displaying the strength of a password,
    or showing the charge level of a battery.
  @end{short}

  Use the function @fun{gtk-level-bar-set-value} to set the current value, and
  the function @fun{gtk-level-bar-add-offset-value} to set the value offsets at
  which the bar will be considered in a different state. GTK will add two
  offsets by default on the level bar: @code{\"low\"} and @code{\"high\"},
  with values 0.25 and 0.75 respectively.

  The default interval of values is between zero and one, but it is possible to
  modify the interval using the functions @fun{gtk-level-bar-set-min-value} and
  @fun{gtk-level-bar-set-max-value}. The value will be always drawn in
  proportion to the admissible interval, i. e. a value of 15 with a specified
  interval between 10 and 20 is equivalent to a value of 0.5 with an interval
  between 0 and 1. When @code{:discrete} is used, the bar level is rendered as
  a finite and number of separated blocks instead of a single one. The number
  of blocks that will be rendered is equal to the number of units specified by
  the admissible interval. For instance, to build a bar rendered with five
  blocks, it is sufficient to set the minimum value to 0 and the maximum value
  to 5 after changing the indicator mode to discrete.

  @begin[Example]{dictionary}
    Adding a custom offset on the bar.
    @begin{pre}
 static GtkWidget *
 create_level_bar (void)
 {
   GtkWidget *level_bar;

   level_bar = gtk_level_bar_new ();

   /* This changes the value of the default low offset */
   gtk_level_bar_add_offset_value (GTK_LEVEL_BAR (level_bar),
                                   GTK_LEVEL_BAR_OFFSET_LOW, 0.10);

    /* This adds a new offset to the bar; the application will
    * be able to change its color by using the following selector,
    * either by adding it to its CSS file or using
    * gtk_css_provider_load_from_data() and gtk_style_context_add_provider()
    *
    * .level-bar.fill-block.level-my-offset {
    *   background-color: green;
    *   border-style: solid;
    *   border-color: black;
    *   border-style: 1px;
    * @}
    */
    gtk_level_bar_add_offset_value (GTK_LEVEL_BAR (level_bar),
                                    \"my-offset\", 0.60);
    return level_bar;
 @}
    @end{pre}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"min-block-height\" style property}
      @code{\"min-block-height\"} of type @code{:int} (Read / Write) @br{}
      The @code{\"min-block-height\"} style property determines the minimum
      height for blocks filling the @sym{gtk-level-bar} widget. @br{}
      Allowed values: >= 1 @br{}
      Default value: 3 @br{}
      Since 3.6

    @subheading{The \"min-block-width\" style property}
      @code{\"min-block-width\"} of type @code{:int} (Read / Write) @br{}
      The @code{\"min-block-width\"} style property determines the minimum width
      for blocks filling the @sym{gtk-level-bar} widget. @br{}
      Allowed values: >= 1 @br{}
      Default value: 3 @br{}
      Since 3.6
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"offset-changed\" signal}
      @begin{pre}
lambda (levelbar name)   : Has Details
      @end{pre}
      Emitted when an offset specified on the bar changes value as an effect to
      the function @fun{gtk-level-bar-add-offset-value} being called.
      The signal supports detailed connections; you can connect to the detailed
      signal \"changed::x\" in order to only receive callbacks when the value of
      offset \"x\" changes.
      @begin[code]{table}
        @entry[levelbar]{A @sym{gtk-level-bar}.}
        @entry[name]{The name of the offset that changed value.}
      @end{table}
      Since 3.6
  @end{dictionary}
  @see-slot{gtk-level-bar-inverted}
  @see-slot{gtk-level-bar-max-value}
  @see-slot{gtk-level-bar-min-value}
  @see-slot{gtk-level-bar-mode}
  @see-slot{gtk-level-bar-value}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "inverted" 'gtk-level-bar) 't)
 "The @code{\"inverted\" property} of type @code{:boolean} (Read / Write) @br{}
  Level bars normally grow from top to bottom or left to right. Inverted level
  bars grow in the opposite direction. @br{}
  Default value: @code{nil} @br{}
  Since 3.8")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "max-value" 'gtk-level-bar) 't)
 "The @code{\"max-value\"} property of type @code{:double} (Read / Write) @br{}
  The @code{\"max-value\"} property determines the maximum value of the interval
  that can be displayed by the bar. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1 @br{}
  Since 3.6")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "min-value" 'gtk-level-bar) 't)
 "The @code{\"min-value\"} property of type @code{:double} (Read / Write) @br{}
  The @code{\"min-value\"} property determines the minimum value of the interval
  that can be displayed by the bar. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 3.6")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "mode" 'gtk-level-bar) 't)
 "The @code{\"mode\"} property of type @symbol{gtk-level-bar-mode}
  (Read / Write) @br{}
  The @code{\"mode\"} property determines the way @sym{gtk-level-bar} interprets
  the value properties to draw the level fill area. Specifically, when the value
  is @code{:continuous}, @sym{gtk-level-bar} will draw a single block
  representing the current value in that area; when the value is
  @code{:discrete}, the widget will draw a succession of separate blocks filling
  the draw area, with the number of blocks being equal to the units separating
  the integral roundings of @code{\"min-value\"} and @code{\"max-value\"}. @br{}
  Default value: @code{:continuous} @br{}
  Since 3.6")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "value" 'gtk-level-bar) 't)
 "The @code{\"value\"} property of type @code{:double} (Read / Write) @br{}
  The @code{\"value\"} property determines the currently filled value of the
  level bar. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 3.6")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-level-bar-inverted atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-level-bar-inverted 'function)
 "@version{2014-2-3}
  Accessor of the slot @slot[gtk-level-bar]{inverted} of the
  @class{gtk-level-bar} class.
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-get-inverted}
  @see-function{gtk-level-bar-set-inverted}")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-level-bar-max-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-level-bar-max-value 'function)
 "@version{2014-2-3}
  Accessor of the slot @slot[gtk-level-bar]{max-value} of the
  @class{gtk-level-bar} class.
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-get-max-value}
  @see-function{gtk-level-bar-set-max-value}")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-level-bar-min-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-level-bar-min-value 'function)
 "@version{2014-2-3}
  Accessor of the slot @slot[gtk-level-bar]{min-value} of the
  @class{gtk-level-bar} class.
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-get-min-value}
  @see-function{gtk-level-bar-set-min-value}")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-level-bar-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-level-bar-mode 'function)
 "@version{2014-2-3}
  Accessor of the slot @slot[gtk-level-bar]{mode} of the
  @class{gtk-level-bar} class.
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-get-mode}
  @see-function{gtk-level-bar-set-mode}")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-level-bar-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-level-bar-value 'function)
 "@version{2014-2-3}
  Accessor of the slot @slot[gtk-level-bar]{value} of the
  @class{gtk-level-bar} class.
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-get-value}
  @see-function{gtk-level-bar-set-value}")

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-level-bar-new))

#+gtk-3-6
(defun gtk-level-bar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-2-3}
  @return{A @class{gtk-level-bar}.}
  @short{Creates a new @class{gtk-level-bar}.}

  Since 3.6
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-new-for-interval}"
  (make-instance 'gtk-level-bar))

#+gtk-3-6
(export 'gtk-level-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_new_for_interval ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-level-bar-new-for-interval))

#+gtk-3-6
(defun gtk-level-bar-new-for-interval (min-value max-value)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-3}
  @argument[min-value]{a positive value}
  @argument[max-value]{a positive value}
  @return{A @class{gtk-level-bar}.}
  @begin{short}
    Utility constructor that creates a new @class{gtk-level-bar} for the
    specified interval.
  @end{short}

  Since 3.6
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-new}"
  (make-instance 'gtk-level-bar
                 :min-value min-value
                 :max-value max-value))

#+gtk-3-6
(export 'gtk-level-bar-new-for-interval)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_set_mode ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-level-bar-set-mode))

#+gtk-3-6
(defun gtk-level-bar-set-mode (level-bar mode)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-3}
  @argument[level-bar]{a @class{gtk-level-bar}}
  @argument[mode]{a @symbol{gtk-level-bar-mode}}
  @begin{short}
    Sets the value of the @slot[gtk-level-bar]{mode} property.
  @end{short}

  Since 3.6
  @see-class{gtk-level-bar}
  @see-symbol{gtk-level-bar-mode}
  @see-function{gtk-level-bar-get-mode}"
  (setf (gtk-level-bar-mode level-bar) mode))

#+gtk-3-6
(export 'gtk-level-bar-set-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_get_mode ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-level-bar-get-mode))

#+gtk-3-6
(defun gtk-level-bar-get-mode (level-bar)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-3}
  @argument[level-bar]{a @class{gtk-level-bar}}
  @return{A @symbol{gtk-level-bar-mode}.}
  @begin{short}
    Returns the value of the @slot[gtk-level-bar]{mode} property.
  @end{short}

  Since 3.6
  @see-class{gtk-level-bar}
  @see-symbol{gtk-level-bar-mode}
  @see-function{gtk-level-bar-set-mode}"
  (gtk-level-bar-mode level-bar))

#+gtk-3-6
(export 'gtk-level-bar-get-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_set_value ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-level-bar-set-value))

#+gtk-3-6
(defun gtk-level-bar-set-value (level-bar value)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-3}
  @argument[level-bar]{a @class{gtk-level-bar}}
  @argument[value]{a value in the interval between
    @slot[gtk-level-bar]{min-value} and @slot[gtk-level-bar]{max-value}}
  @begin{short}
    Sets the value of the @slot[gtk-level-bar]{value} property.
  @end{short}

  Since 3.6
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-get-value}"
  (setf (gtk-level-bar-value level-bar) value))

#+gtk-3-6
(export 'gtk-level-bar-set-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_get_value ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-level-bar-get-value))

#+gtk-3-6
(defun gtk-level-bar-get-value (level-bar)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-3}
  @argument[level-bar]{a @class{gtk-level-bar}}
  @begin{return}
    A value in the interval between @slot[gtk-level-bar]{min-value} and
    @slot[gtk-level-bar]{max-value}.
  @end{return}
  @begin{short}
    Returns the value of the @slot[gtk-level-bar]{value} property.
  @end{short}

  Since 3.6
  @see-class{gtk-level-bar}
  @see-function{gtk-level-bar-set-value}"
  (gtk-level-bar-value level-bar))

#+gtk-3-6
(export 'gtk-level-bar-get-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_set_min_value ()
;;;
;;; void                gtk_level_bar_set_min_value         (GtkLevelBar *self,
;;;                                                          gdouble value);
;;;
;;; Sets the value of the "min-value" property.
;;;
;;; self :
;;;     a GtkLevelBar
;;;
;;; value :
;;;     a positive value
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-level-bar-set-min-value))

#+gtk-3-6
(defun gtk-level-bar-set-min-value (level-bar min-value)
  (setf (gtk-level-bar-min-value level-bar) min-value))

#+gtk-3-6
(export 'gtk-level-bar-set-min-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_get_min_value ()
;;;
;;; gdouble             gtk_level_bar_get_min_value         (GtkLevelBar *self);
;;;
;;; Returns the value of the "min-value" property.
;;;
;;; self :
;;;     a GtkLevelBar
;;;
;;; Returns :
;;;     a positive value
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-level-bar-get-min-value))

#+gtk-3-6
(defun gtk-level-bar-get-min-value (level-bar)
  (gtk-level-bar-min-value level-bar))

#+gtk-3-6
(export 'gtk-level-bar-get-min-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_set_max_value ()
;;;
;;; void                gtk_level_bar_set_max_value         (GtkLevelBar *self,
;;;                                                          gdouble value);
;;;
;;; Sets the value of the "max-value" property.
;;;
;;; self :
;;;     a GtkLevelBar
;;;
;;; value :
;;;     a positive value
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-level-bar-set-max-value))

#+gtk-3-6
(defun gtk-level-bar-set-max-value (level-bar max-value)
  (setf (gtk-level-bar-max-value level-bar) max-value))

#+gtk-3-6
(export 'gtk-level-bar-set-max-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_get_max_value ()
;;;
;;; gdouble             gtk_level_bar_get_max_value         (GtkLevelBar *self);
;;;
;;; Returns the value of the "max-value" property.
;;;
;;; self :
;;;     a GtkLevelBar
;;;
;;; Returns :
;;;     a positive value
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-level-bar-get-max-value))

#+gtk-3-6
(defun gtk-level-bar-get-max-value (level-bar)
  (gtk-level-bar-max-value level-bar))

#+gtk-3-6
(export 'gtk-level-bar-get-max-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_set_inverted ()
;;;
;;; void                gtk_level_bar_set_inverted          (GtkLevelBar *self,
;;;                                                          gboolean inverted);
;;;
;;; Sets the value of the "inverted" property.
;;;
;;; self :
;;;     a GtkLevelBar
;;;
;;; inverted :
;;;     TRUE to invert the level bar
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

#+gtk-3-8
(declaim (inline gtk-level-bar-set-inverted))

#+gtk-3-8
(defun gtk-level-bar-set-inverted (level-bar inverted)
  (setf (gtk-level-bar-inverted level-bar) inverted))

#+gtk-3-8
(export 'gtk-level-bar-set-inverted)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_get_inverted ()
;;;
;;; gboolean            gtk_level_bar_get_inverted          (GtkLevelBar *self);
;;;
;;; Return the value of the "inverted" property.
;;;
;;; self :
;;;     a GtkLevelBar
;;;
;;; Returns :
;;;     TRUE if the level bar is inverted
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

#+gtk-3-8
(declaim (inline gtk-level-bar-get-inverted))

#+gtk-3-8
(defun gtk-level-bar-get-inverted (level-bar)
  (gtk-level-bar-inverted level-bar))

#+gtk-3-8
(export 'gtk-level-bar-get-inverted)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_add_offset_value ()
;;;
;;; void                gtk_level_bar_add_offset_value      (GtkLevelBar *self,
;;;                                                          const gchar *name,
;;;                                                          gdouble value);
;;;
;;; Adds a new offset marker on self at the position specified by value. When
;;; the bar value is in the interval topped by value (or between value and
;;; "max-value" in case the offset is the last one on the bar) a style class
;;; named level-name will be applied when rendering the level bar fill. If
;;; another offset marker named name exists, its value will be replaced by
;;; value.
;;;
;;; self :
;;;     a GtkLevelBar
;;;
;;; name :
;;;     the name of the new offset
;;;
;;; value :
;;;     the value for the new offset
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(defcfun ("gtk_level_bar_add_offset_value" gtk-level-bar-add-offset-value) :void
  (level-bar (g-object gtk-level-bar))
  (name :string)
  (value :double))

#+gtk-3-6
(export 'gtk-level-bar-add-offset-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_remove_offset_value ()
;;;
;;; void                gtk_level_bar_remove_offset_value   (GtkLevelBar *self,
;;;                                                          const gchar *name);
;;;
;;; Removes an offset marker previously added with
;;; gtk_level_bar_add_offset_value().
;;;
;;; self :
;;;     a GtkLevelBar
;;;
;;; name :
;;;     the name of an offset in the bar. [allow-none]
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(defcfun ("gtk_level_bar_remove_offset_value" gtk-level-bar-remove-offset-value)
    :void
  (level-bar (g-object gtk-level-bar))
  (name :string))

#+gtk-3-6
(export 'gtk-level-bar-remove-offset-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_get_offset_value ()
;;;
;;; gboolean            gtk_level_bar_get_offset_value      (GtkLevelBar *self,
;;;                                                          const gchar *name,
;;;                                                          gdouble *value);
;;;
;;; Fetches the value specified for the offset marker name in self, returning
;;; TRUE in case an offset named name was found.
;;;
;;; self :
;;;     a GtkLevelBar
;;;
;;; name :
;;;     the name of an offset in the bar. [allow-none]
;;;
;;; value :
;;;     location where to store the value. [out]
;;;
;;; Returns :
;;;     TRUE if the specified offset is found
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(defcfun ("gtk_level_bar_get_offset_value" %gtk-level-bar-get-offset-value)
    :boolean
  (level-bar (g-object gtk-level-bar-get-offset-value))
  (name :string)
  (value :double))

#+gtk-3-6
(defun gtk-level-bar-get-offset-value (level-bar name)
  (with-foreign-object (value :double)
    (%gtk-level-bar-get-offset-value level-bar name value)
    (mem-ref value :double)))

#+gtk-3-6
(export 'gtk-level-bar-get-offset-value)

;;; --- End of file gtk.level-bar.lisp -----------------------------------------
