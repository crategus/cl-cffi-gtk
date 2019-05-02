;;; ----------------------------------------------------------------------------
;;; gtk.shortcuts-shortcut.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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
;;; GtkShortcutsShortcut
;;;
;;;     Represents a keyboard shortcut in a GtkShortcutsWindow
;;;
;;; Types and Values
;;;
;;;     GtkShortcutsShortcut
;;;     GtkShortcutType
;;;
;;; Properties
;;;
;;;         GtkSizeGroup*  accel-size-group    Write
;;;                gchar*  accelerator         Read / Write
;;;                gchar*  action-name         Read / Write
;;;     GtkTextDirection   direction           Read / Write
;;;                GIcon*  icon                Read / Write
;;;             gboolean   icon-set            Read / Write
;;;      GtkShortcutType   shortcut-type       Read / Write
;;;                gchar*  subtitle            Read / Write
;;;             gboolean   subtitle-set        Read / Write
;;;                gchar*  title               Read / Write
;;;         GtkSizeGroup*  title-size-group    Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkShortcutsShortcut
;;;
;;; Implemented Interfaces
;;;
;;;     GtkShortcutsShortcut implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkShortcutType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkShortcutType" gtk-shortcut-type
  (:export t
   :type-initializer "gtk_shortcut_type_get_type")
  :accelerator
  :gesture-pinch
  :gesture-stretch
  :gesture-rotate-clockwise
  :gesture-rotate-conterclockwise
  :gesture-two-finger-swipe-left
  :gesture-two-finger-swipe-right
  :gesture)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcut-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-shortcut-type atdoc:*external-symbols*)
 "@version{2019-4-12}
  @begin{short}
    GtkShortcutType specifies the kind of shortcut that is being described.
  @end{short}
  More values may be added to this enumeration over time.
  @begin{pre}
(define-g-enum \"GtkShortcutType\" gtk-shortcut-type
  (:export t
   :type-initializer \"gtk_shortcut_type_get_type\")
  :accelerator
  :gesture-pinch
  :gesture-stretch
  :gesture-rotate-clockwise
  :gesture-rotate-conterclockwise
  :gesture-two-finger-swipe-left
  :gesture-two-finger-swipe-right
  :gesture)
  @end{pre}
  @begin[code]{table}
    @entry[:accelerator]{The shortcut is a keyboard accelerator. The
      @code{accelerator} property will be used.}
    @entry[:gesture-pinch]{The shortcut is a pinch gesture. GTK+ provides an
      icon and subtitle.}
    @entry[:gesture-stretch]{The shortcut is a stretch gesture. GTK+ provides an
      icon and subtitle.}
    @entry[:gesture-rotate-clockwise]{The shortcut is a clockwise rotation
      gesture. GTK+ provides an icon and subtitle.}
    @entry[:gesture-rotate-counter-clockwise]{The shortcut is a counterclockwise
      rotation gesture. GTK+ provides an icon and subtitle.}
    @entry[:gesture-two-finger-swipe-left]{The shortcut is a two-finger swipe
      gesture. GTK+ provides an icon and subtitle.}
    @entry[:gesture-two-finger-swipe-right]{The shortcut is a two-finger swipe
      gesture. GTK+ provides an icon and subtitle.}
    @entry[:gesture]{The shortcut is a gesture. The @code{icon} property will
      be used.}
  @end{table}
  Since 3.20
  @see-class{gtk-shortcuts-shortcut}")

;;; ----------------------------------------------------------------------------
;;; struct GtkShortcutsShortcut
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkShortcutsShortcut" gtk-shortcuts-shortcut
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_shortcuts_shortcut_get_type")
  ((accel-size-group
    gtk-shortcuts-shortcut-accel-size-group
    "accel-size-group" "GtkSizeGroup" nil t)
   (accelerator
    gtk-shortcuts-shortcut-accelerator
    "accelerator" "gchararray" t t)
   #+gtk-3-22
   (action-name
    gtk-shortcuts-shortcut-action-name
    "action-name" "gchararray" t t)
   (direction
    gtk-shortcuts-shortcut-direction
    "direction" "GtkTextDirection" t t)
   (icon
    gtk-shortcuts-shortcut-icon
    "icon" "GIcon" t t)
   (icon-set
    gtk-shortcus-shortcut-icon-set
    "icon-set" "gboolean" t t)
   (shortcut-type
    gtk-shortcuts-shortcut-shortcut-type
    "shortcut-type" "GtkShortcutType" t t)
   (subtitle
    gtk-shortcuts-shortcut-subtitle
    "subtitle" "gchararray" t t)
   (subtitle-set
    gtk-shortcuts-shortcut-subtitle-set
    "subtitle-set" "gboolean" t t)
   (title
    gtk-shortcuts-shortcut-title
    "title" "gchararray" t t)
   (title-size-group
    gtk-shortcuts-shortcut-title-size-group
    "title-size-group" "GtkSizeGroup" nil t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-shortcuts-shortcut 'type)
 "@version{2019-4-12}
  @begin{short}
    A @sym{gtk-shortcuts-shortcut} represents a single keyboard shortcut or
    gesture with a short text.
  @end{short}
  This widget is only meant to be used with @class{gtk-shortcuts-window}.
  @see-slot{gtk-shortcuts-shortcut-accel-size-group}
  @see-slot{gtk-shortcuts-shortcut-accelerator}
  @see-slot{gtk-shortcuts-shortcut-action-name}
  @see-slot{gtk-shortcuts-shortcut-direction}
  @see-slot{gtk-shortcuts-shortcut-icon}
  @see-slot{gtk-shortcuts-shortcut-icon-set}
  @see-slot{gtk-shortcuts-shortcut-shortcut-type}
  @see-slot{gtk-shortcuts-shortcut-subtitle}
  @see-slot{gtk-shortcuts-shortcut-subtitle-set}
  @see-slot{gtk-shortcuts-shortcut-title}
  @see-slot{gtk-shortcuts-shortcut-title-size-group}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-shortcuts-shortcut-accel-size-group --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-size-group"
                      'gtk-shortcuts-shortcut) 't)
 "The @code{accel-size-group} property of type @symbol{gtk-size-group}
  (Write) @br{}
  The size group for the accelerator portion of this shortcut. This is used
  internally by GTK+, and must not be modified by applications.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-shortcut-accel-size-group
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-shortcut-accel-size-group 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-shortcut]{accel-size-group} of the
    @class{gtk-shortcuts-shortcut} class.
  @end{short}
  @see-class{gtk-shortcuts-shortcut}")

;;; --- gtk-shortcuts-shortcut-accelerator -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accelerator"
                      'gtk-shortcuts-shortcut) 't)
 "The @code{accelerator} property of type @code{:string} (Read / Write) @br{}
  The accelerator(s) represented by this object. This property is used if
  \"shortcut-type\" is set to @code{:accelerator}.
  The syntax of this property is (an extension of) the syntax understood by the
  @fun{gtk-accelerator-parse} function. Multiple accelerators can be specified
  by separating them with a space, but keep in mind that the available width is
  limited. It is also possible to specify ranges of shortcuts, using ... between
  the keys. Sequences of keys can be specified using a + or & between the keys.
  @br{}
  @em{Examples:} @br{}
  A single shortcut: <ctl><alt>delete @br{}
  Two alternative shortcuts: <shift>a Home @br{}
  A range of shortcuts: <alt>1...<alt>9 @br{}
  Several keys pressed together: Control_L&Control_R @br{}
  A sequence of shortcuts or keys: <ctl>c+<ctl>x @br{}
  Use + instead of & when the keys may (or have to be) pressed sequentially
  (e.g use t+t for 'press the t key twice'). @br{}
  Note that <, > and & need to be escaped as <, > and & when used in .ui files.
  @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-shortcut-accelerator
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-shortcut-accelerator 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-shortcut]{accelerator} of the
    @class{gtk-shortcuts-shortcut} class.
  @end{short}
  @see-class{gtk-shortcuts-shortcut}")

;;; --- gtk-shortcuts-shortcut-action-name -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "action-name"
                      'gtk-shortcuts-shortcut) 't)
 "The @code{action-name} property of type @code{:string} (Read / Write) @br{}
  A detailed action name. If this is set for a shortcut of type
  @code{:accelerator}, then GTK+ will use the accelerators that are associated
  with the action via the @fun{gtk-application-set-accels-for-action}, and
  setting @code{accelerator} is not necessary. @br{}
  Default value: @code{nil} @br{}
  Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-shortcut-action-name
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-shortcut-action-name 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-shortcut]{action-name} of the
    @class{gtk-shortcuts-shortcut} class.
  @end{short}
  @see-class{gtk-shortcuts-shortcut}")

;;; --- gtk-shortcuts-shortcut-direction ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "direction"
                      'gtk-shortcuts-shortcut) 't)
 "The @code{direction} property of type @symbol{GtkTextDirection} (Read / Write)
  @br{}
  The text direction for which this shortcut is active. If the shortcut is used
  regardless of the text direction, set this property to @code{:none}. @br{}
  Default value: @code{:dir-none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-shortcut-direction atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-shortcut-direction 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-shortcut]{direction} of the
    @class{gtk-shortcuts-shortcut} class.
  @end{short}
  @see-class{gtk-shortcuts-shortcut}")

;;; --- gtk-shortcuts-shortcut-icon --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon"
                      'gtk-shortcuts-shortcut) 't)
 "The @code{icon} property of type @class{GIcon} (Read / Write) @br{}
  An icon to represent the shortcut or gesture. This property is used if
  @code{shortcut-type} is set to @code{:gesture}. For the other predefined
  gesture types, GTK+ provides an icon on its own.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-shortcut-icon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-shortcut-icon 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-shortcut]{icon} of the
    @class{gtk-shortcuts-shortcut} class.
  @end{short}
  @see-class{gtk-shortcuts-shortcut}")

;;; --- gtk-shortcuts-shortcut-icon-set ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-set"
                      'gtk-shortcuts-shortcut) 't)
 "The @code{icon-set} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} if an icon has been set. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-shortcut-icon-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-shortcut-icon-set 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-shortcut]{icon-set} of the
    @class{gtk-shortcuts-shortcut} class.
  @end{short}
  @see-class{gtk-shortcuts-shortcut}")

;;; --- gtk-shortcuts-shortcut-shortcut-type -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shortcut-type"
                      'gtk-shortcuts-shortcut) 't)
 "The @code{shortcut-type} property of type @symbol{gtk-shortcut-type}
  (Read / Write) @br{}
  The type of shortcut that is represented. @br{}
  Default value: @code{:accelerator}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-shortcut-shortcut-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-shortcut-shortcut-type 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-shortcut]{shortcut-type} of the
    @class{gtk-shortcuts-shortcut} class.
  @end{short}
  @see-class{gtk-shortcuts-shortcut}")

;;; --- gtk-shortcuts-shortcut-subtitle ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "subtitle"
                      'gtk-shortcuts-shortcut) 't)
 "The @code{subtitle} property of type @code{:string} (Read / Write) @br{}
  The subtitle for the shortcut or gesture.
  This is typically used for gestures and should be a short, one-line text that
  describes the gesture itself. For the predefined gesture types, GTK+ provides
  a subtitle on its own. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-shortcut-subtitle atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-shortcut-subtitle 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-shortcut]{subtitle} of the
    @class{gtk-shortcuts-shortcut} class.
  @end{short}
  @see-class{gtk-shortcuts-shortcut}")

;;; --- gtk-shortcuts-shortcut-subtitle-set ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "subtitle-set"
                      'gtk-shortcuts-shortcut) 't)
 "The @code{subtitle-set} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} if a subtitle has been set. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-shortcut-subtitle-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-shortcut-subtitle-set 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-shortcut]{subtitle-set} of the
    @class{gtk-shortcuts-shortcut} class.
  @end{short}
  @see-class{gtk-shortcuts-shortcut}")

;;; --- gtk-shortcuts-shortcut-title -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title"
                      'gtk-shortcuts-shortcut) 't)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The textual description for the shortcut or gesture represented by this
  object. This should be a short string that can fit in a single line. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-shortcut-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-shortcut-title 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-shortcut]{title} of the
    @class{gtk-shortcuts-shortcut} class.
  @end{short}
  @see-class{gtk-shortcuts-shortcut}")

;;; --- gtk-shortcuts-shortcut-title-size-group --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title-size-group"
                      'gtk-shortcuts-shortcut) 't)
 "The @code{title-size-group} property of type @symbol{gtk-size-group}
  (Write) @br{}
  The size group for the textual portion of this shortcut. This is used
  internally by GTK+, and must not be modified by applications. @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-shortcut-title-size-group
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-shortcut-title-size-group 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-shortcut]{title-size-group} of the
    @class{gtk-shortcuts-shortcut} class.
  @end{short}
  @see-class{gtk-shortcuts-shortcut}")

;;; --- End of file gtk.shortcuts-shortcut.lisp --------------------------------
