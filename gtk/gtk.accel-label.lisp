;;; ----------------------------------------------------------------------------
;;; gtk.accel-label.lisp
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
;;; GtkAccelLabel
;;;
;;;     A label which displays an accelerator key on the right of the text.
;;;
;;; Types and Values
;;;
;;;     GtkAccelLabel
;;;
;;; Functions
;;;
;;;     gtk_accel_label_new
;;;     gtk_accel_label_set_accel_closure
;;;     gtk_accel_label_get_accel_widget
;;;     gtk_accel_label_set_accel_widget
;;;     gtk_accel_label_get_accel_width
;;;     gtk_accel_label_set_accel
;;;     gtk_accel_label_get_accel
;;;     gtk_accel_label_refetch
;;;
;;; Properties
;;;
;;;      GClosure*   accel-closure    Read / Write
;;;     GtkWidget*   accel-widget     Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkMisc
;;;                 ╰── GtkLabel
;;;                     ╰── GtkAccelLabel
;;;
;;; Implemented Interfaces
;;;     GtkAccelLabel implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAccelLabel
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAccelLabel" gtk-accel-label
  (:superclass gtk-label
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_accel_label_get_type")
  ((accel-closure
    gtk-accel-label-accel-closure
    "accel-closure" "GClosure" t t)
   (accel-widget
    gtk-accel-label-accel-widget
    "accel-widget" "GtkWidget" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-accel-label 'type)
 "@version{2021-11-13}
  @begin{short}
    The @sym{gtk-accel-label} widget is a subclass of the @class{gtk-label}
    class that also displays an accelerator key on the right of the label text,
    e.g. \"Ctrl+Q\".
  @end{short}
  It is commonly used in menus to show the keyboard short-cuts for commands.

  @image[accel-label]{}

  The accelerator key to display is not set explicitly. Instead, the accel label
  displays the accelerators which have been added to a particular widget. This
  widget is set by calling the @fun{gtk-accel-label-accel-widget} function.

  For example, a @class{gtk-menu-item} widget may have an accelerator added to
  emit the \"activate\" signal when the \"Ctrl+Q\" key combination is pressed.
  A @sym{gtk-accel-label} widget is created and added to the
  @class{gtk-menu-item} widget, and the @fun{gtk-accel-label-accel-widget}
  function is called with the @class{gtk-menu-item} widget as the second
  argument. The accel label will now display \"Ctrl+Q\" after its label.

  Note that creating a @class{gtk-menu-item} widget with the
  @fun{gtk-menu-item-new-with-label} function, or one of the similar functions
  for the @class{gtk-check-menu-item} and @class{gtk-radio-menu-item} widgets,
  automatically adds a @sym{gtk-accel-label} widget to the @class{gtk-menu-item}
  widget and calls the @fun{gtk-accel-label-accel-widget} function to set it up
  for you.

  A accel label will only display accelerators which have the @code{:visible}
  value of the @symbol{gtk-accel-flags} flags set. A accel label can display
  multiple accelerators and even signal names, though it is almost always used
  to display just one accelerator key.
  @begin[Example]{dictionary}
    Creating a menu item with an accelerator key.
    @begin{pre}
(let (...
      (item-file-quit (make-instance 'gtk-menu-item
                                     :label \"Quit\")))
  ;; Add an accelerator to the QUIT menu item
  (let ((group (gtk-accel-group-new)))
    (gtk-window-add-accel-group window group)
    (gtk-widget-add-accelerator item-file-quit
                                \"activate\"
                                group
                                (gdk-keyval-from-name \"q\")
                                :control-mask
                                :visible)
    ...)
...)
    @end{pre}
  @end{dictionary}
  @see-slot{gtk-accel-label-accel-closure}
  @see-slot{gtk-accel-label-accel-widget}
  @see-class{gtk-label}
  @see-class{gtk-menu-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-accel-label-accel-closure ------------------------------------------

;; TODO: GClosure is in the C implementatin a boxed type, but not in Lisp.
;; Therefore the accessor for accel-closure does not work.

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-closure"
                                               'gtk-accel-label) 't)
 "The @code{accel-closure} property of type @symbol{g-closure} (Read / Write)
  @br{}
  The closure to be monitored for accelerator changes.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-accel-label-accel-closure atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-accel-label-accel-closure 'function)
 "@version{2021-11-13}
  @syntax[]{(gtk-accel-label-accel-closure object) => closure}
  @syntax[]{(setf (gtk-accel-label-accel-closure object) closure)}
  @argument[label]{a @class{gtk-accel-label} widget}
  @argument[closure]{a @symbol{g-closure} instance to monitor for accelerator
    changes}
  @begin{short}
    Accessor of the @slot[gtk-accel-label]{accel-closure} slot of the
    @class{gtk-accel-label} class.
  @end{short}

  The @sym{gtk-accel-label-accel-closure} slot access function gets the closure
  to be monitored by this accelerator. The
  @sym{(setf gtk-accel-label-accel-closure)} slot access function sets the
  closure. The closure must be connected to an accelerator group, see the
  @code{gtk_accel_group_connect()} function.
  @see-class{gtk-accel-label}
  @see-symbol{g-closure}")

;;; --- gtk-accel-label-accel-widget -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-widget"
                                               'gtk-accel-label) 't)
 "The @code{accel-widget} property of type @class{gtk-widget} (Read / Write)
  @br{}
  The widget to be monitored for accelerator changes.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-accel-label-accel-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-accel-label-accel-widget 'function)
 "@version{2021-11-13}
  @syntax[]{(gtk-accel-label-accel-widget object) => widget}
  @syntax[]{(setf (gtk-accel-label-accel-widget object) widget)}
  @argument[label]{a @class{gtk-accel-label} widget}
  @argument[widget]{a @class{gtk-widget} object to be monitored}
  @begin{short}
    Accessor of the @slot[gtk-accel-label]{accel-widget} slot of the
    @class{gtk-accel-label} class.
  @end{short}

  The @sym{gtk-accel-label-accel-widget} slot access function returns the
  widget monitored by the accelerator label. The
  @sym{(setf gtk-accel-label-accel-widget)} slot access function sets the
  widget.
  @see-class{gtk-accel-label}
  @see-class{gtk-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-accel-label-new))

(defun gtk-accel-label-new (text)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-13}
  @argument[text]{a string with the text of the label}
  @return{A new @class{gtk-accel-label} widget.}
  @begin{short}
    Creates a new accel label.
  @end{short}
  @see-class{gtk-accel-label}"
  (make-instance 'gtk-accel-label
                 :label text))

(export 'gtk-accel-label-new)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_get_accel_width () -> gtk-accel-label-accel-width
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_label_get_accel_width" gtk-accel-label-accel-width) :int
  #+cl-cffi-gtk-documentation
 "@version{2021-11-13}
  @argument[label]{a @class{gtk-accel-label} widget}
  @return{An integer with the width needed to display the accelerator key(s).}
  @begin{short}
    Returns the width needed to display the accelerator key(s).
  @end{short}
  This is used by menus to align all of the @class{gtk-menu-item} widgets, and
  should not be needed by applications.
  @see-class{gtk-accel-label}
  @see-class{gtk-menu-item}"
  (label (g-object gtk-accel-label)))

(export 'gtk-accel-label-accel-width)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_set_accel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_label_set_accel" gtk-accel-label-set-accel) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-13}
  @argument[label]{a @class{gtk-accel-label} widget}
  @argument[key]{an unsigned integer with a keyval, or 0}
  @argument[mods]{a  @symbol{gdk-modifier-type} modifier mask for the accel}
  @begin{short}
    Manually sets a keyval and modifier mask as the accelerator rendered by
    @arg{label}.
  @end{short}
  If a keyval and modifier are explicitly set then these values are used
  regardless of any associated accel closure or widget. Providing an @arg{key}
  argument of 0 removes the manual setting.
  @see-class{gtk-accel-label}
  @see-symbol{gdk-modifier-type}"
  (label (g-object gtk-accel-label))
  (key :uint)
  (mods gdk-modifier-type))

(export 'gtk-accel-label-set-accel)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_get_accel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_label_get_accel" %gtk-accel-label-get-accel) :void
  (label (g-object gtk-accel-label))
  (key (:pointer :uint))
  (mods (:pointer gdk-modifier-type)))

(defun gtk-accel-label-get-accel (label)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-13}
  @argument[label]{a @class{gtk-accel-label} widget}
  @begin{return}
    @arg{key} --  an unsigned integer with a keyval @br{}
    @arg{mods} -- a @symbol{gdk-modifier-type} modifier mask
  @end{return}
  @begin{short}
    Gets the keyval and modifier mask set with the
    @fun{gtk-accel-label-set-accel} function.
  @end{short}
  @see-class{gtk-accel-label}
  @see-symbol{gdk-modifier-type}
  @see-function{gtk-accel-label-set-accel}"
  (with-foreign-objects ((key :uint)
                         (mods 'gdk-modifier-type))
    (%gtk-accel-label-get-accel label key mods)
    (values (mem-ref key :uint)
            (mem-ref mods 'gdk-modifier-type))))

(export 'gtk-accel-label-get-accel)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_refetch ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_label_refetch" gtk-accel-label-refetch) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-11-13}
  @argument[label]{a @class{gtk-accel-label} widget}
  @return{Always returns @em{false}.}
  @begin{short}
    Recreates the string representing the accelerator keys.
  @end{short}
  This should not be needed since the string is automatically updated whenever
  accelerators are added or removed from the associated widget.
  @see-class{gtk-accel-label}"
  (label (g-object gtk-accel-label)))

(export 'gtk-accel-label-refetch)

;;; End of file gtk.accel-label.lisp -------------------------------------------
