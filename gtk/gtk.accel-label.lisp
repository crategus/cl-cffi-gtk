;;; ----------------------------------------------------------------------------
;;; gtk.accel-label.lisp
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
 "@version{2020-4-19}
  @begin{short}
    The @sym{gtk-accel-label} widget is a subclass of @class{gtk-label} that
    also displays an accelerator key on the right of the label text, e.g.
    \"Ctl+S\".
  @end{short}
  It is commonly used in menus to show the keyboard short-cuts for commands.

  @image[accel-label]{}

  The accelerator key to display is not set explicitly. Instead, the
  @sym{gtk-accel-label} displays the accelerators which have been added to a
  particular widget. This widget is set by calling the function
  @fun{gtk-accel-label-accel-widget}.

  For example, a @class{gtk-menu-item} widget may have an accelerator added to
  emit the \"activate\" signal when the \"Ctl+S\" key combination is pressed. A
  @sym{gtk-accel-label} is created and added to the @class{gtk-menu-item}, and
  the function @fun{gtk-accel-label-accel-widget} is called with the
  @class{gtk-menu-item} as the second argument. The @sym{gtk-accel-label} will
  now display \"Ctl+S\" after its label.

  Note that creating a @class{gtk-menu-item} widget with the function
  @fun{gtk-menu-item-new-with-label} (or one of the similar functions for
  @class{gtk-check-menu-item} and @class{gtk-radio-menu-item}) automatically
  adds a @sym{gtk-accel-label} to the @class{gtk-menu-item} and calls the
  function @fun{gtk-accel-label-accel-widget} to set it up for you.

  A @sym{gtk-accel-label} will only display accelerators which have
  @code{:visible} set, see @symbol{gtk-accel-flags}. A @sym{gtk-accel-label}
  can display multiple accelerators and even signal names, though it is almost
  always used to display just one accelerator key.
  @begin[Example]{dictionary}
    Creating a simple menu item with an accelerator key.
    @begin{pre}
GtkWidget *save_item;
GtkAccelGroup *accel_group;

/* Create a GtkAccelGroup and add it to the window. */
accel_group = gtk_accel_group_new ();
gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);

/* Create the menu item using the convenience function. */
save_item = gtk_menu_item_new_with_label (\"Save\");
gtk_widget_show (save_item);
gtk_container_add (GTK_CONTAINER (menu), save_item);

/* Now add the accelerator to the GtkMenuItem. Note that since we called
   gtk_menu_item_new_with_label() to create the GtkMenuItem the
   GtkAccelLabel is automatically set up to display the GtkMenuItem
   accelerators. We just need to make sure we use GTK_ACCEL_VISIBLE
   here. */
gtk_widget_add_accelerator (save_item, \"activate\", accel_group,
                            GDK_KEY_s,
                            GDK_CONTROL_MASK,
                            GTK_ACCEL_VISIBLE);
    @end{pre}
  @end{dictionary}
  @see-slot{gtk-accel-label-accel-closure}
  @see-slot{gtk-accel-label-accel-widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-accel-label-accel-closure ------------------------------------------

;; TODO: GClosure is in the C implementatin a boxed type, but not in Lisp.
;; Therefore the accessor for accel-closure does not work.

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-closure"
                                               'gtk-accel-label) 't)
 "The @code{accel-closure} property of type @symbol{g-closure}
  (Read / Write) @br{}
  The closure to be monitored for accelerator changes.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-accel-label-accel-closure atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-accel-label-accel-closure 'function)
 "@version{2020-4-19}
  @syntax[]{(gtk-accel-label-accel-closure object) => closure}
  @syntax[]{(setf (gtk-accel-label-accel-closure object) closure)}
  @argument[label]{a @class{gtk-accel-label} widget}
  @argument[closure]{the closure to monitor for accelerator changes}
  @begin{short}
    Accessor of the @slot[gtk-accel-label]{accel-closure} slot of the
    @class{gtk-accel-label} class.
  @end{short}

  The slot access function @sym{gtk-accel-label-accel-closure} gets the closure
  to be monitored by this accelerator. The slot access function
  @sym{(setf gtk-accel-label-accel-closure)} sets the closure to be monitored
  by this accelerator label.

  The closure must be connected to an accelerator group; see the function
  @fun{gtk-accel-group-connect}.
  @see-class{gtk-accel-label}
  @see-function{gtk-accel-group-connect}")

;;; --- gtk-accel-label-accel-widget -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-widget"
                                               'gtk-accel-label) 't)
 "The @code{accel-widget} property of type @class{gtk-widget}
  (Read / Write) @br{}
  The widget to be monitored for accelerator changes.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-accel-label-accel-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-accel-label-accel-widget 'function)
 "@version{2020-4-19}
  @syntax[]{(gtk-accel-label-accel-widget object) => widget}
  @syntax[]{(setf (gtk-accel-label-accel-widget object) widget)}
  @argument[label]{a @class{gtk-accel-label} widget}
  @argument[widget]{the widget to be monitored}
  @begin{short}
    Accessor of the @slot[gtk-accel-label]{accel-widget} slot of the
    @class{gtk-accel-label} class.
  @end{short}

  The slot access function @sym{gtk-accel-label-accel-widget} returns the
  widget monitored by the accelerator label. The slot access functions
  @sym{(setf gtk-accel-label-accel-widget)} sets the widget to be monitored
  by this accelerator label.
  @see-class{gtk-accel-label}")

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-accel-label-new))

(defun gtk-accel-label-new (text)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-19}
  @argument[text]{a string with the text of the label}
  @return{A new @class{gtk-accel-label} widget.}
  @begin{short}
    Creates a new accel label widget.
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
 "@version{2020-4-19}
  @argument[label]{a @class{gtk-accel-label} widget}
  @return{An integer with the width needed to display the accelerator key(s).}
  @begin{short}
    Returns the width needed to display the accelerator key(s).
  @end{short}
  This is used by menus to align all of the @class{gtk-menu-item} widgets, and
  should not be needed by applications.
  @see-class{gtk-accel-label}"
  (label (g-object gtk-accel-label)))

(export 'gtk-accel-label-accel-width)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_set_accel ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(defcfun ("gtk_accel_label_set_accel" gtk-accel-label-set-accel) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-19}
  @argument[accel-label]{a @class{gtk-accel-label} widget}
  @argument[accelerator-key]{an unsigned integer with a keyval, or 0}
  @argument[accelerator-mods]{the modifier mask of type
    @symbol{gdk-modifier-type} for the accel}
  @begin{short}
    Manually sets a keyval and modifier mask as the accelerator rendered by
    @arg{accel-label}.
  @end{short}

  If a keyval and modifier are explicitly set then these values are used
  regardless of any associated accel closure or widget.

  Providing an @arg{accelerator-key} of 0 removes the manual setting.

  Since 3.6
  @see-class{gtk-accel-label}"
  (accel-label (g-object gtk-accel-label))
  (accelerator-key :uint)
  (accelerator-mods gdk-modifier-type))

#+gtk-3-6
(export 'gtk-accel-label-set-accel)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_get_accel ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun ("gtk_accel_label_get_accel" %gtk-accel-label-get-accel) :void
  (accel-label (g-object gtk-accel-label))
  (accelerator-key (:pointer :uint))
  (accelerator-mods (:pointer gdk-modifier-type)))

#+gtk-3-12
(defun gtk-accel-label-get-accel (accel-label)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-9}
  @argument[accel-label]{a @class{gtk-accel-label} widget}
  @begin{return}
    @code{accelerator-key} --  an unsigned integer with a keyval @br{}
    @code{accelerator-mods} -- the modifier mask of type
                               @symbol{gdk-modifier-type}
  @end{return}
  @begin{short}
    Gets the keyval and modifier mask set with the function
    @fun{gtk-accel-label-set-accel}.
  @end{short}

  Since 3.12
  @see-class{gtk-accel-label}
  @see-function{gtk-accel-label-set-accel}"
  (with-foreign-objects ((accelerator-key :uint)
                         (accelerator-mods 'gdk-modifier-type))
    (%gtk-accel-label-get-accel accel-label accelerator-key accelerator-mods)
    (values (mem-ref accelerator-key :uint)
            (mem-ref accelerator-mods 'gdk-modifier-type))))

#+gtk-3-12
(export 'gtk-accel-label-get-accel)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_refetch ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_label_refetch" gtk-accel-label-refetch) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-4-19}
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
