;;; ----------------------------------------------------------------------------
;;; gtk.accel-label.lisp
;;;
;;; Documentation strings for the library GTK.
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

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "accel-closure" 'gtk-accel-label) 't)
 "@version{2013-1-13}
  The @arg{\"accel-closure\"} property of type @symbol{g-closure}
  (Read / Write)@br{}
  The closure to be monitored for accelerator changes.")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "accel-widget" 'gtk-accel-label) 't)
 "@version{2013-1-13}
  The @arg{\"accel-widget\"} property of type @class{gtk-widget}
  (Read / Write)@br{}
  The widget to be monitored for accelerator changes.")

;;; --- gtk-accel-label --------------------------------------------------------

(setf (documentation 'gtk-accel-label 'type)
 "@version{2013-1-12}
  @begin{short}
    The GtkAccelLabel widget is a subclass of GtkLabel that also displays an
    accelerator key on the right of the label text, e.g. 'Ctl+S'. It is commonly
    used in menus to show the keyboard short-cuts for commands.
  @end{short}

  The accelerator key to display is not set explicitly. Instead, the
  GtkAccelLabel displays the accelerators which have been added to a
  particular widget. This widget is set by calling
  gtk_accel_label_set_accel_widget().

  For example, a GtkMenuItem widget may have an accelerator added to emit the
  \"activate\" signal when the 'Ctl+S' key combination is pressed. A
  GtkAccelLabel is created and added to the GtkMenuItem, and
  gtk_accel_label_set_accel_widget() is called with the GtkMenuItem as the
  second argument. The GtkAccelLabel will now display 'Ctl+S' after its label.

  Note that creating a GtkMenuItem with gtk_menu_item_new_with_label() (or one
  of the similar functions for GtkCheckMenuItem and GtkRadioMenuItem)
  automatically adds a GtkAccelLabel to the GtkMenuItem and calls
  gtk_accel_label_set_accel_widget() to set it up for you.

  A GtkAccelLabel will only display accelerators which have GTK_ACCEL_VISIBLE
  set (see GtkAccelFlags). A GtkAccelLabel can display multiple accelerators
  and even signal names, though it is almost always used to display just one
  accelerator key.
 
  Example 49. Creating a simple menu item with an accelerator key.
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
  @see-slot{gtk-accel-label-accel-closure}
  @see-slot{gtk-accel-label-accel-widget}
")

;;; --- gtk-accel-label-new ----------------------------------------------------

(setf (documentation 'gtk-accel-label-new 'function)
 "@version{2013-1-13}
  @argument[string]{the label string. Must be @code{nil}.}
  @return{a new @class{gtk-accel-label} instance.}
  @short{Creates a new GtkAccelLabel.}")

;;; --- gtk-accel-label-accel-closure ------------------------------------------

(setf (gethash 'gtk-accel-label-accel-closure atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-accel-label-accel-closure 'function)
 "@version{2013-1-13}
  @begin{short}
    Accessor of the slot @arg{\"accel-closure\"} of the @class{gtk-accel-label}
    class.
  @end{short}")

;;; --- gtk-accel-label-set-accel-closure --------------------------------------

(setf (documentation 'gtk-accel-label-set-accel-closure 'function)
 "@version{2013-1-13}
  @argument[accel-label]{a @class{gtk-accel-label} instance}
  @argument[accel-closure]{the closure to monitor for accelerator changes.}
  @begin{short}
    Sets the closure to be monitored by this accelerator label.
  @end{short}
  The closure must be connected to an accelerator group;
  see @fun{gtk-accel-group-connect}.
  @see-function{gtk-accel-group-connect}")

;;; --- gtk-accel-label-accel-widget -------------------------------------------

(setf (gethash 'gtk-accel-label-accel-widget atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-accel-label-accel-widget 'function)
 "@version{2013-1-13}
  @begin{short}
    Accessor of the slot @arg{\"accel-widget\"} of the @class{gtk-accel-label}
    class.
  @end{short}")

;;; --- gtk-accel-label-get-accel-widget ---------------------------------------

(setf (documentation 'gtk-accel-label-get-accel-widget 'function)
 "@version{2013-1-13}
  @argument[accel-label]{a @class{gtk-accel-label} instance}
  @return{the object monitored by the accelerator label, or @code{nil}}
  @begin{short}
    Fetches the widget monitored by this accelerator label.
  @end{short}
  See @fun{gtk-accel-label-set-accel-widget}.
  @see-function{gtk-accel-label-set-accel-widget}")

;;; --- gtk-accel-label-set-accel-widget ---------------------------------------

(setf (documentation 'gtk-accel-label-set-accel-widget 'function)
 "@version{2013-1-13}
  @argument[accel-label]{a @class{gtk-accel-label} instance}
  @argument[accel-widget]{the widget to be monitored.}
  @begin{short}
    Sets the widget to be monitored by this accelerator label.
  @end{short}")

;;; --- gtk-accel-label-get-accel-width ----------------------------------------

(setf (documentation 'gtk-accel-label-get-accel-width 'function)
 "@version{2013-1-13}
  @argument[accel-label]{a @class{gtk-accel-label} instance}
  @return{The width needed to display the accelerator key(s).}
  @begin{short}
    Returns the width needed to display the accelerator key(s).
  @end{short}
  This is used by menus to align all of the @class{gtk-menu-item} widgets, and
  shouldn't be needed by applications.")

;;; --- gtk-accel-label-refetch ------------------------------------------------

(setf (documentation 'gtk-accel-label-refetch 'function)
 "@version{2013-1-13}
  @argument[accel-label]{a @class{gtk-accel-label} instance}
  @return{Always returns @code{nil}.}
  @begin{short}
    Recreates the string representing the accelerator keys.
  @end{short}
  This should not be needed since the string is automatically updated whenever
  accelerators are added or removed from the associated widget.")

;;; End of file atdoc-gtk.accel-label.lisp -------------------------------------
