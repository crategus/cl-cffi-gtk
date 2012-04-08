;;; ----------------------------------------------------------------------------
;;; gtk.accel-label.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkAccelLabel
;;; 
;;; A label which displays an accelerator key on the right of the text
;;; 
;;; Synopsis
;;; 
;;;     GtkAccelLabel
;;;
;;;     gtk_accel_label_new
;;;     gtk_accel_label_set_accel_closure
;;;     gtk_accel_label_get_accel_widget
;;;     gtk_accel_label_set_accel_widget
;;;     gtk_accel_label_get_accel_width
;;;     gtk_accel_label_refetch
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkMisc
;;;                      +----GtkLabel
;;;                            +----GtkAccelLabel
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkAccelLabel implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "accel-closure"            GClosure*             : Read / Write
;;;   "accel-widget"             GtkWidget*            : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkAccelLabel widget is a subclass of GtkLabel that also displays an
;;; accelerator key on the right of the label text, e.g. 'Ctl+S'. It is
;;; commonly used in menus to show the keyboard short-cuts for commands.
;;; 
;;; The accelerator key to display is not set explicitly. Instead, the
;;; GtkAccelLabel displays the accelerators which have been added to a
;;; particular widget. This widget is set by calling
;;; gtk_accel_label_set_accel_widget().
;;; 
;;; For example, a GtkMenuItem widget may have an accelerator added to emit the
;;; "activate" signal when the 'Ctl+S' key combination is pressed. A
;;; GtkAccelLabel is created and added to the GtkMenuItem, and
;;; gtk_accel_label_set_accel_widget() is called with the GtkMenuItem as the
;;; second argument. The GtkAccelLabel will now display 'Ctl+S' after its label.
;;; 
;;; Note that creating a GtkMenuItem with gtk_menu_item_new_with_label() (or
;;; one of the similar functions for GtkCheckMenuItem and GtkRadioMenuItem)
;;; automatically adds a GtkAccelLabel to the GtkMenuItem and calls
;;; gtk_accel_label_set_accel_widget() to set it up for you.
;;; 
;;; A GtkAccelLabel will only display accelerators which have GTK_ACCEL_VISIBLE
;;; set (see GtkAccelFlags). A GtkAccelLabel can display multiple accelerators
;;; and even signal names, though it is almost always used to display just one
;;; accelerator key.
;;; 
;;; Example 49. Creating a simple menu item with an accelerator key.
;;; 
;;; GtkWidget *save_item;
;;; GtkAccelGroup *accel_group;
;;; 
;;; /* Create a GtkAccelGroup and add it to the window. */
;;; accel_group = gtk_accel_group_new ();
;;; gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);
;;; 
;;; /* Create the menu item using the convenience function. */
;;; save_item = gtk_menu_item_new_with_label ("Save");
;;; gtk_widget_show (save_item);
;;; gtk_container_add (GTK_CONTAINER (menu), save_item);
;;;
;;; /* Now add the accelerator to the GtkMenuItem. Note that since we called
;;;    gtk_menu_item_new_with_label() to create the GtkMenuItem the
;;;    GtkAccelLabel is automatically set up to display the GtkMenuItem
;;;    accelerators. We just need to make sure we use GTK_ACCEL_VISIBLE here.*/
;;; gtk_widget_add_accelerator (save_item, "activate", accel_group,
;;;                             GDK_KEY_s, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-closure" property
;;; 
;;;   "accel-closure"            GClosure*             : Read / Write
;;; 
;;; The closure to be monitored for accelerator changes.
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-widget" property
;;; 
;;;   "accel-widget"             GtkWidget*            : Read / Write
;;; 
;;; The widget to be monitored for accelerator changes.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAccelLabel
;;; 
;;; struct GtkAccelLabel;
;;; 
;;; The GtkAccelLabel struct contains private data only, and should be accessed
;;; using the functions below.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAccelLabel" gtk-accel-label
  (:superclass gtk-label
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_accel_label_get_type")
  ((accel-closure gtk-accel-label-accel-closure
    "accel-closure" "GClosure" t t)
   (accel-widget gtk-accel-label-accel-widget
    "accel-widget" "GtkWidget" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_new ()
;;; 
;;; GtkWidget * gtk_accel_label_new (const gchar *string)
;;; 
;;; Creates a new GtkAccelLabel.
;;; 
;;; string :
;;;     the label string. Must be non-NULL.
;;; 
;;; Returns :
;;;     a new GtkAccelLabel.
;;; ----------------------------------------------------------------------------

(defun gtk-accel-label-new (str)
  (make-instance 'gtk-accel-label :label str))

(export 'gtk-accel-label-new)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_set_accel_closure ()
;;; 
;;; void gtk_accel_label_set_accel_closure (GtkAccelLabel *accel_label,
;;;                                         GClosure *accel_closure)
;;; 
;;; Sets the closure to be monitored by this accelerator label. The closure
;;; must be connected to an accelerator group; see gtk_accel_group_connect().
;;; 
;;; accel_label :
;;;     a GtkAccelLabel
;;; 
;;; accel_closure :
;;;     the closure to monitor for accelerator changes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_get_accel_widget ()
;;; 
;;; GtkWidget * gtk_accel_label_get_accel_widget (GtkAccelLabel *accel_label)
;;; 
;;; Fetches the widget monitored by this accelerator label.
;;; See gtk_accel_label_set_accel_widget().
;;; 
;;; accel_label :
;;;     a GtkAccelLabel
;;; 
;;; Returns :
;;;     the object monitored by the accelerator label, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_set_accel_widget ()
;;; 
;;; void gtk_accel_label_set_accel_widget (GtkAccelLabel *accel_label,
;;;                                        GtkWidget *accel_widget)
;;; 
;;; Sets the widget to be monitored by this accelerator label.
;;; 
;;; accel_label :
;;;     a GtkAccelLabel
;;; 
;;; accel_widget :
;;;     the widget to be monitored.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_get_accel_width ()
;;; 
;;; guint gtk_accel_label_get_accel_width (GtkAccelLabel *accel_label)
;;; 
;;; Returns the width needed to display the accelerator key(s). This is used by
;;; menus to align all of the GtkMenuItem widgets, and shouldn't be needed by
;;; applications.
;;; 
;;; accel_label :
;;;     a GtkAccelLabel.
;;; 
;;; Returns :
;;;     the width needed to display the accelerator key(s).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_refetch ()
;;; 
;;; gboolean gtk_accel_label_refetch (GtkAccelLabel *accel_label)
;;; 
;;; Recreates the string representing the accelerator keys. This should not be
;;; needed since the string is automatically updated whenever accelerators are
;;; added or removed from the associated widget.
;;; 
;;; accel_label :
;;;     a GtkAccelLabel.
;;; 
;;; Returns :
;;;     always returns FALSE.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.accel-label.lisp ---------------------------------------
