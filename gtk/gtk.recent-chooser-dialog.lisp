;;; ----------------------------------------------------------------------------
;;; gtk.recent-chooser-dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkRecentChooserDialog
;;;
;;; Displays recently used files in a dialog
;;;
;;; Synopsis
;;;
;;;     GtkRecentChooserDialog
;;;
;;;     gtk_recent_chooser_dialog_new
;;;     gtk_recent_chooser_dialog_new_for_manager
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkWindow
;;;                                  +----GtkDialog
;;;                                        +----GtkRecentChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;; GtkRecentChooserDialog implements AtkImplementorIface, GtkBuildable and
;;; GtkRecentChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentChooserDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRecentChooserDialog" gtk-recent-chooser-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkRecentChooser")
   :type-initializer "gtk_recent_chooser_dialog_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-recent-chooser-dialog 'type)
 "@version{2013-5-26}
  @begin{short}
    @sym{gtk-recent-chooser-dialog} is a dialog box suitable for displaying the
    recently used documents. This widgets works by putting a
    @class{gtk-recent-chooser-widget} inside a @class{gtk-dialog}. It exposes
    the @class{gtk-recent-chooser-iface} interface, so you can use all the
    @class{gtk-recent-chooser} functions on the recent chooser dialog as
    well as those for @class{gtk-dialog}.
  @end{short}
  Note that @sym{gtk-recent-chooser-dialog} does not have any methods of its
  own. Instead, you should use the functions that work on a
  @class{gtk-recent-chooser}.

  @b{Example:} Typical usage.
  In the simplest of cases, you can use the following code to use a
  @sym{gtk-recent-chooser-dialog} to select a recently used file:
  @begin{pre}
 GtkWidget *dialog;

 dialog = gtk_recent_chooser_dialog_new
                                      (\"Recent Documents\",
                                       parent_window,
                                       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                       GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                                       NULL);

 if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
   {
     GtkRecentInfo *info;

     info= gtk_recent_chooser_get_current_item (GTK_RECENT_CHOOSER (dialog));
     open_file (gtk_recent_info_get_uri (info));
     gtk_recent_info_unref (info);
   @}

 gtk_widget_destroy (dialog);
  @end{pre}
  Recently used files are supported since GTK+ 2.10.
  @see-class{gtk-recent-chooser}
  @see-class{gtk-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_dialog_new ()
;;;
;;; GtkWidget * gtk_recent_chooser_dialog_new (const gchar *title,
;;;                                            GtkWindow *parent,
;;;                                            const gchar *first_button_text,
;;;                                            ...);
;;;
;;; Creates a new GtkRecentChooserDialog. This function is analogous to
;;; gtk_dialog_new_with_buttons().
;;;
;;; title :
;;;     Title of the dialog, or NULL.
;;;
;;; parent :
;;;     Transient parent of the dialog, or NULL,.
;;;
;;; first_button_text :
;;;     stock ID or text to go in the first button, or NULL
;;;
;;; ... :
;;;     response ID for the first button, then additional (button, id) pairs,
;;;     ending with NULL
;;;
;;; Returns :
;;;     a new GtkRecentChooserDialog
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_dialog_new_for_manager ()
;;;
;;; GtkWidget * gtk_recent_chooser_dialog_new_for_manager
;;;                                             (const gchar *title,
;;;                                              GtkWindow *parent,
;;;                                              GtkRecentManager *manager,
;;;                                              const gchar *first_button_text,
;;;                                              ...);
;;;
;;; Creates a new GtkRecentChooserDialog with a specified recent manager.
;;;
;;; This is useful if you have implemented your own recent manager, or if you
;;; have a customized instance of a GtkRecentManager object.
;;;
;;; title :
;;;     Title of the dialog, or NULL
;;;
;;; parent :
;;;     Transient parent of the dialog, or NULL,
;;;
;;; manager :
;;;     a GtkRecentManager
;;;
;;; first_button_text :
;;;     stock ID or text to go in the first button, or NULL
;;;
;;; ... :
;;;     response ID for the first button, then additional (button, id) pairs,
;;;     ending with NULL
;;;
;;; Returns :
;;;     a new GtkRecentChooserDialog
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; --- End of file gkt.recent-chooser-dialog.lisp -----------------------------
