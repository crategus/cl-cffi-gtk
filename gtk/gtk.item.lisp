;;; ----------------------------------------------------------------------------
;;; gtk.item.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 2 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkItem
;;; 
;;; Abstract base class for GtkMenuItem, GtkListItem and GtkTreeItem
;;; 
;;; Synopsis
;;; 
;;;     GtkItem
;;;
;;;     gtk_item_select
;;;     gtk_item_deselect
;;;     gtk_item_toggle
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkObject
;;;                +----GtkWidget
;;;                      +----GtkContainer
;;;                            +----GtkBin
;;;                                  +----GtkItem
;;;                                        +----GtkMenuItem
;;;                                        +----GtkListItem
;;;                                        +----GtkTreeItem
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkItem implements AtkImplementorIface and GtkBuildable.
;;;
;;; Signals
;;; 
;;;   "deselect"                                       : Run First
;;;   "select"                                         : Run First
;;;   "toggle"                                         : Run First
;;; 
;;; Description
;;; 
;;; The GtkItem widget is an abstract base class for GtkMenuItem, GtkListItem
;;; and GtkTreeItem.
;;; 
;;; GtkItem is deprecated and will be removed in GTK+ 3.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "deselect" signal
;;; 
;;; void user_function (GtkItem *item,
;;;                     gpointer user_data)      : Run First
;;; 
;;; Emitted when the item is deselected.
;;; 
;;; item :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "select" signal
;;; 
;;; void user_function (GtkItem *item,
;;;                     gpointer user_data)      : Run First
;;; 
;;; Emitted when the item is selected.
;;; 
;;; item :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggle" signal
;;; 
;;; void user_function (GtkItem *item,
;;;                     gpointer user_data)      : Run First
;;; 
;;; Emitted when the item is toggled.
;;; 
;;; item :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkItem
;;; 
;;; struct GtkItem;
;;; 
;;; The GtkItem struct contains private data only, and should be accessed using
;;; the functions below.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkItem" gtk-item
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_item_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_item_select ()
;;; 
;;; void gtk_item_select (GtkItem *item);
;;; 
;;; Warning
;;; 
;;; gtk_item_select has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use gtk_menu_item_select() instead
;;; 
;;; Emits the "select" signal on the given item.
;;; 
;;; item :
;;;     a GtkItem
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_deselect ()
;;; 
;;; void gtk_item_deselect (GtkItem *item);
;;; 
;;; Warning
;;; 
;;; gtk_item_deselect has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use gtk_menu_item_deselect() instead
;;; 
;;; Emits the "deselect" signal on the given item.
;;; 
;;; item :
;;;     a GtkItem
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_toggle ()
;;; 
;;; void gtk_item_toggle (GtkItem *item);
;;; 
;;; Warning
;;; 
;;; gtk_item_toggle has been deprecated since version 2.22 and should not be
;;; used in newly-written code. This function will be removed in GTK+ 3
;;; 
;;; Emits the "toggle" signal on the given item.
;;; 
;;; item :
;;;     a GtkItem
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.item.lisp ----------------------------------------------
