;;; ----------------------------------------------------------------------------
;;; gtk.recent-chooser-menu.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkRecentChooserMenu
;;; 
;;; Displays recently used files in a menu
;;; 
;;; Synopsis
;;; 
;;;     GtkRecentChooserMenu
;;;
;;;     gtk_recent_chooser_menu_new
;;;     gtk_recent_chooser_menu_new_for_manager
;;;     gtk_recent_chooser_menu_get_show_numbers
;;;     gtk_recent_chooser_menu_set_show_numbers
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkMenuShell
;;;                            +----GtkMenu
;;;                                  +----GtkRecentChooserMenu
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkRecentChooserMenu implements AtkImplementorIface, GtkBuildable,
;;; GtkRecentChooser and GtkActivatable.
;;;
;;; Properties
;;; 
;;;   "show-numbers"             gboolean              : Read / Write
;;; 
;;; Description
;;; 
;;; GtkRecentChooserMenu is a widget suitable for displaying recently used files
;;; inside a menu. It can be used to set a sub-menu of a GtkMenuItem using
;;; gtk_menu_item_set_submenu(), or as the menu of a GtkMenuToolButton.
;;; 
;;; Note that GtkRecentChooserMenu does not have any methods of its own.
;;; Instead, you should use the functions that work on a GtkRecentChooser.
;;; 
;;; Note also that GtkRecentChooserMenu does not support multiple filters, as it
;;; has no way to let the user choose between them as the GtkRecentChooserWidget
;;; and GtkRecentChooserDialog widgets do. Thus using
;;; gtk_recent_chooser_add_filter() on a GtkRecentChooserMenu widget will yield
;;; the same effects as using gtk_recent_chooser_set_filter(), replacing any
;;; currently set filter with the supplied filter;
;;; gtk_recent_chooser_remove_filter() will remove any currently set
;;; GtkRecentFilter object and will unset the current filter;
;;; gtk_recent_chooser_list_filters() will return a list containing a single
;;; GtkRecentFilter object.
;;; 
;;; Recently used files are supported since GTK+ 2.10.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-numbers" property
;;; 
;;;   "show-numbers"             gboolean              : Read / Write
;;; 
;;; Whether the first ten items in the menu should be prepended by a number
;;; acting as a unique mnemonic.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentChooserMenu
;;; 
;;; struct GtkRecentChooserMenu;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRecentChooserMenu" gtk-recent-chooser-menu
  (:superclass gtk-menu
   :export t
   :interfaces ("AtkImplementorIface" "GtkActivatable" "GtkBuildable"
                "GtkRecentChooser")
   :type-initializer "gtk_recent_chooser_menu_get_type")
  ((show-numbers
    gtk-recent-chooser-menu-show-numbers
    "show-numbers" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_menu_new ()
;;; 
;;; GtkWidget * gtk_recent_chooser_menu_new (void);
;;; 
;;; Creates a new GtkRecentChooserMenu widget.
;;; 
;;; This kind of widget shows the list of recently used resources as a menu,
;;; each item as a menu item. Each item inside the menu might have an icon,
;;; representing its MIME type, and a number, for mnemonic access.
;;; 
;;; This widget implements the GtkRecentChooser interface.
;;; 
;;; This widget creates its own GtkRecentManager object. See the
;;; gtk_recent_chooser_menu_new_for_manager() function to know how to create a
;;; GtkRecentChooserMenu widget bound to another GtkRecentManager object.
;;; 
;;; Returns :
;;;     a new GtkRecentChooserMenu
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_menu_new_for_manager ()
;;; 
;;; GtkWidget * gtk_recent_chooser_menu_new_for_manager
;;;                                                  (GtkRecentManager *manager)
;;; 
;;; Creates a new GtkRecentChooserMenu widget using manager as the underlying
;;; recently used resources manager.
;;; 
;;; This is useful if you have implemented your own recent manager, or if you
;;; have a customized instance of a GtkRecentManager object or if you wish to
;;; share a common GtkRecentManager object among multiple GtkRecentChooser
;;; widgets.
;;; 
;;; manager :
;;;     a GtkRecentManager
;;; 
;;; Returns :
;;;     a new GtkRecentChooserMenu, bound to manager.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_menu_get_show_numbers ()
;;; 
;;; gboolean gtk_recent_chooser_menu_get_show_numbers
;;;                                                 (GtkRecentChooserMenu *menu)
;;; 
;;; Returns the value set by gtk_recent_chooser_menu_set_show_numbers().
;;; 
;;; menu :
;;;     a GtkRecentChooserMenu
;;; 
;;; Returns :
;;;     TRUE if numbers should be shown
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_menu_set_show_numbers ()
;;; 
;;; void gtk_recent_chooser_menu_set_show_numbers (GtkRecentChooserMenu *menu,
;;;                                                gboolean show_numbers);
;;; 
;;; Sets whether a number should be added to the items of menu. The numbers are
;;; shown to provide a unique character for a mnemonic to be used inside ten
;;; menu item's label. Only the first the items get a number to avoid clashes.
;;; 
;;; menu :
;;;     a GtkRecentChooserMenu
;;; 
;;; show_numbers :
;;;     whether to show numbers
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.recent-chooser-menu.lisp -------------------------------
