;;; ----------------------------------------------------------------------------
;;; gtk.recent-action.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
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
;;;
;;; GtkRecentAction
;;;
;;; An action of which represents a list of recently used files
;;;
;;; Synopsis
;;;
;;;     GtkRecentAction
;;;
;;;     gtk_recent_action_new
;;;     gtk_recent_action_new_for_manager
;;;     gtk_recent_action_get_show_numbers
;;;     gtk_recent_action_set_show_numbers
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GtkAction
;;;          +----GtkRecentAction
;;;
;;; Implemented Interfaces
;;;
;;; GtkRecentAction implements GtkBuildable and GtkRecentChooser.
;;;
;;; Properties
;;;
;;;   "show-numbers"             gboolean              : Read / Write
;;;
;;; Description
;;;
;;; A GtkRecentAction represents a list of recently used files, which can be
;;; shown by widgets such as GtkRecentChooserDialog or GtkRecentChooserMenu.
;;;
;;; To construct a submenu showing recently used files, use a GtkRecentAction as
;;; the action for a <menuitem>. To construct a menu toolbutton showing the
;;; recently used files in the popup menu, use a GtkRecentAction as the action
;;; for a <toolitem> element.
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
;;; Whether the items should be displayed with a number.
;;;
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentAction
;;;
;;; struct GtkRecentAction;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRecentAction" gtk-recent-action
  (:superclass gtk-action
   :export t
   :interfaces ("GtkBuildable"
                "GtkRecentChooser")
   :type-initializer "gtk_recent_action_get_type")
  ((show-numbers
    gtk-recent-action-show-numbers
    "show-numbers" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_recent_action_new ()
;;;
;;; GtkAction * gtk_recent_action_new (const gchar *name,
;;;                                    const gchar *label,
;;;                                    const gchar *tooltip,
;;;                                    const gchar *stock_id);
;;;
;;; Creates a new GtkRecentAction object. To add the action to a GtkActionGroup
;;; and set the accelerator for the action, call
;;; gtk_action_group_add_action_with_accel().
;;;
;;; name :
;;;     a unique name for the action
;;;
;;; label :
;;;     the label displayed in menu items and on buttons, or NULL
;;;
;;; tooltip :
;;;     a tooltip for the action, or NULL
;;;
;;; stock_id :
;;;     the stock icon to display in widgets representing the action, or NULL
;;;
;;; Returns :
;;;     the newly created GtkRecentAction.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_action_new_for_manager ()
;;;
;;; GtkAction * gtk_recent_action_new_for_manager (const gchar *name,
;;;                                                const gchar *label,
;;;                                                const gchar *tooltip,
;;;                                                const gchar *stock_id,
;;;                                                GtkRecentManager *manager);
;;;
;;; Creates a new GtkRecentAction object. To add the action to a GtkActionGroup
;;; and set the accelerator for the action, call
;;; gtk_action_group_add_action_with_accel().
;;;
;;; name :
;;;     a unique name for the action
;;;
;;; label :
;;;     the label displayed in menu items and on buttons, or NULL
;;;
;;; tooltip :
;;;     a tooltip for the action, or NULL
;;;
;;; stock_id :
;;;     the stock icon to display in widgets representing the action, or NULL
;;;
;;; manager :
;;;     a GtkRecentManager, or NULL for using the default GtkRecentManager
;;;
;;; Returns :
;;;     the newly created GtkRecentAction
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_action_get_show_numbers ()
;;;
;;; gboolean gtk_recent_action_get_show_numbers (GtkRecentAction *action);
;;;
;;; Returns the value set by gtk_recent_chooser_menu_set_show_numbers().
;;;
;;; action :
;;;     a GtkRecentAction
;;;
;;; Returns :
;;;     TRUE if numbers should be shown.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_action_set_show_numbers ()
;;;
;;; void gtk_recent_action_set_show_numbers (GtkRecentAction *action,
;;;                                          gboolean show_numbers);
;;;
;;; Sets whether a number should be added to the items shown by the widgets
;;; representing action. The numbers are shown to provide a unique character for
;;; a mnemonic to be used inside the menu item's label. Only the first ten items
;;; get a number to avoid clashes.
;;;
;;; action :
;;;     a GtkRecentAction
;;;
;;; show_numbers :
;;;     TRUE if the shown items should be numbered
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.recent-action.lisp -------------------------------------
