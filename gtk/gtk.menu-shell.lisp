;;; ----------------------------------------------------------------------------
;;; gtk.menu-shell.lisp
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
;;; GtkMenuShell
;;; 
;;; A base class for menu objects
;;; 
;;; Synopsis
;;; 
;;;     GtkMenuShell
;;;     
;;;     gtk_menu_shell_append
;;;     gtk_menu_shell_prepend
;;;     gtk_menu_shell_insert
;;;     gtk_menu_shell_deactivate
;;;     gtk_menu_shell_select_item
;;;     gtk_menu_shell_select_first
;;;     gtk_menu_shell_deselect
;;;     gtk_menu_shell_activate_item
;;;     gtk_menu_shell_cancel
;;;     gtk_menu_shell_set_take_focus
;;;     gtk_menu_shell_get_take_focus
;;;     gtk_menu_shell_get_selected_item
;;;     gtk_menu_shell_get_parent_shell
;;;
;;;     GtkMenuDirectionType
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkMenuShell
;;;                            +----GtkMenuBar
;;;                            +----GtkMenu
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkMenuShell implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "take-focus"               gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "activate-current"                               : Action
;;;   "cancel"                                         : Action
;;;   "cycle-focus"                                    : Action
;;;   "deactivate"                                     : Run First
;;;   "insert"                                         : Run First
;;;   "move-current"                                   : Action
;;;   "move-selected"                                  : Run Last
;;;   "selection-done"                                 : Run First
;;; 
;;; Description
;;; 
;;; A GtkMenuShell is the abstract base class used to derive the GtkMenu and
;;; GtkMenuBar subclasses.
;;; 
;;; A GtkMenuShell is a container of GtkMenuItem objects arranged in a list
;;; which can be navigated, selected, and activated by the user to perform
;;; application functions. A GtkMenuItem can have a submenu associated with it,
;;; allowing for nested hierarchical menus.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "take-focus" property
;;; 
;;;   "take-focus"               gboolean              : Read / Write
;;; 
;;; A boolean that determines whether the menu and its submenus grab the
;;; keyboard focus. See gtk_menu_shell_set_take_focus() and
;;; gtk_menu_shell_get_take_focus().
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate-current" signal
;;; 
;;; void user_function (GtkMenuShell *menushell,
;;;                     gboolean      force_hide,
;;;                     gpointer      user_data)       : Action
;;; 
;;; An action signal that activates the current menu item within the menu shell.
;;; 
;;; menushell :
;;;     the object which received the signal
;;; 
;;; force_hide :
;;;     if TRUE, hide the menu after activating the menu item
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "cancel" signal
;;; 
;;; void user_function (GtkMenuShell *menushell,
;;;                     gpointer      user_data)      : Action
;;; 
;;; An action signal which cancels the selection within the menu shell. Causes
;;; the "selection-done" signal to be emitted.
;;; 
;;; menushell :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "cycle-focus" signal
;;; 
;;; void user_function (GtkMenuShell    *menushell,
;;;                     GtkDirectionType direction,
;;;                     gpointer         user_data)      : Action
;;; 
;;; A keybinding signal which moves the focus in the given direction.
;;; 
;;; menushell :
;;;     the object which received the signal
;;; 
;;; direction :
;;;     the direction to cycle in
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "deactivate" signal
;;; 
;;; void user_function (GtkMenuShell *menushell,
;;;                     gpointer      user_data)      : Run First
;;; 
;;; This signal is emitted when a menu shell is deactivated.
;;; 
;;; menushell :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "insert" signal
;;; 
;;; void user_function (GtkMenuShell *menu_shell,
;;;                     GtkWidget    *child,
;;;                     gint          position,
;;;                     gpointer      user_data)       : Run First
;;; 
;;; The ::insert signal is emitted when a new GtkMenuItem is added to a
;;; GtkMenuShell. A separate signal is used instead of GtkContainer::add because
;;; of the need for an additional position parameter.
;;; 
;;; The inverse of this signal is the GtkContainer::removed signal.
;;; 
;;; menu_shell :
;;;     the object on which the signal is emitted
;;; 
;;; child :
;;;     the GtkMenuItem that is being inserted
;;; 
;;; position :
;;;     the position at which the insert occurs
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 3.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "move-current" signal
;;; 
;;; void user_function (GtkMenuShell        *menushell,
;;;                     GtkMenuDirectionType direction,
;;;                     gpointer             user_data)      : Action
;;; 
;;; An keybinding signal which moves the current menu item in the direction
;;; specified by direction.
;;; 
;;; menushell :
;;;     the object which received the signal
;;; 
;;; direction :
;;;     the direction to move
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "move-selected" signal
;;; 
;;; gboolean user_function (GtkMenuShell *menu_shell,
;;;                         gint          distance,
;;;                         gpointer      user_data)       : Run Last
;;; 
;;; The ::move-selected signal is emitted to move the selection to another item.
;;; 
;;; menu_shell :
;;;     the object on which the signal is emitted
;;; 
;;; distance :
;;;     +1 to move to the next item, -1 to move to the previous
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop the signal emission, FALSE to continue
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-done" signal
;;; 
;;; void user_function (GtkMenuShell *menushell,
;;;                     gpointer      user_data)      : Run First
;;; 
;;; This signal is emitted when a selection has been completed within a menu
;;; shell.
;;; 
;;; menushell :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuShell
;;; 
;;; struct GtkMenuShell;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuShell" gtk-menu-shell
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_menu_shell_get_type")
  ((take-focus
    gtk-menu-shell-take-focus
    "take-focus" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_append ()
;;; 
;;; void gtk_menu_shell_append (GtkMenuShell *menu_shell, GtkWidget *child);
;;; 
;;; Adds a new GtkMenuItem to the end of the menu shell's item list.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; 
;;; child :
;;;     The GtkMenuItem to add
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_append" gtk-menu-shell-append) :void
  (menu-shell (g-object gtk-menu-shell))
  (child (g-object gtk-widget)))

(export 'gtk-menu-shell-append)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_prepend ()
;;; 
;;; void gtk_menu_shell_prepend (GtkMenuShell *menu_shell, GtkWidget *child);
;;; 
;;; Adds a new GtkMenuItem to the beginning of the menu shell's item list.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; 
;;; child :
;;;     The GtkMenuItem to add
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_prepend" gtk-menu-shell-prepend) :void
  (menu-shell g-object)
  (child g-object))

(export 'gtk-menu-shell-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_insert ()
;;; 
;;; void gtk_menu_shell_insert (GtkMenuShell *menu_shell,
;;;                             GtkWidget *child,
;;;                             gint position);
;;; 
;;; Adds a new GtkMenuItem to the menu shell's item list at the position
;;; indicated by position.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; 
;;; child :
;;;     The GtkMenuItem to add
;;; 
;;; position :
;;;     The position in the item list where child is added. Positions are
;;;     numbered from 0 to n-1
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_insert" gtk-menu-shell-insert) :void
  (menu-shell g-object)
  (child g-object)
  (position :int))

(export 'gtk-menu-shell-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_deactivate ()
;;; 
;;; void gtk_menu_shell_deactivate (GtkMenuShell *menu_shell);
;;; 
;;; Deactivates the menu shell.
;;; 
;;; Typically this results in the menu shell being erased from the screen.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_deactivate" gtk-menu-shell-deactivate) :void
  (menu-shell g-object))

(export 'gtk-menu-shell-deactivate)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_select_item ()
;;; 
;;; void gtk_menu_shell_select_item (GtkMenuShell *menu_shell,
;;;                                  GtkWidget *menu_item);
;;; 
;;; Selects the menu item from the menu shell.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; 
;;; menu_item :
;;;     The GtkMenuItem to select
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_select_item" gtk-menu-shell-select-item) :void
  (menu-shell g-object)
  (menu-item g-object))

(export 'gtk-menu-shell-select-item)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_select_first ()
;;; 
;;; void gtk_menu_shell_select_first (GtkMenuShell *menu_shell,
;;;                                   gboolean search_sensitive);
;;; 
;;; Select the first visible or selectable child of the menu shell; don't
;;; select tearoff items unless the only item is a tearoff item.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; 
;;; search_sensitive :
;;;     If TRUE, search for the first selectable menu item, otherwise select
;;;     nothing if the first item isn't sensitive. This should be FALSE if the
;;;     menu is being popped up initially.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_select_first" %gtk-menu-shell-select-first) :void
  (menu-shell g-object)
  (search-sensitive :boolean))

(defun gtk-menu-shell-select-first (menu-shell &optional (search-sensitive t))
  (%gtk-menu-shell-select-first menu-shell search-sensitive))

(export 'gtk-menu-shell-select-first)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_deselect ()
;;; 
;;; void gtk_menu_shell_deselect (GtkMenuShell *menu_shell);
;;; 
;;; Deselects the currently selected item from the menu shell, if any.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_deselect" gtk-menu-shell-deselect) :void
  (menu-shell g-object))

(export 'gtk-menu-shell-deselect)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_activate_item ()
;;; 
;;; void gtk_menu_shell_activate_item (GtkMenuShell *menu_shell,
;;;                                    GtkWidget *menu_item,
;;;                                    gboolean force_deactivate);
;;; 
;;; Activates the menu item within the menu shell.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; 
;;; menu_item :
;;;     the GtkMenuItem to activate
;;; 
;;; force_deactivate :
;;;     if TRUE, force the deactivation of the menu shell after the menu item
;;;     is activated
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_activate_item" %gtk-menu-shell-activate-item) :void
  (menu-shell g-object)
  (menu-item g-object)
  (force-deactivate :boolean))

(defun gtk-menu-shell-activate-item (menu-shell menu-item
                                                &optional force-deactivate)
  (%gtk-menu-shell-activate-item menu-shell menu-item force-deactivate))

(export 'gtk-menu-shell-activate-item)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_cancel ()
;;; 
;;; void gtk_menu_shell_cancel (GtkMenuShell *menu_shell);
;;; 
;;; Cancels the selection within the menu shell.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_cancel" gtk-menu-shell-cancel) :void
  (menu-shell g-object))

(export 'gtk-menu-shell-cancel)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_set_take_focus ()
;;; 
;;; void gtk_menu_shell_set_take_focus (GtkMenuShell *menu_shell,
;;;                                     gboolean take_focus);
;;; 
;;; If take_focus is TRUE (the default) the menu shell will take the keyboard
;;; focus so that it will receive all keyboard events which is needed to enable
;;; keyboard navigation in menus.
;;; 
;;; Setting take_focus to FALSE is useful only for special applications like
;;; virtual keyboard implementations which should not take keyboard focus.
;;; 
;;; The take_focus state of a menu or menu bar is automatically propagated to
;;; submenus whenever a submenu is popped up, so you don't have to worry about
;;; recursively setting it for your entire menu hierarchy. Only when
;;; programmatically picking a submenu and popping it up manually, the
;;; take_focus property of the submenu needs to be set explicitely.
;;; 
;;; Note that setting it to FALSE has side-effects:
;;; 
;;; If the focus is in some other app, it keeps the focus and keynav in the
;;; menu doesn't work. Consequently, keynav on the menu will only work if the
;;; focus is on some toplevel owned by the onscreen keyboard.
;;; 
;;; To avoid confusing the user, menus with take_focus set to FALSE should not
;;; display mnemonics or accelerators, since it cannot be guaranteed that they
;;; will work.
;;; 
;;; See also gdk_keyboard_grab()
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; 
;;; take_focus :
;;;     TRUE if the menu shell should take the keyboard focus on popup
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-shell-set-take-focus))

(defun gtk-menu-shell-set-take-focus (menu-shell take-focus)
  (setf (gtk-menu-shell-take-focus menu-shell) take-focus))

(export 'gtk-menu-shell-set-take-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_get_take_focus ()
;;; 
;;; gboolean gtk_menu_shell_get_take_focus (GtkMenuShell *menu_shell);
;;; 
;;; Returns TRUE if the menu shell will take the keyboard focus on popup.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; 
;;; Returns :
;;;     TRUE if the menu shell will take the keyboard focus on popup.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-shell-get-take-focus))

(defun gtk-menu-shell-get-take-focus (menu-shell)
  (gtk-menu-shell-take-focus menu-shell))

(export 'gtk-menu-shell-get-take-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_get_selected_item ()
;;; 
;;; GtkWidget * gtk_menu_shell_get_selected_item (GtkMenuShell *menu_shell);
;;; 
;;; Gets the currently selected item.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; 
;;; Returns :
;;;     the currently selected item. [transfer none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_get_parent_shell ()
;;; 
;;; GtkWidget * gtk_menu_shell_get_parent_shell (GtkMenuShell *menu_shell);
;;; 
;;; Gets the parent menu shell.
;;; 
;;; The parent menu shell of a submenu is the GtkMenu or GtkMenuBar from which
;;; it was opened up.
;;; 
;;; menu_shell :
;;;     a GtkMenuShell
;;; 
;;; Returns :
;;;     the parent GtkMenuShell
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkMenuDirectionType
;;; 
;;; typedef enum {
;;;   GTK_MENU_DIR_PARENT,
;;;   GTK_MENU_DIR_CHILD,
;;;   GTK_MENU_DIR_NEXT,
;;;   GTK_MENU_DIR_PREV
;;; } GtkMenuDirectionType;
;;; 
;;; An enumeration representing directional movements within a menu.
;;; 
;;; GTK_MENU_DIR_PARENT
;;;     To the parent menu shell
;;; 
;;; GTK_MENU_DIR_CHILD
;;;     To the submenu, if any, associated with the item
;;; 
;;; GTK_MENU_DIR_NEXT
;;;     To the next menu item
;;; 
;;; GTK_MENU_DIR_PREV
;;;     To the previous menu item
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkMenuDirectionType" gtk-menu-direction-type
  (:export t
   :type-initializer "gtk_menu_direction_type_get_type")
  (:parent 0)
  (:child 1)
  (:next 2)
  (:prev 3))

;;; --- End of file gtk.menu-shell.lisp ----------------------------------------
