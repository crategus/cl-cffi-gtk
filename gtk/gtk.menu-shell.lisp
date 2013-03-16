;;; ----------------------------------------------------------------------------
;;; gtk.menu-shell.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuShell
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuShell" gtk-menu-shell
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_menu_shell_get_type")
  ((take-focus
    gtk-menu-shell-take-focus
    "take-focus" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-menu-shell 'type)
 "@version{2013-3-16}
  @begin{short}
    A GtkMenuShell is the abstract base class used to derive the GtkMenu and
    GtkMenuBar subclasses.
  @end{short}

  A GtkMenuShell is a container of GtkMenuItem objects arranged in a list
  which can be navigated, selected, and activated by the user to perform
  application functions. A GtkMenuItem can have a submenu associated with it,
  allowing for nested hierarchical menus.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-current\" signal}
      An action signal that activates the current menu item within the menu
      shell.
      @begin{pre}
 lambda (menushell force-hide)   : Action
      @end{pre}
      @begin[code]{table}
        @entry[menushell]{the object which received the signal}
        @entry[force-hide]{if TRUE, hide the menu after activating the menu
          item}
      @end{table}
    @subheading{The \"cancel\" signal}
      An action signal which cancels the selection within the menu shell. Causes
      the \"selection-done\" signal to be emitted.
      @begin{pre}
 lambda (menushell)   : Action
      @end{pre}
      @begin[code]{table}
        @entry[menushell]{the object which received the signal}
      @end{table}
    @subheading{The \"cycle-focus\" signal}
      A keybinding signal which moves the focus in the given direction.
      @begin{pre}
 lambda (menushell direction)   : Action
      @end{pre}
      @begin[code]{table}
        @entry[menushell]{the object which received the signal}
        @entry[direction]{the direction to cycle in}
      @end{table}
    @subheading{The \"deactivate\" signal}
      This signal is emitted when a menu shell is deactivated.
      @begin{pre}
 lambda (menushell)
      @end{pre}
      @begin[code]{table}
        @entry[menushell]{the object which received the signal}
      @end{table}
    @subheading{The \"insert\" signal}
      The \"insert\" signal is emitted when a new GtkMenuItem is added to a
      GtkMenuShell. A separate signal is used instead of
      @code{GtkContainer::add} because of the need for an additional position
      parameter. @br{}
      The inverse of this signal is the @code{GtkContainer::removed} signal.
      @begin{pre}
 lambda (menushell child position)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[menu_shell]{the object on which the signal is emitted}
        @entry[child]{the GtkMenuItem that is being inserted}
        @entry[position]{the position at which the insert occurs}
      @end{table}
      Since 3.2

    @subheading{The \"move-current\" signal}
      An keybinding signal which moves the current menu item in the direction
      specified by direction.
      @begin{pre}
 lambda (menushell direction)   : Action
      @end{pre}
      @begin[code]{table}
        @entry[menushell]{the object which received the signal}
        @entry[direction]{the direction to move}
      @end{table}
    @subheading{The \"move-selected\" signal}
      The \"move-selected\" signal is emitted to move the selection to another
      item.
      @begin{pre}
 lambda (menushell distance)   : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[menushell]{the object on which the signal is emitted}
        @entry[distance]{+1 to move to the next item, -1 to move to the
          previous}
        @entry[Returns]{TRUE to stop the signal emission, FALSE to continue}
      @end{table}
      Since 2.12

    @subheading{The \"selection-done\" signal}
      This signal is emitted when a selection has been completed within a menu
      shell.
      @begin{pre}
 lambda (menushell)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[menushell]{the object which received the signal}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-menu-shell-take-focus}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "take-focus" 'gtk-menu-shell) 't)
 "The @code{\"take-focus\"} property of type @code{:boolean} (Read / Write)@br{}
  A boolean that determines whether the menu and its submenus grab the
  keyboard focus. See gtk_menu_shell_set_take_focus() and
  gtk_menu_shell_get_take_focus(). @br{}
  Default value: @em{true}@br{}
  Since 2.8")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-shell-take-focus atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-shell-take-focus 'function)
 "@version{2013-3-16}
  Accessor of the slot @code{\"take-focus\"} of the @class{gtk-menu-shell}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_append ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_append" gtk-menu-shell-append) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu-shell]{a GtkMenuShell}
  @argument[child]{The GtkMenuItem to add}
  Adds a new GtkMenuItem to the end of the menu shell's item list."
  (menu-shell (g-object gtk-menu-shell))
  (child (g-object gtk-widget)))

(export 'gtk-menu-shell-append)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_prepend ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_prepend" gtk-menu-shell-prepend) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu-shell]{a GtkMenuShell}
  @argument[child]{The GtkMenuItem to add}
  Adds a new GtkMenuItem to the beginning of the menu shell's item list."
  (menu-shell g-object)
  (child g-object))

(export 'gtk-menu-shell-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_insert" gtk-menu-shell-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu-shell]{a GtkMenuShell}
  @argument[child]{The GtkMenuItem to add}
  @argument[position]{The position in the item list where child is added.
    Positions are numbered from 0 to n-1}
  Adds a new GtkMenuItem to the menu shell's item list at the position
  indicated by position."
  (menu-shell g-object)
  (child g-object)
  (position :int))

(export 'gtk-menu-shell-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_deactivate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_deactivate" gtk-menu-shell-deactivate) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu-shell]{a GtkMenuShell}
  @short{Deactivates the menu shell.}

  Typically this results in the menu shell being erased from the screen."
  (menu-shell g-object))

(export 'gtk-menu-shell-deactivate)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_select_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_select_item" gtk-menu-shell-select-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu-shell]{a GtkMenuShell}
  @argument[menu-item]{The GtkMenuItem to select}
  Selects the menu item from the menu shell."
  (menu-shell g-object)
  (menu-item g-object))

(export 'gtk-menu-shell-select-item)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_select_first ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_select_first" %gtk-menu-shell-select-first) :void
  (menu-shell g-object)
  (search-sensitive :boolean))

(defun gtk-menu-shell-select-first (menu-shell &optional (search-sensitive t))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu-shell]{a GtkMenuShell}
  @argument[search-sensitive]{if TRUE, search for the first selectable menu
    item, otherwise select nothing if the first item isn't sensitive. This
    should be FALSE if the menu is being popped up initially.}
  @begin{short}
    Select the first visible or selectable child of the menu shell; don't select
    tearoff items unless the only item is a tearoff item.
  @end{short}

  Since 2.2"
  (%gtk-menu-shell-select-first menu-shell search-sensitive))

(export 'gtk-menu-shell-select-first)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_deselect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_deselect" gtk-menu-shell-deselect) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu_shell]{a GtkMenuShell}
  Deselects the currently selected item from the menu shell, if any."
  (menu-shell g-object))

(export 'gtk-menu-shell-deselect)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_activate_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_activate_item" %gtk-menu-shell-activate-item) :void
  (menu-shell g-object)
  (menu-item g-object)
  (force-deactivate :boolean))

(defun gtk-menu-shell-activate-item (menu-shell menu-item
                                                &optional force-deactivate)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu_shell]{a GtkMenuShell}
  @argument[menu_item]{the GtkMenuItem to activate}
  @argument[force_deactivate]{if TRUE, force the deactivation of the menu shell
    after the menu item is activated}
  Activates the menu item within the menu shell."
  (%gtk-menu-shell-activate-item menu-shell menu-item force-deactivate))

(export 'gtk-menu-shell-activate-item)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_cancel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_cancel" gtk-menu-shell-cancel) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu_shell]{a GtkMenuShell}
  @short{Cancels the selection within the menu shell.}

  Since 2.4"
  (menu-shell g-object))

(export 'gtk-menu-shell-cancel)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_set_take_focus ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-shell-set-take-focus))

(defun gtk-menu-shell-set-take-focus (menu-shell take-focus)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu_shell]{a GtkMenuShell}
  @argument[take_focus]{TRUE if the menu shell should take the keyboard focus
    on popup}
  @begin{short}
    If take_focus is TRUE (the default) the menu shell will take the keyboard
    focus so that it will receive all keyboard events which is needed to enable
    keyboard navigation in menus.
  @end{short}

  Setting take_focus to FALSE is useful only for special applications like
  virtual keyboard implementations which should not take keyboard focus.

  The take_focus state of a menu or menu bar is automatically propagated to
  submenus whenever a submenu is popped up, so you don't have to worry about
  recursively setting it for your entire menu hierarchy. Only when
  programmatically picking a submenu and popping it up manually, the
  take_focus property of the submenu needs to be set explicitely.

  Note that setting it to FALSE has side-effects:

  If the focus is in some other app, it keeps the focus and keynav in the menu
  doesn't work. Consequently, keynav on the menu will only work if the focus
  is on some toplevel owned by the onscreen keyboard.

  To avoid confusing the user, menus with take_focus set to FALSE should not
  display mnemonics or accelerators, since it cannot be guaranteed that they
  will work.
  
  See also gdk_keyboard_grab()

  Since 2.8"
  (setf (gtk-menu-shell-take-focus menu-shell) take-focus))

(export 'gtk-menu-shell-set-take-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_get_take_focus ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-shell-get-take-focus))

(defun gtk-menu-shell-get-take-focus (menu-shell)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu_shell]{a GtkMenuShell}
  @return{TRUE if the menu shell will take the keyboard focus on popup.}
  @begin{short}
    Returns TRUE if the menu shell will take the keyboard focus on popup.
  @end{short}

  Since 2.8"
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
;;;     the currently selected item
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
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkMenuDirectionType" gtk-menu-direction-type
  (:export t
   :type-initializer "gtk_menu_direction_type_get_type")
  (:parent 0)
  (:child 1)
  (:next 2)
  (:prev 3))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-direction-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-menu-direction-type atdoc:*external-symbols*)
 "@version{2013-3-16}
  @begin{short}
    An enumeration representing directional movements within a menu.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkMenuDirectionType\" gtk-menu-direction-type
  (:export t
   :type-initializer \"gtk_menu_direction_type_get_type\")
  (:parent 0)
  (:child 1)
  (:next 2)
  (:prev 3))
  @end{pre}
  @begin[code]{table}
    @entry[:paren]{To the parent menu shell}
    @entry[:child]{To the submenu, if any, associated with the item}
    @entry[:next]{To the next menu item}
    @entry[:prev]{To the previous menu item}
  @end{table}")

;;; --- End of file gtk.menu-shell.lisp ----------------------------------------
