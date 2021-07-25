;;; ----------------------------------------------------------------------------
;;; gtk.menu-shell.lisp
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
;;; GtkMenuShell
;;;
;;;     A base class for menu objects
;;;
;;; Types and Values
;;;
;;;     GtkMenuShell
;;;     GtkMenuDirectionType
;;;
;;; Functions
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
;;;     gtk_menu_shell_bind_model
;;;
;;; Properties
;;;
;;;     gboolean    take-focus          Read / Write
;;;
;;; Signals
;;;
;;;         void    activate-current    Action
;;;         void    cancel              Action
;;;         void    cycle-focus         Action
;;;         void    deactivate          Run First
;;;         void    insert              Run First
;;;         void    move-current        Action
;;;     gboolean    move-selected       Run Last
;;;         void    selection-done      Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkMenuShell
;;;                     ├── GtkMenuBar
;;;                     ╰── GtkMenu
;;;
;;; Implemented Interfaces
;;;
;;;     GtkMenuShell implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-direction-type atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-menu-direction-type atdoc:*external-symbols*)
 "@version{2021-7-17}
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
    @entry[:parent]{Movement to the parent menu shell.}
    @entry[:child]{Movement to the submenu, if any, associated with the item.}
    @entry[:next]{Movement to the next menu item.}
    @entry[:prev]{Movement to the previous menu item.}
  @end{table}
  @see-class{gtk-menu-shell}")

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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-menu-shell 'type)
 "@version{2021-7-17}
  @begin{short}
    The @sym{gtk-menu-shell} class is the abstract base class used to derive
    the @class{gtk-menu} and @class{gtk-menu-bar} subclasses.
  @end{short}

  A @sym{gtk-menu-shell} widget is a container of @class{gtk-menu-item} widgets
  arranged in a list which can be navigated, selected, and activated by the user
  to perform application functions. A @class{gtk-menu-item} widget can have a
  submenu associated with it, allowing for nested hierarchical menus.

  @subheading{Terminology}
  A menu item can be \"selected\", this means that it is displayed in the
  prelight state, and if it has a submenu, that submenu will be popped up.

  A menu is \"active\" when it is visible onscreen and the user is selecting
  from it. A menubar is not active until the user clicks on one of its
  menuitems. When a menu is active, passing the mouse over a submenu will pop
  it up.

  There is also a concept of the current menu and a current menu item. The
  current menu item is the selected menu item that is furthest down in the
  hierarchy. Every active menu shell does not necessarily contain a selected
  menu item, but if it does, then the parent menu shell must also contain a
  selected menu item. The current menu is the menu that contains the current
  menu item. It will always have a GTK grab and receive all key presses.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-current\" signal}
      @begin{pre}
 lambda (menushell force-hide)    :action
      @end{pre}
      An action signal that activates the current menu item within the menu
      shell.
      @begin[code]{table}
        @entry[menushell]{The @sym{gtk-menu-shell} widget which received the
          signal.}
        @entry[force-hide]{If @em{true}, hide the menu after activating the
          menu item.}
      @end{table}
    @subheading{The \"cancel\" signal}
      @begin{pre}
 lambda (menushell)    :action
      @end{pre}
      An action signal which cancels the selection within the menu shell.
      Causes the \"selection-done\" signal to be emitted.
      @begin[code]{table}
        @entry[menushell]{The @sym{gtk-menu-shell} widget which received the
          signal.}
      @end{table}
    @subheading{The \"cycle-focus\" signal}
      @begin{pre}
 lambda (menushell direction)    :action
      @end{pre}
      A keybinding signal which moves the focus in the given direction.
      @begin[code]{table}
        @entry[menushell]{The @sym{gtk-menu-shell} widget which received the
          signal.}
        @entry[direction]{The value of the @symbol{gtk-direction-type}
          enumeration to cycle in.}
      @end{table}
    @subheading{The \"deactivate\" signal}
      @begin{pre}
 lambda (menushell)    :run-first
      @end{pre}
      This signal is emitted when a menu shell is deactivated.
      @begin[code]{table}
        @entry[menushell]{The @sym{gtk-menu-shell} widget which received the
          signal.}
      @end{table}
    @subheading{The \"insert\" signal}
      @begin{pre}
 lambda (menushell child position)    :run-first
      @end{pre}
      The signal is emitted when a new menu item is added to a
      @sym{gtk-menu-shell} widget. A separate signal is used instead of the
      \"GtkContainer::add\" signal because of the need for an additional
      position parameter. The inverse of this signal is the
      \"GtkContainer::removed\" signal.
      @begin[code]{table}
        @entry[menushell]{The @sym{gtk-menu-shell} widget on which the signal
          is emitted.}
        @entry[child]{The @class{gtk-menu-item} widget that is being inserted.}
        @entry[position]{An integer with the position at which the insert
          occurs.}
      @end{table}
    @subheading{The \"move-current\" signal}
      @begin{pre}
 lambda (menushell direction)    :action
      @end{pre}
      An keybinding signal which moves the current menu item in the direction
      specified by @arg{direction}.
      @begin[code]{table}
        @entry[menushell]{The @sym{gtk-menu-shell} widget which received the
          signal.}
        @entry[direction]{The value of the @symbol{gtk-direction-type}
          enumeration to move.}
      @end{table}
    @subheading{The \"move-selected\" signal}
      @begin{pre}
 lambda (menushell distance)    :run-last
      @end{pre}
      The signal is emitted to move the selection to another item.
      @begin[code]{table}
        @entry[menushell]{The @sym{gtk-menu-shell} widget on which the signal
          is emitted.}
        @entry[distance]{+1 to move to the next item, -1 to move to the
          previous.}
        @entry[Returns]{@em{True} to stop the signal emission, @em{false} to
          continue.}
      @end{table}
    @subheading{The \"selection-done\" signal}
      @begin{pre}
 lambda (menushell)    :run-first
      @end{pre}
      This signal is emitted when a selection has been completed within a menu
      shell.
      @begin[code]{table}
        @entry[menushell]{The @sym{gtk-menu-shell} widget which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-menu-shell-take-focus}
  @see-class{gtk-menu}
  @see-class{gtk-menu-bar}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-menu-shell-take-focus ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "take-focus" 'gtk-menu-shell) 't)
 "The @code{take-focus} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether the menu and its submenus grab the keyboard focus. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-shell-take-focus atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-shell-take-focus 'function)
 "@version{2021-7-17}
  @syntax[]{(gtk-menu-shell-take-focus object) => take-focus}
  @syntax[]{(setf (gtk-menu-shell-take-focus object) take-focus)}
  @argument[object]{a @class{gtk-menu-shell} widget}
  @argument[take-focus]{@em{true} if the menu shell widget should take the
    keyboard focus on popup}
  @begin{short}
    Accessor of the @slot[gtk-menu-shell]{take-focus} slot of the
    @class{gtk-menu-shell} class.
  @end{short}

  If @arg{take-focus} is @em{true}, the default, the menu shell will take the
  keyboard focus so that it will receive all keyboard events which is needed
  to enable keyboard navigation in menus. Setting @arg{take-focus} to
  @em{false} is useful only for special applications like virtual keyboard
  implementations which should not take keyboard focus.

  The @arg{take-focus} state of a menu or menu bar is automatically propagated
  to submenus whenever a submenu is popped up, so you do not have to worry
  about recursively setting it for your entire menu hierarchy. Only when
  programmatically picking a submenu and popping it up manually, the
  @arg{take-focus} property of the submenu needs to be set explicitely.
  @begin[Note]{dictionary}
    The setting of @arg{take-focus} to @em{false} has side-effects. If the focus
    is in some other application, it keeps the focus and keyboard navigation in
    the menu does not work. Consequently, keyboard navigation on the menu will
    only work if the focus is on some toplevel owned by the onscreen keyboard.
    To avoid confusing the user, menus with @arg{take-focus} set to @em{false}
    should not display mnemonics or accelerators, since it cannot be guaranteed
    that they will work.
  @end{dictionary}
  @see-class{gtk-menu-shell}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_append ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_append" gtk-menu-shell-append) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @argument[child]{the @class{gtk-widget} child widget to add}
  @begin{short}
    Adds a new menu item to the end of the menu shell's item list.
  @end{short}
  @see-class{gtk-menu-shell}
  @see-class{gtk-widget}
  @see-function{gtk-menu-shell-prepend}
  @see-function{gtk-menu-shell-insert}"
  (menushell (g-object gtk-menu-shell))
  (child (g-object gtk-widget)))

(export 'gtk-menu-shell-append)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_prepend ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_prepend" gtk-menu-shell-prepend) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @argument[child]{the @class{gtk-widget} child widget to add}
  @begin{short}
    Adds a new menu item to the beginning of the menu shell's item list.
  @end{short}
  @see-class{gtk-menu-shell}
  @see-class{gtk-widget}
  @see-function{gtk-menu-shell-append}
  @see-function{gtk-menu-shell-insert}"
  (menushell (g-object gtk-menu-shell))
  (child (g-object gtk-widget)))

(export 'gtk-menu-shell-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_insert" gtk-menu-shell-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @argument[child]{the @class{gtk-widget} child widget to add}
  @argument[position]{an integer with the position in the item list where child
    is added, positions are numbered from 0 to n-1}
  @begin{short}
    Adds a new menu item to the menu shell's item list at the position
    indicated by @arg{position}.
  @end{short}
  @see-class{gtk-menu-shell}
  @see-class{gtk-widget}
  @see-function{gtk-menu-shell-append}
  @see-function{gtk-menu-shell-prepend}"
  (menushell (g-object gtk-menu-shell))
  (child (g-object gtk-widget))
  (position :int))

(export 'gtk-menu-shell-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_deactivate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_deactivate" gtk-menu-shell-deactivate) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @begin{short}
    Deactivates the menu shell.
  @end{short}
  Typically this results in the menu shell being erased from the screen.
  @see-class{gtk-menu-shell}"
  (menushell (g-object gtk-menu-shell)))

(export 'gtk-menu-shell-deactivate)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_select_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_select_item" gtk-menu-shell-select-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @argument[menuitem]{the @class{gtk-menu-item} widget to select}
  @begin{short}
    Selects the menu item from the menu shell.
  @end{short}
  @see-class{gtk-menu-shell}
  @see-class{gtk-menu-item}"
  (menushell (g-object gtk-menu-shell))
  (menuitem (g-object gtk-menu-item)))

(export 'gtk-menu-shell-select-item)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_select_first ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_select_first" gtk-menu-shell-select-first) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @argument[sensitive]{if @em{true}, search for the first selectable menu item,
    otherwise select nothing if the first item is not sensitive, this should be
    @em{false} if the menu is being popped up initially}
  @begin{short}
    Select the first visible or selectable child widget of the menu shell.
  @end{short}
  Do not select tearoff items unless the only item is a tearoff item.
  @see-class{gtk-menu-shell}"
  (menushell (g-object gtk-menu-shell))
  (sensitive :boolean))

(export 'gtk-menu-shell-select-first)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_deselect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_deselect" gtk-menu-shell-deselect) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @begin{short}
    Deselects the currently selected item from the menu shell, if any.
  @end{short}
  @see-class{gtk-menu-shell}"
  (menushell (g-object gtk-menu-shell)))

(export 'gtk-menu-shell-deselect)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_activate_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_activate_item" gtk-menu-shell-activate-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @argument[menuitem]{the @class{gtk-menu-item} widget to activate}
  @argument[deactivate]{if @em{true}, force the deactivation of the menu shell
    after the menu item is activated}
  @begin{short}
    Activates the menu item within the menu shell.
  @end{short}
  @see-class{gtk-menu-shell}
  @see-class{gtk-menu-item}"
  (menushell (g-object gtk-menu-shell))
  (menuitem (g-object gtk-menu-item))
  (deactivate :boolean))

(export 'gtk-menu-shell-activate-item)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_cancel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_cancel" gtk-menu-shell-cancel) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @short{Cancels the selection within the menu shell.}
  @see-class{gtk-menu-shell}"
  (menushell (g-object gtk-menu-shell)))

(export 'gtk-menu-shell-cancel)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_get_selected_item () -> gtk-menu-shell-selected-item
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_get_selected_item" gtk-menu-shell-selected-item)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @return{The currently selected @class{gtk-menu-item} widget.}
  @begin{short}
    Gets the currently selected item.
  @end{short}
  @see-class{gtk-menu-shell}
  @see-class{gtk-menu-item}"
  (menushell (g-object gtk-menu-shell)))

(export 'gtk-menu-shell-selected-item)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_get_parent_shell () -> gtk-menu-shell-parent-shell
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_get_parent_shell" gtk-menu-shell-parent-shell)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @return{The parent @class{gtk-menu-shell} widget.}
  @begin{short}
    Gets the parent menu shell.
  @end{short}
  The parent menu shell of a submenu is the @class{gtk-menu} or
  @class{gtk-menu-bar} widget from which it was opened up.
  @see-class{gtk-menu-shell}
  @see-class{gtk-menu}
  @see-class{gtk-menu-bar}"
  (menushell (g-object gtk-menu-shell)))

(export 'gtk-menu-shell-parent-shell)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_shell_bind_model ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_shell_bind_model" gtk-menu-shell-bind-model) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-17}
  @argument[menushell]{a @class{gtk-menu-shell} widget}
  @argument[model]{the @class{g-menu-model} object to bind to or @code{nil}
    to remove binding}
  @argument[namespace]{a string with the namespace for actions in the menu
    model}
  @argument[separators]{@em{true} if toplevel items in shell should have
    separators between them}
  @begin{short}
    Establishes a binding between a menu shell and a menu model.
  @end{short}

  The contents of the menu shell are removed and then refilled with menu items
  according to the menu model. When the menu model changes, the menu shell is
  updated. Calling this function twice on the menu shell with different menu
  model will cause the first binding to be replaced with a binding to the new
  menu model. If the menu model is @code{nil} then any previous binding is
  undone and all children are removed.

  The argument @arg{separators} determines if toplevel items, e.g. sections,
  have separators inserted between them. This is typically desired for menus
  but does not make sense for menubars.

  If the argument @arg{namespace} is non-@code{nil} then the effect is as if all
  actions mentioned in the model have their names prefixed with the namespace,
  plus a dot. For example, if the action \"quit\" is mentioned and
  @arg{namespace} is \"app\" then the effective action name is \"app.quit\".

  For most cases you are probably better off using the functions
  @fun{gtk-menu-new-from-model} or @fun{gtk-menu-bar-new-from-model} or just
  directly passing the @class{g-menu-model} object to the functions
  @fun{gtk-application-app-menu} or @fun{gtk-application-menubar}.
  @see-class{gtk-menu-shell}
  @see-class{g-menu-model}
  @see-function{gtk-menu-new-from-model}
  @see-function{gtk-menu-bar-new-from-model}
  @see-function{gtk-application-app-menu}
  @see-function{gtk-application-menubar}"
  (menushell (g-object gtk-menu-shell))
  (model (g-object g-menu-model))
  (namespace :string)
  (separators :boolean))

(export 'gtk-menu-shell-bind-model)

;;; --- End of file gtk.menu-shell.lisp ----------------------------------------
