;;; ----------------------------------------------------------------------------
;;; gtk.radio-menu.item.lisp
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
;;; GtkRadioMenuItem
;;;
;;;     A choice from multiple check menu items
;;;
;;; Types and Values
;;;
;;;     GtkRadioMenuItem
;;;
;;; Functions
;;;
;;;     gtk_radio_menu_item_new
;;;     gtk_radio_menu_item_new_with_label
;;;     gtk_radio_menu_item_new_with_mnemonic
;;;     gtk_radio_menu_item_new_from_widget
;;;     gtk_radio_menu_item_new_with_label_from_widget
;;;     gtk_radio_menu_item_new_with_mnemonic_from_widget
;;;     gtk_radio_menu_item_set_group
;;;     gtk_radio_menu_item_get_group
;;;     gtk_radio_menu_item_join_group
;;;
;;; Properties
;;;
;;;     GtkRadioMenuItem*  group    Write
;;;
;;; Signals
;;;
;;;     void  group-changed    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkMenuItem
;;;                         ╰── GtkCheckMenuItem
;;;                             ╰── GtkRadioMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRadioMenuItem implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRadioMenuItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRadioMenuItem" gtk-radio-menu-item
  (:superclass gtk-check-menu-item
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_radio_menu_item_get_type")
  ((group
    gtk-radio-menu-item-group
    "group" "GtkRadioMenuItem" nil t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-radio-menu-item 'type)
 "@version{2020-7-18}
  @begin{short}
    A radio menu item is a check menu item that belongs to a group. At each
    instant exactly one of the radio menu items from a group is selected.
  @end{short}

  The group list does not need to be freed, as each @sym{gtk-radio-menu-item}
  will remove itself and its list item when it is destroyed.

  The correct way to create a group of radio menu items is approximatively
  this:

  @b{Example:} How to create a group of radio menu items.
  @begin{pre}
(let (menu-item last-menu-item)
  ;; Add three menu items to a group
  (dolist (label '(\"First Menu Item\" \"Second Menu Item\" \"Third Menu Item\"))
    (setf menu-item (gtk-radio-menu-item-new-with-label nil label))
    (gtk-radio-menu-item-join-group menu-item last-menu-item)
    (setf last-menu-item menu-item)))
  @end{pre}
  @begin[Signal Details]{dictionary}
    @subheading{The \"group-changed\" signal}
      @begin{pre}
 lambda (radiomenuitem)    : Run First
      @end{pre}
      @begin[code]{table}
        @entry[radiomenuitem]{The @sym{gtk-radio-menu-item} widget which
          received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-radio-menu-item-group}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "group" 'gtk-radio-menu-item) 't)
 "The @code{group} property of type @class{gtk-radio-menu-item} (Write) @br{}
  The radio menu item whose group this widget belongs to.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-menu-item-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-menu-item-group 'function)
 "@version{2020-7-18}
  @begin{short}
    Accessor of the @slot[gtk-radio-menu-item]{group} slot of the
    @class{gtk-radio-menu-item} class.
  @end{short}
  @see-class{gtk-radio-menu-item}
  @see-function{gtk-radio-menu-item-get-group}
  @see-function{gtk-radio-menu-item-set-group}")

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_menu_item_new" gtk-radio-menu-item-new)
    (g-object gtk-radio-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-19}
  @argument[group]{the @class{gtk-radio-menu-item} group to which the radio menu
    item is to be attached}
  @return{A new @class{gtk-radio-menu-item}.}
  @begin{short}
    Creates a new radio menu item.
  @end{short}
  @see-class{gtk-radio-menu-item}"
  (group (g-slist (g-object gtk-radio-menu-item))))

(export 'gtk-radio-menu-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new_with_label ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_menu_item_new_with_label"
           gtk-radio-menu-item-new-with-label) (g-object gtk-radio-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-18}
  @argument[group]{the @class{gtk-radio-menu-item} group to which the radio menu
    item is to be attached}
  @argument[label]{a string with the text for the label}
  @return{A new @class{gtk-radio-menu-item}.}
  @begin{short}
    Creates a new radio menu item whose child is a simple @class{gtk-label}
    widget.
  @end{short}
  @see-class{gtk-radio-menu-item}"
  (group (g-slist (g-object gtk-radio-menu-item)))
  (label :string))

(export 'gtk-radio-menu-item-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_menu_item_new_with_mnemonic"
           gtk-radio-menu-item-new-with-mnemonic) (g-object gtk-radio-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-18}
  @argument[group]{the @class{gtk-radio-menu-item} group to which the radio menu
    item is to be attached}
  @argument[label]{a string with the text of the radio menu item, with an
    underscore in front of the mnemonic character}
  @return{A new @class{gtk-radio-menu-item} widget.}
  @begin{short}
    Creates a new radio menu item containing a label.
  @end{short}
  The label will be created using the function
  @fun{gtk-label-new-with-mnemonic}, so underscores in the label indicate the
  mnemonic for the menu item.
  @see-class{gtk-radio-menu-item}"
  (group (g-slist (g-object gtk-radio-menu-item)))
  (label :string))

(export 'gtk-radio-menu-item-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new_from_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_menu_item_new_from_widget"
           gtk-radio-menu-item-new-from-widget) (g-object gtk-radio-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-18}
  @argument[group-member]{a @class{gtk-radio-menu-item} widget to get the radio
    group from or @code{nil}}
  @return{A new @class{gtk-radio-menu-item}.}
  @begin{short}
    Creates a new radio menu item adding it to the same group as
    @arg{group-member}.
  @end{short}
  @see-class{gtk-radio-menu-item}
  @see-function{gtk-radio-menu-item-new-with-label-from-widget}"
  (group-member (g-object gtk-radio-menu-item)))

(export 'gtk-radio-menu-item-new-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new_with_label_from_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_menu_item_new_with_label_from_widget"
           gtk-radio-menu-item-new-with-label-from-widget)
    (g-object gtk-radio-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-18}
  @argument[group-member]{the @class{gtk-radio-menu-item} group to which the
    radio menu item is to be attached}
  @argument[label]{a string with the text for the label}
  @return{A new @class{gtk-radio-menu-item}.}
  @begin{short}
    Creates a new radio menu item whose child is a simple @class{gtk-label}
    widget.
  @end{short}
  The new radio menu item is added to the same group as @arg{group-member}.
  @see-class{gtk-radio-menu-item}"
  (group-member (g-object gtk-radio-menu-item))
  (label :string))

(export 'gtk-radio-menu-item-new-with-label-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new_with_mnemonic_from_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_menu_item_new_with_mnemonic_from_widget"
           gtk-radio-menu-item-new-with-mnemonic-from-widget)
    (g-object gtk-radio-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-18}
  @argument[group-member]{the @class{gtk-radio-menu-item} group to which the
    radio menu item is to be attached}
  @argument[label]{a string with the text of the radio menu item, with an
    underscore in front of the mnemonic character}
  @return{A new @class{gtk-radio-menu-item}.}
  @begin{short}
    Creates a new radio menu item containing a label.
  @end{short}
  The label will be created using the function
  @fun{gtk-label-new-with-mnemonic}, so underscores in label indicate the
  mnemonic for the menu item.

  The new radio menu item is added to the same group as @arg{group-member}.
  @see-class{gtk-radio-menu-item}"
  (group-member (g-object gtk-radio-menu-item))
  (label :string))

(export 'gtk-radio-menu-item-new-with-mnemonic-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_set_group ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-radio-menu-item-set-group))

(defun gtk-radio-menu-item-set-group (radio-menu-item group)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-18}
  @argument[radio-menu-item]{a @class{gtk-radio-menu-item} widget}
  @argument[group]{the @class{gtk-radio-menu-item} group}
  @begin{short}
    Sets the group of a radio menu item, or changes it.
  @end{short}
  @see-class{gtk-radio-menu-item}"
  (setf (gtk-radio-menu-item-group radio-menu-item) group))

(export 'gtk-radio-menu-item-set-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_get_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_menu_item_get_group" gtk-radio-menu-item-get-group)
    (g-slist (g-object gtk-radio-menu-item) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-18}
  @argument[menu-item]{a @class{gtk-radio-menu-item} widget}
  @return{The group of @arg{menu-item}.}
  @begin{short}
    Returns the group to which the radio menu item belongs, as a list of
    @class{gtk-radio-menu-item} widgets.
  @end{short}
  @see-class{gtk-radio-menu-item}"
  (menu-item (g-object gtk-radio-menu-item)))

(export 'gtk-radio-menu-item-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_join_group ()
;;; ----------------------------------------------------------------------------

#+gtk-3-18
(defcfun ("gtk_radio_menu_item_join_group" gtk-radio-menu-item-join-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-18}
  @argument[menu-item]{a @class{gtk-radio-menu-item} widget}
  @argument[group-source]{a @class{gtk-radio-menu-item} widget whose group we
    are joining, or @code{nil} to remove @arg{menu-item} from iths current
    group}
  @begin{short}
    Joins a radio menu item to the group of another radio menu item.
  @end{short}

  This function should be used by language bindings to avoid the memory
  manangement of the opaque @code{GSList} of the functions
  @code{gtk_radio_menu_item_get_group()} and
  @code{gtk_radio_menu_item_set_group()}.

  @begin[Example]{dictionary}
  A common way to set up a group of radio menu item instances is:
    @begin{pre}
(let (menu-item last-menu-item)
  ;; Add three menu items to a group
  (dolist (label '(\"First Menu Item\" \"Second Menu Item\" \"Third Menu Item\"))
    (setf menu-item (gtk-radio-menu-item-new-with-label nil label))
    (gtk-radio-menu-item-join-group menu-item last-menu-item)
    (setf last-menu-item menu-item)))
    @end{pre}
  @end{dictionary}
  @see-class{gtk-radio-menu-item}"
  (menu-item (g-object gtk-radio-menu-item))
  (group-source (g-object gtk-radio-menu-item)))

#+gtk-3-18
(export 'gtk-radio-menu-item-join-group)

;;; --- End of file gtk.radio-menu-item.lisp -----------------------------------
