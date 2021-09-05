;;; ----------------------------------------------------------------------------
;;; gio.menu.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2021 Dieter Kaiser
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
;;; GMenu
;;;
;;;     An implementation of GMenuModel
;;;
;;; Types and Values
;;;
;;;     GMenu
;;;     GMenuItem
;;;
;;; Functions
;;;
;;;     g_menu_new
;;;     g_menu_freeze
;;;     g_menu_insert
;;;     g_menu_prepend
;;;     g_menu_append
;;;     g_menu_insert_item
;;;     g_menu_append_item
;;;     g_menu_prepend_item
;;;     g_menu_insert_section
;;;     g_menu_prepend_section
;;;     g_menu_append_section
;;;     g_menu_append_submenu
;;;     g_menu_insert_submenu
;;;     g_menu_prepend_submenu
;;;     g_menu_remove
;;;     g_menu_remove_all
;;;
;;;     g_menu_item_new
;;;     g_menu_item_new_section
;;;     g_menu_item_new_submenu
;;;     g_menu_item_new_from_model
;;;     g_menu_item_set_label
;;;     g_menu_item_set_icon
;;;     g_menu_item_set_action_and_target_value
;;;     g_menu_item_set_action_and_target
;;;     g_menu_item_set_detailed_action
;;;     g_menu_item_set_section
;;;     g_menu_item_set_submenu
;;;
;;;     g_menu_item_get_attribute_value
;;;     g_menu_item_get_attribute
;;;     g_menu_item_get_link
;;;     g_menu_item_set_attribute_value
;;;     g_menu_item_set_attribute
;;;     g_menu_item_set_link
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GMenuItem
;;;     ╰── GMenuModel
;;;         ╰── GMenu
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GMenu
;;; ----------------------------------------------------------------------------

(define-g-object-class "GMenu" g-menu
  (:superclass g-menu-model
   :export t
   :interfaces nil
   :type-initializer "g_menu_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-menu 'type)
 "@version{2021-8-16}
  @begin{short}
    The @sym{g-menu} class is an implementation of the abstract
    @class{g-menu-model} class.
  @end{short}
  You populate a @sym{g-menu} object by adding @class{g-menu-item} objects to
  it.

  There are some convenience functions to allow you to directly add items,
  avoiding a @class{g-menu-item} object, for the common cases. To add a regular
  item, use the function @fun{g-menu-insert}. To add a section, use the function
  @fun{g-menu-insert-section}. To add a submenu, use the function
  @fun{g-menu-insert-submenu}.
  @see-class{g-menu-model}
  @see-class{g-menu-item}")

;;; ----------------------------------------------------------------------------
;;; g_menu_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-menu-new))

(defun g-menu-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @return{A new @class{g-menu} object.}
  @short{Creates a new @class{g-menu} object.}
  The new menu has no items.
  @see-class{g-menu}"
  (make-instance 'g-menu))

(export 'g-menu-new)

;;; ----------------------------------------------------------------------------
;;; g_menu_freeze ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_freeze" g-menu-freeze) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @begin{short}
    Marks menu as frozen.
  @end{short}
  After the menu is frozen, it is an error to attempt to make any changes to it.
  In effect this means that the @class{g-menu} API must no longer be used.

  This function causes the function @fun{g-menu-model-is-mutable} to begin
  returning @em{false}, which has some positive performance implications.
  @see-class{g-menu}
  @see-function{g-menu-model-is-mutable}"
  (menu (g-object g-menu)))

(export 'g-menu-freeze)

;;; ----------------------------------------------------------------------------
;;; g_menu_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_insert" %g-menu-insert) :void
  (menu (g-object g-menu))
  (position :int)
  (label :string)
  (action :string))

(defun g-menu-insert (menu position label action)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[position]{an integer with the position at which to insert the item}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[action]{the detailed action string, or @code{nil}}
  @begin{short}
    Convenience function for inserting a normal menu item into the menu.
  @end{short}
  Combine the functions @fun{g-menu-item-new} and @fun{g-menu-insert-item} for
  a more flexible alternative.
  @see-class{g-menu}
  @see-function{g-menu-item-new}
  @see-function{g-menu-insert-item}"
  (%g-menu-insert menu
                  position
                  (if label label (null-pointer))
                  (if action action (null-pointer))))

(export 'g-menu-insert)

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_prepend" %g-menu-prepend) :void
  (menu (g-object g-menu))
  (label :string)
  (action :string))

(defun g-menu-prepend (menu label action)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[action]{the detailed action string, or @code{nil}}
  @begin{short}
    Convenience function for prepending a normal menu item to the start of the
    menu.
  @end{short}
  Combine the functions @fun{g-menu-item-new} and @fun{g-menu-insert-item} for
  a more flexible alternative.
  @see-class{g-menu}
  @see-function{g-menu-item-new}
  @see-function{g-menu-insert-item}"
  (%g-menu-prepend menu
                   (if label label (null-pointer))
                   (if action action (null-pointer))))

(export 'g-menu-prepend)

;;; ----------------------------------------------------------------------------
;;; g_menu_append ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_append" %g-menu-append) :void
  (menu (g-object g-menu))
  (label :string)
  (action :string))

(defun g-menu-append (menu label action)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[action]{the detailed action string, or @code{nil}}
  @begin{short}
    Convenience function for appending a normal menu item to the end of the
    menu.
  @end{short}
  Combine the functions @fun{g-menu-item-new} and @fun{g-menu-insert-item} for
  a more flexible alternative.
  @see-class{g-menu}
  @see-function{g-menu-item-new}
  @see-function{g-menu-insert-item}"
  (%g-menu-append menu
                  (if label label (null-pointer))
                  (if action action (null-pointer))))

(export 'g-menu-append)

;;; ----------------------------------------------------------------------------
;;; g_menu_insert_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_insert_item" g-menu-insert-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[position]{an integer with the position at which to insert the item}
  @argument[item]{a @class{g-menu-item} object to insert}
  @begin{short}
    Inserts a menu item into the menu.
  @end{short}

  The \"insertion\" is actually done by copying all of the attribute and link
  values of @arg{item} and using them to form a new menu item within the menu.
  As such, @arg{item} itself is not really inserted, but rather, a menu item
  that is exactly the same as the one presently described by @arg{item}.

  This means that @arg{item} is essentially useless after the insertion occurs.
  Any changes you make to it are ignored unless it is inserted again, at which
  point its updated values will be copied. You should probably just free
  @arg{item} once you are done.

  There are many convenience functions to take care of common cases. See the
  functions @fun{g-menu-insert}, @fun{g-menu-insert-section} and
  @fun{g-menu-insert-submenu} as well as \"prepend\" and \"append\" variants of
  each of these functions.
  @see-class{g-menu}
  @see-class{g-menu-item}
  @see-function{g-menu-insert}
  @see-function{g-menu-insert-section}
  @see-function{g-menu-insert-submenu}"
  (menu (g-object g-menu))
  (position :int)
  (item (g-object g-menu-item)))

(export 'g-menu-insert-item)

;;; ----------------------------------------------------------------------------
;;; g_menu_append_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_append_item" g-menu-append-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[item]{a @class{g-menu-item} object to append}
  @begin{short}
    Appends the menu item to the end of the menu.
  @end{short}
  See the function @fun{g-menu-insert-item} for more information.
  @see-class{g-menu}
  @see-class{g-menu-item}
  @see-function{g-menu-insert-item}"
  (menu (g-object g-menu))
  (item (g-object g-menu-item)))

(export 'g-menu-append-item)

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_prepend_item" g-menu-prepend-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[item]{a @class{g-menu-item} object to prepend}
  @begin{short}
    Prepends the menu item to the start of the menu.
  @end{short}
  See the function @fun{g-menu-insert-item} for more information.
  @see-class{g-menu}
  @see-class{g-menu-item}
  @see-function{g-menu-insert-item}"
  (menu (g-object g-menu))
  (item (g-object g-menu-item)))

(export 'g-menu-prepend-item)

;;; ----------------------------------------------------------------------------
;;; g_menu_insert_section ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_insert_section" %g-menu-insert-section) :void
  (menu (g-object g-menu))
  (position :int)
  (label :string)
  (section (g-object g-menu-model)))

(defun g-menu-insert-section (menu position label section)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @argument[menu]{a @class{g-menu} object}
  @argument[position]{an integer with the position at which to insert the item}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[section]{a @class{g-menu-model} object with the items of the
    section}
  @begin{short}
    Convenience function for inserting a section menu item into menu.
  @end{short}
  Combine the functions @fun{g-menu-item-new-section} and
  @fun{g-menu-insert-item} for a more flexible alternative.
  @see-class{g-menu}
  @see-class{g-menu-model}
  @see-function{g-menu-item-new-section}
  @see-function{g-menu-insert-item}"
  (%g-menu-insert-section menu
                          position
                          (if label label (null-pointer))
                          section))

(export 'g-menu-insert-section)

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend_section ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_prepend_section" %g-menu-prepend-section) :void
  (menu (g-object g-menu))
  (label :string)
  (section (g-object g-menu-model)))

(defun g-menu-prepend-section (menu label section)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[section]{a @class{g-menu-model} object with the items of the
    section}
  @begin{short}
    Convenience function for prepending a section menu item to the start of
    the menu.
  @end{short}
  Combine the functions @fun{g-menu-item-new-section} and
  @fun{g-menu-insert-item} for a more flexible alternative.
  @see-class{g-menu}
  @see-class{g-menu-model}
  @see-function{g-menu-item-new-section}
  @see-function{g-menu-insert-item}"
  (%g-menu-prepend-section menu
                           (if label label (null-pointer))
                           section))

(export 'g-menu-prepend-section)

;;; ----------------------------------------------------------------------------
;;; g_menu_append_section ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_append_section" %g-menu-append-section) :void
  (menu (g-object g-menu))
  (label :string)
  (section (g-object g-menu-model)))

(defun g-menu-append-section (menu label section)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[section]{a @class{g-menu-model} object with the items of the
    section}
  @begin{short}
    Convenience function for appending a section menu item to the emd of
    the menu.
  @end{short}
  Combine the functions @fun{g-menu-item-new-section} and
  @fun{g-menu-insert-item} for a more flexible alternative.
  @see-class{g-menu}
  @see-class{g-menu-model}
  @see-function{g-menu-item-new-section}
  @see-function{g-menu-insert-item}"
  (%g-menu-append-section menu
                          (if label label (null-pointer))
                          section))

(export 'g-menu-append-section)

;;; ----------------------------------------------------------------------------
;;; g_menu_append_submenu ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_append_submenu" %g-menu-append-submenu) :void
  (menu (g-object g-menu))
  (label :string)
  (submenu (g-object g-menu-model)))

(defun g-menu-append-submenu (menu label submenu)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[submenu]{a @class{g-menu-model} object with the items of the
    submenu}
  @begin{short}
    Convenience function for appending a submenu menu item to the end of the
    menu.
  @end{short}
  Combine the functions @fun{g-menu-item-new-submenu} and
  @fun{g-menu-insert-item} for a more flexible alternative.
  @see-class{g-menu}
  @see-class{g-menu-model}
  @see-function{g-menu-item-new-submenu}
  @see-function{g-menu-insert-item}"
  (%g-menu-append-submenu menu (if label label (null-pointer)) submenu))

(export 'g-menu-append-submenu)

;;; ----------------------------------------------------------------------------
;;; g_menu_insert_submenu ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_insert_submenu" %g-menu-insert-submenu) :void
  (menu (g-object g-menu))
  (position :int)
  (label :string)
  (submenu (g-object g-menu-model)))

(defun g-menu-insert-submenu (menu position label submenu)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[position]{an integer with the position at which to insert the item}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[submenu]{a @class{g-menu-model} object with the items of the
    submenu}
  @begin{short}
    Convenience function for inserting a submenu menu item into the menu.
  @end{short}
  Combine the functions @fun{g-menu-item-new-submenu} and
  @fun{g-menu-insert-item} for a more flexible alternative.
  @see-class{g-menu}
  @see-class{g-menu-model}
  @see-function{g-menu-item-new-submenu}
  @see-function{g-menu-insert-item}"
  (%g-menu-insert-submenu menu
                          position
                          (if label label (null-pointer))
                          submenu))

(export 'g-menu-insert-submenu)

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend_submenu ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_prepend_submenu" %g-menu-prepend-submenu) :void
  (menu (g-object g-menu))
  (label :string)
  (submenu (g-object g-menu-model)))

(defun g-menu-prepend-submenu (menu label submenu)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[submenu]{a @class{g-menu-model} object with the items of the
    submenu}
  @begin{short}
    Convenience function for prepending a submenu menu item to the start of the
    menu.
  @end{short}
  Combine the functions @fun{g-menu-item-new-submenu} and
  @fun{g-menu-insert-item} for a more flexible alternative.
  @see-class{g-menu}
  @see-class{g-menu-model}
  @see-function{g-menu-item-new-submenu}
  @see-function{g-menu-insert-item}"
  (%g-menu-prepend-submenu menu
                           (if label label (null-pointer))
                           submenu))

(export 'g-menu-prepend-submenu)

;;; ----------------------------------------------------------------------------
;;; g_menu_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_remove" g-menu-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[menu]{a @class{g-menu} object}
  @argument[position]{an integer with the position of the menu item to remove}
  @begin{short}
    Removes an item from the menu.
  @end{short}
  The argument @arg{position} gives the index of the menu item to remove.

  It is an error if @arg{position} is not in the range from 0 to one less than
  the number of menu items in the menu.

  It is not possible to remove items by identity since items are added to the
  menu simply by copying their links and attributes, i.e. identity of the menu
  item itself is not preserved.
  @see-class{g-menu}"
  (menu (g-object g-menu))
  (position :int))

(export 'g-menu-remove)

;;; ----------------------------------------------------------------------------
;;; g_menu_remove_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_remove_all" g-menu-remove-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @begin{short}
    Removes all items in the menu.
  @end{short}
  @see-class{g-menu}"
  (menu (g-object g-menu)))

(export 'g-menu-remove-all)

;;; ----------------------------------------------------------------------------
;;; GMenuItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GMenuItem" g-menu-item
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "g_menu_item_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-menu-item 'type)
 "@version{2021-8-16}
  @begin{short}
    The @sym{g-menu-item} object is an opaque structure type.
  @end{short}
  You must access it using the API functions.
  @see-class{g-menu}")

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_item_new" %g-menu-item-new) (g-object g-menu-item)
  (label :string)
  (action :string))

(defun g-menu-item-new (label action)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[action]{a detailed action string, or @code{nil}}
  @return{A new @class{g-menu-item} object.}
  @begin{short}
    Creates a new @class{g-menu-item} object.
  @end{short}

  If @arg{label} is non-@code{nil} it is used to set the \"label\" attribute of
  the new menu item.

  If @arg{action} is non-@code{nil} it is used to set the \"action\" and
  possibly the \"target\" attribute of the new item. See the function
  @fun{g-menu-item-set-detailed-action} for more information.
  @see-class{g-menu-item}
  @see-function{g-menu-item-set-detailed-action}"
  (%g-menu-item-new (if label label (null-pointer))
                    (if action action (null-pointer))))

(export 'g-menu-item-new)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new_section ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_item_new_section" %g-menu-item-new-section)
    (g-object g-menu-item)
  (label :string)
  (section (g-object g-menu-model)))

(defun g-menu-item-new-section (label section)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-18}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[section]{a @class{g-menu-model} object with the menu items of the
    section}
  @return{A new @class{g-menu-item} object.}
  @begin{short}
    Creates a new @class{g-menu-item} object representing a section.
  @end{short}
  This is a convenience API around the functions @fun{g-menu-item-new} and
  @fun{g-menu-item-set-section}.

  The effect of having one menu appear as a section of another is exactly as
  it sounds: the items from section become a direct part of the menu that
  @arg{item} is added to.

  Visual separation is typically displayed between two non-empty sections. If
  label is non-@code{nil} then it will be encorporated into this visual
  indication. This allows for labeled subsections of a menu.

  As a simple example, consider a typical \"Edit\" menu from a simple program.
  It probably contains an \"Undo\" and \"Redo\" item, followed by a separator,
  followed by \"Cut\", \"Copy\" and \"Paste\".

  This would be accomplished by creating three @class{g-menu} objects. The first
  would be populated with the \"Undo\" and \"Redo\" items, and the second with
  the \"Cut\", \"Copy\" and \"Paste\" items. The first and second menus would
  then be added as submenus of the third. In XML format, this would look
  something like the following:
  @begin{pre}
<menu id='edit-menu'>
  <section>
    <item label='Undo'/>
    <item label='Redo'/>
  </section>
  <section>
    <item label='Cut'/>
    <item label='Copy'/>
    <item label='Paste'/>
  </section>
</menu>
  @end{pre}
  The following example is exactly equivalent. It is more illustrative of the
  exact relationship between the menus and items, keeping in mind that the
  'link' element defines a new menu that is linked to the containing one. The
  style of the second example is more verbose and difficult to read, and
  therefore not recommended except for the purpose of understanding what is
  really going on.
  @begin{pre}
<menu id='edit-menu'>
  <item>
    <link name='section'>
      <item label='Undo'/>
      <item label='Redo'/>
    </link>
  </item>
  <item>
    <link name='section'>
      <item label='Cut'/>
      <item label='Copy'/>
      <item label='Paste'/>
    </link>
  </item>
</menu>
  @end{pre}
  @see-class{g-menu-item}
  @see-function{g-menu-item-new}
  @see-function{g-menu-item-set-section}"
  (%g-menu-item-new-section (if label label (null-pointer)) section))

(export 'g-menu-item-new-section)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new_submenu ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_item_new_submenu" %g-menu-item-new-submenu)
    (g-object g-menu-item)
  (label :string)
  (submenu (g-object g-menu-model)))

(defun g-menu-item-new-submenu (label submenu)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[submenu]{a @class{g-menu-model} object with the menu items of the
    submenu}
  @return{A new @class{g-menu-item} object.}
  @begin{short}
    Creates a new @class{g-menu-item} object representing a submenu.
  @end{short}
  This is a convenience API around the functions @fun{g-menu-item-new} and
  @fun{g-menu-item-set-submenu}.
  @see-class{g-menu-item}
  @see-class{g-menu-model}
  @see-function{g-menu-item-new}
  @see-function{g-menu-item-set-submenu}"
  (%g-menu-item-new-submenu (if label label (null-pointer)) submenu))

(export 'g-menu-item-new-submenu)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new_from_model ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_item_new_from_model" g-menu-item-new-from-model)
    (g-object g-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[model]{a @class{g-menu-model} object}
  @argument[index]{an integer with the index of an menu item in @arg{model}}
  @return{A new @class{g-menu-item} object.}
  @begin{short}
    Creates a @class{g-menu-item} object as an exact copy of an existing menu
    item in a @class{g-menu-model} object.
  @end{short}
  The argument @arg{index} must be valid, i.e. be sure to call the function
  @fun{g-menu-model-n-items} first.
  @see-class{g-menu-item}
  @see-class{g-menu-model}
  @see-function{g-menu-model-n-items}"
  (model (g-object g-menu-model))
  (index :int))

(export 'g-menu-item-new-from-model)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_label ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu-item_set_label" %g-menu-item-set-label) :void
  (item (g-object g-menu-item))
  (label :string))

(defun g-menu-item-set-label (item label)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[item]{a @class{g-menu-item} object}
  @argument[label]{a string with the label to set, or @code{nil}}
  @begin{short}
    Sets or unsets the \"label\" attribute of the menu item.
  @end{short}
  If @arg{label} is non-@code{nil} it is used as the label for the menu item.
  If it is @code{nil} then the label attribute is unset.
  @see-class{g-menu-item}"
  (%g-menu-item-set-label item (if label label (null-pointer))))

(export 'g-menu-item-set-label)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_item_set_icon" g-menu-item-set-icon) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-5}
  @argument[item]{a @class{g-menu-item} object}
  @argument[icon]{a @class{g-icon} object}
  @begin{short}
    Sets or unsets the icon on the menu item.
  @end{short}

  This call is the same as calling the @fun{g-icon-serialize} function and using
  the result as the value to the @fun{g-menu-item-attribute-value} function
  for \"icon\".

  This API is only intended for use with \"noun\" menu items. Things like
  bookmarks or applications in an \"Open With\" menu. Do not use it on menu
  items corresponding to verbs, e.g. stock icons for 'Save' or 'Quit'.

  If the @arg{icon} argument is @code{nil} then the icon is unset.
  @see-class{g-menu-item}
  @see-function{g-icon-serialize}
  @see-function{g-menu-item-attribute-value}"
  (item (g-object g-menu-item))
  (icon (g-object g-icon)))

(export 'g-menu-item-set-icon)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_action_and_target_value ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_item_set_action_and_target_value"
          %g-menu-item-set-action-and-target-value) :void
  (item (g-object g-menu-item))
  (action :string)
  (value (:pointer (:struct g-variant))))

(defun g-menu-item-set-action-and-target-value (item action value)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[item]{a @class{g-menu-item} object}
  @argument[action]{a string with the name of the action for this menu item}
  @argument[value]{a @symbol{g-variant} instance to use as the action target}
  @begin{short}
    Sets or unsets the \"action\" and \"target\" attributes of the menu item.
  @end{short}

  If @arg{action} is @code{nil} then both the \"action\" and \"target\"
  attributes are unset, and @arg{value} is ignored.

  If @arg{action} is non-@code{nil} then the \"action\" attribute is set. The
  \"target\" attribute is then set to the value of @arg{value} if it is
  non-@code{nil} or unset otherwise.

  Normal menu items, i.e. not submenu, section or other custom item types, are
  expected to have the \"action\" attribute set to identify the action that they
  are associated with. The state type of the action help to determine the
  disposition of the menu item. See teh @class{g-action} and
  @class{g-action-group} documentation for an overview of actions.

  In general, clicking on the menu item will result in activation of the named
  action with the \"target\" attribute given as the parameter to the action
  invocation. If the \"target\" attribute is not set then the action is invoked
  with no parameter.

  If the action has no state then the menu item is usually drawn as a plain
  menu item, i.e. with no additional decoration.

  If the action has a boolean state then the menu item is usually drawn as a
  toggle menu item, i.e. with a checkmark or equivalent indication. The item
  should be marked as 'toggled' or 'checked' when the boolean state is
  @em{true}.

  If the action has a string state then the menu item is usually drawn as a
  radio menu item, i.e. with a radio bullet or equivalent indication. The item
  should be marked as 'selected' when the string state is equal to the value
  of the target property.

  See the functions @fun{g-menu-item-set-action-and-target} or
  @fun{g-menu-item-set-detailed-action} for two equivalent calls that are
  probably more convenient for most uses.
  @see-class{g-menu-item}
  @see-symbol{g-variant}
  @see-function{g-menu-item-set-action-and-target}
  @see-function{g-menu-item-set-detailed-action}"
  (%g-menu-item-set-action-and-target-value item
                                            (if action action (null-pointer))
                                            (if value value (null-pointer))))

(export 'g-menu-item-set-action-and-target-value)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_action_and_target ()
;;;
;;; void g_menu_item_set_action_and_target (GMenuItem *menu_item,
;;;                                         const gchar *action,
;;;                                         const gchar *format_string,
;;;                                         ...);
;;;
;;; Sets or unsets the "action" and "target" attributes of menu_item.
;;;
;;; If action is NULL then both the "action" and "target" attributes are unset
;;; (and format_string is ignored along with the positional parameters).
;;;
;;; If action is non-NULL then the "action" attribute is set. format_string is
;;; then inspected. If it is non-NULL then the proper position parameters are
;;; collected to create a GVariant instance to use as the target value. If it is
;;; NULL then the positional parameters are ignored and the "target" attribute
;;; is unset.
;;;
;;; See also g_menu_item_set_action_and_target_value() for an equivalent call
;;; that directly accepts a GVariant. See g_menu_item_set_detailed_action() for
;;; a more convenient version that works with string-typed targets.
;;;
;;; See also g_menu_item_set_action_and_target_value() for a description of the
;;; semantics of the action and target attributes.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; action :
;;;     the name of the action for this item. [allow-none]
;;;
;;; format_string :
;;;     a GVariant format string. [allow-none]
;;;
;;; ... :
;;;     positional parameters, as per format_string
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_detailed_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_item_set_detailed_action" g-menu-item-set-detailed-action)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[item]{a @class{g-menu-item} object}
  @argument[action]{a detailed action string}
  @begin{short}
    Sets the \"action\" and possibly the \"target\" attribute of the menu item.
  @end{short}

  The format of @arg{action} is the same format parsed by the function
  @fun{g-action-parse-detailed-name}.

  See the functions @fun{g-menu-item-set-action-and-target} or
  @fun{g-menu-item-set-action-and-target-value} for more flexible, but slightly
  less convenient, alternatives.

  See also the function @fun{g-menu-item-set-action-and-target-value} for a
  description of the semantics of the action and target attributes.
  @see-class{g-menu-item}
  @see-function{g-action-parse-detailed-name}
  @see-function{g-menu-item-set-action-and-target}
  @see-function{g-menu-item-set-action-and-target-value}"
  (item (g-object g-menu-item))
  (action :string))

(export 'g-menu-item-set-detailed-action)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_section ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_item_set_section" %g-menu-item-set-section) :void
  (item (g-object g-menu-item))
  (section (g-object g-menu-model)))

(defun g-menu-item-set-section (item section)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[item]{a @class{g-menu-item} object}
  @argument[section]{a @class{g-menu-model}, or @code{nil}}
  @begin{short}
    Sets or unsets the \"section\" link of the menu item to @arg{section}.
  @end{short}

  The effect of having one menu appear as a section of another is exactly as
  it sounds: the items from @arg{section} become a direct part of the menu that
  @arg{item} is added to. See the function @fun{g-menu-item-new-section} for
  more information about what it means for a menu item to be a section.
  @see-class{g-menu-item}
  @see-class{g-menu-model}
  @see-function{g-menu-item-new-section}"
  (%g-menu-item-set-section item (if section section (null-pointer))))

(export 'g-menu-item-set-section)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_submenu ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_item_set_submenu" %g-menu-item-set-submenu) :void
  (item (g-object g-menu-item))
  (submenu (g-object g-menu-model)))

(defun g-menu-item-set-submenu (item submenu)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-16}
  @argument[item]{a @class{g-menu-item} object}
  @argument[submenu]{a @class{g-menu-model} object, or @code{nil}}
  @begin{short}
    Sets or unsets the \"submenu\" link of the menu item to @arg{submenu}.
  @end{short}

  If @arg{submenu} is non-@code{nil}, it is linked to. If it is @code{nil} then
  the link is unset.

  The effect of having one menu appear as a submenu of another is exactly as
  it sounds.
  @see-class{g-menu-item}
  @see-class{g-menu-model}"
  (%g-menu-item-set-submenu item (if submenu submenu (null-pointer))))

(export 'g-menu-item-set-submenu)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_get_attribute_value ()
;;; g_menu_item_set_attribute_value () -> g-menu-item-attribute-value
;;; ----------------------------------------------------------------------------

(defun (setf g-menu-item-attribute-value) (value item attribute)
  (foreign-funcall "g_menu_item_set_attribute_value"
                   (g-object g-menu-item) item
                   :string attribute
                   (:pointer (:struct g-variant)) value
                   :void)
  value)

(defcfun ("g_menu_item_get_attribute_value" %g-menu-item-attribute-value)
    (:pointer (:struct g-variant))
  (item (g-object g-menu-item))
  (attribute :string)
  (vtype (g-boxed-foreign g-variant-type)))

(defun g-menu-item-attribute-value (item attribute &optional (vtype nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-8-17}
  @syntax[]{(g-menu-item-attribute-value item attribute) => value}
  @syntax[]{(g-menu-item-attribute-value item attribute vtype) => value}
  @syntax[]{(setf (g-menu-item-attribute-value item attribute) value)}
  @argument[item]{a @class{g-menu-item} object}
  @argument[attribute]{a string with the attribute name}
  @argument[vtype]{the optional expected @class{g-variant-type} type or a type
    string of the attribute}
  @argument[value]{a @symbol{g-variant} value to use as the value, or
    @code{nil}}
  @begin{short}
    The function @sym{g-menu-item-attribute-value} queries the named attribute
    on the menu item.
  @end{short}
  The function @sym{(setf g-menu-item-attribute-value)} sets or unsets an
  attribute.

  If the argument @arg{vtype} is specified and the attribute does not have this
  type, @code{nil} is returned. @code{Nil} is also returned if the attribute
  simply does not exist.

  The attribute to set or unset is specified by @arg{attribute}. This can be
  one of the standard attribute names \"label\", \"action\", \"target\", or a
  custom attribute name. Attribute names are restricted to lowercase
  characters, numbers and '-'. Furthermore, the names must begin with a
  lowercase character, must not end with a '-', and must not contain
  consecutive dashes.

  If @arg{value} is non-@code{nil} then it is used as the new value for the
  attribute. If @arg{value} is @code{nil} then the attribute is unset. If the
  @symbol{g-variant} value is floating, it is consumed.
  @see-class{g-menu-item}
  @see-symbol{g-variant}
  @see-class{g-variant-type}"
  (if (stringp vtype)
      (let ((vtype1 (g-variant-type-new vtype)))
        (unwind-protect
          (%g-menu-item-attribute-value item attribute vtype1)
          (g-variant-type-free vtype1)))
      (%g-menu-item-attribute-value item attribute vtype)))

(export 'g-menu-item-attribute-value)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_get_attribute ()
;;;
;;; gboolean g_menu_item_get_attribute (GMenuItem *menu_item,
;;;                                     const gchar *attribute,
;;;                                     const gchar *format_string,
;;;                                     ...);
;;;
;;; Queries the named attribute on menu_item.
;;;
;;; If the attribute exists and matches the GVariantType corresponding to
;;; format_string then format_string is used to deconstruct the value into the
;;; positional parameters and TRUE is returned.
;;;
;;; If the attribute does not exist, or it does exist but has the wrong type,
;;; then the positional parameters are ignored and FALSE is returned.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; attribute :
;;;     the attribute name to query
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     positional parameters, as per format_string
;;;
;;; Returns :
;;;     TRUE if the named attribute was found with the expected type
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_attribute ()
;;;
;;; void g_menu_item_set_attribute (GMenuItem *menu_item,
;;;                                 const gchar *attribute,
;;;                                 const gchar *format_string,
;;;                                 ...);
;;;
;;; Sets or unsets an attribute on menu_item.
;;;
;;; The attribute to set or unset is specified by attribute. This can be one of
;;; the standard attribute names G_MENU_ATTRIBUTE_LABEL,
;;; G_MENU_ATTRIBUTE_ACTION, G_MENU_ATTRIBUTE_TARGET, or a custom attribute
;;; name. Attribute names are restricted to lowercase characters, numbers and
;;; '-'. Furthermore, the names must begin with a lowercase character, must not
;;; end with a '-', and must not contain consecutive dashes.
;;;
;;; If format_string is non-NULL then the proper position parameters are
;;; collected to create a GVariant instance to use as the attribute value. If it
;;; is NULL then the positional parameterrs are ignored and the named attribute
;;; is unset.
;;;
;;; See also g_menu_item_set_attribute_value() for an equivalent call that
;;; directly accepts a GVariant.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; attribute :
;;;     the attribute to set
;;;
;;; format_string :
;;;     a GVariant format string, or NULL. [allow-none]
;;;
;;; ... :
;;;     positional parameters, as per format_string
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_get_link ()
;;; g_menu_item_set_link () -> g-menu-item-link
;;; ----------------------------------------------------------------------------

(defun (setf g-menu-item-link) (value item link)
  (foreign-funcall "g_menu_item_set_link"
                   (g-object g-menu-item) item
                   :string link
                   (g-object g-menu-model) value
                   :void)
  value)

(defcfun ("g_menu_item_get_link" g-menu-item-link) (g-object g-menu-model)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @syntax[]{(g-menu-item-link item link) => model}
  @syntax[]{(setf (g-menu-item-link item link) model)}
  @argument[item]{a @class{g-menu-item} object}
  @argument[link]{a string with the type of link to establish or unset}
  @argument[model]{the @class{g-menu-model} object to link to, or @code{nil} to
    unset}
  @begin{short}
    The function @sym{g-menu-item-link} queries the named link on @arg{item}.
  @end{short}
  The function @sym{(set g-menu-item-link)} creates a link from @arg{item} to
  @arg{model} if non-@code{nil}, or unsets it.

  Links are used to establish a relationship between a particular menu item
  and another menu. For example, \"submenu\" is used to associate a submenu with
  a particular menu item, and \"section\" is used to create a section. Other
  types of link can be used, but there is no guarantee that clients will be able
  to make sense of them. Link types are restricted to lowercase characters,
  numbers and '-'. Furthermore, the names must begin with a lowercase character,
  must not end with a '-', and must not contain consecutive dashes.
  @see-class{g-menu-item}
  @see-class{g-menu-model}"
  (item (g-object g-menu-item))
  (link :string))

(export 'g-menu-item-link)

;;; --- End of file gio.menu.lisp ----------------------------------------------
