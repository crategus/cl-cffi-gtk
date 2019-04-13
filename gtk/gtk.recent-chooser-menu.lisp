;;; ----------------------------------------------------------------------------
;;; gtk.recent-chooser-menu.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;;     Displays recently used files in a menu
;;;
;;; Types and Values
;;;
;;;     GtkRecentChooserMenu
;;;
;;; Functions
;;;
;;;     gtk_recent_chooser_menu_new
;;;     gtk_recent_chooser_menu_new_for_manager
;;;     gtk_recent_chooser_menu_get_show_numbers
;;;     gtk_recent_chooser_menu_set_show_numbers
;;;
;;; Properties
;;;
;;;     gboolean   show-numbers    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkMenuShell
;;;                     ╰── GtkMenu
;;;                         ╰── GtkRecentChooserMenu
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRecentChooserMenu implements AtkImplementorIface, GtkBuildable,
;;;     GtkRecentChooser and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentChooserMenu
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkRecentChooserMenu" 'gtk-recent-chooser-menu))

(define-g-object-class "GtkRecentChooserMenu" gtk-recent-chooser-menu
  (:superclass gtk-menu
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkRecentChooser"
                "GtkActivatable")
   :type-initializer "gtk_recent_chooser_menu_get_type")
  ((show-numbers
    gtk-recent-chooser-menu-show-numbers
    "show-numbers" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-recent-chooser-menu 'type)
 "@version{2013-5-28}
  @begin{short}
    @sym{gtk-recent-chooser-menu} is a widget suitable for displaying recently
    used files inside a menu. It can be used to set a sub-menu of a
    @class{gtk-menu-item} using the generic function
    @fun{gtk-menu-item-submenu}, or as the menu of a
    @class{gtk-menu-tool-button}.
  @end{short}

  Note that @sym{gtk-recent-chooser-menu} does not have any methods of its own.
  Instead, you should use the functions that work on a
  @class{gtk-recent-chooser}.

  Note also that @sym{gtk-recent-chooser-menu} does not support multiple
  filters, as it has no way to let the user choose between them as the
  @class{gtk-recent-chooser-widget} and @class{gtk-recent-chooser-dialog}
  widgets do. Thus using the function @fun{gtk-recent-chooser-add-filter} on a
  @sym{gtk-recent-chooser-menu} widget will yield the same effects as using the
  function @fun{gtk-recent-chooser-set-filter}, replacing any currently set
  filter with the supplied filter; the function
  @fun{gtk-recent-chooser-remove-filter} will remove any currently set
  @class{gtk-recent-filter} object and will unset the current filter; the
  function @fun{gtk-recent-chooser-list-filters} will return a list containing
  a single @class{gtk-recent-filter} object.

  Recently used files are supported since GTK+ 2.10.")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-recent-chooser-menu-show-numbers -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-numbers"
                                               'gtk-recent-chooser-menu) 't)
 "The @code{show-numbers} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the first ten items in the menu should be prepended by a number
  acting as a unique mnemonic. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-choose-menu-show-numbers atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-chooser-menu-show-numbers 'function)
 "@version{2013-11-23}
  Accessor of the slot @slot[gtk-recent-chooser-menu-show-numbers]{show-numbers}
  of the @class{gtk-recent-chooser-menu} class.
  @see-class{gtk-recent-chooser-menu}
  @see-function{gtk-recent-chooser-menu-get-show-numbers}
  @see-function{gtk-recent-chooser-menu-set-show-numbers}")

;;; ----------------------------------------------------------------------------

;; TODO: Check the implementation of the child properties.
;;       GtkRecentChooserMenu has no documented child properties.

#|
(define-child-property "GtkRecentChooserMenu"
                       gtk-recent-chooser-menu-child-left-attach
                       "left-attach" "gint" t t t)

(define-child-property "GtkRecentChooserMenu"
                       gtk-recent-chooser-menu-child-right-attach
                       "right-attach" "gint" t t t)

(define-child-property "GtkRecentChooserMenu"
                       gtk-recent-chooser-menu-child-top-attach
                       "top-attach" "gint" t t t)

(define-child-property "GtkRecentChooserMenu"
                       gtk-recent-chooser-menu-child-bottom-attach
                       "bottom-attach" "gint" t t t)
|#

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_menu_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-menu-new))

(defun gtk-recent-chooser-menu-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @return{A new @class{gtk-recent-chooser-menu} widget}
  @begin{short}
    Creates a new @class{gtk-recent-chooser-menu} widget.
  @end{short}

  This kind of widget shows the list of recently used resources as a menu,
  each item as a menu item. Each item inside the menu might have an icon,
  representing its MIME type, and a number, for mnemonic access.

  This widget implements the @class{gtk-recent-chooser} interface.

  This widget creates its own @class{gtk-recent-manager} object. See the
  @fun{gtk-recent-chooser-menu-new-for-manager} function to know how to create
  a @class{gtk-recent-chooser-menu} widget bound to another
  @class{gtk-recent-manager} object.
  @see-class{gtk-recent-chooser-menu}
  @see-class{gtk-recent-chooser}
  @see-class{gtk-recent-manager}
  @see-function{gtk-recent-chooser-menu-new-for-manager}"
  (make-instance 'gtk-recent-chooser-menu))

(export 'gtk-recent-chooser-menu-new)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_menu_new_for_manager ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-menu-new-for-manager))

(defun gtk-recent-chooser-menu-new-for-manager (manager)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[manager]{a @class{gtk-recent-manager} object}
  @return{A new @class{gtk-recent-chooser-menu}, bound to @arg{manager}.}
  @begin{short}
    Creates a new @class{gtk-recent-chooser-menu} widget using @arg{manager} as
    the underlying recently used resources manager.
  @end{short}

  This is useful if you have implemented your own recent manager, or if you
  have a customized instance of a @class{gtk-recent-manager} object or if you
  wish to share a common @class{gtk-recent-manager} object among multiple
  @class{gtk-recent-chooser} widgets.
  @see-class{gtk-recent-chooser-menu}
  @see-class{gtk-recent-manager}
  @see-function{gtk-recent-chooser-menu-new}"
  (make-instance 'gtk-recent-chooser-menu
                 :recent-manager manager))

(export 'gtk-recent-chooser-menu-new-for-manager)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_menu_get_show_numbers ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-menu-get-show-numbers))

(defun gtk-recent-chooser-menu-get-show-numbers (menu)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[menu]{a @class{gtk-recent-chooser-menu} widget}
  @return{@em{True} if numbers should be shown.}
  @begin{short}
    Returns the value set by the function
    @fun{gtk-recent-chooser-menu-set-show-numbers}.
  @end{short}
  @see-class{gtk-recent-chooser-menu}
  @see-function{gtk-recent-chooser-menu-set-show-numbers}"
  (gtk-recent-chooser-menu-show-numbers menu))

(export 'gtk-recent-chooser-menu-get-show-numbers)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_menu_set_show_numbers ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-menu-set-show-numbers))

(defun gtk-recent-chooser-menu-set-show-numbers (menu show-numbers)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[menu]{a @class{gtk-recent-chooser-menu} widget}
  @argument[show-numbers]{whether to show numbers}
  @begin{short}
    Sets whether a number should be added to the items of menu.
  @end{short}
  The numbers are shown to provide a unique character for a mnemonic to be used
  inside ten menu item's label. Only the first the items get a number to avoid
  clashes.
  @see-class{gtk-recent-chooser-menu}
  @see-function{gtk-recent-chooser-menu-get-show-numbers}"
  (setf (gtk-recent-chooser-menu-show-numbers menu) show-numbers))

(export 'gtk-recent-chooser-menu-set-show-numbers)

;;; --- End of file gtk.recent-chooser-menu.lisp -------------------------------
