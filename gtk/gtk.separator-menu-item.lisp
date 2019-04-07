;;; ----------------------------------------------------------------------------
;;; gtk.separator-menu-item.lisp
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
;;; GtkSeparatorMenuItem
;;;
;;;     A separator used in menus
;;;
;;; Types and Values
;;;
;;;     GtkSeparatorMenuItem
;;;
;;; Functions
;;;
;;;     gtk_separator_menu_item_new
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkMenuItem
;;;                         ╰── GtkSeparatorMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSeparatorMenuItem implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSeparatorMenuItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSeparatorMenuItem" gtk-separator-menu-item
  (:superclass gtk-menu-item
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_separator_menu_item_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-separator-menu-item 'type)
 "@version{2013-6-1}
  The @sym{gtk-separator-menu-item} widget is a separator used to group items
  within a menu. It displays a horizontal line with a shadow to make it appear
  sunken into the interface.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-separator-menu-item} has a single CSS node with name
    @class{separator}.
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; gtk_separator_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-separator-menu-item-new))

(defun gtk-separator-menu-item-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-1-26}
  @return{A new @class{gtk-separator-menu-item} widget.}
  @begin{short}
    Creates a new @class{gtk-separator-menu-item} widget.
  @end{short}
  @see-class{gtk-separator-menu-item}"
  (make-instance 'gtk-separator-menu-item))

(export 'gtk-separator-menu-item-new)

;;; --- End of file gtk.separator-menu-item.lisp -------------------------------
