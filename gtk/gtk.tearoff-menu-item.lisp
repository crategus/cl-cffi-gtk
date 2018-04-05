;;; ----------------------------------------------------------------------------
;;; gtk.tearoff-menu-item.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See http://www.gtk.org.
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
;;; GtkTearoffMenuItem
;;;
;;; A menu item used to tear off and reattach its menu
;;;
;;; Synopsis
;;;
;;;     GtkTearoffMenuItem
;;;
;;;     gtk_tearoff_menu_item_new
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkMenuItem
;;;                                  +----GtkTearoffMenuItem
;;;
;;; Implemented Interfaces
;;;
;;; GtkTearoffMenuItem implements AtkImplementorIface, GtkBuildable and
;;; GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTearoffMenuItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTearoffMenuItem" gtk-tearoff-menu-item
  (:superclass gtk-menu-item
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_tearoff_menu_item_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tearoff-menu-item 'type)
 "@version{2013-6-1}
  @begin{short}
    A @sym{gtk-tearoff-menu-item} is a special @class{gtk-menu-item} which is
    used to tear off and reattach its menu.
  @end{short}

  When its menu is shown normally, the @class{gtk-tearoff-menu-item} is drawn as
  a dotted line indicating that the menu can be torn off. Activating it causes
  its menu to be torn off and displayed in its own window as a tearoff menu.

  When its menu is shown as a tearoff menu, the @class{gtk-tearoff-menu-item}
  is drawn as a dotted line which has a left pointing arrow graphic indicating
  that the tearoff menu can be reattached. Activating it will erase the tearoff
  menu window.

  @subheading{Note}
    @sym{gtk-tearoff-menu-item} is deprecated and should not be used in newly
    written code. Menus are not meant to be torn around.")

;;; ----------------------------------------------------------------------------
;;; gtk_tearoff_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tearoff-menu-item-new))

(defun gtk-tearoff-menu-item-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @return{A new @class{gtk-tearoff-menu-item} widget.}
  @subheading{Warning}
    @sym{gtk-tearoff-menu-item-new} has been deprecated since version 3.4 and
    should not be used in newly-written code. @class{gtk-tearoff-menu-item} is
    deprecated and should not be used in newly written code.

  @short{Creates a new @class{gtk-tearoff-menu-item} widget.}"
  (make-instance 'gtk-tearoff-menu-item))

(export 'gtk-tearoff-menu-item-new)

;;; --- End of file gtk.tearoff-menu-item.lisp ---------------------------------
