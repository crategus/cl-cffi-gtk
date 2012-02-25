;;; ----------------------------------------------------------------------------
;;; gtk.separator-menu-item.lisp
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
;;; GtkSeparatorMenuItem
;;; 
;;; A separator used in menus
;;; 
;;; Synopsis
;;; 
;;;     GtkSeparatorMenuItem
;;;
;;;     gtk_separator_menu_item_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkMenuItem
;;;                                  +----GtkSeparatorMenuItem
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkSeparatorMenuItem implements AtkImplementorIface, GtkBuildable and
;;; GtkActivatable.
;;;
;;; Description
;;; 
;;; The GtkSeparatorMenuItem is a separator used to group items within a menu.
;;; It displays a horizontal line with a shadow to make it appear sunken into
;;; the interface.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSeparatorMenuItem
;;; 
;;; struct GtkSeparatorMenuItem;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSeparatorMenuItem" gtk-separator-menu-item
  (:superclass gtk-menu-item
   :export t
   :interfaces ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
   :type-initializer "gtk_separator_menu_item_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_separator_menu_item_new ()
;;; 
;;; GtkWidget * gtk_separator_menu_item_new (void);
;;; 
;;; Creates a new GtkSeparatorMenuItem.
;;; 
;;; Returns :
;;;     a new GtkSeparatorMenuItem.
;;; ----------------------------------------------------------------------------

(defun gtk-separator-menu-item-new ()
  (make-instance 'gtk-separator-menu-item))

(export 'gtk-separator-menu-item)

;;; --- End of file gtk.separator-menu-item.lisp -------------------------------
