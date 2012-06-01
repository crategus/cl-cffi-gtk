;;; ----------------------------------------------------------------------------
;;; gtk.menu-bar.lisp
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
;;;﻿
;;; GtkMenuBar
;;; 
;;; A subclass of GtkMenuShell which holds GtkMenuItem widgets
;;;     
;;; Synopsis
;;; 
;;;     GtkMenuBar
;;;     
;;;     gtk_menu_bar_new
;;;     gtk_menu_bar_new_from_model
;;;     
;;;     GtkPackDirection
;;;     
;;;     gtk_menu_bar_set_pack_direction
;;;     gtk_menu_bar_get_pack_direction
;;;     gtk_menu_bar_set_child_pack_direction
;;;     gtk_menu_bar_get_child_pack_direction
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkMenuShell
;;;                            +----GtkMenuBar
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkMenuBar implements AtkImplementorIface and GtkBuildable.
;;; 
;;; Properties
;;; 
;;;   "child-pack-direction"     GtkPackDirection      : Read / Write
;;;   "pack-direction"           GtkPackDirection      : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "internal-padding"         gint                  : Read
;;;   "shadow-type"              GtkShadowType         : Read
;;; 
;;; Description
;;; 
;;; The GtkMenuBar is a subclass of GtkMenuShell which contains one or more
;;; GtkMenuItems. The result is a standard menu bar which can hold many menu
;;; items.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "child-pack-direction" property
;;; 
;;;   "child-pack-direction"     GtkPackDirection      : Read / Write
;;; 
;;; The child pack direction of the menubar. It determines how the widgets
;;; contained in child menuitems are arranged.
;;; 
;;; Default value: GTK_PACK_DIRECTION_LTR
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "pack-direction" property
;;; 
;;;   "pack-direction"           GtkPackDirection      : Read / Write
;;; 
;;; The pack direction of the menubar. It determines how menuitems are arranged
;;; in the menubar.
;;; 
;;; Default value: GTK_PACK_DIRECTION_LTR
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "internal-padding" style property
;;; 
;;;   "internal-padding"         gint                  : Read
;;; 
;;; Amount of border space between the menubar shadow and the menu items.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "shadow-type" style property
;;; 
;;;   "shadow-type"              GtkShadowType         : Read
;;; 
;;; Style of bevel around the menubar.
;;; 
;;; Default value: GTK_SHADOW_OUT
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuBar
;;; 
;;; struct GtkMenuBar;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuBar" gtk-menu-bar
  (:superclass gtk-menu-shell
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_menu_bar_get_type")
  ((child-pack-direction
    gtk-menu-bar-child-pack-direction
    "child-pack-direction" "GtkPackDirection" t t)
   (pack-direction
    gtk-menu-bar-pack-direction
    "pack-direction" "GtkPackDirection" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_new ()
;;; 
;;; GtkWidget * gtk_menu_bar_new (void);
;;; 
;;; Creates a new GtkMenuBar
;;; 
;;; Returns :
;;;     the new menu bar, as a GtkWidget
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-bar-new))

(defun gtk-menu-bar-new ()
  (make-instance 'gtk-menu-bar))

(export 'gtk-menu-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_new_from_model ()
;;; 
;;; GtkWidget * gtk_menu_bar_new_from_model (GMenuModel *model);
;;; 
;;; Creates a new GtkMenuBar and populates it with menu items and submenus
;;; according to model.
;;; 
;;; The created menu items are connected to actions found in the
;;; GtkApplicationWindow to which the menu bar belongs - typically by means of
;;; being contained within the GtkApplicationWindows widget hierarchy.
;;; 
;;; model :
;;;     a GMenuModel
;;; 
;;; Returns :
;;;     a new GtkMenuBar
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkPackDirection
;;; 
;;; typedef enum {
;;;   GTK_PACK_DIRECTION_LTR,
;;;   GTK_PACK_DIRECTION_RTL,
;;;   GTK_PACK_DIRECTION_TTB,
;;;   GTK_PACK_DIRECTION_BTT
;;; } GtkPackDirection;
;;; 
;;; Determines how widgets should be packed insided menubars and menuitems
;;; contained in menubars.
;;; 
;;; GTK_PACK_DIRECTION_LTR
;;;     Widgets are packed left-to-right
;;; 
;;; GTK_PACK_DIRECTION_RTL
;;;     Widgets are packed right-to-left
;;; 
;;; GTK_PACK_DIRECTION_TTB
;;;     Widgets are packed top-to-bottom
;;; 
;;; GTK_PACK_DIRECTION_BTT
;;;     Widgets are packed bottom-to-top
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPackDirection" gtk-pack-direction
  (:export t
   :type-initializer "gtk_pack_direction_get_type")
  (:ltr 0)
  (:rtl 1)
  (:ttb 2)
  (:btt 3))

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_set_pack_direction ()
;;; 
;;; void gtk_menu_bar_set_pack_direction (GtkMenuBar *menubar,
;;;                                       GtkPackDirection pack_dir);
;;; 
;;; Sets how items should be packed inside a menubar.
;;; 
;;; menubar :
;;;     a GtkMenuBar
;;; 
;;; pack_dir :
;;;     a new GtkPackDirection
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-bar-set-pack-direction))

(defun gtk-menu-bar-set-pack-direction (menubar pack-dir)
  (setf (gtk-menu-bar-pack-direction menubar) pack-dir))

(export 'gtk-menu-bar-set-pack-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_get_pack_direction ()
;;; 
;;; GtkPackDirection gtk_menu_bar_get_pack_direction (GtkMenuBar *menubar);
;;; 
;;; Retrieves the current pack direction of the menubar. See
;;; gtk_menu_bar_set_pack_direction().
;;; 
;;; menubar :
;;;     a GtkMenuBar
;;; 
;;; Returns :
;;;     the pack direction
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-bar-get-pack-direction))

(defun gtk-menu-bar-get-pack-direction (menubar)
  (gtk-menu-bar-pack-direction menubar))

(export 'gtk-menu-bar-get-pack-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_set_child_pack_direction ()
;;; 
;;; void gtk_menu_bar_set_child_pack_direction (GtkMenuBar *menubar,
;;;                                             GtkPackDirection child_pack_dir)
;;; 
;;; Sets how widgets should be packed inside the children of a menubar.
;;; 
;;; menubar :
;;;     a GtkMenuBar
;;; 
;;; child_pack_dir :
;;;     a new GtkPackDirection
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-bar-set-child-pack-direction))

(defun gtk-menu-bar-set-child-pack-direction (menubar child-pack-dir)
  (setf (gtk-menu-bar-child-pack-direction menubar) child-pack-dir))

(export 'gtk-menu-bar-set-child-pack-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_get_child_pack_direction ()
;;; 
;;; GtkPackDirection gtk_menu_bar_get_child_pack_direction (GtkMenuBar *menubar)
;;; 
;;; Retrieves the current child pack direction of the menubar. See
;;; gtk_menu_bar_set_child_pack_direction().
;;; 
;;; menubar :
;;;     a GtkMenuBar
;;; 
;;; Returns :
;;;     the child pack direction
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-bar-get-child-pack-direction))

(defun gtk-menu-bar-get-child-pack-direction (menubar)
  (gtk-menu-bar-child-pack-direction menubar))

(export 'gtk-menu-bar-get-child-pack-direction)

;;; --- End of file gtk.menu-bar.lisp ------------------------------------------
