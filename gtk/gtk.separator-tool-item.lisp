;;; ----------------------------------------------------------------------------
;;; gtk.separator-tool-item.lisp
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
;;;
;;; GtkSeparatorToolItem
;;; 
;;; A toolbar item that separates groups of other toolbar items
;;;     
;;; Synopsis
;;; 
;;;     GtkSeparatorToolItem
;;;
;;;     gtk_separator_tool_item_new
;;;     gtk_separator_tool_item_set_draw
;;;     gtk_separator_tool_item_get_draw
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkToolItem
;;;                                  +----GtkSeparatorToolItem
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkSeparatorToolItem implements AtkImplementorIface, GtkBuildable and
;;; GtkActivatable.
;;;
;;; Properties
;;; 
;;;   "draw"                     gboolean              : Read / Write
;;; 
;;; Description
;;; 
;;; A GtkSeparatorItem is a GtkToolItem that separates groups of other
;;; GtkToolItems. Depending on the theme, a GtkSeparatorToolItem will often look
;;; like a vertical line on horizontally docked toolbars.
;;; 
;;; If the GtkToolbar child property "expand" is TRUE and the property "draw" is
;;; FALSE, a GtkSeparatorToolItem will act as a "spring" that forces other items
;;; to the ends of the toolbar.
;;; 
;;; Use gtk_separator_tool_item_new() to create a new GtkSeparatorToolItem.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "draw" property
;;; 
;;;   "draw"                     gboolean              : Read / Write
;;; 
;;; Whether the separator is drawn, or just blank.
;;; 
;;; Default value: TRUE
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSeparatorToolItem
;;; 
;;; struct GtkSeparatorToolItem;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSeparatorToolItem" gtk-separator-tool-item
  (:superclass gtk-tool-item
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_separator_tool_item_get_type")
  ((draw
    gtk-separator-tool-item-draw
    "draw" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_separator_tool_item_new ()
;;; 
;;; GtkToolItem * gtk_separator_tool_item_new (void);
;;; 
;;; Create a new GtkSeparatorToolItem
;;; 
;;; Returns :
;;;     the new GtkSeparatorToolItem
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-separator-tool-item-new))

(defun gtk-separator-tool-item-new ()
  (make-instance 'gtk-separator-tool-item-new))

(export 'gtk-separator-tool-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_separator_tool_item_set_draw ()
;;; 
;;; void gtk_separator_tool_item_set_draw (GtkSeparatorToolItem *item,
;;;                                        gboolean draw);
;;; 
;;; Whether item is drawn as a vertical line, or just blank. Setting this to
;;; FALSE along with gtk_tool_item_set_expand() is useful to create an item that
;;; forces following items to the end of the toolbar.
;;; 
;;; item :
;;;     a GtkSeparatorToolItem
;;; 
;;; draw :
;;;     whether item is drawn as a vertical line
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-separator-tool-item-set-draw))

(defun gtk-separator-tool-item-set-draw (item draw)
  (setf (gtk-separator-tool-item-draw item) draw))

(export 'gtk-separator-tool-item-set-draw)

;;; ----------------------------------------------------------------------------
;;; gtk_separator_tool_item_get_draw ()
;;; 
;;; gboolean gtk_separator_tool_item_get_draw (GtkSeparatorToolItem *item);
;;; 
;;; Returns whether item is drawn as a line, or just blank. See
;;; gtk_separator_tool_item_set_draw().
;;; 
;;; item :
;;;     a GtkSeparatorToolItem
;;; 
;;; Returns :
;;;     TRUE if item is drawn as a line, or just blank.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-separator-tool-item-get-draw))

(defun gtk-separator-tool-item-get-draw (item)
  (gtk-separator-tool-item-draw item))

(export 'gtk-separator-tool-item-get-draw)

;;; --- End of file gtk.separator-tool-item.lisp -------------------------------
