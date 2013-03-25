;;; ----------------------------------------------------------------------------
;;; gtk.separator-tool-item.lisp
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSeparatorToolItem
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-separator-tool-item 'type)
 "@version{2013-3-23}
  @begin{short}
    A GtkSeparatorItem is a GtkToolItem that separates groups of other
    GtkToolItems. Depending on the theme, a GtkSeparatorToolItem will often look
    like a vertical line on horizontally docked toolbars.
  @end{short}

  If the GtkToolbar child property \"expand\" is TRUE and the property \"draw\"
  is FALSE, a GtkSeparatorToolItem will act as a \"spring\" that forces other
  items to the ends of the toolbar.

  Use gtk_separator_tool_item_new() to create a new GtkSeparatorToolItem.

  @see-slot{gtk-separator-tool-item-draw}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "draw"
                                               'gtk-separator-tool-item) 't)
 "The @code{\"draw\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether the separator is drawn, or just blank. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-separator-tool-item-draw atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-separator-tool-item-draw 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"draw\"} of the @class{gtk-separator-tool-item}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_separator_tool_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-separator-tool-item-new))

(defun gtk-separator-tool-item-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @return{The new GtkSeparatorToolItem.}
  @short{Create a new GtkSeparatorToolItem}

  Since 2.4"
  (make-instance 'gtk-separator-tool-item-new))

(export 'gtk-separator-tool-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_separator_tool_item_set_draw ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-separator-tool-item-set-draw))

(defun gtk-separator-tool-item-set-draw (item draw)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[item]{a GtkSeparatorToolItem}
  @argument[draw]{whether item is drawn as a vertical line}
  @begin{short}
    Whether item is drawn as a vertical line, or just blank. Setting this to
    FALSE along with gtk_tool_item_set_expand() is useful to create an item that
    forces following items to the end of the toolbar.
  @end{short}

  Since 2.4"
  (setf (gtk-separator-tool-item-draw item) draw))

(export 'gtk-separator-tool-item-set-draw)

;;; ----------------------------------------------------------------------------
;;; gtk_separator_tool_item_get_draw ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-separator-tool-item-get-draw))

(defun gtk-separator-tool-item-get-draw (item)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[item]{a GtkSeparatorToolItem}
  @return{TRUE if item is drawn as a line, or just blank.}
  @begin{short}
    Returns whether item is drawn as a line, or just blank. See
    gtk_separator_tool_item_set_draw().
  @end{short}

  Since 2.4"
  (gtk-separator-tool-item-draw item))

(export 'gtk-separator-tool-item-get-draw)

;;; --- End of file gtk.separator-tool-item.lisp -------------------------------
