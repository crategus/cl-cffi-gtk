;;; ----------------------------------------------------------------------------
;;; gtk.separator-tool-item.lisp
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
;;; GtkSeparatorToolItem
;;;
;;;     A toolbar item that separates groups of other toolbar items
;;;
;;; Types and Values
;;;
;;;     GtkSeparatorToolItem
;;;
;;; Functions
;;;
;;;     gtk_separator_tool_item_new
;;;     gtk_separator_tool_item_set_draw
;;;     gtk_separator_tool_item_get_draw
;;;
;;; Properties
;;;
;;;     gboolean  draw    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkToolItem
;;;                         ╰── GtkSeparatorToolItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSeparatorToolItem implements AtkImplementorIface, GtkBuildable and
;;;     GtkActivatable.
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-separator-tool-item 'type)
 "@version{2014-1-26}
  @begin{short}
    A @sym{gtk-separator-tool-item} is a @class{gtk-tool-item} that separates
    groups of other @class{gtk-tool-item}'s. Depending on the theme, a
    @sym{gtk-separator-tool-item} will often look like a vertical line on
    horizontally docked toolbars.
  @end{short}

  If the @class{gtk-toolbar} child property @code{\"expand\"} is @em{true} and
  the property @code{\"draw\"} is @code{nil}, a @sym{gtk-separator-tool-item}
  will act as a \"spring\" that forces other items to the ends of the toolbar.

  Use the function @fun{gtk-separator-tool-item-new} to create a new
  @sym{gtk-separator-tool-item}.
  @see-function{gtk-separator-tool-item-new}
  @see-function{gtk-separator-tool-item-set-draw}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "draw"
                                               'gtk-separator-tool-item) 't)
 "The @code{draw} property of type @code{:boolean} (Read / Write) @br{}
  Whether the separator is drawn, or just blank. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-separator-tool-item-draw atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-separator-tool-item-draw 'function)
 "@version{2014-1-26}
  @begin{short}
    Accessor of the slot @slot[gtk-separator-tool-item]{draw} of the
    @class{gtk-separator-tool-item} class.
  @end{short}
  @see-class{gtk-separator-tool-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_separator_tool_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-separator-tool-item-new))

(defun gtk-separator-tool-item-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-1-26}
  @return{The new @class{gtk-separator-tool-item} widget.}
  @short{Create a new @class{gtk-separator-tool-item} widget.}
  @see-class{gtk-separator-tool-item}"
  (make-instance 'gtk-separator-tool-item-new))

(export 'gtk-separator-tool-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_separator_tool_item_set_draw ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-separator-tool-item-set-draw))

(defun gtk-separator-tool-item-set-draw (item draw)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-26}
  @argument[item]{a @class{gtk-separator-tool-item} widget}
  @argument[draw]{whether item is drawn as a vertical line}
  @begin{short}
    Whether item is drawn as a vertical line, or just blank.
  @end{short}
  Setting this to @code{nil} along with the function 
  @fun{gtk-tool-item-set-expand} is useful to create an item that forces
  following items to the end of the toolbar.
  @see-class{gtk-separator-tool-item}
  @see-function{gtk-separator-tool-item-get-draw}
  @see-function{gtk-tool-item-set-expand}"
  (setf (gtk-separator-tool-item-draw item) draw))

(export 'gtk-separator-tool-item-set-draw)

;;; ----------------------------------------------------------------------------
;;; gtk_separator_tool_item_get_draw ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-separator-tool-item-get-draw))

(defun gtk-separator-tool-item-get-draw (item)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-26}
  @argument[item]{a @class{gtk-separator-tool-item} widget}
  @return{@em{True} if @arg{item} is drawn as a line, or just blank.}
  @begin{short}
    Returns whether item is drawn as a line, or just blank.
  @end{short}
  See the function @fun{gtk-separator-tool-item-set-draw}.
  @see-class{gtk-separator-tool-item}
  @see-function{gtk-separator-tool-item-set-draw}"
  (gtk-separator-tool-item-draw item))

(export 'gtk-separator-tool-item-get-draw)

;;; --- End of file gtk.separator-tool-item.lisp -------------------------------
