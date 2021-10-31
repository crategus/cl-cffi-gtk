;;; ----------------------------------------------------------------------------
;;; gtk.separator-tool-item.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     gtk_separator_tool_item_set_draw                   Accessor
;;;     gtk_separator_tool_item_get_draw                   Accessor
;;;
;;; Properties
;;;
;;;     gboolean    draw    Read / Write
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
 "@version{*2021-10-31}
  @begin{short}
    A @sym{gtk-separator-tool-item} widget is a @class{gtk-tool-item} widget
    that separates groups of other @class{gtk-tool-item} widgets.
  @end{short}
  Depending on the theme, a separator tool item will often look like a vertical
  line on horizontally docked toolbars.

  If the @code{expand} child property of the @class{gtk-toolbar} widget is
  @em{true} and the @code{draw} property is @em{false}, a separator tool item
  will act as a \"spring\" that forces other items to the ends of the toolbar.

  Use the @fun{gtk-separator-tool-item-new} function to create a new
  @sym{gtk-separator-tool-item} widget.
  @see-slot{gtk-separator-tool-item-draw}
  @see-class{gtk-tool-item}")

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
 "@version{2021-10-31}
  @syntax[]{(gtk-separator-tool-item-draw object) => draw)}
  @syntax[]{(setf (gtk-separator-tool-item-draw object) draw)}
  @argument[object]{a @class{gtk-separator-tool-item} widget}
  @argument[draw]{a boolean whether @arg{object} is drawn as a vertical line}
  @begin{short}
    Accessor of the @slot[gtk-separator-tool-item]{draw} slot of the
    @class{gtk-separator-tool-item} class.
  @end{short}

  The @sym{gtk-separator-tool-item-draw} slot access function returns whether
  the separator tool item is drawn as a line, or just blank. The
  @sym{(setf gtk-separator-tool-item-draw)} slot access function returns whether
  item is drawn as a vertical line, or just blank.

  Setting this to @em{false} along with the @fun{gtk-tool-item-expand} function
  is useful to create an item that forces following items to the end of the
  toolbar.
  @see-class{gtk-separator-tool-item}
  @see-function{gtk-tool-item-expand}")

;;; ----------------------------------------------------------------------------
;;; gtk_separator_tool_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-separator-tool-item-new))

(defun gtk-separator-tool-item-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-10-31}
  @return{The new @class{gtk-separator-tool-item} widget.}
  @begin{short}
    Create a new separator tool item.
  @end{short}
  @see-class{gtk-separator-tool-item}"
  (make-instance 'gtk-separator-tool-item-new))

(export 'gtk-separator-tool-item-new)

;;; --- End of file gtk.separator-tool-item.lisp -------------------------------
