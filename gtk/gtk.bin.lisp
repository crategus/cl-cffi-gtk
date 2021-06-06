;;; ----------------------------------------------------------------------------
;;; gtk.bin.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
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
;;; GtkBin
;;;
;;;     A container with just one child
;;;
;;; Types and Values
;;;
;;;     GtkBin
;;;
;;; Functions
;;;
;;;     gtk_bin_get_child
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ├── GtkWindow
;;;                     ├── GtkActionBar
;;;                     ├── GtkAlignment
;;;                     ├── GtkComboBox
;;;                     ├── GtkFrame
;;;                     ├── GtkButton
;;;                     ├── GtkMenuItem
;;;                     ├── GtkEventBox
;;;                     ├── GtkExpander
;;;                     ├── GtkFlowBoxChild
;;;                     ├── GtkHandleBox
;;;                     ├── GtkListBoxRow
;;;                     ├── GtkToolItem
;;;                     ├── GtkOverlay
;;;                     ├── GtkScrolledWindow
;;;                     ├── GtkPopover
;;;                     ├── GtkRevealer
;;;                     ├── GtkSearchBar
;;;                     ├── GtkStackSidebar
;;;                     ╰── GtkViewport
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBin implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkBin
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkBin" gtk-bin
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_bin_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-bin 'type)
 "@version{2025-5-25}
  @begin{short}
    The @sym{gtk-bin} widget is a container with just one child.
  @end{short}
  It is not very useful itself, but it is useful for deriving subclasses, since
  it provides common code needed for handling a single child widget.

  Many GTK widgets are subclasses of the @sym{gtk-bin} class, including the
  @class{gtk-window}, @class{gtk-button}, @class{gtk-frame}, or
  @class{gtk-scrolled-window} classes.
  @see-class{gtk-window}
  @see-class{gtk-button}
  @see-class{gtk-frame}
  @see-class{gtk-scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; gtk_bin_get_child () -> gtk-bin-child
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_bin_get_child" gtk-bin-child) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-12}
  @argument[bin]{a @class{gtk-bin} widget}
  @return{The @class{gtk-widget} child widget of @arg{bin}.}
  @begin{short}
    Gets the child widget of the bin widget, or @code{nil} if @arg{bin}
    contains no child widget.
  @end{short}
  @see-class{gtk-bin}
  @see-class{gtk-widget}"
  (bin (g-object gtk-bin)))

(export 'gtk-bin-child)

;;; --- End of file gtk.bin.lisp -----------------------------------------------
