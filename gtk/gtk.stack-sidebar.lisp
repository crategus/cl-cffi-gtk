;;; ----------------------------------------------------------------------------
;;; gtk.stack-sidebar.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2021 Dieter Kaiser
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
;;; GtkStackSidebar
;;;
;;;     An automatic sidebar widget
;;;
;;; Types and Values
;;;
;;;     GtkStackSidebar
;;;
;;; Functions
;;;
;;;     gtk_stack_sidebar_new
;;;     gtk_stack_sidebar_set_stack                        Accessor
;;;     gtk_stack_sidebar_get_stack                        Accessor
;;;
;;; Properties
;;;
;;;     GtkStack  stack  Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkStackSidebar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkStackSidebar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkStackSidebar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkStackSidebar" gtk-stack-sidebar
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_stack_sidebar_get_type")
  ((stack
    gtk-stack-sidebar-stack
    "stack" "GtkStack" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-stack-sidebar 'type)
 "@version{*2021-12-16}
  @begin{short}
    A @sym{gtk-stack-sidebar} widget enables you to quickly and easily provide
    a consistent \"sidebar\" object for your user interface.
  @end{short}

  @image[stack-sidebar]{}

  In order to use a @sym{gtk-stack-sidebar} widget, you simply use a
  @class{gtk-stack} widget to organize your UI flow, and add the sidebar to your
  sidebar area. You can use the @fun{gtk-stack-sidebar-stack} slot access
  function to connect the @sym{gtk-stack-sidebar} widget to the
  @class{gtk-stack} widget.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-stack-sidebar} implementation has a single CSS node with name
    stacksidebar and @code{.sidebar} style class. When circumstances require it,
    the @sym{gtk-stack-sidebar} widget adds the @code{.needs-attention} style
    class to the widgets representing the stack pages.
  @end{dictionary}
  @see-slot{gtk-stack-sidebar-stack}
  @see-class{gtk-stack}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-stack-sidebar-stack -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stack" 'gtk-stack-sidebar)
                     't)
 "The @code{stack} property of type @class{gtk-stack} (Read / Write) @br{}
  Associated stack for this stack sidebar.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-sidebar-stack atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-sidebar-stack 'function)
 "@version{2021-12-8}
  @syntax[]{(gtk-stack-sidebar-stack object) => stack}
  @syntax[]{(setf (gtk-stack-sidebar-stack object) stack)}
  @argument[object]{a @class{gtk-stack-sidebar} widget}
  @argument[stack]{a @class{gtk-stack} widget}
  @begin{short}
    Accessor of the @slot[gtk-stack-sidebar]{stack} slot of the
    @class{gtk-stack-sidebar} class.
  @end{short}

  The @sym{gtk-stack-sidebar-stack} slot access function retrieves the stack.
  The @sym{(setf gtk-stack-sidebar-stack)} slot access function sets the stack
  associated with this stack sidebar.

  The stack sidebar will automatically update according to the order (packing)
  and items within the given stack.
  @see-class{gtk-stack-sidebar}
  @see-class{gtk-stack}")

;;; ----------------------------------------------------------------------------
;;; gtk_stack_sidebar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-stack-sidebar-new))

(defun gtk-stack-sidebar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-12-8}
  @return{The new @class{gtk-stack-sidebar} widget.}
  @short{Creates a new stack sidebar.}
  @see-class{gtk-stack-sidebar}"
  (make-instance 'gtk-stack-sidebar))

(export 'gtk-stack-sidebar-new)

;;; --- End of file gtk.stack-sidebar.lisp -------------------------------------
