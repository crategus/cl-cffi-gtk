;;; ----------------------------------------------------------------------------
;;; gtk.stack-switcher.lisp
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
;;; GtkStackSwitcher
;;;
;;;     A controller for GtkStack
;;;
;;; Types and Values
;;;
;;;     GtkStackSwitcher
;;;
;;; Functions
;;;
;;;     gtk_stack_switcher_new
;;;     gtk_stack_switcher_set_stack                       Accessor
;;;     gtk_stack_switcher_get_stack                       Accessor
;;;
;;; Properties
;;;
;;;     gint      icon-size  Read / Write
;;;     GtkStack  stack      Read / Write / Construct
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkStackSwitcher
;;;
;;; Implemented Interfaces
;;;
;;;     GtkStackSwitcher implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkStackSwitcher
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkStackSwitcher" gtk-stack-switcher
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_stack_switcher_get_type")
  ((icon-size
    gtk-stack-switcher-icon-size
    "icon-size" "gint" t t)
   (stack
    gtk-stack-switcher-stack
    "stack" "GtkStack" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-stack-switcher 'type)
 "@version{2021-12-8}
  @begin{short}
    The @sym{gtk-stack-switcher} widget acts as a controller for a
    @class{gtk-stack} widget. It shows a row of buttons to switch between
    the various pages of the associated stack widget.
  @end{short}

  @image[stackswitcher]{}

  All the content for the buttons comes from the child properties of the
  @class{gtk-stack} widget. The button visibility in a @sym{gtk-stack-switcher}
  widget is controlled by the visibility of the child widget in the
  @class{gtk-stack} widget.

  It is possible to associate multiple @sym{gtk-stack-switcher} widgets with
  the same @class{gtk-stack} widget.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-stack-switcher} implementation has a single CSS node named
    @code{stackswitcher} and @code{.stack-switcher} style class. When
    circumstances require it, the @sym{gtk-stack-switcher} widget adds the
    @code{.needs-attention} style class to the widgets representing the stack
    pages.
  @end{dictionary}
  @see-slot{gtk-stack-switcher-icon-size}
  @see-slot{gtk-stack-switcher-stack}
  @see-class{gtk-stack}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-stack-switcher-icon-size -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size" 'gtk-stack-switcher)
                     't)
 "The @code{icon-size} property of type @code{:int} (Read / Write) @br{}
  Use this property to change the size of the image displayed when a stack
  switcher is displaying icons. Since 3.20 @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-switcher-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-switcher-icon-size 'function)
 "@version{2021-12-8}
  @syntax[]{(gtk-stack-switcher-icon-size object) => size}
  @syntax[]{(setf (gtk-stack-switcher-icon-size object) size)}
  @argument[object]{a @class{gtk-stack-switcher} widget}
  @argument[size]{an integer with size of the image}
  @begin{short}
    Accessor of the @slot[gtk-stack-switcher]{icon-size} slot of the
    @class{gtk-stack-switcher} class.
  @end{short}

  Use the @slot[gtk-stack-switcher]{icon-size} property to change the size of
  the image displayed when a stack switcher is displaying icons.

  Since 3.20
  @see-class{gtk-stack-switcher}")

;;; --- gtk-stack-switcher-stack -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stack" 'gtk-stack-switcher)
                     't)
 "The @code{stack} property of type @class{gtk-stack} (Read / Write) @br{}
  The stack to control. @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-switcher-stack atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-switcher-stack 'function)
 "@version{2021-12-8}
  @syntax[]{(gtk-stack-switcher-stack object) => stack}
  @syntax[]{(setf (gtk-stack-switcher-stack object) stack)}
  @argument[object]{a @class{gtk-stack-switcher} widget}
  @argument[stack]{a @class{gtk-stack} widget}
  @begin{short}
    Accessor of the @slot[gtk-stack-switcher]{stack} slot of the
    @class{gtk-stack-switcher} class.
  @end{short}

  The @sym{gtk-stack-switcher-stack} slot access function retrieves the stack.
  The @sym{(setf gtk-stack-switcher-stack)} slot access function sets the stack
  to control.
  @see-class{gtk-stack-switcher}
  @see-class{gtk-stack}")

;;; ----------------------------------------------------------------------------
;;; gtk_stack_switcher_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-stack-switcher-new))

(defun gtk-stack-switcher-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-12-8}
  @return{The new @class{gtk-stack-switcher} widget.}
  @short{Creates a new stack switcher.}
  @see-class{gtk-stack-switcher}"
  (make-instance 'gtk-stack-switcher))

(export 'gtk-stack-switcher-new)

;;; --- End of file gtk.stack-switcher.lisp ------------------------------------
