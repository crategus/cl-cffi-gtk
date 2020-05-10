;;; ----------------------------------------------------------------------------
;;; gtk.stack-switcher.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2020 Dieter Kaiser
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
 "@version{2020-5-8}
  @begin{short}
    The @sym{gtk-stack-switcher} widget acts as a controller for a
    @class{gtk-stack} container. It shows a row of buttons to switch between
    the various pages of the associated stack widget.
  @end{short}

  @image[stackswitcher]{}

  All the content for the buttons comes from the child properties of the
  @class{gtk-stack} container. The button visibility in a
  @sym{gtk-stack-switcher} widget is controlled by the visibility of the child
  in the @class{gtk-stack} container.

  It is possible to associate multiple @sym{gtk-stack-switcher} widgets with
  the same @class{gtk-stack} container.

  The @sym{gtk-stack-switcher} widget was added in GTK+ 3.10.
  @begin[CSS nodes]{dictionary}
    @sym{gtk-stack-switcher} has a single CSS node named @code{stackswitcher}
    and style class @code{.stack-switcher}.

    When circumstances require it, @sym{gtk-stack-switcher} adds the
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
  Use the @code{icon-size} property to change the size of the image displayed
  when a @class{gtk-stack-switcher} is displaying icons. Since 3.20 @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-switcher-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-switcher-icon-size 'function)
 "@version{2020-5-8}
  @syntax[]{(gtk-stack-switcher-icon-size object) => icon-size}
  @syntax[]{(setf (gtk-stack-switcher-icon-size object) icon-size)}
  @argument[object]{a @class{gtk-stack-switcher} widget}
  @argument[icon-size]{an integer with size of the image}
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
  The @class{gtk-stack}. Since 3.10 @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-switcher-stack atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-switcher-stack 'function)
 "@version{2020-5-8}
  @syntax[]{(gtk-stack-switcher-stack object) => stack}
  @syntax[]{(setf (gtk-stack-switcher-stack object) stack)}
  @argument[object]{a @class{gtk-stack-switcher} widget}
  @argument[stack]{a @class{gtk-stack} container}
  @begin{short}
    Accessor of the @slot[gtk-stack-switcher]{stack} slot of the
    @class{gtk-stack-switcher} class.
  @end{short}

  The slot access function @sym{gtk-stack-switcher-stack} retrieves the stack.
  The slot access function @sym{(setf gtk-stack-switcher-stack)} sets the stack
  to control.

  Since 3.10
  @see-class{gtk-stack-switcher}")

;;; ----------------------------------------------------------------------------
;;; gtk_stack_switcher_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-stack-switcher-new))

(defun gtk-stack-switcher-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @return{The new @class{gtk-stack-switcher} widget.}
  @short{Creates a new stack switcher.}

  Since 3.10
  @see-class{gtk-stack-switcher}"
  (make-instance 'gtk-stack-switcher))

(export 'gtk-stack-switcher-new)

;;; --- End of file gtk.stack-switcher.lisp ------------------------------------
