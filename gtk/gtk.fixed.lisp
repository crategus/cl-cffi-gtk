;;; ----------------------------------------------------------------------------
;;; gtk.fixed.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkFixed
;;;
;;; A container which allows you to position widgets at fixed coordinates
;;;
;;; Synopsis
;;;
;;;     GtkFixed
;;;
;;;     gtk_fixed_new
;;;     gtk_fixed_put
;;;     gtk_fixed_move
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkFixed
;;;
;;; Implemented Interfaces
;;;
;;; GtkFixed implements AtkImplementorIface and GtkBuildable.
;;;
;;; Child Properties
;;;
;;;   "x"                        gint                  : Read / Write
;;;   "y"                        gint                  : Read / Write
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFixed
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkFixed" 'gtk-fixed))

(define-g-object-class "GtkFixed" gtk-fixed
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_fixed_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-fixed 'type)
 "@version{2013-5-18}
  @begin{short}
    The @class{gtk-fixed} widget is a container which can place child widgets at
    fixed positions and with fixed sizes, given in pixels. @class{gtk-fixed}
    performs no automatic layout management.
  @end{short}

  For most applications, you should not use this container! It keeps you from
  having to learn about the other GTK+ containers, but it results in broken
  applications. With @class{gtk-fixed}, the following things will result in
  truncated text, overlapping widgets, and other display bugs:
  @begin{itemize}
    @begin{item}
      Themes, which may change widget sizes.
    @end{item}
    @begin{item}
      Fonts other than the one you used to write the app will of course
      change the size of widgets containing text; keep in mind that users may
      use a larger font because of difficulty reading the default, or they
      may be using Windows or the framebuffer port of GTK+, where different
      fonts are available.
    @end{item}
    @begin{item}
      Translation of text into other languages changes its size. Also,
      display of non-English text will use a different font in many cases.
    @end{item}
  @end{itemize}
  In addition, the fixed widget cannot properly be mirrored in right-to-left
  languages such as Hebrew and Arabic. i. e. normally GTK+ will flip the
  interface to put labels to the right of the thing they label, but it cannot
  do that with @class{gtk-fixed}. So your application will not be usable in
  right-to-left languages.

  Finally, fixed positioning makes it kind of annoying to add/remove GUI
  elements, since you have to reposition all the other elements. This is a
  long-term maintenance problem for your application.

  If you know none of these things are an issue for your application, and
  prefer the simplicity of @class{gtk-fixed}, by all means use the widget. But
  you should be aware of the tradeoffs.
  @begin[Child Property Details]{dictionary}
    @subheading{The \"x\" child property}
      @code{\"x\"} of type @code{:int} (Read / Write)@br{}
      x position of child widget.@br{}
      Default value: 0

    @subheading{The @code{\"y\"} child property}
      @code{\"y\"} of type @code{:int} (Read / Write)@br{}
      y position of child widget.@br{}
      Default value: 0
  @end{dictionary}")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkFixed"
                       gtk-fixed-child-x
                       "x" "gint" t t t)

(define-child-property "GtkFixed"
                       gtk-fixed-child-y
                       "y" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-fixed-child-x atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-fixed-child-x 'function)
 "@version{2013-3-5}
  @begin{short}
    Accessor of the child property @code{\"x\"} of the @class{gtk-fixed} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-fixed-child-y atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-fixed-child-y 'function)
 "@version{2013-3-5}
  @begin{short}
    Accessor of the child property @code{\"y\"} of the @class{gtk-fixed} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-fixed-new))

(defun gtk-fixed-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @return{A new @class{gtk-fixed} container.}
  Creates a new @class{gtk-fixed} container."
  (make-instance 'gtk-fixed))

(export 'gtk-fixed-new)

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_put ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_fixed_put" gtk-fixed-put) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[fixed]{a @class{gtk-fixed} container}
  @argument[widget]{the widget to add}
  @argument[x]{the horizontal position to place the @arg{widget} at}
  @argument[y]{the vertical position to place the @arg{widget} at}
  Adds a widget to a @class{gtk-fixed} container at the given position."
  (fixed g-object)
  (widget g-object)
  (x :int)
  (y :int))

(export 'gtk-fixed-put)

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_move ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_fixed_move" gtk-fixed-move) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[fixed]{a @class{gtk-fixed} container}
  @argument[widget]{the child widget}
  @argument[x]{the horizontal position to move the @arg{widget} to}
  @argument[y]{the vertical position to move the @arg{widget} to}
  Moves a child of a @class{gtk-fixed} container to the given position."
  (fixed (g-object gtk-fixed))
  (widget g-object)
  (x :int)
  (y :int))

(export 'gtk-fixed-move)

;;; --- End of file gtk.fixed.lisp ---------------------------------------------
