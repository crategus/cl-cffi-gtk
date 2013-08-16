;;; ----------------------------------------------------------------------------
;;; gtk.arrow.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkArrow
;;;
;;; Displays an arrow
;;;
;;; Synopsis
;;;
;;;     GtkArrow
;;;
;;;     gtk_arrow_new
;;;     gtk_arrow_set
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkArrow
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkArrow" gtk-arrow
  (:superclass gtk-misc
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_arrow_get_type")
  ((arrow-type
    gtk-arrow-arrow-type
    "arrow-type" "GtkArrowType" t t)
   (shadow-type
    gtk-arrow-shadow-type
    "shadow-type" "GtkShadowType" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-arrow 'type)
 "@version{2013-8-16}
  @begin{short}
    @sym{gtk-arrow} should be used to draw simple arrows that need to point in
    one of the four cardinal directions: up, down, left, or right. The style of
    the arrow can be one of shadow in, shadow out, etched in, or etched out.
    Note that these directions and style types may be ammended in versions of
    GTK+ to come.
  @end{short}

  @sym{gtk-arrow} will fill any space alloted to it, but since it is inherited
  from @class{gtk-misc}, it can be padded and/or aligned, to fill exactly the
  space the programmer desires.

  Arrows are created with a call to the function @fun{gtk-arrow-new}. The
  direction or style of an arrow can be changed after creation by using the
  function @fun{gtk-arrow-set}.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"arrow-scaling\" style property}
      @code{\"arrow-scaling\"} of type @code{:float} (Read) @br{}
      Amount of space used up by arrow. @br{}
      Allowed values: [0,1] @br{}
      Default value: 0.7
  @end{dictionary}
  @see-slot{gtk-arrow-arrow-type}
  @see-slot{gtk-arrow-shadow-type}
  @see-function{gtk-arrow-new}
  @see-function{gtk-arrow-set}
  @see-class{gtk-misc}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "arrow-type" 'gtk-arrow) 't)
 "The @code{\"arrow-type\"} property of type @symbol{gtk-arrow-type}
  (Read / Write) @br{}
  The direction the arrow should point. @br{}
  Default value: @code{:right}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type" 'gtk-arrow) 't)
 "The @code{\"shadow-type\"} property of type @symbol{gtk-shadow-type}
  (Read / Write) @br{}
  Appearance of the shadow surrounding the arrow. @br{}
  Default value: @code{:out}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-arrow-arrow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-arrow-arrow-type 'function)
 "@version{2013-8-16}
  Accessor of the slot @code{\"arrow-type\"} of the @class{gtk-arrow} class.
  @see-class{gtk-arrow}
  @see-function{gtk-arrow-set}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-arrow-shadow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-arrow-shadow-type 'function)
 "@version{2013-8-16}
  Accessor of the slot @code{\"shadow-type\"} of the @class{gtk-arrow} class.
  @see-class{gtk-arrow}
  @see-function{gtk-arrow-set}")

;;; ----------------------------------------------------------------------------
;;; gtk_arrow_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-arrow-new))

(defun gtk-arrow-new (arrow-type shadow-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-16}
  @argument[arrow-type]{a valid @symbol{gtk-arrow-type}}
  @argument[shadow-type]{a valid @symbol{gtk-shadow-type}}
  @return{The new @class{gtk-arrow} widget.}
  @short{Creates a new @class{gtk-arrow} widget.}
  @see-class{gtk-arrow}
  @see-symbol{gtk-arrow-type}
  @see-symbol{gtk-shadow-type}"
  (make-instance 'gtk-arrow
                 :arrow-type arrow-type
                 :shadow-type shadow-type))

(export 'gtk-arrow-new)

;;; ----------------------------------------------------------------------------
;;; gtk_arrow_set ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-arrow-set))

(defun gtk-arrow-set (arrow arrow-type shadow-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-16}
  @argument[arrow]{a widget of type @class{gtk-arrow}}
  @argument[arrow-type]{a valid @symbol{gtk-arrow-type}}
  @argument[shadow-type]{a valid @symbol{gtk-shadow-type}}
  @short{Sets the direction and style of the @class{gtk-arrow} widget.}
  @see-class{gtk-arrow}
  @see-symbol{gtk-arrow-type}
  @see-symbol{gtk-shadow-type}"
  (setf (gtk-arrow-arrow-type arrow) arrow-type
        (gtk-arrow-shadow-type arrow) shadow-type))

(export 'gtk-arrow-set)

;;; --- End of file gtk.arrow.lisp ---------------------------------------------
