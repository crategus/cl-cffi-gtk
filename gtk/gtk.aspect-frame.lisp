;;; ----------------------------------------------------------------------------
;;; gtk.aspect-frame.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
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
;;; GtkAspectFrame
;;;
;;; A frame that constrains its child to a particular aspect ratio
;;;
;;; Synopsis
;;;
;;;     GtkAspectFrame
;;;
;;;     gtk_aspect_frame_new
;;;     gtk_aspect_frame_set
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAspectFrame
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAspectFrame" gtk-aspect-frame
  (:superclass gtk-frame
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_aspect_frame_get_type")
  ((obey-child
    gtk-aspect-frame-obey-child
    "obey-child" "gboolean" t t)
   (ratio
    gtk-aspect-frame-ratio
    "ratio" "gfloat" t t)
   (xalign
    gtk-aspect-frame-xalign
    "xalign" "gfloat" t t)
   (yalign
    gtk-aspect-frame-yalign
    "yalign" "gfloat" t t)))

;;; --- gtk-aspect-frame -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-aspect-frame 'type)
 "@version{2013-5-17}
  @begin{short}
    The @sym{gtk-aspect-frame} is useful when you want pack a widget so that it
    can resize but always retains the same aspect ratio.
  @end{short}
  For instance, one might be drawing a small preview of a larger image.
  @sym{gtk-aspect-frame} derives from @class{gtk-frame}, so it can draw a label
  and a frame around the child. The frame will be \"shrink-wrapped\" to the size
  of the child.
  @see-slot{gtk-aspect-frame-obey-child}
  @see-slot{gtk-aspect-frame-ratio}
  @see-slot{gtk-aspect-frame-xalign}
  @see-slot{gtk-aspect-frame-yalign}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "obey-child"
                                               'gtk-aspect-frame) 't)
 "The @code{\"obey-child\"} property of type @code{:boolean} (Read / Write)@br{}
  Force aspect ratio to match that of the frame's child.@br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ratio" 'gtk-aspect-frame) 't)
 "The @code{\"ratio\"} property of type @code{:float} (Read / Write)@br{}
  Aspect ratio if @code{\"obey-child\"} is @code{nil}. @br{}
  Allowed values: [0.0001, 10000.0] @br{}
  Default value: 1.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-aspect-frame) 't)
 "The @code{\"xalign\"} property of type @code{:float} (Read / Write)@br{}
  X alignment of the child.@br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-aspect-frame) 't)
 "The @code{\"yalign\"} property of type @code{:float} (Read / Write)@br{}
  Y alignment of the child. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-aspect-frame-obey-child --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-aspect-frame-obey-child atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-aspect-frame-obey-child 'function)
 "@version{2013-5-17}
  Accessor of the slot @code{\"obey-child\"} of the @class{gtk-aspect-frame}
  class.")

;;; --- gtk-aspect-frame-ratio -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-aspect-frame-ratio atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-aspect-frame-ratio 'function)
 "@version{2013-5-17}
  Accessor of the slot @code{\"ratio\"} of the @class{gtk-aspect-frame} class.")

;;; --- gtk-aspect-frame-xalign ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-aspect-frame-xalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-aspect-frame-xalign 'function)
 "@version{2013-5-17}
  Accessor of the slot @code{\"xalign\"} of the @class{gtk-aspect-frame}
  class.")

;;; --- gtk-aspect-frame-yalign ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-aspect-frame-yalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-aspect-frame-yalign 'function)
 "@version{2013-5-17}
  Accessor of the slot @code{\"yalign\"} of the @class{gtk-aspect-frame}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_aspect_frame_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-aspect-frame-new))

(defun gtk-aspect-frame-new (label xalign yalign ratio obey-child)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-17}
  @argument[label]{label text}
  @argument[xalign]{horizontal alignment of the child within the allocation of
    the @class{gtk-aspect-frame}. This ranges from 0.0 (left aligned) to 1.0
    (right aligned)}
  @argument[yalign]{vertical alignment of the child within the allocation of the
    @class{gtk-aspect-frame}. This ranges from 0.0 (left aligned) to 1.0 (right
    aligned)}
  @argument[ratio]{the desired aspect ratio}
  @argument[obey-child]{If @em{true}, @arg{ratio} is ignored, and the aspect
    ratio is taken from the requistion of the child}
  @return{The new @class{gtk-aspect-frame} container.}
  Create a new @class{gtk-aspect-frame} container."
  (make-instance 'gtk-aspect-frame
                 :label label
                 :xalign xalign
                 :yalign yalign
                 :ratio ratio
                 :obey-child obey-child))

(export 'gtk-aspect-frame-new)

;;; ----------------------------------------------------------------------------
;;; gtk_aspect_frame_set ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-aspect-frame-set))

(defun gtk-aspect-frame-set (aspect-frame xalign yalign ratio obey-child)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-17}
  @argument[aspect-frame]{a @class{gtk-aspect-frame} container}
  @argument[xalign]{horizontal alignment of the child within the allocation of
    the @class{gtk-aspect-frame} container. This ranges from 0.0 (left aligned)
    to 1.0 (right aligned)}
  @argument[yalign]{vertical alignment of the child within the allocation of the
    @class{gtk-aspect-frame} container. This ranges from 0.0 (left aligned) to
    1.0 (right aligned)}
  @argument[ratio]{the desired aspect ratio}
  @argument[obey-child]{If @em{true}, @arg{ratio} is ignored, and the aspect
    ratio is taken from the requistion of the child}
  Set parameters for an existing @class{gtk-aspect-frame} container."
  (setf (gtk-aspect-frame-xalign aspect-frame) xalign
        (gtk-aspect-frame-yalign aspect-frame) yalign
        (gtk-aspect-frame-ratio aspect-frame) ratio
        (gtk-aspect-frame-obey-child aspect-frame) obey-child))

(export 'gtk-aspect-frame-set)

;;; --- End of file gtk.aspect-frame.lisp --------------------------------------
