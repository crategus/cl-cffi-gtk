;;; ----------------------------------------------------------------------------
;;; gtk.aspect-frame.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     A frame that constrains its child to a particular aspect ratio
;;;
;;; Types and Values
;;;
;;;     GtkAspectFrame
;;;
;;; Functions
;;;
;;;     gtk_aspect_frame_new
;;;     gtk_aspect_frame_set
;;;
;;; Properties
;;;
;;;     gboolean  obey-child  Read / Write
;;;       gfloat  ratio       Read / Write
;;;       gfloat  xalign      Read / Write
;;;       gfloat  yalign      Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;        ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkFrame
;;;                        ╰── GtkAspectFrame
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAspectFrame implements AtkImplementorIface and GtkBuildable.
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-aspect-frame 'type)
 "@version{2020-4-30}
  @begin{short}
    The @sym{gtk-aspect-frame} is useful when you want pack a widget so that it
    can resize but always retains the same aspect ratio.
  @end{short}
  For instance, one might be drawing a small preview of a larger image.
  @sym{gtk-aspect-frame} derives from @class{gtk-frame}, so it can draw a label
  and a frame around the child. The frame will be \"shrink-wrapped\" to the
  size of the child.
  @begin[CSS nodes]{dictionary}
    @sym{gtk-aspect-frame} uses a CSS node with name @code{frame}.
  @end{dictionary}
  @see-slot{gtk-aspect-frame-obey-child}
  @see-slot{gtk-aspect-frame-ratio}
  @see-slot{gtk-aspect-frame-xalign}
  @see-slot{gtk-aspect-frame-yalign}
  @see-class{gtk-frame}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-aspect-frame-obey-child --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "obey-child"
                                               'gtk-aspect-frame) 't)
 "The @code{obey-child} property of type @code{:boolean} (Read / Write) @br{}
  Force aspect ratio to match that of the frame's child. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-aspect-frame-obey-child atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-aspect-frame-obey-child 'function)
 "@version{2020-4-30}
  @syntax[]{(gtk-aspect-frame-obey-child object) => obey-child}
  @syntax[]{(setf (gtk-aspect-frame-obey-child object) obey-child)}
  @argument[object]{a @class{gtk-aspect-frame} container}
  @argument[obey-child]{a boolean whether to force the aspect ratio}
  @begin{short}
    Accessor of the @slot[gtk-aspect-frame]{obey-child} slot of the
    @class{gtk-aspect-frame} class.
  @end{short}

  WHether to force the aspect ratio to match that of the frame's child.
  @see-class{gtk-aspect-frame}")

;;; --- gtk-aspect-frame-ratio -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ratio" 'gtk-aspect-frame) 't)
 "The @code{ratio} property of type @code{:float} (Read / Write) @br{}
  Aspect ratio if @code{obey-child} is @em{false}. @br{}
  Allowed values: [0.0001, 10000.0] @br{}
  Default value: 1.0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-aspect-frame-ratio atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-aspect-frame-ratio 'function)
 "@version{2020-4-30}
  @syntax[]{(gtk-aspect-frame-ratio object) => ratio}
  @syntax[]{(setf (gtk-aspect-frame-ratio object) ratio)}
  @argument[object]{a @class{gtk-aspect-frame} container}
  @argument[ratio]{a float with an aspect ratio}
  @begin{short}
    Accessor of the @slot[gtk-aspect-frame]{ratio} slot of the
    @class{gtk-aspect-frame} class.
  @end{short}

  The aspect ratio if the @slot[gtk-aspect-frame]{obey-child} property is
  @em{false}. Allowed values are in [0.0001, 10000.0]. The default value is 1.0.
  @see-class{gtk-aspect-frame}")

;;; --- gtk-aspect-frame-xalign ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-aspect-frame) 't)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  The x alignment of the child. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-aspect-frame-xalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-aspect-frame-xalign 'function)
 "@version{2020-4-30}
  @syntax[]{(gtk-aspect-frame-xalign object) => xalign}
  @syntax[]{(setf (gtk-aspect-frame-xalign object) xalign)}
  @argument[object]{a @class{gtk-aspect-frame} container}
  @argument[xalign]{a float with the x alignment of the child}
  @begin{short}
    Accessor of the @slot[gtk-aspect-frame]{xalign} slot of the
    @class{gtk-aspect-frame} class.
  @end{short}

  The x alignment of the child in the aspect frame container.
  @see-class{gtk-aspect-frame}")

;;; --- gtk-aspect-frame-yalign ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-aspect-frame) 't)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  The y alignment of the child. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-aspect-frame-yalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-aspect-frame-yalign 'function)
 "@version{2020-4-30}
  @syntax[]{(gtk-aspect-frame-yalign object) => yalign}
  @syntax[]{(setf (gtk-aspect-frame-yalign object) yalign)}
  @argument[object]{a @class{gtk-aspect-frame} container}
  @argument[yalign]{a float with the y alignment of the child}
  @begin{short}
    Accessor of the @slot[gtk-aspect-frame]{yalign} slot of the
    @class{gtk-aspect-frame} class.
  @end{short}

  The y alignment of the child in the aspect frame container.
  @see-class{gtk-aspect-frame}")

;;; ----------------------------------------------------------------------------
;;; gtk_aspect_frame_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-aspect-frame-new))

(defun gtk-aspect-frame-new (label xalign yalign ratio obey-child)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-30}
  @argument[label]{a string with the label text}
  @argument[xalign]{a float with the horizontal alignment of the child within
    the allocation of the aspect frame, this ranges from 0.0 (left aligned) to
    1.0 (right aligned)}
  @argument[yalign]{a float with the vertical alignment of the child within the
    allocation of the aspect frame, this ranges from 0.0 (left aligned) to 1.0
    (right aligned)}
  @argument[ratio]{a float with the desired aspect ratio}
  @argument[obey-child]{if @em{true}, @arg{ratio} is ignored, and the aspect
    ratio is taken from the requistion of the child}
  @return{The new @class{gtk-aspect-frame} container.}
  @begin{short}
    Create a new aspect frame container.
  @end{short}
  @see-class{gtk-aspect-frame}"
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

(defun gtk-aspect-frame-set (aspect-frame xalign yalign ratio obey-child)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-30}
  @argument[aspect-frame]{a @class{gtk-aspect-frame} container}
  @argument[xalign]{a float with the horizontal alignment of the child within
    the allocation of the aspect frame container, this ranges from 0.0
    (left aligned) to 1.0 (right aligned)}
  @argument[yalign]{a float with the vertical alignment of the child within the
    allocation of the aspect frame container, this ranges from 0.0
    (left aligned) to 1.0 (right aligned)}
  @argument[ratio]{a float with the desired aspect ratio}
  @argument[obey-child]{if @em{true}, @arg{ratio} is ignored, and the aspect
    ratio is taken from the requistion of the child}
  @begin{short}
    Set parameters for an existing aspect frame container.
  @end{short}
  @see-class{gtk-aspect-frame}"
  (setf (gtk-aspect-frame-xalign aspect-frame) xalign
        (gtk-aspect-frame-yalign aspect-frame) yalign
        (gtk-aspect-frame-ratio aspect-frame) ratio
        (gtk-aspect-frame-obey-child aspect-frame) obey-child))

(export 'gtk-aspect-frame-set)

;;; --- End of file gtk.aspect-frame.lisp --------------------------------------
