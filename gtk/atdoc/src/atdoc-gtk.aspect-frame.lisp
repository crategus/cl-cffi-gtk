;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.aspect-frame.lisp
;;;
;;; Documentation strings for the library GTK+.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

(in-package :gtk)

;;; --- gtk-aspect-frame -------------------------------------------------------

(setf (documentation 'gtk-aspect-frame 'type)
 "@version{2013-1-29}
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
  @see-slot{gtk-aspect-frame-yalign}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "obey-child" 'gtk-aspect-frame) 't)
 "The @code{\"obey-child\"} property of type @code{gboolean} (Read / Write)@br{}
  Force aspect ratio to match that of the frame's child.@br{}
  Default value: @arg{true}")

(setf (documentation (atdoc:get-slot-from-name "ratio" 'gtk-aspect-frame) 't)
 "The @code{\"ratio\"} property of type @code{gfloat} (Read / Write)@br{}
  Aspect ratio if obey_child is FALSE.@br{}
  Allowed values: @code{[0.0001,10000]}@br{}
  Default value: @code{1}")

(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-aspect-frame) 't)
 "The @code{\"xalign\"} property of type @code{gfloat} (Read / Write)@br{}
  X alignment of the child.@br{}
  Allowed values: @code{[0,1]}
  Default value: @code{0.5}")

(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-aspect-frame) 't)
 "The @code{\"yalign\"} property of type @code{gfloat} (Read / Write)@br{}
  Y alignment of the child.@br{}
  Allowed values: @code{[0,1]}@br{}
  Default value: @code{0.5}")

;;; --- End of Properties ------------------------------------------------------

;;; --- gtk-aspect-frame-new ---------------------------------------------------

(setf (documentation 'gtk-aspect-frame-new 'function)
 "@version{2013-1-29}
  @argument[label]{Label text.}
  @argument[xalign]{Horizontal alignment of the child within the allocation of
    the @sym{gtk-aspect-frame}. This ranges from 0.0 (left aligned) to 1.0
    (right aligned)}
  @argument[yalign]{Vertical alignment of the child within the allocation of the
    @sym{gtk-aspect-frame}. This ranges from 0.0 (left aligned) to 1.0 (right
    aligned)}
  @argument[ratio]{The desired aspect ratio.}
  @argument[obey-child]{If TRUE, ratio is ignored, and the aspect ratio is taken
    from the requistion of the child.}
  @return{The new @sym{gtk-aspect-frame} instance.}
  @begin{short}
    Create a new @class{gtk-aspect-frame} widget.
  @end{short}")

;;; --- gtk-aspect-frame-set ---------------------------------------------------

(setf (documentation 'gtk-frame-set 'function)
 "@version{2013-1-29}
  @argument[aspect-frame]{a @class{gtk-aspect-frame} instance}
  @argument[xalign]{Horizontal alignment of the child within the allocation of
    the @class{gtk-aspect-frame}. This ranges from 0.0 (left aligned) to 1.0
    (rightaligned)}
  @argument[yalign]{Vertical alignment of the child within the allocation of the
    @class{gtk-aspect-frame}. This ranges from 0.0 (left aligned) to 1.0 (right
    aligned)}
  @argument[ratio]{The desired aspect ratio.}
  @argument[obey-child]{If TRUE, ratio is ignored, and the aspect ratio is taken
    from the requistion of the child.}
  @begin{short}
    Set parameters for an existing GtkAspectFrame.
  @end{short}")

;;; --- End of file atdoc-gtk.aspect-frame.lisp --------------------------------
