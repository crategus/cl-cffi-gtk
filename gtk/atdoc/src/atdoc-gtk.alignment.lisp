;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.alignment.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; Documentation strings for the library GTK+.
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

;;; --- gtk-alignment ----------------------------------------------------------

(setf (documentation 'gtk-alignment 'type)
 "@version{2013-1-29}
  @begin{short}
    The @sym{gtk-alignment} widget controls the alignment and size of its child
    widget.
  @end{short}
  It has four settings: xscale, yscale, xalign, and yalign.

  The scale settings are used to specify how much the child widget should
  expand to fill the space allocated to the GtkAlignment. The values can range
  from 0 (meaning the child doesn't expand at all) to 1 (meaning the child
  expands to fill all of the available space).
  
  The align settings are used to place the child widget within the available
  area. The values range from 0 (top or left) to 1 (bottom or right). Of
  course, if the scale settings are both set to 1, the alignment settings have
  no effect.
  @begin[Note]{dictionary}
    Note that the desired effect can in most cases be achieved by using the
    \"halign\", \"valign\" and \"margin\" properties on the child widget, so
    @sym{gtk-alignment} should not be used in new code.
  @end{dictionary}
  @see-slot{gtk-alignment-bottom-padding}
  @see-slot{gtk-alignment-left-padding}
  @see-slot{gtk-alignment-right-padding}
  @see-slot{gtk-alignment-top-padding}
  @see-slot{gtk-alignment-xalign}
  @see-slot{gtk-alignment-xscale}
  @see-slot{gtk-alignment-yalign}
  @see-slot{gtk-alignment-yscale}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "bottom-padding" 'gtk-alignment) 't)
 "The @code{\"bottom-padding\"} property of type @code{guint}
  (Read / Write)@br{}
  The padding to insert at the bottom of the widget.@br{}
  Allowed values: @code{= G_MAXINT}@br{}
  Default value: @code{0}@br{}
  Since 2.4")

(setf (documentation (atdoc:get-slot-from-name "left-padding" 'gtk-alignment) 't)
 "The @code{\"left-padding\"} property of type @code{guint} (Read / Write)@br{}
  The padding to insert at the left of the widget.@br{}
  Allowed values: @code{<= G_MAXINT}@br{}
  Default value: @code{0}@br{}
  Since 2.4")

(setf (documentation (atdoc:get-slot-from-name "right-padding" 'gtk-alignment) 't)
 "The @code{\"right-padding\"} property of type @code{guint} (Read / Write)@br{}
  The padding to insert at the right of the widget.@br{}
  Allowed values: @code{<= G_MAXINT}
  Default value: @code{0}@br{}
  Since 2.4")

(setf (documentation (atdoc:get-slot-from-name "top-padding" 'gtk-alignment) 't)
 "The @code{\"top-padding\"} property of type @code{guint} (Read / Write)@br{}
  The padding to insert at the top of the widget.@br{}
  Allowed values: @code{<= G_MAXINT}
  Default value: @code{0}
  Since 2.4")

(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-alignment) 't)
 "The @code{\"xalign\"} property of type @code{gfloat} (Read / Write)@br{}
  Horizontal position of child in available space. 0.0 is left aligned, 1.0 is
  right aligned.@br{}
  Allowed values: @code{[0,1]}
  Default value: 0.5")

(setf (documentation (atdoc:get-slot-from-name "xscale" 'gtk-alignment) 't)
 "The @code{\"xscale\"} property @code{gfloat} (Read / Write)@br{}
  If available horizontal space is bigger than needed for the child, how much
  of it to use for the child. 0.0 means none, 1.0 means all.@br{}
  Allowed values: [0,1]@br{}
  Default value: @code{1}")

(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-alignment) 't)
 "The @code{\"yalign\"} property of type @code{gfloat} (Read / Write)@br{}
  Vertical position of child in available space. 0.0 is top aligned, 1.0 is
  bottom aligned.@br{}
  Allowed values: @code{[0,1]}
  Default value: @code{0.5}")

(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-alignment) 't)
 "The @code{\"yscale\"} property of type @code{gfloat} (Read / Write)@br{}
  If available vertical space is bigger than needed for the child, how much of
  it to use for the child. 0.0 means none, 1.0 means all.@br{}
  Allowed values: @code{[0,1]}@br{}
  Default value: @code{1}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-alignment-bottom-padding -------------------------------------------

(setf (gethash 'gtk-alignment-bottom-padding atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-alignment-bottom-padding 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"bottom-padding\" of the @class{gtk-widget} class.
  @end{short}")

;;; --- gtk-alignment-left-padding ---------------------------------------------

(setf (gethash 'gtk-alignment-left-padding atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-alignment-left-padding 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"left-padding\" of the @class{gtk-widget} class.
  @end{short}")

;;; --- gtk-alignment-right-padding --------------------------------------------

(setf (gethash 'gtk-alignment-right-padding atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-alignment-right-padding 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"right-padding\" of the @class{gtk-widget} class.
  @end{short}")

;;; --- gtk-alignment-top-padding ----------------------------------------------

(setf (gethash 'gtk-alignment-top-padding atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-alignment-top-padding 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"top-padding\" of the @class{gtk-widget} class.
  @end{short}")

;;; --- gtk-alignment-xalign ---------------------------------------------------

(setf (gethash 'gtk-alignment-xalign atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-alignment-xalign 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"xalign\" of the @class{gtk-widget} class.
  @end{short}")

;;; --- gtk-alignment-yalign ---------------------------------------------------

(setf (gethash 'gtk-alignment-yalign atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-alignment-yalign 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"yalign\" of the @class{gtk-widget} class.
  @end{short}")

;;; --- gtk-alignment-xscale ---------------------------------------------------

(setf (gethash 'gtk-alignment-xscale atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-alignment-xscale 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"xscale\" of the @class{gtk-widget} class.
  @end{short}")

;;; --- gtk-alignment-yscale ---------------------------------------------------

(setf (gethash 'gtk-alignment-yscale atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-alignment-yscale 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"yscale\" of the @class{gtk-widget} class.
  @end{short}")

;;; --- End of Accessors -------------------------------------------------------

;;; --- gtk-alignment-new ------------------------------------------------------

(setf (documentation 'gtk-alignment-new 'function)
 "@version{2013-1-29}
  @argument[xalign]{the horizontal alignment of the child widget, from 0 (left)
    to 1 (right).}
  @argument[yalign]{the vertical alignment of the child widget, from 0 (top)
    to 1 (bottom).}
  @argument[scale]{the amount that the child widget expands horizontally to fill
    up unused space, from 0 to 1. A value of 0 indicates that the child widget
    should never expand. A value of 1 indicates that the child widget will
    expand to fill all of the space allocated for the @class{gtk-alignment}.}
  @argument[yscale]{the amount that the child widget expands vertically to fill
    up unused space, from 0 to 1. The values are similar to xscale.}
  @return{The new @class{gtk-alignment} widget.}
  @begin{short}
    Creates a new @class{gtk-alignment} widget.
  @end{short}")

;;; --- gtk-alignment-set ------------------------------------------------------

(setf (documentation 'gtk-alignment-set 'function)
 "@version{2013-1-29}
  @argument[alignment]{a @class{gtk-alignment} instance.}
  @argument[xalign]{the horizontal alignment of the child widget, from 0 (left)
    to 1 (right).}
  @argument[yalign]{the vertical alignment of the child widget, from 0 (top)
    to 1 (bottom).}
  @argument[xscale]{the amount that the child widget expands horizontally to
    fill up unused space, from 0 to 1. A value of 0 indicates that the child
    widget should never expand. A value of 1 indicates that the child widget
    will expand to fill all of the space allocated for the
    @class{gtk-alignment}.}
  @argument[yscale]{the amount that the child widget expands vertically to fill
    up unused space, from 0 to 1. The values are similar to xscale.}
  @begin{short}
    Sets the @class{gtk-alignment} values.
  @end{short}")

;;; --- gtk-alignment-get-padding ----------------------------------------------

(setf (documentation 'gtk-alignment-get-padding 'function)
 "@version{2013-1-29}
  @argument[alignment]{a @class{gtk-alignment} instance}
  @argument[padding-top]{location to store the padding for the top of the
    widget, or NULL}
  @argument[padding-bottom]{location to store the padding for the bottom of the
    widget, or NULL}
  @argument[padding-left]{location to store the padding for the left of the
    widget, or NULL}
  @argument[padding-right]{location to store the padding for the right of the
    widget, or NULL}
  @begin{short}
    Gets the padding on the different sides of the widget.
  @end{short}
  See @class{gtk-alignment-set-padding}.

  Since 2.4")

;;; --- gtk-alignment-set-padding ----------------------------------------------

(setf (documentation 'gtk-alignment-set-padding 'function)
 "@version{2013-1-29}
  @argument[alignment]{a @class{gtk-alignment} instance}
  @argument[padding-top]{the padding at the top of the widget}
  @argument[padding-bottom]{the padding at the bottom of the widget}
  @argument[padding-left]{the padding at the left of the widget}
  @argument[padding-right]{the padding at the right of the widget.}
  @begin{short}
    Sets the padding on the different sides of the widget.
  @end{short}
  The padding adds blank space to the sides of the widget. For instance, this
  can be used to indent the child widget towards the right by adding padding on
  the left.

  Since 2.4")

;;; --- End of file atdoc-gtk.alignment.lisp -----------------------------------
