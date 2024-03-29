;;; ----------------------------------------------------------------------------
;;; gtk.alignment.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkAlignment
;;;
;;;     A widget which controls the alignment and size of its child
;;;
;;; Types and Values
;;;
;;;     GtkAlignment
;;;
;;; Functions
;;;
;;;     GtkWidget*   gtk_alignment_new
;;;          void    gtk_alignment_set
;;;          void    gtk_alignment_get_padding
;;;          void    gtk_alignment_set_padding
;;;
;;; Properties
;;;
;;;         guint    bottom-padding    Read / Write
;;;         guint    left-padding      Read / Write
;;;         guint    right-padding     Read / Write
;;;         guint    top-padding       Read / Write
;;;        gfloat    xalign            Read / Write
;;;        gfloat    xscale            Read / Write
;;;        gfloat    yalign            Read / Write
;;;        gfloat    yscale            Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkAlignment
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAlignment implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAlignment
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAlignment" gtk-alignment
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_alignment_get_type")
  ((bottom-padding
    gtk-alignment-bottom-padding
    "bottom-padding" "guint" t t)
   (left-padding
    gtk-alignment-left-padding
    "left-padding" "guint" t t)
   (right-padding
    gtk-alignment-right-padding
    "right-padding" "guint" t t)
   (top-padding
    gtk-alignment-top-padding
    "top-padding" "guint" t t)
   (xalign
    gtk-alignment-xalign
    "xalign" "gfloat" t t)
   (xscale
    gtk-alignment-xscale
    "xscale" "gfloat" t t)
   (yalign
    gtk-alignment-yalign
    "yalign" "gfloat" t t)
   (yscale
    gtk-alignment-yscale
    "yscale" "gfloat" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-alignment 'type)
 "@version{*2021-7-25}
  @begin{short}
    The @sym{gtk-alignment} widget controls the alignment and size of its child
    widget.
  @end{short}
  It has four settings: @code{xscale}, @code{yscale}, @code{xalign},
  and @code{yalign}.

  The scale settings are used to specify how much the child widget should
  expand to fill the space allocated to the alignment. The values can range
  from 0.0, meaning the child does not expand at all, to 1.0, meaning the
  child expands to fill all of the available space.

  The align settings are used to place the child widget within the available
  area. The values range from 0.0, top or left, to 1.0, bottom or right. Of
  course, if the scale settings are both set to 1.0, the alignment settings
  have no effect.
  @begin[Warning]{dictionary}
    The @sym{gtk-alignment} widget has been deprecated in 3.14 and should not
    be used in newly written code. The desired effect can be achieved by using
    the @slot[gtk-widget]{halign}, @slot[gtk-widget]{valign} and
    @slot[gtk-widget]{margin} properties on the child widget.
  @end{dictionary}
  @see-slot{gtk-alignment-bottom-padding}
  @see-slot{gtk-alignment-left-padding}
  @see-slot{gtk-alignment-right-padding}
  @see-slot{gtk-alignment-top-padding}
  @see-slot{gtk-alignment-xalign}
  @see-slot{gtk-alignment-xscale}
  @see-slot{gtk-alignment-yalign}
  @see-slot{gtk-alignment-yscale}
  @see-class{gtk-widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-alignment-bottom-padding -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "bottom-padding"
                                               'gtk-alignment) 't)
 "The @code{bottom-padding} property of type @code{:uint} (Read / Write) @br{}
  The padding to insert at the bottom of the child widget. @br{}
  Allowed values: < @code{G_MAXINT} @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-alignment-bottom-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-alignment-bottom-padding 'function)
 "@version{*2021-7-25}
  @begin{short}
    Accessor of the @slot[gtk-alignment]{bottom-padding} slot of the
    @class{gtk-alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-bottom-padding} has been deprecated since
    version 3.14 and should not be used in newly written code. Use the function
    @fun{gtk-widget-margin-bottom} instead.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-function{gtk-widget-margin-bottom}")

;;; --- gtk-alignment-left-padding ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "left-padding"
                                               'gtk-alignment) 't)
 "The @code{left-padding} property of type @code{:uint} (Read / Write) @br{}
  The padding to insert at the left of the child widget. @br{}
  Allowed values: < @code{G_MAXINT} @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-alignment-left-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-alignment-left-padding 'function)
 "@version{*2021-7-25}
  @begin{short}
    Accessor of the @slot[gtk-alignment]{left-padding} slot of the
    @class{gtk-alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-left-padding} has been deprecated since
    version 3.14 and should not be used in newly written code. Use the function
    @fun{gtk-widget-margin-start} instead.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-function{gtk-widget-margin-start}")

;;; --- gtk-alignment-right-padding --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "right-padding"
                                               'gtk-alignment) 't)
 "The @code{right-padding} property of type @code{:uint} (Read / Write) @br{}
  The padding to insert at the right of the child widget. @br{}
  Allowed values: < @code{G_MAXINT} @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-alignment-right-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-alignment-right-padding 'function)
 "@version{*2021-7-25}
  @begin{short}
    Accessor of the @slot[gtk-alignment]{right-padding} slot of the
    @class{gtk-alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-right-padding} has been deprecated since
    version 3.14 and should not be used in newly written code. Use the function
    @fun{gtk-widget-margin-end} instead.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-function{gtk-widget-margin-end}")

;;; --- gtk-alignment-top-padding ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "top-padding"
                                               'gtk-alignment) 't)
 "The @code{top-padding} property of type @code{:uint} (Read / Write) @br{}
  The padding to insert at the top of the child widget. @br{}
  Allowed values: < @code{G_MAXINT} @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-alignment-top-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-alignment-top-padding 'function)
 "@version{*2021-7-25}
  @begin{short}
    Accessor of the @slot[gtk-alignment]{top-padding} slot of the
    @class{gtk-alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-top-padding} has been deprecated since
    version 3.14 and should not be used in newly written code. Use the function
    @fun{gtk-widget-margin-top} instead.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-function{gtk-widget-margin-top}")

;;; --- gtk-alignment-xalign ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-alignment) 't)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  Horizontal position of the child widget in available space. 0.0 is left
  aligned, 1.0 is right aligned. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-alignment-xalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-alignment-xalign 'function)
 "@version{*2021-7-25}
  @begin{short}
    Accessor of the @slot[gtk-alignment]{xalign} slot of the
    @class{gtk-alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-xalign} has been deprecated since version
    3.14 and should not be used in newly written code. Use the function
    @fun{gtk-widget-halign} instead.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-function{gtk-widget-halign}")

;;; --- gtk-alignment-xscale ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xscale" 'gtk-alignment) 't)
 "The @code{xscale} property @code{:float} (Read / Write) @br{}
  If available horizontal space is bigger than needed for the child widget, how
  much of it to use for the child. 0.0 means none, 1.0 means all. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 1.0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-alignment-xscale atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-alignment-xscale 'function)
 "@version{*2021-7-25}
  @begin{short}
    Accessor of the @slot[gtk-alignment]{xscale} slot of the
    @class{gtk-alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-xscale} has been deprecated since version
    3.14 and should not be used in newly written code. Use the function
    @fun{gtk-widget-hexpand} instead.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-function{gtk-widget-hexpand}")

;;; --- gtk-alignment-yalign ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-alignment) 't)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  Vertical position of the child widget in available space. 0.0 is top aligned,
  1.0 is bottom aligned. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-alignment-yalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-alignment-yalign 'function)
 "@version{*2021-7-25}
  @begin{short}
    Accessor of the @slot[gtk-alignment]{yalign} slot of the
    @class{gtk-alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-yalign} has been deprecated since version
    3.14 and should not be used in newly written code. Use the function
    @fun{gtk-widget-valign} instead.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-function{gtk-widget-valign}")

;;; --- gtk-alignment-yscale ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "yscale" 'gtk-alignment) 't)
 "The @code{yscale} property of type @code{:float} (Read / Write) @br{}
  If available vertical space is bigger than needed for the child widget, how
  much of it to use for the child. 0.0 means none, 1.0 means all. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 1.0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-alignment-yscale atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-alignment-yscale 'function)
 "@version{*2021-7-25}
  @begin{short}
    Accessor of the @slot[gtk-alignment]{yscale} slot of the
    @class{gtk-alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-yscale} has been deprecated since version
    3.14 and should not be used in newly written code. Use the function
    @fun{gtk-widget-vexpand} instead.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-function{gtk-widget-vexpand}")

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-alignment-new))

(defun gtk-alignment-new (xalign yalign xscale yscale)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-9}
  @argument[xalign]{a float with the horizontal alignment of the child widget,
    from 0.0 (left) to 1.0 (right)}
  @argument[yalign]{a float with the vertical alignment of the child widget,
    from 0.0 (top) to 1.0 (bottom)}
  @argument[xscale]{a float with the amount that the child widget expands
    horizontally to fill up unused space, from 0.0 to 1.0, a value of 0.0
    indicates that the child widget should never expand, a value of 1.0
    indicates that the child widget will expand to fill all of the space
    allocated for the alignment}
  @argument[yscale]{a float with the amount that the child widget expands
    vertically to fill up unused space, from 0.0 to 1.0, he values are similar
    to @arg{xscale}}
  @return{The new @class{gtk-alignment} widget.}
  @short{Creates a new alignment.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-new} has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gtk-widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-class{gtk-widget}"
  (make-instance 'gtk-alignment
                 :xalign xalign
                 :yalign yalign
                 :xscale xscale
                 :yscale yscale))

(export 'gtk-alignment-new)

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_set ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-alignment-set))

(defun gtk-alignment-set (alignment xalign yalign xscale yscale)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-9}
  @argument[alignment]{a @class{gtk-alignment} widget}
  @argument[xalign]{a float with the horizontal alignment of the child widget,
    from 0.0 (left) to 1.0 (right)}
  @argument[yalign]{a float with the vertical alignment of the child widget,
    from 0.0 (top) to 1.0 (bottom)}
  @argument[xscale]{a float with the amount that the child widget expands
    horizontally to fill up unused space, from 0.0 to 1.0, a value of 0.0
    indicates that the child widget should never expand, a value of 1.0
    indicates that the child widget will expand to fill all of the space
    allocated for the alignment}
  @argument[yscale]{a float with the amount that the child widget expands
    vertically to fill up unused space, from 0.0 to 1.0, the values are similar
    to @arg{xscale}}
  @short{Sets the alignment values.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-set} has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gtk-widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-class{gtk-widget}"
  (setf (gtk-alignment-xalign alignment) xalign
        (gtk-alignment-yalign alignment) yalign
        (gtk-alignment-xscale alignment) xscale
        (gtk-alignment-yscale alignment) yscale))

(export 'gtk-alignment-set)

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_get_padding ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-alignment-get-padding))

(defun gtk-alignment-get-padding (alignment)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-9}
  @argument[alignment]{a @class{gtk-alignment} widget}
  @begin{return}
    @arg{top} -- an unsigned integer with the padding for the top of the child
    widget @br{}
    @arg{bottom} -- an unsigned integer with the padding for the bottom of the
    child widget @br{}
    @arg{left} -- an unsigned integer with the padding for the left of the
    child widget @br{}
    @arg{right} -- an unsigned integer with the padding for the right of the
    child widget
  @end{return}
  @begin{short}
    Gets the padding on the different sides of the child widget.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-get-padding} has been deprecated since
    version 3.14 and should not be used in newly written code. Use the
    @class{gtk-widget} alignment and margin properties.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-class{gtk-widget}
  @see-function{gtk-alignment-set-padding}"
  (values (gtk-alignment-top-padding alignment)
          (gtk-alignment-bottom-padding alignment)
          (gtk-alignment-left-padding alignment)
          (gtk-alignment-right-padding alignment)))

(export 'gtk-alignment-get-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_set_padding ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-alignment-set-padding))

(defun gtk-alignment-set-padding (alignment top bottom left right)
 #+cl-cffi-gtk-documentation
 "@version{*2021-7-25}
  @argument[alignment]{a @class{gtk-alignment} widget}
  @argument[top]{an unsigned integer with the padding at the top of the child
    widget}
  @argument[bottom]{an unsigned integer with the padding at the bottom of the
    child widget}
  @argument[left]{an unsigned integer with the padding at the left of the
    child widget}
  @argument[right]{an unsigned integer with the padding at the right of the
    child widget}
  @begin{short}
    Sets the padding on the different sides of the widget.
  @end{short}
  The padding adds blank space to the sides of the child widget. For instance,
  this can be used to indent the child widget towards the right by adding
  padding on the left.
  @begin[Warning]{dictionary}
    The function @sym{gtk-alignment-set-padding} has been deprecated since
    version 3.14 and should not be used in newly written code. Use the
    @class{gtk-widget} alignment and margin properties.
  @end{dictionary}
  @see-class{gtk-alignment}
  @see-class{gtk-widget}
  @see-function{gtk-alignment-get-padding}"
  (setf (gtk-alignment-top-padding alignment) top
        (gtk-alignment-bottom-padding alignment) bottom
        (gtk-alignment-left-padding alignment) left
        (gtk-alignment-right-padding alignment) right))

(export 'gtk-alignment-set-padding)

;;; --- End of file gtk.alignment.lisp -----------------------------------------
