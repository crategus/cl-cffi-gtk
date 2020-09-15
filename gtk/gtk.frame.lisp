;;; ----------------------------------------------------------------------------
;;; gtk.frame.lisp
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
;;; GtkFrame
;;;
;;;     A bin with a decorative frame and optional label
;;;
;;; Types and Values
;;;
;;;     GtkFrame
;;;
;;; Functions
;;;
;;;     gtk_frame_new
;;;     gtk_frame_set_label                                Accessor
;;;     gtk_frame_set_label_widget                         Accessor
;;;     gtk_frame_set_label_align
;;;     gtk_frame_set_shadow_type                          Accessor
;;;     gtk_frame_get_label                                Accessor
;;;     gtk_frame_get_label_align
;;;     gtk_frame_get_label_widget                         Accessor
;;;     gtk_frame_get_shadow_type                          Accessor
;;;
;;; Properties
;;;
;;;             gchar*   label           Read / Write
;;;         GtkWidget*   label-widget    Read / Write
;;;            gfloat    label-xalign    Read / Write
;;;            gfloat    label-yalign    Read / Write
;;;     GtkShadowType    shadow-type     Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkFrame
;;;                         ╰── GtkAspectFrame
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFrame implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFrame
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkFrame" 'gtk-frame))

(define-g-object-class "GtkFrame" gtk-frame
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_frame_get_type")
  ((label
    gtk-frame-label
    "label" "gchararray" t t)
   (label-widget
    gtk-frame-label-widget
    "label-widget" "GtkWidget" t t)
   (label-xalign
    gtk-frame-label-xalign
    "label-xalign" "gfloat" t t)
   (label-yalign
    gtk-frame-label-yalign
    "label-yalign" "gfloat" t t)
   (shadow-type
    gtk-frame-shadow-type
    "shadow-type" "GtkShadowType" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-frame 'type)
 "@version{2020-9-5}
  @begin{short}
    The frame widget is a @class{gtk-bin} widget that surrounds its child with
    a decorative frame and an optional label.
  @end{short}

  @image[frame]{}

  If present, the label is drawn in a gap in the top side of the frame. The
  position of the label can be controlled with the function
  @fun{gtk-frame-label-align}.
  @begin[GtkFrame as GtkBuildable]{dictionary}
    The @sym{gtk-frame} implementation of the @class{gtk-buildable} interface
    supports placing a child in the label position by specifying
    @code{\"label\"} as the @code{type} attribute of a @code{<child>}
    element. A normal content child can be specified without specifying a
    @code{<child>} type attribute.

    @b{Example:} A UI definition fragment with @sym{gtk-frame}
    @begin{pre}
 <object class=\"GtkFrame\">
  <child type=\"label\">
    <object class=\"GtkLabel\" id=\"frame-label\"/>
  </child>
  <child>
    <object class=\"GtkEntry\" id=\"frame-content\"/>
  </child>
 </object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 frame
 ├── border[.flat]
 ├── <label widget>
 ╰── <child>
    @end{pre}
    @sym{gtk-frame} has a main CSS node named @code{frame} and a subnode named
    @code{border}. The @code{border} node is used to draw the visible border.
    You can set the appearance of the border using CSS properties like
    @code{border-style} on the @code{border} node.

    The border node can be given the style class @code{.flat}, which is used by
    themes to disable drawing of the border. To do this from code, call the
    @fun{gtk-frame-shadow-type} slot access function with @code{:none} to add
    the @code{.flat} class or any other shadow type to remove it.
  @end{dictionary}
  @see-slot{gtk-frame-label}
  @see-slot{gtk-frame-label-widget}
  @see-slot{gtk-frame-label-xalign}
  @see-slot{gtk-frame-label-yalign}
  @see-slot{gtk-frame-shadow-type}
  @see-class{gtk-bin}
  @see-class{gtk-widget}
  @see-class{gtk-buildable}
  @see-symbol{gtk-shadow-type}
  @see-function{gtk-frame-label-align}")

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-frame-label --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-frame) 't)
 "The @code{label} property of type  @code{:string} (Read / Write) @br{}
  Text of the frame's label. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-frame-label atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-frame-label 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-frame-label object) => label}
  @syntax[]{(setf (gtk-frame-label object) label)}
  @argument[object]{a @class{gtk-frame} widget}
  @argument[label]{a @code{:string} with the text to use as the label of the
    frame}
  @begin{short}
    Accessor of the @slot[gtk-frame]{label} slot of the @class{gtk-frame} class.
  @end{short}

  The slot access function @sym{gtk-frame-label} returns the text in the label,
  or @code{nil} if there was no label widget or the label widget was not a
  @class{gtk-label}. The slot access function @sym{(setf gtk-frame-label)} sets
  the text of the label. If @arg{label} is @code{nil}, the current label is
  removed.

  The frame will have a @class{gtk-label} widget for the label widget if a
  non-@code{nil} argument was passed to the @fun{gtk-frame-new} funcion.
  @see-class{gtk-frame}
  @see-function{gtk-frame-label-widget}")

;;; --- gtk-frame-label-widget -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-widget" 'gtk-frame) 't)
 "The @code{label-widget} property of type @class{gtk-widget} (Read / Write)
  @br{}
  A widget to display in place of the usual frame label.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-frame-label-widget atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-frame-label-widget 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-frame-label-widget object) => label-widget}
  @syntax[]{(setf (gtk-frame-label-widget object) label-widget)}
  @argument[object]{a @class{gtk-frame} widget}
  @argument[label-widget]{the new @class{gtk-widget} label widget}
  @begin{short}
    Accessor of the @slot[gtk-frame]{label-widget} slot of the @class{gtk-frame}
    class.
  @end{short}

  The slot access function @sym{gtk-frame-label-widget} retrieves the label
  widget for the frame. The slot access function
  @sym{(setf gtk-frame-label-widget)} sets the label widget for the frame.

  This is the widget that will appear embedded in the top edge of the frame as
  a title.
  @see-class{gtk-frame}
  @see-function{gtk-frame-label}")

;;; --- gtk-frame-label-xalign -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-xalign" 'gtk-frame) 't)
 "The @code{label-xalign} property of type @code{:float} (Read / Write) @br{}
  The horizontal alignment of the label. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-frame-label-xalign atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-frame-label-xalign 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-frame-label-xalign object) => xalign}
  @syntax[]{(setf (gtk-frame-label-xalign object) xalign)}
  @argument[object]{a @class{gtk-frame} widget}
  @argument[xalign]{a @code{:float} with the horizontal alignment of the label}
  @begin{short}
    Accessor of the @slot[gtk-frame]{label-xalign} slot of the @class{gtk-frame}
    class.
  @end{short}
  @see-class{gtk-frame}
  @see-function{gtk-frame-label-yalign}")

;;; --- gtk-frame-label-yalign -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-yalign" 'gtk-frame) 't)
 "The @code{label-yalign} property of type @code{:float} (Read / Write) @br{}
  The vertical alignment of the label. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-frame-label-yalign atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-frame-label-yalign 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-frame-label-yalign object) => yalign}
  @syntax[]{(setf (gtk-frame-label-yalign object) yalign)}
  @argument[object]{a @class{gtk-frame} widget}
  @argument[yalign]{a @code{:float} with the vertical alignment of the label}
  @begin{short}
    Accessor of the @slot[gtk-frame]{label-yalign} slot of the @class{gtk-frame}
    class.
  @end{short}
  @see-class{gtk-frame}
  @see-function{gtk-frame-xalign}")

;;; --- gtk-frame-shadow-type --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type" 'gtk-frame) 't)
 "The @code{shadow-type} property of type @symbol{gtk-shadow-type}
  (Read / Write) @br{}
  Appearance of the frame border. @br{}
  Default value: @code{:etched-in}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-frame-shadow-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-frame-shadow-type 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-frame-shadow-type object) => type}
  @syntax[]{(setf (gtk-frame-shadow-type object) type)}
  @argument[object]{a @class{gtk-frame} widget}
  @argument[type]{a value of the @symbol{gtk-shadow-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-frame]{shadow-type} slot of the @class{gtk-frame}
    class.
  @end{short}

  The slot access function @sym{gtk-frame-shadow-type} retrieves the shadow
  type of the frame. The slot access function @sym{(setf gtk-frame-shadow-type)}
  sets the shadow type for the frame.
  @see-class{gtk-frame}
  @see-symbol{gtk-shadow-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_frame_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-new))

(defun gtk-frame-new (label)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-5}
  @argument[label]{a @code{:string} with the text to use as the label of the
    frame}
  @return{A new @class{gtk-frame} widget.}
  @begin{short}
    Creates a new frame widget, with an optional label.
  @end{short}
  If @arg{label} is @code{nil}, the label is omitted.
  @see-class{gtk-frame}"
  (make-instance 'gtk-frame
                 :label label))

(export 'gtk-frame-new)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_label_align ()
;;; gtk_frame_set_label_align () -> gtk-frame-label-align
;;; ----------------------------------------------------------------------------

(defun (setf gtk-frame-label-align) (align frame)
  (destructuring-bind (xalign yalign) align
     (values (setf (gtk-frame-label-xalign frame) xalign)
             (setf (gtk-frame-label-yalign frame) yalign))))

(defun gtk-frame-label-align (frame)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-5}
  @syntax[]{(gtk-frame-label-align frame) => xalign, yalign}
  @syntax[]{(setf (gtk-frame-label-align frame) (list xalign yalign))}
  @argument[frame]{a @class{gtk-frame} widget}
  @argument[xalign]{a @code{:float} with the position of the label along the
    top edge of the widget, a value of 0.0 represents left alignment, 1.0
    represents right alignment}
  @argument[yalign]{a @code{:float} with the y alignment of the label, a value
    of 0.0 aligns under the frame, 1.0 aligns above the frame, if the values
    are exactly 0.0 or 1.0 the gap in the frame will not be painted because the
    label will be completely above or below the frame}
  @begin{short}
    Accessor of the @slot[gtk-frame]{label-xalign} and
    @slot[gtk-frame]{label-yalign} properties of a frame widget.
  @end{short}

  The function @sym{gtk-frame-label-align} retrieves the x and y alignment of
  the frame's label. The function @sym{(setf gtk-frame-label-align)} sets the
  alignment of the frame container's label. The default values for a newly
  created frame are 0.0 and 0.5.
  @see-class{gtk-frame}
  @see-function{gtk-frame-xalign}
  @see-function{gtk-frame-yalign}"
  (values (gtk-frame-label-xalign frame)
          (gtk-frame-label-yalign frame)))

(export 'gtk-frame-label-align)

;;; --- End of file gtk.frame.lisp ---------------------------------------------
