;;; ----------------------------------------------------------------------------
;;; gtk.frame.lisp
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

;;; --- gtk-frame --------------------------------------------------------------

(setf (documentation 'gtk-frame 'type)
 "@version{2013-1-29}
  @begin{short}
    The frame widget is a Bin that surrounds its child with a decorative frame
    and an optional label.
  @end{short}
  If present, the label is drawn in a gap in the top side of the frame. The
  position of the label can be controlled with @fun{gtk-frame-set-label-align}.

  @heading{GtkFrame as GtkBuildable}
  The @sym{gtk-frame} implementation of the @class{gtk-buildable} interface
  supports placing a child in the label position by specifying \"label\" as the
  \"type\" attribute of a @code{<child>} element. A normal content child can be
  specified without specifying a @code{<child>} type attribute.

  Example 94. A UI definition fragment with GtkFrame
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
  @see-slot{gtk-frame-label}
  @see-slot{gtk-frame-label-widget}
  @see-slot{gtk-frame-label-xalign}
  @see-slot{gtk-frame-label-yalign}
  @see-slot{gtk-frame-shadow-type}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-frame) 't)
 "The \"label\" property of type  @code{gchar*} (Read / Write)@br{}
  Text of the frame's label.@br{}
  Default value: @code{nil}")

(setf (documentation (atdoc:get-slot-from-name "label-widget" 'gtk-frame) 't)
 "The \"label-widget\" property of type @code{gtk-widet} (Read / Write)@br{}
  A widget to display in place of the usual frame label.")

(setf (documentation (atdoc:get-slot-from-name "label-xalign" 'gtk-frame) 't)
 "The \"label-xalign\" property of type  @code{gfloat} (Read / Write)@br{}
  The horizontal alignment of the label.@br{}
  Allowed values: @code{[0,1]}@br{}
  Default value: @code{0}")

(setf (documentation (atdoc:get-slot-from-name "label-yalign" 'gtk-frame) 't)
 "The \"label-yalign\" property of type @code{gfloat} (Read / Write)@br{}
  The vertical alignment of the label.@br{}
  Allowed values: @code{[0,1]}@br{}
  Default value: @code{0.5}")

(setf (documentation (atdoc:get-slot-from-name "shadow-type" 'gtk-frame) 't)
 "The \"shadow-type\" property of type @symbol{gtk-shadow-type}
  (Read / Write)@br{}
  Appearance of the frame border.@br{}
  Default value: @code{:etched-in}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-frame-label --------------------------------------------------------

(setf (gethash 'gtk-frame-label atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-frame-label 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"label\" of the @class{gtk-frame} class.
  @end{short}")

;;; --- gtk-frame-label-widget -------------------------------------------------

(setf (gethash 'gtk-frame-label-widget atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-frame-label-widget 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"label-widget\" of the @class{gtk-frame} class.
  @end{short}")

;;; --- gtk-frame-label-xalign -------------------------------------------------

(setf (gethash 'gtk-frame-label-xalign atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-frame-label-xalign 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"label-xalign\" of the @class{gtk-frame} class.
  @end{short}")

;;; --- gtk-frame-label-yalign -------------------------------------------------

(setf (gethash 'gtk-frame-label-yalign atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-frame-label-yalign 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"label-yalign\" of the @class{gtk-frame} class.
  @end{short}")

;;; --- gtk-frame-shadow-type --------------------------------------------------

(setf (gethash 'gtk-frame-shadow-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-frame-shadow-type 'function)
 "@version{2013-1-29}
  @begin{short}
    Accessor of the slot \"shadow-type\" of the @class{gtk-frame} class.
  @end{short}")

;;; --- End of Accessors -------------------------------------------------------

;;; --- gtk-frame-new ----------------------------------------------------------

(setf (documentation 'gtk-frame-new 'function)
 "@version{2013-1-29}
  @argument[label]{the text to use as the label of the frame}
  @return{a new @class{gtk-frame} widget}
  @begin{short}
    Creates a new @class{gtk-frame} widget, with optional label @arg{label}. If
    @arg{label} is @code{nil}, the label is omitted.
  @end{short}")

;;; --- gtk-frame-set-label ----------------------------------------------------

(setf (documentation 'gtk-frame-set-label 'function)
 "@version{2013-1-29}
  @argument[frame]{a @class{gtk-frame} widget}
  @argument[label]{the text to use as the label of the frame}
  @begin{short}
    Sets the text of the label. If label is NULL, the current label is removed.
  @end{short}")

;;; --- gtk-frame-set-label-widget ---------------------------------------------

(setf (documentation 'gtk-frame-set-label-widget 'function)
 "@version{2013-1-29}
  @argument[frame]{a @class{gtk-frame} widget}
  @argument[label-widget]{the new label widget}
  @begin{short}
    Sets the label widget for the frame. This is the widget that will appear
    embedded in the top edge of the frame as a title.
  @end{short}")

;;; --- gtk-frame-set-label-align ----------------------------------------------

(setf (documentation 'gtk-frame-set-label-align 'function)
 "@version{2013-1-29}
  @argument[frame]{a @class{gtk-frame} widget}
  @argument[xalign]{The position of the label along the top edge of the widget.
    A value of 0.0 represents left alignment; 1.0 represents right alignment.}
  @argument[yalign]{The y alignment of the label. A value of 0.0 aligns under
    the frame; 1.0 aligns above the frame. If the values are exactly 0.0 or 1.0
    the gap in the frame won't be painted because the label will be completely
    above or below the frame.}
  @begin{short}
    Sets the alignment of the frame widget's label. The default values for a
    newly created frame are 0.0 and 0.5.
  @end{short}")

;;; --- gtk-frame-set-shadow-type ----------------------------------------------

(setf (documentation 'gtk-frame-set-shadow-type 'function)
 "@version{2013-1-29}
  @argument[frame]{a @class{gtk-frame} widget}
  @argument[type]{the new @symbol{gtk-shadow-type}}
  @begin{short}
    Sets the shadow type for frame.
  @end{short}")

;;; --- gtk-frame-get-label ----------------------------------------------------

(setf (documentation 'gtk-frame-get-label 'function)
 "@version{2013-1-29}
  @argument[frame]{a @class{gtk-frame} widget}
  @return{the text in the label, or NULL if there was no label widget or the
    label widget was not a GtkLabel. This string is owned by GTK+ and must not
    be modified or freed.}
  @begin{short}
    If the frame's label widget is a GtkLabel, returns the text in the label
    widget.
  @end{short}
  (The frame will have a GtkLabel for the label widget if a non-NULL
  argument was passed to gtk_frame_new().)")

;;; --- gtk-frame-get-label-align ----------------------------------------------

(setf (documentation 'gtk-frame-get-label-align 'function)
 "@version{2013-1-29}
  @argument[frame]{a @class{gtk-frame} widget}
  @argument[xalign]{location to store X alignment of frame's label, or NULL}
  @argument[yalign]{location to store X alignment of frame's label, or NULL}
  @begin{short}
    Retrieves the X and Y alignment of the frame's label.
  @end{short}
  See @fun{gtk-frame-set-label-align}.")

;;; --- gtk-frame-get-label-widget ---------------------------------------------

(setf (documentation 'gtk-frame-get-label-widget 'function)
 "@version{2013-1-29}
  @argument[frame]{a @class{gtk-frame} widget}
  @return{the label widget, or NULL if there is none}
  @begin{short}
    Retrieves the label widget for the frame. See gtk_frame_set_label_widget().
  @end{short}")

;;; --- gtk-frame-get-shadow-type ----------------------------------------------

(setf (documentation 'gtk-frame-get-shadow-type 'function)
 "@version{2013-1-29}
  @argument[frame]{a @class{gtk-frame} widget}
  @return{the current shadow type of the frame.}
  @begin{short}
    Retrieves the shadow type of the frame. See gtk_frame_set_shadow_type().
  @end{short}")

;;; --- End of file atdoc-gtk.frame.lisp ---------------------------------------
