;;; ----------------------------------------------------------------------------
;;; gtk.color-chooser-widget.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; GtkColorChooserWidget
;;; 
;;; A widget for choosing colors
;;;     
;;; Synopsis
;;; 
;;;     GtkColorChooserWidget
;;;
;;;     gtk_color_chooser_widget_new
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorChooserWidget
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkColorChooserWidget" gtk-color-chooser-widget
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkColorChooser")
   :type-initializer "gtk_color_chooser_widget_get_type")
  ((show-editor
    gtk-color-chooser-widget-show-editor
    "show-editor" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-color-chooser-widget 'type)
 "@version{2013-2-24}
  @begin{short}
    The @sym{gtk-color-chooser-widget} widget lets the user select a color. By
    default, the chooser presents a prefined palette of colors, plus a small
    number of settable custom colors. It is also possible to select a different
    color with the single-color editor. To enter the single-color editing mode,
    use the context menu of any color of the palette, or use the '+' button to
    add a new custom color.
  @end{short}

  The chooser automatically remembers the last selection, as well as custom
  colors.
 
  To change the initially selected color, use @fun{gtk-color-chooser-set-rgba}.
  To get the selected font use @fun{gtk-color-chooser-get-rgba}.

  The @sym{gtk-color-chooser-widget} is used in the
  @class{gtk-color-chooser-dialog} to provide a dialog for selecting colors.
  @see-slot{gtk-color-chooser-widget-show-editor}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-editor"
                                               'gtk-color-chooser-widget) 't)
 "The @code{\"show-editor\"} property of type @code{gboolean}
  (Read / Write)@br{}
  The @code{\"show-editor\"} property is @arg{true} when the color chooser is
  showing the single-color editor. It can be set to switch the color chooser
  into single-color editing mode.@br{}
  Default value: @code{nil}@br{}
  Since 3.4")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-color-chooser-widget-show-editor -----------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-chooser-widget-show-editor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-chooser-widget-show-editor 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"show-editor\"} of the
    @class{gtk-color-chooser-widget} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_widget_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-widget-new))

(defun gtk-color-chooser-widget-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-2-24}
  @return{A new @class{gtk-color-chooser-widget} widget.}
  @short{Creates a new @class{gtk-color-chooser-widget} widget.}

  Since 3.4"
  (make-instance 'gtk-color-chooser-widget))

(export 'gtk-color-chooser-widget-new)

;;; --- End of file gtk.color-chooser-widget.lisp ------------------------------
