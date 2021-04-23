;;; ----------------------------------------------------------------------------
;;; gtk.color-chooser-widget.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;;     A widget for choosing colors
;;;
;;; Types and Values
;;;
;;;     GtkColorChooserWidget
;;;
;;; Functions
;;;
;;;     gtk_color_chooser_widget_new
;;;
;;; Properties
;;;
;;;     gboolean    show-editor    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkColorChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkColorChooserWidget implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable and GtkColorChooser.
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-color-chooser-widget 'type)
 "@version{*2021-1-23}
  @begin{short}
    The @sym{gtk-color-chooser-widget} widget lets the user select a color.
  @end{short}
  By default, the chooser presents a prefined palette of colors, plus a small
  number of settable custom colors. It is also possible to select a different
  color with the single-color editor. To enter the single-color editing mode,
  use the context menu of any color of the palette, or use the '+' button to
  add a new custom color.

  The chooser automatically remembers the last selection, as well as custom
  colors. To change the initially selected color or to get the selected color
  use the slot access function @fun{gtk-color-chooser-rgba}.

  The @sym{gtk-color-chooser-widget} widget is used in the
  @class{gtk-color-chooser-dialog} widget to provide a dialog for selecting
  colors.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-color-chooser-widget} class has a single CSS node with name
    @code{colorchooser}.
  @end{dictionary}
  @begin[Example]{dictionary}
    This example shows a color chooser widget in a window. The selected color
    is print on the console.
    @begin{pre}
(defun example-color-chooser-widget ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title \"Example Color Chooser Widget\"
                                 :border-width 12
                                 :default-width 400))
          (color-chooser (make-instance 'gtk-color-chooser-widget)))
        (g-signal-connect window \"destroy\"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        (g-signal-connect color-chooser \"color-activated\"
            (lambda (chooser color)
              (declare (ignore chooser))
              (format t \"Selected color is ~a~%\" (gdk-rgba-to-string color))))
        (gtk-container-add window color-chooser)
        (gtk-widget-show-all window))))
    @end{pre}
  @end{dictionary}
  @see-slot{gtk-color-chooser-widget-show-editor}
  @see-class{gtk-color-chooser}
  @see-class{gtk-color-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accesor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-editor"
                                               'gtk-color-chooser-widget) 't)
 "The @code{show-editor} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} when the color chooser is showing the single-color editor. It can
  be set to switch the color chooser into single-color editing mode. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-chooser-widget-show-editor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-chooser-widget-show-editor 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-color-chooser-widget-show-editor object) => show-editor}
  @syntax[]{(setf (gtk-color-chooser-widget-show-editor object) show-editor)}
  @argument[object]{a @class{gtk-color-chooser-widget} widget}
  @argument[show-editor]{a boolean whether to show the single-color editor}
  @begin{short}
    Accessor of the @slot[gtk-color-chooser-widget]{show-editor} slot of the
    @class{gtk-color-chooser-widget} class.
  @end{short}

  The @code{show-editor} property is @em{true} when the color chooser is
  showing the single-color editor. It can be set to switch the color chooser
  into single-color editing mode.
  @see-class{gtk-color-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_widget_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-widget-new))

(defun gtk-color-chooser-widget-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @return{A new @class{gtk-color-chooser-widget} widget.}
  @short{Creates a new color chooser widget.}
  @see-class{gtk-color-chooser-widget}"
  (make-instance 'gtk-color-chooser-widget))

(export 'gtk-color-chooser-widget-new)

;;; --- End of file gtk.color-chooser-widget.lisp ------------------------------
