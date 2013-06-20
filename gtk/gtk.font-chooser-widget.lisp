;;; ----------------------------------------------------------------------------
;;; gtk.font-chooser-widget.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkFontChooserWidget
;;;
;;; A widget for selecting fonts
;;;
;;; Synopsis
;;;
;;;     GtkFontChooserWidget
;;;
;;;     gtk_font_chooser_widget_new
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontChooserWidget
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontChooserWidget" gtk-font-chooser-widget
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkFontChooser")
   :type-initializer "gtk_font_chooser_widget_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-font-chooser-widget 'type)
 "@version{2013-6-18}
  @begin{short}
    The @sym{gtk-font-chooser-widget} widget lists the available fonts, styles
    and sizes, allowing the user to select a font. It is used in the
    @class{gtk-font-chooser-dialog} widget to provide a dialog box for selecting
    fonts.
  @end{short}

  To set the font which is initially selected, use the functions
  @fun{gtk-font-chooser-set-font} or @fun{gtk-font-chooser-set-font-desc}.

  To get the selected font use the functions @fun{gtk-font-chooser-get-font} or
  @fun{gtk-font-chooser-get-font-desc}.

  To change the text which is shown in the preview area, use the function
  @fun{gtk-font-chooser-set-preview-text}.")

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_widget_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-widget-new))

(defun gtk-font-chooser-widget-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @return{A new @class{gtk-font-chooser-widget} widget.}
  @short{Creates a new @class{gtk-font-chooser-widget} widget.}

  Since 3.2"
  (make-instance 'gtk-font-chooser-widget))

(export 'gtk-font-chooser-widget-new)

;;; --- End of file gtk.font-chooser-widget.lisp -------------------------------
