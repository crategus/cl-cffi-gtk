;;; ----------------------------------------------------------------------------
;;; gtk.font-chooser-widget.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkFontChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;; GtkFontChooserWidget implements AtkImplementorIface, GtkBuildable,
;;; GtkOrientable and GtkFontChooser.
;;;
;;; Description
;;;
;;; The GtkFontChooserWidget widget lists the available fonts, styles and sizes,
;;; allowing the user to select a font. It is used in the GtkFontChooserDialog
;;; widget to provide a dialog box for selecting fonts.
;;;
;;; To set the font which is initially selected, use gtk_font_chooser_set_font()
;;; or gtk_font_chooser_set_font_desc().
;;;
;;; To get the selected font use gtk_font_chooser_get_font() or
;;; gtk_font_chooser_get_font_desc().
;;;
;;; To change the text which is shown in the preview area, use
;;; gtk_font_chooser_set_preview_text().
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontChooserWidget
;;;
;;; struct GtkFontChooserWidget;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFileChooserWidget" gtk-file-chooser-widget
  (:superclass gtk-vbox
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkFontChooser")
   :type-initializer "gtk_font_chooser_widget_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_widget_new ()
;;;
;;; GtkWidget * gtk_font_chooser_widget_new (void);
;;;
;;; Creates a new GtkFontChooserWidget.
;;;
;;; Returns :
;;;     a new GtkFontChooserWidget
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-widget-new))

(defun gtk-font-chooser-widget-new ()
  (make-instance 'gtk-font-chooser-widget))

(export 'gtk-font-chooser-widget-new)

;;; --- End of file gtk.font-chooser-widget.lisp -------------------------------
