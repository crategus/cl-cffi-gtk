;;; ----------------------------------------------------------------------------
;;; gtk.color-chooser-widget.lisp
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
;;; GtkColorChooserWidget
;;; 
;;; A widget for choosing colors
;;;     
;;; Synopsis
;;; 
;;;     GtkColorChooserWidget
;;;
;;;     gtk_color_chooser_widget_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkColorChooserWidget
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkColorChooserWidget implements AtkImplementorIface, GtkBuildable,
;;; GtkOrientable and GtkColorChooser.
;;;
;;; Properties
;;; 
;;;   "show-editor"              gboolean              : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkColorChooserWidget widget lets the user select a color. By default,
;;; the chooser presents a prefined palette of colors, plus a small number of
;;; settable custom colors. It is also possible to select a different color with
;;; the single-color editor. To enter the single-color editing mode, use the
;;; context menu of any color of the palette, or use the '+' button to add a new
;;; custom color.
;;; 
;;; The chooser automatically remembers the last selection, as well as custom
;;; colors.
;;; 
;;; To change the initially selected color, use gtk_color_chooser_set_rgba(). To
;;; get the selected font use gtk_color_chooser_get_rgba().
;;; 
;;; The GtkColorChooserWidget is used in the GtkColorChooserDialog to provide a
;;; dialog for selecting colors.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-editor" property
;;; 
;;;   "show-editor"              gboolean              : Read / Write
;;; 
;;; The ::show-editor property is TRUE when the color chooser is showing the
;;; single-color editor. It can be set to switch the color chooser into
;;; single-color editing mode.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorChooserWidget
;;; 
;;; struct GtkColorChooserWidget;
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
;;; gtk_color_chooser_widget_new ()
;;; 
;;; GtkWidget * gtk_color_chooser_widget_new (void);
;;; 
;;; Creates a new GtkColorChooserWidget.
;;; 
;;; Returns :
;;;     a new GtkColorChooserWidget
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-widget-new))

(defun gtk-color-chooser-widget-new ()
  (make-instance 'gtk-color-chooser-widget))

(export 'gtk-color-chooser-widget-new)

;;; --- End of file gtk.color-chooser-widget.lisp ------------------------------
