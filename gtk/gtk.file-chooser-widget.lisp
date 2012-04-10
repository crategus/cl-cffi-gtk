;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser-widget.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; GtkFileChooserWidget
;;; 
;;; File chooser widget that can be embedded in other widgets
;;; 
;;; Synopsis
;;; 
;;;     GtkFileChooserWidget
;;;
;;;     gtk_file_chooser_widget_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkFileChooserWidget
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkFileChooserWidget implements AtkImplementorIface, GtkBuildable,
;;; GtkOrientable, GtkFileChooser and GtkFileChooserEmbed.
;;;
;;; Description
;;; 
;;; GtkFileChooserWidget is a widget suitable for selecting files. It is the
;;; main building block of a GtkFileChooserDialog. Most applications will only
;;; need to use the latter; you can use GtkFileChooserWidget as part of a larger
;;; window if you have special needs.
;;; 
;;; Note that GtkFileChooserWidget does not have any methods of its own.
;;; Instead, you should use the functions that work on a GtkFileChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;; The interface GtkFileChooserEmbed seems to be not documented in the
;; GTK+ documentation. It is implemented by GtkFileChooserWidget.

;(define-g-interface "GtkFileChooserEmbed" gtk-file-chooser-embed
;    (:export t))

;;; ----------------------------------------------------------------------------
;;; struct GtkFileChooserWidget
;;; 
;;; struct GtkFileChooserWidget;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkFileChooserWidget" 'gtk-file-chooser-widget))

(define-g-object-class "GtkFileChooserWidget" gtk-file-chooser-widget
  (:superclass gtk-vbox
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser"
;                "GtkFileChooserEmbed" 
                "GtkOrientable")
   :type-initializer "gtk_file_chooser_widget_get_type")
  nil)

;;; ----------------------------------------------------------------------------

(define-child-property "GtkFileChooserWidget"
                       gtk-file-chooser-widget-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkFileChooserWidget"
                       gtk-file-chooser-widget-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkFileChooserWidget"
                       gtk-file-chooser-widget-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkFileChooserWidget"
                       gtk-file-chooser-widget-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkFileChooserWidget"
                       gtk-file-chooser-widget-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_widget_new ()
;;; 
;;; GtkWidget * gtk_file_chooser_widget_new (GtkFileChooserAction action);
;;; 
;;; Creates a new GtkFileChooserWidget. This is a file chooser widget that can
;;; be embedded in custom windows, and it is the same widget that is used by
;;; GtkFileChooserDialog.
;;; 
;;; action :
;;;     Open or save mode for the widget
;;; 
;;; Returns :
;;;     a new GtkFileChooserWidget
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.file-chooser-widget.lisp -------------------------------
