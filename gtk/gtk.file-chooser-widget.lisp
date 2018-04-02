;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser-widget.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFileChooserWidget
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkFileChooserWidget" 'gtk-file-chooser-widget))

(define-g-object-class "GtkFileChooserWidget" gtk-file-chooser-widget
  (:superclass gtk-vbox
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkFileChooser"
;                "GtkFileChooserEmbed" 
               )
   :type-initializer "gtk_file_chooser_widget_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-file-chooser-widget 'type)
 "@version{2013-6-18}
  @begin{short}
    @sym{gtk-file-chooser-widget} is a widget suitable for selecting files. It
    is the main building block of a @class{gtk-file-chooser-dialog}. Most
    applications will only need to use the latter; you can use
    @class{gtk-file-chooser-widget} as part of a larger window if you have
    special needs.
  @end{short}

  Note that @sym{gtk-file-chooser-widget} does not have any methods of its own.
  Instead, you should use the functions that work on a @class{gtk-file-chooser}
  object.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-child-expand 'function)
 "@version{2013-8-27}
  Accessor of the child property @code{\"expand\"} of the
  @class{gtk-file-chooser-widget} class.
  @see-class{gtk-file-chooser-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-child-fill 'function)
 "@version{2013-8-27}
  Accessor of the child property @code{\"fill\"} of the
  @class{gtk-file-chooser-widget} class.
  @see-class{gtk-file-chooser-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-child-padding
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-child-padding 'function)
 "@version{2013-8-27}
  Accessor of the child property @code{\"padding\"} of the
  @class{gtk-file-chooser-widget} class.
  @see-class{gtk-file-chooser-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-child-pack-type
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-child-pack-type 'function)
 "@version{2013-8-27}
  Accessor of the child property @code{\"pack-type\"} of the
  @class{gtk-file-chooser-widget} class.
  @see-class{gtk-file-chooser-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-child-position
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-child-position 'function)
 "@version{2013-8-27}
  Accessor of the child property @code{\"position\"} of the
  @class{gtk-file-chooser-widget} class.
  @see-class{gtk-file-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_widget_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-file-chooser-widget-new))

(defun gtk-file-chooser-widget-new (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-27}
  @argument[action]{open or save mode for the widget, a value of the 
    @symbol{gtk-file-chooser-action} enumeration}
  @return{A new @class{gtk-file-chooser-widget} widget.}
  @begin{short}
    Creates a new @class{gtk-file-chooser-widget}.
  @end{short}
  This is a file chooser widget that can be embedded in custom windows, and it
  is the same widget that is used by @class{gtk-file-chooser-dialog}.

  Since 2.4
  @see-class{gtk-file-chooser-widget}
  @see-class{gtk-file-chooser-dialog}
  @see-symbol{gtk-file-chooser-action}"
  (make-instance 'gtk-file-chooser-widget
                 :action action))

(export 'gtk-file-chooser-widget-new)

;;; --- End of file gtk.file-chooser-widget.lisp -------------------------------
