;;; ----------------------------------------------------------------------------
;;; gtk.font-chooser-dialog.lisp
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
;;; GtkFontChooserDialog
;;;
;;; A dialog for selecting fonts
;;;
;;; Synopsis
;;;
;;;     GtkFontChooserDialog
;;;
;;;     gtk_font_chooser_dialog_new
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontChooserDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontChooserDialog" gtk-font-chooser-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkFontChooser")
   :type-initializer "gtk_font_chooser_dialog_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-font-chooser-dialog 'type)
 "@version{2013-3-6}
  @begin{short}
    The @sym{gtk-font-chooser-dialog} widget is a dialog for selecting a font.
    It implements the @class{gtk-font-chooser} interface.
  @end{short}

  @heading{GtkFontChooserDialog as GtkBuildable}
  The @sym{gtk-font-chooser-dialog} implementation of the @class{gtk-buildable}
  interface exposes the buttons with the names \"select_button\" and
  \"cancel_button\".")

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_dialog_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-dialog-new))

(defun gtk-font-chooser-dialog-new (title parent)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[title]{Title of the dialog, or @code{nil}.}
  @argument[parent]{Transient parent of the dialog, or @code{nil}.}
  @return{A new @class{gtk-font-chooser-dialog} widget.}
  @short{Creates a new @class{gtk-font-chooser-dialog} widget.}

  Since 3.2"
  (make-instance 'gtk-font-chooser-dialog
                 :title title
                 :parent parent))

(export 'gtk-font-chooser-dialog-new)

;;; --- End of file gtk.font-chooser-dialog.lisp -------------------------------
