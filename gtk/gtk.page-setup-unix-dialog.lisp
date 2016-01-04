;;; ----------------------------------------------------------------------------
;;; gtk.page-setup-unix-dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkPageSetupUnixDialog
;;;
;;; A page setup dialog
;;;
;;; Synopsis
;;;
;;;     GtkPageSetupUnixDialog
;;;
;;;     gtk_page_setup_unix_dialog_new
;;;     gtk_page_setup_unix_dialog_set_page_setup
;;;     gtk_page_setup_unix_dialog_get_page_setup
;;;     gtk_page_setup_unix_dialog_set_print_settings
;;;     gtk_page_setup_unix_dialog_get_print_settings
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkWindow
;;;                                  +----GtkDialog
;;;                                        +----GtkPageSetupUnixDialog
;;;
;;; Implemented Interfaces
;;;
;;; GtkPageSetupUnixDialog implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPageSetupUnixDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPageSetupUnixDialog" gtk-page-setup-unix-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_page_setup_unix_dialog_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-page-setup-unix-dialog 'type)
 "@version{2013-5-30}
  @begin{short}
    @sym{gtk-page-setup-unix-dialog} implements a page setup dialog for
    platforms which do not provide a native page setup dialog, like Unix. It
    can be used very much like any other GTK+ dialog, at the cost of the
    portability offered by the high-level printing API.
  @end{short}

  Printing support was added in GTK+ 2.10.")

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-page-setup-unix-dialog-new))

(defun gtk-page-setup-unix-dialog-new (title parent)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[title]{the title of the dialog, or @code{nil}}
  @argument[parent]{transient parent of the dialog, or @code{nil}}
  @return{The new @class{gtk-page-setup-unix-dialog} widget.}
  @begin{short}
    Creates a new page setup dialog.
  @end{short}

  Since 2.10"
  (make-instance 'gtk-page-setup-unix-dialog
                 :title title
                 :parent parent))

(export 'gtk-page-setup-unix-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_set_page_setup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_unix_dialog_set_page_setup"
           gtk-page-setup-unix-dialog-set-page-setup) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-9}
  @argument[dialog]{a @class{gtk-page-setup-unix-dialog} widget}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @begin{short}
    Sets the @class{gtk-page-setup} object from which the page setup dialog
    takes its values.
  @end{short}

  Since 2.10
  @see-function{gtk-page-setup-unix-dialog-get-page-setup}"
  (dialog (g-object gtk-page-setup-unix-dialog))
  (page-setup (g-object gtk-page-setup)))

(export 'gtk-page-setup-unix-dialog-set-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_get_page_setup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_unix_dialog_get_page_setup"
           gtk-page-setup-unix-dialog-get-page-setup) (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-9}
  @argument[dialog]{a @class{gtk-page-setup-unix-dialog} widget}
  @return{The current page setup.}
  @begin{short}
    Gets the currently selected page setup from the dialog.
  @end{short}

  Since 2.10
  @see-function{gtk-page-setup-unix-dialog-set-page-setup}"
  (dialog (g-object gtk-page-setup-unix-dialog)))

(export 'gtk-page-setup-unix-dialog-get-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_set_print_settings ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_unix_dialog_set_print_settings"
           gtk-page-setup-unix-dialog-set-print-settings) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-page-setup-unix-dialog} widget}
  @argument[print-settings]{a @class{gtk-print-settings} object}
  @begin{short}
    Sets the @class{gtk-print-settings} from which the page setup dialog takes
    its values.
  @end{short}

  Since 2.10"
  (dialog (g-object gtk-page-setup-unix-dialog))
  (print-settings (g-object gtk-print-settings)))

(export 'gtk-page-setup-unix-dialog-set-print-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_get_print_settings ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_unix_dialog_get_print_settings"
           gtk-page-setup-unix-dialog-get-print-settings) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-page-setup-unix-dialog} widget}
  @return{The current print settings.}
  @begin{short}
    Gets the current print settings from the dialog.
  @end{short}

  Since 2.10"
  (dialog (g-object gtk-page-setup-unix-dialog))
  (print-settings (g-object gtk-print-settings)))

(export 'gtk-page-setup-unix-dialog-get-print-settings)

;;; --- End of file gtk.page-setup-unix-dialog.lisp ----------------------------
