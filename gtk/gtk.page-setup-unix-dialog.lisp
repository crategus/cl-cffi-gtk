;;; ----------------------------------------------------------------------------
;;; gtk.page-setup-unix-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     A page setup dialog
;;;
;;; Types and Values
;;;
;;;     GtkPageSetupUnixDialog
;;;
;;; Functions
;;;
;;;     gtk_page_setup_unix_dialog_new
;;;     gtk_page_setup_unix_dialog_set_page_setup
;;;     gtk_page_setup_unix_dialog_get_page_setup
;;;     gtk_page_setup_unix_dialog_set_print_settings
;;;     gtk_page_setup_unix_dialog_get_print_settings
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkDialog
;;;                             ╰── GtkPageSetupUnixDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPageSetupUnixDialog implements AtkImplementorIface and GtkBuildable.
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
    platforms which do not provide a native page setup dialog, like Unix.
  @end{short}

  @image[pagesetupdialog]{}

  It can be used very much like any other GTK+ dialog, at the cost of the
  portability offered by the high-level printing API.
  @begin[Example]{dictionary}
    @begin{pre}
(defun example-page-setup-unix-dialog ()
  (let (response page-setup)
    (within-main-loop
      (let ((dialog (make-instance 'gtk-page-setup-unix-dialog
                                   :title \"Example Page Setup Dialog\"
                                   :default-height 250
                                   :default-width 400)))
        ;; Signal handler for the dialog to handle the signal \"destroy\".
        (g-signal-connect dialog \"destroy\"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Signal handler for the dialog to handle the signal \"response\".
        (g-signal-connect dialog \"response\"
                          (lambda (dialog response-id)
                            (setf response response-id)
                            (setf page-setup
                                  (gtk-page-setup-unix-dialog-page-setup dialog))
                            (gtk-widget-destroy dialog)))
        ;; Show the dialog
        (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (values response page-setup)))
    @end{pre}
  @end{dictionary}
  @see-class{gtk-page-setup}")

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-page-setup-unix-dialog-new (title parent)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[title]{a string with the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk-window} transient parent of the dialog,
    or @code{nil}}
  @return{The new @class{gtk-page-setup-unix-dialog} widget.}
  @begin{short}
    Creates a new page setup dialog.
  @end{short}
  @see-class{gtk-page-setup-unix-dialog}"
  (let ((dialog (make-instance 'gtk-page-setup-unix-dialog)))
    (when title
      (setf (gtk-window-title dialog) title))
    (when parent
      (setf (gtk-window-transient-for dialog) parent))
    dialog))

(export 'gtk-page-setup-unix-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_set_page_setup ()
;;; gtk_page_setup_unix_dialog_get_page_setup ()
;;; ----------------------------------------------------------------------------

(defun (setf gtk-page-setup-unix-dialog-page-setup) (page-setup dialog)
  (foreign-funcall "gtk_page_setup_unix_dialog_set_page_setup"
                   (g-object gtk-page-setup-unix-dialog) dialog
                   (g-object gtk-page-setup) page-setup :void)
  page-setup)

(defcfun ("gtk_page_setup_unix_dialog_get_page_setup"
           gtk-page-setup-unix-dialog-page-setup) (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-26}
  @syntax[]{(gtk-page-setup-unix-dialog-page-setup dialog) => page-setup}
  @syntax[]{(setf (gtk-page-setup-unix-dialog-page-setup dialog) page-setup)}
  @argument[dialog]{a @class{gtk-page-setup-unix-dialog} widget}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @begin{short}
    Accessor for the page setup of the page setup dialog.
  @end{short}

  The function @sym{gtk-page-setup-unix-dialog-page-setup} gets the currently
  selected page setup from the page setup dialog. The function
  @sym{(setf gtk-page-setup-unix-dialog-page-setup)} sets the page setup object
  from which the page setup dialog takes its values.

  @see-class{gtk-page-setup-unix-dialog}
  @see-class{gtk-page-setup}"
  (dialog (g-object gtk-page-setup-unix-dialog)))

(export 'gtk-page-setup-unix-dialog-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_set_print_settings ()
;;; gtk_page_setup_unix_dialog_get_print_settings ()
;;; ----------------------------------------------------------------------------

(defun (setf gtk-page-setup-unix-dialog-print-settings) (settings dialog)
  (foreign-funcall "gtk_page_setup_unix_dialog_set_print_settings"
                   (g-object gtk-page-setup-unix-dialog) dialog
                   (g-object gtk-print-settings) settings :void)
  settings)

(defcfun ("gtk_page_setup_unix_dialog_get_print_settings"
           gtk-page-setup-unix-dialog-print-settings) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-26}
  @syntax[]{(gtk-page-setup-unix-dialog-print-settings dialog) => settings}
  @syntax[]{(setf (gtk-page-setup-unix-dialog-print-settings dialog) settings)}
  @argument[dialog]{a @class{gtk-page-setup-unix-dialog} widget}
  @argument[settings]{a @class{gtk-print-settings} object}
  @begin{short}
    Accesor for the print settings of the page setup dialog.
  @end{short}

  The function @sym{gtk-page-setup-unix-dialog-print-settings} gets the current
  print settings from the page setup dialog. The function
  @sym{(setf gtk-page-setup-unix-dialog-print-settings)} sets the print settings
  from which the page setup dialog takes its values.
  @see-class{gtk-page-setup-unix-dialog}
  @see-class{gtk-print-settings}"
  (dialog (g-object gtk-page-setup-unix-dialog)))

(export 'gtk-page-setup-unix-dialog-print-settings)

;;; --- End of file gtk.page-setup-unix-dialog.lisp ----------------------------
