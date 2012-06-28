;;; ----------------------------------------------------------------------------
;;; gtk.page-setup-unix-dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;;﻿
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
;;;
;;; Description
;;;
;;; GtkPageSetupUnixDialog implements a page setup dialog for platforms which
;;; don't provide a native page setup dialog, like Unix. It can be used very
;;; much like any other GTK+ dialog, at the cost of the portability offered by
;;; the high-level printing API
;;;
;;; Printing support was added in GTK+ 2.10.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPageSetupUnixDialog
;;;
;;; struct GtkPageSetupUnixDialog;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPageSetupUnixDialog" gtk-page-setup-unix-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_page_setup_unix_dialog_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_new ()
;;;
;;; GtkWidget * gtk_page_setup_unix_dialog_new (const gchar *title,
;;;                                             GtkWindow *parent);
;;;
;;; Creates a new page setup dialog.
;;;
;;; title :
;;;     the title of the dialog, or NULL
;;;
;;; parent :
;;;     transient parent of the dialog, or NULL
;;;
;;; Returns :
;;;     the new GtkPageSetupUnixDialog
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-page-setup-unix-dialog-new))

(defun gtk-page-setup-unix-dialog-new (title parent)
  (make-instance 'gtk-page-setup-unix-dialog
                 :title title
                 :parent parent))

(export 'gtk-page-setup-unix-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_set_page_setup ()
;;;
;;; void gtk_page_setup_unix_dialog_set_page_setup
;;;                                             (GtkPageSetupUnixDialog *dialog,
;;;                                              GtkPageSetup *page_setup);
;;;
;;; Sets the GtkPageSetup from which the page setup dialog takes its values.
;;;
;;; dialog :
;;;     a GtkPageSetupUnixDialog
;;;
;;; page_setup :
;;;     a GtkPageSetup
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_unix_dialog_set_page_setup"
           gtk-page-setup-unix-dialog-set-page-setup) :void
  (dialog (g-object gtk-page-setup-unix-dialog))
  (page-setup (g-object gkt-page-setup)))

(export 'gtk-page-setup-unix-dialog-set-page-setpup)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_get_page_setup ()
;;;
;;; GtkPageSetup * gtk_page_setup_unix_dialog_get_page_setup
;;;                                            (GtkPageSetupUnixDialog *dialog);
;;;
;;; Gets the currently selected page setup from the dialog.
;;;
;;; dialog :
;;;     a GtkPageSetupUnixDialog
;;;
;;; Returns :
;;;     the current page setup
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_unix_dialog_get_page_setup"
           gtk-page-setup-unix-dialog-get-page-setup) (g-object gtk-page-setup)
  (dialog (g-object gtk-page-setup-unix-dialog)))

(export 'gtk-page-setup-unix-dialog-get-page-setpup)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_set_print_settings ()
;;;
;;; void gtk_page_setup_unix_dialog_set_print_settings
;;;                                          (GtkPageSetupUnixDialog *dialog,
;;;                                           GtkPrintSettings *print_settings);
;;;
;;; Sets the GtkPrintSettings from which the page setup dialog takes its values.
;;;
;;; dialog :
;;;     a GtkPageSetupUnixDialog
;;;
;;; print_settings :
;;;     a GtkPrintSettings
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_unix_dialog_set_print_settings"
           gtk-page-setup-unix-dialog-set-print-settings) :void
  (dialog (g-object gkt-page-setup-unix-dialog))
  (print-settings (g-object gtk-print-settings)))

(export 'gtk-page-setup-unix-dialog-set-print-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_get_print_settings ()
;;;
;;; GtkPrintSettings * gtk_page_setup_unix_dialog_get_print_settings
;;;                                            (GtkPageSetupUnixDialog *dialog);
;;;
;;; Gets the current print settings from the dialog.
;;;
;;; dialog :
;;;     a GtkPageSetupUnixDialog
;;;
;;; Returns :
;;;     the current print settings
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_unix_dialog_get_print_settings"
           gtk-page-setup-unix-dialog-get-print-settings) :void
  (dialog (g-object gkt-page-setup-unix-dialog))
  (print-settings (g-object gtk-print-settings)))

(export 'gtk-page-setup-unix-dialog-get-print-settings)

;;; --- End of file gtk.page-setup-unix-dialog.lisp ----------------------------
