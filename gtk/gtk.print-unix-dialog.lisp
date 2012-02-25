;;; ----------------------------------------------------------------------------
;;; gtk.print-unix-dialog.lisp
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
;;; GtkPrintUnixDialog
;;; 
;;; A print dialog
;;; 
;;; Synopsis
;;; 
;;;     GtkPrintUnixDialog
;;;
;;;     gtk_print_unix_dialog_new
;;;     gtk_print_unix_dialog_set_page_setup
;;;     gtk_print_unix_dialog_get_page_setup
;;;     gtk_print_unix_dialog_set_current_page
;;;     gtk_print_unix_dialog_get_current_page
;;;     gtk_print_unix_dialog_set_settings
;;;     gtk_print_unix_dialog_get_settings
;;;     gtk_print_unix_dialog_get_selected_printer
;;;     gtk_print_unix_dialog_add_custom_tab
;;;     gtk_print_unix_dialog_set_support_selection
;;;     gtk_print_unix_dialog_get_support_selection
;;;     gtk_print_unix_dialog_set_has_selection
;;;     gtk_print_unix_dialog_get_has_selection
;;;     gtk_print_unix_dialog_set_embed_page_setup
;;;     gtk_print_unix_dialog_get_embed_page_setup
;;;     gtk_print_unix_dialog_get_page_setup_set
;;;
;;;     GtkPrintCapabilities
;;;
;;;     gtk_print_unix_dialog_set_manual_capabilities
;;;     gtk_print_unix_dialog_get_manual_capabilities
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
;;;                                        +----GtkPrintUnixDialog
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkPrintUnixDialog implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "current-page"             gint                  : Read / Write
;;;   "embed-page-setup"         gboolean              : Read / Write
;;;   "has-selection"            gboolean              : Read / Write
;;;   "manual-capabilities"      GtkPrintCapabilities  : Read / Write
;;;   "page-setup"               GtkPageSetup*         : Read / Write
;;;   "print-settings"           GtkPrintSettings*     : Read / Write
;;;   "selected-printer"         GtkPrinter*           : Read
;;;   "support-selection"        gboolean              : Read / Write
;;; 
;;; Description
;;; 
;;; GtkPrintUnixDialog implements a print dialog for platforms which don't
;;; provide a native print dialog, like Unix. It can be used very much like any
;;; other GTK+ dialog, at the cost of the portability offered by the high-level
;;; printing API
;;; 
;;; In order to print something with GtkPrintUnixDialog, you need to use
;;; gtk_print_unix_dialog_get_selected_printer() to obtain a GtkPrinter object
;;; and use it to construct a GtkPrintJob using gtk_print_job_new().
;;; 
;;; GtkPrintUnixDialog uses the following response values:
;;; 
;;; GTK_RESPONSE_OK
;;;     for the "Print" button
;;; 
;;; GTK_RESPONSE_APPLY
;;;     for the "Preview" button
;;; 
;;; GTK_RESPONSE_CANCEL
;;;     for the "Cancel" button
;;; 
;;; Printing support was added in GTK+ 2.10.
;;; 
;;; GtkPrintUnixDialog as GtkBuildable
;;; 
;;; The GtkPrintUnixDialog implementation of the GtkBuildable interface exposes
;;; its notebook internal children with the name "notebook".
;;; 
;;; Example 98. A GtkPrintUnixDialog UI definition fragment.
;;; 
;;; <object class="GtkPrintUnixDialog" id="dialog1">
;;;   <child internal-child="notebook">
;;;     <object class="GtkNotebook" id="notebook">
;;;       <child>
;;;         <object class="GtkLabel" id="tabcontent">
;;;         <property name="label">Content on notebook tab</property>
;;;         </object>
;;;       </child>
;;;       <child type="tab">
;;;         <object class="GtkLabel" id="tablabel">
;;;           <property name="label">Tab label</property>
;;;         </object>
;;;         <packing>
;;;           <property name="tab_expand">False</property>
;;;           <property name="tab_fill">False</property>
;;;         </packing>
;;;       </child>
;;;     </object>
;;;   </child>
;;; </object>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "current-page" property
;;; 
;;;   "current-page"             gint                  : Read / Write
;;; 
;;; The current page in the document.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "embed-page-setup" property
;;; 
;;;   "embed-page-setup"         gboolean              : Read / Write
;;; 
;;; TRUE if page setup combos are embedded in GtkPrintUnixDialog.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-selection" property
;;; 
;;;   "has-selection"            gboolean              : Read / Write
;;; 
;;; Whether the application has a selection.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "manual-capabilities" property
;;; 
;;;   "manual-capabilities"      GtkPrintCapabilities  : Read / Write
;;; 
;;; Capabilities the application can handle.
;;;
;;; ----------------------------------------------------------------------------
;;; The "page-setup" property
;;; 
;;;   "page-setup"               GtkPageSetup*         : Read / Write
;;; 
;;; The GtkPageSetup to use.
;;;
;;; ----------------------------------------------------------------------------
;;; The "print-settings" property
;;; 
;;;   "print-settings"           GtkPrintSettings*     : Read / Write
;;; 
;;; The GtkPrintSettings used for initializing the dialog.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selected-printer" property
;;; 
;;;   "selected-printer"         GtkPrinter*           : Read
;;; 
;;; The GtkPrinter which is selected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "support-selection" property
;;; 
;;;   "support-selection"        gboolean              : Read / Write
;;; 
;;; Whether the dialog supports selection.
;;; 
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPrintUnixDialog
;;; 
;;; struct GtkPrintUnixDialog;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPrintUnixDialog" gtk-print-unix-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_print_unix_dialog_get_type")
  ((current-page
    gtk-print-unix-dialog-current-page
    "current-page" "gint" t t)
   (page-setup
    gtk-print-unix-dialog-page-setup
    "page-setup" "GtkPageSetup" t t)
   (print-settings
    gtk-print-unix-dialog-print-settings
    "print-settings" "GtkPrintSettings" t t)
   (selected-printer
    gtk-print-unix-dialog-selected-printer
    "selected-printer" "GtkPrinter" t nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_new ()
;;; 
;;; GtkWidget * gtk_print_unix_dialog_new (const gchar *title,
;;;                                        GtkWindow *parent);
;;; 
;;; Creates a new GtkPrintUnixDialog.
;;; 
;;; title :
;;;     Title of the dialog, or NULL. [allow-none]
;;; 
;;; parent :
;;;     Transient parent of the dialog, or NULL. [allow-none]
;;; 
;;; Returns :
;;;     a new GtkPrintUnixDialog
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_page_setup ()
;;; 
;;; void gtk_print_unix_dialog_set_page_setup (GtkPrintUnixDialog *dialog,
;;;                                            GtkPageSetup *page_setup);
;;; 
;;; Sets the page setup of the GtkPrintUnixDialog.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; page_setup :
;;;     a GtkPageSetup
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_page_setup ()
;;; 
;;; GtkPageSetup * gtk_print_unix_dialog_get_page_setup
;;;                                                 (GtkPrintUnixDialog *dialog)
;;; 
;;; Gets the page setup that is used by the GtkPrintUnixDialog.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; Returns :
;;;     the page setup of dialog
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_current_page ()
;;; 
;;; void gtk_print_unix_dialog_set_current_page (GtkPrintUnixDialog *dialog,
;;;                                              gint current_page);
;;; 
;;; Sets the current page number. If current_page is not -1, this enables the
;;; current page choice for the range of pages to print.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; current_page :
;;;     the current page number.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_current_page ()
;;; 
;;; gint gtk_print_unix_dialog_get_current_page (GtkPrintUnixDialog *dialog)
;;; 
;;; Gets the current page of the GtkPrintDialog.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; Returns :
;;;     the current page of dialog
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_settings ()
;;; 
;;; void gtk_print_unix_dialog_set_settings (GtkPrintUnixDialog *dialog,
;;;                                          GtkPrintSettings *settings);
;;; 
;;; Sets the GtkPrintSettings for the GtkPrintUnixDialog. Typically, this is
;;; used to restore saved print settings from a previous print operation before
;;; the print dialog is shown.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; settings :
;;;     a GtkPrintSettings, or NULL
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_settings ()
;;; 
;;; GtkPrintSettings * gtk_print_unix_dialog_get_settings
;;;                                                 (GtkPrintUnixDialog *dialog)
;;; 
;;; Gets a new GtkPrintSettings object that represents the current values in
;;; the print dialog. Note that this creates a new object, and you need to
;;; unref it if don't want to keep it.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; Returns :
;;;     a new GtkPrintSettings object with the values from dialog
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_selected_printer ()
;;; 
;;; GtkPrinter * gtk_print_unix_dialog_get_selected_printer
;;;                                                 (GtkPrintUnixDialog *dialog)
;;; 
;;; Gets the currently selected printer.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; Returns :
;;;     the currently selected printer
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_add_custom_tab ()
;;; 
;;; void gtk_print_unix_dialog_add_custom_tab (GtkPrintUnixDialog *dialog,
;;;                                            GtkWidget *child,
;;;                                            GtkWidget *tab_label);
;;; 
;;; Adds a custom tab to the print dialog.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; child :
;;;     the widget to put in the custom tab
;;; 
;;; tab_label :
;;;     the widget to use as tab label
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_support_selection ()
;;; 
;;; void gtk_print_unix_dialog_set_support_selection
;;;                                                 (GtkPrintUnixDialog *dialog,
;;;                                                  gboolean support_selection)
;;; 
;;; Sets whether the print dialog allows user to print a selection.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; support_selection :
;;;     TRUE to allow print selection
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_support_selection ()
;;; 
;;; gboolean gtk_print_unix_dialog_get_support_selection
;;;                                                 (GtkPrintUnixDialog *dialog)
;;; 
;;; Gets the value of "support-selection" property.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; Returns :
;;;     whether the application supports print of selection
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_has_selection ()
;;; 
;;; void gtk_print_unix_dialog_set_has_selection (GtkPrintUnixDialog *dialog,
;;;                                               gboolean has_selection);
;;; 
;;; Sets whether a selection exists.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; has_selection :
;;;     TRUE indicates that a selection exists
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_has_selection ()
;;; 
;;; gboolean gtk_print_unix_dialog_get_has_selection
;;;                                                 (GtkPrintUnixDialog *dialog)
;;; 
;;; Gets the value of "has-selection" property.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; Returns :
;;;     whether there is a selection
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_embed_page_setup ()
;;; 
;;; void gtk_print_unix_dialog_set_embed_page_setup (GtkPrintUnixDialog *dialog,
;;;                                                  gboolean embed);
;;; 
;;; Embed page size combo box and orientation combo box into page setup page.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; embed :
;;;     embed page setup selection
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_embed_page_setup ()
;;; 
;;; gboolean gtk_print_unix_dialog_get_embed_page_setup
;;;                                                 (GtkPrintUnixDialog *dialog)
;;; 
;;; Gets the value of "embed-page-setup" property.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; Returns :
;;;     whether there is a selection
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_page_setup_set ()
;;; 
;;; gboolean gtk_print_unix_dialog_get_page_setup_set
;;;                                                 (GtkPrintUnixDialog *dialog)
;;; 
;;; Gets the page setup that is used by the GtkPrintUnixDialog.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; Returns :
;;;     whether a page setup was set by user.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintCapabilities
;;; 
;;; typedef enum {
;;;   GTK_PRINT_CAPABILITY_PAGE_SET         = 1 << 0,
;;;   GTK_PRINT_CAPABILITY_COPIES           = 1 << 1,
;;;   GTK_PRINT_CAPABILITY_COLLATE          = 1 << 2,
;;;   GTK_PRINT_CAPABILITY_REVERSE          = 1 << 3,
;;;   GTK_PRINT_CAPABILITY_SCALE            = 1 << 4,
;;;   GTK_PRINT_CAPABILITY_GENERATE_PDF     = 1 << 5,
;;;   GTK_PRINT_CAPABILITY_GENERATE_PS      = 1 << 6,
;;;   GTK_PRINT_CAPABILITY_PREVIEW          = 1 << 7,
;;;   GTK_PRINT_CAPABILITY_NUMBER_UP        = 1 << 8,
;;;   GTK_PRINT_CAPABILITY_NUMBER_UP_LAYOUT = 1 << 9
;;; } GtkPrintCapabilities;
;;; 
;;; An enum for specifying which features the print dialog should offer. If
;;; neither GTK_PRINT_CAPABILITY_GENERATE_PDF nor
;;; GTK_PRINT_CAPABILITY_GENERATE_PS is specified, GTK+ assumes that all
;;; formats are supported.
;;; 
;;; GTK_PRINT_CAPABILITY_PAGE_SET
;;;     Print dialog will offer printing even/odd pages.
;;; 
;;; GTK_PRINT_CAPABILITY_COPIES
;;;     Print dialog will allow to print multiple copies.
;;; 
;;; GTK_PRINT_CAPABILITY_COLLATE
;;;     Print dialog will allow to collate multiple copies.
;;; 
;;; GTK_PRINT_CAPABILITY_REVERSE
;;;     Print dialog will allow to print pages in reverse order.
;;; 
;;; GTK_PRINT_CAPABILITY_SCALE
;;;     Print dialog will allow to scale the output.
;;; 
;;; GTK_PRINT_CAPABILITY_GENERATE_PDF
;;;     The program will send the document to the printer in PDF format
;;; 
;;; GTK_PRINT_CAPABILITY_GENERATE_PS
;;;     The program will send the document to the printer in Postscript format
;;; 
;;; GTK_PRINT_CAPABILITY_PREVIEW
;;;     Print dialog will offer a preview
;;; 
;;; GTK_PRINT_CAPABILITY_NUMBER_UP
;;;     Print dialog will offer printing multiple pages per sheet. Since 2.12
;;; 
;;; GTK_PRINT_CAPABILITY_NUMBER_UP_LAYOUT
;;;     Print dialog will allow to rearrange pages when printing multiple pages
;;;     per sheet. Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_manual_capabilities ()
;;; 
;;; void gtk_print_unix_dialog_set_manual_capabilities
;;;                                          (GtkPrintUnixDialog *dialog,
;;;                                           GtkPrintCapabilities capabilities)
;;; 
;;; This lets you specify the printing capabilities your application supports.
;;; For instance, if you can handle scaling the output then you pass
;;; GTK_PRINT_CAPABILITY_SCALE. If you don't pass that, then the dialog will
;;; only let you select the scale if the printing system automatically handles
;;; scaling.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; capabilities :
;;;     the printing capabilities of your application
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_manual_capabilities ()
;;; 
;;; GtkPrintCapabilities gtk_print_unix_dialog_get_manual_capabilities
;;;                                                 (GtkPrintUnixDialog *dialog)
;;; 
;;; Gets the value of "manual-capabilities" property.
;;; 
;;; dialog :
;;;     a GtkPrintUnixDialog
;;; 
;;; Returns :
;;;     the printing capabilities
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.print-unix-dialog.lisp ---------------------------------
