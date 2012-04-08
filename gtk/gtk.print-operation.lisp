;;; ----------------------------------------------------------------------------
;;; gtk.print-operation.lisp
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
;;; GtkPrintOperation
;;; 
;;; High-level Printing API
;;; 
;;; Synopsis
;;;
;;;     GtkPrintOperationPreview
;;;     GtkPrintOperation
;;;     GtkPrintStatus
;;;     GtkPrintOperationAction
;;;     GtkPrintOperationResult
;;;     GtkPrintError
;;;
;;;     GTK_PRINT_ERROR
;;;
;;;     gtk_print_operation_new
;;;     gtk_print_operation_set_allow_async
;;;     gtk_print_operation_get_error
;;;     gtk_print_operation_set_default_page_setup
;;;     gtk_print_operation_get_default_page_setup
;;;     gtk_print_operation_set_print_settings
;;;     gtk_print_operation_get_print_settings
;;;     gtk_print_operation_set_job_name
;;;     gtk_print_operation_set_n_pages
;;;     gtk_print_operation_get_n_pages_to_print
;;;     gtk_print_operation_set_current_page
;;;     gtk_print_operation_set_use_full_page
;;;     gtk_print_operation_set_unit
;;;     gtk_print_operation_set_export_filename
;;;     gtk_print_operation_set_show_progress
;;;     gtk_print_operation_set_track_print_status
;;;     gtk_print_operation_set_custom_tab_label
;;;     gtk_print_operation_run
;;;     gtk_print_operation_cancel
;;;     gtk_print_operation_draw_page_finish
;;;     gtk_print_operation_set_defer_drawing
;;;     gtk_print_operation_get_status
;;;     gtk_print_operation_get_status_string
;;;     gtk_print_operation_is_finished
;;;     gtk_print_operation_set_support_selection
;;;     gtk_print_operation_get_support_selection
;;;     gtk_print_operation_set_has_selection
;;;     gtk_print_operation_get_has_selection
;;;     gtk_print_operation_set_embed_page_setup
;;;     gtk_print_operation_get_embed_page_setup
;;;     gtk_print_run_page_setup_dialog
;;;     gtk_print_run_page_setup_dialog_async
;;;
;;;     gtk_print_operation_preview_end_preview
;;;     gtk_print_operation_preview_is_selected
;;;     gtk_print_operation_preview_render_page
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkPrintOperation
;;; 
;;;   GInterface
;;;    +----GtkPrintOperationPreview
;;; 
;;; Prerequisites
;;; 
;;; GtkPrintOperationPreview requires GObject.
;;;
;;; Implemented Interfaces
;;; 
;;; GtkPrintOperation implements GtkPrintOperationPreview.
;;;
;;; Known Implementations
;;; 
;;; GtkPrintOperationPreview is implemented by GtkPrintOperation.
;;;
;;; Properties
;;; 
;;;   "allow-async"              gboolean              : Read / Write
;;;   "current-page"             gint                  : Read / Write
;;;   "custom-tab-label"         gchar*                : Read / Write
;;;   "default-page-setup"       GtkPageSetup*         : Read / Write
;;;   "embed-page-setup"         gboolean              : Read / Write
;;;   "export-filename"          gchar*                : Read / Write
;;;   "has-selection"            gboolean              : Read / Write
;;;   "job-name"                 gchar*                : Read / Write
;;;   "n-pages"                  gint                  : Read / Write
;;;   "n-pages-to-print"         gint                  : Read
;;;   "print-settings"           GtkPrintSettings*     : Read / Write
;;;   "show-progress"            gboolean              : Read / Write
;;;   "status"                   GtkPrintStatus        : Read
;;;   "status-string"            gchar*                : Read
;;;   "support-selection"        gboolean              : Read / Write
;;;   "track-print-status"       gboolean              : Read / Write
;;;   "unit"                     GtkUnit               : Read / Write
;;;   "use-full-page"            gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "begin-print"                                    : Run Last
;;;   "create-custom-widget"                           : Run Last
;;;   "custom-widget-apply"                            : Run Last
;;;   "done"                                           : Run Last
;;;   "draw-page"                                      : Run Last
;;;   "end-print"                                      : Run Last
;;;   "paginate"                                       : Run Last
;;;   "preview"                                        : Run Last
;;;   "request-page-setup"                             : Run Last
;;;   "status-changed"                                 : Run Last
;;;   "update-custom-widget"                           : Run Last
;;;   "got-page-size"                                  : Run Last
;;;   "ready"                                          : Run Last
;;; 
;;; Description
;;; 
;;; GtkPrintOperation is the high-level, portable printing API. It looks a bit
;;; different than other GTK+ dialogs such as the GtkFileChooser, since some
;;; platforms don't expose enough infrastructure to implement a good print
;;; dialog. On such platforms, GtkPrintOperation uses the native print dialog.
;;; On platforms which do not provide a native print dialog, GTK+ uses its own,
;;; see GtkPrintUnixDialog.
;;; 
;;; The typical way to use the high-level printing API is to create a
;;; GtkPrintOperation object with gtk_print_operation_new() when the user
;;; selects to print. Then you set some properties on it, e.g. the page size,
;;; any GtkPrintSettings from previous print operations, the number of pages,
;;; the current page, etc.
;;; 
;;; Then you start the print operation by calling gtk_print_operation_run(). It
;;; will then show a dialog, let the user select a printer and options. When the
;;; user finished the dialog various signals will be emitted on the
;;; GtkPrintOperation, the main one being "draw-page", which you are supposed
;;; to catch and render the page on the provided GtkPrintContext using Cairo.
;;; 
;;; Example 95. The high-level printing API
;;; 
;;; static GtkPrintSettings *settings = NULL;
;;; 
;;; static void
;;; do_print (void)
;;; {
;;;   GtkPrintOperation *print;
;;;   GtkPrintOperationResult res;
;;; 
;;;   print = gtk_print_operation_new ();
;;; 
;;;   if (settings != NULL)
;;;     gtk_print_operation_set_print_settings (print, settings);
;;; 
;;;   g_signal_connect (print, "begin_print", G_CALLBACK (begin_print), NULL);
;;;   g_signal_connect (print, "draw_page", G_CALLBACK (draw_page), NULL);
;;; 
;;;   res = gtk_print_operation_run (print,
;;;                                  GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
;;;                                  GTK_WINDOW (main_window), NULL);
;;; 
;;;   if (res == GTK_PRINT_OPERATION_RESULT_APPLY)
;;;     {
;;;       if (settings != NULL)
;;;         g_object_unref (settings);
;;;       settings = g_object_ref (gtk_print_operation_get_print_settings (print));
;;;     }
;;; 
;;;   g_object_unref (print);
;;; }
;;; 
;;; By default GtkPrintOperation uses an external application to do print
;;; preview. To implement a custom print preview, an application must connect
;;; to the preview signal. The functions
;;; gtk_print_operation_print_preview_render_page(),
;;; gtk_print_operation_preview_end_preview() and
;;; gtk_print_operation_preview_is_selected() are useful when implementing a
;;; print preview.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "allow-async" property
;;; 
;;;   "allow-async"              gboolean              : Read / Write
;;; 
;;; Determines whether the print operation may run asynchronously or not.
;;; 
;;; Some systems don't support asynchronous printing, but those that do will
;;; return GTK_PRINT_OPERATION_RESULT_IN_PROGRESS as the status, and emit the
;;; "done" signal when the operation is actually done.
;;; 
;;; The Windows port does not support asynchronous operation at all (this is
;;; unlikely to change). On other platforms, all actions except for
;;; GTK_PRINT_OPERATION_ACTION_EXPORT support asynchronous operation.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "current-page" property
;;; 
;;;   "current-page"             gint                  : Read / Write
;;; 
;;; The current page in the document.
;;; 
;;; If this is set before gtk_print_operation_run(), the user will be able to
;;; select to print only the current page.
;;; 
;;; Note that this only makes sense for pre-paginated documents.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "custom-tab-label" property
;;; 
;;;   "custom-tab-label"         gchar*                : Read / Write
;;; 
;;; Used as the label of the tab containing custom widgets. Note that this
;;; property may be ignored on some platforms.
;;; 
;;; If this is NULL, GTK+ uses a default label.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "default-page-setup" property
;;; 
;;;   "default-page-setup"       GtkPageSetup*         : Read / Write
;;; 
;;; The GtkPageSetup used by default.
;;; 
;;; This page setup will be used by gtk_print_operation_run(), but it can be
;;; overridden on a per-page basis by connecting to the "request-page-setup"
;;; signal.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "embed-page-setup" property
;;; 
;;;   "embed-page-setup"         gboolean              : Read / Write
;;; 
;;; If TRUE, page size combo box and orientation combo box are embedded into
;;; page setup page.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "export-filename" property
;;; 
;;;   "export-filename"          gchar*                : Read / Write
;;; 
;;; The name of a file to generate instead of showing the print dialog.
;;; Currently, PDF is the only supported format.
;;; 
;;; The intended use of this property is for implementing "Export to PDF"
;;; actions.
;;; 
;;; "Print to PDF" support is independent of this and is done by letting the
;;; user pick the "Print to PDF" item from the list of printers in the print
;;; dialog.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-selection" property
;;; 
;;;   "has-selection"            gboolean              : Read / Write
;;; 
;;; Determines whether there is a selection in your application. This can allow
;;; your application to print the selection. This is typically used to make a
;;; "Selection" button sensitive.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "job-name" property
;;; 
;;;   "job-name"                 gchar*                : Read / Write
;;; 
;;; A string used to identify the job (e.g. in monitoring applications like
;;; eggcups).
;;; 
;;; If you don't set a job name, GTK+ picks a default one by numbering
;;; successive print jobs.
;;; 
;;; Default value: ""
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "n-pages" property
;;; 
;;;   "n-pages"                  gint                  : Read / Write
;;; 
;;; The number of pages in the document.
;;; 
;;; This must be set to a positive number before the rendering starts. It may
;;; be set in a "begin-print" signal hander.
;;; 
;;; Note that the page numbers passed to the "request-page-setup" and
;;; "draw-page" signals are 0-based, i.e. if the user chooses to print all
;;; pages, the last ::draw-page signal will be for page n_pages - 1.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "n-pages-to-print" property
;;; 
;;;   "n-pages-to-print"         gint                  : Read
;;; 
;;; The number of pages that will be printed.
;;; 
;;; Note that this value is set during print preparation phase
;;; (GTK_PRINT_STATUS_PREPARING), so this value should never be get before the
;;; data generation phase (GTK_PRINT_STATUS_GENERATING_DATA). You can connect to
;;; the "status-changed" signal and call
;;; gtk_print_operation_get_n_pages_to_print() when print status is
;;; GTK_PRINT_STATUS_GENERATING_DATA. This is typically used to track the
;;; progress of print operation.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "print-settings" property
;;; 
;;;   "print-settings"           GtkPrintSettings*     : Read / Write
;;; 
;;; The GtkPrintSettings used for initializing the dialog.
;;; 
;;; Setting this property is typically used to re-establish print settings from
;;; a previous print operation, see gtk_print_operation_run().
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-progress" property
;;; 
;;;   "show-progress"            gboolean              : Read / Write
;;; 
;;; Determines whether to show a progress dialog during the print operation.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "status" property
;;; 
;;;   "status"                   GtkPrintStatus        : Read
;;; 
;;; The status of the print operation.
;;; 
;;; Default value: GTK_PRINT_STATUS_INITIAL
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "status-string" property
;;; 
;;;   "status-string"            gchar*                : Read
;;; 
;;; A string representation of the status of the print operation. The string is
;;; translated and suitable for displaying the print status e.g. in a
;;; GtkStatusbar.
;;; 
;;; See the "status" property for a status value that is suitable for
;;; programmatic use.
;;; 
;;; Default value: ""
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "support-selection" property
;;; 
;;;   "support-selection"        gboolean              : Read / Write
;;; 
;;; If TRUE, the print operation will support print of selection. This allows
;;; the print dialog to show a "Selection" button.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "track-print-status" property
;;; 
;;;   "track-print-status"       gboolean              : Read / Write
;;; 
;;; If TRUE, the print operation will try to continue report on the status of
;;; the print job in the printer queues and printer. This can allow your
;;; application to show things like "out of paper" issues, and when the print
;;; job actually reaches the printer. However, this is often implemented using
;;; polling, and should not be enabled unless needed.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "unit" property
;;; 
;;;   "unit"                     GtkUnit               : Read / Write
;;; 
;;; The transformation for the cairo context obtained from GtkPrintContext is
;;; set up in such a way that distances are measured in units of unit.
;;; 
;;; Default value: GTK_UNIT_PIXEL
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-full-page" property
;;; 
;;;   "use-full-page"            gboolean              : Read / Write
;;; 
;;; If TRUE, the transformation for the cairo context obtained from
;;; GtkPrintContext puts the origin at the top left corner of the page (which
;;; may not be the top left corner of the sheet, depending on page orientation
;;; and the number of pages per sheet). Otherwise, the origin is at the top left
;;; corner of the imageable area (i.e. inside the margins).
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "begin-print" signal
;;; 
;;; void user_function (GtkPrintOperation *operation,
;;;                     GtkPrintContext   *context,
;;;                     gpointer           user_data)      : Run Last
;;; 
;;; Emitted after the user has finished changing print settings in the dialog,
;;; before the actual rendering starts.
;;; 
;;; A typical use for ::begin-print is to use the parameters from the
;;; GtkPrintContext and paginate the document accordingly, and then set the
;;; number of pages with gtk_print_operation_set_n_pages().
;;; 
;;; operation :
;;;     the GtkPrintOperation on which the signal was emitted
;;; 
;;; context :
;;;     the GtkPrintContext for the current operation
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "create-custom-widget" signal
;;; 
;;; GObject* user_function (GtkPrintOperation *operation,
;;;                         gpointer           user_data)      : Run Last
;;; 
;;; Emitted when displaying the print dialog. If you return a widget in a
;;; handler for this signal it will be added to a custom tab in the print
;;; dialog. You typically return a container widget with multiple widgets in it.
;;; 
;;; The print dialog owns the returned widget, and its lifetime is not
;;; controlled by the application. However, the widget is guaranteed to stay
;;; around until the "custom-widget-apply" signal is emitted on the operation.
;;; Then you can read out any information you need from the widgets.
;;; 
;;; operation :
;;;     the GtkPrintOperation on which the signal was emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     a custom widget that gets embedded in the print dialog, or NULL
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "custom-widget-apply" signal
;;; 
;;; void user_function (GtkPrintOperation *operation,
;;;                     GtkWidget         *widget,
;;;                     gpointer           user_data)      : Run Last
;;; 
;;; Emitted right before "begin-print" if you added a custom widget in the
;;; "create-custom-widget" handler. When you get this signal you should read
;;; the information from the custom widgets, as the widgets are not guaraneed
;;; to be around at a later time.
;;; 
;;; operation :
;;;     the GtkPrintOperation on which the signal was emitted
;;; 
;;; widget :
;;;     the custom widget added in create-custom-widget
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "done" signal
;;; 
;;; void user_function (GtkPrintOperation      *operation,
;;;                     GtkPrintOperationResult result,
;;;                     gpointer                user_data)      : Run Last
;;; 
;;; Emitted when the print operation run has finished doing everything required
;;; for printing.
;;; 
;;; result gives you information about what happened during the run. If result
;;; is GTK_PRINT_OPERATION_RESULT_ERROR then you can call
;;; gtk_print_operation_get_error() for more information.
;;; 
;;; If you enabled print status tracking then gtk_print_operation_is_finished()
;;; may still return FALSE after "done" was emitted.
;;; 
;;; operation :
;;;     the GtkPrintOperation on which the signal was emitted
;;; 
;;; result :
;;;     the result of the print operation
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "draw-page" signal
;;; 
;;; void user_function (GtkPrintOperation *operation,
;;;                     GtkPrintContext   *context,
;;;                     gint               page_nr,
;;;                     gpointer           user_data)      : Run Last
;;; 
;;; Emitted for every page that is printed. The signal handler must render
;;; the page_nr's page onto the cairo context obtained from context using
;;; gtk_print_context_get_cairo_context().
;;; 
;;; static void
;;; draw_page (GtkPrintOperation *operation,
;;;            GtkPrintContext   *context,
;;;            gint               page_nr,
;;;            gpointer           user_data)
;;; {
;;;   cairo_t *cr;
;;;   PangoLayout *layout;
;;;   gdouble width, text_height;
;;;   gint layout_height;
;;;   PangoFontDescription *desc;
;;;   
;;;   cr = gtk_print_context_get_cairo_context (context);
;;;   width = gtk_print_context_get_width (context);
;;;   
;;;   cairo_rectangle (cr, 0, 0, width, HEADER_HEIGHT);
;;;   
;;;   cairo_set_source_rgb (cr, 0.8, 0.8, 0.8);
;;;   cairo_fill (cr);
;;;   
;;;   layout = gtk_print_context_create_pango_layout (context);
;;;   
;;;   desc = pango_font_description_from_string ("sans 14");
;;;   pango_layout_set_font_description (layout, desc);
;;;   pango_font_description_free (desc);
;;;   
;;;   pango_layout_set_text (layout, "some text", -1);
;;;   pango_layout_set_width (layout, width * PANGO_SCALE);
;;;   pango_layout_set_alignment (layout, PANGO_ALIGN_CENTER);
;;;                   
;;;   pango_layout_get_size (layout, NULL, &layout_height);
;;;   text_height = (gdouble)layout_height / PANGO_SCALE;
;;;   
;;;   cairo_move_to (cr, width / 2,  (HEADER_HEIGHT - text_height) / 2);
;;;   pango_cairo_show_layout (cr, layout);
;;;   
;;;   g_object_unref (layout);
;;; }
;;; 
;;; Use gtk_print_operation_set_use_full_page() and
;;; gtk_print_operation_set_unit() before starting the print operation to set
;;; up the transformation of the cairo context according to your needs.
;;; 
;;; operation :
;;;     the GtkPrintOperation on which the signal was emitted
;;; 
;;; context :
;;;     the GtkPrintContext for the current operation
;;; 
;;; page_nr :
;;;     the number of the currently printed page (0-based)
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "end-print" signal
;;; 
;;; void user_function (GtkPrintOperation *operation,
;;;                     GtkPrintContext   *context,
;;;                     gpointer           user_data)      : Run Last
;;; 
;;; Emitted after all pages have been rendered. A handler for this signal can
;;; clean up any resources that have been allocated in the "begin-print"
;;; handler.
;;; 
;;; operation :
;;;     the GtkPrintOperation on which the signal was emitted
;;; 
;;; context :
;;;     the GtkPrintContext for the current operation
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "paginate" signal
;;; 
;;; gboolean user_function (GtkPrintOperation *operation,
;;;                         GtkPrintContext   *context,
;;;                         gpointer           user_data)      : Run Last
;;; 
;;; Emitted after the "begin-print" signal, but before the actual rendering
;;; starts. It keeps getting emitted until a connected signal handler returns
;;; TRUE.
;;; 
;;; The ::paginate signal is intended to be used for paginating a document in
;;; small chunks, to avoid blocking the user interface for a long time. The
;;; signal handler should update the number of pages using
;;; gtk_print_operation_set_n_pages(), and return TRUE if the document has been
;;; completely paginated.
;;; 
;;; If you don't need to do pagination in chunks, you can simply do it all in
;;; the ::begin-print handler, and set the number of pages from there.
;;; 
;;; operation :
;;;     the GtkPrintOperation on which the signal was emitted
;;; 
;;; context :
;;;     the GtkPrintContext for the current operation
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if pagination is complete
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "preview" signal
;;; 
;;; gboolean user_function (GtkPrintOperation        *operation,
;;;                         GtkPrintOperationPreview *preview,
;;;                         GtkPrintContext          *context,
;;;                         GtkWindow                *parent,
;;;                         gpointer                  user_data)      : Run Last
;;; 
;;; Gets emitted when a preview is requested from the native dialog.
;;; 
;;; The default handler for this signal uses an external viewer application to
;;; preview.
;;; 
;;; To implement a custom print preview, an application must return TRUE from
;;; its handler for this signal. In order to use the provided context for the
;;; preview implementation, it must be given a suitable cairo context with
;;; gtk_print_context_set_cairo_context().
;;; 
;;; The custom preview implementation can use
;;; gtk_print_operation_preview_is_selected() and
;;; gtk_print_operation_preview_render_page() to find pages which are selected
;;; for print and render them. The preview must be finished by calling
;;; gtk_print_operation_preview_end_preview() (typically in response to the
;;; user clicking a close button).
;;; 
;;; operation :
;;;     the GtkPrintOperation on which the signal was emitted
;;; 
;;; preview :
;;;     the GtkPrintPreviewOperation for the current operation
;;; 
;;; context :
;;;     the GtkPrintContext that will be used
;;; 
;;; parent :
;;;     the GtkWindow to use as window parent, or NULL
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if the listener wants to take over control of the preview
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "request-page-setup" signal
;;; 
;;; void user_function (GtkPrintOperation *operation,
;;;                     GtkPrintContext   *context,
;;;                     gint               page_nr,
;;;                     GtkPageSetup      *setup,
;;;                     gpointer           user_data)      : Run Last
;;; 
;;; Emitted once for every page that is printed, to give the application a
;;; chance to modify the page setup. Any changes done to setup will be in force
;;; only for printing this page.
;;; 
;;; operation :
;;;     the GtkPrintOperation on which the signal was emitted
;;; 
;;; context :
;;;     the GtkPrintContext for the current operation
;;; 
;;; page_nr :
;;;     the number of the currently printed page (0-based)
;;; 
;;; setup :
;;;     the GtkPageSetup
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "status-changed" signal
;;; 
;;; void user_function (GtkPrintOperation *operation,
;;;                     gpointer           user_data)      : Run Last
;;; 
;;; Emitted at between the various phases of the print operation. See
;;; GtkPrintStatus for the phases that are being discriminated. Use
;;; gtk_print_operation_get_status() to find out the current status.
;;; 
;;; operation :
;;;     the GtkPrintOperation on which the signal was emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "update-custom-widget" signal
;;; 
;;; void user_function (GtkPrintOperation *operation,
;;;                     GtkWidget         *widget,
;;;                     GtkPageSetup      *setup,
;;;                     GtkPrintSettings  *settings,
;;;                     gpointer           user_data)      : Run Last
;;; 
;;; Emitted after change of selected printer. The actual page setup and print
;;; settings are passed to the custom widget, which can actualize itself
;;; according to this change.
;;; 
;;; operation :
;;;     the GtkPrintOperation on which the signal was emitted
;;; 
;;; widget :
;;;     the custom widget added in create-custom-widget
;;; 
;;; setup :
;;;     actual page setup
;;; 
;;; settings :
;;;     actual print settings
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "got-page-size" signal
;;; 
;;; void user_function (GtkPrintOperationPreview *preview,
;;;                     GtkPrintContext          *context,
;;;                     GtkPageSetup             *page_setup,
;;;                     gpointer                  user_data)       : Run Last
;;; 
;;; The ::got-page-size signal is emitted once for each page that gets rendered
;;; to the preview.
;;; 
;;; A handler for this signal should update the context according to page_setup
;;; and set up a suitable cairo context, using
;;; gtk_print_context_set_cairo_context().
;;; 
;;; preview :
;;;     the object on which the signal is emitted
;;; 
;;; context :
;;;     the current GtkPrintContext
;;; 
;;; page_setup :
;;;     the GtkPageSetup for the current page
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;;
;;; ----------------------------------------------------------------------------
;;; The "ready" signal
;;; 
;;; void user_function (GtkPrintOperationPreview *preview,
;;;                     GtkPrintContext          *context,
;;;                     gpointer                  user_data)      : Run Last
;;; 
;;; The ::ready signal gets emitted once per preview operation, before the
;;; first page is rendered.
;;; 
;;; A handler for this signal can be used for setup tasks.
;;; 
;;; preview :
;;;     the object on which the signal is emitted
;;; 
;;; context :
;;;     the current GtkPrintContext
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintOperationPreview
;;; 
;;; typedef struct _GtkPrintOperationPreview GtkPrintOperationPreview;
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkPrintOperationPreview" gtk-print-operation-preview
  (:export t
   :type-initializer "gtk_print_operation_preview_get_type"))

;;; ----------------------------------------------------------------------------
;;; struct GtkPrintOperation
;;; 
;;; struct GtkPrintOperation;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPrintOperation" gtk-print-operation
  (:superclass g-object
    :export t
    :interfaces ("GtkPrintOperationPreview")
    :type-initializer "gtk_print_operation_get_type")
  ((allow-async
    gtk-print-operation-allow-async
    "allow-async" "gboolean" t t)
   (current-page
    gtk-print-operation-current-page
    "current-page" "gint" t t)
   (custom-tab-label
    gtk-print-operation-custom-tab-label
    "custom-tab-label" "gchararray" t t)
   (default-page-setup
    gtk-print-operation-default-page-setup
    "default-page-setup" "GtkPageSetup" t t)
   (export-filename
    gtk-print-operation-export-filename
    "export-filename" "gchararray" t t)
   (job-name
    gtk-print-operation-job-name
    "job-name" "gchararray" t t)
   (n-pages
    gtk-print-operation-n-pages
    "n-pages" "gint" t t)
   (print-settings
    gtk-print-operation-print-settings
    "print-settings" "GtkPrintSettings" t t)
   (show-progress
    gtk-print-operation-show-progress
    "show-progress" "gboolean" t t)
   (status
    gtk-print-operation-status
    "status" "GtkPrintStatus" t nil)
   (status-string
    gtk-print-operation-status-string
    "status-string" "gchararray" t nil)
   (track-print-status
    gtk-print-operation-track-print-status
    "track-print-status" "gboolean" t t)
   (unit
    gtk-print-operation-unit
    "unit" "GtkUnit" t t)
   (use-full-page
    gtk-print-operation-use-full-page
    "use-full-page" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintStatus
;;; 
;;; typedef enum {
;;;   GTK_PRINT_STATUS_INITIAL,
;;;   GTK_PRINT_STATUS_PREPARING,
;;;   GTK_PRINT_STATUS_GENERATING_DATA,
;;;   GTK_PRINT_STATUS_SENDING_DATA,
;;;   GTK_PRINT_STATUS_PENDING,
;;;   GTK_PRINT_STATUS_PENDING_ISSUE,
;;;   GTK_PRINT_STATUS_PRINTING,
;;;   GTK_PRINT_STATUS_FINISHED,
;;;   GTK_PRINT_STATUS_FINISHED_ABORTED
;;; } GtkPrintStatus;
;;; 
;;; The status gives a rough indication of the completion of a running print
;;; operation.
;;; 
;;; GTK_PRINT_STATUS_INITIAL
;;;     The printing has not started yet; this status is set initially, and
;;;     while the print dialog is shown.
;;; 
;;; GTK_PRINT_STATUS_PREPARING
;;;     This status is set while the begin-print signal is emitted and during
;;;     pagination.
;;; 
;;; GTK_PRINT_STATUS_GENERATING_DATA
;;;     This status is set while the pages are being rendered.
;;; 
;;; GTK_PRINT_STATUS_SENDING_DATA
;;;     The print job is being sent off to the printer.
;;; 
;;; GTK_PRINT_STATUS_PENDING
;;;     The print job has been sent to the printer, but is not printed for some
;;;     reason, e.g. the printer may be stopped.
;;; 
;;; GTK_PRINT_STATUS_PENDING_ISSUE
;;;     Some problem has occurred during printing, e.g. a paper jam.
;;; 
;;; GTK_PRINT_STATUS_PRINTING
;;;     The printer is processing the print job.
;;; 
;;; GTK_PRINT_STATUS_FINISHED
;;;     The printing has been completed successfully.
;;; 
;;; GTK_PRINT_STATUS_FINISHED_ABORTED
;;;     The printing has been aborted.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintStatus" gtk-print-status
  (:export t
   :type-initializer "gtk_print_status_get_type")
  (:initial 0)
  (:preparing 1)
  (:generating-data 2)
  (:sending-data 3)
  (:pending 4)
  (:pending-issue 5)
  (:printing 6)
  (:finished 7)
  (:finished-aborted 8))

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintOperationAction
;;; 
;;; typedef enum {
;;;   GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
;;;   GTK_PRINT_OPERATION_ACTION_PRINT,
;;;   GTK_PRINT_OPERATION_ACTION_PREVIEW,
;;;   GTK_PRINT_OPERATION_ACTION_EXPORT
;;; } GtkPrintOperationAction;
;;; 
;;; The action parameter to gtk_print_operation_run() determines what action
;;; the print operation should perform.
;;; 
;;; GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG
;;;     Show the print dialog.
;;; 
;;; GTK_PRINT_OPERATION_ACTION_PRINT
;;;     Start to print without showing the print dialog, based on the current
;;;     print settings.
;;; 
;;; GTK_PRINT_OPERATION_ACTION_PREVIEW
;;;     Show the print preview.
;;; 
;;; GTK_PRINT_OPERATION_ACTION_EXPORT
;;;     Export to a file. This requires the export-filename property to be set.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintOperationAction" gtk-print-operation-action
  (:export t
   :type-initializer "gtk_print_operation_action_get_type")
  (:print-dialog 0)
  (:print 1)
  (:preview 2)
  (:export 3))

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintOperationResult
;;; 
;;; typedef enum {
;;;   GTK_PRINT_OPERATION_RESULT_ERROR,
;;;   GTK_PRINT_OPERATION_RESULT_APPLY,
;;;   GTK_PRINT_OPERATION_RESULT_CANCEL,
;;;   GTK_PRINT_OPERATION_RESULT_IN_PROGRESS
;;; } GtkPrintOperationResult;
;;; 
;;; A value of this type is returned by gtk_print_operation_run().
;;; 
;;; GTK_PRINT_OPERATION_RESULT_ERROR
;;;     An error has occured.
;;; 
;;; GTK_PRINT_OPERATION_RESULT_APPLY
;;;     The print settings should be stored.
;;; 
;;; GTK_PRINT_OPERATION_RESULT_CANCEL
;;;     The print operation has been canceled, the print settings should not be
;;;     stored.
;;; 
;;; GTK_PRINT_OPERATION_RESULT_IN_PROGRESS
;;;     The print operation is not complete yet. This value will only be
;;;     returned when running asynchronously.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintOperationResult" gtk-print-operation-result
  (:export t
   :type-initializer "gtk_print_operation_result_get_type")
  (:error 0)
  (:apply 1)
  (:cancel 2)
  (:in-progress 3))

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintError
;;; 
;;; typedef enum {
;;;   GTK_PRINT_ERROR_GENERAL,
;;;   GTK_PRINT_ERROR_INTERNAL_ERROR,
;;;   GTK_PRINT_ERROR_NOMEM,
;;;   GTK_PRINT_ERROR_INVALID_FILE
;;; } GtkPrintError;
;;; 
;;; Error codes that identify various errors that can occur while using the
;;; GTK+ printing support.
;;; 
;;; GTK_PRINT_ERROR_GENERAL
;;;     An unspecified error occurred.
;;; 
;;; GTK_PRINT_ERROR_INTERNAL_ERROR
;;;     An internal error occurred.
;;; 
;;; GTK_PRINT_ERROR_NOMEM
;;;     A memory allocation failed.
;;; 
;;; GTK_PRINT_ERROR_INVALID_FILE
;;;     An error occurred while loading a page setup or paper size from a key
;;;     file.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintError" gtk-print-error
  (:export t
   :type-initializer "gtk_print_error_get_type")
  (:general 0)
  (:internal-error 1)
  (:nomem 2)
  (:invalid-file 3))

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_ERROR
;;; 
;;; #define GTK_PRINT_ERROR gtk_print_error_quark ()
;;; 
;;; The error domain for GtkPrintError errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_new ()
;;; 
;;; GtkPrintOperation * gtk_print_operation_new (void);
;;; 
;;; Creates a new GtkPrintOperation.
;;; 
;;; Returns :
;;;     a new GtkPrintOperation
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_allow_async ()
;;; 
;;; void gtk_print_operation_set_allow_async (GtkPrintOperation *op,
;;;                                           gboolean allow_async);
;;; 
;;; Sets whether the gtk_print_operation_run() may return before the print
;;; operation is completed. Note that some platforms may not allow asynchronous
;;; operation.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; allow_async :
;;;     TRUE to allow asynchronous operation
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_error ()
;;; 
;;; void gtk_print_operation_get_error (GtkPrintOperation *op, GError **error);
;;; 
;;; Call this when the result of a print operation is
;;; GTK_PRINT_OPERATION_RESULT_ERROR, either as returned by
;;; gtk_print_operation_run(), or in the "done" signal handler. The returned
;;; GError will contain more details on what went wrong.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; error :
;;;     return location for the error
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_default_page_setup ()
;;; 
;;; void gtk_print_operation_set_default_page_setup
;;;                                           (GtkPrintOperation *op,
;;;                                            GtkPageSetup *default_page_setup)
;;; 
;;; Makes default_page_setup the default page setup for op.
;;; 
;;; This page setup will be used by gtk_print_operation_run(), but it can be
;;; overridden on a per-page basis by connecting to the "request-page-setup"
;;; signal.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; default_page_setup :
;;;     a GtkPageSetup, or NULL
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_default_page_setup ()
;;; 
;;; GtkPageSetup * gtk_print_operation_get_default_page_setup
;;;                                                      (GtkPrintOperation *op)
;;; 
;;; Returns the default page setup, see
;;; gtk_print_operation_set_default_page_setup().
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Returns :
;;;     the default page setup
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_print_settings ()
;;; 
;;; void gtk_print_operation_set_print_settings
;;;                                           (GtkPrintOperation *op,
;;;                                            GtkPrintSettings *print_settings)
;;; 
;;; Sets the print settings for op. This is typically used to re-establish print
;;; settings from a previous print operation, see gtk_print_operation_run().
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; print_settings :
;;;     GtkPrintSettings
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_print_settings ()
;;; 
;;; GtkPrintSettings * gtk_print_operation_get_print_settings
;;;                                                      (GtkPrintOperation *op)
;;; 
;;; Returns the current print settings.
;;; 
;;; Note that the return value is NULL until either
;;; gtk_print_operation_set_print_settings() or gtk_print_operation_run() have
;;; been called.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Returns :
;;;     the current print settings of op
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_job_name ()
;;; 
;;; void gtk_print_operation_set_job_name (GtkPrintOperation *op,
;;;                                        const gchar *job_name);
;;; 
;;; Sets the name of the print job. The name is used to identify the job (e.g.
;;; in monitoring applications like eggcups).
;;; 
;;; If you don't set a job name, GTK+ picks a default one by numbering
;;; successive print jobs.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; job_name :
;;;     a string that identifies the print job
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_n_pages ()
;;; 
;;; void gtk_print_operation_set_n_pages (GtkPrintOperation *op, gint n_pages)
;;; 
;;; Sets the number of pages in the document.
;;; 
;;; This must be set to a positive number before the rendering starts. It may be
;;; set in a "begin-print" signal hander.
;;; 
;;; Note that the page numbers passed to the "request-page-setup" and
;;; "draw-page" signals are 0-based, i.e. if the user chooses to print all
;;; pages, the last ::draw-page signal will be for page n_pages - 1.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; n_pages :
;;;     the number of pages
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_n_pages_to_print ()
;;; 
;;; gint gtk_print_operation_get_n_pages_to_print (GtkPrintOperation *op);
;;; 
;;; Returns the number of pages that will be printed.
;;; 
;;; Note that this value is set during print preparation phase
;;; (GTK_PRINT_STATUS_PREPARING), so this function should never be called before
;;; the data generation phase (GTK_PRINT_STATUS_GENERATING_DATA). You can
;;; connect to the "status-changed" signal and call
;;; gtk_print_operation_get_n_pages_to_print() when print status is
;;; GTK_PRINT_STATUS_GENERATING_DATA. This is typically used to track the
;;; progress of print operation.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Returns :
;;;     the number of pages that will be printed
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_current_page ()
;;; 
;;; void gtk_print_operation_set_current_page (GtkPrintOperation *op,
;;;                                            gint current_page);
;;; 
;;; Sets the current page.
;;; 
;;; If this is called before gtk_print_operation_run(), the user will be able
;;; to select to print only the current page.
;;; 
;;; Note that this only makes sense for pre-paginated documents.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; current_page :
;;;     the current page, 0-based
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_use_full_page ()
;;; 
;;; void gtk_print_operation_set_use_full_page (GtkPrintOperation *op,
;;;                                             gboolean full_page);
;;; 
;;; If full_page is TRUE, the transformation for the cairo context obtained from
;;; GtkPrintContext puts the origin at the top left corner of the page (which
;;; may not be the top left corner of the sheet, depending on page orientation
;;; and the number of pages per sheet). Otherwise, the origin is at the top left
;;; corner of the imageable area (i.e. inside the margins).
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; full_page :
;;;     TRUE to set up the GtkPrintContext for the full page
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_unit ()
;;; 
;;; void gtk_print_operation_set_unit (GtkPrintOperation *op, GtkUnit unit);
;;; 
;;; Sets up the transformation for the cairo context obtained from
;;; GtkPrintContext in such a way that distances are measured in units of unit.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; unit :
;;;     the unit to use
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_export_filename ()
;;; 
;;; void gtk_print_operation_set_export_filename (GtkPrintOperation *op,
;;;                                               const gchar *filename);
;;; 
;;; Sets up the GtkPrintOperation to generate a file instead of showing the
;;; print dialog. The indended use of this function is for implementing
;;; "Export to PDF" actions. Currently, PDF is the only supported format.
;;; 
;;; "Print to PDF" support is independent of this and is done by letting the
;;; user pick the "Print to PDF" item from the list of printers in the print
;;; dialog.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; filename :
;;;     the filename for the exported file
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_show_progress ()
;;; 
;;; void gtk_print_operation_set_show_progress (GtkPrintOperation *op,
;;;                                             gboolean show_progress);
;;; 
;;; If show_progress is TRUE, the print operation will show a progress dialog
;;; during the print operation.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; show_progress :
;;;     TRUE to show a progress dialog
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_track_print_status ()
;;; 
;;; void gtk_print_operation_set_track_print_status (GtkPrintOperation *op,
;;;                                                  gboolean track_status);
;;; 
;;; If track_status is TRUE, the print operation will try to continue report on
;;; the status of the print job in the printer queues and printer. This can
;;; allow your application to show things like "out of paper" issues, and when
;;; the print job actually reaches the printer.
;;; 
;;; This function is often implemented using some form of polling, so it should
;;; not be enabled unless needed.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; track_status :
;;;     TRUE to track status after printing
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_custom_tab_label ()
;;; 
;;; void gtk_print_operation_set_custom_tab_label (GtkPrintOperation *op,
;;;                                                const gchar *label);
;;; 
;;; Sets the label for the tab holding custom widgets.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; label :
;;;     the label to use, or NULL to use the default label
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_run ()
;;; 
;;; GtkPrintOperationResult gtk_print_operation_run
;;;                                             (GtkPrintOperation *op,
;;;                                              GtkPrintOperationAction action,
;;;                                              GtkWindow *parent,
;;;                                              GError **error);
;;; 
;;; Runs the print operation, by first letting the user modify print settings in
;;; the print dialog, and then print the document.
;;; 
;;; Normally that this function does not return until the rendering of all pages
;;; is complete. You can connect to the "status-changed" signal on op to obtain
;;; some information about the progress of the print operation. Furthermore, it
;;; may use a recursive mainloop to show the print dialog.
;;; 
;;; If you call gtk_print_operation_set_allow_async() or set the "allow-async"
;;; property the operation will run asynchronously if this is supported on the
;;; platform. The "done" signal will be emitted with the result of the operation
;;; when the it is done (i.e. when the dialog is canceled, or when the print
;;; succeeds or fails).
;;; 
;;; if (settings != NULL)
;;;   gtk_print_operation_set_print_settings (print, settings);
;;;   
;;; if (page_setup != NULL)
;;;   gtk_print_operation_set_default_page_setup (print, page_setup);
;;;   
;;; g_signal_connect (print, "begin-print", 
;;;                   G_CALLBACK (begin_print), &data);
;;; g_signal_connect (print, "draw-page", 
;;;                   G_CALLBACK (draw_page), &data);
;;;  
;;; res = gtk_print_operation_run (print, 
;;;                                GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG, 
;;;                                parent, 
;;;                                &error);
;;;  
;;; if (res == GTK_PRINT_OPERATION_RESULT_ERROR)
;;;  {
;;;    error_dialog = gtk_message_dialog_new (GTK_WINDOW (parent),
;;;                                  GTK_DIALOG_DESTROY_WITH_PARENT,
;;;                          GTK_MESSAGE_ERROR,
;;;                          GTK_BUTTONS_CLOSE,
;;;                          "Error printing file:\n%s",
;;;                          error->message);
;;;    g_signal_connect (error_dialog, "response", 
;;;                      G_CALLBACK (gtk_widget_destroy), NULL);
;;;    gtk_widget_show (error_dialog);
;;;    g_error_free (error);
;;;  }
;;; else if (res == GTK_PRINT_OPERATION_RESULT_APPLY)
;;;  {
;;;    if (settings != NULL)
;;; g_object_unref (settings);
;;;    settings = g_object_ref (gtk_print_operation_get_print_settings (print));
;;;  }
;;; 
;;; Note that gtk_print_operation_run() can only be called once on a given
;;; GtkPrintOperation.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; action :
;;;     the action to start
;;; 
;;; parent :
;;;     transient parent of the dialog
;;; 
;;; error :
;;;     return location for errors, or NULL
;;; 
;;; Returns :
;;;     the result of the print operation. A return value of
;;;     GTK_PRINT_OPERATION_RESULT_APPLY indicates that the printing was
;;;     completed successfully. In this case, it is a good idea to obtain the
;;;     used print settings with gtk_print_operation_get_print_settings() and
;;;     store them for reuse with the next print operation. A value of
;;;     GTK_PRINT_OPERATION_RESULT_IN_PROGRESS means the operation is running
;;;     asynchronously, and will emit the "done" signal when done.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_cancel ()
;;; 
;;; void gtk_print_operation_cancel (GtkPrintOperation *op);
;;; 
;;; Cancels a running print operation. This function may be called from a
;;; "begin-print", "paginate" or "draw-page" signal handler to stop the
;;; currently running print operation.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_draw_page_finish ()
;;; 
;;; void gtk_print_operation_draw_page_finish (GtkPrintOperation *op);
;;; 
;;; Signalize that drawing of particular page is complete.
;;; 
;;; It is called after completion of page drawing (e.g. drawing in another
;;; thread). If gtk_print_operation_set_defer_drawing() was called before, then
;;; this function has to be called by application. In another case it is called
;;; by the library itself.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_defer_drawing ()
;;; 
;;; void gtk_print_operation_set_defer_drawing (GtkPrintOperation *op);
;;; 
;;; Sets up the GtkPrintOperation to wait for calling of
;;; gtk_print_operation_draw_page_finish() from application. It can be used for
;;; drawing page in another thread.
;;; 
;;; This function must be called in the callback of "draw-page" signal.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_status ()
;;; 
;;; GtkPrintStatus gtk_print_operation_get_status (GtkPrintOperation *op);
;;; 
;;; Returns the status of the print operation. Also see
;;; gtk_print_operation_get_status_string().
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Returns :
;;;     the status of the print operation
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_status_string ()
;;; 
;;; const gchar * gtk_print_operation_get_status_string (GtkPrintOperation *op)
;;; 
;;; Returns a string representation of the status of the print operation. The
;;; string is translated and suitable for displaying the print status e.g. in
;;; a GtkStatusbar.
;;; 
;;; Use gtk_print_operation_get_status() to obtain a status value that is
;;; suitable for programmatic use.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Returns :
;;;     a string representation of the status of the print operation
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_is_finished ()
;;; 
;;; gboolean gtk_print_operation_is_finished (GtkPrintOperation *op);
;;; 
;;; A convenience function to find out if the print operation is finished,
;;; either successfully (GTK_PRINT_STATUS_FINISHED) or unsuccessfully
;;; (GTK_PRINT_STATUS_FINISHED_ABORTED).
;;; 
;;; Note: when you enable print status tracking the print operation can be in
;;; a non-finished state even after done has been called, as the operation
;;; status then tracks the print job status on the printer.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Returns :
;;;     TRUE, if the print operation is finished.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_support_selection ()
;;; 
;;; void gtk_print_operation_set_support_selection (GtkPrintOperation *op,
;;;                                                 gboolean support_selection);
;;; 
;;; Sets whether selection is supported by GtkPrintOperation.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; support_selection :
;;;     TRUE to support selection
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_support_selection ()
;;; 
;;; gboolean gtk_print_operation_get_support_selection (GtkPrintOperation *op);
;;; 
;;; Gets the value of "support-selection" property.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Returns :
;;;     whether the application supports print of selection
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_has_selection ()
;;; 
;;; void gtk_print_operation_set_has_selection (GtkPrintOperation *op,
;;;                                             gboolean has_selection);
;;; 
;;; Sets whether there is a selection to print.
;;; 
;;; Application has to set number of pages to which the selection will draw by
;;; gtk_print_operation_set_n_pages() in a callback of "begin-print".
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; has_selection :
;;;     TRUE indicates that a selection exists
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_has_selection ()
;;; 
;;; gboolean gtk_print_operation_get_has_selection (GtkPrintOperation *op);
;;; 
;;; Gets the value of "has-selection" property.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Returns :
;;;     whether there is a selection
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_embed_page_setup ()
;;; 
;;; void gtk_print_operation_set_embed_page_setup (GtkPrintOperation *op,
;;;                                                gboolean embed);
;;; 
;;; Embed page size combo box and orientation combo box into page setup page.
;;; Selected page setup is stored as default page setup in GtkPrintOperation.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; embed :
;;;     TRUE to embed page setup selection in the GtkPrintDialog
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_embed_page_setup ()
;;; 
;;; gboolean gtk_print_operation_get_embed_page_setup (GtkPrintOperation *op);
;;; 
;;; Gets the value of "embed-page-setup" property.
;;; 
;;; op :
;;;     a GtkPrintOperation
;;; 
;;; Returns :
;;;     whether page setup selection combos are embedded
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_run_page_setup_dialog ()
;;; 
;;; GtkPageSetup * gtk_print_run_page_setup_dialog (GtkWindow *parent,
;;;                                                 GtkPageSetup *page_setup,
;;;                                                 GtkPrintSettings *settings)
;;; 
;;; Runs a page setup dialog, letting the user modify the values from
;;; page_setup. If the user cancels the dialog, the returned GtkPageSetup is
;;; identical to the passed in page_setup, otherwise it contains the
;;; modifications done in the dialog.
;;; 
;;; Note that this function may use a recursive mainloop to show the page setup
;;; dialog. See gtk_print_run_page_setup_dialog_async() if this is a problem.
;;; 
;;; parent :
;;;     transient parent
;;; 
;;; page_setup :
;;;     an existing GtkPageSetup
;;; 
;;; settings :
;;;     a GtkPrintSettings
;;; 
;;; Returns :
;;;     a new GtkPageSetup
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkPageSetupDoneFunc ()
;;; 
;;; void (*GtkPageSetupDoneFunc) (GtkPageSetup *page_setup, gpointer data)
;;; 
;;; The type of function that is passed to
;;; gtk_print_run_page_setup_dialog_async().
;;; 
;;; This function will be called when the page setup dialog is dismissed, and
;;; also serves as destroy notify for data.
;;; 
;;; page_setup :
;;;     the GtkPageSetup that has been
;;; 
;;; data :
;;;     user data that has been passed to
;;;     gtk_print_run_page_setup_dialog_async()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_run_page_setup_dialog_async ()
;;; 
;;; void gtk_print_run_page_setup_dialog_async (GtkWindow *parent,
;;;                                             GtkPageSetup *page_setup,
;;;                                             GtkPrintSettings *settings,
;;;                                             GtkPageSetupDoneFunc done_cb,
;;;                                             gpointer data);
;;; 
;;; Runs a page setup dialog, letting the user modify the values from page_setup.
;;; 
;;; In contrast to gtk_print_run_page_setup_dialog(), this function returns
;;; after showing the page setup dialog on platforms that support this, and
;;; calls done_cb from a signal handler for the ::response signal of the dialog.
;;; 
;;; parent :
;;;     transient parent, or NULL
;;; 
;;; page_setup :
;;;     an existing GtkPageSetup, or NULL
;;; 
;;; settings :
;;;     a GtkPrintSettings
;;; 
;;; done_cb :
;;;     a function to call when the user saves the modified page setup
;;; 
;;; data :
;;;     user data to pass to done_cb
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_end_preview ()
;;; 
;;; void gtk_print_operation_preview_end_preview
;;;                                          (GtkPrintOperationPreview *preview)
;;; 
;;; Ends a preview.
;;; 
;;; This function must be called to finish a custom print preview.
;;; 
;;; preview :
;;;     a GtkPrintOperationPreview
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_is_selected ()
;;; 
;;; gboolean gtk_print_operation_preview_is_selected
;;;                                          (GtkPrintOperationPreview *preview,
;;;                                           gint page_nr);
;;; 
;;; Returns whether the given page is included in the set of pages that have
;;; been selected for printing.
;;; 
;;; preview :
;;;     a GtkPrintOperationPreview
;;; 
;;; page_nr :
;;;     a page number
;;; 
;;; Returns :
;;;     TRUE if the page has been selected for printing
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_render_page ()
;;; 
;;; void gtk_print_operation_preview_render_page
;;;                                          (GtkPrintOperationPreview *preview,
;;;                                           gint page_nr);
;;; 
;;; Renders a page to the preview, using the print context that was passed to
;;; the "preview" handler together with preview.
;;; 
;;; A custom iprint preview should use this function in its ::expose handler to
;;; render the currently selected page.
;;; 
;;; Note that this function requires a suitable cairo context to be associated
;;; with the print context.
;;; 
;;; preview :
;;;     a GtkPrintOperationPreview
;;; 
;;; page_nr :
;;;     the page to render
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.print-operation.lisp -----------------------------------
