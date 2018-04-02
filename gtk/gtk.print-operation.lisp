;;; ----------------------------------------------------------------------------
;;; gtk.print-operation.lisp
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
;;; GtkPrintOperation
;;;
;;; High-level Printing API
;;;
;;; Synopsis
;;;
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
;;;     GtkPrintOperationPreview
;;;
;;;     gtk_print_operation_preview_end_preview
;;;     gtk_print_operation_preview_is_selected
;;;     gtk_print_operation_preview_render_page
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintOperationPreview
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkPrintOperationPreview" gtk-print-operation-preview
  (:export t
   :type-initializer "gtk_print_operation_preview_get_type"))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-print-operation-preview 'type)
 "@version{2013-7-21}
  @short{}
  @begin{pre}
(define-g-interface \"GtkPrintOperationPreview\" gtk-print-operation-preview
  (:export t
   :type-initializer \"gtk_print_operation_preview_get_type\"))
  @end{pre}
  @see-class{gtk-print-operation}")

;;; ----------------------------------------------------------------------------
;;; struct GtkPrintOperation
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
   (embed-page-setup
    gtk-print-operation-embed-page-setup
    "embed-page-setup" "gboolean" t t)
   (export-filename
    gtk-print-operation-export-filename
    "export-filename" "gchararray" t t)
   (has-selection
    gtk-print-operation-has-selection
    "has-selection" "gboolean" t t)
   (job-name
    gtk-print-operation-job-name
    "job-name" "gchararray" t t)
   (n-pages
    gtk-print-operation-n-pages
    "n-pages" "gint" t t)
   (n-pages-to-print
    gtk-print-operation-n-pages-to-print
    "n-pages-to-print" "gint" t nil)
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
   (support-selection
    gtk-print-operation-support-selection
    "support-selection" "gboolean" t t)
   (track-print-status
    gtk-print-operation-track-print-status
    "track-print-status" "gboolean" t t)
   (unit
    gtk-print-operation-unit
    "unit" "GtkUnit" t t)
   (use-full-page
    gtk-print-operation-use-full-page
    "use-full-page" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-print-operation 'type)
 "@version{2013-5-30}
  @begin{short}
    @sym{gtk-print-operation} is the high-level, portable printing API. It looks
    a bit different than other GTK+ dialogs such as the
    @class{gtk-file-chooser}, since some platforms do not expose enough
    infrastructure to implement a good print dialog. On such platforms,
    @sym{gtk-print-operation} uses the native print dialog. On platforms which
    do not provide a native print dialog, GTK+ uses its own, see
    @class{gtk-print-unix-dialog}.
  @end{short}

  The typical way to use the high-level printing API is to create a
  @sym{gtk-print-operation} object with the @fun{gtk-print-operation-new}
  function when the user selects to print. Then you set some properties on it,
  e. g. the page size, any @class{gtk-print-settings} from previous print
  operations, the number of pages, the current page, etc.

  Then you start the print operation by calling the
  @fun{gtk-print-operation-run}. It will then show a dialog, let the user select
  a printer and options. When the user finished the dialog various signals will
  be emitted on the @sym{gtk-print-operation}, the main one being \"draw-page\",
  which you are supposed to catch and render the page on the provided
  @class{gtk-print-context} using Cairo.

  @b{Example:} The high-level printing API
  @begin{pre}
 static GtkPrintSettings *settings = NULL;

   static void
   do_print (void)
   {
     GtkPrintOperation *print;
     GtkPrintOperationResult res;

     print = gtk_print_operation_new ();

     if (settings != NULL)
       gtk_print_operation_set_print_settings (print, settings);

     g_signal_connect (print, \"begin_print\", G_CALLBACK (begin_print), NULL);
     g_signal_connect (print, \"draw_page\", G_CALLBACK (draw_page), NULL);

     res = gtk_print_operation_run (print,
                                    GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
                                    GTK_WINDOW (main_window), NULL);

     if (res == GTK_PRINT_OPERATION_RESULT_APPLY)
       {
         if (settings != NULL)
           g_object_unref (settings);
         settings =
               g_object_ref (gtk_print_operation_get_print_settings (print));
       @}

     g_object_unref (print);
   @}
  @end{pre}
  By default @sym{gtk-print-operation} uses an external application to do print
  preview. To implement a custom print preview, an application must connect to
  the preview signal. The functions
  @fun{gtk-print-operation-print-preview-render-page},
  @fun{gtk-print-operation-preview-end-preview} and
  @fun{gtk-print-operation-preview-is-selected} are useful when implementing a
  print preview.
  @begin[Signal Details]{dictionary}
    @subheading{The \"begin-print\" signal}
      @begin{pre}
 lambda (operation context)   : Run Last
      @end{pre}
      Emitted after the user has finished changing print settings in the dialog,
      before the actual rendering starts.
      A typical use for ::begin-print is to use the parameters from the
      @class{gtk-print-context} and paginate the document accordingly, and then
      set the number of pages with the @fun{gtk-print-operation-set-n-pages}
      function.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} on which the signal was
          emitted.}
        @entry[context]{The @class{gtk-print-context} for the current
          operation.}
      @end{table}
      Since 2.10

    @subheading{The \"create-custom-widget\" signal}
      @begin{pre}
 lambda (operation)   : Run Last
      @end{pre}
      Emitted when displaying the print dialog. If you return a widget in a
      handler for this signal it will be added to a custom tab in the print
      dialog. You typically return a container widget with multiple widgets in
      it.
      The print dialog owns the returned widget, and its lifetime is not
      controlled by the application. However, the widget is guaranteed to stay
      around until the \"custom-widget-apply\" signal is emitted on the
      operation. Then you can read out any information you need from the
      widgets.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} on which the signal was
          emitted.}
        @entry[Returns]{A custom widget that gets embedded in the print dialog,
          or @code{nil}.}
      @end{table}
      Since 2.10

    @subheading{The \"custom-widget-apply\" signal}
      @begin{pre}
 lambda (operation widget)   : Run Last
      @end{pre}
      Emitted right before the \"begin-print\" signal if you added a custom
      widget in the \"create-custom-widget\" handler. When you get this signal
      you should read the information from the custom widgets, as the widgets
      are not guaraneed to be around at a later time.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} on which the signal
          was emitted.}
        @entry[widget]{The custom widget added in create-custom-widget.}
      @end{table}
      Since 2.10

    @subheading{The \"done\" signal}
      @begin{pre}
 lambda (operation result)   : Run Last
      @end{pre}
      Emitted when the print operation run has finished doing everything
      required for printing.
      @arg{result} gives you information about what happened during the run.
      If result is @code{:error} then you can call the
      @fun{gtk-print-operation-get-error} function for more information.
      If you enabled print status tracking then the
      @fun{gtk-print-operation-is-finished} function may still return @code{nil}
      after \"done\" was emitted.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} on which the signal was
          emitted.}
        @entry[result]{The result of the print operation.}
      @end{table}
      Since 2.10

    @subheading{The \"draw-page\" signal}
      @begin{pre}
 lambda (operation context page-nr)   : Run Last
      @end{pre}
      Emitted for every page that is printed. The signal handler must render
      the @arg{page-nr}'s page onto the cairo context obtained from context
      using the @fun{gtk-print-context-get-cairo-context} function.
      @begin{pre}
   static void
   draw_page (GtkPrintOperation *operation,
              GtkPrintContext   *context,
              gint               page_nr,
              gpointer           user_data)
   {
     cairo_t *cr;
     PangoLayout *layout;
     gdouble width, text_height;
     gint layout_height;
     PangoFontDescription *desc;

     cr = gtk_print_context_get_cairo_context (context);
     width = gtk_print_context_get_width (context);

     cairo_rectangle (cr, 0, 0, width, HEADER_HEIGHT);

     cairo_set_source_rgb (cr, 0.8, 0.8, 0.8);
     cairo_fill (cr);

     layout = gtk_print_context_create_pango_layout (context);

     desc = pango_font_description_from_string (\"sans 14\");
     pango_layout_set_font_description (layout, desc);
     pango_font_description_free (desc);

     pango_layout_set_text (layout, \"some text\", -1);
     pango_layout_set_width (layout, width * PANGO_SCALE);
     pango_layout_set_alignment (layout, PANGO_ALIGN_CENTER);

     pango_layout_get_size (layout, NULL, &layout_height);
     text_height = (gdouble)layout_height / PANGO_SCALE;

     cairo_move_to (cr, width / 2,  (HEADER_HEIGHT - text_height) / 2);
     pango_cairo_show_layout (cr, layout);

     g_object_unref (layout);
   @}
      @end{pre}
      Use the functions @fun{gtk-print-operation-set-use-full-page} and
      @fun{gtk-print-operation-set-unit} before starting the print operation to
      set up the transformation of the cairo context according to your needs.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} on which the signal was
          emitted.}
        @entry[context]{The @class{gtk-print-context} for the current
          operation.}
        @entry[page-nr]{The number of the currently printed page (0-based).}
      @end{table}
      Since 2.10

    @subheading{The \"end-print\" signal}
      @begin{pre}
 lambda (operation context)   : Run Last
      @end{pre}
      Emitted after all pages have been rendered. A handler for this signal can
      clean up any resources that have been allocated in the \"begin-print\"
      handler.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} on which the signal
          was emitted.}
        @entry[context]{The @class{gtk-print-context} for the current
          operation.}
      @end{table}
      Since 2.10

    @subheading{The \"paginate\" signal}
      @begin{pre}
 lambda (operation context)   : Run Last
      @end{pre}
      Emitted after the \"begin-print\" signal, but before the actual rendering
      starts. It keeps getting emitted until a connected signal handler returns
      @em{true}.
      The \"paginate\" signal is intended to be used for paginating a document
      in small chunks, to avoid blocking the user interface for a long time.
      The signal handler should update the number of pages using the
      @fun{gtk-print-operation-set-n-pages} function, and return @em{true} if
      the document has been completely paginated.
      If you do not need to do pagination in chunks, you can simply do it all
      in the \"begin-print\" handler, and set the number of pages from there.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} on which the signal
          was emitted.}
        @entry[context]{The @class{gtk-print-context} for the current
          operation.}
        @entry[Returns]{@em{True} if pagination is complete.}
      @end{table}
      Since 2.10

    @subheading{The \"preview\" signal}
      @begin{pre}
 lambda (operation preview context parent)   : Run Last
      @end{pre}
      Gets emitted when a preview is requested from the native dialog.
      The default handler for this signal uses an external viewer application
      to preview.
      To implement a custom print preview, an application must return @em{true}
      from its handler for this signal. In order to use the provided context
      for the preview implementation, it must be given a suitable cairo context
      with the @fun{gtk-print-context-set-cairo-context} function.
      The custom preview implementation can use the functions
      @fun{gtk-print-operation-preview-is-selected} and
      @fun{gtk-print-operation-preview-render-page} to find pages which are
      selected for print and render them. The preview must be finished by
      calling the @fun{gtk-print-operation-preview-end-preview} function
      (typically in response to the user clicking a close button).
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} on which the signal was
          emitted.}
        @entry[preview]{The @class{gtk-print-preview-operation} for the current
          operation.}
        @entry[context]{The @class{gtk-print-context} that will be used.}
        @entry[parent]{The @class{gtk-window} to use as window parent,
          or @code{nil}.}
        @entry[Returns]{@em{True} if the listener wants to take over control of
          the preview.}
      @end{table}
      Since 2.10

    @subheading{The \"request-page-setup\" signal}
      @begin{pre}
 lambda (operation context page-nr setup)   : Run Last
      @end{pre}
      Emitted once for every page that is printed, to give the application a
      chance to modify the page setup. Any changes done to setup will be in
      force only for printing this page.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} on which the signal was
          emitted.}
        @entry[context]{The @class{gtk-print-context} for the current
          operation.}
        @entry[page-nr]{The number of the currently printed page (0-based).}
        @entry[setup]{The @sym{gtk-page-setup}.}
      @end{table}
      Since 2.10

    @subheading{The \"status-changed\" signal}
      @begin{pre}
 lambda (operation)   : Run Last
      @end{pre}
      Emitted at between the various phases of the print operation. See the
      @symbol{gtk-print-status} enumeration for the phases that are being
      discriminated. Use the @fun{gtk-print-operation-get-status} to find out
      the current status.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} on which the signal was
          emitted.}
      @end{table}
      Since 2.10

    @subheading{The \"update-custom-widget\" signal}
      @begin{pre}
 lambda (operation widget setup settings)   : Run Last
      @end{pre}
      Emitted after change of selected printer. The actual page setup and print
      settings are passed to the custom widget, which can actualize itself
      according to this change.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} on which the signal was
          emitted.}
        @entry[widget]{The custom widget added in create-custom-widget.}
        @entry[setup]{Actual page setup.}
        @entry[settings]{Actual print settings.}
      @end{table}
      Since 2.18

    @subheading{The \"got-page-size\" signal}
      @begin{pre}
 lambda (preview context page-setup)   : Run Last
      @end{pre}
      The \"got-page-size\" signal is emitted once for each page that gets
      rendered to the preview.
      A handler for this signal should update the context according to
      @arg{page-setup} and set up a suitable cairo context, using the
      @fun{gtk-print-context-set-cairo-context}.
      @begin[code]{table}
        @entry[preview]{The object on which the signal is emitted.}
        @entry[context]{The current @class{gtk-print-context}.}
        @entry[page-setup]{The @sym{gtk-page-setup} for the current page.}
      @end{table}

    @subheading{The \"ready\" signal}
      @begin{pre}
 lambda (preview context)   : Run Last
      @end{pre}
      The \"ready\" signal gets emitted once per preview operation, before the
      first page is rendered.
      A handler for this signal can be used for setup tasks.
      @begin[code]{table}
        @entry[preview]{The object on which the signal is emitted.}
        @entry[context]{The current @class{gtk-print-context}.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-print-operation-allow-async}
  @see-slot{gtk-print-operation-current-page}
  @see-slot{gtk-print-operation-custom-tab-label}
  @see-slot{gtk-print-operation-default-page-setup}
  @see-slot{gtk-print-operation-embed-page-setup}
  @see-slot{gtk-print-operation-export-filename}
  @see-slot{gtk-print-operation-has-selection}
  @see-slot{gtk-print-operation-job-name}
  @see-slot{gtk-print-operation-n-pages}
  @see-slot{gtk-print-operation-n-pages-to-print}
  @see-slot{gtk-print-operation-print-settings}
  @see-slot{gtk-print-operation-show-progress}
  @see-slot{gtk-print-operation-status}
  @see-slot{gtk-print-operation-status-string}
  @see-slot{gtk-print-operation-support-selection}
  @see-slot{gtk-print-operation-track-print-status}
  @see-slot{gtk-print-operation-unit}
  @see-slot{gtk-print-operation-use-full-page}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "allow-async"
                                               'gtk-print-operation) 't)
 "The @code{\"allow-async\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Determines whether the print operation may run asynchronously or not.
  Some systems do not support asynchronous printing, but those that do will
  return @code{:in-progress} as the status, and emit the \"done\" signal when
  the operation is actually done.
  The Windows port does not support asynchronous operation at all (this is
  unlikely to change). On other platforms, all actions except for
  @code{:export} support asynchronous operation. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "current-page"
                                               'gtk-print-operation) 't)
 "The @code{\"current-page\"} property of type @code{:int} (Read / Write) @br{}
  The current page in the document.
  If this is set before the @fun{gtk-print-operation-run} function, the user
  will be able to select to print only the current page.
  Note that this only makes sense for pre-paginated documents. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "custom-tab-label"
                                               'gtk-print-operation) 't)
 "The @code{\"custom-tab-label\"} property of type @code{:string}
  (Read / Write) @br{}
  Used as the label of the tab containing custom widgets. Note that this
  property may be ignored on some platforms.
  If this is @code{nil}, GTK+ uses a default label. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "default-page-setup"
                                               'gtk-print-operation) 't)
 "The @code{\"default-page-setup\"} property of type @class{gtk-page-setup}
  (Read / Write) @br{}
  The @class{gtk-page-setup} used by default.
  This page setup will be used by the @fun{gtk-print-operation-run} function,
  but it can be overridden on a per-page basis by connecting to the
  \"request-page-setup\" signal. @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "embed-page-setup"
                                               'gtk-print-operation) 't)
 "The @code{\"embed-page-setup\"} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, page size combo box and orientation combo box are embedded into
  page setup page. @br{}
  Default value: @code{nil} @br{}
  Since 2.18")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "export-filename"
                                               'gtk-print-operation) 't)
 "The @code{\"export-filename\"} property of type @code{:string}
  (Read / Write) @br{}
  The name of a file to generate instead of showing the print dialog.
  Currently, PDF is the only supported format.
  The intended use of this property is for implementing \"Export to PDF\"
  actions.
  \"Print to PDF\" support is independent of this and is done by letting the
  user pick the \"Print to PDF\" item from the list of printers in the print
  dialog. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-selection"
                                               'gtk-print-operation) 't)
 "The @code{\"has-selection\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Determines whether there is a selection in your application. This can allow
  your application to print the selection. This is typically used to make a
  \"Selection\" button sensitive. @br{}
  Default value: @code{nil} @br{}
  Since 2.18")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "job-name"
                                               'gtk-print-operation) 't)
 "The @code{\"job-name\"} property of type @code{:string} (Read / Write) @br{}
  A string used to identify the job (e. g. in monitoring applications like
  eggcups).
  If you do not set a job name, GTK+ picks a default one by numbering
  successive print jobs. @br{}
  Default value: \"\" @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "n-pages"
                                               'gtk-print-operation) 't)
 "The @code{\"n-pages\"} property of type @code{:int} (Read / Write) @br{}
  The number of pages in the document.
  This must be set to a positive number before the rendering starts. It may be
  set in a \"begin-print\" signal hander.
  Note that the page numbers passed to the \"request-page-setup\" and
  \"draw-page\" signals are 0-based, i. e. if the user chooses to print all
  pages, the last \"draw-page\" signal will be for page n_pages - 1. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "n-pages-to-print"
                                               'gtk-print-operation) 't)
 "The @code{\"n-pages-to-print\"} property of type @code{:int} (Read) @br{}
  The number of pages that will be printed.
  Note that this value is set during print preparation phase
  (@code{:preparing}), so this value should never be get before the data
  generation phase (@code{:generating-data}). You can connect to the
  \"status-changed\" signal and call the
  @fun{gtk-print-operation-get-n-pages-to-print} function when print status is
  @code{:generating-data}. This is typically used to track the progress of print
  operation. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.18")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "print-settings"
                                               'gtk-print-operation) 't)
 "The @code{\"print-settings\"} property of type @symbol{gtk-print-settings}
  (Read / Write) @br{}
  The @sym{gtk-print-settings} used for initializing the dialog. Setting this
  property is typically used to re-establish print settings from a previous
  print operation, see the @fun{gtk-print-operation-run} function. @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-progress"
                                               'gtk-print-operation) 't)
 "The @code{\"show-progress\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Determines whether to show a progress dialog during the print operation. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "status"
                                               'gtk-print-operation) 't)
 "The @code{\"status\"} property of type @symbol{gtk-print-status} (Read) @br{}
  The status of the print operation.
  Default value: @code{:initial} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "status-string"
                                               'gtk-print-operation) 't)
 "The @code{\"status-string\"} property of type @code{:string} (Read) @br{}
  A string representation of the status of the print operation. The string is
  translated and suitable for displaying the print status e. g. in a
  @class{gtk-statusbar}.
  See the \"status\" property for a status value that is suitable for
  programmatic use. @br{}
  Default value: \"\" @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "support-selection"
                                               'gtk-print-operation) 't)
 "The @code{\"support-selection\"} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, the print operation will support print of selection. This allows
  the print dialog to show a \"Selection\" button. @br{}
  Default value: @code{nil} @br{}
  Since 2.18")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "track-print-status"
                                               'gtk-print-operation) 't)
 "The @code{\"track-print-status\"} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, the print operation will try to continue report on the status of
  the print job in the printer queues and printer. This can allow your
  application to show things like \"out of paper\" issues, and when the print
  job actually reaches the printer. However, this is often implemented using
  polling, and should not be enabled unless needed. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "unit"
                                               'gtk-print-operation) 't)
 "The @code{\"unit\"} property of type @symbol{gtk-unit} (Read / Write) @br{}
  The transformation for the cairo context obtained from
  @class{gtk-print-context} is set up in such a way that distances are measured
  in units of unit. @br{}
  Default value: @code{:pixel} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-full-page"
                                               'gtk-print-operation) 't)
 "The @code{\"use-full-page\"} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, the transformation for the cairo context obtained from
  @class{gtk-print-context} puts the origin at the top left corner of the page
  (which may not be the top left corner of the sheet, depending on page
  orientation and the number of pages per sheet). Otherwise, the origin is at
  the top left corner of the imageable area (i. e. inside the margins). @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-allow-async atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-allow-async 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"allow-async\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-allow-async}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-current-page atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-current-page 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"current-page\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-current-page}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-custom-tab-label
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-custom-tab-label 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"custom-tab-label\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-custom-tab-label}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-default-page-setup
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-default-page-setup 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"default-page-setup\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-default-page-setup}
  @see-function{gtk-print-operation-set-default-page-setup}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-embed-page-setup
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-embed-page-setup 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"embed-page-setup\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-embed-page-setup}
  @see-function{gtk-print-operation-set-embed-page-setup}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-export-filename atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-export-filename 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"export-filename\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-export-filename}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-has-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-has-selection 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"has-selection\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-has-selection}
  @see-function{gtk-print-operation-set-has-selection}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-job-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-job-name 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"job-name\"} of the @class{gtk-print-operation}
  class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-job-name}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-n-pages atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-n-pages 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"n-pages\"} of the @class{gtk-print-operation}
  class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-n-pages}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-n-pages-to-print
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-n-pages-to-print 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"n-pages-to-print\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-n-pages-to-print}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-print-settings atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-print-settings 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"print-settings\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-print-settings}
  @see-function{gtk-print-operation-set-print-settings}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-show-progress atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-show-progress 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"show-progress\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-show-progress}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-status atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-status 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"status\"} of the @class{gtk-print-operation}
  class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-status}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-status-string atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-status-string 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"status-string\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-status-string}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-support-selection
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-support-selection 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"support-selection\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-support-selection}
  @see-function{gtk-print-operation-set-support-selection}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-track-print-status
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-track-print-status 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"track-print-status\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-track-print-status}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-unit atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-unit 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"unit\"} of the @class{gtk-print-operation}
  class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-unit}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-use-full-page atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-use-full-page 'function)
 "@version{2013-11-15}
  Accessor of the slot @code{\"use-full-page\"} of the
  @class{gtk-print-operation} class.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-use-full-page}")

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintStatus
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-status atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-print-status atdoc:*external-symbols*)
 "@version{2013-10-21}
  @begin{short}
    The status gives a rough indication of the completion of a running print
    operation.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPrintStatus\" gtk-print-status
  (:export t
   :type-initializer \"gtk_print_status_get_type\")
  (:initial 0)
  (:preparing 1)
  (:generating-data 2)
  (:sending-data 3)
  (:pending 4)
  (:pending-issue 5)
  (:printing 6)
  (:finished 7)
  (:finished-aborted 8))
  @end{pre}
  @begin[code]{table}
    @entry[:initial]{The printing has not started yet; this status is set
      initially, and while the print dialog is shown.}
    @entry[:preparing]{This status is set while the begin-print signal is
      emitted and during pagination.}
    @entry[:generating-data]{This status is set while the pages are being
      rendered.}
    @entry[:sending-data]{The print job is being sent off to the printer.}
    @entry[:pending]{The print job has been sent to the printer, but is not
      printed for some reason, e. g. the printer may be stopped.}
    @entry[:pending-issue]{Some problem has occurred during printing,
      e. g. a paper jam.}
    @entry[:printing]{The printer is processing the print job.}
    @entry[:finished]{The printing has been completed successfully.}
    @entry[:finished-aborted]{The printing has been aborted.}
  @end{table}
  @see-class{gtk-print-operation}")

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintOperationAction
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintOperationAction" gtk-print-operation-action
  (:export t
   :type-initializer "gtk_print_operation_action_get_type")
  (:print-dialog 0)
  (:print 1)
  (:preview 2)
  (:export 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-action atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-print-operation-action atdoc:*external-symbols*)
 "@version{2013-10-27}
  @begin{short}
    The action parameter to the @fun{gtk-print-operation-run} function
    determines what action the print operation should perform.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPrintOperationAction\" gtk-print-operation-action
  (:export t
   :type-initializer \"gtk_print_operation_action_get_type\")
  (:print-dialog 0)
  (:print 1)
  (:preview 2)
  (:export 3))
  @end{pre}
  @begin[code]{table}
    @entry[:print-dialog]{Show the print dialog.}
    @entry[:print]{Start to print without showing the print dialog, based on
      the current print settings.}
    @entry[:preview]{Show the print preview.}
    @entry[:export]{Export to a file. This requires the
      @code{\"export-filename\"} property to be set.}
  @end{table}
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-run}")

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintOperationResult
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintOperationResult" gtk-print-operation-result
  (:export t
   :type-initializer "gtk_print_operation_result_get_type")
  (:error 0)
  (:apply 1)
  (:cancel 2)
  (:in-progress 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-result atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-print-operation-result atdoc:*external-symbols*)
 "@version{2013-10-27}
  @begin{short}
    A value of this type is returned by the @fun{gtk-print-operation-run}
    function.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPrintOperationResult\" gtk-print-operation-result
  (:export t
   :type-initializer \"gtk_print_operation_result_get_type\")
  (:error 0)
  (:apply 1)
  (:cancel 2)
  (:in-progress 3))
  @end{pre}
  @begin[code]{table}
    @entry[:error]{An error has occured.}
    @entry[:apply]{The print settings should be stored.}
    @entry[:cancel]{The print operation has been canceled, the print settings
      should not be stored.}
    @entry[:in-progress]{The print operation is not complete yet. This value
      will only be returned when running asynchronously.}
  @end{table}
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-run}")

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintError
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintError" gtk-print-error
  (:export t
   :type-initializer "gtk_print_error_get_type")
  (:general 0)
  (:internal-error 1)
  (:nomem 2)
  (:invalid-file 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-error atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-print-error atdoc:*external-symbols*)
 "@version{2013-5-30}
  @begin{short}
    Error codes that identify various errors that can occur while using the GTK+
    printing support.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPrintError\" gtk-print-error
  (:export t
   :type-initializer \"gtk_print_error_get_type\")
  (:general 0)
  (:internal-error 1)
  (:nomem 2)
  (:invalid-file 3))
  @end{pre}
  @begin[code]{table}
    @entry[:general]{An unspecified error occurred.}
    @entry[:internal-error]{An internal error occurred.}
    @entry[:nomem]{A memory allocation failed.}
    @entry[:invalid-file]{An error occurred while loading a page setup or paper
      size from a key file.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_ERROR
;;;
;;; #define GTK_PRINT_ERROR gtk_print_error_quark ()
;;;
;;; The error domain for GtkPrintError errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-new))

(defun gtk-print-operation-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @return{A new @class{gtk-print-operation} object}
  @begin{short}
    Creates a new @class{gtk-print-operation} object.
  @end{short}

  Since 2.10
  @see-class{gtk-print-operation}"
  (make-instance 'gtk-print-operation))

(export 'gtk-print-operation-new)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_allow_async ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-allow-async))

(defun gtk-print-operation-set-allow-async (op allow-async)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[allow-async]{@em{true} to allow asynchronous operation}
  @begin{short}
    Sets whether the function @fun{gtk-print-operation-run} may return before
    the print operation is completed.
  @end{short}
  Note that some platforms may not allow asynchronous operation.

  Since 2.10
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-run}"
  (setf (gtk-print-operation-allow-async op) allow-async))

(export 'gtk-print-operation-set-allow-async)

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
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-default-page-setup))

(defun gtk-print-operation-set-default-page-setup (op default-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[default-page-setup]{a @class{gtk-page-setup}, or @code{nil}}
  @begin{short}
    Makes @arg{default-page-setup} the default page setup for op.
  @end{short}

  This page setup will be used by the function @fun{gtk-print-operation-run},
  but it can be overridden on a per-page basis by connecting to the
  \"request-page-setup\" signal.

  Since 2.10
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-run}
  @see-function{gtk-print-operatrion-get-default-page-setup}"
  (setf (gtk-print-operation-default-page-setup op) default-page-setup))

(export 'gtk-print-operation-set-default-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_default_page_setup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-get-default-page-setup))

(defun gtk-print-operation-get-default-page-setup (op)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @return{The default page setup.}
  @begin{short}
    Returns the default page setup.
  @end{short}
  See the function @fun{gtk-print-operation-set-default-page-setup}.

  Since 2.10
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-default-page-setup}"
  (gtk-print-operation-default-page-setup op))

(export 'gtk-print-operation-get-default-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_print_settings ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-print-settings))

(defun gtk-print-operation-set-print-settings (op print-settings)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[print-settings]{@class{gtk-print-settings} object}
  @begin{short}
    Sets the print settings for op. This is typically used to re-establish print
    settings from a previous print operation.
  @end{short}
  See the functionn @fun{gtk-print-operation-run}.

  Since 2.10
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-run}
  @see-function{gtk-print-operation-get-print-settings}"
  (setf (gtk-print-operation-print-settings op) print-settings))

(export 'gtk-print-operation-set-print-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_print_settings ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-get-print-settings))

(defun gtk-print-operation-get-print-settings (op)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @return{The current print settings of @arg{op}.}
  @begin{short}
    Returns the current print settings.
  @end{short}

  Note that the return value is @code{nil} until either the function
  @fun{gtk-print-operation-set-print-settings} or the function
  @fun{gtk-print-operation-run} have been called.

  Since 2.10
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-run}
  @see-function{gtk-print-operation-set-print-settings}"
  (gtk-print-operation-print-settings op))

(export 'gtk-print-operation-get-print-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_job_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-job-name))

(defun gtk-print-operation-set-job-name (op job-name)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[job-name]{a string that identifies the print job}
  @begin{short}
    Sets the name of the print job.
  @end{short}
  The name is used to identify the job, e. g. in monitoring applications like
  eggcups.

  If you do not set a job name, GTK+ picks a default one by numbering
  successive print jobs.

  Since 2.10
  @see-class{gtk-print-operation}"
  (setf (gtk-print-operation-job-name op) job-name))

(export 'gtk-print-operation-set-job-name)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_n_pages ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-n-pages))

(defun gtk-print-operation-set-n-pages (op n-pages)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[n-pages]{the number of pages}
  @begin{short}
    Sets the number of pages in the document.
  @end{short}

  This must be set to a positive number before the rendering starts. It may be
  set in a \"begin-print\" signal hander.

  Note that the page numbers passed to the \"request-page-setup\" and
  \"draw-page\" signals are 0-based, i. e. if the user chooses to print all
  pages, the last \"draw-page\" signal will be for page @arg{n-pages} - 1.

  Since 2.10
  @see-class{gtk-print-operation}"
  (setf (gtk-print-operation-n-pages op) n-pages))

(export 'gtk-print-operation-set-n-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_n_pages_to_print ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-get-n-pages-to-print))

(defun gtk-print-operation-get-n-pages-to-print (op)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @return{The number of pages that will be printed.}
  @begin{short}
    Returns the number of pages that will be printed.
  @end{short}

  Note that this value is set during print preparation phase @code{:preparing},
  so this function should never be called before the data generation phase
  @code{:generating-data}. You can connect to the \"status-changed\" signal and
  call the function @sym{gtk-print-operation-get-n-pages-to-print} when print
  status is @code{:generating-data}. This is typically used to track the
  progress of print operation.

  Since 2.18
  @see-class{gtk-print-operation}
  @see-symbol{gtk-print-status}"
  (gtk-print-operation-n-pages-to-print op))

(export 'gtk-print-operation-get-n-pages-to-print)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_current_page ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-current-page))

(defun gtk-print-operation-set-current-page (op current-page)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[current-page]{the current page, 0-based}
  @begin{short}
    Sets the current page.
  @end{short}

  If this is called before the function @fun{gtk-print-operation-run}, the user
  will be able to select to print only the current page.

  Note that this only makes sense for pre-paginated documents.

  Since 2.10
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-run}"
  (setf (gtk-print-operation-current-page op) current-page))

(export 'gtk-print-operation-set-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_use_full_page ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-use-full-page))

(defun gtk-print-operation-set-use-full-page (op full-page)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[full-page]{@em{true} to set up the @class{gtk-print-context} for
    the full page}
  @begin{short}
    If @arg{full-page} is @em{true}, the transformation for the cairo context
    obtained from @class{gtk-print-context} puts the origin at the top left
    corner of the page, which may not be the top left corner of the sheet,
    depending on page orientation and the number of pages per sheet.
  @end{short}
  Otherwise, the origin is at the top left corner of the imageable area, i. e.
  inside the margins.

  Since 2.10
  @see-class{gtk-print-operation}"
  (setf (gtk-print-operation-use-full-page op) full-page))

(export 'gtk-print-operation-set-use-full-page)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_unit ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-unit))

(defun gtk-print-operation-set-unit (op unit)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[unit]{the unit to use}
  @begin{short}
    Sets up the transformation for the cairo context obtained from
    @class{gtk-print-context} in such a way that distances are measured in units
    of @arg{unit}.
  @end{short}

  Since 2.10
  @see-class{gtk-print-operation}
  @see-class{gtk-print-context}
  @see-symbol{gtk-unit}"
  (setf (gtk-print-operation-unit op) unit))

(export 'gtk-print-operation-set-unit)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_export_filename ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-export-filename))

(defun gtk-print-operation-set-export-filename (op filename)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[filename]{the filename for the exported file}
  @begin{short}
    Sets up the @class{gtk-print-operation} to generate a file instead of
    showing the print dialog.
  @end{short}
  The indended use of this function is for implementing \"Export to PDF\"
  actions. Currently, PDF is the only supported format.

  \"Print to PDF\" support is independent of this and is done by letting the
  user pick the \"Print to PDF\" item from the list of printers in the print
  dialog.

  Since 2.10
  @see-class{gtk-print-operation}"
  (setf (gtk-print-operation-export-filename op) filename))

(export 'gtk-print-operation-set-export-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_show_progress ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-show-progress))

(defun gtk-print-operation-set-show-progress (op show-progress)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[show-progress]{@em{true} to show a progress dialog}
  @begin{short}
    If @arg{show-progress} is @em{true}, the print operation will show a
    progress dialog during the print operation.
  @end{short}

  Since 2.10
  @see-class{gtk-print-operation}"
  (setf (gtk-print-operation-show-progress op) show-progress))

(export 'gtk-print-operation-set-show-progress)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_track_print_status ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-track-print-status))

(defun gtk-print-operation-set-track-print-status (op track-status)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[track-status]{@em{true} to track status after printing}
  @begin{short}
    If @arg{track-status} is @em{true}, the print operation will try to continue
    report on the status of the print job in the printer queues and printer.
  @end{short}
  This can allow your application to show things like \"out of paper\" issues,
  and when the print job actually reaches the printer.

  This function is often implemented using some form of polling, so it should
  not be enabled unless needed.

  Since 2.10
  @see-class{gtk-print-operation}"
  (setf (gtk-print-operation-track-print-status op) track-status))

(export 'gtk-print-operation-set-track-print-status)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_custom_tab_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-custom-tab-label))

(defun gtk-print-operation-set-custom-tab-label (op label)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[label]{the label to use, or @code{nil} to use the default label}
  @begin{short}
    Sets the label for the tab holding custom widgets.
  @end{short}

  Since 2.10
  @see-class{gtk-print-operation}"
  (setf (gtk-print-operation-custom-tab-label op) label))

(export 'gtk-print-operation-set-custom-tab-label)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_run ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_run" %gtk-print-operation-run)
    gtk-print-operation-result
  (op (g-object gtk-print-operation))
  (action gtk-print-operation-action)
  (parent (g-object gtk-window))
  (error :pointer))

(defun gtk-print-operation-run (op action parent)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-22}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[action]{the action to start}
  @argument[parent]{transient parent of the dialog}
  @begin{return}
    The result of the print operation. A return value of @code{:apply} indicates
    that the printing was completed successfully. In this case, it is a good
    idea to obtain the used print settings with the function
    @fun{gtk-print-operation-get-print-settings} and store them for reuse with
    the next print operation. A value of @code{:in-progress} means the operation
    is running asynchronously, and will emit the \"done\" signal when done.
  @end{return}
  @begin{short}
    Runs the print operation, by first letting the user modify print settings in
    the print dialog, and then print the document.
  @end{short}

  Normally that this function does not return until the rendering of all pages
  is complete. You can connect to the \"status-changed\" signal on op to obtain
  some information about the progress of the print operation. Furthermore, it
  may use a recursive mainloop to show the print dialog.

  If you call the function @fun{gtk-print-operation-set-allow-async} or set the
  @slot[gtk-print-operation]{allow-async} property the operation will run
  asynchronously if this is supported on the platform. The \"done\" signal will
  be emitted with the result of the operation when the it is done, i. e. when
  the dialog is canceled, or when the print succeeds or fails.
  @begin{pre}
   if (settings != NULL)
     gtk_print_operation_set_print_settings (print, settings);

   if (page_setup != NULL)
     gtk_print_operation_set_default_page_setup (print, page_setup);

   g_signal_connect (print, \"begin-print\",
                     G_CALLBACK (begin_print), &data);
   g_signal_connect (print, \"draw-page\",
                     G_CALLBACK (draw_page), &data);

   res = gtk_print_operation_run (print,
                                  GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
                                  parent,
                                  &error);

   if (res == GTK_PRINT_OPERATION_RESULT_ERROR)
    {
      error_dialog = gtk_message_dialog_new (GTK_WINDOW (parent),
                                             GTK_DIALOG_DESTROY_WITH_PARENT,
                                             GTK_MESSAGE_ERROR,
                                             GTK_BUTTONS_CLOSE,
                                             \"Error printing file:\n%s\",
                                             error->message);
      g_signal_connect (error_dialog, \"response\",
                        G_CALLBACK (gtk_widget_destroy), NULL);
      gtk_widget_show (error_dialog);
      g_error_free (error);
    @}
   else if (res == GTK_PRINT_OPERATION_RESULT_APPLY)
    {
      if (settings != NULL)
      g_object_unref (settings);
      settings = g_object_ref (gtk_print_operation_get_print_settings (print));
    @}
  @end{pre}
  Note that the function @sym{gtk-print-operation-run} can only be called once
  on a given @class{gtk-print-operation}.

  Since 2.10
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-allow-async}"
  (with-g-error (err)
    (%gtk-print-operation-run op action parent err)))

(export 'gtk-print-operation-run)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_cancel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_cancel" gtk-print-operation-cancel) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[op]{a @class{gtk-print-operation} object}
  @begin{short}
    Cancels a running print operation.
  @end{short}
  This function may be called from a \"begin-print\", \"paginate\" or
  \"draw-page\" signal handler to stop the currently running print operation.

  Since 2.10
  @see-class{gtk-print-operation}"
  (op (g-object gtk-print-operation)))

(export 'gtk-print-operation-cancel)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_draw_page_finish ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_draw_page_finish"
           gtk-print-operation-draw-page-finish) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[op]{a @class{gtk-print-operation} object}
  @begin{short}
    Signalize that drawing of particular page is complete.
  @end{short}

  It is called after completion of page drawing, e. g. drawing in another
  thread. If the function @fun{gtk-print-operation-set-defer-drawing} was called
  before, then this function has to be called by application. In another case it
  is called by the library itself.

  Since 2.16
  @see-class{gtk-print-operation}
  @fun{gtk-print-operation-set-defer-drawing}"
  (op (g-object gtk-print-operation)))

(export 'gtk-print-operation)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_defer_drawing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_set_defer_drawing"
           gtk-print-operation-set-defer-drawing) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[op]{a @class{gtk-print-operation} object}
  @begin{short}
    Sets up the @class{gtk-print-operation} to wait for calling of the function
    @fun{gtk-print-operation-draw-page-finish} from application. It can be used
    for drawing page in another thread.
  @end{short}

  This function must be called in the callback of \"draw-page\" signal.

  Since 2.16
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-draw-page-finish}"
  (op (g-object gtk-print-operation)))

(export 'gtk-print-operation-set-defer-drawing)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_status ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-get-status))

(defun gtk-print-operatorion-get-status (op)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @return{The status of the print operation.}
  @begin{short}
    Returns the status of the print operation.
  @end{short}
  Also see the function @fun{gtk-print-operation-get-status-string}.

  Since 2.10
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-status-string}"
  (gtk-print-operation-status op))

(export 'gtk-print-operation-get-status)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_status_string ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-get-status-string))

(defun gtk-print-operation-get-status-string (op)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @return{A string representation of the status of the print operation.}
  @begin{short}
    Returns a string representation of the status of the print operation.
  @end{short}
  The string is translated and suitable for displaying the print status e. g.
  in a @class{gtk-statusbar}.

  Use the function @fun{gtk-print-operation-get-status} to obtain a status value
  that is suitable for programmatic use.

  Since 2.10
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-status}"
  (gtk-print-operation-status-string op))

(export 'gtk-print-operation-get-status-string)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_is_finished ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_is_finished" gtk-print-operation-is-finished)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[op]{a @class{gtk-print-operation} object}
  @return{@em{True}, if the print operation is finished.}
  @begin{short}
    A convenience function to find out if the print operation is finished,
    either successfully (@code{:finished}) or unsuccessfully
    (@code{:finished-aborted}).
  @end{short}

  @subheading{Note}
    When you enable print status tracking the print operation can be in a
    non-finished state even after done has been called, as the operation status
    then tracks the print job status on the printer.

  Since 2.10
  @see-class{gtk-print-operation}"
  (op (g-object gtk-print-operation)))

(export 'gtk-print-operation)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_support_selection ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-support-selection))

(defun gtk-print-operation-set-support-selection (op support-selection)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[support-selection]{@em{true} to support selection}
  @begin{short}
    Sets whether selection is supported by @class{gtk-print-operation}.
  @end{short}

  Since 2.18
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-support-selection}"
  (setf (gtk-print-operation-support-selection op) support-selection))

(export 'gtk-print-operation-set-support-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_support_selection ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-get-support-selection))

(defun gtk-print-operation-get-support-selection (op)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @return{Whether the application supports print of selection.}
  @begin{short}
    Gets the value of the @code{\"support-selection\"} property.
  @end{short}

  Since 2.18
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-support-selection}"
  (gtk-print-operation-support-selection op))

(export 'gtk-print-operation-get-support-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_has_selection ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-has-selection))

(defun gtk-print-operation-set-has-selection (op has-selection)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[has-selection]{@em{true} indicates that a selection exists}
  @begin{short}
    Sets whether there is a selection to print.
  @end{short}

  Application has to set number of pages to which the selection will draw by
  the function @fun{gtk-print-operation-set-n-pages} in a callback of
  @code{\"begin-print\"}.

  Since 2.18
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-get-has-selection}
  @see-function{gtk-print-operation-set-n-pages}"
  (setf (gtk-print-operation-has-selection op) has-selection))

(export 'gtk-print-operation-set-has-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_has_selection ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-get-has-selection))

(defun gtk-print-operation-get-has-selection (op)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @return{Whether there is a selection.}
  @begin{short}
    Gets the value of the @code{\"has-selection\"} property.
  @end{short}

  Since 2.18
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-has-selection}"
  (gtk-print-operation-has-selection op))

(export 'gtk-print-operation-get-has-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_embed_page_setup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-set-embed-page-setup))

(defun gtk-print-operation-set-embed-page-setup (op embed)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @argument[embed]{@em{true} to embed page setup selection in the
    @class{gtk-print-dialog}}
  @begin{short}
    Embed page size combo box and orientation combo box into page setup page.
  @end{short}
  Selected page setup is stored as default page setup in
  @class{gtk-print-operation}.

  Since 2.18
  @class{gtk-print-operation}
  @class{gtk-print-dialog}
  @see-function{gtk-print-operation-get-embed-page-setup}"
  (setf (gtk-print-operation-embed-page-setup op) embed))

(export 'gtk-print-operation-set-embed-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_embed_page_setup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-get-embed-page-setup))

(defun gtk-print-operation-get-embed-page-setup (op)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[op]{a @class{gtk-print-operation} object}
  @return{Whether page setup selection combos are embedded.}
  @begin{short}
    Gets the value of @code{\"embed-page-setup\"} property.
  @end{short}

  Since 2.18
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-set-embed-page-setup}"
  (gtk-print-operation-embed-page-setup op))

(export 'gtk-print-operation-get-embed-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_run_page_setup_dialog ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_run_page_setup_dialog" gtk-print-run-page-setup-dialog)
    (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[parent]{transient parent}
  @argument[page-setup]{an existing @class{gtk-page-setup} object}
  @argument[settings]{a @class{gtk-print-settings} object}
  @return{A new @class{gtk-page-setup} object.}
  @begin{short}
    Runs a page setup dialog, letting the user modify the values from
    @arg{page-setup}.
  @end{short}
  If the user cancels the dialog, the returned @class{gtk-page-setup} object is
  identical to the passed in @arg{page-setup}, otherwise it contains the
  modifications done in the dialog.

  Note that this function may use a recursive mainloop to show the page setup
  dialog. See the function @fun{gtk-print-run-page-setup-dialog-async} if this
  is a problem.

  Since 2.10
  @see-class{gtk-page-setup}
  @see-class{gtk-print-settings}
  @see-function{gtk-print-run-page-setup-dialog-async}"
  (parent (g-object gtk-window))
  (page-setup (g-object gtk-page-setup))
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-run-page-setup-dialog)

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

(defcallback gtk-page-setup-done-func-cb :void
    ((page-setup (g-object gtk-page-setup))
     (data :pointer))
  (let ((fn (glib:get-stable-pointer-value data)))
    (funcall fn page-setup)))

;;; ----------------------------------------------------------------------------
;;; gtk_print_run_page_setup_dialog_async ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_run_page_setup_dialog_async"
          %gtk-print-run-page-setup-dialog-async) :void
  (parent (g-object gtk-window))
  (page-setup (g-object gtk-page-setup))
  (settings (g-object gtk-print-settings))
  (done-cb :pointer)
  (data :pointer))

(defun gtk-print-run-page-setup-dialog-async (parent page-setup settings done-cb)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[parent]{transient parent, or @code{nil}}
  @argument[page-setup]{an existing @class{gtk-page-setup}, or @code{nil}}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[done-cb]{a function to call when the user saves the modified page
    setup}
  @begin{short}
    Runs a page setup dialog, letting the user modify the values from
    @arg{page-setup}.
  @end{short}

  In contrast to the function @fun{gtk-print-run-page-setup-dialog}, this
  function returns after showing the page setup dialog on platforms that support
  this, and calls @arg{done-cb} from a signal handler for the \"response\"
  signal of the dialog.

  Since 2.10"
  (with-stable-pointer (done-cb-ptr done-cb)
    (%gtk-print-run-page-setup-dialog-async
                                          parent
                                          page-setup
                                          settings
                                          (callback gtk-page-setup-done-func-cb)
                                          done-cb-ptr)))

(export 'gtk-print-run-page-setup-dialag-async)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_end_preview ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_preview_end_preview"
           gtk-print-operation-preview-end-preview) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-2}
  @argument[preview]{a @class{gtk-print-operation-preview} object}
  @short{Ends a preview.}

  This function must be called to finish a custom print preview.

  Since 2.10
  @see-class{gtk-print-operation}
  @see-class{gtk-print-operation-preview}"
  (preview (g-object gtk-print-operation-preview)))

(export 'gtk-print-operation-preview-end-preview)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_is_selected ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_preview_is_selected"
           gtk-print-operation-preview-is-selected)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[preview]{a @class{gtk-print-operation-preview} object}
  @argument[page-nr]{a page number}
  @return{@em{True} if the page has been selected for printing.}
  @begin{short}
    Returns whether the given page is included in the set of pages that have
    been selected for printing.
  @end{short}

  Since 2.10
  @see-class{gtk-print-operation-preview}"
  (preview (g-object gtk-print-operation-preview))
  (page-nr :int))

(export 'gtk-print-operation-preview-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_render_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_preview_render_page"
           gtk-print-operation-preview-render-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[preview]{a @class{gtk-print-operation-preview} object}
  @argument[page-nr]{the page to render}
  @begin{short}
    Renders a page to the preview, using the print context that was passed to
    the \"preview\" handler together with preview.
  @end{short}

  A custom print preview should use this function in its \"expose\" handler to
  render the currently selected page.

  Note that this function requires a suitable cairo context to be associated
  with the print context.

  Since 2.10
  @see-class{gtk-print-operation-preview-render-page}"
  (preview (g-object gtk-print-operation-preview))
  (page-nr :int))

(export 'gtk-print-operation-preview-render-page)

;;; --- End of file gtk.print-operation.lisp -----------------------------------
