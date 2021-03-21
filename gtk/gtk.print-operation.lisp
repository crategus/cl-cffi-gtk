;;; ----------------------------------------------------------------------------
;;; gtk.print-operation.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     High-level Printing API
;;;
;;; Types and Values
;;;
;;;     GtkPrintStatus
;;;     GtkPrintOperationAction
;;;     GtkPrintOperationResult
;;;     GtkPrintError
;;;
;;;     GTK_PRINT_ERROR
;;;
;;;     GtkPrintOperationPreview
;;;     GtkPrintOperation
;;;
;;; Functions
;;;
;;;     gtk_print_operation_new
;;;     gtk_print_operation_set_allow_async                Accessor
;;;     gtk_print_operation_get_error
;;;     gtk_print_operation_set_default_page_setup         Accessor
;;;     gtk_print_operation_get_default_page_setup         Accessor
;;;     gtk_print_operation_set_print_settings             Accessor
;;;     gtk_print_operation_get_print_settings             Accessor
;;;     gtk_print_operation_set_job_name                   Accessor
;;;     gtk_print_operation_set_n_pages                    Accessor
;;;     gtk_print_operation_get_n_pages_to_print           Accessor
;;;     gtk_print_operation_set_current_page               Accessor
;;;     gtk_print_operation_set_use_full_page              Accessor
;;;     gtk_print_operation_set_unit                       Accessor
;;;     gtk_print_operation_set_export_filename            Accessor
;;;     gtk_print_operation_set_show_progress              Accessor
;;;     gtk_print_operation_set_track_print_status         Accessor
;;;     gtk_print_operation_set_custom_tab_label           Accessor
;;;
;;;     gtk_print_operation_run
;;;     gtk_print_operation_cancel
;;;     gtk_print_operation_draw_page_finish
;;;     gtk_print_operation_set_defer_drawing
;;;     gtk_print_operation_get_status                     Accessor
;;;     gtk_print_operation_get_status_string              Accessor
;;;     gtk_print_operation_is_finished
;;;     gtk_print_operation_set_support_selection          Accessor
;;;     gtk_print_operation_get_support_selection          Accessor
;;;     gtk_print_operation_set_has_selection              Accessor
;;;     gtk_print_operation_get_has_selection              Accessor
;;;     gtk_print_operation_set_embed_page_setup           Accessor
;;;     gtk_print_operation_get_embed_page_setup           Accessor
;;;     gtk_print_run_page_setup_dialog
;;;
;;;     GtkPageSetupDoneFunc
;;;     gtk_print_run_page_setup_dialog_async
;;;
;;;     gtk_print_operation_preview_end_preview
;;;     gtk_print_operation_preview_is_selected
;;;     gtk_print_operation_preview_render_page
;;;
;;; Properties
;;;
;;;         gboolean   allow-async             Read / Write
;;;             gint   current-page            Read / Write
;;;            gchar*  custom-tab-label        Read / Write
;;;     GtkPageSetup*  default-page-setup      Read / Write
;;;         gboolean   embed-page-setup        Read / Write
;;;            gchar*  export-filename         Read / Write
;;;         gboolean   has-selection           Read / Write
;;;            gchar*  job-name                Read / Write
;;;             gint   n-pages                 Read / Write
;;;             gint   n-pages-to-print        Read
;;; GtkPrintSettings*  print-settings          Read / Write
;;;         gboolean   show-progress           Read / Write
;;;   GtkPrintStatus   status                  Read
;;;            gchar*  status-string           Read
;;;         gboolean   support-selection       Read / Write
;;;         gboolean   track-print-status      Read / Write
;;;          GtkUnit   unit                    Read / Write
;;;         gboolean   use-full-page           Read / Write
;;;
;;; Signals
;;;
;;;             void   begin-print             Run Last
;;;          GObject*  create-custom-widget    Run Last
;;;             void   custom-widget-apply     Run Last
;;;             void   done                    Run Last
;;;             void   draw-page               Run Last
;;;             void   end-print               Run Last
;;;         gboolean   paginate                Run Last
;;;         gboolean   preview                 Run Last
;;;             void   request-page-setup      Run Last
;;;             void   status-changed          Run Last
;;;             void   update-custom-widget    Run Last
;;;             void   got-page-size           Run Last
;;;             void   ready                   Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkPrintOperationPreview
;;;
;;;     GObject
;;;     ╰── GtkPrintOperation
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPrintOperation implements GtkPrintOperationPreview.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

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
 "@version{2020-4-8}
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
    @entry[:initial]{The printing has not started yet. This status is set
      initially, and while the print dialog is shown.}
    @entry[:preparing]{This status is set while the \"begin-print\" signal is
      emitted and during pagination.}
    @entry[:generating-data]{This status is set while the pages are being
      rendered.}
    @entry[:sending-data]{The print job is being sent off to the printer.}
    @entry[:pending]{The print job has been sent to the printer, but is not
      printed for some reason, e.g. the printer may be stopped.}
    @entry[:pending-issue]{Some problem has occurred during printing,
      e.g. a paper jam.}
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
 "@version{2020-4-8}
  @begin{short}
    The action parameter to the function @fun{gtk-print-operation-run}
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
      @slot[gtk-print-operation]{export-filename} property of the
      @class{gtk-print-operation} object to be set.}
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
 "@version{2020-4-8}
  @begin{short}
    A value of this type is returned by the function
    @fun{gtk-print-operation-run}.
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
 "@version{2020-4-8}
  @begin{short}
    Error codes that identify various errors that can occur while using the
    GTK+ printing support.
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
  @end{table}
  @see-class{gtk-print-operation}")

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_ERROR
;;;
;;; #define GTK_PRINT_ERROR gtk_print_error_quark ()
;;;
;;; The error domain for GtkPrintError errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkPrintOperationPreview
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkPrintOperationPreview" gtk-print-operation-preview
  (:export t
   :type-initializer "gtk_print_operation_preview_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-preview atdoc:*class-name-alias*)
      "Interface"
      (documentation 'gtk-print-operation-preview 'type)
 "@version{2021-3-17}
  @begin{short}
    The @sym{gtk-print-operation-preview} interface is implemented by the
    @class{gtk-print-operation} class.
  @end{short}

  By default the @class{gtk-print-operation} object uses an external
  application to do print preview. To implement a custom print preview, an
  application must connect to the preview signal. The functions
  @fun{gtk-print-operation-preview-render-page},
  @fun{gtk-print-operation-preview-end-preview} and
  @fun{gtk-print-operation-preview-is-selected} are useful when implementing
  a print preview.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-preview-render-page}
  @see-function{gtk-print-operation-preview-end-preview}
  @see-function{gtk-print-operation-preview-is-selected}")

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
 "@version{2020-4-8}
  @begin{short}
    @sym{gtk-print-operation} is the high-level, portable printing API.
  @end{short}
  It looks a bit different than other GTK+ dialogs such as the
  @class{gtk-file-chooser}, since some platforms do not expose enough
  infrastructure to implement a good print dialog. On such platforms,
  @sym{gtk-print-operation} uses the native print dialog. On platforms which
  do not provide a native print dialog, GTK+ uses its own, see
  @class{gtk-print-unix-dialog}.

  The typical way to use the high-level printing API is to create a
  @sym{gtk-print-operation} object with the @fun{gtk-print-operation-new}
  function when the user selects to print. Then you set some properties on it,
  e.g. the page size, any @class{gtk-print-settings} from previous print
  operations, the number of pages, the current page, etc.

  Then you start the print operation by calling the
  @fun{gtk-print-operation-run} function. It will then show a dialog, let the
  user select a printer and options. When the user finished the dialog various
  signals will be emitted on the @sym{gtk-print-operation}, the main one being
  \"draw-page\", which you are supposed to catch and render the page on the
  provided @class{gtk-print-context} using Cairo.

  By default @sym{gtk-print-operation} uses an external application to do print
  preview. To implement a custom print preview, an application must connect to
  the \"preview\" signal. The functions
  @fun{gtk-print-operation-preview-render-page},
  @fun{gtk-print-operation-preview-end-preview} and
  @fun{gtk-print-operation-preview-is-selected} are useful when
  implementing a print preview.
  @begin[Example]{dictionary}
    The high-level printing API.
    @begin{pre}
(defvar *print-settings* nil)

(defun do-print-operation (window)
  (let ((response nil)
        (print (gtk-print-operation-new)))
    ;; Connect signal handlers for the print operation
    (g-signal-connect print \"draw-page\" #'draw-page)
    (g-signal-connect print \"begin-print\" #'begin-print)
    ;; Restore the print settings
    (when *print-settings*
      (setf (gtk-print-operation-print-settings print) *print-settings*))
    ;; Perform the print operation
    (setf response (gtk-print-operation-run print :print-dialog window))
    ;; Check the response and save the print settings
    (when (eq :apply response)
      (setf *print-settings* (gtk-print-operation-print-settings print)))))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"begin-print\" signal}
      @begin{pre}
 lambda (operation context)    : Run Last
      @end{pre}
      Emitted after the user has finished changing print settings in the dialog,
      before the actual rendering starts. A typical use for the \"begin-print\"
      signal is to use the parameters from the @class{gtk-print-context} and
      paginate the document accordingly, and then set the number of pages with
      the function @fun{gtk-print-operation-n-pages}.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} object on which the
          signal was emitted.}
        @entry[context]{The @class{gtk-print-context} object for the current
          operation.}
      @end{table}
    @subheading{The \"create-custom-widget\" signal}
      @begin{pre}
 lambda (operation)    : Run Last
      @end{pre}
      Emitted when displaying the print dialog. If you return a widget in a
      handler for this signal it will be added to a custom tab in the print
      dialog. You typically return a container widget with multiple widgets in
      it. The print dialog owns the returned widget, and its lifetime is not
      controlled by the application. However, the widget is guaranteed to stay
      around until the \"custom-widget-apply\" signal is emitted on the
      operation. Then you can read out any information you need from the
      widgets.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} object on which the
          signal was emitted.}
        @entry[Returns]{A custom widget that gets embedded in the print dialog,
          or @code{nil}.}
      @end{table}
    @subheading{The \"custom-widget-apply\" signal}
      @begin{pre}
 lambda (operation widget)    : Run Last
      @end{pre}
      Emitted right before the \"begin-print\" signal if you added a custom
      widget in the \"create-custom-widget\" handler. When you get this signal
      you should read the information from the custom widgets, as the widgets
      are not guaraneed to be around at a later time.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} object on which the
          signal was emitted.}
        @entry[widget]{The custom widget added in a \"create-custom-widget\"
          handler.}
      @end{table}
    @subheading{The \"done\" signal}
      @begin{pre}
 lambda (operation result)    : Run Last
      @end{pre}
      Emitted when the print operation run has finished doing everything
      required for printing. @arg{result} gives you information about what
      happened during the run. If @arg{result} is @code{:error} then you can
      call the function @code{gtk_print_operation_get_error ()} for more
      information. If you enabled print status tracking then the function
      @fun{gtk-print-operation-is-finished} may still return @em{false} after
      the \"done\" signal was emitted.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} object on which the
          signal was emitted.}
        @entry[result]{The result of type @symbol{gtk-print-operation-result}
          of the print operation.}
      @end{table}
    @subheading{The \"draw-page\" signal}
      @begin{pre}
 lambda (operation context page-nr)    : Run Last
      @end{pre}
      Emitted for every page that is printed. The signal handler must render the
      @arg{page-nr}'s page onto the Cairo context obtained from @arg{context}
      using the function @fun{gtk-print-context-cairo-context}. Use the
      functions @fun{gtk-print-operation-use-full-page} and
      @fun{gtk-print-operation-unit} before starting the print operation to set
      up the transformation of the Cairo context according to your needs.
      @begin{pre}
(defun draw-page (operation context page-nr)
  (declare (ignore operation page-nr))
  (let ((text-height 0)
        (cr (gtk-print-context-cairo-context context))
        (width (floor (gtk-print-context-get-width context)))
        (layout (gtk-print-context-create-pango-layout context)))
    ;; Print a grey colored header
    (cairo-rectangle cr 0 0 width *header-height*)
    (cairo-set-source-rgb cr 0.9 0.9 0.9)
    (cairo-fill cr)
    ;; Set the font and text to print
    (setf (pango-layout-font-description layout)
          (pango-font-description-from-string \"sans 14\"))
    (setf (pango-layout-text layout) \"Title\")
    (setf (pango-layout-width layout) (* width +pango-scale+))
    (setf (pango-layout-alignment layout) :center)
    ;; Get the height of the text
    (multiple-value-bind (width height)
        (pango-layout-size layout)
      (setf text-height (/ height +pango-scale+)))
    ;; Set color to black and center the text in header
    (cairo-set-source-rgb cr 0.0 0.0 0.0)
    (cairo-move-to cr 0 (floor (/ (- *header-height* text-height) 2)))
    (pango-cairo-show-layout cr layout)))
      @end{pre}
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} object on which the
          signal was emitted.}
        @entry[context]{The @class{gtk-print-context} object for the current
          operation.}
        @entry[page-nr]{The 0-based number of the currently printed page.}
      @end{table}
    @subheading{The \"end-print\" signal}
      @begin{pre}
 lambda (operation context)    : Run Last
      @end{pre}
      Emitted after all pages have been rendered. A handler for this signal can
      clean up any resources that have been allocated in the \"begin-print\"
      handler.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} object on which the
          signal was emitted.}
        @entry[context]{The @class{gtk-print-context} object for the current
          operation.}
      @end{table}
    @subheading{The \"paginate\" signal}
      @begin{pre}
 lambda (operation context)    : Run Last
      @end{pre}
      Emitted after the \"begin-print\" signal, but before the actual rendering
      starts. It keeps getting emitted until a connected signal handler returns
      @em{true}.
      The \"paginate\" signal is intended to be used for paginating a document
      in small chunks, to avoid blocking the user interface for a long time.
      The signal handler should update the number of pages using the function
      @fun{gtk-print-operation-n-pages}, and return @em{true} if the document
      has been completely paginated. If you do not need to do pagination in
      chunks, you can simply do it all in the \"begin-print\" handler, and set
      the number of pages from there.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} object on which the
          signal was emitted.}
        @entry[context]{The @class{gtk-print-context} object for the current
          operation.}
        @entry[Returns]{@em{True} if pagination is complete.}
      @end{table}
    @subheading{The \"preview\" signal}
      @begin{pre}
 lambda (operation preview context parent)    : Run Last
      @end{pre}
      Gets emitted when a preview is requested from the native dialog. The
      default handler for this signal uses an external viewer application to
      preview. To implement a custom print preview, an application must return
      @em{true} from its handler for this signal. In order to use the provided
      context for the preview implementation, it must be given a suitable cairo
      context with the function @fun{gtk-print-context-set-cairo-context}. The
      custom preview implementation can use the functions
      @fun{gtk-print-operation-preview-is-selected} and
      @fun{gtk-print-operation-preview-render-page} to find pages which are
      selected for print and render them. The preview must be finished by
      calling the function @fun{gtk-print-operation-preview-end-preview},
      typically in response to the user clicking a close button.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} object on which the
          signal was emitted.}
        @entry[preview]{The @class{gtk-print-preview-operation} object for the
          current operation.}
        @entry[context]{The @class{gtk-print-context} object that will be used.}
        @entry[parent]{The @class{gtk-window} widget to use as window parent,
          or @code{nil}.}
        @entry[Returns]{@em{True} if the listener wants to take over control of
          the preview.}
      @end{table}
    @subheading{The \"request-page-setup\" signal}
      @begin{pre}
 lambda (operation context page-nr setup)    : Run Last
      @end{pre}
      Emitted once for every page that is printed, to give the application a
      chance to modify the page setup. Any changes done to the page setup will
      be in force only for printing this page.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} object on which the
          signal was emitted.}
        @entry[context]{The @class{gtk-print-context} object for the current
          operation.}
        @entry[page-nr]{The 0-based number of the currently printed page.}
        @entry[setup]{The @class{gtk-page-setup} object.}
      @end{table}
    @subheading{The \"status-changed\" signal}
      @begin{pre}
 lambda (operation)    : Run Last
      @end{pre}
      Emitted at between the various phases of the print operation. See the
      @symbol{gtk-print-status} enumeration for the phases that are being
      discriminated. Use the function @fun{gtk-print-operation-status} to
      find out the current status.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} object on which the
          signal was emitted.}
      @end{table}
    @subheading{The \"update-custom-widget\" signal}
      @begin{pre}
 lambda (operation widget setup settings)    : Run Last
      @end{pre}
      Emitted after change of the selected printer. The actual page setup and
      print settings are passed to the custom widget, which can actualize
      itself according to this change.
      @begin[code]{table}
        @entry[operation]{The @sym{gtk-print-operation} object on which the
          signal was emitted.}
        @entry[widget]{The custom widget added in the \"create-custom-widget\"
          handler.}
        @entry[setup]{Actual @class{gtk-page-setup} object.}
        @entry[settings]{Actual @class{gtk-print-settings} object.}
      @end{table}
    @subheading{The \"got-page-size\" signal}
      @begin{pre}
 lambda (preview context page-setup)    : Run Last
      @end{pre}
      The \"got-page-size\" signal is emitted once for each page that gets
      rendered to the preview. A handler for this signal should update the
      context according to the @arg{page-setup} argument and set up a suitable
      Cairo context, using the function
      @fun{gtk-print-context-set-cairo-context}.
      @begin[code]{table}
        @entry[preview]{The @class{gtk-print-operation-preview} object on which
          the signal is emitted.}
        @entry[context]{The current @class{gtk-print-context} object.}
        @entry[page-setup]{The @class{gtk-page-setup} object for the current
          page.}
      @end{table}
    @subheading{The \"ready\" signal}
      @begin{pre}
 lambda (preview context)    : Run Last
      @end{pre}
      The \"ready\" signal gets emitted once per preview operation, before the
      first page is rendered. A handler for this signal can be used for setup
      tasks.
      @begin[code]{table}
        @entry[preview]{The @class{gtk-print-operation-preview} object on which
          the signal is emitted.}
        @entry[context]{The current @class{gtk-print-context} object.}
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
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-print-operation-allow-async ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "allow-async"
                                               'gtk-print-operation) 't)
 "The @code{allow-async} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether the print operation may run asynchronously or not. Some
  systems do not support asynchronous printing, but those that do will return
  @code{:in-progress} as the status, and emit the \"done\" signal when the
  operation is actually done. The Windows port does not support asynchronous
  operation at all, this is unlikely to change. On other platforms, all actions
  except for @code{:export} support asynchronous operation. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-allow-async atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-allow-async 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-allow-async object) => alloc-async}
  @syntax[]{(setf (gtk-print-operation-allow-aysnc object) alloc-async)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[allow-async]{@em{true} to allow asynchronous operation}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{allow-async} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{(setf gtk-print-operation-allow-async)} sets
  whether the function @fun{gtk-print-operation-run} may return before the
  print operation is completed.

  Note that some platforms may not allow asynchronous operation.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-run}")

;;; --- gtk-print-operation-current-page ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "current-page"
                                               'gtk-print-operation) 't)
 "The @code{current-page} property of type @code{:int} (Read / Write) @br{}
  The current page in the document. If this is set before the function
  @fun{gtk-print-operation-run}, the user will be able to select to print only
  the current page. Note that this only makes sense for pre-paginated documents.
  @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-current-page atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-current-page 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-current-page object) => current-page}
  @syntax[]{(setf (gtk-print-operation-current-page object) current-page)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[current-page]{the current page, 0-based}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{current-page} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{(setf gtk-print-operation-current-page)}
  sets the current page.

  If this is called before the function @fun{gtk-print-operation-run}, the user
  will be able to select to print only the current page.

  Note that this only makes sense for pre-paginated documents.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-custom-tab-label -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "custom-tab-label"
                                               'gtk-print-operation) 't)
 "The @code{custom-tab-label} property of type @code{:string} (Read / Write)
  @br{}
  Used as the label of the tab containing custom widgets. Note that this
  property may be ignored on some platforms. If this is @code{nil}, GTK+ uses
  a default label. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-custom-tab-label
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-custom-tab-label 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-tab-label object) => label}
  @syntax[]{(setf (gtk-print-operation-tab-label object) label)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[label]{a @code{:string} with the label to use, or @code{nil} to use
    the default label}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{custom-tab-label} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{(setf gtk-print-operation-custom-tab-label)}
  sets the label for the tab holding custom widgets.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-default-page-setup ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "default-page-setup"
                                               'gtk-print-operation) 't)
 "The @code{default-page-setup} property of type @class{gtk-page-setup}
  (Read / Write) @br{}
  The page setup used by default. This page setup will be used by the function
  @fun{gtk-print-operation-run}, but it can be overridden on a per-page basis
  by connecting to the \"request-page-setup\" signal.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-default-page-setup
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-default-page-setup 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-default-page-setup object) => page-setup}
  @syntax[]{(setf (gtk-print-operation-default-page-setup object) page-setup)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[page-setup]{a @class{gtk-page-setup}, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{default-page-setup} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{gtk-print-operation-default-page-setup} returns
  the default page setup. The slot access function
  @sym{(setf gtk-print-operation-current-page)}  makes @arg{page-setup} the
  default page setup for the print operation.

  This page setup will be used by the function @fun{gtk-print-operation-run},
  but it can be overridden on a per-page basis by connecting to the
  \"request-page-setup\" signal.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-embed-page-setup -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "embed-page-setup"
                                               'gtk-print-operation) 't)
 "The @code{embed-page-setup} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, the page size combo box and the orientation combo box are
  embedded into the page setup page. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-embed-page-setup
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-embed-page-setup 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-embed-page-setup object) => embed}
  @syntax[]{(setf (gtk-print-operation-embed-page-setup object) embed)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[embed]{@em{true} to embed page setup selection in the print dialog}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{embed-page-setup} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{gtk-print-operation-embed-page-setup} returns
  whether page setup selection combos are embedded. The slot access function
  @sym{(setf gtk-print-operation-embed-page-setup)} embed page size combo box
  and orientation combo box into page setup page.

  Selected page setup is stored as default page setup in the
  @class{gtk-print-operation} object.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-export-filename ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "export-filename"
                                               'gtk-print-operation) 't)
 "The @code{export-filename} property of type @code{:string}
  (Read / Write) @br{}
  The name of a file to generate instead of showing the print dialog. Currently,
  PDF is the only supported format. The intended use of this property is for
  implementing \"Export to PDF\" actions. \"Print to PDF\" support is
  independent of this and is done by letting the user pick the \"Print to PDF\"
  item from the list of printers in the print dialog. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-export-filename atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-export-filename 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-export-filename object) => filename}
  @syntax[]{(setf (gtk-print-operation-export-filename object) filename)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[filename]{a @code{:string} with the filename for the exported file}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{export-filename} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{(setf gtk-print-operation-export-filename)}
  sets up the @class{gtk-print-operation} object to generate a file instead of
  showing the print dialog.

  The indended use of this function is for implementing \"Export to PDF\"
  actions. Currently, PDF is the only supported format.

  \"Print to PDF\" support is independent of this and is done by letting the
  user pick the \"Print to PDF\" item from the list of printers in the print
  dialog.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-has-selection --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-selection"
                                               'gtk-print-operation) 't)
 "The @code{has-selection} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether there is a selection in your application. This can allow
  your application to print the selection. This is typically used to make a
  \"Selection\" button sensitive. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-has-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-has-selection 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-has-selection object object) => has-selection}
  @syntax[]{(setf (gtk-print-operation-has-selection object) has-selection)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[has-selection]{@em{true} indicates that a selection exists}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{has-selection} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{gtk-print-operation-has-selection} returns
  whether there is a selection. The slot access function
  @sym{(setf gtk-print-operation-has-selection)} sets whether there is a
  selection to print.

  The application has to set the number of pages to which the selection will
  draw by the function @fun{gtk-print-operation-n-pages} in a callback of the
  \"begin-print\" signal.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-job-name -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "job-name"
                                               'gtk-print-operation) 't)
 "The @code{job-name} property of type @code{:string} (Read / Write) @br{}
  A string used to identify the job, e.g. in monitoring applications like
  eggcups. If you do not set a job name, GTK+ picks a default one by numbering
  successive print jobs. @br{}
  Default value: \"\" ")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-job-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-job-name 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-job-name object) => job-name}
  @syntax[]{(setf (gtk-print-operation-job-name object) job-name)}
  @argument[object]{a @class{gtk-print-operation-job-name} object}
  @argument[job-name]{a string that identifies the print job}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{job-name} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{(setf gtk-print-operation-job-name)} sets the
  name of the print job.

  The name is used to identify the job, e.g. in monitoring applications like
  eggcups.

  If you do not set a job name, GTK+ picks a default one by numbering
  successive print jobs.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-n-pages --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "n-pages"
                                               'gtk-print-operation) 't)
 "The @code{n-pages} property of type @code{:int} (Read / Write) @br{}
  The number of pages in the document. This must be set to a positive number
  before the rendering starts. It may be set in a \"begin-print\" signal
  hander. Note that the page numbers passed to the \"request-page-setup\" and
  \"draw-page\" signals are 0-based, i.e. if the user chooses to print all
  pages, the last \"draw-page\" signal will be for page @code{n-pages} - 1.@br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-n-pages atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-n-pages 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-n-pages object) => n-pages}
  @syntax[]{(setf (gtk-print-operation-n-pages object) n-pages)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[n-pages]{an integer with the number of pages}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{n-pages} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The @sym{(setf gtk-print-operation-n-pages)} slot access function
  sets the number of pages in the document.

  This must be set to a positive number before the rendering starts. It may be
  set in a \"begin-print\" signal hander.

  Note that the page numbers passed to the \"request-page-setup\" and
  \"draw-page\" signals are 0-based, i.e. if the user chooses to print all
  pages, the last \"draw-page\" signal will be for page @arg{n-pages} - 1.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-n-pages-to-print -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "n-pages-to-print"
                                               'gtk-print-operation) 't)
 "The @code{n-pages-to-print} property of type @code{:int} (Read) @br{}
  The number of pages that will be printed. Note that this value is set during
  print preparation phase @code{:preparing}, so this value should never be get
  before the data generation phase @code{:generating-data}. You can connect to
  the \"status-changed\" signal and call the function
  @fun{gtk-print-operation-n-pages-to-print} when print status is
  @code{:generating-data}. This is typically used to track the progress of
  print operation. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-n-pages-to-print
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-n-pages-to-print 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-n-pages-to-print object) => n-pages}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[n-pages]{the number of pages to print}
  @begin{short}
    Accessor of the @code{n-pages-to-print} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{gtk-print-operation-n-pages} returns the number
  of pages that will be printed.

  Note that this value is set during print preparation phase @code{:preparing},
  so this function should never be called before the data generation phase
  @code{:generating-data}. You can connect to the \"status-changed\" signal and
  call the function @sym{gtk-print-operation-n-pages-to-print} when print
  status is @code{:generating-data}. This is typically used to track the
  progress of print operation.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-print-settings -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "print-settings"
                                               'gtk-print-operation) 't)
 "The @code{print-settings} property of type @class{gtk-print-settings}
  (Read / Write) @br{}
  The print settings used for initializing the dialog. Setting this property is
  typically used to re-establish print settings from a previous print operation,
  see the function @fun{gtk-print-operation-run}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-print-settings atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-print-settings 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-print-settings object) => settings}
  @syntax[]{(setf (gtk-print-operation-print-settings object) settings)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[settings]{a @class{gtk-print-settings} object}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{print-settings} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{gtk-print-operation-print-settings} returns
  the current print settings. The slot access function
  @sym{(setf gtk-print-operation-print-settings)} sets the print settings for
  the print operation. This is typically used to re-establish print settings
  from a previous print operation.

  Note that the return value is @code{nil} until either the function
  @sym{gtk-print-operation-print-settings} or the function
  @fun{gtk-print-operation-run} have been called.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-run}")

;;; --- gtk-print-operation-show-progress --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-progress"
                                               'gtk-print-operation) 't)
 "The @code{show-progress} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether to show a progress dialog during the print operation. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-show-progress atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-show-progress 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-show-progress object) => show-progress}
  @syntax[]{(setf (gtk-print-operation-show-progress object) show-progress)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[show-progress]{@em{true} to show a progress dialog}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{show-progress} of the
    @class{gtk-print-operation} class.
  @end{short}

  If @arg{show-progress} is @em{true}, the print operation will show a progress
  dialog during the print operation.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-status ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "status"
                                               'gtk-print-operation) 't)
 "The @code{status} property of type @symbol{gtk-print-status} (Read) @br{}
  The status of the print operation. @br{}
  Default value: @code{:initial}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-status atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-status 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-status object) => status}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[status]{the status of type @symbol{gtk-print-status} of the print
    operation}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{status} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{gtk-print-operation-status} returns the status
  of the print operation.

  Also see the function @fun{gtk-print-operation-status-string}.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-status-string}")

;;; --- gtk-print-operation-status-string --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "status-string"
                                               'gtk-print-operation) 't)
 "The @code{status-string} property of type @code{:string} (Read) @br{}
  A string representation of the status of the print operation. The string is
  translated and suitable for displaying the print status e.g. in a
  @class{gtk-statusbar} widget. See the @code{status} property for a status
  value that is suitable for programmatic use. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-status-string atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-status-string 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-status-string object) => status-string}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[status-string]{a string representation of the status of the print
    operation}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{status-string} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{gtk-print-operation-status-string} returns a
  string representation of the status of the print operation.

  The string is translated and suitable for displaying the print status e.g.
  in a @class{gtk-statusbar} widget.

  Use the function @fun{gtk-print-operation-status} to obtain a status value
  that is suitable for programmatic use.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-status}")

;;; --- gtk-print-operation-support-selection ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "support-selection"
                                               'gtk-print-operation) 't)
 "The @code{support-selection} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, the print operation will support print of selection. This
  allows the print dialog to show a \"Selection\" button. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-support-selection
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-support-selection 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-status-support-selection object) => support-selection}
  @syntax[]{(setf (gtk-print-operation-support-selection object) support-selection)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[support-selection]{@em{true} to support selection}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{support-selection} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The slot access function @sym{gtk-print-operation-support-selection} returns
  whether the application supports print of selection. The slot access function
  @sym{(setf gtk-print-operation-support-selection)} sets whether selection is
  supported by the print operation.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-track-print-status ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "track-print-status"
                                               'gtk-print-operation) 't)
 "The @code{track-print-status} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, the print operation will try to continue report on the status
  of the print job in the printer queues and printer. This can allow your
  application to show things like \"out of paper\" issues, and when the print
  job actually reaches the printer. However, this is often implemented using
  polling, and should not be enabled unless needed. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-track-print-status
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-track-print-status 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-track-print-status object) => track-status}
  @syntax[]{(setf (gtk-print-operation-track-print-status object) track-status)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[track-status]{@em{true} to track status after printing}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{track-print-status} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  If @arg{track-status} is @em{true}, the print operation will try to continue
  report on the status of the print job in the printer queues and printer. This
  can allow your application to show things like \"out of paper\" issues, and
  when the print job actually reaches the printer.

  This function is often implemented using some form of polling, so it should
  not be enabled unless needed.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-unit -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "unit"
                                               'gtk-print-operation) 't)
 "The @code{unit} property of type @symbol{gtk-unit} (Read / Write) @br{}
  The transformation for the Cairo context obtained from
  @class{gtk-print-context} is set up in such a way that distances are measured
  in units of a value of the @symbol{gtk-unit} enumeration. @br{}
  Default value: @code{:pixel}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-unit atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-unit 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-unit object) => unit}
  @syntax[]{(setf (gtk-print-operation-unit object) unit)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[unit]{the unit to use}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{unit} slot of the
    @class{gtk-print-operation} class.
  @end{short}

  The @sym{(setf gtk-print-operation-unit)} slot access function sets up the
  transformation for the Cairo context obtained from @class{gtk-print-context}
  in such a way that distances are measured in units of a value of the
  @symbol{gtk-unit} enumeration.
  @see-class{gtk-print-operation}")

;;; --- gtk-print-operation-use-full-page --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-full-page"
                                               'gtk-print-operation) 't)
 "The @code{use-full-page} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, the transformation for the Cairo context obtained from
  @class{gtk-print-context} puts the origin at the top left corner of the page,
  which may not be the top left corner of the sheet, depending on page
  orientation and the number of pages per sheet. Otherwise, the origin is at
  the top left corner of the imageable area, i.e. inside the margins. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-operation-use-full-page atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-operation-use-full-page 'function)
 "@version{2020-4-8}
  @syntax[]{(gtk-print-operation-use-full-page object) => full-page}
  @syntax[]{(setf (gtk-print-operation-use-full-page object) full-page)}
  @argument[object]{a @class{gtk-print-operation} object}
  @argument[full-page]{@em{true} to set up the @class{gtk-print-context} for
    the full page}
  @begin{short}
    Accessor of the @slot[gtk-print-operation]{use-full-page} of the
    @class{gtk-print-operation} class.
  @end{short}

  If @arg{full-page} is @em{true}, the transformation for the Cairo context
  obtained from @class{gtk-print-context} puts the origin at the top left corner
  of the page, which may not be the top left corner of the sheet, depending on
  page orientation and the number of pages per sheet. Otherwise, the origin is
  at the top left corner of the imageable area, i.e. inside the margins.
  @see-class{gtk-print-operation}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-operation-new))

(defun gtk-print-operation-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @return{A new @class{gtk-print-operation} object.}
  @begin{short}
    Creates a new @class{gtk-print-operation} object.
  @end{short}
  @see-class{gtk-print-operation}"
  (make-instance 'gtk-print-operation))

(export 'gtk-print-operation-new)

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

;; TODO: Do more work on reporting errors

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_run ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_run" %gtk-print-operation-run)
    gtk-print-operation-result
  (operation (g-object gtk-print-operation))
  (action gtk-print-operation-action)
  (parent (g-object gtk-window))
  (error :pointer))

(defun gtk-print-operation-run (operation action parent)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[operation]{a @class{gtk-print-operation} object}
  @argument[action]{the action of type @symbol{gtk-print-operation-action} to
    start}
  @argument[parent]{transient parent of the dialog}
  @begin{return}
    The result of the print operation. A return value of @code{:apply}
    indicates that the printing was completed successfully. In this case, it
    is a good idea to obtain the used print settings with the function
    @fun{gtk-print-operation-print-settings} and store them for reuse with
    the next print operation. A value of @code{:in-progress} means the operation
    is running asynchronously, and will emit the \"done\" signal when done.
  @end{return}
  @begin{short}
    Runs the print operation, by first letting the user modify print settings
    in the print dialog, and then print the document.
  @end{short}

  Normally that this function does not return until the rendering of all pages
  is complete. You can connect to the \"status-changed\" signal on the print
  operation to obtain some information about the progress of the print
  operation. Furthermore, it may use a recursive mainloop to show the print
  dialog.

  If you call the function @fun{gtk-print-operation-allow-async} the operation
  will run asynchronously if this is supported on the platform. The \"done\"
  signal will be emitted with the result of the operation when the it is done,
  i.e. when the dialog is canceled, or when the print succeeds or fails.

  Note that the function @sym{gtk-print-operation-run} can only be called once
  on a given @class{gtk-print-operation}.
  @begin[Example]{dictionary}
    @begin{pre}
(defun do-print-operation (window)
  (let ((response nil)
        (print (gtk-print-operation-new)))
    ;; Connect signal handlers for the print operation
    (g-signal-connect print \"draw-page\" #'draw-page)
    (g-signal-connect print \"begin-print\" #'begin-print)
    ;; Restore the print settings
    (when *print-settings*
      (setf (gtk-print-operation-print-settings print) *print-settings*))
    ;; Restore the page setup
    (when *page-setup*
      (setf (gtk-print-operation-page-setup print) *page-setup*))
    ;; Perform the print operation
    (setf response (gtk-print-operation-run print :print-dialog window))
    ;; Check the response and save the print settings
    (when (eq :apply response)
      (setf *print-settings* (gtk-print-operation-print-settings print)))))
    @end{pre}
  @end{dictionary}
  @see-class{gtk-print-operation}
  @see-symbol{gtk-print-operation-action}
  @see-function{gtk-print-operation-allow-async}"
  (with-g-error (err)
    (%gtk-print-operation-run operation action parent err)))

(export 'gtk-print-operation-run)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_cancel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_cancel" gtk-print-operation-cancel) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[operation]{a @class{gtk-print-operation} object}
  @begin{short}
    Cancels a running print operation.
  @end{short}
  This function may be called from a \"begin-print\", \"paginate\" or
  \"draw-page\" signal handler to stop the currently running print operation.
  @see-class{gtk-print-operation}"
  (operation (g-object gtk-print-operation)))

(export 'gtk-print-operation-cancel)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_draw_page_finish ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_draw_page_finish"
           gtk-print-operation-draw-page-finish) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[operation]{a @class{gtk-print-operation} object}
  @begin{short}
    Signalize that drawing of the particular page is complete.
  @end{short}

  The function is called after completion of page drawing, e.g. drawing in
  another thread. If the function @fun{gtk-print-operation-set-defer-drawing}
  was called before, then this function has to be called by the application.
  In another case it is called by the library itself.
  @see-class{gtk-print-operation}
  @fun{gtk-print-operation-set-defer-drawing}"
  (operation (g-object gtk-print-operation)))

(export 'gtk-print-operation)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_defer_drawing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_set_defer_drawing"
           gtk-print-operation-set-defer-drawing) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[operation]{a @class{gtk-print-operation} object}
  @begin{short}
    Sets up the @class{gtk-print-operation} object to wait for calling of the
    function @fun{gtk-print-operation-draw-page-finish} from the application.
  @end{short}
  It can be used for drawing page in another thread.

  This function must be called in the callback of a \"draw-page\" signal.
  @see-class{gtk-print-operation}
  @see-function{gtk-print-operation-draw-page-finish}"
  (operation (g-object gtk-print-operation)))

(export 'gtk-print-operation-set-defer-drawing)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_is_finished ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_is_finished" gtk-print-operation-is-finished)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-4-8}
  @argument[operation]{a @class{gtk-print-operation} object}
  @return{@em{True}, if the print operation is finished.}
  @begin{short}
    A convenience function to find out if the print operation is finished,
    either successfully @code{:finished} or unsuccessfully
    @code{:finished-aborted}.
  @end{short}
  @begin[Note]{dictionary}
    When you enable print status tracking the print operation can be in a
    non-finished state even after the \"done\" signal has been called, as the
    operation status then tracks the print job status on the printer.
  @end{dictionary}
  @see-class{gtk-print-operation}"
  (operation (g-object gtk-print-operation)))

(export 'gtk-print-operation-is-finished)

;;; ----------------------------------------------------------------------------
;;; gtk_print_run_page_setup_dialog ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_run_page_setup_dialog" gtk-print-run-page-setup-dialog)
    (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
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
  dialog. See the @code{gtk_print_run_page_setup_dialog_async()} function if
  this is a problem.
  @see-class{gtk-print-operation}
  @see-class{gtk-page-setup}
  @see-class{gtk-print-settings}"
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

;; TODO: This function does not work. We do not export the function.

(defcfun ("gtk_print_run_page_setup_dialog_async"
          %gtk-print-run-page-setup-dialog-async) :void
  (parent (g-object gtk-window))
  (page-setup (g-object gtk-page-setup))
  (settings (g-object gtk-print-settings))
  (done-cb :pointer)
  (data :pointer))

(defun gtk-print-run-page-setup-dialog-async (parent page-setup settings done-cb)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-24}
  @argument[parent]{a @class{gtk-window} transient parent, or @code{nil}}
  @argument[page-setup]{an existing @class{gtk-page-setup}, or @code{nil}}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[done-cb]{a function to call when the user saves the modified page
    setup}
  @begin{short}
    Runs a page setup dialog, letting the user modify the values from
    @arg{page-setup}.
  @end{short}

  In contrast to the @fun{gtk-print-run-page-setup-dialog} function, this
  function returns after showing the page setup dialog on platforms that support
  this, and calls @arg{done-cb} from a signal handler for the \"response\"
  signal of the dialog.
  @see-class{gtk-print-operation}"
  (with-stable-pointer (ptr done-cb)
    (%gtk-print-run-page-setup-dialog-async
                                          parent
                                          page-setup
                                          settings
                                          (callback gtk-page-setup-done-func-cb)
                                          ptr)))

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_end_preview ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_preview_end_preview"
           gtk-print-operation-preview-end-preview) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-8}
  @argument[preview]{a @class{gtk-print-operation-preview} object}
  @short{Ends a preview.}

  This function must be called to finish a custom print preview.
  @see-class{gtk-print-operation}
  @see-class{gtk-print-operation-preview}"
  (preview (g-object gtk-print-operation-preview)))

(export 'gtk-print-operation-preview-end-preview)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_is_selected ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_operation_preview_is_selected"
           gtk-print-operation-preview-is-selected) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[preview]{a @class{gtk-print-operation-preview} object}
  @argument[page-nr]{a page number}
  @return{@em{True} if the page has been selected for printing.}
  @begin{short}
    Returns whether the given page is included in the set of pages that have
    been selected for printing.
  @end{short}
  @see-class{gtk-print-operation}
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
 "@version{2020-4-9}
  @argument[preview]{a @class{gtk-print-operation-preview} object}
  @argument[page-nr]{the number of the page to render}
  @begin{short}
    Renders a page to the preview, using the print context that was passed to
    the \"preview\" handler together with preview.
  @end{short}

  A custom print preview should use this function in its \"expose\" handler to
  render the currently selected page.

  Note that this function requires a suitable Cairo context to be associated
  with the print context.
  @see-class{gtk-print-operation}
  @see-class{gtk-print-operation-preview-render-page}"
  (preview (g-object gtk-print-operation-preview))
  (page-nr :int))

(export 'gtk-print-operation-preview-render-page)

;;; --- End of file gtk.print-operation.lisp -----------------------------------
