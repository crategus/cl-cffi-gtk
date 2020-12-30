;;; ----------------------------------------------------------------------------
;;; gtk.printer.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2020 Dieter Kaiser
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
;;; GtkPrinter
;;;
;;;     Represents a printer
;;;
;;; Types and Values
;;;
;;;     GtkPrinter
;;;     GtkPrintBackend
;;;
;;; Functions
;;;
;;;     gtk_printer_new
;;;     gtk_printer_get_backend
;;;     gtk_printer_get_name
;;;     gtk_printer_get_state_message
;;;     gtk_printer_get_description
;;;     gtk_printer_get_location
;;;     gtk_printer_get_icon_name
;;;     gtk_printer_get_job_count
;;;     gtk_printer_is_active
;;;     gtk_printer_is_paused
;;;     gtk_printer_is_accepting_jobs
;;;     gtk_printer_is_virtual
;;;     gtk_printer_is_default
;;;     gtk_printer_accepts_ps
;;;     gtk_printer_accepts_pdf
;;;     gtk_printer_list_papers
;;;     gtk_printer_compare
;;;     gtk_printer_has_details
;;;     gtk_printer_request_details
;;;     gtk_printer_get_capabilities
;;;     gtk_printer_get_default_page_size
;;;     gtk_printer_get_hard_margins
;;;     gtk_enumerate_printers
;;;
;;; Properties
;;;
;;;        gboolean   accepting-jobs      Read
;;;        gboolean   accepts-pdf         Read / Write / Construct Only
;;;        gboolean   accepts-ps          Read / Write / Construct Only
;;; GtkPrintBackend*  backend             Read / Write / Construct Only
;;;           gchar*  icon-name           Read
;;;        gboolean   is-virtual          Read / Write / Construct Only
;;;            gint   job-count           Read
;;;           gchar*  location            Read
;;;           gchar*  name                Read / Write / Construct Only
;;;        gboolean   paused              Read
;;;           gchar*  state-message       Read
;;;
;;; Signals
;;;
;;;            void   details-acquired    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GtkPrintBackend
;;;     ╰── GtkPrinter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPrinter
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPrinter" gtk-printer
  (:superclass g-object
    :export t
    :interfaces nil
    :type-initializer "gtk_printer_get_type")
  ((accepting-jobs
    gtk-printer-accepting-jobs
    "accepting-jobs" "gboolean" t nil)
   (accepts-pdf
    gtk-printer-accepts-pdf
    "accepts-pdf" "gboolean" t t)
   (accepts-ps
    gtk-printer-accepts-ps
    "accepts-ps" "gboolean" t t)
   (backend
    gtk-printer-backend
    "backend" "GtkPrintBackend" t t)
   (icon-name
    gtk-printer-icon-name
    "icon-name" "gchararray" t nil)
   (is-virtual
    gtk-printer-is-virtual
    "is-virtual" "gboolean" t t)
   (job-count
    gtk-printer-job-count
    "job-count" "gint" t nil)
   (location
    gtk-printer-location
    "location" "gchararray" t nil)
   (name
    gtk-printer-name
    "name" "gchararray" t t)
   (paused
    gtk-printer-paused
    "paused" "gboolean" t nil)
   (state-message
    gtk-printer-message
    "state-message" "gchararray" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-printer 'type)
 "@version{2020-4-9}
  @begin{short}
    A @sym{gtk-printer} object represents a printer.
  @end{short}
  You only need to deal directly with printers if you use the non-portable
  @class{gtk-print-unix-dialog} API.

  A @sym{gtk-printer} object allows to get status information about the printer,
  such as its description, its location, the number of queued jobs, etc. Most
  importantly, a @sym{gtk-printer} object can be used to create a
  @class{gtk-print-job} object, which lets you print to the printer.
  @begin[Signal Details]{dictionary}
    @subheading{The \"details-acquired\" signal}
      @begin{pre}
 lambda (printer success)    : Run Last
      @end{pre}
      Gets emitted in response to a request for detailed information about a
      printer from the print backend. The success parameter indicates if the
      information was actually obtained.
      @begin[code]{table}
        @entry[printer]{The @sym{gtk-printer} object on which the signal is
          emitted.}
        @entry[success]{@em{True} if the details were successfully acquired.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-printer-accepting-jobs}
  @see-slot{gtk-printer-accepts-pdf}
  @see-slot{gtk-printer-accepts-ps}
  @see-slot{gtk-printer-backend}
  @see-slot{gtk-printer-icon-name}
  @see-slot{gtk-printer-is-virtual}
  @see-slot{gtk-printer-job-count}
  @see-slot{gtk-printer-location}
  @see-slot{gtk-printer-name}
  @see-slot{gtk-printer-paused}
  @see-slot{gtk-printer-state-message}
  @see-class{gtk-print-unix-dialog}
  @see-class{gtk-print-job}
  @see-class{gtk-print-backend}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-printer-accepting-jobs ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accepting-jobs" 'gtk-printer) t)
 "The @code{accepting-jobs} property of type @code{:boolean} (Read) @br{}
  This property is @em{true} if the printer is accepting jobs. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-accepting-jobs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-accepting-jobs 'function)
 "@version{2019-4-9}
  @syntax[]{(gtk-printer-accepting-jobs object) => accepting-jobs)}
  @argument[object]{a @class{gtk-printer} object}
  @argument[accepting-jobs]{a @code{:boolean} wether the printer is accepting
    jobs.}
  @begin{short}
    Accessor of the @slot[gtk-printer]{accepting-jobs} slot of the
    @class{gtk-printer} class.
  @end{short}
  @see-class{gtk-printer}
  @see-function{gtk-printer-is-accepting-jobs}")

;;; --- gtk-printer-accepts-pdf ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accepts-pdf" 'gtk-printer) t)
 "The @code{accepts-pdf} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  @em{True} if this printer can accept PDF. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-accepts-pdf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-accepts-pdf 'function)
 "@version{2020-4-9}
  @syntax[]{(gtk-printer-accepts-pdf object) => accepts-pdf)}
  @argument[object]{a @class{gtk-printer} object}
  @argument[accepts-pdf]{a boolean wether the printer can accept PDF}
  @begin{short}
    Accessor of the @slot[gtk-printer]{accepts-pdf} slot of the
    @class{gtk-printer} class.
  @end{short}
  Returns whether the printer accepts input in PDF format.
  @see-class{gtk-printer}")

;;; --- gtk-printer-accepts-ps -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accepts-ps" 'gtk-printer) t)
 "The @code{accepts-ps} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  @em{True} if this printer can accept PostScript. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-accepts-ps atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-accepts-ps 'function)
 "@version{2020-4-9}
  @syntax[]{(gtk-printer-accepts-ps object) => accepts-ps)}
  @argument[object]{a @class{gtk-printer} object}
  @argument[accepts-ps]{a boolean wether the printer can accept PostScript}
  @begin{short}
    Accessor of the @slot[gtk-printer]{accepts-ps} slot of the
    @class{gtk-printer} class.
  @end{short}
  Returns whether the printer accepts input in PostScript format.
  @see-class{gtk-printer}")

;;; --- gtk-printer-backend ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "backend" 'gtk-printer) t)
 "The @code{backend} property of type @class{gtk-print-backend}
  (Read / Write / Construct Only) @br{}
  Backend for the printer.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-backend atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-backend 'function)
 "@version{2020-4-9}
  @syntax[]{(gtk-printer-backend object) => backend}
  @argument[object]{a @class{gtk-printer} object}
  @argument[backend]{a @class{gtk-print-backend} object}
  @return{The backend of printer.}
  @begin{short}
    Accessor of the @slot[gtk-printer]{backend} slot of the
    @class{gtk-printer} class.
  @end{short}
  Returns the backend of the printer.
  @see-class{gtk-printer}")

;;; --- gtk-printer-icon-name --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-printer) t)
 "The @code{icon-name} property of type @code{g-string} (Read) @br{}
  The icon name to use for the printer. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-icon-name 'function)
 "@version{2020-4-9}
  @syntax[]{(gtk-printer-icon-name object) => icon-name}
  @argument[object]{a @class{gtk-printer} object}
  @argument[icon-name]{a string with the icon name}
  @begin{short}
    Accessor of the @slot[gtk-printer]{icon-name} slot of the
    @class{gtk-printer} class.
  @end{short}
  Gets the name of the icon to use for the printer.
  @see-class{gtk-printer}")

;;; --- gtk-printer-is-virtual -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-virtual" 'gtk-printer) t)
 "The @code{is-virtual} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  @em{False} if this represents a real hardware printer. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-is-virtual atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-is-virtual 'function)
 "@version{2020-4-9}
  @syntax[]{(gtk-printer-is-virtual object) => is-virtual}
  @argument[object]{a @class{gtk-printer} object}
  @argument[is-virtual]{a boolean wether the printer is real hardware printer}
  @begin{short}
    Accessor of the @slot[gtk-printer]{is-virtual} slot of the
    @class{gtk-printer} class.
  @end{short}
  Returns whether the printer is virtual, i.e. does not represent actual
  printer hardware, but something like a CUPS class.
  @see-class{gtk-printer}")

;;; --- gtk-printer-job-count --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "job-count" 'gtk-printer) t)
 "The @code{job-count} property of type @code{:int} (Read) @br{}
  Number of jobs queued in the printer. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-job-count atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-job-count 'function)
 "@version{2020-4-9}
  @syntax[]{(gtk-printer-job-count object) => job-count}
  @argument[object]{a @class{gtk-printer} object}
  @argument[job-count]{the number of jobs queued on the printer}
  @begin{short}
    Accessor of the @slot[gtk-printer]{job-count} slot of the
    @class{gtk-printer} class.
  @end{short}
  Gets the number of jobs currently queued on the printer.
  @see-class{gtk-printer}")

;;; --- gtk-printer-location ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "location" 'gtk-printer) t)
 "The @code{location} property of type @code{g-string} (Read) @br{}
  The location of the printer. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-location atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-location 'function)
 "@version{2020-4-9}
  @syntax{]{(gtk-printer-location object) => location}
  @argument[object]{a @class{gtk-printer} object}
  @argument[location]{a string with the location of the printer}
  @begin{short}
    Accessor of the @slot[gtk-printer]{location} slot of the
    @class{gtk-printer} class.
  @end{short}
  Returns a description of the location of the printer.
  @see-class{gtk-printer}")

;;; --- gtk-printer-name -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'gtk-printer) t)
 "The @code{name} property of type @code{g-string}
  (Read / Write / Construct) @br{}
  Name of the printer. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-name 'function)
 "@version{2020-4-9}
  @syntax{]{(gtk-printer-name object) => name}
  @argument[object]{a @class{gtk-printer} object}
  @argument[name]{a string with the name of the printer}
  @begin{short}
    Accessor of the @slot[gtk-printer]{name} slot of the
    @class{gtk-printer} class.
  @end{short}
  Returns the name of the printer.
  @see-class{gtk-printer}
  @see-function{gtk-printer-get-name}")

;;; --- gtk-printer-paused -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "paused" 'gtk-printer) t)
 "The @code{paused} property of type @code{:boolean} (Read) @br{}
  This property is @em{true} if this printer is paused. A paused printer still
  accepts jobs, but it does not print them. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-paused atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-paused 'function)
 "@version{2020-4-9}
  @syntax[]{(gtk-printer-paused object) => paused}
  @argument[object]{a @class{gtk-printer} object}
  @argument[paused]{a boolean wether the printer is paused}
  @begin{short}
    Accessor of the @slot[gtk-printer]{paused} slot of the
    @class{gtk-printer} class.
  @end{short}
  @see-class{gtk-printer}
  @see-function{gtk-printer-is-paused}")

;;; --- gtk-printer-state-message ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state-message" 'gtk-printer) t)
 "The @code{state-message} property of type @code{g-string} (Read) @br{}
  String giving the current state of the printer. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-state-message atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-state-message 'function)
 "@version{2020-4-9}
  @syntax[]{(gtk-printer-state-message object) => state-message}
  @argument[object]{a @class{gtk-printer} object}
  @argument[state-message]{a string with the current state of the printer}
  @begin{short}
    Accessor of the @slot[gtk-printer]{state-message} slot of the
    @class{gtk-printer} class.
  @end{short}
  Returns the state message describing the current state of the printer.
  @see-class{gtk-printer}")

;;; ----------------------------------------------------------------------------
;;; GtkPrintBackend
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPrintBackend" gtk-print-backend
  (:superclass g-object
    :export t
    :interfaces nil
    :type-initializer "gtk_print_backend_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-print-backend 'type)
 "@version{2020-4-9}
  @short{No documentation.}
  @see-class{gtk-printer}")

;;; ----------------------------------------------------------------------------
;;; gtk_printer_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-printer-new))

(defun gtk-printer-new (name backend virtual)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[name]{a string with the name of the printer}
  @argument[backend]{a @class{gtk-print-backend} object}
  @argument[virtual]{a boolean whether the printer is virtual}
  @return{A new @class{gtk-printer} object.}
  @short{Creates a new @class{gtk-printer} object.}
  @see-class{gtk-printer}
  @see-class{gtk-print-backend}"
  (make-instance 'gtk-printer
                 :name name
                 :backend backend
                 :is-virtual virtual))

(export 'gtk-printer-new)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_description ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_get_description" gtk-printer-description) g-string
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[printer]{a @class{gtk-printer} object}
  @return{A string with the description of @arg{printer}.}
  @short{Gets the description of the printer.}
  @see-class{gtk-printer}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-description)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_is_active ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_is_active" gtk-printer-is-active) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if @arg{printer} is active.}
  @begin{short}
    Returns whether the printer is currently active, i.e. accepts new jobs.
  @end{short}
  @see-class{gtk-printer}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-is-active)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_is_paused ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_is_paused" gtk-printer-is-paused) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if @arg{printer} is paused.}
  @begin{short}
    Returns whether the printer is currently paused.
  @end{short}
  A paused printer still accepts jobs, but it is not printing them.
  @see-class{gtk-printer}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-is-paused)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_is_accepting_jobs ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_is_accepting_jobs" gtk-printer-is-accepting-jobs)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if @arg{printer} is accepting jobs.}
  @short{Returns whether the printer is accepting jobs.}
  @see-class{gtk-printer}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-is-accepting-jobs)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_is_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_is_default" gtk-printer-is-default) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if @arg{printer} is the default.}
  @begin{short}
    Returns whether the printer is the default printer.
  @end{short}
  @see-class{gtk-printer}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-is-default)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_accepts_ps ()
;;; ----------------------------------------------------------------------------

;;; Implemented as the Accessor of the "accepts-ps" slot

;;; ----------------------------------------------------------------------------
;;; gtk_printer_accepts_pdf ()
;;; ----------------------------------------------------------------------------

;;; Implemented as the Accessor of the "accepts-pdf" slot

;;; ----------------------------------------------------------------------------
;;; gtk_printer_list_papers ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_list_papers" gtk-printer-list-papers)
    (g-list (g-object gtk-page-setup))
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[printer]{a @class{gtk-printer} object}
  @return{A list of @class{gtk-page-setup} objects.}
  @short{Lists all the paper sizes the printer supports.}
  This will return an empty list unless the printer's details are available,
  see the functions @fun{gtk-printer-has-details} and
  @fun{gtk-printer-request-details}.
  @see-class{gtk-printer}
  @see-function{gtk-printer-has-details}
  @see-function{gtk-printer-request-details}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-list-papers)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_compare ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_compare" gtk-printer-compare) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[a]{a @class{gtk-printer} object}
  @argument[b]{another @class{gtk-printer} object}
  @begin{return}
    0 if the printer match, a negative value if @arg{a} < @arg{b}, or a
    positive value if @arg{a} > @arg{b}.
  @end{return}
  @short{Compares two printers.}
  @see-class{gtk-printer}"
  (a (g-object gtk-printer))
  (b (g-object gtk-printer)))

(export 'gtk-printer-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_has_details ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_has_details" gtk-printer-has-details) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if @arg{printer} details are available.}
  @begin{short}
    Returns whether the printer details are available.
  @end{short}
  @see-class{gtk-printer}
  @see-function{gtk-printer-request-details}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-has-details)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_request_details ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_request_details" gtk-printer-request-details) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[printer]{a @class{gtk-printer} object}
  @begin{short}
    Requests the printer details.
  @end{short}
  When the details are available, the \"details-acquired\" signal will be
  emitted on printer.
  @see-class{gtk-printer}
  @see-function{gtk-printer-has-details}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-request-details)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_capabilities ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_get_capabilities" gtk-printer-capabilities)
    gtk-print-capabilities
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[printer]{a @class{gtk-printer} object}
  @return{The printer's capabilities of type @symbol{gtk-print-capabilities}.}
  @begin{short}
    Returns the printer's capabilities.
  @end{short}

  This is useful when you are using the
  @slot[gtk-print-unix-dialog]{manual-capabilities} setting of
  @class{gtk-print-unix-dialog} and need to know which settings the printer can
  handle and which you must handle yourself.

  This will return 0 unless the printer's details are available, see the
  functions @fun{gtk-printer-has-details} and @fun{gtk-printer-request-details}.
  @see-class{gtk-printer}
  @see-symbol{gtk-print-capabilities}
  @see-class{gtk-print-unix-dialog}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-capabilities)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_default_page_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_get_default_page_size" gtk-printer-default-page-size)
    (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[printer]{a @class{gtk-printer} object}
  @return{A @class{gtk-page-setup} object with the default page size of the
    printer.}
  @short{Returns the default page size of the printer.}
  @see-class{gtk-printer}
  @see-class{gtk-page-setup}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-default-page-size)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_hard_margins ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_get_hard_margins" %gtk-printer-hard-margins) :boolean
  (printer (g-object gtk-printer))
  (top (:pointer :double))
  (bottom (:pointer :double))
  (left (:pointer :double))
  (right (:pointer :double)))

(defun gtk-printer-hard-margins (printer)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[printer]{a @class{gtk-printer} object}
  @begin{return}
    @code{top} -- a @code{:double} with the top margin @br{}
    @code{bottom} -- a @code{:double} with the bottom margin @br{}
    @code{left} -- a @code{:double} with the left margin in @br{}
    @code{right} -- a @code{:double} with the right margin
  @end{return}
  @begin{short}
    Retrieve the hard margins of the printer, i.e. the margins that define
    the area at the borders of the paper that the printer cannot print to.
  @end{short}
  @begin[Note]{dictionary}
    This will not succeed unless the printer's details are available,
    see the functions @fun{gtk-printer-has-details} and
    @fun{gtk-printer-request-details}.
  @end{dictionary}
  @see-class{gtk-printer}
  @see-function{gtk-printer-has-details}
  @see-function{gtk-printer-request-details}"
  (with-foreign-objects ((top :double)
                         (bottom :double)
                         (left :double)
                         (right :double))
    (when (%gtk-printer-hard-margins printer top bottom left right)
      (values (mem-ref top :double)
              (mem-ref bottom :double)
              (mem-ref left :double)
              (mem-ref right :double)))))

(export 'gtk-printer-hard-margins)

;;; ----------------------------------------------------------------------------
;;; GtkPrinterFunc ()
;;;
;;; gboolean (*GtkPrinterFunc) (GtkPrinter *printer, gpointer data);
;;;
;;; The type of function passed to gtk_enumerate_printers(). Note that you need
;;; to ref printer, if you want to keep a reference to it after the function has
;;; returned.
;;;
;;; printer :
;;;     a GtkPrinter
;;;
;;; data :
;;;     user data passed to gtk_enumerate_printers(). [closure]
;;;
;;; Returns :
;;;     TRUE to stop the enumeration, FALSE to continue
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcallback gtk-printer-func-cb :boolean
    ((printer (g-object gtk-printer))
     (data :pointer))
  (funcall (glib::get-stable-pointer-value data) printer))

;;; ----------------------------------------------------------------------------
;;; gtk_enumerate_printers ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_enumerate_printers" %gtk-enumerate-printers) :void
  (func :pointer)
  (data :pointer)
  (destroy :pointer)
  (wait :boolean))

(defun gtk-enumerate-printers (func wait)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[func]{a function to call for each printer}
  @argument[wait]{if @em{true}, wait in a recursive mainloop until all printers
    are enumerated; otherwise return early}
  @begin{short}
    Calls a function for all printers.
  @end{short}
  If @arg{func} returns @em{true}, the enumeration is stopped.
  @see-class{gtk-printer}"
  (%gtk-enumerate-printers (callback gtk-printer-func-cb)
                           (glib:allocate-stable-pointer func)
                           (callback glib:stable-pointer-destroy-notify-cb)
                           wait))

(export 'gtk-enumerate-printers)

;;; --- End of file gtk.printer.lisp -------------------------------------------
