;;; ----------------------------------------------------------------------------
;;; gtk.printer.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2019 Dieter Kaiser
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
;;; Synopsis
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
    "icon-name" "gchar" t nil)
   (is-virtual
    gtk-printer-is-virtual
    "is-virtual" "gboolean" t t)
   (job-count
    gtk-printer-job-count
    "job-count" "gint" t nil)
   (location
    gtk-printer-location
    "location" "gchar" t nil)
   (name
    gtk-printer-name
    "name" "gchar" t t)
   (paused
    gtk-printer-paused
    "paused" "gboolean" t nil)
   (state-message
    gtk-printer-message
    "state-message" "gchar" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-printer 'type)
 "@version{2013-10-18}
  @begin{short}
    A @sym{gtk-printer} object represents a printer. You only need to deal
    directly with printers if you use the non-portable
    @class{gtk-print-unix-dialog} API.
  @end{short}

  A @sym{gtk-printer} allows to get status information about the printer, such
  as its description, its location, the number of queued jobs, etc. Most
  importantly, a @sym{gtk-printer} object can be used to create a
  @class{gtk-print-job} object, which lets you print to the printer.

  Printing support was added in GTK+ 2.10.
  @begin[Signal Details]{dictionary}
    @subheading{The \"details-acquired\" signal}
      @begin{pre}
 lambda (printer success)   : Run Last
      @end{pre}
      Gets emitted in response to a request for detailed information about a
      printer from the print backend. The success parameter indicates if the
      information was actually obtained.
      @begin[code]{table}
        @entry[printer]{The @sym{gtk-printer} on which the signal is emitted.}
        @entry[success]{@em{True} if the details were successfully acquired.}
      @end{table}
      Since 2.10
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
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accepting-jobs" 'gtk-printer) t)
 "The @code{\"accepting-jobs\"} property of type @code{:boolean} (Read) @br{}
  This property is @em{true} if the printer is accepting jobs. @br{}
  Default value: @em{true} @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accepts-pdf" 'gtk-printer) t)
 "The @code{\"accepts-pdf\"} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  @em{True} if this printer can accept PDF. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accepts-ps" 'gtk-printer) t)
 "The @code{\"accepts-ps\"} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  @em{True} if this printer can accept PostScript. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "backend" 'gtk-printer) t)
 "The @code{\"backend\"} property of type @class{gtk-print-backend}
  (Read / Write / Construct Only) @br{}
  Backend for the printer.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-printer) t)
 "The @code{\"icon-name\"} property of type @code{:string} (Read) @br{}
  The icon name to use for the printer. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-virtual" 'gtk-printer) t)
 "The @code{\"is-virtual\"} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  @code{Nil} if this represents a real hardware printer. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "job-count" 'gtk-printer) t)
 "The @code{\"job-count\"} property of type @code{:int} (Read) @br{}
  Number of jobs queued in the printer. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "location" 'gtk-printer) t)
 "The @code{\"location\"} property of type @code{:string} (Read) @br{}
  The location of the printer. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'gtk-printer) t)
 "The @code{\"name\"} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  Name of the printer. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "paused" 'gtk-printer) t)
 "The @code{\"paused\"} property of type @code{:boolean} (Read) @br{}
  This property is @em{true} if this printer is paused. A paused printer still
  accepts jobs, but it does not print them. @br{}
  Default value: @code{nil} @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state-message" 'gtk-printer) t)
 "The @code{\"state-message\"} property of type @code{:string} (Read) @br{}
  String giving the current state of the printer. @br{}
  Default value: \"\"")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-accepting-jobs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-accepting-jobs 'function)
 "@version{2013-10-19}
  Accessor of the slot @code{\"accepting-jobs\"} of the @class{gtk-printer}
  class.
  @see-class{gtk-printer}
  @see-function{gtk-printer-is-accepting-jobs}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-accepts-pdf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-accepts-pdf 'function)
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if printer accepts PDF.}
  @begin{short}
    Accessor of the slot @code{\"accepts-pdf\"} of the @class{gtk-printer}
    class.
  @end{short}
  Returns whether the printer accepts input in PDF format.

  Since 2.10
  @see-class{gtk-printer}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-accepts-ps atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-accepts-ps 'function)
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if printer accepts PostScript.}
  @begin{short}
    Accessor of the slot @code{\"accepts-ps\"} of the @class{gtk-printer}
    class.
  @end{short}
  Returns whether the printer accepts input in PostScript format.

  Since 2.10
  @see-class{gtk-printer}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-backend atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-backend 'function)
 "@version{2013-10-19}
  Accessor of the slot @code{\"backend\"} of the @class{gtk-printer} class.
  @see-class{gtk-printer}
  @see-function{gtk-printer-get-backend}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-icon-name 'function)
 "@version{2013-10-19}
  Accessor of the slot @code{\"icon-name\"} of the @class{gtk-printer} class.
  @see-class{gtk-printer}
  @see-function{gtk-printer-get-icon-name}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-is-virtual atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-is-virtual 'function)
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if @arg{printer} is virtual.}
  @begin{short}
    Accessor of the slot @code{\"is-virtual\"} of the @class{gtk-printer} class.
  @end{short}
  Returns whether the printer is virtual, i. e. does not represent actual
  printer hardware, but something like a CUPS class.

  Since 2.10
  @see-class{gtk-printer}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-job-count atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-job-count 'function)
 "@version{2013-10-19}
  Accessor of the slot @code{\"job-count\"} of the @class{gtk-printer} class.
  @see-class{gtk-printer}
  @see-function{gtk-printer-get-job-count}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-location atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-location 'function)
 "@version{2013-10-19}
  Accessor of the slot @code{\"location\"} of the @class{gtk-printer} class.
  @see-class{gtk-printer}
  @see-function{gtk-printer-get-location}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-name 'function)
 "@version{2013-10-19}
  Accessor of the slot @code{\"name\"} of the @class{gtk-printer} class.
  @see-class{gtk-printer}
  @see-function{gtk-printer-get-name}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-paused atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-paused 'function)
 "@version{2013-10-19}
  Accessor of the slot @code{\"paused\"} of the @class{gtk-printer} class.
  @see-class{gtk-printer}
  @see-function{gtk-printer-is-paused}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-printer-state-message atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-printer-state-message 'function)
 "@version{2013-10-19}
  Accessor of the slot @code{\"state-message\"} of the @class{gtk-printer}
  class.
  @see-class{gtk-printer}
  @see-function{gtk-printer-get-state-message}")

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
 "@version{2013-10-18}
")

;;; ----------------------------------------------------------------------------
;;; gtk_printer_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-printer-new))

(defun gtk-printer-new (name backend virtual)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[name]{the name of the printer}
  @argument[backend]{a @class{gtk-print-backend} object}
  @argument[virtual]{whether the printer is virtual}
  @return{A new @class{gtk-printer} object.}
  @short{Creates a new @class{gtk-printer}.}

  Since 2.10
  @see-class{gtk-printer}
  @see-class{gtk-print-backend}"
  (make-instance 'gtk-printer
                 :name name
                 :backend backend
                 :is-virtual virtual))

(export 'gtk-printer-new)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_backend ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-printer-get-backend))

(defun gtk-printer-get-backend (printer)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[[printer]{a @class{gtk-printer} object}
  @return{The backend of printer.}
  @short{Returns the backend of the printer.}

  Since 2.10
  @see-class{gtk-printer}"
  (gtk-printer-backend printer))

(export 'gtk-printer-get-backend)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-printer-get-name))

(defun gtk-printer-get-name (printer)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{The name of @arg{printer}.}
  @short{Returns the name of the printer.}

  Since 2.10
  @see-class{gtk-printer}"
  (gtk-printer-name printer))

(export 'gtk-printer-get-name)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_state_message ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-printer-get-state-message))

(defun gtk-printer-get-state-message (printer)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{The state message of @arg{printer}.}
  @begin{short}
    Returns the state message describing the current state of the printer.
  @end{short}

  Since 2.10
  @see-class{gtk-printer}"
  (gtk-printer-state-message printer))

(export 'gtk-printer-get-state-message)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_description ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_get_description" gtk-printer-get-description) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{The description of @arg{printer}.}
  @short{Gets the description of the printer.}

  Since 2.10
  @see-class{gtk-printer}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-get-description)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_location ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-printer-get-location))

(defun gtk-printer-get-location (printer)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{The location of @arg{printer}.}
  @short{Returns a description of the location of the printer.}

  Since 2.10
  @see-class{gtk-printer}"
  (gtk-printer-location printer))

(export 'gtk-printer-get-location)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-printer-get-icon-name))

(defun gtk-printer-get-icon-name (printer)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{The icon name for @arg{printer}.}
  @short{Gets the name of the icon to use for the printer.}

  Since 2.10
  @see-class{gtk-printer}"
  (gtk-printer-icon-name printer))

(export 'gtk-printer-get-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_job_count ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-printer-get-job-count))

(defun gtk-printer-get-job-count (printer)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{The number of jobs on @arg{printer}.}
  @short{Gets the number of jobs currently queued on the printer.}

  Since 2.10
  @see-class{gtk-printer}"
  (gtk-printer-job-count printer))

(export 'gtk-printer-get-job-count)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_is_active ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_is_active" gtk-printer-is-active) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if @arg{printer} is active.}
  @begin{short}
    Returns whether the printer is currently active, i. e. accepts new jobs.
  @end{short}

  Since 2.10
  @see-class{gtk-printer}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-is-active)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_is_paused ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_is_paused" gtk-printer-is-paused) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if @arg{printer} is paused.}
  @begin{short}
    Returns whether the printer is currently paused.
  @end{short}
  A paused printer still accepts jobs, but it is not printing them.

  Since 2.14
  @see-class{gtk-printer}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-is-paused)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_is_accepting_jobs ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_is_accepting_jobs" gtk-printer-is-accepting-jobs)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if printer is accepting jobs.}
  @short{Returns whether the printer is accepting jobs.}

  Since 2.14
  @see-class{gtk-printer}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-is-accepting-jobs)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_is_virtual ()
;;; ----------------------------------------------------------------------------

;;; Implemented as the Accesor of the slot "is-virtual"

;;; ----------------------------------------------------------------------------
;;; gtk_printer_is_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_is_default" gtk-printer-is-default) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if @arg{printer} is the default.}
  @begin{short}
    Returns whether the printer is the default printer.
  @end{short}

  Since 2.10
  @see-class{gtk-printer}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-is-default)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_accepts_ps ()
;;; ----------------------------------------------------------------------------

;;; Implemented as the Accessor of the slot "accepts-ps"

;;; ----------------------------------------------------------------------------
;;; gtk_printer_accepts_pdf ()
;;; ----------------------------------------------------------------------------

;;; Implemented as the Accessor of the slot "accepts-pdf"

;;; ----------------------------------------------------------------------------
;;; gtk_printer_list_papers ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_list_papers" gtk-printer-list-papers)
    (g-list (g-object gtk-page-setup))
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @begin{return}
    A newly allocated list of newly allocated @class{gtk-page-setup}s.
  @end{return}
  @short{Lists all the paper sizes printer supports.}
  This will return an empty list unless the printer's details are available,
  see the functions @fun{gtk-printer-has-details} and
  @fun{gtk-printer-request-details}.

  Since 2.12
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
 "@version{2013-10-19}
  @argument[a]{a @class{gtk-printer} object}
  @argument[b]{another @class{gtk-printer} object}
  @begin{return}
    0 if the printer match, a negative value if @arg{a} < @arg{b}, or a
    positive value if @arg{a} > @arg{b}.
  @end{return}
  @short{Compares two printers.}

  Since 2.10
  @see-class{gtk-printer}"
  (a (g-object gtk-printer))
  (b (g-object gtk-printer)))

(export 'gtk-printer-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_has_details ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_has_details" gtk-printer-has-details) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{@em{True} if @arg{printer} details are available.}
  @begin{short}
    Returns whether the printer details are available.
  @end{short}

  Since 2.12
  @see-class{gtk-printer}
  @see-function{gtk-printer-request-details}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-has-details)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_request_details ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_request_details" gtk-printer-request-details) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @begin{short}
    Requests the printer details.
  @end{short}
  When the details are available, the \"details-acquired\" signal will be
  emitted on printer.

  Since 2.12
  @see-class{gtk-printer}
  @see-function{gtk-printer-has-details}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-request-details)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_capabilities ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_get_capabilities" gtk-printer-get-capabilities)
    gtk-print-capabilities
 #+cl-cffi-gtk-documentation
 "@version{2013-10-19}
  @argument[printer]{a @class{gtk-printer} object}
  @return{The printer's capabilities.}
  @begin{short}
    Returns the printer's capabilities.
  @end{short}

  This is useful when you are using @class{gtk-print-unix-dialog}'s
  @code{\"manual-capabilities\"} setting and need to know which settings the
  printer can handle and which you must handle yourself.

  This will return 0 unless the printer's details are available, see the
  functions @fun{gtk-printer-has-details} and @fun{gtk-printer-request-details}.

  Since 2.12
  @see-class{gtk-printer}
  @see-symbol{gtk-print-capabilities}
  @see-class{gtk-print-unix-dialog}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-get-capabilities)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_default_page_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_get_default_page_size" gtk-printer-get-default-page-size)
    (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[printer]{a @class{gtk-printer} object}
  @return{A newly allocated @class{gtk-page-setup} object with default page size
    of the printer.}
  @short{Returns default page size of printer.}

  Since 2.14
  @see-class{gtk-printer}
  @see-class{gtk-page-setup}"
  (printer (g-object gtk-printer)))

(export 'gtk-printer-get-default-page-size)

;;; ----------------------------------------------------------------------------
;;; gtk_printer_get_hard_margins ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_printer_get_hard_margins" %gtk-printer-get-hard-margins) :boolean
  (printer (g-object gtk-printer))
  (top (:pointer :double))
  (bottom (:pointer :double))
  (left (:pointer :double))
  (right (:pointer :double)))

(defun gtk-printer-get-hard-margins (printer)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-20}
  @argument[printer]{a @class{gtk-printer} object}
  @begin{return}
    @code{top} -- the top margin @br{}
    @code{bottom} -- the bottom margin @br{}
    @code{left} -- the left margin in @br{}
    @code{right} -- the right margin
  @end{return}
  @begin{short}
    Retrieve the hard margins of printer, i. e. the margins that define the area
    at the borders of the paper that the printer cannot print to.
  @end{short}

  Note: This will not succeed unless the printer's details are available, see
  the functions @fun{gtk-printer-has-details} and
  @fun{gtk-printer-request-details}.

  Since 2.20
  @see-class{gtk-printer}
  @see-function{gtk-printer-has-details}
  @see-function{gtk-printer-request-details}"
  (with-foreign-objects ((top :double)
                         (bottom :double)
                         (left :double)
                         (right :double))
    (when (%gtk-printer-get-hard-margins printer top bottom left right)
      (values top bottom left right))))

(export 'gtk-printer-get-hard-margins)

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
 "@version{2013-10-20}
  @argument[func]{a function to call for each printer}
  @argument[wait]{if @em{true}, wait in a recursive mainloop until all printers
    are enumerated; otherwise return early}
  @begin{short}
    Calls a function for all @class{gtk-printer}s.
  @end{short}
  If @arg{func} returns @em{true}, the enumeration is stopped.

  Since 2.10
  @see-class{gtk-printer}"
  (%gtk-enumerate-printers (callback gtk-printer-func-cb)
                           (glib:allocate-stable-pointer func)
                           (callback glib:stable-pointer-destroy-notify-cb)
                           wait))

(export 'gtk-enumerate-printers)

;;; --- End of file gtk.printer.lisp -------------------------------------------
