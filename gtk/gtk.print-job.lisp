;;; ----------------------------------------------------------------------------
;;; gtk.print-job.lisp
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
;;; GtkPrintJob
;;;
;;;     Represents a print job
;;;
;;; Synopsis
;;;
;;;     GtkPrintJob
;;;
;;;     gtk_print_job_new
;;;     gtk_print_job_get_settings
;;;     gtk_print_job_get_printer
;;;     gtk_print_job_get_title
;;;     gtk_print_job_get_status
;;;     gtk_print_job_set_source_fd
;;;     gtk_print_job_set_source_file
;;;     gtk_print_job_get_surface
;;;     gtk_print_job_send
;;;     gtk_print_job_set_track_print_status
;;;     gtk_print_job_get_track_print_status
;;;     gtk_print_job_get_pages
;;;     gtk_print_job_set_pages
;;;     gtk_print_job_get_page_ranges
;;;     gtk_print_job_set_page_ranges
;;;     gtk_print_job_get_page_set
;;;     gtk_print_job_set_page_set
;;;     gtk_print_job_get_num_copies
;;;     gtk_print_job_set_num_copies
;;;     gtk_print_job_get_scale
;;;     gtk_print_job_set_scale
;;;     gtk_print_job_get_n_up
;;;     gtk_print_job_set_n_up
;;;     gtk_print_job_get_n_up_layout
;;;     gtk_print_job_set_n_up_layout
;;;     gtk_print_job_get_rotate
;;;     gtk_print_job_set_rotate
;;;     gtk_print_job_get_collate
;;;     gtk_print_job_set_collate
;;;     gtk_print_job_get_reverse
;;;     gtk_print_job_set_reverse
;;;
;;; Properties
;;;
;;;     GtkPageSetup*  page-setup            Read / Write / Construct Only
;;;       GtkPrinter*  printer               Read / Write / Construct Only
;;; GtkPrintSettings*  settings              Read / Write / Construct Only
;;;            gchar*  title                 Read / Write / Construct Only
;;;         gboolean   track-print-status    Read / Write
;;;
;;; Signals
;;;
;;;     void   status-changed    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkPrintJob
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPrintJob
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPrintJob" gtk-print-job
  (:superclass g-object
    :export t
    :interfaces nil
    :type-initializer "gtk_print_job_get_type")
  ((page-setup
    gtk-print-job-page-setup
    "page-setup" "GtkPageSetup" t t)
   (printer
    gtk-print-job-printer
    "printer" "GtkPrinter" t t)
   (settings
    gtk-print-job-settings
    "settings" "GtkPrintSettings" t t)
   (title
    gtk-print-job-title
    "title" "gchararray" t t)
   (track-print-status
    gtk-print-job-track-print-status
    "track-print-status" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-print-job 'type)
 "@version{2013-10-21}
  @begin{short}
    A @sym{gtk-print-job} object represents a job that is sent to a printer. You
    only need to deal directly with print jobs if you use the non-portable
    @class{gtk-print-unix-dialog} API.
  @end{short}

  Use the function @fun{gtk-print-job-get-surface} to obtain the cairo surface
  onto which the pages must be drawn. Use the function @fun{gtk-print-job-send}
  to send the finished job to the printer. If you do not use cairo
  @sym{gtk-print-job} also supports printing of manually generated postscript,
  via the function @fun{gtk-print-job-set-source-file}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"status-changed\" signal}
      @begin{pre}
 lambda (job)   : Run Last
      @end{pre}
      Gets emitted when the status of a job changes. The signal handler can use
      the function @fun{gtk-print-job-get-status} to obtain the new status.
      @begin[code]{table}
        @entry[job]{The @sym{gtk-print-job} object on which the signal was
        emitted.}
      @end{table}
  @end{dictionary}
  Since 2.10
  @see-slot{gtk-print-job-page-setup}
  @see-slot{gtk-print-job-printer}
  @see-slot{gtk-print-job-settings}
  @see-slot{gtk-print-job-title}
  @see-slot{gtk-print-job-track-print-status}
  @see-class{gtk-print-unix-dialog}
  @see-function{gtk-print-job-get-surface}
  @see-function{gtk-print-job-send}
  @see-function{gtk-print-job-set-source-file}
  @see-function{gtk-print-job-get-status}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page-setup" 'gtk-print-job) t)
 "The @code{\"page-setup\"} property of type @class{gtk-page-setup}
  (Read / Write / Construct Only) @br{}
  Page Setup.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "printer" 'gtk-print-job) t)
 "The @code{\"printer\"} property of type @class{gtk-printer}
  (Read / Write / Construct Only) @br{}
  Printer to print the job to.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "settings" 'gtk-print-job) t)
 "The @code{\"settings\"} property of type @class{gtk-print-settings}
  (Read / Write / Construct Only) @br{}
  Printer settings.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title" 'gtk-print-job) t)
 "The @code{\"title\"} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  Title of the print job. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "track-print-status"
                                               'gtk-print-job) t)
 "The @code{\"track-print-status\"} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the print job will continue to emit \"status-changed\" signals
  after the print data has been sent to the printer or print server. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-job-page-setup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-job-page-setup 'function)
 "@version{2013-10-21}
  Accessor of the slot @code{\"page-setup\"} of the @class{gtk-print-job}
  class.
  @see-class{gtk-print-job}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-job-printer atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-job-printer 'function)
 "@version{2013-10-21}
  Accessor of the slot @code{\"printer\"} of the @class{gtk-print-job}
  class.
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-printer}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-job-settings atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-job-settings 'function)
 "@version{2013-10-21}
  Accessor of the slot @code{\"settings\"} of the @class{gtk-print-job}
  class.
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-settings}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-job-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-job-title 'function)
 "@version{2013-10-21}
  Accessor of the slot @code{\"title\"} of the @class{gtk-print-job}
  class.
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-title}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-job-track-print-status atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-job-track-print-status 'function)
 "@version{2013-10-21}
  Accessor of the slot @code{\"track-print-status\"} of the
  @class{gtk-print-job} class.
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-track-print-status}
  @see-function{gtk-print-job-set-track-print-status}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-job-new))

(defun gtk-print-job-new (title printer settings page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[title]{the job title}
  @argument[printer]{a @class{gtk-printer} oject}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @return{A new @class{gtk-print-job} object}
  @short{Creates a new @class{gtk-print-job}.}

  Since 2.10
  @see-class{gtk-print-job}
  @see-class{gtk-printer}
  @see-class{gtk-print-settings}
  @see-class{gtk-page-setup}"
  (make-instance 'gtk-print-job
                 :title title
                 :printer printer
                 :settings settings
                 :page-setup page-setup))

(export 'gtk-print-job-new)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_settings ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-job-get-settings))

(defun gtk-print-job-get-settings (job)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The settings of @arg{job}.}
  @short{Gets the @class{gtk-print-settings} of the print job.}

  Since 2.10
  @see-class{gtk-print-job}
  @see-class{gtk-print-settings}"
  (gtk-print-job-settings job))

(export 'gtk-print-job-get-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_printer ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-job-get-printer))

(defun gtk-print-job-get-printer (job)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The printer of @arg{job}.}
  @short{Gets the @class{gtk-printer} of the print @arg{job}.}

  Since 2.10
  @see-class{gtk-print-job}
  @see-class{gtk-printer}"
  (gtk-print-job-printer job))

(export 'gtk-print-job-get-printer)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-job-get-title))

(defun gtk-print-job-get-title (job)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The title of @arg{job}.}
  @short{Gets the job title.}

  Since 2.10
  @see-class{gtk-print-job}"
  (gtk-print-job-title job))

(export 'gtk-print-job-get-title)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_status ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_status" gtk-print-job-get-status)
    gtk-print-status
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The status of @arg{job}.}
  @short{Gets the status of the print job.}

  Since 2.10
  @see-class{gtk-print-job}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-get-status)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_source_fd ()
;;;
;;; gboolean
;;; gtk_print_job_set_source_fd (GtkPrintJob *job,
;;;                              int fd,
;;;                              GError **error);
;;;
;;; Make the GtkPrintJob send an existing document to the printing system. The
;;; file can be in any format understood by the platforms printing system
;;; (typically PostScript, but on many platforms PDF may work too). See
;;; gtk_printer_accepts_pdf() and gtk_printer_accepts_ps().
;;;
;;; This is similar to gtk_print_job_set_source_file(), but takes expects an
;;; open file descriptor for the file, instead of a filename.
;;;
;;; job :
;;;     a GtkPrintJob
;;;
;;; fd :
;;;     a file descriptor
;;;
;;; error :
;;;     return location for errors
;;;
;;; Returns :
;;;     FALSE if an error occurred
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_source_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_set_source_file" %gtk-print-job-set-source-file)
    :boolean
  (job (g-object gtk-print-job))
  (filename :string)
  (error :pointer))

(defun gtk-print-job-set-source-file (job filename)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[filename]{the file to be printed}
  @return{@code{Nil} if an error occurred.}
  @begin{short}
    Make the @class{gtk-print-job} send an existing document to the printing
    system.
  @end{short}
  The file can be in any format understood by the platforms printing system,
  typically PostScript, but on many platforms PDF may work too. See the
  functions @fun{gtk-printer-accepts-pdf} and @fun{gtk-printer-accepts-ps}.

  Since 2.10
  @see-class{gtk-print-job}
  @see-function{gtk-printer-accepts-pdf}
  @see-function{gtk-printer-accepts-ps}"
  (with-g-error (err)
    (%gtk-print-job-set-source-file job filename err)))

(export 'gtk-print-job-set-source-file)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_surface" %gtk-print-job-get-surface)
    (:pointer (:struct cairo-surface-t))
  (job (g-object gtk-print-job))
  (error :pointer))

(defun gtk-print-job-get-surface (job)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The cairo surface of @arg{job}.}
  @begin{short}
    Gets a cairo surface onto which the pages of the print job should be
    rendered.
  @end{short}

  Since 2.10
  @see-class{gtk-print-job}
  @see-symbol{cairo-surface-t}"
  (with-g-error (err)
    (%gtk-print-job-get-surface job err)))

(export 'gtk-print-job-get-surface)

;;; ----------------------------------------------------------------------------
;;; GtkPrintJobCompleteFunc ()
;;;
;;; void (*GtkPrintJobCompleteFunc) (GtkPrintJob *print_job,
;;;                                  gpointer user_data,
;;;                                  const GError *error);
;;;
;;; The type of callback that is passed to gtk_print_job_send(). It is called
;;; when the print job has been completely sent.
;;;
;;; print_job :
;;;     the GtkPrintJob
;;;
;;; user_data :
;;;     user data that has been passed to gtk_print_job_send()
;;;
;;; error :
;;;     a GError that contains error information if the sending of the print
;;;     job failed, otherwise NULL
;;; ----------------------------------------------------------------------------

(defcallback gtk-print-job-complete-func-cb :void
    ((print-job (g-object gtk-print-job))
     (data :pointer)
     (error :pointer))
  (funcall (glib:get-stable-pointer-value data) print-job error))

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_send ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_send" %gtk-print-job-send) :void
  (job (g-object gtk-print-job))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun gtk-print-job-send (job func)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[func]{function to call when the job completes or an error occurs}
  @short{Sends the print job off to the printer.}

  Since 2.10
  @see-class{gtk-print-job}"
  (%gtk-print-job-send job
                       (callback gtk-print-job-complete-func-cb)
                       (glib:allocate-stable-pointer func)
                       (callback glib:stable-pointer-destroy-notify-cb)))

(export 'gtk-print-job-send)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_track_print_status ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-job-set-track-print-status))

(defun gtk-print-job-set-track-print-status (job track-status)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[track-status]{@em{true} to track status after printing}
  @begin{short}
    If @arg{track-status} is @em{true}, the print job will try to continue
    report on the status of the print job in the printer queues and printer.
  @end{short}
  This can allow your application to show things like \"out of paper\" issues,
  and when the print job actually reaches the printer.

  This function is often implemented using some form of polling, so it should
  not be enabled unless needed.

  Since 2.10
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-track-print-status}"
  (setf (gtk-print-job-track-print-status job) track-status))

(export 'gtk-print-job-set-track-print-status)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_track_print_status ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-job-get-track-print-status))

(defun gtk-print-job-get-track-print-status (job)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[job]{a @class{gtk-print-job} object}
  @return{@em{True} if print job status will be reported after printing.}
  @begin{short}
    Returns wheter jobs will be tracked after printing.
  @end{short}
  For details, see the function @fun{gtk-print-job-set-track-print-status}.

  Since 2.10
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-set-track-print-status}"
  (gtk-print-job-track-print-status job))

(export 'gtk-print-job-get-track-print-status)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_pages ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_pages" gtk-print-job-get-pages) gtk-print-pages
 #+cl-cffi-gtk-documentation
 "@version{2013-10-21}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The @symbol{gtk-print-pages} setting.}
  @short{Gets the @symbol{gtk-print-pages} setting for this job.}

  Since 3.0
  @see-class{gtk-print-job}
  @see-symbol{gtk-print-pages}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-get-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_pages ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_set_pages" gtk-print-job-set-pages) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[pages]{the @symbol{gtk-print-pages} setting}
  @begin{short}
    Sets the @symbol{gtk-print-pages} setting for this job.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-symbol{gtk-print-pages}"
  (job (g-object gtk-print-job))
  (pages gtk-print-pages))

(export 'gtk-print-job-set-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_page_ranges ()
;;;
;;; GtkPageRange * gtk_print_job_get_page_ranges (GtkPrintJob *job,
;;;                                               gint *n_ranges);
;;;
;;; Gets the page ranges for this job.
;;;
;;; job :
;;;     a GtkPrintJob
;;;
;;; n_ranges :
;;;     return location for the number of ranges
;;;
;;; Returns :
;;;     a pointer to an array of GtkPageRange structs.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_page_ranges ()
;;;
;;; void gtk_print_job_set_page_ranges (GtkPrintJob *job,
;;;                                     GtkPageRange *ranges,
;;;                                     gint n_ranges);
;;;
;;; Sets the page ranges for this job.
;;;
;;; job :
;;;     a GtkPrintJob
;;;
;;; ranges :
;;;     pointer to an array of GtkPageRange structs. [array length=n_ranges]
;;;
;;; n_ranges :
;;;     the length of the ranges array
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_page_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_page_set" gtk-print-job-get-page-set) gtk-page-set
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The @symbol{gtk-page-set} setting.}
  @begin{short}
    Gets the @symbol{gtk-page-set} setting for this job.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-symbol{gtk-page-set}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-get-page-set)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_page_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_set_page_set" gtk-print-job-set-page-set) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[page-set]{a @symbol{gtk-page-set} setting}
  @begin{short}
    Sets the @symbol{gtk-page-set} setting for this job.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-symbol{gtk-page-set}"
  (job (g-object gtk-print-job))
  (page-set gtk-page-set))

(export 'gtk-print-job-set-page-set)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_num_copies ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_num_copies" gtk-print-job-get-num-copies) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The number of copies.}
  @short{Gets the number of copies of this job.}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-set-num-copies}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-get-num-copies)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_num_copies ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_set_num_copies" gtk-print-job-set-num-copies) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[num-copies]{the number of copies}
  @short{Sets the number of copies for this job.}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-num-copies}"
  (job (g-object gtk-print-job))
  (num-copies :int))

(export 'gtk-print-job-set-num-copies)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_scale ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_scale" gtk-print-job-get-scale) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The scale.}
  @begin{short}
    Gets the scale for this job, where 1.0 means unscaled.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-set-scale}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-get-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_scale ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_set_scale" gtk-print-job-set-scale) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[scale]{the scale}
  @begin{short}
    Sets the scale for this job, where 1.0 means unscaled.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-scale}"
  (job (g-object gkt-print-job))
  (scale :double))

(export 'gtk-print-job-set-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_n_up ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_n_up" gtk-print-job-get-n-up) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The n-up setting.}
  @begin{short}
    Gets the n-up setting for this job.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-set-n-up}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-get-n-up)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_n_up ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_set_n_up" gtk-print-job-set-n-up) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[n-up]{the n-up value}
  @begin{short}
    Sets the n-up setting for this job.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-n-up}"
  (job (g-object gtk-print-job))
  (n-up :uint))

(export 'gtk-print-job-set-n-up)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_n_up_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_n_up_layout" gtk-print-job-get-n-up-layout)
    gtk-number-up-layout
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The n-up layout.}
  @begin{short}
    Gets the n-up layout setting for this job.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-set-n-up-layout}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-get-n-up-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_n_up_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_set_n_up_layout" gtk-print-job-set-n-layout) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[layout]{the n-up layout setting}
  @begin{short}
    Sets the n-up layout setting for this job.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-n-up-layout}"
  (job (g-object gtk-print-job))
  (layout gtk-number-up-layout))

(export 'gtk-print-job-set-n-up-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_rotate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_rotate" gtk-print-job-get-rotate) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @return{Whether the job is printed rotated.}
  @begin{short}
    Gets whether the job is printed rotated.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-set-rotate}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-get-rotate)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_rotate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_set_rotate" gtk-print-job-set-rotate) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[rotate]{whether to print rotated}
  @begin{short}
    Sets whether this job is printed rotated.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-rotate}"
  (job (g-object gtk-print-job))
  (rotate :boolean))

(export 'gtk-print-job-set-rotate)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_collate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_collate" gtk-print-job-get-collate) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @return{Whether the job is printed collated.}
  @begin{short}
    Gets whether this job is printed collated.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-set-collate}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-get-collate)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_collate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_set_collate" gtk-print-job-set-collate) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[collate]{whether the job is printed collated}
  @begin{short}
    Sets whether this job is printed collated.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-collate}"
  (job (g-object gtk-print-job))
  (collate :boolean))

(export 'gtk-print-job-set-collate)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_reverse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_reverse" gtk-print-job-get-reverse) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @return{Whether the job is printed reversed.}
  @begin{short}
    Gets whether this job is printed reversed.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-set-reverse}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-get-reverse)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_reverse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_set_reverse" gtk-print-job-set-reverse) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-23}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[reverse]{whether the job is printed reversed}
  @begin{short}
    Sets whether this job is printed reversed.
  @end{short}

  Since 3.0
  @see-class{gtk-print-job}
  @see-function{gtk-print-job-get-reverse}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-set-reverse)

;;; --- End of file gtk.print-job.lisp -----------------------------------------
