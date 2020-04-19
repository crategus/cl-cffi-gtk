;;; ----------------------------------------------------------------------------
;;; gtk.print-job.lisp
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
;;; GtkPrintJob
;;;
;;;     Represents a print job
;;;
;;; Types and Values
;;;
;;;     GtkPrintJob
;;;
;;; Functions
;;;
;;;     gtk_print_job_new
;;;     gtk_print_job_get_settings                         Accessor
;;;     gtk_print_job_get_printer                          Accessor
;;;     gtk_print_job_get_title                            Accessor
;;;     gtk_print_job_get_status
;;;     gtk_print_job_set_source_fd
;;;     gtk_print_job_set_source_file
;;;     gtk_print_job_get_surface
;;;     gtk_print_job_send
;;;     gtk_print_job_set_track_print_status               Accessor
;;;     gtk_print_job_get_track_print_status               Accessor
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
;;;     GtkPageSetup*   page-setup            Read / Write / Construct Only
;;;       GtkPrinter*   printer               Read / Write / Construct Only
;;; GtkPrintSettings*   settings              Read / Write / Construct Only
;;;            gchar*   title                 Read / Write / Construct Only
;;;         gboolean    track-print-status    Read / Write
;;;
;;; Signals
;;;
;;;             void    status-changed        Run Last
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
 "@version{2020-4-10}
  @begin{short}
    A @sym{gtk-print-job} object represents a job that is sent to a printer.
  @end{short}
  You only need to deal directly with print jobs if you use the non-portable
  @class{gtk-print-unix-dialog} API.

  Use the function @fun{gtk-print-job-surface} to obtain the cairo surface onto
  which the pages must be drawn. Use the function @fun{gtk-print-job-send} to
  send the finished job to the printer. If you do not use Cairo
  @sym{gtk-print-job} also supports printing of manually generated PostScript,
  via the function @fun{gtk-print-job-set-source-file}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"status-changed\" signal}
      @begin{pre}
 lambda (job)    : Run Last
      @end{pre}
      Gets emitted when the status of a job changes. The signal handler can use
      the function @fun{gtk-print-job-status} to obtain the new status.
      @begin[code]{table}
        @entry[job]{The @sym{gtk-print-job} object on which the signal was
        emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-print-job-page-setup}
  @see-slot{gtk-print-job-printer}
  @see-slot{gtk-print-job-settings}
  @see-slot{gtk-print-job-title}
  @see-slot{gtk-print-job-track-print-status}
  @see-class{gtk-print-unix-dialog}
  @see-function{gtk-print-job-surface}
  @see-function{gtk-print-job-send}
  @see-function{gtk-print-job-set-source-file}
  @see-function{gtk-print-job-status}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-print-job-page-setup -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page-setup" 'gtk-print-job) t)
 "The @code{page-setup} property of type @class{gtk-page-setup}
  (Read / Write / Construct Only) @br{}
  Page Setup.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-job-page-setup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-job-page-setup 'function)
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-page-setup object) => page-setup}
  @argument[object]{a @class{gtk-print-job} object}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @begin{short}
    Accessor of the @slot[gtk-print-job]{page-setup} slot of the
    @class{gtk-print-job} class.
  @end{short}
  @see-class{gtk-print-job}
  @see-class{gtk-page-setup}")

;;; --- gtk-print-job-printer --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "printer" 'gtk-print-job) t)
 "The @code{printer} property of type @class{gtk-printer}
  (Read / Write / Construct Only) @br{}
  Printer to print the job to.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-job-printer atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-job-printer 'function)
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-printer object) => printer}
  @argument[object]{a @class{gtk-print-job} object}
  @argument[printer]{a @class{gtk-printer} object}
  @begin{short}
    Accessor of the @slot[gtk-print-job]{printer} of the
    @class{gtk-print-job} class.
  @end{short}
  Gets the printer of the print job.
  @see-class{gtk-print-job}
  @see-class{gtk-printer}")

;;; --- gtk-print-job-settings -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "settings" 'gtk-print-job) t)
 "The @code{settings} property of type @class{gtk-print-settings}
  (Read / Write / Construct Only) @br{}
  Printer settings.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-job-settings atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-job-settings 'function)
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-settings object) => settings}
  @argument[object]{a @class{gtk-print-job} object}
  @argument[settings]{a @class{gtk-print-settings} object}
  @begin{short}
    Accessor of the @slot[gtk-print-job]{settings} slot of the
    @class{gtk-print-job} class.
  @end{short}
  Gets the print settings of the print job.
  @see-class{gtk-print-job}
  @see-class{gtk-print-settings}")

;;; --- gtk-print-job-title ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title" 'gtk-print-job) t)
 "The @code{title} property of type @code{g-string}
  (Read / Write / Construct Only) @br{}
  Title of the print job. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-job-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-job-title 'function)
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-title object) => title}
  @argument[object]{a @class{gtk-print-job} object}
  @argument[title]{a string with the job title.}
  @begin{short}
    Accessor of the @slot[gtk-print-job]{title} slot of the
    @class{gtk-print-job} class.
  @end{short}
  Gets the job title.
  @see-class{gtk-print-job}")

;;; --- gtk-print-job-track-print-status ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "track-print-status"
                                               'gtk-print-job) t)
 "The @code{track-print-status} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the print job will continue to emit \"status-changed\" signals
  after the print data has been sent to the printer or print server. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-job-track-print-status atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-job-track-print-status 'function)
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-track-print-status object) => track-status}
  @syntax[]{(setf (gtk-print-job-track-print-status object) track-status)}
  @argument[object]{a @class{gtk-print-job} object}
  @argument[track-status]{@em{true} to track status after printing}
  @begin{short}
    Accessor of the @slot[gtk-print-job]{track-print-status} slot of the
    @class{gtk-print-job} class.
  @end{short}

  The slot access function @sym{gtk-print-job-track-print-status} returns
  whether jobs will be tracked after printing.

  If @arg{track-status} is @em{true}, the print job will try to continue
  report on the status of the print job in the printer queues and printer.

  This can allow your application to show things like \"out of paper\" issues,
  and when the print job actually reaches the printer. This function is often
  implemented using some form of polling, so it should not be enabled unless
  needed.
  @see-class{gtk-print-job}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-job-new))

(defun gtk-print-job-new (title printer settings page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @argument[title]{a string with the job title}
  @argument[printer]{a @class{gtk-printer} object}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @return{A new @class{gtk-print-job} object.}
  @short{Creates a new @class{gtk-print-job} object.}
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
;;; gtk_print_job_get_status () -> gtk-print-job-status
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_status" gtk-print-job-status)
    gtk-print-status
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The status of type @symbol{gtk-print-status} of @arg{job}.}
  @short{Gets the status of the print job.}
  @see-class{gtk-print-job}
  @see-symbol{gtk-print-status}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-status)

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
  (filename g-string)
  (error :pointer))

(defun gtk-print-job-set-source-file (job filename)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[filename]{a string with the file name to be printed}
  @return{@code{False} if an error occurred.}
  @begin{short}
    Make the print job send an existing document to the printing system.
  @end{short}
  The file can be in any format understood by the platforms printing system,
  typically PostScript, but on many platforms PDF may work too. See the
  functions @fun{gtk-printer-accepts-pdf} and @fun{gtk-printer-accepts-ps}.
  @see-class{gtk-print-job}
  @see-function{gtk-printer-accepts-pdf}
  @see-function{gtk-printer-accepts-ps}"
  (with-g-error (err)
    (%gtk-print-job-set-source-file job filename err)))

(export 'gtk-print-job-set-source-file)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_surface () -> gtk-print-job-surface
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_job_get_surface" %gtk-print-job-get-surface)
    (:pointer (:struct cairo-surface-t))
  (job (g-object gtk-print-job))
  (error :pointer))

(defun gtk-print-job-surface (job)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @argument[job]{a @class{gtk-print-job} object}
  @return{The cairo surface of type @symbol{cairo-surface-t} of @arg{job}.}
  @begin{short}
    Gets a cairo surface onto which the pages of the print job should be
    rendered.
  @end{short}
  @see-class{gtk-print-job}
  @see-symbol{cairo-surface-t}"
  (with-g-error (err)
    (%gtk-print-job-get-surface job err)))

(export 'gtk-print-job-surface)

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
 "@version{2020-4-10}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[func]{function to call when the job completes or an error occurs}
  @short{Sends the print job off to the printer.}
  @see-class{gtk-print-job}"
  (%gtk-print-job-send job
                       (callback gtk-print-job-complete-func-cb)
                       (glib:allocate-stable-pointer func)
                       (callback glib:stable-pointer-destroy-notify-cb)))

(export 'gtk-print-job-send)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_pages () -> gtk-print-job-pages
;;; gtk_print_job_set_pages () -> (setf gtk-print-job-pages)
;;; ----------------------------------------------------------------------------

(defun (setf gtk-print-job-pages) (pages job)
  (foreign-funcall "gtk_print_job_set_pages"
                   (g-object gtk-print-job) job
                   gtk-print-pages pages
                   :void)
  pages)

(defcfun ("gtk_print_job_get_pages" gtk-print-job-pages) gtk-print-pages
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-pages job) => pages}
  @syntax[]{(setf (gtk-print-job-pages job) pages)}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[pages]{the @symbol{gtk-print-pages} setting}
  @begin{short}
    Accessor of the page setting for the print job.
  @end{short}

  The function @sym{gtk-print-job-pages} gets the pages setting for the print
  job. The function @sym{(setf gtk-print-job-pages)} sets the pages setting for
  the print job.
  @see-class{gtk-print-job}
  @see-symbol{gtk-print-pages}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_page_ranges () -> gtk-print-job-page-ranges
;;; gtk_print_job_set_page_ranges () -> (setf gtk-print-job-page-ranges)
;;; ----------------------------------------------------------------------------

(defun (setf gtk-print-job-page-ranges) (ranges job)
  (setf (gtk-print-settings-page-ranges (gtk-print-job-settings job)) ranges))

(defun gtk-print-job-page-ranges (job)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-page-ranges job) => page-ranges}
  @syntax[]{(setf (gtk-print-job-page-ranges job) page-ranges)}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[page-ranges]{a list with the page ranges}
  @begin{short}
    Accessor of the page ranges for the print job.
  @end{short}

  The function @sym{gtk-print-job-page-ranges} gets the page ranges for the
  print job. The function @sym{(setf gtk-print-job-page-ranges)} sets the page
  for the print job.
  @see-class{gtk-print-job}"
  (gtk-print-settings-page-ranges (gtk-print-job-settings job)))

(export 'gtk-print-job-page-ranges)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_page_set () -> gtk-print-job-page-set
;;; gtk_print_job_set_page_set () -> (setf gtk-print-job-page-set)
;;; ----------------------------------------------------------------------------

(defun (setf gtk-print-job-page-set) (page-set job)
  (foreign-funcall "gtk_print_job_set_page_set"
                   (g-object gtk-print-job) job
                   gtk-page-set page-set
                   :void)
  page-set)

(defcfun ("gtk_print_job_get_page_set" gtk-print-job-page-set) gtk-page-set
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-page-set job) => page-set}
  @syntax[]{(setf (gtk-print-job-page-set job) page-set)}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[page-set]{a @symbol{gtk-page-set} setting}
  @begin{short}
    Accessor of the @symbol{gtk-page-set} setting for the print job.
  @end{short}

  The function @sym{gtk-print-job-page-set} gets the setting for the print job.
  The function @sym{(setf gtk-print-job-page-set)} sets the setting for the
  print job.
  @see-class{gtk-print-job}
  @see-symbol{gtk-page-set}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-page-set)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_num_copies () -> gtk-print-job-num-copies
;;; gtk_print_job_set_num_copies () -> (setf gtk-print-job-num-copies)
;;; ----------------------------------------------------------------------------

(defun (setf gtk-print-job-numcopies) (num-copies job)
  (foreign-funcall "gtk_print_job_set_num_copies"
                   (g-object gtk-print-job) job
                   :int num-copies)
  num-copies)

(defcfun ("gtk_print_job_get_num_copies" gtk-print-job-num-copies) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-num-copies job) => num-copies}
  @syntax[]{(setf (gtk-print-job-num-copies job) num-copies)}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[num-copies]{an integer with the number of copies}
  @begin{short}
    Accessor of the number of copies for the print job.
  @end{short}

  The function @sym{gtk-print-job-num-copies} gets the number of copies of the
  print job. The function @sym{(setf gtk-print-job-num-copies)} sets the number
  of copies for the print job.
  @see-class{gtk-print-job}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-num-copies)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_scale () -> gtk-print-job-scale
;;; gtk_print_job_set_scale () -> (setf gtk-print-job-scale)
;;; ----------------------------------------------------------------------------

(defun (setf gtk-print-job-scale) (scale job)
  (foreign-funcall "gtk_print_job_set_scale"
                   (g-object gtk-print-job) job
                   :double scale
                   :void)
  scale)

(defcfun ("gtk_print_job_get_scale" gtk-print-job-scale) :double
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-scale job) => scale}
  @syntax[]{(setf (gtk-print-job-scale job) scale)}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[scale]{a @code{:double} with the scale}
  @begin{short}
    Accessor of the scale for the print job.
  @end{short}

  The function @sym{gtk-print-job-scale} gets the scale for the print job,
  where 1.0 means unscaled. The function @sym{(setf gtk-print-job-scale)} sets
  the scale for the print job.
  @see-class{gtk-print-job}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_n_up () -> gtk-print-job-n-up
;;; gtk_print_job_set_n_up () -> (setf gtk-print-job-n-up)
;;; ----------------------------------------------------------------------------

(defun (setf gtk-print-job-n-up) (n-up job)
  (foreign-funcall "gtk_print_job_set_n_up"
                   (g-object gtk-print-job) job
                   :uint n-up
                   :void)
  n-up)

(defcfun ("gtk_print_job_get_n_up" gtk-print-job-n-up) :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-n-up job) => n-up}
  @syntax[]{(setf (gtk-print-job-n-up job) n-up)}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[n-up]{an unsigned integer with the n-up value}
  @begin{short}
    Accessor of the n-up setting for the print job.
  @end{short}

  The function @sym{gtk-print-job-n-up} gets the n-up setting for the print
  job. The function @sym{(setf gtk-print-job-n-up)} sets the n-up setting for
  the print job.
  @see-class{gtk-print-job}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-n-up)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_n_up_layout () -> gtk-print-job-n-up-layout
;;; gtk_print_job_set_n_up_layout () -> (setf gtk-print-job-n-up-layout)
;;; ----------------------------------------------------------------------------

(defun (setf gtk-print-job-n-up-layout) (layout job)
  (foreign-funcall "gtk_print_job_set_n_up_layout"
                   (g-object gtk-print-job) job
                   gtk-number-up-layout layout
                   :void)
  layout)

(defcfun ("gtk_print_job_get_n_up_layout" gtk-print-job-n-up-layout)
    gtk-number-up-layout
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-n-up-layout job) => layout}
  @syntax[]{(setf (gtk-print-job-n-up-layout job) layout)}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[layout]{the layout setting of type @symbol{gtk-number-up-layout}}
  @return{The n-up layout.}
  @begin{short}
    Accessor of the layout setting for the print job.
  @end{short}

  The function @sym{gtk-print-job-n-up-layout} gets the layout setting for the
  print job. The function @sym{(setf gtk-print-job-n-up-layout)} sets the
  layout setting for the print job.
  @see-class{gtk-print-job}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-n-up-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_rotate ()
;;; gtk_print_job_set_rotate ()
;;; ----------------------------------------------------------------------------

(defun (setf gtk-print-job-rotate) (rotate job)
  (foreign-funcall "gtk_print_job_set_rotate"
                   (g-object gtk-print-job) job
                   :boolean rotate
                   :void)
  rotate)

(defcfun ("gtk_print_job_get_rotate" gtk-print-job-rotate) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-rotate job) => rotate}
  @syntax[]{(setf (gtk-print-job-rotate job) rotate)}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[rotate]{a boolean whether to print rotated}
  @begin{short}
    Accessor of the rotate setting for the print job.
  @end{short}

  The function @sym{gtk-print-job-rotate} gets whether the job is printed
  rotated. The function @sym{(setf gtk-print-job-rotate)} sets whether the job
  is printed rotated.
  @see-class{gtk-print-job}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-rotate)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_collate ()
;;; gtk_print_job_set_collate ()
;;; ----------------------------------------------------------------------------

(defun (setf gtk-print-job-collate) (collate job)
  (foreign-funcall "gtk_print_job_set_collate"
                   (g-object gtk-print-job) job
                   :boolean collate
                   :void)
  collate)

(defcfun ("gtk_print_job_get_collate" gtk-print-job-collate) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-collate job) => collate}
  @syntax[]{(setf (gtk-print-job-collate job) collate)}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[collate]{a boolean whether the job is printed collated}
  @return{Whether the job is printed collated.}
  @begin{short}
    Accessor of the collate setting of the print job.
  @end{short}

  The function @sym{gtk-print-job-collate} gets whether the job is printed
  collated. The function @sym{(setf gtk-print-job-collate)} sets whether the
  job is printed collated.
  @see-class{gtk-print-job}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-collate)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_reverse ()
;;; gtk_print_job_set_reverse ()
;;; ----------------------------------------------------------------------------

(defun (setf gtk-print-job-reverse) (reverse job)
  (foreign-funcall "gtk_print_job_set_reverse"
                   (g-object gtk-print-job) job
                   :boolean reverse
                   :void)
  reverse)

(defcfun ("gtk_print_job_get_reverse" gtk-print-job-reverse) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-4-10}
  @syntax[]{(gtk-print-job-reverse job) => reverse}
  @syntax[]{(setf (gtk-print-job-reverse job) reverse)}
  @argument[job]{a @class{gtk-print-job} object}
  @argument[reverse]{a boolean whether the job is printed reversed}
  @begin{short}
    Accessor of the reverse setting of the print job.
  @end{short}

  The function @sym{gtk-print-job-reverse} gets whether the job is printed
  reversed. The function @sym{(setf gtk-print-job-reverse)} sets whether the
  job is printed reversed.
  @see-class{gtk-print-job}"
  (job (g-object gtk-print-job)))

(export 'gtk-print-job-reverse)

;;; --- End of file gtk.print-job.lisp -----------------------------------------
