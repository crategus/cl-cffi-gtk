;;; ----------------------------------------------------------------------------
;;; atdoc-glib.error.lisp
;;;
;;; Documentation strings for the library GLib.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

(in-package :glib)

;;; --- g-error ----------------------------------------------------------------

(setf (documentation 'g-error 'type)
 "@version{2013-01-01}
  @begin{short}
    The GError structure contains information about an error that has occurred.
  @end{short}
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(defcstruct g-error
  (:domain g-quark)
  (:code :int)
  (:message (:string :free-from-foreign nil)))
    @end{pre}
    @begin{table}
      @entry[:domain]{error domain, e.g. G_FILE_ERROR}
      @entry[:code]{error code, e.g. G_FILE_ERROR_NOENT}
      @entry[:message]{human-readable informative error message}
    @end{table}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------

;;; Lisp support for GError

#|
(define-condition g-error-condition (error)
  ((domain  :initarg :domain
            :initform nil
            :reader g-error-condition-domain)
   (code    :initarg :code
            :initform nil
            :reader g-error-condition-code)
   (message :initarg :message
            :initform nil
            :reader g-error-condition-message))
  (:report (lambda (err stream)
             (format stream
                     "GError: Domain: ~S, Code: ~S, Message: ~A"
                     (g-error-condition-domain err)
                     (g-error-condition-code err)
                     (g-error-condition-message err)))))

(defun maybe-raise-g-error-condition (err)
  (unless (null-pointer-p err)
    (error 'g-error-condition
           :domain (foreign-slot-value err 'g-error :domain)
           :code (foreign-slot-value err 'g-error :code)
           :message (foreign-slot-value err 'g-error :message))))

(defmacro with-g-error ((err) &body body)
  `(with-foreign-object (,err :pointer)
     (setf (mem-ref ,err :pointer) (null-pointer))
     (unwind-protect
          (progn ,@body)
       (maybe-raise-g-error-condition (mem-ref ,err :pointer))
       (g-clear-error ,err))))

(defmacro with-catching-to-g-error ((err) &body body)
  `(handler-case
       (progn ,@body)
     (g-error-condition (e)
       (g-set-error-literal ,err
                            (g-error-condition-domain e)
                            (g-error-condition-code e)
                            (g-error-condition-message e)))))
|#

;;; ----------------------------------------------------------------------------
;;; g_error_new ()
;;; 
;;; GError * g_error_new (GQuark domain, gint code, const gchar *format, ...);
;;; 
;;; Creates a new GError with the given domain and code, and a message formatted
;;; with format.
;;; 
;;; domain :
;;;     error domain
;;; 
;;; code :
;;;     error code
;;; 
;;; format :
;;;     printf()-style format for error message
;;; 
;;; ... :
;;;     parameters for message format
;;; 
;;; Returns :
;;;     a new GError
;;; ----------------------------------------------------------------------------

;;; --- g-error-new-literal ----------------------------------------------------

(setf (documentation 'g-error-new-literal 'function)
 "@version{2013-01-01}
  @argument[domain]{error domain}
  @argument[code]{error code}
  @argument[message]{error message}
  @return{a new GError}
  @begin{short}
    Creates a new GError; unlike g_error_new(), message is not a printf()-style
    format string.
  @end{short}
  Use this function if message contains text you don't have control over, that
  could include printf() escape sequences.")

;;; ----------------------------------------------------------------------------
;;; g_error_new_valist ()
;;; 
;;; GError * g_error_new_valist (GQuark domain,
;;;                              gint code,
;;;                              const gchar *format,
;;;                              va_list args);
;;; 
;;; Creates a new GError with the given domain and code, and a message formatted
;;; with format.
;;; 
;;; domain :
;;;     error domain
;;; 
;;; code :
;;;     error code
;;; 
;;; format :
;;;     printf()-style format for error message
;;; 
;;; args :
;;;     va_list of parameters for the message format
;;; 
;;; Returns :
;;;     a new GError
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; --- g-error-free -----------------------------------------------------------

(setf (documentation 'g-error-free 'function)
 "@version{2013-01-01}
  @argument[error]{a GError}
  @short{Frees a GError and associated resources.}")

;;; --- g-error-copy -----------------------------------------------------------

(setf (documentation 'g-error-copy 'function)
 "@version{2013-01-01}
  @argument[error]{a GError}
  @return{a new GError}
  @short{Makes a copy of error.}")

;;; --- g-error-matches --------------------------------------------------------

(setf (documentation 'g-error-matches 'function)
 "@version{2013-01-01}
  @argument[error]{a GError or NULL}
  @argument[domain]{an error domain}
  @argument[code]{an error code}
  @return{whether error has domain and code}
  @begin{short}
    Returns TRUE if error matches domain and code, FALSE otherwise.
  @end{short}
  In particular, when error is NULL, FALSE will be returned.")

;;; ----------------------------------------------------------------------------
;;; g_set_error ()
;;; 
;;; void g_set_error (GError **err,
;;;                   GQuark domain,
;;;                   gint code,
;;;                   const gchar *format,
;;;                   ...);
;;; 
;;; Does nothing if err is NULL; if err is non-NULL, then *err must be NULL. A
;;; new GError is created and assigned to *err.
;;; 
;;; err :
;;;     a return location for a GError, or NULL
;;; 
;;; domain :
;;;     error domain
;;; 
;;; code :
;;;     error code
;;; 
;;; format :
;;;     printf()-style format
;;; 
;;; ... :
;;;     args for format
;;; ----------------------------------------------------------------------------

;;; --- g-set-error-literal ----------------------------------------------------

(setf (documentation 'g-set-error-literal 'function)
 "@version{2013-01-01}
  @argument[err]{a return location for a GError, or NULL}
  @argument[domain]{error domain}
  @argument[code]{error code}
  @argument[message]{error message}
  @begin{short}
    Does nothing if err is NULL; if err is non-NULL, then *err must be NULL.
  @end{short}
  A new GError is created and assigned to *err. Unlike g_set_error(), message is
  not a printf()-style format string. Use this function if message contains
  text you don't have control over, that could include printf() escape
  sequences.
 
  Since 2.18")

;;; --- g-propagte-error -------------------------------------------------------

(setf (documentation 'g-propagate-error 'function)
 "@version{2013-01-01}
  @argument[dest]{error return location}
  @argument[src]{error to move into the return location}
  @begin{short}
    If dest is NULL, free src; otherwise, moves src into *dest.
  @end{short}
  The error variable dest points to must be NULL.")

;;; --- g-clear-error ----------------------------------------------------------

(setf (documentation 'g-clear-error 'function)
 "@version{2013-01-01}
  @argument[err]{a GError return location}
  @begin{short}
    If err is NULL, does nothing. If err is non-NULL, calls g_error_free() on
    *err and sets *err to NULL.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; g_prefix_error ()
;;; 
;;; void g_prefix_error (GError **err, const gchar *format, ...);
;;; 
;;; Formats a string according to format and prefix it to an existing error
;;; message. If err is NULL (ie: no error variable) then do nothing.
;;; 
;;; If *err is NULL (ie: an error variable is present but there is no error
;;; condition) then also do nothing. Whether or not it makes sense to take
;;; advantage of this feature is up to you.
;;; 
;;; err :
;;;     a return location for a GError, or NULL
;;; 
;;; format :
;;;     printf()-style format string
;;; 
;;; ... :
;;;     arguments to format
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_propagate_prefixed_error ()
;;; 
;;; void g_propagate_prefixed_error (GError **dest,
;;;                                  GError *src,
;;;                                  const gchar *format,
;;;                                  ...);
;;; 
;;; If dest is NULL, free src; otherwise, moves src into *dest. *dest must be
;;; NULL. After the move, add a prefix as with g_prefix_error().
;;; 
;;; dest :
;;;     error return location
;;; 
;;; src :
;;;     error to move into the return location
;;; 
;;; format :
;;;     printf()-style format string
;;; 
;;; ... :
;;;     arguments to format
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------


;;; --- End of file atdoc-glib.error.lisp --------------------------------------
