;;; ----------------------------------------------------------------------------
;;; glib.error.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.36.3 Reference Manual. See <http://www.gtk.org>.
;;; The API documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Error Reporting
;;;
;;; A system for reporting errors
;;;
;;; Synopsis
;;;
;;;     GError
;;;
;;;     g_error_new
;;;     g_error_new_literal
;;;     g_error_new_valist
;;;     g_error_free
;;;     g_error_copy
;;;     g_error_matches
;;;     g_set_error
;;;     g_set_error_literal
;;;     g_propagate_error
;;;     g_clear_error
;;;     g_prefix_error
;;;     g_propagate_prefixed_error
;;;
;;; Description
;;;
;;; GLib provides a standard method of reporting errors from a called function
;;; to the calling code. (This is the same problem solved by exceptions in other
;;; languages.) It's important to understand that this method is both a data
;;; type (the GError object) and a set of rules. If you use GError incorrectly,
;;; then your code will not properly interoperate with other code that uses
;;; GError, and users of your API will probably get confused.
;;;
;;; First and foremost: GError should only be used to report recoverable runtime
;;; errors, never to report programming errors. If the programmer has screwed
;;; up, then you should use g_warning(), g_return_if_fail(), g_assert(),
;;; g_error(), or some similar facility. (Incidentally, remember that the
;;; g_error() function should only be used for programming errors, it should not
;;; be used to print any error reportable via GError.)
;;;
;;; Examples of recoverable runtime errors are "file not found" or "failed to
;;; parse input." Examples of programming errors are "NULL passed to strcmp()"
;;; or "attempted to free the same pointer twice." These two kinds of errors are
;;; fundamentally different: runtime errors should be handled or reported to the
;;; user, programming errors should be eliminated by fixing the bug in the
;;; program. This is why most functions in GLib and GTK+ do not use the GError
;;; facility.
;;;
;;; Functions that can fail take a return location for a GError as their last
;;; argument. For example:
;;;
;;;   gboolean g_file_get_contents (const gchar  *filename,
;;;                                 gchar       **contents,
;;;                                 gsize        *length,
;;;                                 GError      **error);
;;;
;;; If you pass a non-NULL value for the error argument, it should point to a
;;; location where an error can be placed. For example:
;;;
;;;   gchar *contents;
;;;   GError *err = NULL;
;;;   g_file_get_contents ("foo.txt", &contents, NULL, &err);
;;;   g_assert ((contents == NULL && err != NULL) ||
;;;             (contents != NULL && err == NULL));
;;;   if (err != NULL)
;;;     {
;;;       /* Report error to user, and free error */
;;;       g_assert (contents == NULL);
;;;       fprintf (stderr, "Unable to read file: %s\n", err->message);
;;;       g_error_free (err);
;;;     }
;;;   else
;;;     {
;;;       /* Use file contents */
;;;       g_assert (contents != NULL);
;;;     }
;;;
;;; Note that err != NULL in this example is a reliable indicator of whether
;;; g_file_get_contents() failed. Additionally, g_file_get_contents() returns a
;;; boolean which indicates whether it was successful.
;;;
;;; Because g_file_get_contents() returns FALSE on failure, if you are only
;;; interested in whether it failed and don't need to display an error message,
;;; you can pass NULL for the error argument:
;;;
;;;   if (g_file_get_contents ("foo.txt", &contents, NULL, NULL))
;;;     /* no error occurred */ ;
;;;   else
;;;     /* error */ ;
;;;
;;; The GError object contains three fields: domain indicates the module the
;;; error-reporting function is located in, code indicates the specific error
;;; that occurred, and message is a user-readable error message with as many
;;; details as possible. Several functions are provided to deal with an error
;;; received from a called function: g_error_matches() returns TRUE if the error
;;; matches a given domain and code, g_propagate_error() copies an error into an
;;; error location (so the calling function will receive it), and
;;; g_clear_error() clears an error location by freeing the error and resetting
;;; the location to NULL. To display an error to the user, simply display
;;; error->message, perhaps along with additional context known only to the
;;; calling function (the file being opened, or whatever -- though in the
;;; g_file_get_contents() case, error->message already contains a filename).
;;;
;;; When implementing a function that can report errors, the basic tool is
;;; g_set_error(). Typically, if a fatal error occurs you want to g_set_error(),
;;; then return immediately. g_set_error() does nothing if the error location
;;; passed to it is NULL. Here's an example:
;;;
;;;   gint
;;;   foo_open_file (GError **error)
;;;   {
;;;     gint fd;
;;;
;;;     fd = open ("file.txt", O_RDONLY);
;;;
;;;     if (fd < 0)
;;;       {
;;;         g_set_error (error,
;;;                      FOO_ERROR,                 /* error domain */
;;;                      FOO_ERROR_BLAH,            /* error code */
;;;                      "Failed to open file: %s", /* error message */
;;;                      g_strerror (errno));
;;;         return -1;
;;;       }
;;;     else
;;;       return fd;
;;;   }
;;;
;;; Things are somewhat more complicated if you yourself call another function
;;; that can report a GError. If the sub-function indicates fatal errors in some
;;; way other than reporting a GError, such as by returning TRUE on success, you
;;; can simply do the following:
;;;
;;;   gboolean
;;;   my_function_that_can_fail (GError **err)
;;;   {
;;;     g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
;;;
;;;     if (!sub_function_that_can_fail (err))
;;;       {
;;;         /* assert that error was set by the sub-function */
;;;         g_assert (err == NULL || *err != NULL);
;;;         return FALSE;
;;;       }
;;;
;;;     /* otherwise continue, no error occurred */
;;;     g_assert (err == NULL || *err == NULL);
;;;   }
;;;
;;; If the sub-function does not indicate errors other than by reporting a
;;; GError, you need to create a temporary GError since the passed-in one may be
;;; NULL. g_propagate_error() is intended for use in this case.
;;;
;;;   gboolean
;;;   my_function_that_can_fail (GError **err)
;;;   {
;;;     GError *tmp_error;
;;;
;;;     g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
;;;
;;;     tmp_error = NULL;
;;;     sub_function_that_can_fail (&tmp_error);
;;;
;;;     if (tmp_error != NULL)
;;;       {
;;;         /* store tmp_error in err, if err != NULL,
;;;          * otherwise call g_error_free() on tmp_error
;;;          */
;;;         g_propagate_error (err, tmp_error);
;;;         return FALSE;
;;;       }
;;;
;;;     /* otherwise continue, no error occurred */
;;;   }
;;;
;;; Error pileups are always a bug. For example, this code is incorrect:
;;;
;;;   gboolean
;;;   my_function_that_can_fail (GError **err)
;;;   {
;;;     GError *tmp_error;
;;;
;;;     g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
;;;
;;;     tmp_error = NULL;
;;;     sub_function_that_can_fail (&tmp_error);
;;;     other_function_that_can_fail (&tmp_error);
;;;
;;;     if (tmp_error != NULL)
;;;       {
;;;         g_propagate_error (err, tmp_error);
;;;         return FALSE;
;;;       }
;;;   }
;;;
;;; tmp_error should be checked immediately after sub_function_that_can_fail(),
;;; and either cleared or propagated upward. The rule is: after each error, you
;;; must either handle the error, or return it to the calling function. Note
;;; that passing NULL for the error location is the equivalent of handling an
;;; error by always doing nothing about it. So the following code is fine,
;;; assuming errors in sub_function_that_can_fail() are not fatal to
;;; my_function_that_can_fail():
;;;
;;;   gboolean
;;;   my_function_that_can_fail (GError **err)
;;;   {
;;;     GError *tmp_error;
;;;
;;;     g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
;;;
;;;     sub_function_that_can_fail (NULL); /* ignore errors */
;;;
;;;     tmp_error = NULL;
;;;     other_function_that_can_fail (&tmp_error);
;;;
;;;     if (tmp_error != NULL)
;;;       {
;;;         g_propagate_error (err, tmp_error);
;;;         return FALSE;
;;;       }
;;;   }
;;;
;;; Note that passing NULL for the error location ignores errors; it's
;;; equivalent to try { sub_function_that_can_fail(); } catch (...) {} in C++.
;;; It does not mean to leave errors unhandled; it means to handle them by doing
;;; nothing.
;;;
;;; Error domains and codes are conventionally named as follows:
;;;
;;;     The error domain is called <NAMESPACE>_<MODULE>_ERROR, for example
;;;     G_SPAWN_ERROR or G_THREAD_ERROR:
;;;
;;;       #define G_SPAWN_ERROR g_spawn_error_quark ()
;;;
;;;       GQuark
;;;       g_spawn_error_quark (void)
;;;       {
;;;         return g_quark_from_static_string ("g-spawn-error-quark");
;;;       }
;;;
;;;     The quark function for the error domain is called
;;;     <namespace>_<module>_error_quark, for example g_spawn_error_quark() or
;;;     g_thread_error_quark().
;;;
;;;     The error codes are in an enumeration called <Namespace><Module>Error;
;;;     for example,GThreadError or GSpawnError.
;;;
;;;     Members of the error code enumeration are called
;;;     <NAMESPACE>_<MODULE>_ERROR_<CODE>, for example G_SPAWN_ERROR_FORK or
;;;     G_THREAD_ERROR_AGAIN.
;;;
;;;     If there's a "generic" or "unknown" error code for unrecoverable errors
;;;     it doesn't make sense to distinguish with specific codes, it should be
;;;     called <NAMESPACE>_<MODULE>_ERROR_FAILED, for example
;;;     G_SPAWN_ERROR_FAILED.
;;;
;;; Summary of rules for use of GError:
;;;
;;;     Do not report programming errors via GError.
;;;
;;;     The last argument of a function that returns an error should be a
;;;     location where a GError can be placed (i.e. "GError** error"). If GError
;;;     is used with varargs, the GError** should be the last argument before
;;;     the "...".
;;;
;;;     The caller may pass NULL for the GError** if they are not interested in
;;;     details of the exact error that occurred.
;;;
;;;     If NULL is passed for the GError** argument, then errors should not be
;;;     returned to the caller, but your function should still abort and return
;;;     if an error occurs. That is, control flow should not be affected by
;;;     whether the caller wants to get a GError.
;;;
;;;     If a GError is reported, then your function by definition had a fatal
;;;     failure and did not complete whatever it was supposed to do. If the
;;;     failure was not fatal, then you handled it and you should not report it.
;;;     If it was fatal, then you must report it and discontinue whatever you
;;;     were doing immediately.
;;;
;;;     If a GError is reported, out parameters are not guaranteed to be set to
;;;     any defined value.
;;;
;;;     A GError* must be initialized to NULL before passing its address to a
;;;     function that can report errors.
;;;
;;;     "Piling up" errors is always a bug. That is, if you assign a new GError
;;;     to a GError* that is non-NULL, thus overwriting the previous error, it
;;;     indicates that you should have aborted the operation instead of
;;;     continuing. If you were able to continue, you should have cleared the
;;;     previous error with g_clear_error(). g_set_error() will complain if you
;;;     pile up errors.
;;;
;;;     By convention, if you return a boolean value indicating success then
;;;     TRUE means success and FALSE means failure. If FALSE is returned, the
;;;     error must be set to a non-NULL value.
;;;
;;;     A NULL return value is also frequently used to mean that an error
;;;     occurred. You should make clear in your documentation whether NULL is a
;;;     valid return value in non-error cases; if NULL is a valid value, then
;;;     users must check whether an error was returned to see if the function
;;;     succeeded.
;;;
;;;     When implementing a function that can report errors, you may want to add
;;;     a check at the top of your function that the error return location is
;;;     either NULL or contains a NULL error (e.g. g_return_if_fail
;;;     (error == NULL || *error == NULL);).
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; struct GError
;;; ----------------------------------------------------------------------------

(defcstruct g-error
  (:domain g-quark)
  (:code :int)
  (:message (:string :free-from-foreign nil)))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-error atdoc:*type-name-alias*) "CStruct"
      (documentation 'g-error 'type)
 "@version{2013-6-16}
  @begin{short}
    The @sym{g-error} structure contains information about an error that has
    occurred.
  @end{short}
  @begin{pre}
(defcstruct g-error
  (:domain g-quark)
  (:code :int)
  (:message (:string :free-from-foreign nil)))
  @end{pre}
  @begin{table}
    @entry[:domain]{Error domain, e. g. @code{G_FILE_ERROR}.}
    @entry[:code]{Error code, e. g. @code{G_FILE_ERROR_NOENT}.}
    @entry[:message]{Human-readable informative error message.}
  @end{table}")

(export 'g-error)

;;; ----------------------------------------------------------------------------

;;; Lisp support for GError

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
           :domain (foreign-slot-value err '(:struct g-error) :domain)
           :code (foreign-slot-value err '(:struct g-error) :code)
           :message (foreign-slot-value err '(:struct g-error) :message))))

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

;;; ----------------------------------------------------------------------------
;;; g_error_new_literal ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_error_new_literal" g-error-new-literal) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-6-16}
  @argument[domain]{error domain}
  @argument[code]{error code}
  @argument[message]{error message}
  @return{A new @type{g-error} structure.}
  @begin{short}
    Creates a new @type{g-error} structure; unlike the function
    @fun{g-error-new}, message is not a @code{printf()}-style format string.
  @end{short}
  Use this function if message contains text you do not have control over, that
  could include @code{printf()} escape sequences.
  @see-function{g-error-new}"
  (domain g-quark)
  (code :int)
  (message :string))

(export 'g-error-new-literal)

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

;;; ----------------------------------------------------------------------------
;;; g_error_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_error_free" g-error-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-16}
  @argument[error]{a @type{g-error} structure}
  Frees a @type{g-error} structure and associated resources."
  (error :pointer))

(export 'g-error-free)

;;; ----------------------------------------------------------------------------
;;; g_error_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_error_copy" g-error-copy) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-6-16}
  @argument[error]{a @type{g-error} structure}
  @return{A new @type{g-error} structure.}
  Makes a copy of @arg{error}."
  (error :pointer))

(export 'g-error-copy)

;;; ----------------------------------------------------------------------------
;;; g_error_matches ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_error_matches" g-error-matches) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-16}
  @argument[error]{a @type{g-error} structure or @code{nil}}
  @argument[domain]{an error domain}
  @argument[code]{an error code}
  @return{Whether @arg{error} has @arg{domain} and @arg{code}.}
  @begin{short}
    Returns @em{true} if @arg{error} matches @arg{domain} and @arg{code},
    @code{nil} otherwise.
  @end{short}
  In particular, when @arg{error} is @code{nil}, @code{nil} will be returned."
  (error :pointer)
  (domain g-quark)
  (code :int))

(export 'g-error-matches)

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

;;; ----------------------------------------------------------------------------
;;; g_set_error_literal ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_set_error_literal" g-set-error-literal) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-16}
  @argument[err]{a return location for a @type{g-error} structure,
    or @code{nil}}
  @argument[domain]{error domain}
  @argument[code]{error code}
  @argument[message]{error message}
  @begin{short}
    Does nothing if @arg{err} is @code{NULL}; if @arg{err} is
    non-@code{NULL}, then @arg{err} must be @code{NULL}.
  @end{short}
  A new @type{g-error} is created and assigned to @arg{err}. Unlike the function
  @fun{g-set-error}, message is not a @code{printf()}-style format string. Use
  this function if message contains text you do not have control over, that
  could include @code{printf()} escape sequences.

  Since 2.18
  @see-function{g-set-error}"
  (err :pointer)
  (domain g-quark)
  (code :int)
  (message :string))

(export 'g-set-error-literal)

;;; ----------------------------------------------------------------------------
;;; g_propagate_error ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_propagate_error" g-propagate-error) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-16}
  @argument[dest]{error return location}
  @argument[src]{error to move into the return location}
  @begin{short}
    If @arg{dest} is @code{NULL}, free @arg{src}; otherwise, moves @arg{src}
    into @arg{dest}.
  @end{short}
  The error variable @arg{dest} points to must be @code{NULL}."
  (dest :pointer)
  (src :pointer))

(export 'g-propagate-error)

;;; ----------------------------------------------------------------------------
;;; g_clear_error ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_clear_error" g-clear-error) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-16}
  @argument[err]{a @type{g-error} structure return location}
  If @arg{err} is @code{NULL}, does nothing. If @arg{err} is non-@code{NULL},
  calls the function @fun{g-error-free} on @arg{err} and sets @arg{err} to
  @code{NULL}."
  (err :pointer))

(export 'g-clear-error)

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

;;; --- End of file glib.error.lisp --------------------------------------------
