;;; ----------------------------------------------------------------------------
;;; glib.error.lisp
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.30.2 Reference Manual.  See http://www.gtk.org.
;;;
;;; ----------------------------------------------------------------------------
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
;;;     g-error
;;;
;;;     g-error-new-literal
;;;     g-error-free
;;;     g-error-copy
;;;     g-error-matches
;;;     g-set-error-literal
;;;     g-propagate-error
;;;     g-clear-error
;;;
;;; The following symbols are not implemented:
;;;
;;;     g_error_new
;;;     g_error_new_valist
;;;     g_set_error
;;;     g_prefix_error
;;;     g_propagate_prefixed_error
;;; 
;;; Description
;;; 
;;; GLib provides a standard method of reporting errors from a called function
;;; to the calling code. (This is the same problem solved by exceptions in
;;; other languages.) It's important to understand that this method is both a
;;; data type (the GError object) and a set of rules. If you use GError
;;; incorrectly, then your code will not properly interoperate with other code
;;; that uses GError, and users of your API will probably get confused.
;;; 
;;; First and foremost: GError should only be used to report recoverable
;;; runtime errors, never to report programming errors. If the programmer has
;;; screwed up, then you should use g_warning(), g_return_if_fail(),
;;; g_assert(), g_error(), or some similar facility. (Incidentally, remember
;;; that the g_error() function should only be used for programming errors, it
;;; should not be used to print any error reportable via GError.)
;;; 
;;; Examples of recoverable runtime errors are "file not found" or "failed to
;;; parse input." Examples of programming errors are "NULL passed to strcmp()"
;;; or "attempted to free the same pointer twice." These two kinds of errors
;;; are fundamentally different: runtime errors should be handled or reported
;;; to the user, programming errors should be eliminated by fixing the bug in
;;; the program. This is why most functions in GLib and GTK+ do not use the
;;; GError facility.
;;; 
;;; Functions that can fail take a return location for a GError as their last
;;; argument. For example:
;;; 
;;;  1 gboolean g_file_get_contents (const gchar  *filename,
;;;  2                               gchar       **contents,
;;;  3                               gsize        *length,
;;;  4                               GError      **error);
;;; 
;;; If you pass a non-NULL value for the error argument, it should point to a
;;; location where an error can be placed. For example:
;;; 
;;;  1 gchar *contents;
;;;  2 GError *err = NULL;
;;;  3 g_file_get_contents ("foo.txt", &contents, NULL, &err);
;;;  4 g_assert ((contents == NULL && err != NULL) ||
;;;              (contents != NULL && err == NULL));
;;;  5 if (err != NULL)
;;;  6   {
;;;  7     /* Report error to user, and free error */
;;;  8     g_assert (contents == NULL);
;;;  9     fprintf (stderr, "Unable to read file: %s\n", err->message);
;;; 10     g_error_free (err);
;;; 11   }
;;; 12 else
;;; 13   {
;;; 14     /* Use file contents */
;;; 15     g_assert (contents != NULL);
;;; 16   }
;;; 
;;; Note that err != NULL in this example is a reliable indicator of whether
;;; g_file_get_contents() failed. Additionally, g_file_get_contents() returns
;;; a boolean which indicates whether it was successful.
;;; 
;;; Because g_file_get_contents() returns FALSE on failure, if you are only
;;; interested in whether it failed and don't need to display an error message,
;;; you can pass NULL for the error argument:
;;; 
;;;  1 if (g_file_get_contents ("foo.txt", &contents, NULL, NULL))
;;;  2   /* no error occurred */ ;
;;;  3 else
;;;  4   /* error */ ;
;;; 
;;; The GError object contains three fields: domain indicates the module the
;;; error-reporting function is located in, code indicates the specific error
;;; that occurred, and message is a user-readable error message with as many
;;; details as possible. Several functions are provided to deal with an error
;;; received from a called function: g_error_matches() returns TRUE if the
;;; error matches a given domain and code, g_propagate_error() copies an error
;;; into an error location (so the calling function will receive it), and
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
;;;  1 gint
;;;  2 foo_open_file (GError **error)
;;;  3 {
;;;  4   gint fd;
;;;  5 
;;;  6   fd = open ("file.txt", O_RDONLY);
;;;  7 
;;;  8   if (fd < 0)
;;;  9     {
;;; 10       g_set_error (error,
;;; 11                    FOO_ERROR,                 /* error domain */
;;; 12                    FOO_ERROR_BLAH,            /* error code */
;;; 13                    "Failed to open file: %s", /* error message string */
;;; 14                    g_strerror (errno));
;;; 15       return -1;
;;; 16     }
;;; 17   else
;;; 18     return fd;
;;; 19 }
;;; 
;;; Things are somewhat more complicated if you yourself call another function
;;; that can report a GError. If the sub-function indicates fatal errors in
;;; some way other than reporting a GError, such as by returning TRUE on
;;; success, you can simply do the following:
;;; 
;;;  1 gboolean
;;;  2 my_function_that_can_fail (GError **err)
;;;  3 {
;;;  4   g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
;;;  5 
;;;  6   if (!sub_function_that_can_fail (err))
;;;  7     {
;;;  8       /* assert that error was set by the sub-function */
;;;  9       g_assert (err == NULL || *err != NULL);
;;; 10       return FALSE;
;;; 11     }
;;; 12 
;;; 13   /* otherwise continue, no error occurred */
;;; 14   g_assert (err == NULL || *err == NULL);
;;; 15 }
;;; 
;;; If the sub-function does not indicate errors other than by reporting a
;;; GError, you need to create a temporary GError since the passed-in one may
;;; be NULL. g_propagate_error() is intended for use in this case. 	
;;; 
;;;  1 gboolean
;;;  2 my_function_that_can_fail (GError **err)
;;;  3 {
;;;  4   GError *tmp_error;
;;;  5 
;;;  6   g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
;;;  7 
;;;  8   tmp_error = NULL;
;;;  9   sub_function_that_can_fail (&tmp_error);
;;; 10 
;;; 11   if (tmp_error != NULL)
;;; 12     {
;;; 13       /* store tmp_error in err, if err != NULL,
;;; 14        * otherwise call g_error_free() on tmp_error
;;; 15        */
;;; 16       g_propagate_error (err, tmp_error);
;;; 17       return FALSE;
;;; 18     }
;;; 20 
;;; 21   /* otherwise continue, no error occurred */
;;; 22 }
;;;  
;;; Error pileups are always a bug. For example, this code is incorrect:
;;; 
;;;  1 gboolean
;;;  2 my_function_that_can_fail (GError **err)
;;;  3 {
;;;  4   GError *tmp_error;
;;;  5 
;;;  6   g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
;;;  7 
;;;  8   tmp_error = NULL;
;;;  9   sub_function_that_can_fail (&tmp_error);
;;; 10   other_function_that_can_fail (&tmp_error);
;;; 11 
;;; 12   if (tmp_error != NULL)
;;; 13     {
;;; 14       g_propagate_error (err, tmp_error);
;;; 15       return FALSE;
;;; 16     }
;;; 17 }
;;; 
;;; tmp_error should be checked immediately after sub_function_that_can_fail(),
;;; and either cleared or propagated upward. The rule is: after each error, you
;;; must either handle the error, or return it to the calling function. Note
;;; that passing NULL for the error location is the equivalent of handling an
;;; error by always doing nothing about it. So the following code is fine,
;;; assuming errors in sub_function_that_can_fail() are not fatal to
;;; my_function_that_can_fail():
;;; 
;;;  1 gboolean
;;;  2 my_function_that_can_fail (GError **err)
;;;  3 {
;;;  4   GError *tmp_error;
;;;  5 
;;;  6   g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
;;;  7 
;;;  8   sub_function_that_can_fail (NULL); /* ignore errors */
;;;  9 
;;; 10   tmp_error = NULL;
;;; 11   other_function_that_can_fail (&tmp_error);
;;; 12 
;;; 13   if (tmp_error != NULL)
;;; 14     {
;;; 15       g_propagate_error (err, tmp_error);
;;; 16       return FALSE;
;;; 17     }
;;; 18 }
;;; 
;;; Note that passing NULL for the error location ignores errors; it's
;;; equivalent to try { sub_function_that_can_fail(); } catch (...) {} in C++.
;;; It does not mean to leave errors unhandled; it means to handle them by
;;; doing nothing.
;;; 
;;; Error domains and codes are conventionally named as follows:
;;; 
;;;     The error domain is called <NAMESPACE>_<MODULE>_ERROR, for example 
;;;     G_SPAWN_ERROR or G_THREAD_ERROR:    	
;;; 
;;;      1 #define G_SPAWN_ERROR g_spawn_error_quark ()
;;;      2
;;;      3 GQuark
;;;      4 g_spawn_error_quark (void)
;;;      5 {
;;;      6   return g_quark_from_static_string ("g-spawn-error-quark");
;;;      7 }
;;; 
;;;     The quark function for the error domain is called
;;;     <namespace>_<module>_error_quark, for example g_spawn_error_quark() or
;;;     %g_thread_error_quark().
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
;;;     G_SPAWN_ERROR_FAILED or G_THREAD_ERROR_FAILED.
;;; 
;;; Summary of rules for use of GError:
;;; 
;;;     Do not report programming errors via GError.
;;; 
;;;     The last argument of a function that returns an error should be a
;;;     location where a GError can be placed (i.e. "GError** error"). If
;;;     GError is used with varargs, the GError** should be the last argument
;;;     before the "...".
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
;;;     When implementing a function that can report errors, you may want to
;;;     add a check at the top of your function that the error return location
;;;     is either NULL or contains a NULL error (e.g. g_return_if_fail
;;;     (error == NULL || *error == NULL);).
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; struct g-error
;;; 
;;; struct GError {
;;;   GQuark       domain;
;;;   gint         code;
;;;   gchar       *message;
;;; };
;;; 
;;; The GError structure contains information about an error that has occurred.
;;; 
;;; GQuark domain;
;;; 	error domain, e.g. G_FILE_ERROR
;;; 
;;; gint code;
;;; 	error code, e.g. G_FILE_ERROR_NOENT
;;; 
;;; gchar *message;
;;; 	human-readable informative error message
;;; ----------------------------------------------------------------------------

(defcstruct g-error
  (:domain g-quark)
  (:code :int)
  (:message (:string :free-from-foreign nil)))

(export 'g-error)

;;; ----------------------------------------------------------------------------
;;; g_error_new ()
;;; 
;;; GError * g_error_new (GQuark domain, gint code, const gchar *format, ...)
;;; 
;;; Creates a new GError with the given domain and code, and a message
;;; formatted with format.
;;; 
;;; domain :
;;; 	error domain
;;; 
;;; code :
;;; 	error code
;;; 
;;; format :
;;; 	printf()-style format for error message
;;; 
;;; ... :
;;; 	parameters for message format
;;; 
;;; Returns :
;;; 	a new GError
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_error_new_literal ()
;;; 
;;; GError* g_error_new_literal (GQuark domain, gint code, const gchar *message)
;;; 
;;; Creates a new GError; unlike g_error_new(), message is not a printf()-style
;;; format string. Use this function if message contains text you don't have
;;; control over, that could include printf() escape sequences.
;;; 
;;; domain :
;;; 	error domain
;;; 
;;; code :
;;; 	error code
;;; 
;;; message :
;;; 	error message
;;; 
;;; Returns :
;;; 	a new GError
;;; ----------------------------------------------------------------------------

(defcfun ("g_error_new_literal" g-error-new-literal) :pointer
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
;;;                              va_list args)
;;; 
;;; Creates a new GError with the given domain and code, and a message
;;; formatted with format.
;;; 
;;; domain :
;;; 	error domain
;;; 
;;; code :
;;; 	error code
;;; 
;;; format :
;;; 	printf()-style format for error message
;;; 
;;; args :
;;; 	va_list of parameters for the message format
;;; 
;;; Returns :
;;; 	a new GError
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_error_free ()
;;; 
;;; void g_error_free (GError *error)
;;; 
;;; Frees a GError and associated resources.
;;; 
;;; error :
;;; 	a GError
;;; ----------------------------------------------------------------------------

(defcfun ("g_error_free" g-error-free) :void
  (error :pointer))

(export 'g-error-free)

;;; ----------------------------------------------------------------------------
;;; g_error_copy ()
;;; 
;;; GError * g_error_copy (const GError *error)
;;; 
;;; Makes a copy of error.
;;; 
;;; error :
;;; 	a GError
;;; 
;;; Returns :
;;; 	a new GError
;;; ----------------------------------------------------------------------------

(defcfun ("g_error_copy" g-error-copy) :pointer
  (error :pointer))

(export 'g-error-copy)

;;; ----------------------------------------------------------------------------
;;; g_error_matches ()
;;; 
;;; gboolean g_error_matches (const GError *error, GQuark domain, gint code)
;;; 
;;; Returns TRUE if error matches domain and code, FALSE otherwise. In
;;; particular, when error is NULL, FALSE will be returned.
;;; 
;;; error :
;;; 	a GError or NULL
;;; 
;;; domain :
;;; 	an error domain
;;; 
;;; code :
;;; 	an error code
;;; 
;;; Returns :
;;; 	whether error has domain and code
;;; ----------------------------------------------------------------------------

(defcfun ("g_error_matches" g-error-matches) :boolean
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
;;;                   ...)
;;; 
;;; Does nothing if err is NULL; if err is non-NULL, then *err must be NULL.
;;; A new GError is created and assigned to *err.
;;; 
;;; err :
;;; 	a return location for a GError, or NULL
;;; 
;;; domain :
;;; 	error domain
;;; 
;;; code :
;;; 	error code
;;; 
;;; format :
;;; 	printf()-style format
;;; 
;;; ... :
;;; 	args for format
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_set_error_literal ()
;;; 
;;; void g_set_error_literal (GError **err,
;;;                           GQuark domain,
;;;                           gint code,
;;;                           const gchar *message)
;;; 
;;; Does nothing if err is NULL; if err is non-NULL, then *err must be NULL.
;;; A new GError is created and assigned to *err. Unlike g_set_error(), message
;;; is not a printf()-style format string. Use this function if message
;;; contains text you don't have control over, that could include printf()
;;; escape sequences.
;;; 
;;; err :
;;; 	a return location for a GError, or NULL
;;; 
;;; domain :
;;; 	error domain
;;; 
;;; code :
;;; 	error code
;;; 
;;; message :
;;; 	error message
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("g_set_error_literal" g-set-error-literal) :void
  (err-ptr :pointer)
  (domain g-quark)
  (code :int)
  (message :string))

(export 'g-set-error-literal)

;;; ----------------------------------------------------------------------------
;;; g_propagate_error ()
;;; 
;;; void g_propagate_error (GError **dest, GError *src)
;;; 
;;; If dest is NULL, free src; otherwise, moves src into *dest. The error
;;; variable dest points to must be NULL.
;;; 
;;; dest :
;;; 	error return location
;;; 
;;; src :
;;; 	error to move into the return location
;;; ----------------------------------------------------------------------------

(defcfun ("g_propagate_error" g-propagate-error) :void
  (dest-ptr :pointer)
  (src-ptr :pointer))

(export 'g-propagate-error)

;;; ----------------------------------------------------------------------------
;;; g_clear_error ()
;;; 
;;; void g_clear_error (GError **err)
;;; 
;;; If err is NULL, does nothing. If err is non-NULL, calls g_error_free() on
;;; *err and sets *err to NULL.
;;; 
;;; err :
;;; 	a GError return location
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_prefix_error ()
;;; 
;;; void g_prefix_error (GError **err, const gchar *format, ...)
;;; 
;;; Formats a string according to format and prefix it to an existing error
;;; message. If err is NULL (ie: no error variable) then do nothing.
;;; 
;;; If *err is NULL (ie: an error variable is present but there is no error
;;; condition) then also do nothing. Whether or not it makes sense to take
;;; advantage of this feature is up to you.
;;; 
;;; err :
;;; 	a return location for a GError, or NULL
;;; 
;;; format :
;;; 	printf()-style format string
;;; 
;;; ... :
;;; 	arguments to format
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("g_clear_error" g-clear-error) :void
  (err-ptr :pointer))

(export 'g-clear-error)

;;; ----------------------------------------------------------------------------
;;; g_propagate_prefixed_error ()
;;; 
;;; void g_propagate_prefixed_error (GError **dest,
;;;                                  GError *src,
;;;                                  const gchar *format,
;;;                                  ...)
;;; 
;;; If dest is NULL, free src; otherwise, moves src into *dest. *dest must be
;;; NULL. After the move, add a prefix as with g_prefix_error().
;;; 
;;; dest :
;;; 	error return location
;;; 
;;; src :
;;; 	error to move into the return location
;;; 
;;; format :
;;; 	printf()-style format string
;;; 
;;; ... :
;;; 	arguments to format
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; --- End of file glib.error.lisp --------------------------------------------
