;;; ----------------------------------------------------------------------------
;;; atdoc-glib.quark.lisp
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

;;; --- g-quark ----------------------------------------------------------------

(setf (documentation 'g-quark 'type)
 "@version{2013-01-01}
  @begin{short}
    A @code{GQuark} is a non-zero integer which uniquely identifies a particular
    string. A @code{GQuark} value of zero is associated to NULL.
  @end{short}

  Quarks are associations between strings and integer identifiers. Given
  either the string or the GQuark identifier it is possible to retrieve the
  other.

  Quarks are used for both Datasets and Keyed Data Lists.

  To create a new quark from a string, use @fun{g-quark-from-string} or
  @code{g_quark_from_static_string()}.

  To find the string corresponding to a given GQuark, use
  @fun{g-quark-to-string}.

  To find the GQuark corresponding to a given string, use
  g_quark_try_string().

  Another use for the string pool maintained for the quark functions is string
  interning, using g_intern_string() or g_intern_static_string(). An interned
  string is a canonical representation for a string. One important advantage
  of interned strings is that they can be compared for equality by a simple
  pointer comparison, rather than using strcmp().")

;;; --- g-quark-from-string ----------------------------------------------------

(setf (documentation 'g-quark-from-string 'function)
 "@version{2013-01-01}
  @argument[string]{a string}
  @return{the GQuark identifying the string, or 0 if string is NULL.}
  @begin{short}
    Gets the GQuark identifying the given string.
  @end{short}
  If the string does not currently have an associated GQuark, a new GQuark is
  created, using a copy of the string.")

;;; ----------------------------------------------------------------------------
;;; g_quark_from_static_string ()
;;; 
;;; GQuark g_quark_from_static_string (const gchar *string);
;;; 
;;; Gets the GQuark identifying the given (static) string. If the string does
;;; not currently have an associated GQuark, a new GQuark is created, linked to
;;; the given string.
;;; 
;;; Note that this function is identical to g_quark_from_string() except that if
;;; a new GQuark is created the string itself is used rather than a copy. This
;;; saves memory, but can only be used if the string will always exist. It can
;;; be used with statically allocated strings in the main program, but not with
;;; statically allocated memory in dynamically loaded modules, if you expect to
;;; ever unload the module again (e.g. do not use this function in GTK+ theme
;;; engines).
;;; 
;;; string :
;;;     a string
;;; 
;;; Returns :
;;;     the GQuark identifying the string, or 0 if string is NULL.
;;; ----------------------------------------------------------------------------

;;; --- g-quark-to-string ------------------------------------------------------

(setf (documentation 'g-quark-to-string 'function)
 "@version{2013-01-01}
  @argument[quark]{a @code{GQuark}.}
  @return{the string associated with the @code{GQuark}.}
  @short{Gets the string associated with the given @code{GQuark}.}")

;;; ----------------------------------------------------------------------------
;;; g_quark_try_string ()
;;; 
;;; GQuark g_quark_try_string (const gchar *string);
;;; 
;;; Gets the GQuark associated with the given string, or 0 if string is NULL or
;;; it has no associated GQuark.
;;; 
;;; If you want the GQuark to be created if it doesn't already exist, use
;;; g_quark_from_string() or g_quark_from_static_string().
;;; 
;;; string :
;;;     a string
;;; 
;;; Returns :
;;;     the GQuark associated with the string, or 0 if string is NULL or there
;;;     is no GQuark associated with it.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_intern_string ()
;;; 
;;; const gchar * g_intern_string (const gchar *string);
;;; 
;;; Returns a canonical representation for string. Interned strings can be
;;; compared for equality by comparing the pointers, instead of using strcmp().
;;; 
;;; string :
;;;     a string
;;; 
;;; Returns :
;;;     a canonical representation for the string
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_intern_static_string ()
;;; 
;;; const gchar * g_intern_static_string (const gchar *string);
;;; 
;;; Returns a canonical representation for string. Interned strings can be
;;; compared for equality by comparing the pointers, instead of using strcmp().
;;; g_intern_static_string() does not copy the string, therefore string must not
;;; be freed or modified.
;;; 
;;; string :
;;;     a static string
;;; 
;;; Returns :
;;;     a canonical representation for the string
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; --- End of file atdoc-glib.quark.lisp --------------------------------------
