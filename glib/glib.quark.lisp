;;; ----------------------------------------------------------------------------
;;; glib.quark.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.30.2 Reference Manual.  See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; Quarks
;;; 
;;; A 2-way association between a string and a unique integer identifier
;;; 
;;; Synopsis
;;; 
;;;     g-quark
;;;     g-quark-from-string
;;;     g-quark-to-string
;;;
;;; The following symbols are not implemented:
;;;
;;;     g_quark_from_static_string
;;;     g_quark_try_string
;;;     g_intern_string
;;;     g_intern_static_string
;;; 
;;; Description
;;; 
;;; Quarks are associations between strings and integer identifiers. Given
;;; either the string or the GQuark identifier it is possible to retrieve the
;;; other.
;;; 
;;; Quarks are used for both Datasets and Keyed Data Lists.
;;; 
;;; To create a new quark from a string, use g_quark_from_string() or
;;; g_quark_from_static_string().
;;; 
;;; To find the string corresponding to a given GQuark, use g_quark_to_string().
;;; 
;;; To find the GQuark corresponding to a given string, use
;;; g_quark_try_string().
;;; 
;;; Another use for the string pool maintained for the quark functions is
;;; string interning, using g_intern_string() or g_intern_static_string(). An
;;; interned string is a canonical representation for a string. One important
;;; advantage of interned strings is that they can be compared for equality by
;;;  a simple pointer comparison, rather than using strcmp().
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; GQuark
;;; 
;;; typedef guint32 GQuark;
;;; 
;;; A GQuark is a non-zero integer which uniquely identifies a particular
;;; string. A GQuark value of zero is associated to NULL.
;;; ----------------------------------------------------------------------------

(defctype %g-quark :uint32)

(define-foreign-type g-quark-type ()
  ()
  (:actual-type %g-quark)
  (:simple-parser g-quark))

(defmethod translate-to-foreign (value (type g-quark-type))
  (g-quark-from-string value))

(defmethod translate-from-foreign (value (type g-quark-type))
  (g-quark-to-string value))

(export 'g-quark)

;;; ----------------------------------------------------------------------------
;;; g_quark_from_string ()
;;; 
;;; GQuark g_quark_from_string (const gchar *string)
;;; 
;;; Gets the GQuark identifying the given string. If the string does not
;;; currently have an associated GQuark, a new GQuark is created, using a copy
;;; of the string.
;;; 
;;; string :
;;;     a string. [allow-none]
;;; 
;;; Returns :
;;;     the GQuark identifying the string, or 0 if string is NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("g_quark_from_string" g-quark-from-string) %g-quark
  (string :string))

(export 'g-quark-from-string)

;;; ----------------------------------------------------------------------------
;;; g_quark_to_string ()
;;; 
;;; const gchar * g_quark_to_string (GQuark quark)
;;; 
;;; Gets the string associated with the given GQuark.
;;; 
;;; quark :
;;;     a GQuark.
;;; 
;;; Returns :
;;;     the string associated with the GQuark.
;;; ----------------------------------------------------------------------------

(defcfun ("g_quark_to_string" g-quark-to-string) :string
  (quark %g-quark))

(export 'g-quark-to-string)

;;; ----------------------------------------------------------------------------
;;; g_quark_from_static_string ()
;;; 
;;; GQuark g_quark_from_static_string (const gchar *string)
;;; 
;;; Gets the GQuark identifying the given (static) string. If the string does
;;; not currently have an associated GQuark, a new GQuark is created, linked to
;;; the given string.
;;; 
;;; Note that this function is identical to g_quark_from_string() except that
;;; if a new GQuark is created the string itself is used rather than a copy.
;;; This saves memory, but can only be used if the string will always exist. It
;;; can be used with statically allocated strings in the main program, but not
;;; with statically allocated memory in dynamically loaded modules, if you
;;; expect to ever unload the module again (e.g. do not use this function in
;;; GTK+ theme engines).
;;; 
;;; string :
;;;     a string.
;;; 
;;; Returns :
;;;     the GQuark identifying the string, or 0 if string is NULL.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_quark_try_string ()
;;; 
;;; GQuark g_quark_try_string (const gchar *string)
;;; 
;;; Gets the GQuark associated with the given string, or 0 if string is NULL or
;;; it has no associated GQuark.
;;; 
;;; If you want the GQuark to be created if it doesn't already exist, use
;;; g_quark_from_string() or g_quark_from_static_string().
;;; 
;;; string :
;;;     a string.
;;; 
;;; Returns :
;;;     the GQuark associated with the string, or 0 if string is NULL or there
;;;     is no GQuark associated with it.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_intern_string ()
;;; 
;;; const gchar * g_intern_string (const gchar *string)
;;; 
;;; Returns a canonical representation for string. Interned strings can be
;;; compared for equality by comparing the pointers, instead of using strcmp().
;;; 
;;; string :
;;;     a string.
;;; 
;;; Returns :
;;;     a canonical representation for the string
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_intern_static_string ()
;;; 
;;; const gchar * g_intern_static_string (const gchar *string)
;;; 
;;; Returns a canonical representation for string. Interned strings can be
;;; compared for equality by comparing the pointers, instead of using strcmp().
;;; g_intern_static_string() does not copy the string, therefore string must
;;; not be freed or modified.
;;; 
;;; string :
;;;     a static string. [allow-none]
;;; 
;;; Returns :
;;;     a canonical representation for the string
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; --- End of file glib.quark.lisp --------------------------------------------
