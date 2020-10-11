;;; ----------------------------------------------------------------------------
;;; glib.quark.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.62 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     A 2-way association between a string and a unique integer identifier
;;;
;;; Types and Values
;;;
;;;     GQuark
;;;
;;; Functions
;;;
;;;     G_DEFINE_QUARK
;;;     g_quark_from_string
;;;     g_quark_from_static_string
;;;     g_quark_to_string
;;;     g_quark_try_string
;;;     g_intern_string
;;;     g_intern_static_string
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; GQuark
;;; ----------------------------------------------------------------------------

;; A GQuark is implemented in the C Library as guint32.

(define-foreign-type g-quark-type ()
  ()
  (:actual-type :uint32)
  (:simple-parser g-quark))

(defmethod translate-to-foreign (value (type g-quark-type))
  (foreign-funcall "g_quark_from_string"
                   :string (if value value (null-pointer))
                   :uint32))

(defmethod translate-from-foreign (value (type g-quark-type))
  (foreign-funcall "g_quark_to_string"
                   :uint32 value
                   :string))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-quark 'type)
 "@version{2020-10-4}
  @begin{short}
    Quarks are associations between strings and integer identifiers.
  @end{short}
  Given either the string or the @code{GQuark} identifier it is possible to
  retrieve the other.
  @begin[Lisp binding]{dictionary}
    In the Lisp binding the type @sym{g-quark} translates a string argument to
    the corresponding @code{GQuark} identifier and a @code{GQuark} return value
    is translated to the corresponding Lisp string. No further functions are
    implemented for the @sym{g-quark} type.

    If the Lisp string does not currently have an associated @code{GQuark}, a
    new @code{GQuark} is created. A @code{GQuark} value of zero is associated
    to @code{nil} in Lisp.

    See the function @fun{g-type-qdata} for attaching a @code{GQuark} identifier
    to a @class{g-type}.
  @end{dictionary}
  @begin[Example]{dictionary}
    Translate a Lisp String to a @code{GQuark} identifier:
    @begin{pre}
  (convert-to-foreign \"GtkWidget\" 'g-quark)
=> 232
  (convert-to-foreign \"gboolean\" 'g-quark)
=> 9
  (convert-to-foreign nil 'g-quark)
=> 0
     @end{pre}
     Translate a @code{GQuark} identifier to a Lisp string:
     @begin{pre}
  (convert-from-foreign 232 'g-quark)
=> \"GtkWidget\"
  (convert-from-foreign 9 'g-quark)
=> \"gboolean\"
  (convert-from-foreign 0 'g-quark)
=> NIL
     @end{pre}
  @end{dictionary}
  @see-function{g-type-qdata}")

(export 'g-quark)

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_QUARK()
;;;
;;; #define G_DEFINE_QUARK(QN, q_n)
;;;
;;; A convenience macro which defines a function returning the GQuark for the
;;; name QN. The function will be named q_n_quark(). Note that the quark name
;;; will be stringified automatically in the macro, so you shouldn't use double
;;; quotes.
;;;
;;; QN :
;;;     the name to return a GQuark for
;;;
;;; q_n :
;;;     prefix for the function name
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_quark_from_string ()
;;; ----------------------------------------------------------------------------

;(defcfun ("g_quark_from_string" %g-quark-from-string) %g-quark
;  (str :string))

;(defun g-quark-from-string (str)
;  (%g-quark-from-string (if str str (null-pointer))))

;#+cl-cffi-gtk-documentation
;(setf (documentation 'g-quark-from-string 'function)
; "@version{2020-10-3}
;  @argument[str]{a string}
;  @return{The @type{g-quark} identifying @arg{str}, or 0 if @arg{str}
;    is @code{nil}.}
;  @begin{short}
;    Gets the @type{g-quark} identifying the given @arg{str}.
;  @end{short}
;  If @arg{str} does not currently have an associated @type{g-quark}, a
;  new @type{g-quark} is created, using a copy of the @arg{str}.
;  @see-type{g-quark}
;  @see-function{g-quark-from-string}")

;(export 'g-quark-from-string)

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

;;; ----------------------------------------------------------------------------
;;; g_quark_to_string ()
;;; ----------------------------------------------------------------------------

;(defcfun ("g_quark_to_string" g-quark-to-string) :string
; "@version{2013-6-1}
;  @argument[quark]{a @type{g-quark}}
;  @return{The string associated with the @type{g-quark}.}
;  Gets the string associated with the given @type{g-quark}."
;  (quark %g-quark))

;(export 'g-quark-to-string)

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

;;; --- End of file glib.quark.lisp --------------------------------------------
