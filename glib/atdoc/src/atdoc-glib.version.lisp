;;; ----------------------------------------------------------------------------
;;; atdoc-glib.version.lisp
;;;
;;; Documentation strings for the library GLib.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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

;;; ----------------------------------------------------------------------------

(setf (gethash 'glib-major-version atdoc:*symbol-name-alias*) "Variable")
(setf (gethash 'glib-major-version atdoc:*external-symbols*)
 "@version{2013-1-15}
  @short{The major version number of the GLib library.}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'glib-minor-version atdoc:*symbol-name-alias*) "Variable")
(setf (gethash 'glib-minor-version atdoc:*external-symbols*)
 "@version{2013-1-15}
  @short{The minor version number of the GLib library.}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'glib-micro-version atdoc:*symbol-name-alias*) "Variable")
(setf (gethash 'glib-micro-version atdoc:*external-symbols*)
 "@version{2013-1-15}
  @short{The micro version number of the GLib library.}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'glib-binary-age atdoc:*symbol-name-alias*) "Variable")
(setf (gethash 'glib-binary-age atdoc:*external-symbols*)
 "@version{2013-1-15}
  @short{The binary age of the GLib library.}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'glib-interface-age atdoc:*symbol-name-alias*) "Variable")
(setf (gethash 'glib-interface-age atdoc:*external-symbols*)
 "@version{2013-1-15}
  @short{The interface age of the GLib library.}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'glib-check-version 'function)
 "@version{2013-1-15}
  @argument[required-major]{the required major version.}
  @argument[required-minor]{the required minor version.}
  @argument[required-micro]{the required micro version.}
  @return{@code{nil} if the GLib library is compatible with the given version,
    or a string describing the version mismatch. The returned string is owned by
    GLib and must not be modified or freed.}
  @short{Checks that the GLib library in use is compatible with the given
    version.}

  Since 2.6
  @see-symbol{glib-major-version}
  @see-symbol{glib-minor-version}
  @see-symbol{glib-micro-version}
  @see-symbol{glib-binary-age}
  @see-symbol{glib-interface-age}")

;;; --- End of file atdoc-glib.version.lisp ------------------------------------
