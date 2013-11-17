;;; ----------------------------------------------------------------------------
;;; cairo.status.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.12.14 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;;
;;; Error handling
;;;
;;; Decoding cairo's status
;;;
;;; Synopsis
;;;
;;;     cairo_status_t
;;;     cairo_status_to_string
;;;     cairo_debug_reset_static_data
;;;
;;; Description
;;;
;;; Cairo uses a single status type to represent all kinds of errors. A status
;;; value of CAIRO_STATUS_SUCCESS represents no error and has an integer value
;;; of zero. All other status values represent an error.
;;;
;;; Cairo's error handling is designed to be easy to use and safe. All major
;;; cairo objects retain an error status internally which can be queried anytime
;;; by the users using cairo*_status() calls. In the mean time, it is safe to
;;; call all cairo functions normally even if the underlying object is in an
;;; error status. This means that no error handling code is required before or
;;; after each individual cairo function call.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; enum cairo_status_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-status-t
  :success
  :no-memory
  :invalid-restore
  :invalid-pop-group
  :no-current-point
  :invalid-matrix
  :invalid-status
  :null-pointer
  :invalid-string
  :invalid-path-data
  :read-error
  :write-error
  :surface-finished
  :surface-type-mismatch
  :pattern-type-mismatch
  :invalid-content
  :invalid-format
  :invalid-visual
  :file-not-found
  :invalid-dash
  :invalid-dsc-comment
  :invalid-index
  :clip-not-representable
  :temp-file-error
  :invalide-stride
  :font-type-mismatch
  :user-font-immutable
  :user-font-error
  :negative-count
  :invalid-clusters
  :invalid-slant
  :invalid-weight
  :invalid-size
  :user-font-not-implemented
  :device-type-mismatch
  :device-error
  :invalid-mesh-construction
  :device-finished
  :last-status)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-status-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-status-t atdoc:*external-symbols*)
 "@version{2013-8-4}
  @begin{short}
    @sym{cairo-status-t} is used to indicate errors that can occur when using
    Cairo. In some cases it is returned directly by functions. but when using
    @symbol{cairo-t}, the last error, if any, is stored in the context and can
    be retrieved with the function @fun{cairo-status}.
  @end{short}

  New entries may be added in future versions. Use the function
  @fun{cairo-status-to-string} to get a human-readable representation of an
  error message.
  @begin{pre}
(defcenum cairo-status-t
  :success
  :no-memory
  :invalid-restore
  :invalid-pop-group
  :no-current-point
  :invalid-matrix
  :invalid-status
  :null-pointer
  :invalid-string
  :invalid-path-data
  :read-error
  :write-error
  :surface-finished
  :surface-type-mismatch
  :pattern-type-mismatch
  :invalid-content
  :invalid-format
  :invalid-visual
  :file-not-found
  :invalid-dash
  :invalid-dsc-comment
  :invalid-index
  :clip-not-representable
  :temp-file-error
  :invalide-stride
  :font-type-mismatch
  :user-font-immutable
  :user-font-error
  :negative-count
  :invalid-clusters
  :invalid-slant
  :invalid-weight
  :invalid-size
  :user-font-not-implemented
  :device-type-mismatch
  :device-error
  :invalid-mesh-construction
  :device-finished
  :last-status)
  @end{pre}
  @begin[code]{table}
    @entry[:success]{No error has occurred. Since 1.0}
    @entry[:no-memory]{Out of memory. Since 1.0}
    @entry[:invalid-store]{The function @fun{cairo-restore} called without
      matching the function @fun{cairo-save}. Since 1.0}
    @entry[:invalid-pop-group]{No saved group to pop, i. e. the function
      @fun{cairo-pop-group} without matching the function
      @fun{cairo-push-group}. Since 1.0}
    @entry[:no-current-point]{No current point defined. Since 1.0}
    @entry[:invalid-matrix]{Invalid matrix (not invertible). Since 1.0}
    @entry[:invalid-status]{Invalid value for an input @sym{cairo-status-t}.
      Since 1.0}
    @entry[:null-pointer]{@code{NULL} pointer. Since 1.0}
    @entry[:invalid-string]{Input string not valid UTF-8. Since 1.0}
    @entry[:path-data]{Input path data not valid. Since 1.0}
    @entry[:read-error]{Error while reading from input stream. Since 1.0}
    @entry[:write-error]{Error while writing to output stream. Since 1.0}
    @entry[:surface-finished]{Target surface has been finished. Since 1.0}
    @entry[:surface-type-mismatch]{The surface type is not appropriate for the
      operation. Since 1.0}
    @entry[:pattern-type-mismatch]{The pattern type is not appropriate for the
      operation. Since 1.0}
    @entry[:invalid-content]{Invalid value for an input
      @symbol{cairo-content-t}. Since 1.0}
    @entry[:invalid-format]{Invalid value for an input @symbol{cairo-format-t}.
      Since 1.0}
    @entry[:invalid-visual]{Invalid value for an input Visual*. Since 1.0}
    @entry[:file-not-found]{File not found. Since 1.0}
    @entry[:invalid-dash]{Invalid value for a dash setting. Since 1.0}
    @entry[:invalid-dsc-comment]{Invalid value for a DSC comment. Since 1.2}
    @entry[:invalid-index]{Invalid index passed to getter. Since 1.4}
    @entry[:clip-not-representable]{Clip region not representable in desired
      format. Since 1.4}
    @entry[:temp-file-error]{Error creating or writing to a temporary file.
      Since 1.6}
    @entry[:invalid-stride]{Invalid value for stride. Since 1.6}
    @entry[:font-type-mismatch]{The font type is not appropriate for the
      operation. Since 1.8}
    @entry[:user-font-immutable]{The user-font is immutable. Since 1.8}
    @entry[:user-font-error]{Error occurred in a user-font callback function.
      Since 1.8}
    @entry[:negative-count]{Negative number used where it is not allowed.
      Since 1.8}
    @entry[:invalid-clusters]{Input clusters do not represent the accompanying
      text and glyph array. Since 1.8}
    @entry[:invalid-slant]{Invalid value for an input
      @symbol{cairo-font-slant-t}. Since 1.8}
    @entry[:invalid-weight]{Invalid value for an input
      @symbol{cairo-font-weight-t}. Since 1.8}
    @entry[:invalid-size]{Invalid value (typically too big) for the size of the
      input (surface, pattern, etc.). Since 1.10}
    @entry[:user-font-not-implemented]{User-font method not implemented.
      Since 1.10}
    @entry[:device-type-mismatch]{The device type is not appropriate for the
      operation. Since 1.10}
    @entry[:device-error]{An operation to the device caused an unspecified
      error. Since 1.10}
    @entry[:invalid-mesh-construction]{A mesh pattern construction operation was
      used outside of a
      @fun{cairo-mesh-pattern-begin-patch}/@fun{cairo-mesh-pattern-end-patch}
      pair of functions. Since 1.12}
    @entry[:device-finished]{Target device has been finished. Since 1.12}
    @entry[:last-status]{This is a special value indicating the number of status
      values defined in this enumeration. When using this value, note that the
      version of cairo at run-time may have additional status values defined
      than the value of this symbol at compile-time. Since 1.10}
  @end{table}
  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-content-t}
  @see-symbol{cairo-format-t}
  @see-symbol{cairo-font-slant-t}
  @see-symbol{cairo-font-weight-t}
  @see-function{cairo-status}
  @see-function{cairo-status-to-string}
  @see-function{cairo-restore}
  @see-function{cairo-pop-group}
  @see-function{cairo-push-group}
  @see-function{cairo-mesh-pattern-begin-patch}
  @see-function{cairo-mesh-pattern-end-patch}")

(export 'cairo-status-t)

;;; ----------------------------------------------------------------------------
;;; cairo_status_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_status_to_string" cairo-status-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-11-16}
  @argument[status]{a cairo status}
  @return{A string representation of the status.}
  @begin{short}
    Provides a human-readable description of a @symbol{cairo-status-t}.
  @end{short}

  Since 1.0
  @see-symbol{cairo-status-t}"
  (status cairo-status-t))

(export 'cairo-status-to-string)

;;; ----------------------------------------------------------------------------
;;; cairo_debug_reset_static_data ()
;;;
;;; void cairo_debug_reset_static_data (void);
;;;
;;; Resets all static data within cairo to its original state, (ie. identical to
;;; the state at the time of program invocation). For example, all caches within
;;; cairo will be flushed empty.
;;;
;;; This function is intended to be useful when using memory-checking tools such
;;; as valgrind. When valgrind's memcheck analyzes a cairo-using program without
;;; a call to cairo_debug_reset_static_data(), it will report all data reachable
;;; via cairo's static objects as "still reachable". Calling
;;; cairo_debug_reset_static_data() just prior to program termination will make
;;; it easier to get squeaky clean reports from valgrind.
;;;
;;; WARNING: It is only safe to call this function when there are no active
;;; cairo objects remaining, (ie. the appropriate destroy functions have been
;;; called as necessary). If there are active cairo objects, this call is likely
;;; to cause a crash, (eg. an assertion failure due to a hash table being
;;; destroyed when non-empty).
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.status.lisp ------------------------------------------
