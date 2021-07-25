;;; ----------------------------------------------------------------------------
;;; cairo.status.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Error handling
;;;
;;;     Decoding cairo's status
;;;
;;; Types and Values
;;;
;;;     cairo_status_t
;;;
;;; Functions
;;;
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
  :jbig2-global-missing
  :png-error
  :freetype-error
  :win32-gdk-error
  :tag-error
  :last-status)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-status-t atdoc:*symbol-name-alias*)
      "CEnum"
      (gethash 'cairo-status-t atdoc:*external-symbols*)
 "@version{2020-12-5}
  @begin{short}
    The @sym{cairo-status-t} enumeration is used to indicate errors that can
    occur when using Cairo.
  @end{short}
  In some cases it is returned directly by functions. but when using a
  @symbol{cairo-t} context, the last error, if any, is stored in the context
  and can be retrieved with the function @fun{cairo-status}.

  New entries may be added in future versions. Use the function
  @fun{cairo-status-to-string} to get a human readable representation of an
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
  :jbig2-global-missing
  :png-error
  :freetype-error
  :win32-gdk-error
  :tag-error
  :last-status)
  @end{pre}
  @begin[code]{table}
    @entry[:success]{No error has occurred.}
    @entry[:no-memory]{Out of memory.}
    @entry[:invalid-store]{The function @fun{cairo-restore} called without
      matching the function @fun{cairo-save}.}
    @entry[:invalid-pop-group]{No saved group to pop, i.e. the function
      @fun{cairo-pop-group} without matching the function
      @fun{cairo-push-group}.}
    @entry[:no-current-point]{No current point defined.}
    @entry[:invalid-matrix]{Invalid matrix (not invertible).}
    @entry[:invalid-status]{Invalid @sym{cairo-status-t} value for an input.}
    @entry[:null-pointer]{@code{NULL} pointer.}
    @entry[:invalid-string]{Input string not valid UTF-8.}
    @entry[:path-data]{Input path data not valid.}
    @entry[:read-error]{Error while reading from input stream.}
    @entry[:write-error]{Error while writing to output stream.}
    @entry[:surface-finished]{Target surface has been finished.}
    @entry[:surface-type-mismatch]{The surface type is not appropriate for the
      operation.}
    @entry[:pattern-type-mismatch]{The pattern type is not appropriate for the
      operation.}
    @entry[:invalid-content]{Invalid @symbol{cairo-content-t} value for an
      input.}
    @entry[:invalid-format]{Invalid @symbol{cairo-format-t} value for an input.}
    @entry[:invalid-visual]{Invalid value for an input Visual.}
    @entry[:file-not-found]{File not found.}
    @entry[:invalid-dash]{Invalid value for a dash setting.}
    @entry[:invalid-dsc-comment]{Invalid value for a DSC comment.}
    @entry[:invalid-index]{Invalid index passed to getter.}
    @entry[:clip-not-representable]{Clip region not representable in desired
      format.}
    @entry[:temp-file-error]{Error creating or writing to a temporary file.}
    @entry[:invalid-stride]{Invalid value for stride.}
    @entry[:font-type-mismatch]{The font type is not appropriate for the
      operation.}
    @entry[:user-font-immutable]{The user-font is immutable.}
    @entry[:user-font-error]{Error occurred in a user-font callback function.}
    @entry[:negative-count]{Negative number used where it is not allowed.}
    @entry[:invalid-clusters]{Input clusters do not represent the accompanying
      text and glyph array.}
    @entry[:invalid-slant]{Invalid @symbol{cairo-font-slant-t} value for an
      input.}
    @entry[:invalid-weight]{Invalid @symbol{cairo-font-weight-t} value for an
      input.}
    @entry[:invalid-size]{Invalid value (typically too big) for the size of the
      input (surface, pattern, etc.).}
    @entry[:user-font-not-implemented]{User-font method not implemented.}
    @entry[:device-type-mismatch]{The device type is not appropriate for the
      operation.}
    @entry[:device-error]{An operation to the device caused an unspecified
      error.}
    @entry[:invalid-mesh-construction]{A mesh pattern construction operation
      was used outside of a @fun{cairo-mesh-pattern-begin-patch} and
      @fun{cairo-mesh-pattern-end-patch} pair of functions.}
    @entry[:device-finished]{Target device has been finished.}
    @entry[:jbig2-global-missing]{@code{CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID} has
      been used on at least one image but no image provided
      @code{CAIRO_MIME_TYPE_JBIG2_GLOBAL}.}
    @entry[:png-error]{Error occurred in @code{libpng} while reading from or
      writing to a PNG file.}
    @entry[:freetype-error]{Error occurred in @code{libfreetype}.}
    @entry[:win32-gdi-error]{Error occurred in the Windows Graphics Device
      Interface.}
    @entry[:tag-error]{Invalid tag name, attributes, or nesting.}
    @entry[:last-status]{This is a special value indicating the number of status
      values defined in this enumeration. When using this value, note that the
      version of Cairo at run-time may have additional status values defined
      than the value of this symbol at compile-time.}
  @end{table}
  @see-symbol{cairo-t}
  @see-symbol{cairo-content-t}
  @see-symbol{cairo-format-t}
  @see-symbol{cairo-font-slant-t}
  @see-symbol{cairo-font-weight-t}
  @see-function{cairo-status}
  @see-function{cairo-status-to-string}
  @see-function{cairo-save}
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
 "@version{2020-12-5}
  @argument[status]{a value of the @symbol{cairo-status-t} enumeration}
  @return{A string representation of the Cario status.}
  @begin{short}
    Provides a human readable description of a Cairo status value.
  @end{short}
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
