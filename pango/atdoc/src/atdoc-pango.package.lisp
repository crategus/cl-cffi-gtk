;;; ----------------------------------------------------------------------------
;;; atdoc-pango.package.lisp
;;;
;;; Documentation strings for the library Pango.
;;;
;;; The documentation has been copied from the Pango Reference Manual
;;; for Pango 1.29.5. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(in-package :pango)

(setf (documentation (find-package :pango) t)
 "@em{Pango} is a text layout and shaping library. @em{Pango} facilitates the
  layout and shaping of multi-language text. Full-function rendering of text and
  cross-platform support is had when @em{Pango} is used with platform APIs or
  3rd party libraries, such as Uniscribe and FreeType, as text rendering
  backends. Pango-processed text will appear similar under different operating
  systems.

  This is the API documentation of a Lisp binding to @em{Pango}. ")

;;; --- End of file atdoc-cairo.package.lisp -----------------------------------
