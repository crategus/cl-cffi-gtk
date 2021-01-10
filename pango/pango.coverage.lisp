;;; ----------------------------------------------------------------------------
;;; pango.fonts.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.48 and modified to document the Lisp binding to the Pango library.
;;; See <http://www.pango.org>. The API documentation of the Lisp binding is
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
;;; Coverage Maps
;;;
;;;     Unicode character range coverage storage
;;;
;;; Types and Values
;;;
;;;     PangoCoverageLevel
;;;     PANGO_TYPE_COVERAGE_LEVEL
;;;     PangoCoverage
;;;
;;; Functions
;;;
;;;     pango_coverage_new
;;;     pango_coverage_ref
;;;     pango_coverage_unref
;;;     pango_coverage_copy
;;;     pango_coverage_get
;;;     pango_coverage_max
;;;     pango_coverage_set
;;;     pango_coverage_to_bytes
;;;     pango_coverage_from_bytes
;;;
;;; Object Hierarchy
;;;
;;;     GEnum
;;;     ╰── PangoCoverageLevel
;;;
;;;     GObject
;;;     ╰── PangoCoverage
;;;
;;; Description
;;;
;;;     It is often necessary in Pango to determine if a particular font can
;;;     represent a particular character, and also how well it can represent
;;;     that character. The PangoCoverage is a data structure that is used to
;;;     represent that information.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; enum PangoCoverageLevel
;;;
;;; Used to indicate how well a font can represent a particular Unicode
;;; character point for a particular script.
;;;
;;; Since 1.44, only PANGO_COVERAGE_NONE and PANGO_COVERAGE_EXACT will be
;;; returned.
;;;
;;; PANGO_COVERAGE_NONE
;;;     The character is not representable with the font.
;;;
;;; PANGO_COVERAGE_FALLBACK
;;;     The character is represented in a way that may be comprehensible but is
;;;     not the correct graphical form. For instance, a Hangul character
;;;     represented as a a sequence of Jamos, or a Latin transliteration of a
;;;     Cyrillic word.
;;;
;;; PANGO_COVERAGE_APPROXIMATE
;;;     The character is represented as basically the correct graphical form,
;;;     but with a stylistic variant inappropriate for the current script.
;;;
;;; PANGO_COVERAGE_EXACT
;;;     The character is represented as the correct graphical form.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_COVERAGE_LEVEL
;;;
;;; #define PANGO_TYPE_COVERAGE_LEVEL (pango_coverage_level_get_type ())
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoCoverage
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoCoverage" pango-coverage
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "pango_coverage_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'pango-coverage 'type)
 "@version{2021-1-8}
  @begin{short}
    The @sym{pango-coverage} class represents a map from Unicode characters to
    PangoCoverageLevel.
  @end{short}
  It is an opaque structure with no public fields.
  @see-symbol{pango-coverage-level}")

;;; ----------------------------------------------------------------------------
;;; pango_coverage_new ()
;;;
;;; PangoCoverage *
;;; pango_coverage_new (void);
;;;
;;; Create a new PangoCoverage
;;;
;;; Returns :
;;;     the newly allocated PangoCoverage, initialized to PANGO_COVERAGE_NONE
;;;     with a reference count of one, which should be freed with
;;;     pango_coverage_unref().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_ref ()
;;;
;;; PangoCoverage *
;;; pango_coverage_ref (PangoCoverage *coverage);
;;;
;;; Increase the reference count on the PangoCoverage by one
;;;
;;; coverage :
;;;     a PangoCoverage.
;;;
;;; Returns :
;;;     coverage .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_unref ()
;;;
;;; void
;;; pango_coverage_unref (PangoCoverage *coverage);
;;;
;;; Decrease the reference count on the PangoCoverage by one. If the result is
;;; zero, free the coverage and all associated memory.
;;;
;;; coverage :
;;;     a PangoCoverage.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_copy ()
;;;
;;; PangoCoverage *
;;; pango_coverage_copy (PangoCoverage *coverage);
;;;
;;; Copy an existing PangoCoverage. (This function may now be unnecessary since
;;; we refcount the structure. File a bug if you use it.)
;;;
;;; coverage :
;;;     a PangoCoverage
;;;
;;; Returns :
;;;     the newly allocated PangoCoverage, with a reference count of one, which
;;;     should be freed with pango_coverage_unref().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_get ()
;;;
;;; PangoCoverageLevel
;;; pango_coverage_get (PangoCoverage *coverage,
;;;                     int index_);
;;;
;;; Determine whether a particular index is covered by coverage
;;;
;;; coverage :
;;;     a PangoCoverage
;;;
;;; index_ :
;;;     the index to check
;;;
;;; Returns :
;;;     the coverage level of coverage for character index_ .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;pango_coverage_max ()
;;;void
;;;pango_coverage_max (PangoCoverage *coverage,
;;;                    PangoCoverage *other);
;;;pango_coverage_max has been deprecated since version 1.44 and should not be used in newly-written code.

;;;This function does nothing

;;;Set the coverage for each index in coverage to be the max (better) value of the current coverage for the index and the coverage for the corresponding index in other .

;;;Parameters
;;;coverage

;;;a PangoCoverage

;;;
;;;other

;;;another PangoCoverage

;;;
;;;pango_coverage_set ()
;;;void
;;;pango_coverage_set (PangoCoverage *coverage,
;;;                    int index_,
;;;                    PangoCoverageLevel level);
;;;Modify a particular index within coverage

;;;Parameters
;;;coverage

;;;a PangoCoverage

;;;
;;;index_

;;;the index to modify

;;;
;;;level

;;;the new level for index_

;;;
;;;pango_coverage_to_bytes ()
;;;void
;;;pango_coverage_to_bytes (PangoCoverage *coverage,
;;;                         guchar **bytes,
;;;                         int *n_bytes);
;;;pango_coverage_to_bytes has been deprecated since version 1.44 and should not be used in newly-written code.

;;;This returns NULL

;;;Convert a PangoCoverage structure into a flat binary format

;;;Parameters
;;;coverage

;;;a PangoCoverage

;;;
;;;bytes

;;;location to store result (must be freed with g_free()).

;;;[out][array length=n_bytes][element-type guint8]
;;;n_bytes

;;;location to store size of result.

;;;[out]
;;;pango_coverage_from_bytes ()
;;;PangoCoverage *
;;;pango_coverage_from_bytes (guchar *bytes,
;;;                           int n_bytes);
;;;pango_coverage_from_bytes has been deprecated since version 1.44 and should not be used in newly-written code.

;;;This returns NULL

;;;Convert data generated from pango_coverage_to_bytes() back to a PangoCoverage

;;;Parameters
;;;bytes

;;;binary data representing a PangoCoverage.

;;;[array length=n_bytes][element-type guint8]
;;;n_bytes

;;;the size of bytes in bytes

;;;
;;;Returns
;;;a newly allocated PangoCoverage, or NULL if the data was invalid.

;;;[transfer full][nullable]

;;; --- End of file pango.coverage.lisp ----------------------------------------
