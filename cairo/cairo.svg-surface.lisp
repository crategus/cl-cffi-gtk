;;; ----------------------------------------------------------------------------
;;; cairo.svg-surface.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 Dieter Kaiser
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
;;; SVG Surfaces
;;;
;;;     Rendering SVG documents
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_SVG_SURFACE
;;;     cairo_svg_version_t
;;;     cairo_svg_unit_t
;;;
;;; Functions
;;;
;;;     cairo_svg_surface_create
;;;     cairo_svg_surface_create_for_stream
;;;     cairo_svg_surface_get_document_unit
;;;     cairo_svg_surface_set_document_unit
;;;     cairo_svg_surface_restrict_to_version
;;;     cairo_svg_get_versions
;;;     cairo_svg_version_to_string
;;;
;;; Description
;;;
;;; The SVG surface is used to render cairo graphics to SVG files and is a
;;; multi-page vector surface backend.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_SVG_SURFACE
;;;
;;; #define CAIRO_HAS_SVG_SURFACE 1
;;;
;;; Defined if the SVG surface backend is available. This macro can be used to
;;; conditionally compile backend-specific code.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_svg_version_t
;;;
;;; cairo_svg_version_t is used to describe the version number of the SVG
;;; specification that a generated SVG file will conform to.
;;;
;;; CAIRO_SVG_VERSION_1_1
;;;     The version 1.1 of the SVG specification. (Since 1.2)
;;;
;;; CAIRO_SVG_VERSION_1_2
;;;     The version 1.2 of the SVG specification. (Since 1.2)
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_svg_unit_t
;;;
;;; CAIRO_SVG_UNIT_USER :
;;;     User unit, a value in the current coordinate system. If used in the root
;;;     element for the initial coordinate systems it corresponds to pixels.
;;;     (Since 1.16)
;;; CAIRO_SVG_UNIT_EM : The size of the element's font. (Since 1.16)
;;; CAIRO_SVG_UNIT_EX : The x-height of the element’s font. (Since 1.16)
;;; CAIRO_SVG_UNIT_PX : Pixels (1px = 1/96th of 1in). (Since 1.16)
;;; CAIRO_SVG_UNIT_IN : Inches (1in = 2.54cm = 96px). (Since 1.16)
;;; CAIRO_SVG_UNIT_CM : Centimeters (1cm = 96px/2.54). (Since 1.16)
;;; CAIRO_SVG_UNIT_MM : Millimeters (1mm = 1/10th of 1cm). (Since 1.16)
;;; CAIRO_SVG_UNIT_PT : Points (1pt = 1/72th of 1in). (Since 1.16)
;;; CAIRO_SVG_UNIT_PC : Picas (1pc = 1/6th of 1in). (Since 1.16)
;;; CAIRO_SVG_UNIT_PERCENT :
;;;     Percent, a value that is some fraction of another reference value.
;;;     (Since 1.16)
;;;
;;; cairo_svg_unit_t is used to describe the units valid for coordinates and
;;; lengths in the SVG specification.
;;;
;;; See also:
;;; https://www.w3.org/TR/SVG/coords.htmlUnits https://www.w3.org/TR/SVG/types.htmlDataTypeLength https://www.w3.org/TR/css-values-3/lengths
;;;
;;; CAIRO_SVG_UNIT_USER
;;; CAIRO_SVG_UNIT_EM
;;; CAIRO_SVG_UNIT_EX
;;; CAIRO_SVG_UNIT_PX
;;; CAIRO_SVG_UNIT_IN
;;; CAIRO_SVG_UNIT_CM
;;; CAIRO_SVG_UNIT_MM
;;; CAIRO_SVG_UNIT_PT
;;; CAIRO_SVG_UNIT_PC
;;; CAIRO_SVG_UNIT_PERCENT
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_svg_surface_create (const char *filename,
;;;                           double width_in_points,
;;;                           double height_in_points);
;;;
;;; Creates a SVG surface of the specified size in points to be written to
;;; filename .
;;;
;;; The SVG surface backend recognizes the following MIME types for the data
;;; attached to a surface (see cairo_surface_set_mime_data()) when it is used
;;; as a source pattern for drawing on this surface: CAIRO_MIME_TYPE_JPEG,
;;; CAIRO_MIME_TYPE_PNG, CAIRO_MIME_TYPE_URI. If any of them is specified, the
;;; SVG backend emits a href with the content of MIME data instead of a surface
;;; snapshot (PNG, Base64-encoded) in the corresponding image tag.
;;;
;;; The unofficial MIME type CAIRO_MIME_TYPE_URI is examined first. If present,
;;; the URI is emitted as is: assuring the correctness of URI is left to the
;;; client code.
;;;
;;; If CAIRO_MIME_TYPE_URI is not present, but CAIRO_MIME_TYPE_JPEG or
;;; CAIRO_MIME_TYPE_PNG is specified, the corresponding data is Base64-encoded
;;; and emitted.
;;;
;;; If CAIRO_MIME_TYPE_UNIQUE_ID is present, all surfaces with the same unique
;;; identifier will only be embedded once.
;;;
;;; filename :
;;;     a filename for the SVG output (must be writable), NULL may be used to
;;;     specify no output. This will generate a SVG surface that may be queried
;;;     and used as a source, without generating a temporary file.
;;;
;;; width_in_points :
;;;     width of the surface, in points (1 point == 1/72.0 inch)
;;;
;;; height_in_points :
;;;     height of the surface, in points (1 point == 1/72.0 inch)
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" surface if an error such as out of memory occurs.
;;;     You can use cairo_surface_status() to check for this.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_svg_surface_create" %cairo-svg-surface-create)
    (:pointer (:struct cairo-surface-t))
  (filename :string)
  (width-in-points :double)
  (height-in-points :double))

(defun cairo-svg-surface-create (filename width-in-points height-in-points)
  (%cairo-svg-surface-create filename
                             (coerce width-in-points 'double-float)
                             (coerce height-in-points 'double-float)))

(export 'cairo-svg-surface-create)

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_create_for_stream ()
;;;
;;; cairo_surface_t *
;;; cairo_svg_surface_create_for_stream (cairo_write_func_t write_func,
;;;                                      void *closure,
;;;                                      double width_in_points,
;;;                                      double height_in_points);
;;;
;;; Creates a SVG surface of the specified size in points to be written
;;; incrementally to the stream represented by write_func and closure .
;;;
;;; write_func :
;;;     a cairo_write_func_t to accept the output data, may be NULL to indicate
;;;     a no-op write_func . With a no-op write_func , the surface may be
;;;     queried or used as a source without generating any temporary files.
;;;
;;; closure :
;;;     the closure argument for write_func
;;;
;;; width_in_points :
;;;     width of the surface, in points (1 point == 1/72.0 inch)
;;;
;;; height_in_points :
;;;     height of the surface, in points (1 point == 1/72.0 inch)
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" surface if an error such as out of memory occurs.
;;;     You can use cairo_surface_status() to check for this.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_get_document_unit ()
;;;
;;; cairo_svg_unit_t
;;; cairo_svg_surface_get_document_unit (cairo_surface_t *surface);
;;;
;;; Get the unit of the SVG surface.
;;;
;;; If the surface passed as an argument is not a SVG surface, the function sets
;;; the error status to CAIRO_STATUS_SURFACE_TYPE_MISMATCH and returns
;;; CAIRO_SVG_UNIT_USER.
;;;
;;; surface :
;;;     a SVG cairo_surface_t
;;;
;;; Returns :
;;;     the SVG unit of the SVG surface.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_set_document_unit ()
;;;
;;; void
;;; cairo_svg_surface_set_document_unit (cairo_surface_t *surface,
;;;                                      cairo_svg_unit_t unit);
;;;
;;; Use the specified unit for the width and height of the generated SVG file.
;;; See cairo_svg_unit_t for a list of available unit values that can be used
;;; here.
;;;
;;; This function can be called at any time before generating the SVG file.
;;;
;;; However to minimize the risk of ambiguities it's recommended to call it
;;; before any drawing operations have been performed on the given surface, to
;;; make it clearer what the unit used in the drawing operations is.
;;;
;;; The simplest way to do this is to call this function immediately after
;;; creating the SVG surface.
;;;
;;; Note if this function is never called, the default unit for SVG documents
;;; generated by cairo will be "pt". This is for historical reasons.
;;;
;;; surface :
;;;     a SVG cairo_surface_t
;;;
;;; unit :
;;;     SVG unit
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_restrict_to_version ()
;;;
;;; void
;;; cairo_svg_surface_restrict_to_version (cairo_surface_t *surface,
;;;                                        cairo_svg_version_t version);
;;;
;;; Restricts the generated SVG file to version . See cairo_svg_get_versions()
;;; for a list of available version values that can be used here.
;;;
;;; This function should only be called before any drawing operations have been
;;; performed on the given surface. The simplest way to do this is to call this
;;; function immediately after creating the surface.
;;;
;;; surface :
;;;     a SVG cairo_surface_t
;;;
;;; version :
;;;     SVG version
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_svg_get_versions ()
;;;
;;; void
;;; cairo_svg_get_versions (cairo_svg_version_t const **versions,
;;;                         int *num_versions);
;;;
;;; Used to retrieve the list of supported versions. See
;;; cairo_svg_surface_restrict_to_version().
;;;
;;; versions :
;;;     supported version list
;;;
;;; num_versions :
;;;     list length
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_svg_version_to_string ()
;;;
;;; const char *
;;; cairo_svg_version_to_string (cairo_svg_version_t version);
;;;
;;; Get the string representation of the given version id. This function will
;;; return NULL if version isn't valid. See cairo_svg_get_versions() for a way
;;; to get the list of valid version ids.
;;;
;;; version :
;;;     a version id
;;;
;;; Returns :
;;;     the string associated to given version.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.svg-surface.lisp -------------------------------------
