;;; ----------------------------------------------------------------------------
;;; cairo.pdf-surface.lisp
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
;;; PDF Surfaces
;;;
;;;     Rendering PDF documents
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_PDF_SURFACE
;;;     CAIRO_PDF_OUTLINE_ROOT
;;;     cairo_pdf_outline_flags_t
;;;     cairo_pdf_metadata_t
;;;     cairo_pdf_version_t
;;;
;;; Functions
;;;
;;;     cairo_pdf_surface_create ()
;;;     cairo_pdf_surface_create_for_stream ()
;;;     cairo_pdf_surface_restrict_to_version ()
;;;     cairo_pdf_get_versions ()
;;;     cairo_pdf_version_to_string ()
;;;     cairo_pdf_surface_set_size ()
;;;     cairo_pdf_surface_add_outline ()
;;;     cairo_pdf_surface_set_metadata ()
;;;     cairo_pdf_surface_set_page_label ()
;;;     cairo_pdf_surface_set_thumbnail_size ()
;;;
;;; Description
;;;
;;; The PDF surface is used to render cairo graphics to Adobe PDF files and is
;;; a multi-page vector surface backend.
;;;
;;; The following mime types are supported: CAIRO_MIME_TYPE_JPEG,
;;; CAIRO_MIME_TYPE_JP2, CAIRO_MIME_TYPE_UNIQUE_ID, CAIRO_MIME_TYPE_JBIG2,
;;; CAIRO_MIME_TYPE_JBIG2_GLOBAL, CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID,
;;; CAIRO_MIME_TYPE_CCITT_FAX, CAIRO_MIME_TYPE_CCITT_FAX_PARAMS.
;;;
;;; JBIG2 Images
;;;
;;; JBIG2 data in PDF must be in the embedded format as described in ISO/IEC
;;; 11544. Image specific JBIG2 data must be in CAIRO_MIME_TYPE_JBIG2. Any
;;; global segments in the JBIG2 data (segments with page association field set
;;; to 0) must be in CAIRO_MIME_TYPE_JBIG2_GLOBAL. The global data may be shared
;;; by multiple images. All images sharing the same global data must set
;;; CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID to a unique identifier. At least one of the
;;; images must provide the global data using CAIRO_MIME_TYPE_JBIG2_GLOBAL. The
;;; global data will only be embedded once and shared by all JBIG2 images with
;;; the same CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID.
;;;
;;; CCITT Fax Images
;;;
;;; The CAIRO_MIME_TYPE_CCITT_FAX mime data requires a number of decoding
;;; parameters These parameters are specified using
;;; CAIRO_MIME_TYPE_CCITT_FAX_PARAMS.
;;;
;;; CAIRO_MIME_TYPE_CCITT_FAX_PARAMS mime data must contain a string of the form
;;; "param1=value1 param2=value2 ...".
;;;
;;; Columns : [required] An integer specifying the width of the image in pixels.
;;;
;;; Rows : [required] An integer specifying the height of the image in scan
;;; lines.
;;;
;;; K : [optional] An integer identifying the encoding scheme used. < 0 is 2
;;; dimensional Group 4, = 0 is Group3 1 dimensional, > 0 is mixed 1 and 2
;;; dimensional encoding. Default is 0.
;;;
;;; EndOfLine : [optional] If true end-of-line bit patterns are present. Default
;;; is false.
;;;
;;; EncodedByteAlign : [optional] If true the end of line is padded with 0 bits
;;; so the next line begins on a byte boundary. Default is false.
;;;
;;; EndOfBlock : [optional] If true the data contains an end-of-block pattern.
;;; Default is true.
;;;
;;; BlackIs1 : [optional] If true 1 bits are black pixels. Default is false.
;;;
;;; DamagedRowsBeforeError : [optional] An integer specifying the number of
;;; damages rows tolerated before an error occurs. Default is 0.
;;;
;;; Boolean values may be "true" or "false", or 1 or 0.
;;;
;;; These parameters are the same as the CCITTFaxDecode parameters in the
;;; PostScript Language Reference and Portable Document Format (PDF). Refer to
;;; these documents for further details.
;;;
;;; An example CAIRO_MIME_TYPE_CCITT_FAX_PARAMS string is:
;;;
;;; "Columns=10230 Rows=40000 K=1 EndOfLine=true EncodedByteAlign=1
;;; BlackIs1=false"
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_PDF_SURFACE
;;;
;;; #define CAIRO_HAS_PDF_SURFACE 1
;;;
;;; Defined if the PDF surface backend is available. This macro can be used to
;;; conditionally compile backend-specific code.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_PDF_OUTLINE_ROOT
;;;
;;; #define CAIRO_PDF_OUTLINE_ROOT 0
;;;
;;; The root outline item in cairo_pdf_surface_add_outline().
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_pdf_outline_flags_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-pdf-outline-flags-t
  (:open 1)
  (:bold 2)
  (:italic 4))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-pdf-outline-flags-t atdoc:*symbol-name-alias*)
      "CFlags"
      (gethash 'cairo-pdf-outline-flags-t atdoc:*external-symbols*)
 "@version{2020-12-17}
  @begin{short}
    The @sym{cairo-pdf-outline-flags-t} flags is used by the function
    @fun{cairo-pdf-surface-add-outline} to specify the attributes of an outline
    item.
  @end{short}
  These flags may be bitwise-or'd to produce any combination of flags.
  @begin{pre}
(defcenum cairo-pdf-outline-flags-t
  (:open 1)
  (:bold 2)
  (:italic 4))
  @end{pre}
  @begin[code]{table}
    @entry[:open]{The outline item defaults to open in the PDF viewer.}
    @entry[:bold]{The outline item is displayed by the viewer in bold text.}
    @entry[:italic]{The outline item is displayed by the viewer in italic text.}
  @end{table}
  @see-function{cairo-pdf-surface-add-outline}")

(export 'cairo-pdf-outline-flags-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_pdf_metadata_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-pdf-metadata-t
  :title
  :author
  :subject
  :keywords
  :creator
  :create-date
  :mod-date)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-pdf-metadata-t atdoc:*symbol-name-alias*)
      "CEnum"
      (gethash 'cairo-pdf-metadata-t atdoc:*external-symbols*)
 "@version{2020-12-17}
  @begin{short}
    The @sym{cairo-pdf-metadata-t} enumeration is used by the function
    @fun{cairo-pdf-surface-set-metadata} to specify the metadata to set.
  @end{short}
  @begin[code]{table}
    @entry[:title]{The document title.}
    @entry[:author]{The document author.}
    @entry[:subject]{The document subject.}
    @entry[:keywords]{The document keywords.}
    @entry[:creator]{The document creator.}
    @entry[:create-date]{The document creation date.}
    @entry[:mod-date]{The document modification date.}
  @end{table}
  @see-function{cairo-pdf-surface-set-metadata}")

(export 'cairo-pdf-metadata-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_pdf_version_t
;;;
;;; cairo_pdf_version_t is used to describe the version number of the PDF
;;; specification that a generated PDF file will conform to.
;;;
;;; CAIRO_PDF_VERSION_1_4
;;;     The version 1.4 of the PDF specification. (Since 1.10)
;;;
;;; CAIRO_PDF_VERSION_1_5
;;;     The version 1.5 of the PDF specification. (Since 1.10)
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pdf_surface_create" %cairo-pdf-surface-create)
    (:pointer (:struct cairo-surface-t))
  (filename :string)
  (width-in-points :double)
  (height-in-points :double))

(defun cairo-pdf-surface-create (filename width-in-points height-in-points)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-27}
  @argument[filename]{a string with a filename for the PDF output (must be
    writable), @code{nil} may be used to specify no output, this will generate
    a PDF surface that may be queried and used as a source, without generating
    a temporary file}
  @argument[width-in-points]{a double float with the width of the surface,
    in points (1 point == 1/72.0 inch)}
  @argument[height-in-points]{a double float with the height of the surface,
    in points (1 point == 1/72.0 inch)}
  @begin{return}
    A pointer to the newly created surface. The caller owns the surface and
    should call the function @fun{cairo-surface-destroy} when done with it.

    This function always returns a valid pointer, but it will return a pointer
    to a \"nil\" surface if an error such as out of memory occurs. You can use
    the function @fun{cairo-surface-status} to check for this.
  @end{return}
  @begin{short}
    Creates a PDF surface of the specified size in points to be written to
    @arg{filename}.
  @end{short}
  @see-symbol{cairo-surface-t}
  @see-function{cairo-surface-destroy}
  @see-function{cairo-surface-status}"
  (%cairo-pdf-surface-create filename
                             (coerce width-in-points 'double-float)
                             (coerce height-in-points 'double-float)))

(export 'cairo-pdf-surface-create)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_create_for_stream ()
;;;
;;; cairo_surface_t *
;;; cairo_pdf_surface_create_for_stream (cairo_write_func_t write_func,
;;;                                      void *closure,
;;;                                      double width_in_points,
;;;                                      double height_in_points);
;;;
;;; Creates a PDF surface of the specified size in points to be written
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
;;; cairo_pdf_surface_restrict_to_version ()
;;;
;;; void
;;; cairo_pdf_surface_restrict_to_version (cairo_surface_t *surface,
;;;                                        cairo_pdf_version_t version);
;;;
;;; Restricts the generated PDF file to version . See cairo_pdf_get_versions()
;;; for a list of available version values that can be used here.
;;;
;;; This function should only be called before any drawing operations have been
;;; performed on the given surface. The simplest way to do this is to call this
;;; function immediately after creating the surface.
;;;
;;; surface :
;;;     a PDF cairo_surface_t
;;;
;;; version :
;;;     PDF version
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_get_versions ()
;;;
;;; void
;;; cairo_pdf_get_versions (cairo_pdf_version_t const **versions,
;;;                         int *num_versions);
;;;
;;; Used to retrieve the list of supported versions. See
;;; cairo_pdf_surface_restrict_to_version().
;;;
;;; versions :
;;;     supported version list
;;;
;;; num_versions :
;;;     list length
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_version_to_string ()
;;;
;;; const char *
;;; cairo_pdf_version_to_string (cairo_pdf_version_t version);
;;;
;;; Get the string representation of the given version id. This function will
;;; return NULL if version isn't valid. See cairo_pdf_get_versions() for a way
;;; to get the list of valid version ids.
;;;
;;; version :
;;;     a version id
;;;
;;; Returns :
;;;     the string associated to given version.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_set_size ()
;;;
;;; void
;;; cairo_pdf_surface_set_size (cairo_surface_t *surface,
;;;                             double width_in_points,
;;;                             double height_in_points);
;;;
;;; Changes the size of a PDF surface for the current (and subsequent) pages.
;;;
;;; This function should only be called before any drawing operations have been
;;; performed on the current page. The simplest way to do this is to call this
;;; function immediately after creating the surface or immediately after
;;; completing a page with either cairo_show_page() or cairo_copy_page().
;;;
;;; surface :
;;;     a PDF cairo_surface_t
;;;
;;; width_in_points :
;;;     new surface width, in points (1 point == 1/72.0 inch)
;;;
;;; height_in_points :
;;;     new surface height, in points (1 point == 1/72.0 inch)
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_add_outline ()
;;;
;;; int
;;; cairo_pdf_surface_add_outline (cairo_surface_t *surface,
;;;                                int parent_id,
;;;                                const char *utf8,
;;;                                const char *link_attribs,
;;;                                cairo_pdf_outline_flags_t flags);
;;;
;;; Add an item to the document outline hierarchy with the name utf8 that links
;;; to the location specified by link_attribs . Link attributes have the same
;;; keys and values as the Link Tag, excluding the "rect" attribute. The item
;;; will be a child of the item with id parent_id . Use CAIRO_PDF_OUTLINE_ROOT
;;; as the parent id of top level items.
;;;
;;; surface :
;;;     a PDF cairo_surface_t
;;;
;;; parent_id :
;;;     the id of the parent item or CAIRO_PDF_OUTLINE_ROOT if this is a top
;;;     level item.
;;;
;;; utf8 :
;;;     the name of the outline
;;;
;;; link_attribs :
;;;     the link attributes specifying where this outline links to
;;;
;;; flags :
;;;     outline item flags
;;;
;;; Returns :
;;;     the id for the added item.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_set_metadata ()
;;;
;;; void
;;; cairo_pdf_surface_set_metadata (cairo_surface_t *surface,
;;;                                 cairo_pdf_metadata_t metadata,
;;;                                 const char *utf8);
;;;
;;; Set document metadata. The CAIRO_PDF_METADATA_CREATE_DATE and
;;; CAIRO_PDF_METADATA_MOD_DATE values must be in ISO-8601 format:
;;; YYYY-MM-DDThh:mm:ss. An optional timezone of the form "[+/-]hh:mm" or "Z"
;;; for UTC time can be appended. All other metadata values can be any UTF-8
;;; string.
;;;
;;; For example:
;;;
;;; cairo_pdf_surface_set_metadata (surface,
;;;                                 CAIRO_PDF_METADATA_TITLE, "My Document");
;;; cairo_pdf_surface_set_metadata (surface,
;;;                                 CAIRO_PDF_METADATA_CREATE_DATE,
;;;                                 "2015-12-31T23:59+02:00");
;;;
;;; surface :
;;;     a PDF cairo_surface_t
;;;
;;; metadata :
;;;     The metadata item to set.
;;;
;;; utf8 :
;;;     metadata value
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_set_page_label ()
;;;
;;; void
;;; cairo_pdf_surface_set_page_label (cairo_surface_t *surface,
;;;                                   const char *utf8);
;;;
;;; Set page label for the current page.
;;;
;;; surface :
;;;     a PDF cairo_surface_t
;;;
;;; utf8 :
;;;     The page label.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_set_thumbnail_size ()
;;;
;;; void
;;; cairo_pdf_surface_set_thumbnail_size (cairo_surface_t *surface,
;;;                                       int width,
;;;                                       int height);
;;;
;;; Set the thumbnail image size for the current and all subsequent pages.
;;; Setting a width or height of 0 disables thumbnails for the current and
;;; subsequent pages.
;;;
;;; surface :
;;;     a PDF cairo_surface_t
;;;
;;; width :
;;;     Thumbnail width.
;;;
;;; height :
;;;     Thumbnail height
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.pdf-surface.lisp -------------------------------------
