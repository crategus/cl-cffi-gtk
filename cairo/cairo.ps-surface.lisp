;;; ----------------------------------------------------------------------------
;;; cairo.ps-surface.lisp
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
;;; PostScript Surfaces
;;;
;;;     Rendering PostScript documents
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_PS_SURFACE
;;;     cairo_ps_level_t
;;;
;;; Functions
;;;
;;;     cairo_ps_surface_create
;;;     cairo_ps_surface_create_for_stream
;;;     cairo_ps_surface_restrict_to_level
;;;     cairo_ps_get_levels
;;;     cairo_ps_level_to_string
;;;     cairo_ps_surface_set_eps
;;;     cairo_ps_surface_get_eps
;;;     cairo_ps_surface_set_size
;;;     cairo_ps_surface_dsc_begin_setup
;;;     cairo_ps_surface_dsc_begin_page_setup
;;;     cairo_ps_surface_dsc_comment
;;;
;;; Description
;;;
;;; The PostScript surface is used to render cairo graphics to Adobe PostScript
;;; files and is a multi-page vector surface backend.

;;; The following mime types are supported: CAIRO_MIME_TYPE_JPEG,
;;; CAIRO_MIME_TYPE_UNIQUE_ID, CAIRO_MIME_TYPE_CCITT_FAX,
;;; CAIRO_MIME_TYPE_CCITT_FAX_PARAMS, CAIRO_MIME_TYPE_CCITT_FAX,
;;; CAIRO_MIME_TYPE_CCITT_FAX_PARAMS, CAIRO_MIME_TYPE_EPS,
;;; CAIRO_MIME_TYPE_EPS_PARAMS.
;;;
;;; Source surfaces used by the PostScript surface that have a
;;; CAIRO_MIME_TYPE_UNIQUE_ID mime type will be stored in PostScript printer
;;; memory for the duration of the print job. CAIRO_MIME_TYPE_UNIQUE_ID should
;;; only be used for small frequently used sources.
;;;
;;; The CAIRO_MIME_TYPE_CCITT_FAX and CAIRO_MIME_TYPE_CCITT_FAX_PARAMS mime
;;; types are documented in CCITT Fax Images.
;;;
;;; Embedding EPS files
;;;
;;; Encapsulated PostScript files can be embedded in the PS output by setting
;;; the CAIRO_MIME_TYPE_EPS mime data on a surface to the EPS data and painting
;;; the surface. The EPS will be scaled and translated to the extents of the
;;; surface the EPS data is attached to.
;;;
;;; The CAIRO_MIME_TYPE_EPS mime type requires the CAIRO_MIME_TYPE_EPS_PARAMS
;;; mime data to also be provided in order to specify the embeddding parameters.
;;; CAIRO_MIME_TYPE_EPS_PARAMS mime data must contain a string of the form
;;; "bbox=[llx lly urx ury]" that specifies the bounding box (in PS coordinates)
;;; of the EPS graphics. The parameters are: lower left x, lower left y, upper
;;; right x, upper right y. Normally the bbox data is identical to the
;;; %%BoundingBox data in the EPS file.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_PS_SURFACE
;;;
;;; #define CAIRO_HAS_PS_SURFACE 1
;;;
;;; Defined if the PostScript surface backend is available. This macro can be
;;; used to conditionally compile backend-specific code.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_ps_level_t
;;;
;;; cairo_ps_level_t is used to describe the language level of the PostScript
;;; Language Reference that a generated PostScript file will conform to.
;;;
;;; CAIRO_PS_LEVEL_2 :
;;;     The language level 2 of the PostScript specification. (Since 1.6)
;;;
;;; CAIRO_PS_LEVEL_3 :
;;;     The language level 3 of the PostScript specification. (Since 1.6)
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_ps_surface_create (const char *filename,
;;;                          double width_in_points,
;;;                          double height_in_points);
;;;
;;; Creates a PostScript surface of the specified size in points to be written
;;; to filename . See cairo_ps_surface_create_for_stream() for a more flexible
;;; mechanism for handling the PostScript output than simply writing it to a
;;; named file.
;;;
;;; Note that the size of individual pages of the PostScript output can vary.
;;; See cairo_ps_surface_set_size().
;;;
;;; filename :
;;;     a filename for the PS output (must be writable), NULL may be used to
;;;     specify no output. This will generate a PS surface that may be queried
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

;;;cairo_ps_surface_create_for_stream ()
;;;cairo_surface_t *
;;;cairo_ps_surface_create_for_stream (cairo_write_func_t write_func,
;;;                                    void *closure,
;;;                                    double width_in_points,
;;;                                    double height_in_points);
;;;Creates a PostScript surface of the specified size in points to be written incrementally to the stream represented by write_func and closure . See cairo_ps_surface_create() for a more convenient way to simply direct the PostScript output to a named file.

;;;Note that the size of individual pages of the PostScript output can vary. See cairo_ps_surface_set_size().

;;;Parameters
;;;write_func

;;;a cairo_write_func_t to accept the output data, may be NULL to indicate a no-op write_func . With a no-op write_func , the surface may be queried or used as a source without generating any temporary files.

;;;closure

;;;the closure argument for write_func

;;;width_in_points

;;;width of the surface, in points (1 point == 1/72.0 inch)

;;;height_in_points

;;;height of the surface, in points (1 point == 1/72.0 inch)

;;;Returns
;;;a pointer to the newly created surface. The caller owns the surface and should call cairo_surface_destroy() when done with it.

;;;This function always returns a valid pointer, but it will return a pointer to a "nil" surface if an error such as out of memory occurs. You can use cairo_surface_status() to check for this.

;;;Since: 1.2

;;;cairo_ps_surface_restrict_to_level ()
;;;void
;;;cairo_ps_surface_restrict_to_level (cairo_surface_t *surface,
;;;                                    cairo_ps_level_t level);
;;;Restricts the generated PostSript file to level . See cairo_ps_get_levels() for a list of available level values that can be used here.

;;;This function should only be called before any drawing operations have been performed on the given surface. The simplest way to do this is to call this function immediately after creating the surface.

;;;Parameters
;;;surface

;;;a PostScript cairo_surface_t

;;;level

;;;PostScript level

;;;Since: 1.6

;;;cairo_ps_get_levels ()
;;;void
;;;cairo_ps_get_levels (cairo_ps_level_t const **levels,
;;;                     int *num_levels);
;;;Used to retrieve the list of supported levels. See cairo_ps_surface_restrict_to_level().

;;;Parameters
;;;levels

;;;supported level list

;;;num_levels

;;;list length

;;;Since: 1.6

;;;cairo_ps_level_to_string ()
;;;const char *
;;;cairo_ps_level_to_string (cairo_ps_level_t level);
;;;Get the string representation of the given level id. This function will return NULL if level id isn't valid. See cairo_ps_get_levels() for a way to get the list of valid level ids.

;;;Parameters
;;;level

;;;a level id

;;;Returns
;;;the string associated to given level.

;;;Since: 1.6

;;;cairo_ps_surface_set_eps ()
;;;void
;;;cairo_ps_surface_set_eps (cairo_surface_t *surface,
;;;                          cairo_bool_t eps);
;;;If eps is TRUE, the PostScript surface will output Encapsulated PostScript.

;;;This function should only be called before any drawing operations have been performed on the current page. The simplest way to do this is to call this function immediately after creating the surface. An Encapsulated PostScript file should never contain more than one page.

;;;Parameters
;;;surface

;;;a PostScript cairo_surface_t

;;;eps

;;;TRUE to output EPS format PostScript

;;;Since: 1.6

;;;cairo_ps_surface_get_eps ()
;;;cairo_bool_t
;;;cairo_ps_surface_get_eps (cairo_surface_t *surface);
;;;Check whether the PostScript surface will output Encapsulated PostScript.

;;;Parameters
;;;surface

;;;a PostScript cairo_surface_t

;;;Returns
;;;TRUE if the surface will output Encapsulated PostScript.

;;;Since: 1.6

;;;cairo_ps_surface_set_size ()
;;;void
;;;cairo_ps_surface_set_size (cairo_surface_t *surface,
;;;                           double width_in_points,
;;;                           double height_in_points);
;;;Changes the size of a PostScript surface for the current (and subsequent) pages.

;;;This function should only be called before any drawing operations have been performed on the current page. The simplest way to do this is to call this function immediately after creating the surface or immediately after completing a page with either cairo_show_page() or cairo_copy_page().

;;;Parameters
;;;surface

;;;a PostScript cairo_surface_t

;;;width_in_points

;;;new surface width, in points (1 point == 1/72.0 inch)

;;;height_in_points

;;;new surface height, in points (1 point == 1/72.0 inch)

;;;Since: 1.2

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_dsc_begin_setup ()
;;;
;;; void
;;; cairo_ps_surface_dsc_begin_setup (cairo_surface_t *surface);
;;;
;;; This function indicates that subsequent calls to
;;; cairo_ps_surface_dsc_comment() should direct comments to the Setup section
;;; of the PostScript output.
;;;
;;; This function should be called at most once per surface, and must be called
;;; before any call to cairo_ps_surface_dsc_begin_page_setup() and before any
;;; drawing is performed to the surface.
;;;
;;; See cairo_ps_surface_dsc_comment() for more details.
;;;
;;; surface :
;;;     a PostScript cairo_surface_t
;;;
;;; Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_dsc_begin_page_setup ()
;;;
;;; void
;;; cairo_ps_surface_dsc_begin_page_setup (cairo_surface_t *surface);
;;;
;;; This function indicates that subsequent calls to
;;; cairo_ps_surface_dsc_comment() should direct comments to the PageSetup
;;; section of the PostScript output.
;;;
;;; This function call is only needed for the first page of a surface. It should
;;; be called after any call to cairo_ps_surface_dsc_begin_setup() and before
;;; any drawing is performed to the surface.
;;;
;;; See cairo_ps_surface_dsc_comment() for more details.
;;;
;;; surface :
;;;     a PostScript cairo_surface_t
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_dsc_comment ()
;;;
;;; void
;;; cairo_ps_surface_dsc_comment (cairo_surface_t *surface,
;;;                               const char *comment);
;;;
;;; Emit a comment into the PostScript output for the given surface.
;;;
;;; The comment is expected to conform to the PostScript Language Document
;;; Structuring Conventions (DSC). Please see that manual for details on the
;;; available comments and their meanings. In particular, the %%IncludeFeature
;;; comment allows a device-independent means of controlling printer device
;;; features. So the PostScript Printer Description Files Specification will
;;; also be a useful reference.
;;;
;;; The comment string must begin with a percent character (%) and the total
;;; length of the string (including any initial percent characters) must not
;;; exceed 255 characters. Violating either of these conditions will place
;;; surface into an error state. But beyond these two conditions, this function
;;; will not enforce conformance of the comment with any particular
;;; specification.
;;;
;;; The comment string should not have a trailing newline.
;;;
;;; The DSC specifies different sections in which particular comments can
;;; appear. This function provides for comments to be emitted within three
;;; sections: the header, the Setup section, and the PageSetup section. Comments
;;; appearing in the first two sections apply to the entire document while
;;; comments in the BeginPageSetup section apply only to a single page.
;;;
;;; For comments to appear in the header section, this function should be called
;;; after the surface is created, but before a call to
;;; cairo_ps_surface_dsc_begin_setup().
;;;
;;; For comments to appear in the Setup section, this function should be called
;;; after a call to cairo_ps_surface_dsc_begin_setup() but before a call to
;;; cairo_ps_surface_dsc_begin_page_setup().
;;;
;;; For comments to appear in the PageSetup section, this function should be
;;; called after a call to cairo_ps_surface_dsc_begin_page_setup().
;;;
;;; Note that it is only necessary to call
;;; cairo_ps_surface_dsc_begin_page_setup() for the first page of any surface.
;;; After a call to cairo_show_page() or cairo_copy_page() comments are
;;; unambiguously directed to the PageSetup section of the current page. But it
;;; doesn't hurt to call this function at the beginning of every page as that
;;; consistency may make the calling code simpler.
;;;
;;; As a final note, cairo automatically generates several comments on its own.
;;; As such, applications must not manually generate any of the following
;;; comments:
;;;
;;; Header section: %!PS-Adobe-3.0, %%Creator, %%CreationDate, %%Pages,
;;; %%BoundingBox, %%DocumentData, %%LanguageLevel, %%EndComments.
;;;
;;; Setup section: %%BeginSetup, %%EndSetup
;;;
;;; PageSetup section: %%BeginPageSetup, %%PageBoundingBox, %%EndPageSetup.
;;;
;;; Other sections: %%BeginProlog, %%EndProlog, %%Page, %%Trailer, %%EOF
;;;
;;; Here is an example sequence showing how this function might be used:
;;;
;;; cairo_surface_t *surface = cairo_ps_surface_create (filename, width, height)
;;; ...
;;; cairo_ps_surface_dsc_comment (surface, "%%Title: My excellent document");
;;; cairo_ps_surface_dsc_comment (surface, "%%Copyright: Copyright (C) 2006 Cairo Lover")
;;; ...
;;; cairo_ps_surface_dsc_begin_setup (surface);
;;; cairo_ps_surface_dsc_comment (surface, "%%IncludeFeature: *MediaColor White");
;;; ...
;;; cairo_ps_surface_dsc_begin_page_setup (surface);
;;; cairo_ps_surface_dsc_comment (surface, "%%IncludeFeature: *PageSize A3");
;;; cairo_ps_surface_dsc_comment (surface, "%%IncludeFeature: *InputSlot LargeCapacity");
;;; cairo_ps_surface_dsc_comment (surface, "%%IncludeFeature: *MediaType Glossy");
;;; cairo_ps_surface_dsc_comment (surface, "%%IncludeFeature: *MediaColor Blue");
;;; ... draw to first page here ..
;;; cairo_show_page (cr);
;;; ...
;;; cairo_ps_surface_dsc_comment (surface, "%%IncludeFeature: *PageSize A5");
;;; ...
;;;
;;; surface :
;;;     a PostScript cairo_surface_t
;;;
;;; comment :
;;;     a comment string to be emitted into the PostScript output
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.ps-surface.lisp --------------------------------------
