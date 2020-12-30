;;; ----------------------------------------------------------------------------
;;; cairo.recording-surface.lisp
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
;;; Recording Surfaces
;;;
;;;     Records all drawing operations
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_RECORDING_SURFACE
;;;
;;; Functions
;;;
;;;     cairo_recording_surface_create ()
;;;     cairo_recording_surface_ink_extents ()
;;;     cairo_recording_surface_get_extents ()
;;;
;;; Description
;;;
;;; A recording surface is a surface that records all drawing operations at the
;;; highest level of the surface backend interface, (that is, the level of
;;; paint, mask, stroke, fill, and show_text_glyphs). The recording surface can
;;; then be "replayed" against any target surface by using it as a source
;;; surface.
;;;
;;; If you want to replay a surface so that the results in target will be
;;; identical to the results that would have been obtained if the original
;;; operations applied to the recording surface had instead been applied to the
;;; target surface, you can use code like this:
;;;
;;; cairo_t *cr;
;;;
;;; cr = cairo_create (target);
;;; cairo_set_source_surface (cr, recording_surface, 0.0, 0.0);
;;; cairo_paint (cr);
;;; cairo_destroy (cr);
;;;
;;; A recording surface is logically unbounded, i.e. it has no implicit
;;; constraint on the size of the drawing surface. However, in practice this is
;;; rarely useful as you wish to replay against a particular target surface with
;;; known bounds. For this case, it is more efficient to specify the target
;;; extents to the recording surface upon creation.
;;;
;;; The recording phase of the recording surface is careful to snapshot all
;;; necessary objects (paths, patterns, etc.), in order to achieve accurate
;;; replay. The efficiency of the recording surface could be improved by
;;; improving the implementation of snapshot for the various objects. For
;;; example, it would be nice to have a copy-on-write implementation for
;;; _cairo_surface_snapshot.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_RECORDING_SURFACE
;;;
;;; #define CAIRO_HAS_RECORDING_SURFACE 1
;;;
;;; Defined if the recording surface backend is available. The recording surface
;;; backend is always built in. This macro was added for completeness in cairo
;;; 1.10.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_recording_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_recording_surface_create (cairo_content_t content,
;;;                                 const cairo_rectangle_t *extents);
;;;
;;; Creates a recording-surface which can be used to record all drawing
;;; operations at the highest level (that is, the level of paint, mask, stroke,
;;; fill and show_text_glyphs). The recording surface can then be "replayed"
;;; against any target surface by using it as a source to drawing operations.
;;;
;;; The recording phase of the recording surface is careful to snapshot all
;;; necessary objects (paths, patterns, etc.), in order to achieve accurate
;;; replay.
;;;
;;; content :
;;;     the content of the recording surface
;;;
;;; extents :
;;; the extents to record in pixels, can be NULL to record unbounded operations.
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_recording_surface_ink_extents ()
;;;
;;; void
;;; cairo_recording_surface_ink_extents (cairo_surface_t *surface,
;;;                                      double *x0,
;;;                                      double *y0,
;;;                                      double *width,
;;;                                      double *height);
;;;
;;; Measures the extents of the operations stored within the recording-surface.
;;; This is useful to compute the required size of an image surface (or
;;; equivalent) into which to replay the full sequence of drawing operations.
;;;
;;; surface :
;;;     a cairo_recording_surface_t
;;;
;;; x0 :
;;;     the x-coordinate of the top-left of the ink bounding box
;;;
;;; y0 :
;;;     the y-coordinate of the top-left of the ink bounding box
;;;
;;; width :
;;;     the width of the ink bounding box
;;;
;;; height :
;;;     the height of the ink bounding box
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_recording_surface_get_extents ()
;;;
;;; cairo_bool_t
;;; cairo_recording_surface_get_extents (cairo_surface_t *surface,
;;;                                      cairo_rectangle_t *extents);
;;;
;;; Get the extents of the recording-surface.
;;;
;;; surface :
;;;     a cairo_recording_surface_t
;;;
;;; extents :
;;;     the cairo_rectangle_t to be assigned the extents
;;;
;;; Returns :
;;;     TRUE if the surface is bounded, of recording type, and not in an error
;;;     state, otherwise FALSE
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.recording-surface.lisp -------------------------------
