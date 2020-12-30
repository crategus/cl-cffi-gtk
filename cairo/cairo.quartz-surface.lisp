;;; ----------------------------------------------------------------------------
;;; cairo.quartz-surface.lisp
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
;;; Quartz Surfaces
;;;
;;;     Rendering to Quartz surfaces
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_QUARTZ_SURFACE
;;;
;;; Functions
;;;
;;;     cairo_quartz_surface_create
;;;     cairo_quartz_surface_create_for_cg_context
;;;     cairo_quartz_surface_get_cg_context
;;;
;;; Description
;;;
;;; The Quartz surface is used to render cairo graphics targeting the Apple OS X
;;; Quartz rendering system.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_QUARTZ_SURFACE
;;;
;;; #define CAIRO_HAS_QUARTZ_SURFACE 1
;;;
;;; Defined if the Quartz surface backend is available. This macro can be used
;;; to conditionally compile backend-specific code.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_quartz_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_quartz_surface_create (cairo_format_t format,
;;;                              unsigned int width,
;;;                              unsigned int height);
;;;
;;; Creates a Quartz surface backed by a CGBitmap. The surface is created using
;;; the Device RGB (or Device Gray, for A8) color space. All Cairo operations,
;;; including those that require software rendering, will succeed on this
;;; surface.
;;;
;;; format :
;;;     format of pixels in the surface to create
;;;
;;; width :
;;;     width of the surface, in pixels
;;;
;;; height :
;;;     height of the surface, in pixels
;;;
;;; Returns :
;;;     the newly created surface.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_quartz_surface_create_for_cg_context ()
;;;
;;; cairo_surface_t *
;;; cairo_quartz_surface_create_for_cg_context
;;;                                (CGContextRef cgContext,
;;;                                 unsigned int width,
;;;                                 unsigned int height);
;;;
;;; Creates a Quartz surface that wraps the given CGContext. The CGContext is
;;; assumed to be in the standard Cairo coordinate space (that is, with the
;;; origin at the upper left and the Y axis increasing downward). If the
;;; CGContext is in the Quartz coordinate space (with the origin at the bottom
;;; left), then it should be flipped before this function is called. The flip
;;; can be accomplished using a translate and a scale; for example:
;;;
;;; CGContextTranslateCTM (cgContext, 0.0, height);
;;; CGContextScaleCTM (cgContext, 1.0, -1.0);
;;;
;;; All Cairo operations are implemented in terms of Quartz operations, as long
;;; as Quartz-compatible elements are used (such as Quartz fonts).
;;;
;;; cgContext :
;;;     the existing CGContext for which to create the surface
;;;
;;; width :
;;;     width of the surface, in pixels
;;;
;;; height :
;;;     height of the surface, in pixels
;;;
;;; Returns :
;;;     the newly created Cairo surface.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_quartz_surface_get_cg_context ()
;;;
;;; CGContextRef
;;; cairo_quartz_surface_get_cg_context (cairo_surface_t *surface);
;;;
;;; Returns the CGContextRef that the given Quartz surface is backed by.
;;;
;;; A call to cairo_surface_flush() is required before using the CGContextRef
;;; to ensure that all pending drawing operations are finished and to restore
;;; any temporary modification cairo has made to its state. A call to
;;; cairo_surface_mark_dirty() is required after the state or the content of
;;; the CGContextRef has been modified.
;;;
;;; surface :
;;;     the Cairo Quartz surface
;;;
;;; Returns :
;;;     the CGContextRef for the given surface.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; End of file cairo.quartz-surface.lisp --------------------------------------
