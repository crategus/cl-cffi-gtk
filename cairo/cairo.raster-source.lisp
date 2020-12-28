;;; ----------------------------------------------------------------------------
;;; cairo.raster-source.lisp
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
;;; Raster Sources
;;;
;;;     Supplying arbitrary image data
;;;
;;; Functions
;;;
;;;     cairo_pattern_create_raster_source
;;;     cairo_raster_source_pattern_set_callback_data
;;;     cairo_raster_source_pattern_get_callback_data
;;;     cairo_raster_source_pattern_set_acquire
;;;     cairo_raster_source_pattern_get_acquire
;;;     cairo_raster_source_pattern_set_snapshot
;;;     cairo_raster_source_pattern_get_snapshot
;;;     cairo_raster_source_pattern_set_copy
;;;     cairo_raster_source_pattern_get_copy
;;;     cairo_raster_source_pattern_set_finish
;;;     cairo_raster_source_pattern_get_finish
;;;
;;;     cairo_raster_source_acquire_func_t
;;;     cairo_raster_source_release_func_t
;;;     cairo_raster_source_snapshot_func_t
;;;     cairo_raster_source_copy_func_t
;;;     cairo_raster_source_finish_func_t
;;;
;;; Description
;;;
;;;     The raster source provides the ability to supply arbitrary pixel data
;;;     whilst rendering. The pixels are queried at the time of rasterisation by
;;;     means of user callback functions, allowing for the ultimate flexibility.
;;;     For example, in handling compressed image sources, you may keep a MRU
;;;     cache of decompressed images and decompress sources on the fly and
;;;     discard old ones to conserve memory.
;;;
;;;     For the raster source to be effective, you must at least specify the
;;;     acquire and release callbacks which are used to retrieve the pixel data
;;;     for the region of interest and demark when it can be freed afterwards.
;;;     Other callbacks are provided for when the pattern is copied temporarily
;;;     during rasterisation, or more permanently as a snapshot in order to keep
;;;     the pixel data available for printing.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;;cairo_pattern_create_raster_source ()
;;;
;;; cairo_pattern_t *
;;; cairo_pattern_create_raster_source (void *user_data,
;;;                                     cairo_content_t content,
;;;                                     int width,
;;;                                     int height);
;;;
;;; Creates a new user pattern for providing pixel data.
;;;
;;; Use the setter functions to associate callbacks with the returned pattern.
;;; The only mandatory callback is acquire.
;;;
;;; user_data :
;;;     the user data to be passed to all callbacks
;;;
;;; content :
;;;     content type for the pixel data that will be returned. Knowing the
;;;     content type ahead of time is used for analysing the operation and
;;;     picking the appropriate rendering path.
;;;
;;; width :
;;;     maximum size of the sample area
;;;
;;; height :
;;;     maximum size of the sample area
;;;
;;; Returns :
;;;     a newly created cairo_pattern_t. Free with cairo_pattern_destroy() when
;;;     you are done using it.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_pattern_set_callback_data ()
;;;
;;; void
;;; cairo_raster_source_pattern_set_callback_data (cairo_pattern_t *pattern,
;;;                                                void *data);
;;;
;;; Updates the user data that is provided to all callbacks.
;;;
;;; pattern :
;;;     the pattern to update
;;;
;;; data :
;;;     the user data to be passed to all callbacks
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_pattern_get_callback_data ()
;;;
;;; void *
;;; cairo_raster_source_pattern_get_callback_data (cairo_pattern_t *pattern);
;;;
;;; Queries the current user data.
;;;
;;; pattern :
;;;     the pattern to update
;;;
;;; Returns :
;;;     the current user-data passed to each callback
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_pattern_set_acquire ()
;;;
;;; void
;;; cairo_raster_source_pattern_set_acquire
;;;                                (cairo_pattern_t *pattern,
;;;                                 cairo_raster_source_acquire_func_t acquire,
;;;                                 cairo_raster_source_release_func_t release)
;;;
;;; Specifies the callbacks used to generate the image surface for a rendering
;;; operation (acquire) and the function used to cleanup that surface
;;; afterwards.
;;;
;;; The acquire callback should create a surface (preferably an image surface
;;; created to match the target using cairo_surface_create_similar_image())
;;; that defines at least the region of interest specified by extents. The
;;; surface is allowed to be the entire sample area, but if it does contain a
;;; subsection of the sample area, the surface extents should be provided by
;;; setting the device offset (along with its width and height) using
;;; cairo_surface_set_device_offset().
;;;
;;; pattern :
;;;     the pattern to update
;;;
;;; acquire :
;;;     acquire callback
;;;
;;; release :
;;;     release callback
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_pattern_get_acquire ()
;;;
;;; void
;;; cairo_raster_source_pattern_get_acquire
;;;                                (cairo_pattern_t *pattern,
;;;                                 cairo_raster_source_acquire_func_t *acquire,
;;;                                 cairo_raster_source_release_func_t *release)
;;;
;;; Queries the current acquire and release callbacks.
;;;
;;; pattern :
;;;     the pattern to query
;;;
;;; acquire :
;;;     return value for the current acquire callback
;;;
;;; release :
;;;     return value for the current release callback
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_pattern_set_snapshot ()
;;;
;;; void
;;; cairo_raster_source_pattern_set_snapshot
;;;                               (cairo_pattern_t *pattern,
;;;                                cairo_raster_source_snapshot_func_t snapshot)
;;;
;;; Sets the callback that will be used whenever a snapshot is taken of the
;;; pattern, that is whenever the current contents of the pattern should be
;;; preserved for later use. This is typically invoked whilst printing.
;;;
;;; pattern :
;;;     the pattern to update
;;;
;;; snapshot :
;;;     snapshot callback
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_pattern_get_snapshot ()
;;;
;;; cairo_raster_source_snapshot_func_t
;;; cairo_raster_source_pattern_get_snapshot (cairo_pattern_t *pattern);
;;;
;;; Queries the current snapshot callback.
;;;
;;; pattern :
;;;     the pattern to query
;;;
;;; Returns :
;;;     the current snapshot callback
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_pattern_set_copy ()
;;;
;;; void
;;; cairo_raster_source_pattern_set_copy (cairo_pattern_t *pattern,
;;;                                       cairo_raster_source_copy_func_t copy)
;;;
;;; Updates the copy callback which is used whenever a temporary copy of the
;;; pattern is taken.
;;;
;;; pattern :
;;;     the pattern to update
;;;
;;; copy :
;;;     the copy callback
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_pattern_get_copy ()
;;;
;;; cairo_raster_source_copy_func_t
;;; cairo_raster_source_pattern_get_copy (cairo_pattern_t *pattern);
;;;
;;; Queries the current copy callback.
;;;
;;; pattern :
;;;     the pattern to query
;;;
;;; Returns :
;;;     the current copy callback
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_pattern_set_finish ()
;;;
;;; void
;;; cairo_raster_source_pattern_set_finish
;;;                                (cairo_pattern_t *pattern,
;;;                                 cairo_raster_source_finish_func_t finish)
;;;
;;; Updates the finish callback which is used whenever a pattern (or a copy
;;; thereof) will no longer be used.
;;;
;;; pattern :
;;;     the pattern to update
;;;
;;; finish :
;;;     the finish callback
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_pattern_get_finish ()
;;;
;;; cairo_raster_source_finish_func_t
;;; cairo_raster_source_pattern_get_finish (cairo_pattern_t *pattern)
;;;
;;; Queries the current finish callback.
;;;
;;; pattern :
;;;     the pattern to query
;;;
;;; Returns :
;;;     the current finish callback
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_acquire_func_t ()
;;;
;;; cairo_surface_t
;;; (*cairo_raster_source_acquire_func_t) (cairo_pattern_t *pattern,
;;;                                        void *callback_data,
;;;                                        cairo_surface_t *target,
;;;                                        const cairo_rectangle_int_t *extents)
;;;
;;; cairo_raster_source_acquire_func_t is the type of function which is called
;;; when a pattern is being rendered from. It should create a surface that
;;; provides the pixel data for the region of interest as defined by extents,
;;; though the surface itself does not have to be limited to that area. For
;;; convenience the surface should probably be of image type, created with
;;; cairo_surface_create_similar_image() for the target (which enables the
;;; number of copies to be reduced during transfer to the device). Another
;;; option, might be to return a similar surface to the target for explicit
;;; handling by the application of a set of cached sources on the device. The
;;; region of sample data provided should be defined using
;;; cairo_surface_set_device_offset() to specify the top-left corner of the
;;; sample data (along with width and height of the surface).
;;;
;;; pattern :
;;;     the pattern being rendered from
;;;
;;; callback_data :
;;;     the user data supplied during creation
;;;
;;; target :
;;;     the rendering target surface
;;;
;;; extents :
;;;     rectangular region of interest in pixels in sample space
;;;
;;; Returns :
;;;     a cairo_surface_t
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_release_func_t ()
;;;
;;; void
;;;( *cairo_raster_source_release_func_t) (cairo_pattern_t *pattern,
;;;                                        void *callback_data,
;;;                                        cairo_surface_t *surface);
;;;
;;; cairo_raster_source_release_func_t is the type of function which is called
;;; when the pixel data is no longer being access by the pattern for the
;;; rendering operation. Typically this function will simply destroy the
;;; surface created during acquire.
;;;
;;; pattern :
;;;     the pattern being rendered from
;;;
;;; callback_data :
;;;     the user data supplied during creation
;;;
;;; surface :
;;;     the surface created during acquire
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_snapshot_func_t ()
;;;
;;; cairo_status_t
;;; (*cairo_raster_source_snapshot_func_t) (cairo_pattern_t *pattern,
;;;                                         void *callback_data);
;;;
;;; cairo_raster_source_snapshot_func_t is the type of function which is called
;;; when the pixel data needs to be preserved for later use during printing.
;;; This pattern will be accessed again later, and it is expected to provide
;;; the pixel data that was current at the time of snapshotting.
;;;
;;; pattern :
;;;     the pattern being rendered from
;;;
;;; callback_data :
;;;     the user data supplied during creation
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS on success, or one of the cairo_status_t error
;;;     codes for failure.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_copy_func_t ()
;;;
;;; cairo_status_t
;;; (*cairo_raster_source_copy_func_t) (cairo_pattern_t *pattern,
;;;                                     void *callback_data,
;;;                                     const cairo_pattern_t *other);
;;;
;;; cairo_raster_source_copy_func_t is the type of function which is called
;;; when the pattern gets copied as a normal part of rendering.
;;;
;;; pattern :
;;;     the cairo_pattern_t that was copied to
;;;
;;; callback_data :
;;;     the user data supplied during creation
;;;
;;; other :
;;;     the cairo_pattern_t being used as the source for the copy
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS on success, or one of the cairo_status_t error
;;;     codes for failure.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_raster_source_finish_func_t ()
;;;
;;; void
;;; (*cairo_raster_source_finish_func_t) (cairo_pattern_t *pattern,
;;;                                       void *callback_data);
;;;
;;; cairo_raster_source_finish_func_t is the type of function which is called
;;; when the pattern (or a copy thereof) is no longer required.
;;;
;;; pattern :
;;;     the pattern being rendered from
;;;
;;; callback_data :
;;;     the user data supplied during creation
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.raster-source.lisp -----------------------------------
