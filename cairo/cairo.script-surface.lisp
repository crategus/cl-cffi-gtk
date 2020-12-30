;;; ----------------------------------------------------------------------------
;;; cairo.script-surface.lisp
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
;;; Script Surfaces
;;;
;;;     Rendering to replayable scripts
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_SCRIPT_SURFACE
;;;     cairo_script_mode_t
;;;
;;; Functions
;;;
;;;     cairo_script_create
;;;     cairo_script_create_for_stream
;;;     cairo_script_from_recording_surface
;;;     cairo_script_get_mode
;;;     cairo_script_set_mode
;;;     cairo_script_surface_create
;;;     cairo_script_surface_create_for_target
;;;     cairo_script_write_comment
;;;
;;; Description
;;;
;;;     The script surface provides the ability to render to a native script
;;;     that matches the cairo drawing model. The scripts can be replayed using
;;;     tools under the util/cairo-script directory, or with cairo-perf-trace.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_SCRIPT_SURFACE
;;;
;;; #define CAIRO_HAS_SCRIPT_SURFACE 1
;;;
;;; Defined if the script surface backend is available. The script surface
;;; backend is always built in since 1.12.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_script_mode_t
;;;
;;; A set of script output variants.
;;;
;;; CAIRO_SCRIPT_MODE_ASCII
;;;     the output will be in readable text (default). (Since 1.12)
;;;
;;; CAIRO_SCRIPT_MODE_BINARY
;;;     the output will use byte codes. (Since 1.12)
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_script_create ()
;;;
;;; cairo_device_t *
;;; cairo_script_create (const char *filename);
;;;
;;; Creates a output device for emitting the script, used when creating the
;;; individual surfaces.
;;;
;;; filename :
;;;     the name (path) of the file to write the script to
;;;
;;; Returns :
;;;     a pointer to the newly created device. The caller owns the surface and
;;;     should call cairo_device_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" device if an error such as out of memory occurs. You
;;;     can use cairo_device_status() to check for this.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_script_create_for_stream ()
;;;
;;; cairo_device_t *
;;; cairo_script_create_for_stream (cairo_write_func_t write_func,
;;;                                 void *closure);
;;;
;;; Creates a output device for emitting the script, used when creating the
;;; individual surfaces.
;;;
;;; write_func :
;;;     callback function passed the bytes written to the script
;;;
;;; closure :
;;;     user data to be passed to the callback
;;;
;;; Returns :
;;;     a pointer to the newly created device. The caller owns the surface and
;;;     should call cairo_device_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" device if an error such as out of memory occurs. You
;;;     can use cairo_device_status() to check for this.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_script_from_recording_surface ()
;;;
;;; cairo_status_t
;;; cairo_script_from_recording_surface (cairo_device_t *script,
;;;                                      cairo_surface_t *recording_surface);
;;;
;;; Converts the record operations in recording_surface into a script.
;;;
;;; script :
;;;     the script (output device)
;;;
;;; recording_surface :
;;;     the recording surface to replay
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS on successful completion or an error code.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_script_get_mode ()
;;;
;;; cairo_script_mode_t
;;; cairo_script_get_mode (cairo_device_t *script);
;;;
;;; Queries the script for its current output mode.
;;;
;;; script :
;;;     The script (output device) to query
;;;
;;; Returns :
;;;     the current output mode of the script
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_script_set_mode ()
;;;
;;; void
;;; cairo_script_set_mode (cairo_device_t *script,
;;;                        cairo_script_mode_t mode);
;;;
;;; Change the output mode of the script
;;;
;;; script :
;;;     The script (output device)
;;;
;;; mode :
;;;     the new mode
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_script_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_script_surface_create (cairo_device_t *script,
;;;                              cairo_content_t content,
;;;                              double width,
;;;                              double height);
;;;
;;; Create a new surface that will emit its rendering through script
;;;
;;; script :
;;;     the script (output device)
;;;
;;; content :
;;;     the content of the surface
;;;
;;; width :
;;;     width in pixels
;;;
;;; height :
;;;     height in pixels
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" surface if an error such as out of memory occurs.
;;;     You can use cairo_surface_status() to check for this.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_script_surface_create_for_target ()
;;;
;;; cairo_surface_t *
;;; cairo_script_surface_create_for_target
;;;                                (cairo_device_t *script,
;;;                                 cairo_surface_t *target);
;;;
;;; Create a pxoy surface that will render to target and record the operations
;;; to device .
;;;
;;; script :
;;;     the script (output device)
;;;
;;; target :
;;;     a target surface to wrap
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" surface if an error such as out of memory occurs.
;;;     You can use cairo_surface_status() to check for this.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_script_write_comment ()
;;;
;;; void
;;; cairo_script_write_comment (cairo_device_t *script,
;;;                             const char *comment,
;;;                             int len);
;;;
;;; Emit a string verbatim into the script.
;;;
;;; script :
;;;     the script (output device)
;;;
;;; comment :
;;;     the string to emit
;;;
;;; len :
;;;     the length of the sting to write, or -1 to use strlen()
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.script-surface.lisp ----------------------------------
