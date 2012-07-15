;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.load.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK-PixBuf Reference Manual
;;; Version 2.26.1. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; File Loading
;;; 
;;; Loading a pixbuf from a file.
;;; 
;;; Synopsis
;;; 
;;;     gdk_pixbuf_new_from_file
;;;     gdk_pixbuf_new_from_file_at_size
;;;     gdk_pixbuf_new_from_file_at_scale
;;;     gdk_pixbuf_get_file_info
;;;     gdk_pixbuf_new_from_stream
;;;     gdk_pixbuf_new_from_stream_at_scale
;;; 
;;; Description
;;; 
;;; The gdk-pixbuf library provides a simple mechanism for loading an image
;;; from a file in synchronous fashion. This means that the library takes
;;; control of the application while the file is being loaded; from the user's
;;; point of view, the application will block until the image is done loading.
;;; 
;;; This interface can be used by applications in which blocking is acceptable
;;; while an image is being loaded. It can also be used to load small images in
;;; general. Applications that need progressive loading can use the
;;; GdkPixbufLoader functionality instead.
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_file ()
;;; 
;;; GdkPixbuf * gdk_pixbuf_new_from_file (const char *filename,
;;;                                       GError **error);
;;; 
;;; Creates a new pixbuf by loading an image from a file. The file format is
;;; detected automatically. If NULL is returned, then error will be set.
;;; Possible errors are in the GDK_PIXBUF_ERROR and G_FILE_ERROR domains.
;;; 
;;; filename :
;;;     Name of file to load, in the GLib file name encoding
;;; 
;;; error :
;;;     Return location for an error
;;; 
;;; Returns :
;;;     A newly-created pixbuf with a reference count of 1, or NULL if any of
;;;     several error conditions occurred: the file could not be opened, there
;;;     was no loader for the file's format, there was not enough memory to
;;;     allocate the image buffer, or the image file contained invalid data.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_new_from_file" %gdk-pixbuf-new-from-file)
    (g-object gdk-pixbuf :already-referenced)
  (filename :string)
  (error :pointer))

(defun gdk-pixbuf-new-from-file (filename)
  (with-g-error (err)
    (%gdk-pixbuf-new-from-file filename err)))

(export 'gdk-pixbuf-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_file_at_size ()
;;; 
;;; GdkPixbuf * gdk_pixbuf_new_from_file_at_size (const char *filename,
;;;                                               int width,
;;;                                               int height,
;;;                                               GError **error);
;;; 
;;; Creates a new pixbuf by loading an image from a file. The file format is
;;; detected automatically. If NULL is returned, then error will be set.
;;; Possible errors are in the GDK_PIXBUF_ERROR and G_FILE_ERROR domains.
;;; 
;;; The image will be scaled to fit in the requested size, preserving the
;;; image's aspect ratio. Note that the returned pixbuf may be smaller than
;;; width x height, if the aspect ratio requires it. To load and image at the
;;; requested size, regardless of aspect ratio, use
;;; gdk_pixbuf_new_from_file_at_scale().
;;; 
;;; filename :
;;;     Name of file to load, in the GLib file name encoding
;;; 
;;; width :
;;;     The width the image should have or -1 to not constrain the width
;;; 
;;; height :
;;;     The height the image should have or -1 to not constrain the height
;;; 
;;; error :
;;;     Return location for an error
;;; 
;;; Returns :
;;;     A newly-created pixbuf with a reference count of 1, or NULL if any of
;;;     several error conditions occurred: the file could not be opened, there
;;;     was no loader for the file's format, there was not enough memory to
;;;     allocate the image buffer, or the image file contained invalid data.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_file_at_scale ()
;;; 
;;; GdkPixbuf * gdk_pixbuf_new_from_file_at_scale
;;;                                             (const char *filename,
;;;                                              int width,
;;;                                              int height,
;;;                                              gboolean preserve_aspect_ratio,
;;;                                              GError **error);
;;; 
;;; Creates a new pixbuf by loading an image from a file. The file format is
;;; detected automatically. If NULL is returned, then error will be set.
;;; Possible errors are in the GDK_PIXBUF_ERROR and G_FILE_ERROR domains. The
;;; image will be scaled to fit in the requested size, optionally preserving
;;; the image's aspect ratio.
;;; 
;;; When preserving the aspect ratio, a width of -1 will cause the image to be
;;; scaled to the exact given height, and a height of -1 will cause the image
;;; to be scaled to the exact given width. When not preserving aspect ratio,
;;; a width or height of -1 means to not scale the image at all in that
;;; dimension. Negative values for width and height are allowed since 2.8.
;;; 
;;; filename :
;;;     Name of file to load, in the GLib file name encoding
;;; 
;;; width :
;;;     The width the image should have or -1 to not constrain the width
;;; 
;;; height :
;;;     The height the image should have or -1 to not constrain the height
;;; 
;;; preserve_aspect_ratio :
;;;     TRUE to preserve the image's aspect ratio
;;; 
;;; error :
;;;     Return location for an error
;;; 
;;; Returns :
;;;     A newly-created pixbuf with a reference count of 1, or NULL if any of
;;;     several error conditions occurred: the file could not be opened, there
;;;     was no loader for the file's format, there was not enough memory to
;;;     allocate the image buffer, or the image file contained invalid data.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_file_info ()
;;; 
;;; GdkPixbufFormat * gdk_pixbuf_get_file_info (const gchar *filename,
;;;                                             gint *width,
;;;                                             gint *height);
;;; 
;;; Parses an image file far enough to determine its format and size.
;;; 
;;; filename :
;;;     The name of the file to identify.
;;; 
;;; width :
;;;     Return location for the width of the image, or NULL.
;;; 
;;; height :
;;;     Return location for the height of the image, or NULL.
;;; 
;;; Returns :
;;;     A GdkPixbufFormat describing the image format of the file or NULL if
;;;     the image format wasn't recognized. The return value is owned by
;;;     GdkPixbuf and should not be freed.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_stream ()
;;; 
;;; GdkPixbuf * gdk_pixbuf_new_from_stream (GInputStream *stream,
;;;                                         GCancellable *cancellable,
;;;                                         GError **error);
;;; 
;;; Creates a new pixbuf by loading an image from an input stream.
;;; 
;;; The file format is detected automatically. If NULL is returned, then error
;;; will be set. The cancellable can be used to abort the operation from another
;;; thread. If the operation was cancelled, the error GIO_ERROR_CANCELLED will
;;; be returned. Other possible errors are in the
;;; GDK_PIXBUF_ERROR and G_IO_ERROR domains.
;;; 
;;; The stream is not closed.
;;; 
;;; stream :
;;;     a GInputStream to load the pixbuf from
;;; 
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore
;;; 
;;; error :
;;;     Return location for an error
;;; 
;;; Returns :
;;;     A newly-created pixbuf, or NULL if any of several error conditions
;;;     occurred: the file could not be opened, the image format is not
;;;     supported, there was not enough memory to allocate the image buffer,
;;;     the stream contained invalid data, or the operation was cancelled.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_stream_at_scale ()
;;; 
;;; GdkPixbuf * gdk_pixbuf_new_from_stream_at_scale
;;;                                             (GInputStream *stream,
;;;                                              gint width,
;;;                                              gint height,
;;;                                              gboolean preserve_aspect_ratio,
;;;                                              GCancellable *cancellable,
;;;                                              GError **error);
;;; 
;;; Creates a new pixbuf by loading an image from an input stream.
;;; 
;;; The file format is detected automatically. If NULL is returned, then error
;;; will be set. The cancellable can be used to abort the operation from another
;;; thread. If the operation was cancelled, the error GIO_ERROR_CANCELLED will
;;; be returned. Other possible errors are in the GDK_PIXBUF_ERROR and
;;; G_IO_ERROR domains.
;;; 
;;; The image will be scaled to fit in the requested size, optionally preserving
;;; the image's aspect ratio. When preserving the aspect ratio, a width of -1
;;; will cause the image to be scaled to the exact given height, and a height
;;; of -1 will cause the image to be scaled to the exact given width. When not
;;; preserving aspect ratio, a width or height of -1 means to not scale the
;;; image at all in that dimension.
;;; 
;;; The stream is not closed.
;;; 
;;; stream :
;;;     a GInputStream to load the pixbuf from
;;; 
;;; width :
;;;     The width the image should have or -1 to not constrain the width
;;; 
;;; height :
;;;     The height the image should have or -1 to not constrain the height
;;; 
;;; preserve_aspect_ratio :
;;;     TRUE to preserve the image's aspect ratio
;;; 
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore
;;; 
;;; error :
;;;     Return location for an error
;;; 
;;; Returns :
;;;     A newly-created pixbuf, or NULL if any of several error conditions
;;;     occurred: the file could not be opened, the image format is not
;;;     supported, there was not enough memory to allocate the image buffer,
;;;     the stream contained invalid data, or the operation was cancelled.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------


;;; --- End of file gdk-pixbuf.load.lisp ---------------------------------------
