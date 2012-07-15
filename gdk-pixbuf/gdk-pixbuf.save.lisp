;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.save.lisp
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
;;; File saving
;;;
;;; Saving a pixbuf to a file.
;;;     
;;; Synopsis
;;; 
;;;     gdk_pixbuf_savev
;;;     gdk_pixbuf_save
;;;     gdk_pixbuf_save_to_callback
;;;     gdk_pixbuf_save_to_callbackv
;;;     gdk_pixbuf_save_to_buffer
;;;     gdk_pixbuf_save_to_bufferv
;;;     gdk_pixbuf_save_to_stream
;;; 
;;; Description
;;; 
;;; These functions allow to save a GdkPixbuf in a number of file formats. The
;;; formatted data can be written to a file or to a memory buffer. gdk-pixbuf
;;; can also call a user-defined callback on the data, which allows to e.g.
;;; write the image to a socket or store it in a database.
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_savev ()
;;; 
;;; gboolean gdk_pixbuf_savev (GdkPixbuf *pixbuf,
;;;                            const char *filename,
;;;                            const char *type,
;;;                            char **option_keys,
;;;                            char **option_values,
;;;                            GError **error);
;;; 
;;; Saves pixbuf to a file in type, which is currently "jpeg", "png", "tiff",
;;; "ico" or "bmp". If error is set, FALSE will be returned.
;;; See gdk_pixbuf_save() for more details.
;;; 
;;; pixbuf :
;;;     a GdkPixbuf.
;;; 
;;; filename :
;;;     name of file to save.
;;; 
;;; type :
;;;     name of file format.
;;; 
;;; option_keys :
;;;     name of options to set, NULL-terminated.
;;; 
;;; option_values :
;;;     values for named options.
;;; 
;;; error :
;;;     return location for error, or NULL.
;;; 
;;; Returns :
;;;     whether an error was set
;;; ----------------------------------------------------------------------------

;; This function is not exported and is defined for internal use.
;; See the implementation of gdk-pixbuf-save

(defcfun ("gdk_pixbuf_savev" %gdk-pixbuf-savev) :boolean
  (pixbuf (g-object pixbuf))
  (filename :string)
  (type :string)
  (option-keys (:pointer (:pointer :char)))
  (option-values (:pointer (:pointer :char)))
  (error :pointer))

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_save ()
;;; 
;;; gboolean gdk_pixbuf_save (GdkPixbuf *pixbuf,
;;;                           const char *filename,
;;;                           const char *type,
;;;                           GError **error,
;;;                           ...);
;;; 
;;; Saves pixbuf to a file in format type. By default, "jpeg", "png", "ico" and
;;; "bmp" are possible file formats to save in, but more formats may be
;;; installed. The list of all writable formats can be determined in the
;;; following way:
;;; 
;;; void add_if_writable (GdkPixbufFormat *data, GSList **list)
;;; {
;;;   if (gdk_pixbuf_format_is_writable (data))
;;;     *list = g_slist_prepend (*list, data);
;;; }
;;; 
;;; GSList *formats = gdk_pixbuf_get_formats ();
;;; GSList *writable_formats = NULL;
;;; g_slist_foreach (formats, add_if_writable, &writable_formats);
;;; g_slist_free (formats);
;;; 
;;; If error is set, FALSE will be returned. Possible errors include those in
;;; the GDK_PIXBUF_ERROR domain and those in the G_FILE_ERROR domain.
;;; 
;;; The variable argument list should be NULL-terminated; if not empty, it
;;; should contain pairs of strings that modify the save parameters.
;;; For example:
;;; 
;;; gdk_pixbuf_save (pixbuf, handle, "jpeg", &error,
;;;                  "quality", "100", NULL);
;;; 
;;; Currently only few parameters exist. JPEG images can be saved with a
;;; "quality" parameter; its value should be in the range [0,100].
;;; 
;;; Text chunks can be attached to PNG images by specifying parameters of the
;;; form "tEXt::key", where key is an ASCII string of length 1-79. The values
;;; are UTF-8 encoded strings. The PNG compression level can be specified using
;;; the "compression" parameter; it's value is in an integer in the range of
;;; [0,9].
;;; 
;;; ICC color profiles can also be embedded into PNG and TIFF images. The
;;; "icc-profile" value should be the complete ICC profile encoded into base64.
;;; 
;;; gchar *contents;
;;; gchar *contents_encode;
;;; gsize length;
;;; g_file_get_contents ("/home/hughsie/.color/icc/L225W.icm",
;;;                      &contents, &length, NULL);
;;; contents_encode = g_base64_encode ((const guchar *) contents, length);
;;; gdk_pixbuf_save (pixbuf, handle, "png", &error,
;;;                  "icc-profile", contents_encode,
;;;                  NULL);
;;; 
;;; TIFF images recognize a "compression" option which acceps an integer value.
;;; Among the codecs are 1 None, 2 Huffman, 5 LZW, 7 JPEG and 8 Deflate, see
;;; the libtiff documentation and tiff.h for all supported codec values.
;;; 
;;; ICO images can be saved in depth 16, 24, or 32, by using the "depth"
;;; parameter. When the ICO saver is given "x_hot" and "y_hot" parameters, it
;;; produces a CUR instead of an ICO.
;;; 
;;; pixbuf :
;;;     a GdkPixbuf.
;;; 
;;; filename :
;;;     name of file to save.
;;; 
;;; type :
;;;     name of file format.
;;; 
;;; error :
;;;     return location for error, or NULL.
;;; 
;;; Varargs :
;;;     list of key-value save options
;;; 
;;; Returns :
;;;     whether an error was set
;;; ---------------------------------------------------------------------------- 

;; This implementation does not support the arguments error and Varargs.

(defun gdk-pixbuf-save (pixbuf filename type)
  (%gdk-pixbuf-savev pixbuf
                     (etypecase filename
                       (string filename)
                       (pathname (namestring filename)))
                    type
                    (null-pointer)
                    (null-pointer)
                    (null-pointer)))

(export 'gdk-pixbuf-save)

;;; ---------------------------------------------------------------------------- 
;;; GdkPixbufSaveFunc ()
;;; 
;;; gboolean (*GdkPixbufSaveFunc) (const gchar *buf,
;;;                                gsize count,
;;;                                GError **error,
;;;                                gpointer data);
;;; 
;;; Specifies the type of the function passed to gdk_pixbuf_save_to_callback().
;;; It is called once for each block of bytes that is "written" by
;;; gdk_pixbuf_save_to_callback(). If successful it should return TRUE. If an
;;; error occurs it should set error and return FALSE, in which case
;;; gdk_pixbuf_save_to_callback() will fail with the same error.
;;; 
;;; buf :
;;;     bytes to be written
;;; 
;;; count :
;;;     number of bytes in buf.
;;; 
;;; error :
;;;     A location to return an error
;;; 
;;; data :
;;;     user data passed to gdk_pixbuf_save_to_callback()
;;; 
;;; Returns :
;;;     TRUE if successful, FALSE (with error set) if failed.
;;; 
;;; Since 2.4
;;; ---------------------------------------------------------------------------- 

;;; ---------------------------------------------------------------------------- 
;;; gdk_pixbuf_save_to_callback ()
;;; 
;;; gboolean gdk_pixbuf_save_to_callback (GdkPixbuf *pixbuf,
;;;                                       GdkPixbufSaveFunc save_func,
;;;                                       gpointer user_data,
;;;                                       const char *type,
;;;                                       GError **error,
;;;                                       ...);
;;; 
;;; Saves pixbuf in format type by feeding the produced data to a callback.
;;; Can be used when you want to store the image to something other than a file,
;;; such as an in-memory buffer or a socket. If error is set, FALSE will be
;;; returned. Possible errors include those in the GDK_PIXBUF_ERROR domain and
;;; whatever the save function generates.
;;; 
;;; See gdk_pixbuf_save() for more details.
;;; 
;;; pixbuf :
;;;     a GdkPixbuf.
;;; 
;;; save_func :
;;;     a function that is called to save each block of data that the save
;;;     routine generates
;;; 
;;; user_data :
;;;     user data to pass to the save function.
;;; 
;;; type :
;;;     name of file format.
;;; 
;;; error :
;;;     return location for error, or NULL
;;; 
;;; Varargs :
;;;     list of key-value save options
;;; 
;;; Returns :
;;;     whether an error was set
;;; 
;;; Since 2.4
;;; ---------------------------------------------------------------------------- 

;;; ---------------------------------------------------------------------------- 
;;; gdk_pixbuf_save_to_callbackv ()
;;; 
;;; gboolean gdk_pixbuf_save_to_callbackv (GdkPixbuf *pixbuf,
;;;                                        GdkPixbufSaveFunc save_func,
;;;                                        gpointer user_data,
;;;                                        const char *type,
;;;                                        char **option_keys,
;;;                                        char **option_values,
;;;                                        GError **error);
;;; 
;;; Saves pixbuf to a callback in format type, which is currently "jpeg",
;;; "png", "tiff", "ico" or "bmp". If error is set, FALSE will be returned.
;;; See gdk_pixbuf_save_to_callback() for more details.
;;; 
;;; pixbuf :
;;;     a GdkPixbuf.
;;; 
;;; save_func :
;;;     a function that is called to save each block of data that the save
;;;     routine generates.
;;; 
;;; user_data :
;;;     user data to pass to the save function
;;; 
;;; type :
;;;     name of file format.
;;; 
;;; option_keys :
;;;     name of options to set, NULL-terminated.
;;; 
;;; option_values :
;;;     values for named options.
;;; 
;;; error :
;;;     return location for error, or NULL
;;; 
;;; Returns :
;;;     whether an error was set
;;; 
;;; Since 2.4
;;; ---------------------------------------------------------------------------- 

;;; ---------------------------------------------------------------------------- 
;;; gdk_pixbuf_save_to_buffer ()
;;; 
;;; gboolean gdk_pixbuf_save_to_buffer (GdkPixbuf *pixbuf,
;;;                                     gchar **buffer,
;;;                                     gsize *buffer_size,
;;;                                     const char *type,
;;;                                     GError **error,
;;;                                     ...);
;;; 
;;; Saves pixbuf to a new buffer in format type, which is currently "jpeg",
;;; "png", "tiff", "ico" or "bmp". This is a convenience function that
;;; uses gdk_pixbuf_save_to_callback() to do the real work. Note that the
;;; buffer is not nul-terminated and may contain embedded nuls. If error is
;;; set, FALSE will be returned and buffer will be set to NULL. Possible errors
;;; include those in the GDK_PIXBUF_ERROR domain.
;;; 
;;; See gdk_pixbuf_save() for more details.
;;; 
;;; pixbuf :
;;;     a GdkPixbuf.
;;; 
;;; buffer :
;;;     location to receive a pointer to the new buffer.
;;; 
;;; buffer_size :
;;;     location to receive the size of the new buffer.
;;; 
;;; type :
;;;     name of file format.
;;; 
;;; error :
;;;     return location for error, or NULL.
;;; 
;;; Varargs :
;;;     list of key-value save options
;;; 
;;; Returns :
;;;     whether an error was set
;;; 
;;; Since 2.4
;;; ---------------------------------------------------------------------------- 

;;; ---------------------------------------------------------------------------- 
;;; gdk_pixbuf_save_to_bufferv ()
;;; 
;;; gboolean gdk_pixbuf_save_to_bufferv (GdkPixbuf *pixbuf,
;;;                                      gchar **buffer,
;;;                                      gsize *buffer_size,
;;;                                      const char *type,
;;;                                      char **option_keys,
;;;                                      char **option_values,
;;;                                      GError **error);
;;; 
;;; Saves pixbuf to a new buffer in format type, which is currently "jpeg",
;;; "tiff", "png", "ico" or "bmp". See gdk_pixbuf_save_to_buffer() for more
;;; details.
;;; 
;;; pixbuf :
;;;     a GdkPixbuf.
;;; 
;;; buffer :
;;;     location to receive a pointer to the new buffer.
;;; 
;;; buffer_size :
;;;     location to receive the size of the new buffer.
;;; 
;;; type :
;;;     name of file format.
;;; 
;;; option_keys :
;;;     name of options to set, NULL-terminated.
;;; 
;;; option_values :
;;;     values for named options.
;;; 
;;; error :
;;;     return location for error, or NULL.
;;; 
;;; Returns :
;;;     whether an error was set
;;; 
;;; Since 2.4
;;; ---------------------------------------------------------------------------- 

;;; ---------------------------------------------------------------------------- 
;;; gdk_pixbuf_save_to_stream ()
;;; 
;;; gboolean gdk_pixbuf_save_to_stream (GdkPixbuf *pixbuf,
;;;                                     GOutputStream *stream,
;;;                                     const char *type,
;;;                                     GCancellable *cancellable,
;;;                                     GError **error,
;;;                                     ...);
;;; 
;;; Saves pixbuf to an output stream.
;;; 
;;; Supported file formats are currently "jpeg", "tiff", "png", "ico" or "bmp".
;;; See gdk_pixbuf_save_to_buffer() for more details.
;;; 
;;; The cancellable can be used to abort the operation from another thread. If
;;; the operation was cancelled, the error GIO_ERROR_CANCELLED will be returned.
;;; Other possible errors are in the GDK_PIXBUF_ERROR and G_IO_ERROR domains.
;;; 
;;; The stream is not closed.
;;; 
;;; pixbuf :
;;;     a GdkPixbuf
;;; 
;;; stream :
;;;     a GOutputStream to save the pixbuf to
;;; 
;;; type :
;;;     name of file format
;;; 
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore
;;; 
;;; error :
;;;     return location for error, or NULL
;;; 
;;; Varargs :
;;;     list of key-value save options
;;; 
;;; Returns :
;;;     TRUE if the pixbuf was saved successfully, FALSE if an error was set.
;;; 
;;; Since 2.14
;;; ---------------------------------------------------------------------------- 

;;; --- End of file gdk-pixbuf.save.lisp ---------------------------------------
