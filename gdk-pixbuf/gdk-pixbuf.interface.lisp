;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.interface.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.36 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2021 Dieter Kaiser
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
;;; Module Interface
;;;
;;;     Extending GdkPixBuf
;;;
;;; Types and Values
;;;
;;;     GdkPixbufFormat
;;;     GdkPixbufFormatFlags
;;;     GdkPixbufModulePattern
;;;     GdkPixbufModule
;;;     GdkPixbufAnimationClass
;;;     GdkPixbufAnimationIterClass
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_get_formats
;;;     gdk_pixbuf_format_copy
;;;     gdk_pixbuf_format_free
;;;     gdk_pixbuf_format_get_name
;;;     gdk_pixbuf_format_get_description
;;;     gdk_pixbuf_format_get_mime_types
;;;     gdk_pixbuf_format_get_extensions
;;;     gdk_pixbuf_format_is_save_option_supported
;;;     gdk_pixbuf_format_is_writable
;;;     gdk_pixbuf_format_is_scalable
;;;     gdk_pixbuf_format_is_disabled
;;;     gdk_pixbuf_format_set_disabled
;;;     gdk_pixbuf_format_get_license
;;;
;;;     GdkPixbufModuleFillVtableFunc
;;;     GdkPixbufModuleFillInfoFunc
;;;     GdkPixbufModuleSizeFunc
;;;     GdkPixbufModulePreparedFunc
;;;     GdkPixbufModuleUpdatedFunc
;;;
;;; Description
;;;
;;; If GdkPixbuf has been compiled with GModule support, it can be extended by
;;; modules which can load (and perhaps also save) new image and animation
;;; formats. Each loadable module must export a GdkPixbufModuleFillInfoFunc
;;; function named fill_info and a GdkPixbufModuleFillVtableFunc function named
;;; fill_vtable.
;;;
;;; In order to make format-checking work before actually loading the modules
;;; (which may require dlopening image libraries), modules export their
;;; signatures (and other information) via the fill_info function. An external
;;; utility, gdk-pixbuf-query-loaders, uses this to create a text file
;;; containing a list of all available loaders and their signatures. This file
;;; is then read at runtime by GdkPixBuf to obtain the list of available loaders
;;; and their signatures.
;;;
;;; Modules may only implement a subset of the functionality available via
;;; GdkPixbufModule. If a particular functionality is not implemented, the
;;; fill_vtable function will simply not set the corresponding function pointers
;;; of the GdkPixbufModule structure. If a module supports incremental loading
;;; (i.e. provides begin_load, stop_load and load_increment), it does not have
;;; to implement load, since GdkPixBuf can supply a generic load implementation
;;; wrapping the incremental loading.
;;;
;;; Installing a module is a two-step process:
;;;
;;;     copy the module file(s) to the loader directory (normally
;;;     libdir/gtk-2.0/version/loaders, unless overridden by the environment
;;;     variable GDK_PIXBUF_MODULEDIR)
;;;
;;;     call gdk-pixbuf-query-loaders to update the module file (normally
;;;     sysconfdir/gtk-2.0/gdk-pixbuf.loaders, unless overridden by the
;;;     environment variable GDK_PIXBUF_MODULE_FILE)
;;;
;;; The GdkPixBuf interfaces needed for implementing modules are contained in
;;; gdk-pixbuf-io.h (and gdk-pixbuf-animation.h if the module supports
;;; animations). They are not covered by the same stability guarantees as the
;;; regular GdkPixBuf API. To underline this fact, they are protected by #ifdef
;;; GDK_PIXBUF_ENABLE_BACKEND.
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; enum GdkPixbufFormatFlags                              not exported
;;; ----------------------------------------------------------------------------

(defbitfield gdk-pixbuf-format-flags
  (:writable 1)
  (:scalable 2)
  (:threadsave 4))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-format-flags atdoc:*symbol-name-alias*)
      "Bitfield"
      (gethash 'gdk-pixbuf-format-flags atdoc:*external-symbols*)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-22}
  @begin{short}
    Flags which allow a module to specify further details about the supported
    operations.
  @end{short}
  @begin{pre}
(defbitfield gdk-pixbuf-format-flags
  (:writable 1)
  (:scalable 2)
  (:threadsave 4))
  @end{pre}
  @begin[code]{table}
    @entry[:writable]{The module can write out images in the format.}
    @entry[:scalable]{The image format is scalable.}
    @entry[:threadsave]{The module is threadsafe. The @class{gdk-pixbuf} class
      ignores modules that are not marked as threadsafe.}
  @end{table}
  @see-class{gdk-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; struct GdkPixbufModulePattern                          not exported
;;; ----------------------------------------------------------------------------

(defcstruct gdk-pixbuf-module-pattern
  (prefix :string)
  (mask :string)
  (relevance :int))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-module-pattern atdoc:*type-name-alias*)
      "CStruct"
      (documentation 'gdk-pixbuf-module-pattern 'type)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-22}
  @begin{short}
    The signature of a module is a set of prefixes.
  @end{short}
  Prefixes are encoded as pairs of ordinary strings, where the second string,
  called the mask, if not @code{NULL}, must be of the same length as the first
  one and may contain ' ', '!', 'x', 'z', and 'n' to indicate bytes that must be
  matched, not matched, \"don't-care\"-bytes, zeros and non-zeros. Each prefix
  has an associated integer that describes the relevance of the prefix, with 0
  meaning a mismatch and 100 a \"perfect match\".

  Starting with GdkPixbuf version 2.8, the first byte of the mask may be '*',
  indicating an unanchored pattern that matches not only at the beginning, but
  also in the middle. Versions prior to 2.8 will interpret the '*' like an
  'x'.

  The signature of a module is stored as an array of
  @sym{gdk-pixbuf-module-pattern}s. The array is terminated by a pattern where
  the prefix is @code{NULL}.
  @begin{pre}
GdkPixbufModulePattern *signature[] = {
  { \"abcdx\", \" !x z\", 100 @},
  { \"bla\", NULL,  90 @},
  { NULL, NULL, 0 @}
@};
  @end{pre}
  The example matches e.g. \"auud\0\" with relevance 100, and \"blau\" with
  relevance 90.
  @begin{pre}
(defcstruct gdk-pixbuf-module-pattern
  (prefix :string)
  (mask :string)
  (relevance :int))
  @end{pre}
  @begin[code]{table}
    @entry[prefix]{The prefix for this pattern.}
    @entry[mask]{Mask containing bytes which modify how the prefix is matched
      against test data.}
    @entry[relvance]{Relevance of this pattern.}
  @end{table}
  @see-class{gdk-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; struct GdkPixbufModule
;;;
;;; struct GdkPixbufModule {
;;;     char *module_name;
;;;     char *module_path;
;;;     GModule *module;
;;;     GdkPixbufFormat *info;
;;;
;;;     GdkPixbuf *(* load) (FILE    *f,
;;;                          GError **error);
;;;     GdkPixbuf *(* load_xpm_data) (const char **data);
;;;
;;;     /* Incremental loading */
;;;
;;;     gpointer (* begin_load)     (GdkPixbufModuleSizeFunc size_func,
;;;                                  GdkPixbufModulePreparedFunc prepare_func,
;;;                                  GdkPixbufModuleUpdatedFunc update_func,
;;;                                  gpointer user_data,
;;;                                  GError **error);
;;;     gboolean (* stop_load)      (gpointer context,
;;;                                  GError **error);
;;;     gboolean (* load_increment) (gpointer      context,
;;;                                  const guchar *buf,
;;;                                  guint         size,
;;;                                  GError      **error);
;;;
;;;     /* Animation loading */
;;;     GdkPixbufAnimation *(* load_animation) (FILE    *f,
;;;                                                 GError **error);
;;;
;;;     /* Saving */
;;;     gboolean (* save) (FILE      *f,
;;;                        GdkPixbuf *pixbuf,
;;;                        gchar    **param_keys,
;;;                        gchar    **param_values,
;;;                        GError   **error);
;;;
;;;     gboolean (*save_to_callback) (GdkPixbufSaveFunc save_func,
;;;                   gpointer user_data,
;;;                   GdkPixbuf *pixbuf,
;;;                   gchar **option_keys,
;;;                   gchar **option_values,
;;;                   GError **error);
;;; };
;;;
;;; A GdkPixbufModule contains the necessary functions to load and save images
;;; in a certain file format.
;;;
;;; A GdkPixbufModule can be loaded dynamically from a GModule. Each loadable
;;; module must contain a GdkPixbufModuleFillVtableFunc function named
;;; fill_vtable, which will get called when the module is loaded and must set
;;; the function pointers of the GdkPixbufModule.
;;;
;;; char *module_name;
;;;     the name of the module, usually the same as the usual file extension
;;;     for images of this type, eg. "xpm", "jpeg" or "png".
;;;
;;; char *module_path;
;;;     the path from which the module is loaded.
;;;
;;; GModule *module;
;;;     the loaded GModule.
;;;
;;; GdkPixbufFormat *info;
;;;     a GdkPixbufFormat holding information about the module.
;;;
;;; load ()
;;;     loads an image from a file.
;;;
;;; load_xpm_data ()
;;;     loads an image from data in memory.
;;;
;;; begin_load ()
;;;     begins an incremental load.
;;;
;;; stop_load ()
;;;     stops an incremental load.
;;;
;;; load_increment ()
;;;     continues an incremental load.
;;;
;;; load_animation ()
;;;     loads an animation from a file.
;;;
;;; save ()
;;;     saves a GdkPixbuf to a file.
;;;
;;; save_to_callback ()
;;;     saves a GdkPixbuf by calling the given GdkPixbufSaveFunc.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkPixbufAnimationClass
;;;
;;; struct GdkPixbufAnimationClass {
;;;   GObjectClass            parent_class;
;;;   gboolean                (*is_static_image)  (GdkPixbufAnimation *anim);
;;;
;;;   GdkPixbuf*              (*get_static_image) (GdkPixbufAnimation *anim);
;;;
;;;   void                    (*get_size) (GdkPixbufAnimation *anim,
;;;                                        int                 *width,
;;;                                        int                 *height);
;;;
;;;   GdkPixbufAnimationIter* (*get_iter) (GdkPixbufAnimation *anim,
;;;                                        const GTimeVal     *start_time);
;;; };
;;;
;;; Modules supporting animations must derive a type from GdkPixbufAnimation,
;;; providing suitable implementations of the virtual functions.
;;;
;;; GObjectClass parent_class;
;;;     the parent class
;;;
;;; is_static_image ()
;;;     returns whether the given animation is just a static image.
;;;
;;; get_static_image ()
;;;     returns a static image representing the given animation.
;;;
;;; get_size ()
;;;     fills width and height with the frame size of the animation.
;;;
;;; get_iter ()
;;;     returns an iterator for the given animation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkPixbufAnimationIterClass
;;;
;;; struct GdkPixbufAnimationIterClass {
;;;   GObjectClass parent_class;
;;;
;;;   int        (*get_delay_time)   (GdkPixbufAnimationIter *iter);
;;;
;;;   GdkPixbuf* (*get_pixbuf)       (GdkPixbufAnimationIter *iter);
;;;
;;;   gboolean   (*on_currently_loading_frame) (GdkPixbufAnimationIter *iter);
;;;
;;;   gboolean   (*advance)          (GdkPixbufAnimationIter *iter,
;;;                                   const GTimeVal         *current_time);
;;; };
;;;
;;; Modules supporting animations must derive a type from
;;; GdkPixbufAnimationIter, providing suitable implementations of the virtual
;;; functions.
;;;
;;; GObjectClass parent_class;
;;;     the parent class
;;;
;;; get_delay_time ()
;;;     returns the time in milliseconds that the current frame should be shown.
;;;
;;; get_pixbuf ()
;;;     returns the current frame.
;;;
;;; on_currently_loading_frame ()
;;;     returns whether the current frame of iter is being loaded.
;;;
;;; advance ()
;;;     advances the iterator to current_time, possibly changing the current
;;;     frame.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkPixbufFormat
;;; ----------------------------------------------------------------------------

;; Simplified to an implementation as an opaque C structure

(defcstruct gdk-pixbuf-format)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-format atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'gdk-pixbuf-format atdoc:*external-symbols*)
 "@version{2021-7-24}
  @begin{short}
    A @sym{gdk-pixbuf-format} structure contains information about the image
    format accepted by a module.
  @end{short}
  Only modules should access the fields directly, applications should use the
  @code{gdk-pixbuf-format-*} functions.
  @see-class{gdk-pixbuf}")

(export 'gdk-pixbuf-format)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_formats () -> gdk-pixbuf-formats
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_get_formats" gdk-pixbuf-formats)
    (g-slist (:pointer (:struct gdk-pixbuf-format)) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-24}
  @begin{return}
    A list of @symbol{gdk-pixbuf-format} instances describing the supported
    image formats.
  @end{return}
  @begin{short}
    Obtains the available information about the image formats supported by
    the @class{gdk-pixbuf} API.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(mapcar #'gdk-pixbuf-format-name (gdk-pixbuf-formats))
=> (\"wmf\" \"ani\" \"bmp\" \"gif\" \"icns\" \"ico\" \"jpeg\" \"png\" \"pnm\"
    \"qtif\" \"svg\" \"tga\" \"tiff\" \"xbm\" \"xpm\")
    @end{pre}
  @end{dictionary}
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-pixbuf-format}")

(export 'gdk-pixbuf-formats)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_copy ()
;;;
;;; GdkPixbufFormat * gdk_pixbuf_format_copy (const GdkPixbufFormat *format);
;;;
;;; Creates a copy of format
;;;
;;; format :
;;;     a GdkPixbufFormat
;;;
;;; Returns :
;;;     the newly allocated copy of a GdkPixbufFormat. Use
;;;     gdk_pixbuf_format_free() to free the resources when done
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_free ()
;;;
;;; void gdk_pixbuf_format_free (GdkPixbufFormat *format);
;;;
;;; Frees the resources allocated when copying a GdkPixbufFormat using
;;; gdk_pixbuf_format_copy()
;;;
;;; format :
;;;     a GdkPixbufFormat
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_get_name () -> gdk-pixbuf-format-name
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_format_get_name" gdk-pixbuf-format-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-7-24}
  @argument[format]{a @symbol{gdk-pixbuf-format} instance}
  @return{A string with the name of the image format.}
  @short{Returns the name of the image format.}
  @see-symbol{gdk-pixbuf-format}"
  (format (:pointer (:struct gdk-pixbuf-format))))

(export 'gdk-pixbuf-format-name)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_get_description () -> gdk-pixbuf-format-description
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_format_get_description" gdk-pixbuf-format-description)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2021-7-24}
  @argument[format]{a @symbol{gdk-pixbuf-format} instance}
  @return{A string with a description of the image format.}
  @short{Returns a description of the image format.}
  @see-symbol{gdk-pixbuf-format}"
  (format (:pointer (:struct gdk-pixbuf-format))))

(export 'gdk-pixbuf-format-description)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_get_mime_types () -> gdk-pixbuf-format-mime-types
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_format_get_mime_types" gdk-pixbuf-format-mime-types)
    g-strv
 #+cl-cffi-gtk-documentation
 "@version{2021-7-24}
  @argument[format]{a @symbol{gdk-pixbuf-format} instance}
  @return{A list of strings with the MIME types.}
  @short{Returns the MIME types supported by the image format.}
  @see-symbol{gdk-pixbuf-format}"
  (format (:pointer (:struct gdk-pixbuf-format))))

(export 'gdk-pixbuf-format-mime-types)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_get_extensions () -> gdk-pixbuf-format-extensions
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_format_get_extensions" gdk-pixbuf-format-extensions)
    g-strv
 #+cl-cffi-gtk-documentation
 "@version{2021-7-24}
  @argument[format]{a @symbol{gdk-pixbuf-format} instance}
  @return{A list of strings with filename extensions.}
  @begin{short}
    Returns the filename extensions typically used for files in the given
    image format.
  @end{short}
  @see-symbol{gdk-pixbuf-format}"
  (format (:pointer (:struct gdk-pixbuf-format))))

(export 'gdk-pixbuf-format-extensions)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_is_save_option_supported ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_format_is_save_option_supported"
           gdk-pixbuf-format-is-save-option-supported) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-24}
  @argument[format]{a @symbol{gdk-pixbuf-format} instance}
  @argument[option]{a string with the name of an option}
  @return{@em{True} if the specified option is supported.}
  @begin{short}
    Returns @em{true} if the save option specified by @arg{option} is supported
    when saving a pixbuf using the module implementing the image format.
  @end{short}
  See the function @fun{gdk-pixbuf-save} for more information about option keys.

  Since 2.36
  @see-symbol{gdk-pixbuf-format}
  @see-function{gdk-pixbuf-save}"
  (format (:pointer (:struct gdk-pixbuf-format)))
  (option :string))

(export 'gdk-pixbuf-format-is-save-option-supported)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_is_writable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_format_is_writable" gdk-pixbuf-format-is-writable)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-24}
  @argument[format]{a @symbol{gdk-pixbuf-format} instance}
  @return{A boolean whether pixbufs can be saved in the given image format.}
  @begin{short}
    Returns whether pixbufs can be saved in the given image format.
  @end{short}
  @see-symbol{gdk-pixbuf-format}"
  (format (:pointer (:struct gdk-pixbuf-format))))

(export 'gdk-pixbuf-format-is-writable)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_is_scalable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_format_is_scalable" gdk-pixbuf-format-is-scalable)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-24}
  @argument[format]{a @symbol{gdk-pixbuf-format} instance}
  @return{A boolean whether this image format is scalable.}
  @begin{short}
    Returns whether this image format is scalable.
  @end{short}
  If a file is in a scalable format, it is preferable to load it at the desired
  size, rather than loading it at the default size and scaling the resulting
  pixbuf to the desired size.
  @see-symbol{gdk-pixbuf-format}"
  (format (:pointer (:struct gdk-pixbuf-format))))

(export 'gdk-pixbuf-format-is-scalable)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_is_disabled ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_format_is_disabled" gdk-pixbuf-format-is-disabled)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-24}
  @argument[format]{a @symbol{gdk-pixbuf-format} instance}
  @return{A boolean whether this image format is disabled.}
  @begin{short}
    Returns whether this image format is disabled.
  @end{short}
  See the function @fun{gdk-pixbuf-format-set-disabled}.
  @see-symbol{gdk-pixbuf-format}
  @see-function{gdk-pixbuf-format-set-disabled}"
  (format (:pointer (:struct gdk-pixbuf-format))))

(export 'gdk-pixbuf-format-is-disabled)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_set_disabled ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_format_set_disabled" gdk-pixbuf-format-set-disabled) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-24}
  @argument[format]{a @symbol{gdk-pixbuf-format} instance}
  @argument[disabled]{@em{true} to disable the given image format}
  @return{A boolean whether this image format is disabled.}
  @begin{short}
    Disables or enables an image format.
  @end{short}
  If the image format is disabled, the @class{gdk-pixbuf} library will not use
  the image loader for this image format to load images. Applications can use
  this to avoid using image loaders with an inappropriate license, see the
  function @fun{gdk-pixbuf-format-license}.
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-pixbuf-format}
  @see-function{gdk-pixbuf-format-license}"
  (format (:pointer (:struct gdk-pixbuf-format)))
  (disabled :boolean))

(export 'gdk-pixbuf-format-set-disabled)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_format_get_license () -> gdk-pixbuf-format-license
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_format_get_license" gdk-pixbuf-format-license) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-7-24}
  @argument[format]{a @symbol{gdk-pixbuf-format} instance}
  @return{A string describing the license of the image format.}
  @begin{short}
    Returns information about the license of the image loader for the
    image format.
  @end{short}
  The returned string should be a shorthand for a wellknown license, e.g.
  \"LGPL\", \"GPL\", \"QPL\", \"GPL/QPL\", or \"other\" to indicate some other
  license.
  @see-symbol{gdk-pixbuf-format}"
  (format (:pointer (:struct gdk-pixbuf-format))))

(export 'gdk-pixbuf-format-license)

;;; ----------------------------------------------------------------------------
;;; GdkPixbufModuleFillVtableFunc ()
;;;
;;; void (*GdkPixbufModuleFillVtableFunc) (GdkPixbufModule *module);
;;;
;;; Defines the type of the function used to set the vtable of a GdkPixbufModule
;;; when it is loaded.
;;;
;;; module :
;;;     a GdkPixbufModule.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkPixbufModuleFillInfoFunc ()
;;;
;;; void (*GdkPixbufModuleFillInfoFunc) (GdkPixbufFormat *info);
;;;
;;; Defines the type of the function used to fill a GdkPixbufFormat structure
;;; with information about a module.
;;;
;;; info :
;;;     a GdkPixbufFormat.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkPixbufModuleSizeFunc ()
;;;
;;; void (*GdkPixbufModuleSizeFunc) (gint *width,
;;;                                  gint *height,
;;;                                  gpointer user_data);
;;;
;;; Defines the type of the function that gets called once the size of the
;;; loaded image is known.
;;;
;;; The function is expected to set width and height to the desired size to
;;; which the image should be scaled. If a module has no efficient way to
;;; achieve the desired scaling during the loading of the image, it may either
;;; ignore the size request, or only approximate it -- &gdk-pixbuf; will then
;;; perform the required scaling on the completely loaded image.
;;;
;;; If the function sets width or height to zero, the module should interpret
;;; this as a hint that it will be closed soon and shouldn't allocate further
;;; resources. This convention is used to implement gdk_pixbuf_get_file_info()
;;; efficiently.
;;;
;;; width :
;;;     pointer to a location containing the current image width
;;;
;;; height :
;;;     pointer to a location containing the current image height
;;;
;;; user_data :
;;;     the loader.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkPixbufModulePreparedFunc ()
;;;
;;; void (*GdkPixbufModulePreparedFunc) (GdkPixbuf *pixbuf,
;;;                                      GdkPixbufAnimation *anim,
;;;                                      gpointer user_data);
;;;
;;; Defines the type of the function that gets called once the initial setup of
;;; pixbuf is done.
;;;
;;; GdkPixbufLoader uses a function of this type to emit the "area_prepared"
;;; signal.
;;;
;;; pixbuf :
;;;     the GdkPixbuf that is currently being loaded.
;;;
;;; anim :
;;;     if an animation is being loaded, the GdkPixbufAnimation, else NULL.
;;;
;;; user_data :
;;;     the loader.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkPixbufModuleUpdatedFunc ()
;;;
;;; void (*GdkPixbufModuleUpdatedFunc) (GdkPixbuf *pixbuf,
;;;                                     int x,
;;;                                     int y,
;;;                                     int width,
;;;                                     int height,
;;;                                     gpointer user_data);
;;;
;;; Defines the type of the function that gets called every time a region of
;;; pixbuf is updated.
;;;
;;; GdkPixbufLoader uses a function of this type to emit the "area_updated"
;;; signal.
;;;
;;; pixbuf :
;;;     the GdkPixbuf that is currently being loaded.
;;;
;;; x :
;;;     the X origin of the updated area.
;;;
;;; y :
;;;     the Y origin of the updated area.
;;;
;;; width :
;;;     the width of the updated area.
;;;
;;; height :
;;;     the height of the updated area.
;;;
;;; user_data :
;;;     the loader.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk-pixbuf.interface.lisp ----------------------------------
