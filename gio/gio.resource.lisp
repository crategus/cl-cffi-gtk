;;; ----------------------------------------------------------------------------
;;; gio.resource.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.66 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2020 Dieter Kaiser
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
;;; GResource
;;;
;;;     Resource framework
;;;
;;; Types and Values
;;;
;;;     GResource
;;;     GResourceFlags
;;;     GResourceLookupFlags
;;;     GStaticResource
;;;
;;;     G_RESOURCE_ERROR
;;;     GResourceError
;;;
;;; Functions
;;;
;;;     g_resource_load ()
;;;     g_resource_new_from_data ()
;;;     g_resource_ref ()
;;;     g_resource_unref ()
;;;     g_resource_lookup_data ()
;;;     g_resource_open_stream ()
;;;     g_resource_enumerate_children ()
;;;     g_resource_get_info ()
;;;     g_static_resource_init ()
;;;     g_static_resource_fini ()
;;;     g_static_resource_get_resource ()
;;;     g_resources_register ()
;;;     g_resources_unregister ()
;;;     g_resources_lookup_data ()
;;;     g_resources_open_stream ()
;;;     g_resources_enumerate_children ()
;;;     g_resources_get_info ()
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GResource
;;;
;;;     GEnum
;;;     ╰── GResourceError
;;;
;;;     GFlags
;;;     ├── GResourceFlags
;;;     ╰── GResourceLookupFlags
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; enum GResourceFlags
;;;
;;; GResourceFlags give information about a particular file inside a resource
;;; bundle.
;;;
;;; G_RESOURCE_FLAGS_NONE
;;;     No flags set.
;;;
;;; G_RESOURCE_FLAGS_COMPRESSED
;;;     The file is compressed.
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GResourceLookupFlags
;;;
;;; GResourceLookupFlags determine how resource path lookups are handled.
;;;
;;; G_RESOURCE_LOOKUP_FLAGS_NONE
;;;     No flags set.
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GStaticResource
;;;
;;; GStaticResource is an opaque data structure and can only be accessed using
;;; the following functions.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_RESOURCE_ERROR
;;;
;;; #define G_RESOURCE_ERROR (g_resource_error_quark ())
;;;
;;; Error domain for GResource. Errors in this domain will be from the
;;; GResourceError enumeration. See GError for more information on error
;;; domains.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GResourceError
;;;
;;; An error code used with G_RESOURCE_ERROR in a GError returned from a
;;; GResource routine.
;;;
;;; G_RESOURCE_ERROR_NOT_FOUND
;;;     no file was found at the requested path
;;;
;;; G_RESOURCE_ERROR_INTERNAL
;;;     unknown error
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GResource
;;; ----------------------------------------------------------------------------

(gobject:define-g-boxed-opaque g-resource "GResource"
  :alloc (error "GResource can not be created from the Lisp side."))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-resource atdoc:*class-name-alias*) "CStruct"
      (documentation 'g-resource 'type)
 "@version{2020-11-27}
  @begin{short}
    Applications and libraries often contain binary or textual data that is
    really part of the application, rather than user data.
  @end{short}
  For instance @class{gtk-builder} @code{.ui} files, splashscreen images,
  @class{g-menu} markup XML, CSS files, icons, etc. These are often shipped as
  files in @code{$datadir/appname}, or manually included as literal strings in
  the code.

  The @sym{g-resource} API and the @code{glib-compile-resources} program provide
  a convenient and efficient alternative to this which has some nice properties.
  You maintain the files as normal files, so its easy to edit them, but during
  the build the files are combined into a binary bundle that is linked into the
  executable. This means that loading the resource files are efficient (as they
  are already in memory, shared with other instances) and simple (no need to
  check for things like I/O errors or locate the files in the filesystem). It
  also makes it easier to create relocatable applications.

  Resource files can also be marked as compressed. Such files will be included
  in the resource bundle in a compressed form, but will be automatically
  uncompressed when the resource is used. This is very useful e.g. for larger
  text files that are parsed once (or rarely) and then thrown away.

  Resource files can also be marked to be preprocessed, by setting the value of
  the preprocess attribute to a comma-separated list of preprocessing options.
  The only options currently supported are:
  @begin{itemize}
    @begin{item}
      @code{xml-stripblanks} which will use the xmllint command to strip
      ignorable whitespace from the XML file. For this to work, the
      @code{XMLLINT} environment variable must be set to the full path to the
      @code{xmllint} executable, or @code{xmllint} must be in the @code{PATH};
      otherwise the preprocessing step is skipped.
    @end{item}
    @begin{item}
      @code{to-pixdata} which will use the @code{gdk-pixbuf-pixdata} command to
      convert images to the @code{GdkPixdata} format, which allows you to
      create pixbufs directly using the data inside the resource file, rather
      than an (uncompressed) copy if it. For this, the @code{gdk-pixbuf-pixdata}
      program must be in the @code{PATH}, or the @code{GDK_PIXBUF_PIXDATA}
      environment variable must be set to the full path to the
      @code{gdk-pixbuf-pixdata} executable; otherwise the resource compiler
      will abort.
    @end{item}
  @end{itemize}
  Resource files will be exported in the @sym{g-resource} namespace using the
  combination of the given prefix and the filename from the file element. The
  alias attribute can be used to alter the filename to expose them at a
  different location in the resource namespace. Typically, this is used to
  include files from a different source directory without exposing the source
  directory in the resource namespace, as in the example below.

  Resource bundles are created by the @code{glib-compile-resources} program
  which takes an XML file that describes the bundle, and a set of files that
  the XML references. These are combined into a binary resource bundle.

  An example resource description:
  @begin{pre}
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<gresources>
  <gresource prefix=\"/org/gtk/Example\">
    <file>data/splashscreen.png</file>
    <file compressed=\"true\">dialog.ui</file>
    <file preprocess=\"xml-stripblanks\">menumarkup.xml</file>
    <file alias=\"example.css\">data/example.css</file>
  </gresource>
</gresources>
  @end{pre}
  This will create a resource bundle with the following files:
  @begin{pre}
/org/gtk/Example/data/splashscreen.png
/org/gtk/Example/dialog.ui
/org/gtk/Example/menumarkup.xml
/org/gtk/Example/example.css
  @end{pre}
  Note that all resources in the process share the same namespace, so use
  Java-style path prefixes (like in the above example) to avoid conflicts.

  You can then use @code{glib-compile-resources} to compile the XML to a binary
  bundle that you can load with the function @fun{g-resource-load}. However, its
  more common to use the @code{--generate-source} and @code{--generate-header}
  arguments to create a source file and header to link directly into your
  application. This will generate @code{get_resource()},
  @code{register_resource()} and @code{unregister_resource()} functions,
  prefixed by the @code{--c-name} argument passed to
  @code{glib-compile-resources}. The function @code{get_resource()} returns the
  generated @class{g-resource} object. The register and unregister functions
  register the resource so its files can be accessed using
  the function @fun{g-resources-lookup-data}.

  Once a @sym{g-resource} has been created and registered all the data in it
  can be accessed globally in the process by using API calls like the function
  @fun{g-resources-open-stream} to stream the data or the function
  @fun{g-resources-lookup-data} to get a direct pointer to the data. You can
  also use URIs like
  @code{\"resource:///org/gtk/Example/data/splashscreen.png\"} with GFile to
  access the resource data.

  Some higher-level APIs, such as the @class{gtk-application} class, will
  automatically load resources from certain well-known paths in the resource
  namespace as a convenience. See the documentation for those APIs for details.

  There are two forms of the generated source, the default version uses the
  compiler support for constructor and destructor functions (where available)
  to automatically create and register the @sym{g-resource} on startup or
  library load time. If you pass @code{--manual-register}, two functions to
  register/unregister the resource are created instead. This requires an
  explicit initialization call in your application/library, but it works on all
  platforms, even on the minor ones where constructors are not supported.
  (Constructor support is available for at least Win32, Mac OS and Linux.)

  Note that resource data can point directly into the data segment of e.g. a
  library, so if you are unloading libraries during runtime you need to be very
  careful with keeping around pointers to data from a resource, as this goes
  away when the library is unloaded. However, in practice this is not generally
  a problem, since most resource accesses are for your own resources, and
  resource data is often used once, during parsing, and then released.

  When debugging a program or testing a change to an installed version, it is
  often useful to be able to replace resources in the program or library,
  without recompiling, for debugging or quick hacking and testing purposes.
  Since GLib 2.50, it is possible to use the @code{G_RESOURCE_OVERLAYS}
  environment variable to selectively overlay resources with replacements from
  the filesystem. It is a @code{G_SEARCHPATH_SEPARATOR} separated list of
  substitutions to perform during resource lookups.

  A substitution has the form:
  @begin{pre}
/org/gtk/libgtk=/home/desrt/gtk-overlay
  @end{pre}
  The part before the @code{=} is the resource subpath for which the overlay
  applies. The part after is a filesystem path which contains files and
  subdirectories as you would like to be loaded as resources with the
  equivalent names.

  In the example above, if an application tried to load a resource with the
  resource path @code{/org/gtk/libgtk/ui/gtkdialog.ui} then @sym{g-resource}
  would check the filesystem path
  @code{/home/desrt/gtk-overlay/ui/gtkdialog.ui}. If a file was found there, it
  would be used instead. This is an overlay, not an outright replacement, which
  means that if a file is not found at that path, the built-in version will be
  used instead. Whiteouts are not currently supported.

  Substitutions must start with a slash, and must not contain a trailing slash
  before the @code{'='}. The path after the slash should ideally be absolute,
  but this is not strictly required. It is possible to overlay the location of
  a single resource with an individual file.
")

(export (boxed-related-symbols 'g-resource))

;;; ----------------------------------------------------------------------------
;;; g_resource_load ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_resource_load" %g-resource-load) (g-boxed-foreign g-resource)
  (filename :string)
  (err :pointer))

(defun g-resource-load (filename)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-27}
  @argument[filename]{a string with the path of a filename to load, in the
    GLib filenname encoding}
  @return{A new @class{g-resource} structure, or @code{nil} on error.}
  @begin{short}
    Loads a binary resource bundle and creates a @class{g-resource} structure
    representation of it, allowing you to query it for data.
  @end{short}

  If you want to use this resource in the global resource namespace you need
  to register it with the function @fun{g-resources-register}.

  If @arg{filename} is empty or the data in it is corrupt,
  @code{G_RESOURCE_ERROR_INTERNAL} will be returned. If @arg{filename} does not
  exist, or there is an error in reading it, an error from
  @code{g_mapped_file_new()} will be returned.
  @see-class{g-resource}"
  (with-g-error (err)
    (%g-resource-load filename err)))

(export 'g-resource-load)

;;; ----------------------------------------------------------------------------
;;; g_resource_new_from_data ()
;;;
;;; GResource *
;;; g_resource_new_from_data (GBytes *data, GError **error);
;;;
;;; Creates a GResource from a reference to the binary resource bundle. This
;;; will keep a reference to data while the resource lives, so the data should
;;; not be modified or freed.
;;;
;;; If you want to use this resource in the global resource namespace you need
;;; to register it with g_resources_register().
;;;
;;; Note: data must be backed by memory that is at least pointer aligned.
;;; Otherwise this function will internally create a copy of the memory since
;;; GLib 2.56, or in older versions fail and exit the process.
;;;
;;; If data is empty or corrupt, G_RESOURCE_ERROR_INTERNAL will be returned.
;;;
;;;data :
;;;     A GBytes
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a new GResource, or NULL on error.
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resource_ref ()
;;;
;;; GResource *
;;; g_resource_ref (GResource *resource);
;;;
;;; Atomically increments the reference count of resource by one. This function
;;; is MT-safe and may be called from any thread.
;;;
;;; resource :
;;;     A GResource
;;;
;;; Returns :
;;;     The passed in GResource
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resource_unref ()
;;;
;;; void
;;; g_resource_unref (GResource *resource);
;;;
;;; Atomically decrements the reference count of resource by one. If the
;;; reference count drops to 0, all memory allocated by the resource is
;;; released. This function is MT-safe and may be called from any thread.
;;;
;;; resource :
;;;     A GResource
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resource_lookup_data ()
;;;
;;; GBytes *
;;; g_resource_lookup_data (GResource *resource,
;;;                         const char *path,
;;;                         GResourceLookupFlags lookup_flags,
;;;                         GError **error);
;;;
;;; Looks for a file at the specified path in the resource and returns a GBytes
;;; that lets you directly access the data in memory.
;;;
;;; The data is always followed by a zero byte, so you can safely use the data
;;; as a C string. However, that byte is not included in the size of the GBytes.
;;;
;;; For uncompressed resource files this is a pointer directly into the
;;; resource bundle, which is typically in some readonly data section in the
;;; program binary. For compressed files we allocate memory on the heap and
;;; automatically uncompress the data.
;;;
;;; lookup_flags controls the behaviour of the lookup.
;;;
;;; resource :
;;; A GResource
;;;
;;; path :
;;;     A pathname inside the resource
;;;
;;; lookup_flags :
;;;     A GResourceLookupFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     GBytes or NULL on error. Free the returned object with g_bytes_unref().
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resource_open_stream ()
;;;
;;; GInputStream *
;;; g_resource_open_stream (GResource *resource,
;;;                        const char *path,
;;;                        GResourceLookupFlags lookup_flags,
;;;                        GError **error);
;;;
;;; Looks for a file at the specified path in the resource and returns a
;;; GInputStream that lets you read the data.
;;;
;;; lookup_flags controls the behaviour of the lookup.
;;;
;;; resource :
;;;     A GResource
;;;
;;; path :
;;;     A pathname inside the resource
;;;
;;; lookup_flags :
;;;     A GResourceLookupFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     GInputStream or NULL on error. Free the returned object with
;;;     g_object_unref().
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resource_enumerate_children ()
;;;
;;; char **
;;; g_resource_enumerate_children (GResource *resource,
;;;                                const char *path,
;;;                                GResourceLookupFlags lookup_flags,
;;;                                GError **error);
;;;
;;; Returns all the names of children at the specified path in the resource.
;;; The return result is a NULL terminated list of strings which should be
;;; released with g_strfreev().
;;;
;;; If path is invalid or does not exist in the GResource,
;;; G_RESOURCE_ERROR_NOT_FOUND will be returned.
;;;
;;; lookup_flags controls the behaviour of the lookup.
;;;
;;; resource :
;;;     A GResource
;;;
;;; path :
;;;     A pathname inside the resource
;;;
;;; lookup_flags :
;;;     A GResourceLookupFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     an array of constant strings.
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resource_get_info ()
;;;
;;; gboolean
;;; g_resource_get_info (GResource *resource,
;;;                      const char *path,
;;;                      GResourceLookupFlags lookup_flags,
;;;                      gsize *size,
;;;                      guint32 *flags,
;;;                      GError **error);
;;;
;;; Looks for a file at the specified path in the resource and if found returns
;;; information about it.
;;;
;;; lookup_flags controls the behaviour of the lookup.
;;;
;;; resource :
;;;     A GResource
;;;
;;; path :
;;      A pathname inside the resource
;;;
;;; lookup_flags :
;;;     A GResourceLookupFlags
;;;
;;; size :
;;;     a location to place the length of the contents of the file, or NULL if
;;;     the length is not needed.
;;;
;;; flags :
;;;     a location to place the flags about the file, or NULL if the length is
;;;     not needed.
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the file was found. FALSE if there were errors
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_static_resource_init ()
;;;
;;; void
;;; g_static_resource_init (GStaticResource *static_resource);
;;;
;;; Initializes a GResource from static data using a GStaticResource.
;;;
;;; This is normally used by code generated by glib-compile-resources and is
;;; not typically used by other code.
;;;
;;; static_resource :
;;;     pointer to a static GStaticResource
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_static_resource_fini ()
;;;
;;; void
;;; g_static_resource_fini (GStaticResource *static_resource);
;;;
;;; Finalized a GResource initialized by g_static_resource_init().
;;;
;;; This is normally used by code generated by glib-compile-resources and is
;;; not typically used by other code.
;;;
;;; static_resource :
;;;     pointer to a static GStaticResource
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_static_resource_get_resource ()
;;;
;;; GResource *
;;; g_static_resource_get_resource (GStaticResource *static_resource);
;;;
;;; Gets the GResource that was registered by a call to
;;; g_static_resource_init().
;;;
;;; This is normally used by code generated by glib-compile-resources and is
;;; not typically used by other code.
;;;
;;; static_resource :
;;;     pointer to a static GStaticResource
;;;
;;; Returns :
;;;     a GResource.
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resources_register ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_resources_register" g-resources-register) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-27}
  @argument[resource]{a @class{g-resource} structure}
  @begin{short}
    Registers the resource with the process-global set of resources.
  @end{short}
  Once a resource is registered the files in it can be accessed with the global
  resource lookup functions like the function @fun{g-resources-lookup-data}.
  @see-class{g-resource}"
  (resource (g-boxed-foreign g-resource)))

(export 'g-resources-register)

;;; ----------------------------------------------------------------------------
;;; g_resources_unregister ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_resources_unregister" g-resources-unregister) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-27}
  @argument[resource]{a @class{g-resource} structure}
  @begin{short}
    Unregisters the resource from the process-global set of resources.
  @end{short}
  @see-class{g-resource}"
  (resource (g-boxed-foreign g-resource)))

(export 'g-resources-unregister)

;;; ----------------------------------------------------------------------------
;;; g_resources_lookup_data ()
;;;
;;; GBytes *
;;; g_resources_lookup_data (const char *path,
;;;                          GResourceLookupFlags lookup_flags,
;;;                          GError **error);
;;;
;;; Looks for a file at the specified path in the set of globally registered
;;; resources and returns a GBytes that lets you directly access the data in
;;; memory.
;;;
;;; The data is always followed by a zero byte, so you can safely use the data
;;; as a C string. However, that byte is not included in the size of the GBytes.
;;;
;;; For uncompressed resource files this is a pointer directly into the resource
;;; bundle, which is typically in some readonly data section in the program
;;; binary. For compressed files we allocate memory on the heap and
;;; automatically uncompress the data.
;;;
;;; lookup_flags controls the behaviour of the lookup.
;;;
;;; path :
;;;     A pathname inside the resource
;;;
;;; lookup_flags :
;;;     A GResourceLookupFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     GBytes or NULL on error. Free the returned object with g_bytes_unref().
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;; TODO: Implement GBytes

(defcfun ("g_resources_lookup_data" %g-resources-lookup-data) :string
  (path :string)
  (lookup-flags :int) ; nachbessern type ist GResourceLookupFlags
  (err :pointer))

(defun g-resources-lookup-data (filename)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-27}

  @see-class{g-resource}"
  (with-g-error (err)
    (%g-resources-lookup-data filename 0 err)))

;;; ----------------------------------------------------------------------------
;;; g_resources_open_stream ()
;;;
;;; GInputStream *
;;; g_resources_open_stream (const char *path,
;;;                          GResourceLookupFlags lookup_flags,
;;;                          GError **error);
;;;
;;; Looks for a file at the specified path in the set of globally registered
;;; resources and returns a GInputStream that lets you read the data.
;;;
;;; lookup_flags controls the behaviour of the lookup.
;;;
;;; path :
;;;     A pathname inside the resource
;;;
;;; lookup_flags :
;;;     A GResourceLookupFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     GInputStream or NULL on error. Free the returned object with
;;;     g_object_unref().
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resources_enumerate_children ()
;;;
;;; char **
;;; g_resources_enumerate_children (const char *path,
;;;                                 GResourceLookupFlags lookup_flags,
;;;                                 GError **error);
;;;
;;; Returns all the names of children at the specified path in the set of
;;; globally registered resources. The return result is a NULL terminated list
;;; of strings which should be released with g_strfreev().
;;;
;;; lookup_flags controls the behaviour of the lookup.
;;;
;;; path :
;;;     A pathname inside the resource
;;;
;;; lookup_flags :
;;;     A GResourceLookupFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     an array of constant strings.
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resources_get_info ()
;;;
;;; gboolean
;;; g_resources_get_info (const char *path,
;;;                       GResourceLookupFlags lookup_flags,
;;;                       gsize *size,
;;;                       guint32 *flags,
;;;                       GError **error);
;;;
;;; Looks for a file at the specified path in the set of globally registered
;;; resources and if found returns information about it.
;;;
;;; lookup_flags controls the behaviour of the lookup.
;;;
;;; path :
;;;     A pathname inside the resource
;;;
;;; lookup_flags :
;;;     A GResourceLookupFlags
;;;
;;; size :
;;;     a location to place the length of the contents of the file, or NULL if
;;;     the length is not needed.
;;;
;;; flags :
;;;     a location to place the GResourceFlags about the file, or NULL if the
;;;     flags are not needed.
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the file was found. FALSE if there were errors
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.resource.lisp ------------------------------------------
