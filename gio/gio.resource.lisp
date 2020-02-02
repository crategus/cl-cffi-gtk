;;; ----------------------------------------------------------------------------
;;; gio.resource.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.60 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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

#|

Description

Applications and libraries often contain binary or textual data that is really part of the application, rather than user data. For instance GtkBuilder .ui files, splashscreen images, GMenu markup XML, CSS files, icons, etc. These are often shipped as files in $datadir/appname, or manually included as literal strings in the code.

The GResource API and the glib-compile-resources program provide a convenient and efficient alternative to this which has some nice properties. You maintain the files as normal files, so its easy to edit them, but during the build the files are combined into a binary bundle that is linked into the executable. This means that loading the resource files are efficient (as they are already in memory, shared with other instances) and simple (no need to check for things like I/O errors or locate the files in the filesystem). It also makes it easier to create relocatable applications.

Resource files can also be marked as compressed. Such files will be included in the resource bundle in a compressed form, but will be automatically uncompressed when the resource is used. This is very useful e.g. for larger text files that are parsed once (or rarely) and then thrown away.

Resource files can also be marked to be preprocessed, by setting the value of the preprocess attribute to a comma-separated list of preprocessing options. The only options currently supported are:

xml-stripblanks which will use the xmllint command to strip ignorable whitespace from the XML file. For this to work, the XMLLINT environment variable must be set to the full path to the xmllint executable, or xmllint must be in the PATH; otherwise the preprocessing step is skipped.

to-pixdata which will use the gdk-pixbuf-pixdata command to convert images to the GdkPixdata format, which allows you to create pixbufs directly using the data inside the resource file, rather than an (uncompressed) copy if it. For this, the gdk-pixbuf-pixdata program must be in the PATH, or the GDK_PIXBUF_PIXDATA environment variable must be set to the full path to the gdk-pixbuf-pixdata executable; otherwise the resource compiler will abort.

Resource files will be exported in the GResource namespace using the combination of the given prefix and the filename from the file element. The alias attribute can be used to alter the filename to expose them at a different location in the resource namespace. Typically, this is used to include files from a different source directory without exposing the source directory in the resource namespace, as in the example below.

Resource bundles are created by the glib-compile-resources program which takes an XML file that describes the bundle, and a set of files that the XML references. These are combined into a binary resource bundle.

An example resource description:

<?xml version="1.0" encoding="UTF-8"?>
<gresources>
  <gresource prefix="/org/gtk/Example">
    <file>data/splashscreen.png</file>
    <file compressed="true">dialog.ui</file>
    <file preprocess="xml-stripblanks">menumarkup.xml</file>
    <file alias="example.css">data/example.css</file>
  </gresource>
</gresources>
This will create a resource bundle with the following files:

/org/gtk/Example/data/splashscreen.png
/org/gtk/Example/dialog.ui
/org/gtk/Example/menumarkup.xml
/org/gtk/Example/example.css
Note that all resources in the process share the same namespace, so use Java-style path prefixes (like in the above example) to avoid conflicts.

You can then use glib-compile-resources to compile the XML to a binary bundle that you can load with g_resource_load(). However, its more common to use the --generate-source and --generate-header arguments to create a source file and header to link directly into your application. This will generate get_resource(), register_resource() and unregister_resource() functions, prefixed by the --c-name argument passed to glib-compile-resources. get_resource() returns the generated GResource object. The register and unregister functions register the resource so its files can be accessed using g_resources_lookup_data().

Once a GResource has been created and registered all the data in it can be accessed globally in the process by using API calls like g_resources_open_stream() to stream the data or g_resources_lookup_data() to get a direct pointer to the data. You can also use URIs like "resource:///org/gtk/Example/data/splashscreen.png" with GFile to access the resource data.

Some higher-level APIs, such as GtkApplication, will automatically load resources from certain well-known paths in the resource namespace as a convenience. See the documentation for those APIs for details.

There are two forms of the generated source, the default version uses the compiler support for constructor and destructor functions (where available) to automatically create and register the GResource on startup or library load time. If you pass --manual-register, two functions to register/unregister the resource are created instead. This requires an explicit initialization call in your application/library, but it works on all platforms, even on the minor ones where constructors are not supported. (Constructor support is available for at least Win32, Mac OS and Linux.)

Note that resource data can point directly into the data segment of e.g. a library, so if you are unloading libraries during runtime you need to be very careful with keeping around pointers to data from a resource, as this goes away when the library is unloaded. However, in practice this is not generally a problem, since most resource accesses are for your own resources, and resource data is often used once, during parsing, and then released.

When debugging a program or testing a change to an installed version, it is often useful to be able to replace resources in the program or library, without recompiling, for debugging or quick hacking and testing purposes. Since GLib 2.50, it is possible to use the G_RESOURCE_OVERLAYS environment variable to selectively overlay resources with replacements from the filesystem. It is a G_SEARCHPATH_SEPARATOR-separated list of substitutions to perform during resource lookups.

A substitution has the form

/org/gtk/libgtk=/home/desrt/gtk-overlay
The part before the = is the resource subpath for which the overlay applies. The part after is a filesystem path which contains files and subdirectories as you would like to be loaded as resources with the equivalent names.

In the example above, if an application tried to load a resource with the resource path /org/gtk/libgtk/ui/gtkdialog.ui then GResource would check the filesystem path /home/desrt/gtk-overlay/ui/gtkdialog.ui. If a file was found there, it would be used instead. This is an overlay, not an outright replacement, which means that if a file is not found at that path, the built-in version will be used instead. Whiteouts are not currently supported.

Substitutions must start with a slash, and must not contain a trailing slash before the '='. The path after the slash should ideally be absolute, but this is not strictly required. It is possible to overlay the location of a single resource with an individual file.

|#

;;; ----------------------------------------------------------------------------
;;; GResource
;;;
;;; A resource bundle.
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

(gobject:define-g-boxed-opaque g-resource "GResource"
  :alloc (error "GResource can not be created from Lisp side"))

(export (boxed-related-symbols 'g-resource))

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
;;; g_resource_load ()
;;;
;;; GResource *
;;; g_resource_load (const gchar *filename, GError **error);
;;;
;;; Loads a binary resource bundle and creates a GResource representation of it,
;;; allowing you to query it for data.
;;;
;;; If you want to use this resource in the global resource namespace you need
;;; to register it with g_resources_register().
;;;
;;; If filename is empty or the data in it is corrupt, G_RESOURCE_ERROR_INTERNAL
;;; will be returned. If filename doesn’t exist, or there is an error in reading
;;; it, an error from g_mapped_file_new() will be returned.
;;;
;;; filename :
;;;     the path of a filename to load, in the GLib filename encoding.
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a new GResource, or NULL on error.
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

(defcfun ("g_resource_load" %g-resource-load) (g-boxed-foreign g-resource)
  (filename :string)
  (error :pointer))

(defun g-resource-load (filename)
  (with-g-error (error)
    (%g-resource-load filename error)))

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
;;; For uncompressed resource files this is a pointer directly into the resource
;;; bundle, which is typically in some readonly data section in the program
;;; binary. For compressed files we allocate memory on the heap and
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
;;; Returns all the names of children at the specified path in the resource. The
;;; return result is a NULL terminated list of strings which should be released
;;; with g_strfreev().
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
;;; This is normally used by code generated by glib-compile-resources and is not
;;; typically used by other code.
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
;;; This is normally used by code generated by glib-compile-resources and is not
;;; typically used by other code.
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
;;; This is normally used by code generated by glib-compile-resources and is not
;;; typically used by other code.
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
;;;
;;; void
;;; g_resources_register (GResource *resource);
;;;
;;; Registers the resource with the process-global set of resources. Once a
;;; resource is registered the files in it can be accessed with the global
;;; resource lookup functions like g_resources_lookup_data().
;;;
;;; resource :
;;;     A GResource
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resources_unregister ()
;;;
;;; void
;;; g_resources_unregister (GResource *resource);
;;;
;;; Unregisters the resource from the process-global set of resources.
;;;
;;; resource :
;;;     A GResource
;;;
;;; Since: 2.32
;;; ----------------------------------------------------------------------------

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

(defcfun ("g_resources_lookup_data" %g-resources-lookup-data) :string
  (path :string)
  (lookup-flags :int) ; nachbessern type ist GResourceLookupFlags
  (error :pointer))

(defun g-resources-lookup-data (filename)
  (with-g-error (error)
    (%g-resources-lookup-data filename 0 error)))

(export 'g-resources-lookup-data)

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
