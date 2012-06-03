;;; ----------------------------------------------------------------------------
;;; gtk.recent-manager.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;; GtkRecentManager
;;; 
;;; Managing recently used files
;;;     
;;; Synopsis
;;; 
;;;     GtkRecentManager
;;;     GtkRecentInfo
;;;     GtkRecentData
;;;
;;;     GTK_RECENT_MANAGER_ERROR
;;;
;;;     GtkRecentManagerError
;;;
;;;     gtk_recent_manager_new
;;;     gtk_recent_manager_get_default
;;;     gtk_recent_manager_add_item
;;;     gtk_recent_manager_add_full
;;;     gtk_recent_manager_remove_item
;;;     gtk_recent_manager_lookup_item
;;;     gtk_recent_manager_has_item
;;;     gtk_recent_manager_move_item
;;;     gtk_recent_manager_get_items
;;;     gtk_recent_manager_purge_items
;;;     
;;;     gtk_recent_info_ref
;;;     gtk_recent_info_unref
;;;     gtk_recent_info_get_uri
;;;     gtk_recent_info_get_display_name
;;;     gtk_recent_info_get_description
;;;     gtk_recent_info_get_mime_type
;;;     gtk_recent_info_get_added
;;;     gtk_recent_info_get_modified
;;;     gtk_recent_info_get_visited
;;;     gtk_recent_info_get_private_hint
;;;     gtk_recent_info_get_application_info
;;;     gtk_recent_info_get_applications
;;;     gtk_recent_info_last_application
;;;     gtk_recent_info_has_application
;;;     gtk_recent_info_create_app_info
;;;     gtk_recent_info_get_groups
;;;     gtk_recent_info_has_group
;;;     gtk_recent_info_get_icon
;;;     gtk_recent_info_get_gicon
;;;     gtk_recent_info_get_short_name
;;;     gtk_recent_info_get_uri_display
;;;     gtk_recent_info_get_age
;;;     gtk_recent_info_is_local
;;;     gtk_recent_info_exists
;;;     gtk_recent_info_match
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkRecentManager
;;; 
;;; Properties
;;; 
;;;   "filename"                 gchar*               : Read / Write / Construct
;;;   "size"                     gint                 : Read
;;; 
;;; Signals
;;; 
;;;   "changed"                                       : Run First
;;; 
;;; Description
;;; 
;;; GtkRecentManager provides a facility for adding, removing and looking up
;;; recently used files. Each recently used file is identified by its URI, and
;;; has meta-data associated to it, like the names and command lines of the
;;; applications that have registered it, the number of time each application
;;; has registered the same file, the mime type of the file and whether the file
;;; should be displayed only by the applications that have registered it.
;;; 
;;; Note
;;; 
;;; The recently used files list is per user.
;;; 
;;; The GtkRecentManager acts like a database of all the recently used files.
;;; You can create new GtkRecentManager objects, but it is more efficient to use
;;; the default manager created by GTK+.
;;; 
;;; Adding a new recently used file is as simple as:
;;; 
;;; GtkRecentManager *manager;
;;; 
;;; manager = gtk_recent_manager_get_default ();
;;; gtk_recent_manager_add_item (manager, file_uri);
;;; 
;;; The GtkRecentManager will try to gather all the needed information from the
;;; file itself through GIO.
;;; 
;;; Looking up the meta-data associated with a recently used file given its URI
;;; requires calling gtk_recent_manager_lookup_item():
;;; 
;;; GtkRecentManager *manager;
;;; GtkRecentInfo *info;
;;; GError *error = NULL;
;;; 
;;; manager = gtk_recent_manager_get_default ();
;;; info = gtk_recent_manager_lookup_item (manager, file_uri, &error);
;;; if (error)
;;;   {
;;;     g_warning ("Could not find the file: %s", error->message);
;;;     g_error_free (error);
;;;   }
;;; else
;;;  {
;;;    /* Use the info object */
;;;    gtk_recent_info_unref (info);
;;;  }
;;; 
;;; In order to retrieve the list of recently used files, you can use
;;; gtk_recent_manager_get_items(), which returns a list of GtkRecentInfo
;;; structures.
;;; 
;;; A GtkRecentManager is the model used to populate the contents of one, or
;;; more GtkRecentChooser implementations.
;;; 
;;; Note
;;; 
;;; The maximum age of the recently used files list is controllable through the
;;; "gtk-recent-files-max-age" property.
;;; 
;;; Recently used files are supported since GTK+ 2.10.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "filename" property
;;; 
;;;   "filename"                 gchar*               : Read / Write / Construct
;;; 
;;; The full path to the file to be used to store and read the recently used
;;; resources list
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "size" property
;;; 
;;;   "size"                     gint                  : Read
;;; 
;;; The size of the recently used resources list.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: 0
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "changed" signal
;;; 
;;; void user_function (GtkRecentManager *recent_manager,
;;;                     gpointer          user_data)           : Run First
;;; 
;;; Emitted when the current recently used resources manager changes its
;;; contents, either by calling gtk_recent_manager_add_item() or by another
;;; application.
;;; 
;;; recent_manager :
;;;     the recent manager
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentManager
;;; 
;;; struct GtkRecentManager;
;;; 
;;; GtkRecentManager contains only private data and should be accessed using the
;;; provided API.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRecentManager" gtk-recent-manager
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_recent_manager_get_type")
  ((filename
    gtk-recent-manager-filename
    "filename" "gchararray" t nil)
   (size
    gtk-recent-manager-size
    "size" "gint" t nil)))

;;; ----------------------------------------------------------------------------
;;; GtkRecentInfo
;;; 
;;; typedef struct _GtkRecentInfo GtkRecentInfo;
;;; 
;;; GtkRecentInfo is an opaque data structure whose members can only be accessed
;;; using the provided API.
;;; 
;;; GtkRecentInfo constains all the meta-data associated with an entry in the
;;; recently used files list.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentData
;;; 
;;; struct GtkRecentData {
;;;   gchar *display_name;
;;;   gchar *description;
;;; 
;;;   gchar *mime_type;
;;; 
;;;   gchar *app_name;
;;;   gchar *app_exec;
;;; 
;;;   gchar **groups;
;;; 
;;;   gboolean is_private;
;;; };
;;; 
;;; Meta-data to be passed to gtk_recent_manager_add_full() when registering a
;;; recently used resource.
;;; 
;;; gchar *display_name;
;;;     a UTF-8 encoded string, containing the name of the recently used
;;;     resource to be displayed, or NULL;
;;; 
;;; gchar *description;
;;;     a UTF-8 encoded string, containing a short description of the resource,
;;;     or NULL;
;;; 
;;; gchar *mime_type;
;;;     the MIME type of the resource;
;;; 
;;; gchar *app_name;
;;;     the name of the application that is registering this recently used
;;;     resource;
;;; 
;;; gchar *app_exec;
;;;     command line used to launch this resource; may contain the "%f" and "%u"
;;;     escape characters which will be expanded to the resource file path and
;;;     URI respectively when the command line is retrieved;
;;; 
;;; gchar **groups;
;;;     a vector of strings containing groups names;
;;; 
;;; gboolean is_private;
;;;     whether this resource should be displayed only by the applications that
;;;     have registered it or not.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_RECENT_MANAGER_ERROR
;;; 
;;; #define GTK_RECENT_MANAGER_ERROR (gtk_recent_manager_error_quark ())
;;; 
;;; The GError domain for GtkRecentManager errors.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkRecentManagerError
;;; 
;;; typedef enum {
;;;   GTK_RECENT_MANAGER_ERROR_NOT_FOUND,
;;;   GTK_RECENT_MANAGER_ERROR_INVALID_URI,
;;;   GTK_RECENT_MANAGER_ERROR_INVALID_ENCODING,
;;;   GTK_RECENT_MANAGER_ERROR_NOT_REGISTERED,
;;;   GTK_RECENT_MANAGER_ERROR_READ,
;;;   GTK_RECENT_MANAGER_ERROR_WRITE,
;;;   GTK_RECENT_MANAGER_ERROR_UNKNOWN
;;; } GtkRecentManagerError;
;;; 
;;; Error codes for GtkRecentManager operations
;;; 
;;; GTK_RECENT_MANAGER_ERROR_NOT_FOUND
;;;     the URI specified does not exists in the recently used resources list.
;;; 
;;; GTK_RECENT_MANAGER_ERROR_INVALID_URI
;;;     the URI specified is not valid.
;;; 
;;; GTK_RECENT_MANAGER_ERROR_INVALID_ENCODING
;;;     the supplied string is not UTF-8 encoded.
;;; 
;;; GTK_RECENT_MANAGER_ERROR_NOT_REGISTERED
;;;     no application has registered the specified item.
;;; 
;;; GTK_RECENT_MANAGER_ERROR_READ
;;;     failure while reading the recently used resources file.
;;; 
;;; GTK_RECENT_MANAGER_ERROR_WRITE
;;;     failure while writing the recently used resources file.
;;; 
;;; GTK_RECENT_MANAGER_ERROR_UNKNOWN
;;;     unspecified error.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkRecentManagerError" gtk-recent-manager-error
  (:export t
   :type-initializer "gtk_recent_manager_error_get_type")
  (:not-found 0)
  (:invalid-uri 1)
  (:invalid-encoding 2)
  (:not-registered 3)
  (:read 4)
  (:write 5)
  (:unknown 6))

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_new ()
;;; 
;;; GtkRecentManager * gtk_recent_manager_new (void);
;;; 
;;; Creates a new recent manager object. Recent manager objects are used to
;;; handle the list of recently used resources. A GtkRecentManager object
;;; monitors the recently used resources list, and emits the "changed" signal
;;; each time something inside the list changes.
;;; 
;;; GtkRecentManager objects are expensive: be sure to create them only when
;;; needed. You should use gtk_recent_manager_get_default() instead.
;;; 
;;; Returns :
;;;     A newly created GtkRecentManager object.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_get_default ()
;;; 
;;; GtkRecentManager * gtk_recent_manager_get_default (void);
;;; 
;;; Gets a unique instance of GtkRecentManager, that you can share in your
;;; application without caring about memory management.
;;; 
;;; Returns :
;;;     A unique GtkRecentManager. Do not ref or unref it.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_add_item ()
;;; 
;;; gboolean gtk_recent_manager_add_item (GtkRecentManager *manager,
;;;                                       const gchar *uri);
;;; 
;;; Adds a new resource, pointed by uri, into the recently used resources list.
;;; 
;;; This function automatically retrieves some of the needed metadata and
;;; setting other metadata to common default values; it then feeds the data to
;;; gtk_recent_manager_add_full().
;;; 
;;; See gtk_recent_manager_add_full() if you want to explicitly define the
;;; metadata for the resource pointed by uri.
;;; 
;;; manager :
;;;     a GtkRecentManager
;;; 
;;; uri :
;;;     a valid URI
;;; 
;;; Returns :
;;;     TRUE if the new item was successfully added to the recently used
;;;     resources list
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_add_full ()
;;; 
;;; gboolean gtk_recent_manager_add_full (GtkRecentManager *manager,
;;;                                       const gchar *uri,
;;;                                       const GtkRecentData *recent_data);
;;; 
;;; Adds a new resource, pointed by uri, into the recently used resources list,
;;; using the metadata specified inside the GtkRecentData structure passed in
;;; recent_data.
;;; 
;;; The passed URI will be used to identify this resource inside the list.
;;; 
;;; In order to register the new recently used resource, metadata about the
;;; resource must be passed as well as the URI; the metadata is stored in a
;;; GtkRecentData structure, which must contain the MIME type of the resource
;;; pointed by the URI; the name of the application that is registering the
;;; item, and a command line to be used when launching the item.
;;; 
;;; Optionally, a GtkRecentData structure might contain a UTF-8 string to be
;;; used when viewing the item instead of the last component of the URI; a short
;;; description of the item; whether the item should be considered private -
;;; that is, should be displayed only by the applications that have registered
;;; it.
;;; 
;;; manager :
;;;     a GtkRecentManager
;;; 
;;; uri :
;;;     a valid URI
;;; 
;;; recent_data :
;;;     metadata of the resource
;;; 
;;; Returns :
;;;     TRUE if the new item was successfully added to the recently used
;;;     resources list, FALSE otherwise.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_remove_item ()
;;; 
;;; gboolean gtk_recent_manager_remove_item (GtkRecentManager *manager,
;;;                                          const gchar *uri,
;;;                                          GError **error);
;;; 
;;; Removes a resource pointed by uri from the recently used resources list
;;; handled by a recent manager.
;;; 
;;; manager :
;;;     a GtkRecentManager
;;; 
;;; uri :
;;;     the URI of the item you wish to remove
;;; 
;;; error :
;;;     return location for a GError, or NULL
;;; 
;;; Returns :
;;;     TRUE if the item pointed by uri has been successfully removed by the
;;;     recently used resources list, and FALSE otherwise.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_lookup_item ()
;;; 
;;; GtkRecentInfo * gtk_recent_manager_lookup_item (GtkRecentManager *manager,
;;;                                                 const gchar *uri,
;;;                                                 GError **error);
;;; 
;;; Searches for a URI inside the recently used resources list, and returns a
;;; structure containing informations about the resource like its MIME type, or
;;; its display name.
;;; 
;;; manager :
;;;     a GtkRecentManager
;;; 
;;; uri :
;;;     a URI
;;; 
;;; error :
;;;     a return location for a GError, or NULL
;;; 
;;; Returns :
;;;     a GtkRecentInfo structure containing information about the resource
;;;     pointed by uri, or NULL if the URI was not registered in the recently
;;;     used resources list. Free with gtk_recent_info_unref().
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_has_item ()
;;; 
;;; gboolean gtk_recent_manager_has_item (GtkRecentManager *manager,
;;;                                       const gchar *uri);
;;; 
;;; Checks whether there is a recently used resource registered with uri inside
;;; the recent manager.
;;; 
;;; manager :
;;;     a GtkRecentManager
;;; 
;;; uri :
;;;     a URI
;;; 
;;; Returns :
;;;     TRUE if the resource was found, FALSE otherwise.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_move_item ()
;;; 
;;; gboolean gtk_recent_manager_move_item (GtkRecentManager *manager,
;;;                                        const gchar *uri,
;;;                                        const gchar *new_uri,
;;;                                        GError **error);
;;; 
;;; Changes the location of a recently used resource from uri to new_uri.
;;; 
;;; Please note that this function will not affect the resource pointed by the
;;; URIs, but only the URI used in the recently used resources list.
;;; 
;;; manager :
;;;     a GtkRecentManager
;;; 
;;; uri :
;;;     the URI of a recently used resource
;;; 
;;; new_uri :
;;;     the new URI of the recently used resource, or NULL to remove the item
;;;     pointed by uri in the list
;;; 
;;; error :
;;;     a return location for a GError, or NULL
;;; 
;;; Returns :
;;;     TRUE on success.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_get_items ()
;;; 
;;; GList * gtk_recent_manager_get_items (GtkRecentManager *manager);
;;; 
;;; Gets the list of recently used resources.
;;; 
;;; manager :
;;;     a GtkRecentManager
;;; 
;;; Returns :
;;;     a list of newly allocated GtkRecentInfo objects. Use
;;;     gtk_recent_info_unref() on each item inside the list, and then free the
;;;     list itself using g_list_free()
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_purge_items ()
;;; 
;;; gint gtk_recent_manager_purge_items (GtkRecentManager *manager,
;;;                                      GError **error);
;;; 
;;; Purges every item from the recently used resources list.
;;; 
;;; manager :
;;;     a GtkRecentManager
;;; 
;;; error :
;;;     a return location for a GError, or NULL
;;; 
;;; Returns :
;;;     the number of items that have been removed from the recently used
;;;     resources list.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_ref ()
;;; 
;;; GtkRecentInfo * gtk_recent_info_ref (GtkRecentInfo *info);
;;; 
;;; Increases the reference count of recent_info by one.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     the recent info object with its reference count increased by one.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_unref ()
;;; 
;;; void gtk_recent_info_unref (GtkRecentInfo *info);
;;; 
;;; Decreases the reference count of info by one. If the reference count reaches
;;; zero, info is deallocated, and the memory freed.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_uri ()
;;; 
;;; const gchar * gtk_recent_info_get_uri (GtkRecentInfo *info);
;;; 
;;; Gets the URI of the resource.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     the URI of the resource. The returned string is owned by the recent
;;;     manager, and should not be freed.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_display_name ()
;;; 
;;; const gchar * gtk_recent_info_get_display_name (GtkRecentInfo *info);
;;; 
;;; Gets the name of the resource. If none has been defined, the basename of the
;;; resource is obtained.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     the display name of the resource. The returned string is owned by the
;;;     recent manager, and should not be freed.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_description ()
;;; 
;;; const gchar * gtk_recent_info_get_description (GtkRecentInfo *info);
;;; 
;;; Gets the (short) description of the resource.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     the description of the resource. The returned string is owned by the
;;;     recent manager, and should not be freed.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_mime_type ()
;;; 
;;; const gchar * gtk_recent_info_get_mime_type (GtkRecentInfo *info);
;;; 
;;; Gets the MIME type of the resource.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     the MIME type of the resource. The returned string is owned by the
;;;     recent manager, and should not be freed.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_added ()
;;; 
;;; time_t gtk_recent_info_get_added (GtkRecentInfo *info);
;;; 
;;; Gets the timestamp (seconds from system's Epoch) when the resource was added
;;; to the recently used resources list.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     the number of seconds elapsed from system's Epoch when the resource was
;;;     added to the list, or -1 on failure.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_modified ()
;;; 
;;; time_t gtk_recent_info_get_modified (GtkRecentInfo *info);
;;; 
;;; Gets the timestamp (seconds from system's Epoch) when the resource was last
;;; modified.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     the number of seconds elapsed from system's Epoch when the resource was
;;;     last modified, or -1 on failure.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_visited ()
;;; 
;;; time_t gtk_recent_info_get_visited (GtkRecentInfo *info);
;;; 
;;; Gets the timestamp (seconds from system's Epoch) when the resource was last
;;; visited.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     the number of seconds elapsed from system's Epoch when the resource was
;;;     last visited, or -1 on failure.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_private_hint ()
;;; 
;;; gboolean gtk_recent_info_get_private_hint (GtkRecentInfo *info);
;;; 
;;; Gets the value of the "private" flag. Resources in the recently used list
;;; that have this flag set to TRUE should only be displayed by the applications
;;; that have registered them.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     TRUE if the private flag was found, FALSE otherwise.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_application_info ()
;;; 
;;; gboolean gtk_recent_info_get_application_info (GtkRecentInfo *info,
;;;                                                const gchar *app_name,
;;;                                                const gchar **app_exec,
;;;                                                guint *count,
;;;                                                time_t *time_);
;;; 
;;; Gets the data regarding the application that has registered the resource
;;; pointed by info.
;;; 
;;; If the command line contains any escape characters defined inside the
;;; storage specification, they will be expanded.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; app_name :
;;;     the name of the application that has registered this item
;;; 
;;; app_exec :
;;;     return location for the string containing the command line
;;; 
;;; count :
;;;     return location for the number of times this item was registered
;;; 
;;; time_ :
;;;     return location for the timestamp this item was last registered for this
;;;     application
;;; 
;;; Returns :
;;;     TRUE if an application with app_name has registered this resource inside
;;;     the recently used list, or FALSE otherwise. The app_exec string is owned
;;;     by the GtkRecentInfo and should not be modified or freed
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_applications ()
;;; 
;;; gchar ** gtk_recent_info_get_applications (GtkRecentInfo *info,
;;;                                            gsize *length);
;;; 
;;; Retrieves the list of applications that have registered this resource.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; length :
;;;     return location for the length of the returned list
;;; 
;;; Returns :
;;;     a newly allocated NULL-terminated array of strings. Use g_strfreev() to
;;;     free it
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_last_application ()
;;; 
;;; gchar * gtk_recent_info_last_application (GtkRecentInfo *info);
;;; 
;;; Gets the name of the last application that have registered the recently used
;;; resource represented by info.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     an application name. Use g_free() to free it.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_has_application ()
;;; 
;;; gboolean gtk_recent_info_has_application (GtkRecentInfo *info,
;;;                                           const gchar *app_name);
;;; 
;;; Checks whether an application registered this resource using app_name.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; app_name :
;;;     a string containing an application name
;;; 
;;; Returns :
;;;     TRUE if an application with name app_name was found, FALSE otherwise.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_create_app_info ()
;;; 
;;; GAppInfo * gtk_recent_info_create_app_info (GtkRecentInfo *info,
;;;                                             const gchar *app_name,
;;;                                             GError **error);
;;; 
;;; Creates a GAppInfo for the specified GtkRecentInfo
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; app_name :
;;;     the name of the application that should be mapped to a GAppInfo; if NULL
;;;     is used then the default application for the MIME type is used
;;; 
;;; error :
;;;     return location for a GError, or NULL
;;; 
;;; Returns :
;;;     the newly created GAppInfo, or NULL. In case of error, error will be set
;;;     either with a GTK_RECENT_MANAGER_ERROR or a G_IO_ERROR
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_groups ()
;;; 
;;; gchar ** gtk_recent_info_get_groups (GtkRecentInfo *info, gsize *length);
;;; 
;;; Returns all groups registered for the recently used item info. The array of
;;; returned group names will be NULL terminated, so length might optionally be
;;; NULL.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; length :
;;;     return location for the number of groups returned
;;; 
;;; Returns :
;;;     a newly allocated NULL terminated array of strings. Use g_strfreev() to
;;;     free it
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_has_group ()
;;; 
;;; gboolean gtk_recent_info_has_group (GtkRecentInfo *info,
;;;                                     const gchar *group_name);
;;; 
;;; Checks whether group_name appears inside the groups registered for the
;;; recently used item info.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; group_name :
;;;     name of a group
;;; 
;;; Returns :
;;;     TRUE if the group was found.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_icon ()
;;; 
;;; GdkPixbuf * gtk_recent_info_get_icon (GtkRecentInfo *info, gint size);
;;; 
;;; Retrieves the icon of size size associated to the resource MIME type.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; size :
;;;     the size of the icon in pixels
;;; 
;;; Returns :
;;;     a GdkPixbuf containing the icon, or NULL. Use g_object_unref() when
;;;     finished using the icon
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_gicon ()
;;; 
;;; GIcon * gtk_recent_info_get_gicon (GtkRecentInfo *info);
;;; 
;;; Retrieves the icon associated to the resource MIME type.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     a GIcon containing the icon, or NULL. Use g_object_unref() when finished
;;;     using the icon
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_short_name ()
;;; 
;;; gchar * gtk_recent_info_get_short_name (GtkRecentInfo *info);
;;; 
;;; Computes a valid UTF-8 string that can be used as the name of the item in a
;;; menu or list. For example, calling this function on an item that refers to
;;; "file:///foo/bar.txt" will yield "bar.txt".
;;; 
;;; info :
;;;     an GtkRecentInfo
;;; 
;;; Returns :
;;;     A newly-allocated string in UTF-8 encoding; free it with g_free().
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_uri_display ()
;;; 
;;; gchar * gtk_recent_info_get_uri_display (GtkRecentInfo *info);
;;; 
;;; Gets a displayable version of the resource's URI. If the resource is local,
;;; it returns a local path; if the resource is not local, it returns the UTF-8
;;; encoded content of gtk_recent_info_get_uri().
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     a newly allocated UTF-8 string containing the resource's URI or NULL.
;;;     Use g_free() when done using it.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_age ()
;;; 
;;; gint gtk_recent_info_get_age (GtkRecentInfo *info);
;;; 
;;; Gets the number of days elapsed since the last update of the resource
;;; pointed by info.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     a positive integer containing the number of days elapsed since the time
;;;     this resource was last modified.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_is_local ()
;;; 
;;; gboolean gtk_recent_info_is_local (GtkRecentInfo *info);
;;; 
;;; Checks whether the resource is local or not by looking at the scheme of its
;;; URI.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     TRUE if the resource is local.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_exists ()
;;; 
;;; gboolean gtk_recent_info_exists (GtkRecentInfo *info);
;;; 
;;; Checks whether the resource pointed by info still exists. At the moment this
;;; check is done only on resources pointing to local files.
;;; 
;;; info :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     TRUE if the resource exists
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_match ()
;;; 
;;; gboolean gtk_recent_info_match (GtkRecentInfo *info_a,
;;;                                 GtkRecentInfo *info_b);
;;; 
;;; Checks whether two GtkRecentInfo structures point to the same resource.
;;; 
;;; info_a :
;;;     a GtkRecentInfo
;;; 
;;; info_b :
;;;     a GtkRecentInfo
;;; 
;;; Returns :
;;;     TRUE if both GtkRecentInfo structures point to se same resource, FALSE
;;;     otherwise.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.recent-manager.lisp ------------------------------------
