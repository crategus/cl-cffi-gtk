;;; ----------------------------------------------------------------------------
;;; gio.app-info.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;; GAppInfo
;;;
;;;     Application information and launch contexts
;;;
;;; Types and Values
;;;
;;;     GAppInfoCreateFlags
;;;     GAppInfo
;;;     GAppInfoIface
;;;     GAppLaunchContext
;;;
;;; Functions
;;;
;;;     g_app_info_create_from_commandline
;;;     g_app_info_dup
;;;     g_app_info_equal
;;;     g_app_info_get_id
;;;     g_app_info_get_name
;;;     g_app_info_get_display_name
;;;     g_app_info_get_description
;;;     g_app_info_get_executable
;;;     g_app_info_get_commandline
;;;     g_app_info_get_icon
;;;     g_app_info_launch
;;;     g_app_info_supports_files
;;;     g_app_info_supports_uris
;;;     g_app_info_launch_uris
;;;     g_app_info_launch_uris_async
;;;     g_app_info_launch_uris_finish
;;;     g_app_info_should_show
;;;     g_app_info_can_delete
;;;     g_app_info_delete
;;;     g_app_info_reset_type_associations
;;;     g_app_info_set_as_default_for_type
;;;     g_app_info_set_as_default_for_extension
;;;     g_app_info_set_as_last_used_for_type
;;;     g_app_info_add_supports_type
;;;     g_app_info_can_remove_supports_type
;;;     g_app_info_remove_supports_type
;;;     g_app_info_get_supported_types
;;;     g_app_info_get_all
;;;     g_app_info_get_all_for_type
;;;     g_app_info_get_default_for_type
;;;     g_app_info_get_default_for_uri_scheme
;;;     g_app_info_get_fallback_for_type
;;;     g_app_info_get_recommended_for_type
;;;     g_app_info_launch_default_for_uri
;;;     g_app_info_launch_default_for_uri_async
;;;     g_app_info_launch_default_for_uri_finish
;;;     g_app_launch_context_setenv
;;;     g_app_launch_context_unsetenv
;;;     g_app_launch_context_get_environment
;;;     g_app_launch_context_get_display
;;;     g_app_launch_context_get_startup_notify_id
;;;     g_app_launch_context_launch_failed
;;;     g_app_launch_context_new
;;;
;;; Signals
;;;
;;;     void    launch-failed    Run Last
;;;     void    launched         Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GFlags
;;;     ╰── GAppInfoCreateFlags
;;;
;;;     GInterface
;;;     ╰── GAppInfo
;;;
;;;     GObject
;;;     ╰── GAppLaunchContext
;;;
;;; Prerequisites
;;;
;;;     GAppInfo requires GObject.
;;;
;;; Known Implementations
;;;
;;;     GAppInfo is implemented by GDesktopAppInfo.
;;;
;;; Description
;;;
;;; GAppInfo and GAppLaunchContext are used for describing and launching
;;; applications installed on the system.
;;;
;;; As of GLib 2.20, URIs will always be converted to POSIX paths (using
;;; g_file_get_path()) when using g_app_info_launch() even if the application
;;; requested an URI and not a POSIX path. For example for an desktop-file based
;;; application with Exec key totem %U and a single URI, sftp://foo/file.avi,
;;; then /home/user/.gvfs/sftp on foo/file.avi will be passed. This will only
;;; work if a set of suitable GIO extensions (such as gvfs 2.26 compiled with
;;; FUSE support), is available and operational; if this is not the case, the
;;; URI will be passed unmodified to the application. Some URIs, such as
;;; mailto:, of course cannot be mapped to a POSIX path (in gvfs there's no
;;; FUSE mount for it); such URIs will be passed unmodified to the application.
;;;
;;; Specifically for gvfs 2.26 and later, the POSIX URI will be mapped back to
;;; the GIO URI in the GFile constructors (since gvfs implements the GVfs
;;; extension point). As such, if the application needs to examine the URI, it
;;; needs to use g_file_get_uri() or similar on GFile. In other words, an
;;; application cannot assume that the URI passed to e.g.
;;; g_file_new_for_commandline_arg() is equal to the result of g_file_get_uri().
;;; The following snippet illustrates this:
;;;
;;;   GFile *f;
;;;   char *uri;
;;;
;;;   file = g_file_new_for_commandline_arg (uri_from_commandline);
;;;
;;;   uri = g_file_get_uri (file);
;;;   strcmp (uri, uri_from_commandline) == 0; // FALSE
;;;   g_free (uri);
;;;
;;;   if (g_file_has_uri_scheme (file, "cdda"))
;;;     {
;;;       // do something special with uri
;;;     }
;;;   g_object_unref (file);
;;;
;;; This code will work when both cdda://sr0/Track 1.wav and
;;; /home/user/.gvfs/cdda on sr0/Track 1.wav is passed to the application. It
;;; should be noted that it's generally not safe for applications to rely on
;;; the format of a particular URIs. Different launcher applications (e.g. file
;;; managers) may have different ideas of what a given URI means.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; enum GAppInfoCreateFlags
;;;
;;; typedef enum {
;;;   /*< nick=none >*/
;;;   G_APP_INFO_CREATE_NONE                           = 0,
;;;   /*< nick=needs-terminal >*/
;;;   G_APP_INFO_CREATE_NEEDS_TERMINAL                 = (1 << 0),
;;;   /*< nick=supports-uris >*/
;;;   G_APP_INFO_CREATE_SUPPORTS_URIS                  = (1 << 1),
;;;   /*< nick=supports-startup-notification >*/
;;;   G_APP_INFO_CREATE_SUPPORTS_STARTUP_NOTIFICATION  = (1 << 2)
;;; } GAppInfoCreateFlags;
;;;
;;; Flags used when creating a GAppInfo.
;;;
;;; G_APP_INFO_CREATE_NONE
;;;     No flags.
;;;
;;; G_APP_INFO_CREATE_NEEDS_TERMINAL
;;;     Application opens in a terminal window.
;;;
;;; G_APP_INFO_CREATE_SUPPORTS_URIS
;;;     Application supports URI arguments.
;;;
;;; G_APP_INFO_CREATE_SUPPORTS_STARTUP_NOTIFICATION
;;;     Application supports startup notification. Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GAppInfo
;;; ----------------------------------------------------------------------------

(define-g-interface "GAppInfo" g-app-info
  (:export t
   :type-initializer "g_app_info_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-app-info atdoc:*class-name-alias*)
      "Interface"
      (documentation 'g-app-info 'type)
 "@version{2021-4-13}
  @begin{short}
    Information about an installed application and methods to launch it with
    file arguments.
  @end{short}
  @see-class{g-app-launch-context}")

;;; ----------------------------------------------------------------------------
;;; struct GAppInfoIface
;;;
;;; struct GAppInfoIface {
;;;   GTypeInterface g_iface;
;;;
;;;   /* Virtual Table */
;;;
;;;   GAppInfo *   (* dup)                  (GAppInfo           *appinfo);
;;;   gboolean     (* equal)                (GAppInfo           *appinfo1,
;;;                                          GAppInfo           *appinfo2);
;;;   const char * (* get_id)               (GAppInfo           *appinfo);
;;;   const char * (* get_name)             (GAppInfo           *appinfo);
;;;   const char * (* get_description)      (GAppInfo           *appinfo);
;;;   const char * (* get_executable)       (GAppInfo           *appinfo);
;;;   GIcon *      (* get_icon)             (GAppInfo           *appinfo);
;;;   gboolean     (* launch)               (GAppInfo           *appinfo,
;;;                                          GList              *files,
;;;                                          GAppLaunchContext  *launch_context,
;;;                                          GError            **error);
;;;   gboolean     (* supports_uris)        (GAppInfo           *appinfo);
;;;   gboolean     (* supports_files)       (GAppInfo           *appinfo);
;;;   gboolean     (* launch_uris)          (GAppInfo           *appinfo,
;;;                                          GList              *uris,
;;;                                          GAppLaunchContext  *launch_context,
;;;                                          GError            **error);
;;;   gboolean     (* should_show)          (GAppInfo           *appinfo);
;;;
;;;   /* For changing associations */
;;;   gboolean     (* set_as_default_for_type)
;;;                                         (GAppInfo           *appinfo,
;;;                                          const char         *content_type,
;;;                                          GError            **error);
;;;   gboolean     (* set_as_default_for_extension)
;;;                                         (GAppInfo           *appinfo,
;;;                                          const char         *extension,
;;;                                          GError            **error);
;;;   gboolean     (* add_supports_type)    (GAppInfo           *appinfo,
;;;                                          const char         *content_type,
;;;                                          GError            **error);
;;;   gboolean     (* can_remove_supports_type)
;;;                                         (GAppInfo           *appinfo);
;;;   gboolean     (* remove_supports_type) (GAppInfo           *appinfo,
;;;                                          const char         *content_type,
;;;                                          GError            **error);
;;;   gboolean     (* can_delete)           (GAppInfo           *appinfo);
;;;   gboolean     (* do_delete)            (GAppInfo           *appinfo);
;;;   const char * (* get_commandline)      (GAppInfo           *appinfo);
;;;   const char * (* get_display_name)     (GAppInfo           *appinfo);
;;;   gboolean     (* set_as_last_used_for_type)
;;;                                         (GAppInfo           *appinfo,
;;;                                          const char         *content_type,
;;;                                          GError            **error);
;;; };
;;;
;;; Application Information interface, for operating system portability.
;;;
;;; GTypeInterface g_iface;
;;;     The parent interface.
;;;
;;; dup ()
;;;     Copies a GAppInfo.
;;;
;;; equal ()
;;;     Checks two GAppInfos for equality.
;;;
;;; get_id ()
;;;     Gets a string identifier for a GAppInfo.
;;;
;;; get_name ()
;;;     Gets the name of the application for a GAppInfo.
;;;
;;; get_description ()
;;;     Gets a short description for the application described by the GAppInfo.
;;;
;;; get_executable ()
;;;     Gets the executable name for the GAppInfo.
;;;
;;; get_icon ()
;;;     Gets the GIcon for the GAppInfo.
;;;
;;; launch ()
;;;     Launches an application specified by the GAppInfo.
;;;
;;; supports_uris ()
;;;     Indicates whether the application specified supports launching URIs.
;;;
;;; supports_files ()
;;;     Indicates whether the application specified accepts filename arguments.
;;;
;;; launch_uris ()
;;;     Launches an application with a list of URIs.
;;;
;;; should_show ()
;;;     Returns whether an application should be shown (e.g. when getting a list
;;;     of installed applications). FreeDesktop.Org Startup Notification
;;;     Specification.
;;;
;;; set_as_default_for_type ()
;;;     Sets an application as default for a given content type.
;;;
;;; set_as_default_for_extension ()
;;;     Sets an application as default for a given file extension.
;;;
;;; add_supports_type ()
;;;     Adds to the GAppInfo information about supported file types.
;;;
;;; can_remove_supports_type ()
;;;     Checks for support for removing supported file types from a GAppInfo.
;;;
;;; remove_supports_type ()
;;;     Removes a supported application type from a GAppInfo.
;;;
;;; can_delete ()
;;;     Checks if a GAppInfo can be deleted. Since 2.20
;;;
;;; do_delete ()
;;;     Deletes a GAppInfo. Since 2.20
;;;
;;; get_commandline ()
;;;     Gets the commandline for the GAppInfo. Since 2.20
;;;
;;; get_display_name ()
;;;     Gets the display name for the GAppInfo. Since 2.24
;;;
;;; set_as_last_used_for_type ()
;;;     Sets the application as the last used. See
;;;     g_app_info_set_as_last_used_for_type().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GAppLaunchContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GAppLaunchContext" g-app-launch-context
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "g_app_launch_context_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-app-launch-context 'type)
 "@version{2021-4-13}
  @begin{short}
    Integrating the launch with the launching application.
  @end{short}
  This is used to handle for instance startup notification and launching the
  new application on the same screen as the launching window.
  @begin[Signal Details]{dictionary}
    @subheading{The \"launch-failed\" signal}
      The signal is emitted when a @class{g-app-info} launch fails. The startup
      notification ID is provided, so that the launcher can cancel the startup
      notification.
      @begin{pre}
 lambda (context startup-notify-id)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[context]{The @sym{g-app-launch-context} object emitting the
          signal.}
        @entry[startup-notify-id]{A string with the startup notification ID
          for the failed launch.}
      @end{table}
    @subheading{The \"launched\" signal}
      The signal is emitted when a @class{g-app-info} object is successfully
      launched. The argument @arg{platform-data} is an @type{g-variant}
      dictionary mapping strings to variants, i.e. @code{a{sv@}}, which contains
      additional, platform-specific data about this launch. On UNIX, at least
      the \"pid\" and \"startup-notification-id\" keys will be present.
      @begin{pre}
 lambda (context info platform-data)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[context]{The @sym{g-app-launch-context} object emitting the
          signal.}
        @entry[info]{The @class{g-app-info} object that was just launched.}
        @entry[platform-data]{A @type{g-variant} instance with additional
          platform-specific data for this launch.}
      @end{table}
  @end{dictionary}
  @see-class{g-app-info}
  @see-class{gdk-app-launch-context}")

;;; ----------------------------------------------------------------------------
;;; g_app_info_create_from_commandline ()
;;;
;;; GAppInfo * g_app_info_create_from_commandline (const char *commandline,
;;;                                                const char *application_name,
;;;                                                GAppInfoCreateFlags flags,
;;;                                                GError **error);
;;;
;;; Creates a new GAppInfo from the given information.
;;;
;;; Note that for commandline, the quoting rules of the Exec key of the
;;; freedesktop.org Desktop Entry Specification are applied. For example, if
;;; the commandline contains percent-encoded URIs, the percent-character must
;;; be doubled in order to prevent it from being swallowed by Exec key
;;; unquoting. See the specification for exact quoting rules.
;;;
;;; commandline :
;;;     the commandline to use
;;;
;;; application_name :
;;;     the application name, or NULL to use commandline
;;;
;;; flags :
;;;     flags that can specify details of the created GAppInfo
;;;
;;; error :
;;;     a GError location to store the error occurring, NULL to ignore.
;;;
;;; Returns :
;;;     new GAppInfo for given command
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_dup ()
;;;
;;; GAppInfo * g_app_info_dup (GAppInfo *appinfo);
;;;
;;; Creates a duplicate of a GAppInfo.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     a duplicate of appinfo
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_equal ()
;;;
;;; gboolean g_app_info_equal (GAppInfo *appinfo1, GAppInfo *appinfo2);
;;;
;;; Checks if two GAppInfos are equal.
;;;
;;; appinfo1 :
;;;     the first GAppInfo.
;;;
;;; appinfo2 :
;;;     the second GAppInfo.
;;;
;;; Returns :
;;;     TRUE if appinfo1 is equal to appinfo2. FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_id ()
;;;
;;; const char * g_app_info_get_id (GAppInfo *appinfo);
;;;
;;; Gets the ID of an application. An id is a string that identifies the
;;; application. The exact format of the id is platform dependent. For instance,
;;; on Unix this is the desktop file id from the xdg menu specification.
;;;
;;; Note that the returned ID may be NULL, depending on how the appinfo has
;;; been constructed.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     a string containing the application's ID.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_name ()
;;;
;;; const char * g_app_info_get_name (GAppInfo *appinfo);
;;;
;;; Gets the installed name of the application.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     the name of the application for appinfo.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_display_name ()
;;;
;;; const char * g_app_info_get_display_name (GAppInfo *appinfo);
;;;
;;; Gets the display name of the application. The display name is often more
;;; descriptive to the user than the name itself.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     the display name of the application for appinfo, or the name if no
;;;     display name is available.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_description ()
;;;
;;; const char * g_app_info_get_description (GAppInfo *appinfo);
;;;
;;; Gets a human-readable description of an installed application.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     a string containing a description of the application appinfo, or NULL
;;;     if none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_executable ()
;;;
;;; const char * g_app_info_get_executable (GAppInfo *appinfo);
;;;
;;; Gets the executable's name for the installed application.
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; Returns :
;;;     a string containing the appinfo's application binaries name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_commandline ()
;;;
;;; const char * g_app_info_get_commandline (GAppInfo *appinfo);
;;;
;;; Gets the commandline with which the application will be started.
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; Returns :
;;;     a string containing the appinfo's commandline, or NULL if this
;;;     information is not available
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_icon ()
;;;
;;; GIcon * g_app_info_get_icon (GAppInfo *appinfo);
;;;
;;; Gets the icon for the application.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     the default GIcon for appinfo or NULL if there is no default icon
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch ()
;;;
;;; gboolean g_app_info_launch (GAppInfo *appinfo,
;;;                             GList *files,
;;;                             GAppLaunchContext *launch_context,
;;;                             GError **error);
;;;
;;; Launches the application. Passes files to the launched application as
;;; arguments, using the optional launch_context to get information about the
;;; details of the launcher (like what screen it is on). On error, error will
;;; be set accordingly.
;;;
;;; To launch the application without arguments pass a NULL files list.
;;;
;;; Note that even if the launch is successful the application launched can
;;; fail to start if it runs into problems during startup. There is no way to
;;; detect this.
;;;
;;; Some URIs can be changed when passed through a GFile (for instance
;;; unsupported URIs with strange formats like mailto:), so if you have a
;;; textual URI you want to pass in as argument, consider using
;;; g_app_info_launch_uris() instead.
;;;
;;; The launched application inherits the environment of the launching process,
;;; but it can be modified with g_app_launch_context_setenv() and
;;; g_app_launch_context_unsetenv().
;;;
;;; On UNIX, this function sets the GIO_LAUNCHED_DESKTOP_FILE environment
;;; variable with the path of the launched desktop file and
;;; GIO_LAUNCHED_DESKTOP_FILE_PID to the process id of the launched process.
;;; This can be used to ignore GIO_LAUNCHED_DESKTOP_FILE, should it be inherited
;;; by further processes. The DISPLAY and DESKTOP_STARTUP_ID environment
;;; variables are also set, based on information provided in launch_context.
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; files :
;;;     a GList of GFile objects
;;;
;;; launch_context :
;;;     a GAppLaunchContext or NULL
;;;
;;; error :
;;;     a GError
;;;
;;; Returns :
;;;     TRUE on successful launch, FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_supports_files ()
;;;
;;; gboolean g_app_info_supports_files (GAppInfo *appinfo);
;;;
;;; Checks if the application accepts files as arguments.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     TRUE if the appinfo supports files.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_supports_uris ()
;;;
;;; gboolean g_app_info_supports_uris (GAppInfo *appinfo);
;;;
;;; Checks if the application supports reading files and directories from URIs.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     TRUE if the appinfo supports URIs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_uris ()
;;;
;;; gboolean g_app_info_launch_uris (GAppInfo *appinfo,
;;;                                  GList *uris,
;;;                                  GAppLaunchContext *launch_context,
;;;                                  GError **error);
;;;
;;; Launches the application. This passes the uris to the launched application
;;; as arguments, using the optional launch_context to get information about
;;; the details of the launcher (like what screen it is on). On error, error
;;; will be set accordingly.
;;;
;;; To launch the application without arguments pass a NULL uris list.
;;;
;;; Note that even if the launch is successful the application launched can
;;; fail to start if it runs into problems during startup. There is no way to
;;; detect this.
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; uris :
;;;     a GList containing URIs to launch
;;;
;;; launch_context :
;;;     a GAppLaunchContext or NULL
;;;
;;; error :
;;;     a GError
;;;
;;; Returns :
;;;     TRUE on successful launch, FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_uris_async ()
;;;
;;; void
;;; g_app_info_launch_uris_async (GAppInfo *appinfo,
;;;                               GList *uris,
;;;                               GAppLaunchContext *context,
;;;                               GCancellable *cancellable,
;;;                               GAsyncReadyCallback callback,
;;;                               gpointer user_data);
;;;
;;; Async version of g_app_info_launch_uris().
;;;
;;; The callback is invoked immediately after the application launch, but it
;;; waits for activation in case of D-Bus–activated applications and also
;;; provides extended error information for sandboxed applications, see notes
;;; for g_app_info_launch_default_for_uri_async().
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; uris :
;;;     a GList containing URIs to launch.
;;;
;;; context :
;;;     a GAppLaunchContext or NULL.
;;;
;;; cancellable :
;;;     a GCancellable.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is done.
;;;
;;; user_data :
;;;     data to pass to callback .
;;;
;;; Since 2.60
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_uris_finish ()
;;;
;;; gboolean
;;; g_app_info_launch_uris_finish (GAppInfo *appinfo,
;;;                                GAsyncResult *result,
;;;                                GError **error);
;;;
;;; Finishes a g_app_info_launch_uris_async() operation.
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on successful launch, FALSE otherwise.
;;;
;;; Since 2.60
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_should_show ()
;;;
;;; gboolean g_app_info_should_show (GAppInfo *appinfo);
;;;
;;; Checks if the application info should be shown in menus that list available
;;; applications.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     TRUE if the appinfo should be shown, FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_can_delete ()
;;;
;;; gboolean g_app_info_can_delete (GAppInfo *appinfo);
;;;
;;; Obtains the information whether the GAppInfo can be deleted. See
;;; g_app_info_delete().
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; Returns :
;;;     TRUE if appinfo can be deleted
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_delete ()
;;;
;;; gboolean g_app_info_delete (GAppInfo *appinfo);
;;;
;;; Tries to delete a GAppInfo.
;;;
;;; On some platforms, there may be a difference between user-defined GAppInfos
;;; which can be deleted, and system-wide ones which cannot. See
;;; g_app_info_can_delete().
;;;
;;; Virtual: do_delete
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; Returns :
;;;     TRUE if appinfo has been deleted
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_reset_type_associations ()
;;;
;;; void g_app_info_reset_type_associations (const char *content_type);
;;;
;;; Removes all changes to the type associations done by
;;; g_app_info_set_as_default_for_type(),
;;; g_app_info_set_as_default_for_extension(), g_app_info_add_supports_type()
;;; or g_app_info_remove_supports_type().
;;;
;;; content_type :
;;;     a content type
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_default_for_type ()
;;;
;;; gboolean g_app_info_set_as_default_for_type (GAppInfo *appinfo,
;;;                                              const char *content_type,
;;;                                              GError **error);
;;;
;;; Sets the application as the default handler for a given type.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; content_type :
;;;     the content type.
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on success, FALSE on error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_default_for_extension ()
;;;
;;; gboolean g_app_info_set_as_default_for_extension (GAppInfo *appinfo,
;;;                                                   const char *extension,
;;;                                                   GError **error);
;;;
;;; Sets the application as the default handler for the given file extension.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; extension :
;;;     a string containing the file extension (without the dot).
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on success, FALSE on error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_last_used_for_type ()
;;;
;;; gboolean g_app_info_set_as_last_used_for_type (GAppInfo *appinfo,
;;;                                                const char *content_type,
;;;                                                GError **error);
;;;
;;; Sets the application as the last used application for a given type. This
;;; will make the application appear as first in the list returned by
;;; g_app_info_get_recommended_for_type(), regardless of the default
;;; application for that content type.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; content_type :
;;;     the content type.
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on success, FALSE on error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_add_supports_type ()
;;;
;;; gboolean g_app_info_add_supports_type (GAppInfo *appinfo,
;;;                                        const char *content_type,
;;;                                        GError **error);
;;;
;;; Adds a content type to the application information to indicate the
;;; application is capable of opening files with the given content type.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; content_type :
;;;     a string.
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on success, FALSE on error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_can_remove_supports_type ()
;;;
;;; gboolean g_app_info_can_remove_supports_type (GAppInfo *appinfo);
;;;
;;; Checks if a supported content type can be removed from an application.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     TRUE if it is possible to remove supported content types from a given
;;;     appinfo, FALSE if not.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_remove_supports_type ()
;;;
;;; gboolean g_app_info_remove_supports_type (GAppInfo *appinfo,
;;;                                           const char *content_type,
;;;                                           GError **error);
;;;
;;; Removes a supported type from an application, if possible.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; content_type :
;;;     a string.
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on success, FALSE on error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_supported_types ()
;;;
;;; const char ** g_app_info_get_supported_types (GAppInfo *appinfo);
;;;
;;; Retrieves the list of content types that app_info claims to support. If
;;; this information is not provided by the environment, this function will
;;; return NULL. This function does not take in consideration associations
;;; added with g_app_info_add_supports_type(), but only those exported directly
;;; by the application.
;;;
;;; appinfo :
;;;     a GAppInfo that can handle files
;;;
;;; Returns :
;;;     a list of content types.
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_all ()
;;;
;;; GList * g_app_info_get_all (void);
;;;
;;; Gets a list of all of the applications currently registered on this system.
;;;
;;; For desktop files, this includes applications that have NoDisplay=true set
;;; or are excluded from display by means of OnlyShowIn or NotShowIn. See
;;; g_app_info_should_show(). The returned list does not include applications
;;; which have the Hidden key set.
;;;
;;; Returns :
;;;     a newly allocated GList of references to GAppInfos
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_all_for_type ()
;;;
;;; GList * g_app_info_get_all_for_type (const char *content_type);
;;;
;;; Gets a list of all GAppInfos for a given content type, including the
;;; recommended and fallback GAppInfos. See
;;; g_app_info_get_recommended_for_type() and g_app_info_get_fallback_for_type().
;;;
;;; content_type :
;;;     the content type to find a GAppInfo for
;;;
;;; Returns :
;;;     GList of GAppInfos for given content_type or NULL on error
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_type ()
;;;
;;; GAppInfo * g_app_info_get_default_for_type (const char *content_type,
;;;                                             gboolean must_support_uris);
;;;
;;; Gets the default GAppInfo for a given content type.
;;;
;;; content_type :
;;;     the content type to find a GAppInfo for
;;;
;;; must_support_uris :
;;;     if TRUE, the GAppInfo is expected to support URIs
;;;
;;; Returns :
;;;     GAppInfo for given content_type or NULL on error
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_uri_scheme ()
;;;
;;; GAppInfo * g_app_info_get_default_for_uri_scheme (const char *uri_scheme);
;;;
;;; Gets the default application for handling URIs with the given URI scheme. A
;;; URI scheme is the initial part of the URI, up to but not including the ':',
;;; e.g. "http", "ftp" or "sip".
;;;
;;; uri_scheme :
;;;     a string containing a URI scheme.
;;;
;;; Returns :
;;;     GAppInfo for given uri_scheme or NULL on error
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_fallback_for_type ()
;;;
;;; GList * g_app_info_get_fallback_for_type (const gchar *content_type);
;;;
;;; Gets a list of fallback GAppInfos for a given content type, i.e. those
;;; applications which claim to support the given content type by MIME type
;;; subclassing and not directly.
;;;
;;; content_type :
;;;     the content type to find a GAppInfo for
;;;
;;; Returns :
;;;     GList of GAppInfos for given content_type or NULL on error
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_recommended_for_type ()
;;;
;;; GList * g_app_info_get_recommended_for_type (const gchar *content_type);
;;;
;;; Gets a list of recommended GAppInfos for a given content type, i.e. those
;;; applications which claim to support the given content type exactly, and not
;;; by MIME type subclassing. Note that the first application of the list is the
;;; last used one, i.e. the last one for which
;;; g_app_info_set_as_last_used_for_type() has been called.
;;;
;;; content_type :
;;;     the content type to find a GAppInfo for
;;;
;;; Returns :
;;;     GList of GAppInfos for given content_type or NULL on error
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_default_for_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_app_info_launch_default_for_uri"
          %g-app-info-launch-default-for-uri) :boolean
  (uri :string)
  (context (g-object g-app-launch-context))
  (err :pointer))

(defun g-app-info-launch-default-for-uri (uri context)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-14}
  @argument[uri]{a string with the URI to show}
  @argument[context]{an optional @class{g-app-launch-context} object}
  @return{@em{True} on sucess, @em{false} on error.}
  @begin{short}
    Utility function that launches the default application registered to handle
    the specified URI.
  @end{short}
  Synchronous I/O is done on the URI to detect the type of the file if required.
  @see-class{g-app-info}"
  (with-ignore-g-error (err)
    (%g-app-info-launch-default-for-uri uri context err)))

(export 'g-app-info-launch-default-for-uri)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_default_for_uri_async ()
;;;
;;; void
;;; g_app_info_launch_default_for_uri_async (const char *uri,
;;;                                          GAppLaunchContext *context,
;;;                                          GCancellable *cancellable,
;;;                                          GAsyncReadyCallback callback,
;;;                                          gpointer user_data);
;;;
;;; Async version of g_app_info_launch_default_for_uri().
;;;
;;; This version is useful if you are interested in receiving error information
;;; in the case where the application is sandboxed and the portal may present
;;; an application chooser dialog to the user.
;;;
;;; This is also useful if you want to be sure that the D-Bus–activated
;;; applications are really started before termination and if you are
;;; interested in receiving error information from their activation.
;;;
;;; uri :
;;;     the uri to show
;;;
;;; context :
;;;     an optional GAppLaunchContext.
;;;
;;; cancellable :
;;;     a GCancellable.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is done.
;;;
;;; user_data :
;;;     data to pass to callback .
;;;
;;; Since 2.50
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_default_for_uri_finish ()
;;;
;;; gboolean
;;; g_app_info_launch_default_for_uri_finish (GAsyncResult *result,
;;;                                           GError **error);
;;;
;;; Finishes an asynchronous launch-default-for-uri operation.
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     return location for an error, or NULL.
;;;
;;; Returns :
;;;     TRUE if the launch was successful, FALSE if error is set
;;;
;;; Since 2.50
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_setenv ()
;;;
;;; void g_app_launch_context_setenv (GAppLaunchContext *context,
;;;                                   const char *variable,
;;;                                   const char *value);
;;;
;;; Arranges for variable to be set to value in the child's environment when
;;; context is used to launch an application.
;;;
;;; context :
;;;     a GAppLaunchContext
;;;
;;; variable :
;;;     the environment variable to set
;;;
;;; value :
;;;     the value for to set the variable to.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_unsetenv ()
;;;
;;; void g_app_launch_context_unsetenv (GAppLaunchContext *context,
;;;                                     const char *variable);
;;;
;;; Arranges for variable to be unset in the child's environment when context
;;; is used to launch an application.
;;;
;;; context :
;;;     a GAppLaunchContext
;;;
;;; variable :
;;;     the environment variable to remove
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_environment ()
;;;
;;; char ** g_app_launch_context_get_environment (GAppLaunchContext *context);
;;;
;;; Gets the complete environment variable list to be passed to the child
;;; process when context is used to launch an application. This is a
;;; NULL-terminated array of strings, where each string has the form KEY=VALUE.
;;;
;;; context :
;;;     a GAppLaunchContext
;;;
;;; Returns :
;;;     the child's environment
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_display ()
;;;
;;; char * g_app_launch_context_get_display (GAppLaunchContext *context,
;;;                                          GAppInfo *info,
;;;                                          GList *files);
;;;
;;; Gets the display string for the context. This is used to ensure new
;;; applications are started on the same display as the launching application,
;;; by setting the DISPLAY environment variable.
;;;
;;; context :
;;;     a GAppLaunchContext
;;;
;;; info :
;;;     a GAppInfo
;;;
;;; files :
;;;     a GList of GFile objects
;;;
;;; Returns :
;;;     a display string for the display.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_startup_notify_id ()
;;;
;;; char *
;;; g_app_launch_context_get_startup_notify_id (GAppLaunchContext *context,
;;;                                             GAppInfo *info,
;;;                                             GList *files);
;;;
;;; Initiates startup notification for the application and returns the
;;; DESKTOP_STARTUP_ID for the launched operation, if supported.
;;;
;;; Startup notification IDs are defined in the FreeDesktop.Org Startup
;;; Notifications standard.
;;;
;;; context :
;;;     a GAppLaunchContext
;;;
;;; info :
;;;     a GAppInfo
;;;
;;; files :
;;;     a GList of of GFile objects
;;;
;;; Returns :
;;;     a startup notification ID for the application, or NULL if not supported.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_launch_failed ()
;;;
;;; void
;;; g_app_launch_context_launch_failed (GAppLaunchContext *context,
;;;                                     const char *startup_notify_id);
;;;
;;; Called when an application has failed to launch, so that it can cancel the
;;; application startup notification started in
;;; g_app_launch_context_get_startup_notify_id().
;;;
;;; context :
;;;     a GAppLaunchContext.
;;;
;;; startup_notify_id :
;;;     the startup notification id that was returned by
;;;     g_app_launch_context_get_startup_notify_id().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_new ()
;;;
;;; GAppLaunchContext * g_app_launch_context_new (void);
;;;
;;; Creates a new application launch context. This is not normally used,
;;; instead you instantiate a subclass of this, such as GdkAppLaunchContext.
;;;
;;; Returns :
;;;     a GAppLaunchContext.
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.app-info.lisp ------------------------------------------
