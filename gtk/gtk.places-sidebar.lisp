;;; ----------------------------------------------------------------------------
;;; gtk.places-sidebar.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkPlacesSidebar
;;;
;;;     Sidebar that displays frequently-used places in the file system
;;;
;;; Types and Values
;;;
;;;     GtkPlacesSidebar
;;;     GtkPlacesOpenFlags
;;;
;;; Functions
;;;
;;;     gtk_places_sidebar_new
;;;     gtk_places_sidebar_set_open_flags                  Accessor
;;;     gtk_places_sidebar_get_open_flags                  Accessor
;;;     gtk_places_sidebar_set_location                    Accessor
;;;     gtk_places_sidebar_get_location                    Accessor
;;;     gtk_places_sidebar_set_show_recent                 Accessor
;;;     gtk_places_sidebar_get_show_recent                 Accessor
;;;     gtk_places_sidebar_set_show_desktop                Accessor
;;;     gtk_places_sidebar_get_show_desktop                Accessor
;;;     gtk_places_sidebar_add_shortcut
;;;     gtk_places_sidebar_remove_shortcut
;;;     gtk_places_sidebar_list_shortcuts
;;;     gtk_places_sidebar_get_nth_bookmark
;;;     gtk_places_sidebar_get_show_connect_to_server      Accessor
;;;     gtk_places_sidebar_set_show_connect_to_server      Accessor
;;;     gtk_places_sidebar_get_local_only                  Accessor
;;;     gtk_places_sidebar_set_local_only                  Accessor
;;;     gtk_places_sidebar_get_show_enter_location         Accessor
;;;     gtk_places_sidebar_set_show_enter_location         Accessor
;;;     gtk_places_sidebar_get_show_trash                  Accessor
;;;     gtk_places_sidebar_set_show_trash                  Accessor
;;;     gtk_places_sidebar_get_show_other_locations        Accessor
;;;     gtk_places_sidebar_set_show_other_locations        Accessor
;;;     gtk_places_sidebar_set_drop_targets_visible
;;;
;;; Properties
;;;
;;;           gboolean   local-only                         Read / Write
;;;              GFile*  location                           Read / Write
;;; GtkPlacesOpenFlags   open-flags                         Read / Write
;;;           gboolean   populate-all                       Read / Write
;;;           gboolean   show-connect-to-server             Read / Write
;;;           gboolean   show-desktop                       Read / Write
;;;           gboolean   show-enter-location                Read / Write
;;;           gboolean   show-other-locations               Read / Write
;;;           gboolean   show-recent                        Read / Write
;;;           gboolean   show-starred-location              Read / Write
;;;           gboolean   show-trash                         Read / Write
;;;
;;; Signals
;;;
;;;               gint   drag-action-ask                    Run Last
;;;               gint   drag-action-requested              Run Last
;;;               void   drag-perform-drop                  Run First
;;;               void   mount                              Run First
;;;               void   open-location                      Run First
;;;               void   populate-popup                     Run First
;;;               void   show-connect-to-server             Run First
;;;               void   show-enter-location                Run First
;;;               void   show-error-message                 Run First
;;;               void   show-other-locations               Run First
;;;               void   show-other-locations-with-flags    Run First
;;;               void   show-starred-location              Run First
;;;               void   unmount                            Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkScrolledWindow
;;;                         ╰── GtkPlacesSidebar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPlacesSidebar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkPlacesOpenFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkPlacesOpenFlags" gtk-places-open-flags
  (:export t
   :type-initializer "gtk_places_open_flags_get_type")
  (:normal     #.(ash 1 0))
  (:new-tab    #.(ash 1 1))
  (:new-window #.(ash 1 2)))  

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-places-open-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-places-open-flags atdoc:*external-symbols*)
 "@version{2019-4-6}
  @begin{short}
    These flags serve two purposes.
  @end{short}
  First, the application can call the @fun{gtk-places-sidebar-set-open-flags}
  function using these flags as a bitmask. This tells the sidebar that the
  application is able to open folders selected from the sidebar in various ways,
  for example, in new tabs or in new windows in addition to the normal mode.

  Second, when one of these values gets passed back to the application in the
  \"open-location\" signal, it means that the application should open the
  selected location in the normal way, in a new tab, or in a new window. The
  sidebar takes care of determining the desired way to open the location, based
  on the modifier keys that the user is pressing at the time the selection is
  made.

  If the application never calls the @fun{gtk-places-sidebar-set-open-flags}
  function, then the sidebar will only use @code{:normal} in the 
  \"open-location\" signal. This is the default mode of operation.
  @begin{pre}
(define-g-flags \"GtkPlacesOpenFlags\" gtk-places-open-flags
  (:export t
   :type-initializer \"gtk_places_open_flags_get_type\")
  (:normal     #.(ash 1 0))
  (:new-tab    #.(ash 1 1))
  (:new-window #.(ash 1 2)))
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{This is the default mode that GtkPlacesSidebar uses if no
      other flags are specified. It indicates that the calling application
      should open the selected location in the normal way, for example, in the
      folder view beside the sidebar.}
    @entry[:new-tab]{When passed to the @fun{gtk-places-sidebar-set-open-flags}
      function, this indicates that the application can open folders selected
      from the sidebar in new tabs. This value will be passed to the
      \"open-location\" signal when the user selects that a location be opened
      in a new tab instead of in the standard fashion.}
    @entry[:new-window]{Similar to @code{:new-tab}, but indicates that the
      application can open folders in new windows.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; struct GtkPlacesSidebar
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkPlacesSidebar" 'gtk-places-sidebar))

(define-g-object-class "GtkPlacesSidebar" gtk-places-sidebar
  (:superclass gtk-scrolled-window
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_places_sidebar_get_type")
  ((local-only
    gtk-places-sidebar-local-only
    "local-only" "gboolean" t t)
   (location
    gtk-places-sidebar-location
    "location" "GFile" t t)
   (open-flags
    gtk-places-sidebar-open-flags
    "open-flags" "GtkPlacesOpenFlags" t t)
   #+gtk-3-18
   (populate-all
    gtk-places-sidebar-populate-all
    "populate-all" "gboolean" t t)
   (show-connect-to-server
    gtk-places-sidebar-show-connect-to-server
    "show-connect-to-server" "gboolean" t t)
   (show-desktop
    gtk-places-sidebar-show-desktop
    "show-desktop" "gboolean" t t)
   (show-enter-location
    gtk-places-sidebar-show-enter-location
    "show-enter-location" "gboolean" t t)
   (show-other-locations
    gtk-places-sidebar-show-other-locations
    "show-other-locations" "gboolean" t t)
   (show-recent
    gtk-places-sidebar-show-recent
    "show-recent" "gboolean" t t)
   (show-starred-location
    gtk-places-sidebar-show-starred-location
    "show-starred-location" "gboolean" t t)
   (show-trash
    gtk-places-sidebar-show-trash
    "show-trash" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-places-sidebar 'type)
 "@version{2019-4-6}
  @begin{short}
    The @sym{gtk-places-sidebar} is a widget that displays a list of
    frequently-used places in the file system: the user’s home directory, the
    user’s bookmarks, and volumes and drives.
  @end{short}
  This widget is used as a sidebar in the @class{gtk-file-chooser} interface and
  may be used by file managers and similar programs.

  The places sidebar displays drives and volumes, and will automatically mount
  or unmount them when the user selects them.

  Applications can hook to various signals in the places sidebar to customize
  its behavior. For example, they can add extra commands to the context menu of
  the sidebar.

  While bookmarks are completely in control of the user, the places sidebar also
  allows individual applications to provide extra shortcut folders that are
  unique to each application. For example, a Paint program may want to add a
  shortcut for a Clipart folder. You can do this with the
  @fun{gtk-places-sidebar-add-shortcut} function.

  To make use of the places sidebar, an application at least needs to connect to
  the \"open-location\" signal. This is emitted when the user selects in the
  sidebar a location to open. The application should also call the
  @fun{gtk-places-sidebar-set-location} function when it changes the
  currently-viewed location.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-places-sidebar} class uses a single CSS node with name
    @code{placessidebar} and style class @code{.sidebar}.

    Among the children of the places sidebar, the following style classes can be
    used:
    @begin{itemize}
      @item{@code{.sidebar-new-bookmark-row} for the 'Add new bookmark' row}
      @item{@code{.sidebar-placeholder-row} for a row that is a placeholder}
      @item{@code{.has-open-popup} when a popup is open for a row}
    @end{itemize}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"drag-action-ask\" signal}
      @begin{pre}
 lambda (sidebar actions)    : Run Last
      @end{pre}
      The places sidebar emits this signal when it needs to ask the application
      to pop up a menu to ask the user for which drag action to perform.
      @begin[code]{table}
        @entry[sidebar]{The object which received the signal.}
        @entry[actions]{Possible drag actions that need to be asked for.}
        @entry[Returns]{The final drag action that the sidebar should pass to
          the drag side of the drag-and-drop operation.}
      @end{table}
      Since 3.10

    @subheading{The \"drag-action-requested\" signal}
      @begin{pre}
 lambda (sidebar context dest-file source-file-list)    : Run Last
      @end{pre}
      When the user starts a drag-and-drop operation and the sidebar needs to
      ask the application for which drag action to perform, then the sidebar
      will emit this signal.

      The application can evaluate the context for customary actions, or it can
      check the type of the files indicated by @arg{source-file-list} against
      the possible actions for the destination @arg{dest-file}.

      The drag action to use must be the return value of the signal handler.
      @begin[code]{table}
        @entry[sidebar]{The object which received the signal.} 
        @entry[context]{@class{gdk-drag-context} with information about the drag
          operation.}
        @entry[dest-file]{@class{g-file} with the tentative location that is
          being hovered for a drop.}
        @entry[source-file-list]{List of @class{g-file} that are being dragged.}
        @entry[Returns]{The drag action to use, for example,
        @code{GDK_ACTION_COPY} or @code{GDK_ACTION_MOVE}, or 0 if no action is
        allowed here, i. e. drops are not allowed in the specified
        @arg{dest-file}.}
      @end{table}
      Since 3.10

    @subheading{The \"drag-perform-drop\" signal}
      @begin{pre}
 lambda (sidebar dest-file source-file-list action)    : Run First
      @end{pre}
      The places sidebar emits this signal when the user completes a
      drag-and-drop operation and one of the sidebar's items is the destination.
      This item is in the @arg{dest-file}, and the @arg{source-file-list} has
      the list of files that are dropped into it and which should be
      copied/moved/etc. based on the specified @arg{action}.
      @begin[code]{table}
        @entry[sidebar]{The object which received the signal.}
        @entry[dest-file]{Destination @class{g-file}.}
        @entry[source-file-list]{List of @class{g-file} that got dropped.}
        @entry[action]{Drop action to perform.}
      @end{table}
      Since 3.10

    @subheading{The \"mount\" signal}
      @begin{pre}
 lambda (sidebar mount-operation)    : Run First
      @end{pre}
      The places sidebar emits this signal when it starts a new operation
      because the user clicked on some location that needs mounting. In this way
      the application using the @sym{gtk-places-sidebar} can track the progress
      of the operation and, for example, show a notification.
      @begin[code]{table}
        @entry[sidebar]{The object which received the signal.} 
        @entry[mount-operation]{The @class{g-mount-operation} that is going to
          start.}
      @end{table}
      Since 3.20

    @subheading{The \"open-location\" signal}
      @begin{pre}
 lambda (sidebar location open-flags)    : Run First
      @end{pre}
      The places sidebar emits this signal when the user selects a location in
      it. The calling application should display the contents of that location;
      for example, a file manager should show a list of files in the specified
      location.
      @begin[code]{table}
        @entry[sidebar]{The object which received the signal.} 
        @entry[location]{GFile to which the caller should switch.}
        @entry[open-flags]{A single value from the
          @symbol{gtk-places-open-flags} flags specifying how the location
          should be opened.}
      @end{table}
      Since 3.10

The “populate-popup” signal
void
user_function (GtkPlacesSidebar *sidebar,
               GtkWidget        *container,
               GFile            *selected_item,
               GVolume          *selected_volume,
               gpointer          user_data)
The places sidebar emits this signal when the user invokes a contextual popup on one of its items. In the signal handler, the application may add extra items to the menu as appropriate. For example, a file manager may want to add a \"Properties\" command to the menu.

It is not necessary to store the selected_item for each menu item; during their callbacks, the application can use gtk_places_sidebar_get_location() to get the file to which the item refers.

The selected_item argument may be NULL in case the selection refers to a volume. In this case, selected_volume will be non-NULL. In this case, the calling application will have to g_object_ref() the selected_volume and keep it around to use it in the callback.

The container and all its contents are destroyed after the user dismisses the popup. The popup is re-created (and thus, this signal is emitted) every time the user activates the contextual menu.

Before 3.18, the container always was a GtkMenu, and you were expected to add your items as GtkMenuItems. Since 3.18, the popup may be implemented as a GtkPopover, in which case container will be something else, e.g. a GtkBox, to which you may add GtkModelButtons or other widgets, such as GtkEntries, GtkSpinButtons, etc. If your application can deal with this situation, you can set “populate-all” to TRUE to request that this signal is emitted for populating popovers as well.

Parameters
sidebar

the object which received the signal.

 
container

a GtkMenu or another GtkContainer.

[type Gtk.Widget]
selected_item

GFile with the item to which the popup should refer, or NULL in the case of a selected_volume .

[type Gio.File][nullable]
selected_volume

GVolume if the selected item is a volume, or NULL if it is a file.

[type Gio.Volume][nullable]
user_data

user data set when the signal handler was connected.

 
Flags: Run First

Since 3.10

The “show-connect-to-server” signal
void
user_function (GtkPlacesSidebar *sidebar,
               gpointer          user_data)
The places sidebar emits this signal when it needs the calling application to present an way to connect directly to a network server. For example, the application may bring up a dialog box asking for a URL like \"sftp://ftp.example.com\". It is up to the application to create the corresponding mount by using, for example, g_file_mount_enclosing_volume().

GtkPlacesSidebar::show-connect-to-server has been deprecated since version 3.18 and should not be used in newly-written code.

use the “show-other-locations” signal to connect to network servers.

Parameters
sidebar

the object which received the signal.

 
user_data

user data set when the signal handler was connected.

 
Flags: Run First

The “show-enter-location” signal
void
user_function (GtkPlacesSidebar *sidebar,
               gpointer          user_data)
The places sidebar emits this signal when it needs the calling application to present an way to directly enter a location. For example, the application may bring up a dialog box asking for a URL like \"http://http.example.com\".

Parameters
sidebar

the object which received the signal.

 
user_data

user data set when the signal handler was connected.

 
Flags: Run First

Since 3.14

The “show-error-message” signal
void
user_function (GtkPlacesSidebar *sidebar,
               gchar            *primary,
               gchar            *secondary,
               gpointer          user_data)
The places sidebar emits this signal when it needs the calling application to present an error message. Most of these messages refer to mounting or unmounting media, for example, when a drive cannot be started for some reason.

Parameters
sidebar

the object which received the signal.

 
primary

primary message with a summary of the error to show.

 
secondary

secondary message with details of the error to show.

 
user_data

user data set when the signal handler was connected.

 
Flags: Run First

Since 3.10

The “show-other-locations” signal
void
user_function (GtkPlacesSidebar *sidebar,
               gpointer          user_data)
The places sidebar emits this signal when it needs the calling application to present a way to show other locations e.g. drives and network access points. For example, the application may bring up a page showing persistent volumes and discovered network addresses.

GtkPlacesSidebar::show-other-locations has been deprecated since version 3.20 and should not be used in newly-written code.

use the “show-other-locations-with-flags” which includes the open flags in order to allow the user to specify to open in a new tab or window, in a similar way than “open-location”

Parameters
sidebar

the object which received the signal.

 
user_data

user data set when the signal handler was connected.

 
Flags: Run First

Since 3.18

The “show-other-locations-with-flags” signal
void
user_function (GtkPlacesSidebar  *sidebar,
               GtkPlacesOpenFlags open_flags,
               gpointer           user_data)
The places sidebar emits this signal when it needs the calling application to present a way to show other locations e.g. drives and network access points. For example, the application may bring up a page showing persistent volumes and discovered network addresses.

Parameters
sidebar

the object which received the signal.

 
open_flags

a single value from GtkPlacesOpenFlags specifying how it should be opened.

 
user_data

user data set when the signal handler was connected.

Flags: Run First

Since 3.20

The “show-starred-location” signal
void
user_function (GtkPlacesSidebar  *sidebar,
               GtkPlacesOpenFlags open_flags,
               gpointer           user_data)
The places sidebar emits this signal when it needs the calling application to present a way to show the starred files. In GNOME, starred files are implemented by setting the nao:predefined-tag-favorite tag in the tracker database.

Parameters
sidebar

the object which received the signal.

 
open_flags

a single value from GtkPlacesOpenFlags specifying how the starred file should be opened.

 
user_data

user data set when the signal handler was connected.

 
Flags: Run First

Since 3.22.26

The “unmount” signal
void
user_function (GtkPlacesSidebar *sidebar,
               GMountOperation  *mount_operation,
               gpointer          user_data)
The places sidebar emits this signal when it starts a new operation because the user for example ejected some drive or unmounted a mount. In this way the application using the GtkPlacesSidebar can track the progress of the operation and, for example, show a notification.

Parameters
sidebar

the object which received the signal.

 
mount_operation

the GMountOperation that is going to start.

 
user_data

user data set when the signal handler was connected.

 
Flags: Run First

Since 3.20

  @end{dictionary}

  @see-class{gtk-file-chooser}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#|

;;; --- gtk-places-sidebar-local-only ------------------------------------------

The “local-only” property
  “local-only”               gboolean
Whether the sidebar only includes local files.

Flags: Read / Write

Default value: FALSE

;;; --- gtk-places-sidebar-location --------------------------------------------

The “location” property
  “location”                 GFile *
The location to highlight in the sidebar.

Flags: Read / Write

;;; --- gtk-places-sidebar-open-flags ------------------------------------------

The “open-flags” property
  “open-flags”               GtkPlacesOpenFlags
Modes in which the calling application can open locations selected in the sidebar.

Flags: Read / Write

Default value: GTK_PLACES_OPEN_NORMAL

;;; --- gtk-places-sidebar-populate-all ----------------------------------------

The “populate-all” property
  “populate-all”             gboolean
If :populate-all is TRUE, the “populate-popup” signal is also emitted for popovers.

Flags: Read / Write

Default value: FALSE

Since 3.18

;;; --- gtk-places-sidebar-show-connect-to-server ------------------------------

The “show-connect-to-server” property
  “show-connect-to-server”   gboolean
Whether the sidebar includes a builtin shortcut to a 'Connect to server' dialog.

Flags: Read / Write

Default value: FALSE

;;; --- gtk-places-sidebar-show-desktop ----------------------------------------

The “show-desktop” property
  “show-desktop”             gboolean
Whether the sidebar includes a builtin shortcut to the Desktop folder.

Flags: Read / Write

Default value: TRUE

;;; --- gtk-places-sidebar-show-enter-location ---------------------------------

The “show-enter-location” property
  “show-enter-location”      gboolean
Whether the sidebar includes a builtin shortcut to manually enter a location.

Flags: Read / Write

Default value: FALSE

;;; --- gtk-places-sidebar-show-other-locations --------------------------------

The “show-other-locations” property
  “show-other-locations”     gboolean
Whether the sidebar includes an item to show external locations.

Flags: Read / Write

Default value: FALSE

;;; --- gtk-places-sidebar-show-recent -----------------------------------------

The “show-recent” property
  “show-recent”              gboolean
Whether the sidebar includes a builtin shortcut for recent files.

Flags: Read / Write

Default value: TRUE

;;; --- gtk-places-sidebar-show-starred-location -------------------------------

The “show-starred-location” property
  “show-starred-location”    gboolean
Whether the sidebar includes an item to show starred files.

Flags: Read / Write

Default value: FALSE

;;; --- gtk-places-sidebar-show-trash ------------------------------------------

The “show-trash” property
  “show-trash”               gboolean
Whether the sidebar includes a builtin shortcut to the Trash location.

Flags: Read / Write

Default value: TRUE

|#




#|

gtk_places_sidebar_new ()
GtkWidget *
gtk_places_sidebar_new (void);
Creates a new GtkPlacesSidebar widget.

The application should connect to at least the “open-location” signal to be notified when the user makes a selection in the sidebar.

Returns
a newly created GtkPlacesSidebar

Since 3.10

gtk_places_sidebar_set_open_flags ()
void
gtk_places_sidebar_set_open_flags (GtkPlacesSidebar *sidebar,
                                   GtkPlacesOpenFlags flags);
Sets the way in which the calling application can open new locations from the places sidebar. For example, some applications only open locations “directly” into their main view, while others may support opening locations in a new notebook tab or a new window.

This function is used to tell the places sidebar about the ways in which the application can open new locations, so that the sidebar can display (or not) the “Open in new tab” and “Open in new window” menu items as appropriate.

When the “open-location” signal is emitted, its flags argument will be set to one of the flags that was passed in gtk_places_sidebar_set_open_flags().

Passing 0 for flags will cause GTK_PLACES_OPEN_NORMAL to always be sent to callbacks for the “open-location” signal.

Parameters
sidebar

a places sidebar

 
flags

Bitmask of modes in which the calling application can open locations

 
Since 3.10

gtk_places_sidebar_get_open_flags ()
GtkPlacesOpenFlags
gtk_places_sidebar_get_open_flags (GtkPlacesSidebar *sidebar);
Gets the open flags.

Parameters
sidebar

a GtkPlacesSidebar

 
Returns
the GtkPlacesOpenFlags of sidebar

Since 3.10

gtk_places_sidebar_set_location ()
void
gtk_places_sidebar_set_location (GtkPlacesSidebar *sidebar,
                                 GFile *location);
Sets the location that is being shown in the widgets surrounding the sidebar , for example, in a folder view in a file manager. In turn, the sidebar will highlight that location if it is being shown in the list of places, or it will unhighlight everything if the location is not among the places in the list.

Parameters
sidebar

a places sidebar

 
location

location to select, or NULL for no current path.

[nullable]
Since 3.10

gtk_places_sidebar_get_location ()
GFile *
gtk_places_sidebar_get_location (GtkPlacesSidebar *sidebar);
Gets the currently selected location in the sidebar . This can be NULL when nothing is selected, for example, when gtk_places_sidebar_set_location() has been called with a location that is not among the sidebar’s list of places to show.

You can use this function to get the selection in the sidebar . Also, if you connect to the “populate-popup” signal, you can use this function to get the location that is being referred to during the callbacks for your menu items.

Parameters
sidebar

a places sidebar

 
Returns
a GFile with the selected location, or NULL if nothing is visually selected.

[nullable][transfer full]

Since 3.10

gtk_places_sidebar_set_show_recent ()
void
gtk_places_sidebar_set_show_recent (GtkPlacesSidebar *sidebar,
                                    gboolean show_recent);
Sets whether the sidebar should show an item for recent files. The default value for this option is determined by the desktop environment, but this function can be used to override it on a per-application basis.

Parameters
sidebar

a places sidebar

 
show_recent

whether to show an item for recent files

 
Since 3.18

gtk_places_sidebar_get_show_recent ()
gboolean
gtk_places_sidebar_get_show_recent (GtkPlacesSidebar *sidebar);
Returns the value previously set with gtk_places_sidebar_set_show_recent()

Parameters
sidebar

a places sidebar

 
Returns
TRUE if the sidebar will display a builtin shortcut for recent files

Since 3.18

gtk_places_sidebar_set_show_desktop ()
void
gtk_places_sidebar_set_show_desktop (GtkPlacesSidebar *sidebar,
                                     gboolean show_desktop);
Sets whether the sidebar should show an item for the Desktop folder. The default value for this option is determined by the desktop environment and the user’s configuration, but this function can be used to override it on a per-application basis.

Parameters
sidebar

a places sidebar

 
show_desktop

whether to show an item for the Desktop folder

 
Since 3.10

gtk_places_sidebar_get_show_desktop ()
gboolean
gtk_places_sidebar_get_show_desktop (GtkPlacesSidebar *sidebar);
Returns the value previously set with gtk_places_sidebar_set_show_desktop()

Parameters
sidebar

a places sidebar

 
Returns
TRUE if the sidebar will display a builtin shortcut to the desktop folder.

Since 3.10

gtk_places_sidebar_add_shortcut ()
void
gtk_places_sidebar_add_shortcut (GtkPlacesSidebar *sidebar,
                                 GFile *location);
Applications may want to present some folders in the places sidebar if they could be immediately useful to users. For example, a drawing program could add a “/usr/share/clipart” location when the sidebar is being used in an “Insert Clipart” dialog box.

This function adds the specified location to a special place for immutable shortcuts. The shortcuts are application-specific; they are not shared across applications, and they are not persistent. If this function is called multiple times with different locations, then they are added to the sidebar’s list in the same order as the function is called.

Parameters
sidebar

a places sidebar

 
location

location to add as an application-specific shortcut

 
Since 3.10

gtk_places_sidebar_remove_shortcut ()
void
gtk_places_sidebar_remove_shortcut (GtkPlacesSidebar *sidebar,
                                    GFile *location);
Removes an application-specific shortcut that has been previously been inserted with gtk_places_sidebar_add_shortcut(). If the location is not a shortcut in the sidebar, then nothing is done.

Parameters
sidebar

a places sidebar

 
location

location to remove

 
Since 3.10

gtk_places_sidebar_list_shortcuts ()
GSList *
gtk_places_sidebar_list_shortcuts (GtkPlacesSidebar *sidebar);
Gets the list of shortcuts.

Parameters
sidebar

a places sidebar

 
Returns
A GSList of GFile of the locations that have been added as application-specific shortcuts with gtk_places_sidebar_add_shortcut(). To free this list, you can use

g_slist_free_full (list, (GDestroyNotify) g_object_unref);
.

[element-type GFile][transfer full]

Since 3.10

gtk_places_sidebar_get_nth_bookmark ()
GFile *
gtk_places_sidebar_get_nth_bookmark (GtkPlacesSidebar *sidebar,
                                     gint n);
This function queries the bookmarks added by the user to the places sidebar, and returns one of them. This function is used by GtkFileChooser to implement the “Alt-1”, “Alt-2”, etc. shortcuts, which activate the cooresponding bookmark.

Parameters
sidebar

a places sidebar

 
n

index of the bookmark to query

 
Returns
The bookmark specified by the index n , or NULL if no such index exist. Note that the indices start at 0, even though the file chooser starts them with the keyboard shortcut "Alt-1".

[nullable][transfer full]

Since 3.10

gtk_places_sidebar_get_show_connect_to_server ()
gboolean
gtk_places_sidebar_get_show_connect_to_server
                               (GtkPlacesSidebar *sidebar);
gtk_places_sidebar_get_show_connect_to_server has been deprecated since version 3.18 and should not be used in newly-written code.

It is recommended to group this functionality with the drives and network location under the new 'Other Location' item

Returns the value previously set with gtk_places_sidebar_set_show_connect_to_server()

Parameters
sidebar

a places sidebar

 
Returns
TRUE if the sidebar will display a “Connect to Server” item.

gtk_places_sidebar_set_show_connect_to_server ()
void
gtk_places_sidebar_set_show_connect_to_server
                               (GtkPlacesSidebar *sidebar,
                                gboolean show_connect_to_server);
gtk_places_sidebar_set_show_connect_to_server has been deprecated since version 3.18 and should not be used in newly-written code.

It is recommended to group this functionality with the drives and network location under the new 'Other Location' item

Sets whether the sidebar should show an item for connecting to a network server; this is off by default. An application may want to turn this on if it implements a way for the user to connect to network servers directly.

If you enable this, you should connect to the “show-connect-to-server” signal.

Parameters
sidebar

a places sidebar

 
show_connect_to_server

whether to show an item for the Connect to Server command

 
Since 3.10

gtk_places_sidebar_get_local_only ()
gboolean
gtk_places_sidebar_get_local_only (GtkPlacesSidebar *sidebar);
Returns the value previously set with gtk_places_sidebar_set_local_only().

Parameters
sidebar

a places sidebar

 
Returns
TRUE if the sidebar will only show local files.

Since 3.12

gtk_places_sidebar_set_local_only ()
void
gtk_places_sidebar_set_local_only (GtkPlacesSidebar *sidebar,
                                   gboolean local_only);
Sets whether the sidebar should only show local files.

Parameters
sidebar

a places sidebar

 
local_only

whether to show only local files

 
Since 3.12

gtk_places_sidebar_get_show_enter_location ()
gboolean
gtk_places_sidebar_get_show_enter_location
                               (GtkPlacesSidebar *sidebar);
Returns the value previously set with gtk_places_sidebar_set_show_enter_location()

Parameters
sidebar

a places sidebar

 
Returns
TRUE if the sidebar will display an “Enter Location” item.

Since 3.14

gtk_places_sidebar_set_show_enter_location ()
void
gtk_places_sidebar_set_show_enter_location
                               (GtkPlacesSidebar *sidebar,
                                gboolean show_enter_location);
Sets whether the sidebar should show an item for entering a location; this is off by default. An application may want to turn this on if manually entering URLs is an expected user action.

If you enable this, you should connect to the “show-enter-location” signal.

Parameters
sidebar

a places sidebar

 
show_enter_location

whether to show an item to enter a location

 
Since 3.14

gtk_places_sidebar_get_show_trash ()
gboolean
gtk_places_sidebar_get_show_trash (GtkPlacesSidebar *sidebar);
Returns the value previously set with gtk_places_sidebar_set_show_trash()

Parameters
sidebar

a places sidebar

 
Returns
TRUE if the sidebar will display a “Trash” item.

Since 3.18

gtk_places_sidebar_set_show_trash ()
void
gtk_places_sidebar_set_show_trash (GtkPlacesSidebar *sidebar,
                                   gboolean show_trash);
Sets whether the sidebar should show an item for the Trash location.

Parameters
sidebar

a places sidebar

 
show_trash

whether to show an item for the Trash location

 
Since 3.18

gtk_places_sidebar_get_show_other_locations ()
gboolean
gtk_places_sidebar_get_show_other_locations
                               (GtkPlacesSidebar *sidebar);
Returns the value previously set with gtk_places_sidebar_set_show_other_locations()

Parameters
sidebar

a places sidebar

 
Returns
TRUE if the sidebar will display an “Other Locations” item.

Since 3.18

gtk_places_sidebar_set_show_other_locations ()
void
gtk_places_sidebar_set_show_other_locations
                               (GtkPlacesSidebar *sidebar,
                                gboolean show_other_locations);
Sets whether the sidebar should show an item for the application to show an Other Locations view; this is off by default. When set to TRUE, persistent devices such as hard drives are hidden, otherwise they are shown in the sidebar. An application may want to turn this on if it implements a way for the user to see and interact with drives and network servers directly.

If you enable this, you should connect to the “show-other-locations” signal.

Parameters
sidebar

a places sidebar

 
show_other_locations

whether to show an item for the Other Locations view

 
Since 3.18

gtk_places_sidebar_set_drop_targets_visible ()
void
gtk_places_sidebar_set_drop_targets_visible
                               (GtkPlacesSidebar *sidebar,
                                gboolean visible,
                                GdkDragContext *context);
Make the GtkPlacesSidebar show drop targets, so it can show the available drop targets and a "new bookmark" row. This improves the Drag-and-Drop experience of the user and allows applications to show all available drop targets at once.

This needs to be called when the application is aware of an ongoing drag that might target the sidebar. The drop-targets-visible state will be unset automatically if the drag finishes in the GtkPlacesSidebar. You only need to unset the state when the drag ends on some other widget on your application.

Parameters
sidebar

a places sidebar.

 
visible

whether to show the valid targets or not.

 
context

drag context used to ask the source about the action that wants to perform, so hints are more accurate.

 
Since 3.18

Types and Values
GtkPlacesSidebar
typedef struct _GtkPlacesSidebar GtkPlacesSidebar;

|#

;;; --- End of file gtk.places-sidebar.lisp ------------------------------------
