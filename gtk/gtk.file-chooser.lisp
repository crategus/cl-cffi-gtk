;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkFileChooser
;;; 
;;; File chooser interface used by GtkFileChooserWidget and GtkFileChooserDialog
;;; 
;;; Synopsis
;;; 
;;;     GtkFileChooser
;;;     GtkFileChooserAction
;;;     GtkFileChooserConfirmation
;;;
;;;     GTK_FILE_CHOOSER_ERROR
;;;
;;;     GtkFileChooserError
;;;
;;;     gtk_file_chooser_set_action
;;;     gtk_file_chooser_get_action
;;;     gtk_file_chooser_set_local_only
;;;     gtk_file_chooser_get_local_only
;;;     gtk_file_chooser_set_select_multiple
;;;     gtk_file_chooser_get_select_multiple
;;;     gtk_file_chooser_set_show_hidden
;;;     gtk_file_chooser_get_show_hidden
;;;     gtk_file_chooser_set_do_overwrite_confirmation
;;;     gtk_file_chooser_get_do_overwrite_confirmation
;;;     gtk_file_chooser_set_create_folders
;;;     gtk_file_chooser_get_create_folders
;;;     gtk_file_chooser_set_current_name
;;;     gtk_file_chooser_get_filename
;;;     gtk_file_chooser_set_filename
;;;     gtk_file_chooser_select_filename
;;;     gtk_file_chooser_unselect_filename
;;;     gtk_file_chooser_select_all
;;;     gtk_file_chooser_unselect_all
;;;     gtk_file_chooser_get_filenames
;;;     gtk_file_chooser_set_current_folder
;;;     gtk_file_chooser_get_current_folder
;;;     gtk_file_chooser_get_uri
;;;     gtk_file_chooser_set_uri
;;;     gtk_file_chooser_select_uri
;;;     gtk_file_chooser_unselect_uri
;;;     gtk_file_chooser_get_uris
;;;     gtk_file_chooser_set_current_folder_uri
;;;     gtk_file_chooser_get_current_folder_uri
;;;     gtk_file_chooser_set_preview_widget
;;;     gtk_file_chooser_get_preview_widget
;;;     gtk_file_chooser_set_preview_widget_active
;;;     gtk_file_chooser_get_preview_widget_active
;;;     gtk_file_chooser_set_use_preview_label
;;;     gtk_file_chooser_get_use_preview_label
;;;     gtk_file_chooser_get_preview_filename
;;;     gtk_file_chooser_get_preview_uri
;;;     gtk_file_chooser_set_extra_widget
;;;     gtk_file_chooser_get_extra_widget
;;;     gtk_file_chooser_add_filter
;;;     gtk_file_chooser_remove_filter
;;;     gtk_file_chooser_list_filters
;;;     gtk_file_chooser_set_filter
;;;     gtk_file_chooser_get_filter
;;;     gtk_file_chooser_add_shortcut_folder
;;;     gtk_file_chooser_remove_shortcut_folder
;;;     gtk_file_chooser_list_shortcut_folders
;;;     gtk_file_chooser_add_shortcut_folder_uri
;;;     gtk_file_chooser_remove_shortcut_folder_uri
;;;     gtk_file_chooser_list_shortcut_folder_uris
;;;     gtk_file_chooser_get_current_folder_file
;;;     gtk_file_chooser_get_file
;;;     gtk_file_chooser_get_files
;;;     gtk_file_chooser_get_preview_file
;;;     gtk_file_chooser_select_file
;;;     gtk_file_chooser_set_current_folder_file
;;;     gtk_file_chooser_set_file
;;;     gtk_file_chooser_unselect_file
;;; 
;;; Object Hierarchy
;;; 
;;;   GInterface
;;;    +----GtkFileChooser
;;; 
;;; Prerequisites
;;; 
;;; GtkFileChooser requires GtkWidget.
;;;
;;; Known Implementations
;;; 
;;; GtkFileChooser is implemented by GtkFileChooserButton, GtkFileChooserDialog
;;; and GtkFileChooserWidget.
;;;
;;; Properties
;;; 
;;;   "action"                    GtkFileChooserAction  : Read / Write
;;;   "create-folders"            gboolean              : Read / Write
;;;   "do-overwrite-confirmation" gboolean              : Read / Write
;;;   "extra-widget"              GtkWidget*            : Read / Write
;;;   "filter"                    GtkFileFilter*        : Read / Write
;;;   "local-only"                gboolean              : Read / Write
;;;   "preview-widget"            GtkWidget*            : Read / Write
;;;   "preview-widget-active"     gboolean              : Read / Write
;;;   "select-multiple"           gboolean              : Read / Write
;;;   "show-hidden"               gboolean              : Read / Write
;;;   "use-preview-label"         gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "confirm-overwrite"                               : Run Last
;;;   "current-folder-changed"                          : Run Last
;;;   "file-activated"                                  : Run Last
;;;   "selection-changed"                               : Run Last
;;;   "update-preview"                                  : Run Last
;;; 
;;; Description
;;; 
;;; GtkFileChooser is an interface that can be implemented by file selection
;;; widgets. In GTK+, the main objects that implement this interface are
;;; GtkFileChooserWidget, GtkFileChooserDialog, and GtkFileChooserButton. You do
;;; not need to write an object that implements the GtkFileChooser interface
;;; unless you are trying to adapt an existing file selector to expose a
;;; standard programming interface.
;;; 
;;; GtkFileChooser allows for shortcuts to various places in the filesystem. In
;;; the default implementation these are displayed in the left pane. It may be a
;;; bit confusing at first that these shortcuts come from various sources and in
;;; various flavours, so lets explain the terminology here:
;;; 
;;; Bookmarks
;;;     are created by the user, by dragging folders from the right pane to the
;;;     left pane, or by using the "Add". Bookmarks can be renamed and deleted
;;;     by the user.
;;; 
;;; Shortcuts
;;;     can be provided by the application or by the underlying filesystem
;;;     abstraction (e.g. both the gnome-vfs and the Windows filesystems provide
;;;     "Desktop" shortcuts). Shortcuts cannot be modified by the user.
;;; 
;;; Volumes
;;;     are provided by the underlying filesystem abstraction. They are the
;;;     "roots" of the filesystem.
;;; 
;;; File Names and Encodings
;;;
;;; When the user is finished selecting files in a GtkFileChooser, your program
;;; can get the selected names either as filenames or as URIs. For URIs, the
;;; normal escaping rules are applied if the URI contains non-ASCII characters.
;;; However, filenames are always returned in the character set specified by the
;;; G_FILENAME_ENCODING environment variable. Please see the Glib documentation
;;; for more details about this variable.
;;;
;;; Note
;;;
;;; This means that while you can pass the result of
;;; gtk_file_chooser_get_filename() to open(2) or fopen(3), you may not be able
;;; to directly set it as the text of a GtkLabel widget unless you convert it
;;; first to UTF-8, which all GTK+ widgets expect. You should use
;;; g_filename_to_utf8() to convert filenames into strings that can be passed
;;; to GTK+ widgets.
;;; 
;;; Adding a Preview Widget
;;; 
;;; You can add a custom preview widget to a file chooser and then get
;;; notification about when the preview needs to be updated. To install a
;;; preview widget, use gtk_file_chooser_set_preview_widget(). Then, connect to
;;; the "update-preview" signal to get notified when you need to update the
;;; contents of the preview.
;;; 
;;; Your callback should use gtk_file_chooser_get_preview_filename() to see what
;;; needs previewing. Once you have generated the preview for the corresponding
;;; file, you must call gtk_file_chooser_set_preview_widget_active() with a
;;; boolean flag that indicates whether your callback could successfully
;;; generate a preview.
;;; 
;;; Example 84. Sample Usage
;;; 
;;; {
;;;   GtkImage *preview;
;;; 
;;;   ...
;;; 
;;;   preview = gtk_image_new ();
;;; 
;;;   gtk_file_chooser_set_preview_widget (my_file_chooser, preview);
;;;   g_signal_connect (my_file_chooser, "update-preview",
;;;             G_CALLBACK (update_preview_cb), preview);
;;; }
;;; 
;;; static void
;;; update_preview_cb (GtkFileChooser *file_chooser, gpointer data)
;;; {
;;;   GtkWidget *preview;
;;;   char *filename;
;;;   GdkPixbuf *pixbuf;
;;;   gboolean have_preview;
;;; 
;;;   preview = GTK_WIDGET (data);
;;;   filename = gtk_file_chooser_get_preview_filename (file_chooser);
;;; 
;;;   pixbuf = gdk_pixbuf_new_from_file_at_size (filename, 128, 128, NULL);
;;;   have_preview = (pixbuf != NULL);
;;;   g_free (filename);
;;; 
;;;   gtk_image_set_from_pixbuf (GTK_IMAGE (preview), pixbuf);
;;;   if (pixbuf)
;;;     g_object_unref (pixbuf);
;;; 
;;;   gtk_file_chooser_set_preview_widget_active (file_chooser, have_preview);
;;; }
;;; 
;;; Adding Extra Widgets
;;; 
;;; You can add extra widgets to a file chooser to provide options that are not
;;; present in the default design. For example, you can add a toggle button to
;;; give the user the option to open a file in read-only mode. You can use
;;; gtk_file_chooser_set_extra_widget() to insert additional widgets in a file
;;; chooser.
;;; 
;;; Example 85. Sample Usage
;;; 
;;; GtkWidget *toggle;
;;; 
;;; ...
;;; 
;;; toggle = gtk_check_button_new_with_label ("Open file read-only");
;;; gtk_widget_show (toggle);
;;; gtk_file_chooser_set_extra_widget (my_file_chooser, toggle);
;;; }
;;; 
;;; Note
;;;
;;; If you want to set more than one extra widget in the file chooser, you can
;;; a container such as a GtkVBox or a GtkTable and include your widgets in it.
;;; Then, set the container as the whole extra widget.
;;; 
;;; Key Bindings
;;; 
;;; Internally, GTK+ implements a file chooser's graphical user interface with
;;; the private GtkFileChooserDefaultClass. This widget has several key bindings
;;; and their associated signals. This section describes the available key
;;; binding signals.
;;; 
;;; Example 86. GtkFileChooser key binding example
;;; 
;;; The default keys that activate the key-binding signals in
;;; GtkFileChooserDefaultClass are as follows:
;;;
;;; Signal name         Default key combinations
;;; location-popup      Control+L (empty path); / (path of "/"); ~ (path of "~")
;;; up-folder           Alt+Up; Alt+Shift+Up [b]; Backspace
;;; down-folder         Alt+Down; Alt+Shift+Down [c]
;;; home-folder         Alt+Home
;;; desktop-folder      Alt+D
;;; quick-bookmark      Alt+1 through Alt+0
;;; 
;;; You can change these defaults to something else. For example, to add a Shift
;;; modifier to a few of the default bindings, you can include the following
;;; fragment in your .config/gtk-3.0/gtk.css file:
;;; 
;;; @binding-set MyOwnFilechooserBindings
;;; {
;;;   bind "<Alt><Shift>Up"   { "up-folder" () }
;;;   bind "<Alt><Shift>Down" { "down-folder" () }
;;;   bind "<Alt><Shift>Home" { "home-folder" () }
;;; }
;;; 
;;; GtkFileChooserDefault
;;; {
;;;    gtk-key-bindings: MyOwnFilechooserBindings
;;; }
;;; 
;;; ----------------------------------------------------------------------------
;;; The "GtkFileChooserDefault::location-popup" signal
;;; 
;;; void user_function (GtkFileChooserDefault *chooser,
;;;                     const char            *path,
;;;                     gpointer               user_data);
;;; 
;;; This is used to make the file chooser show a "Location" dialog which the
;;; user can use to manually type the name of the file he wishes to select. The
;;; path argument is a string that gets put in the text entry for the file name.
;;; By default this is bound to Control+L with a path string of "" (the empty
;;; string). It is also bound to / with a path string of "/" (a slash): this
;;; lets you type / and immediately type a path name. On Unix systems, this is
;;; bound to ~ (tilde) with a path string of "~" itself for access to home
;;; directories.
;;; 
;;; chooser :
;;;     the object which received the signal.
;;; 
;;; path :
;;;     default contents for the text entry for the file name
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Note
;;;
;;; You can create your own bindings for the "location-popup" signal with custom
;;; path strings, and have a crude form of easily-to-type bookmarks. For
;;; example, say you access the path /home/username/misc very frequently. You
;;; could then create an Alt+M shortcut by including the following in your
;;; .config/gtk-3.0/gtk.css:
;;; 
;;;    @binding-set MiscShortcut
;;;    {
;;;      bind "<Alt>M" { "location-popup" ("/home/username/misc") }
;;;    }
;;; 
;;;    GtkFileChooserDefault
;;;    {
;;;      gtk-key-bindings: MiscShortcut
;;;    }
;;;    
;;; ----------------------------------------------------------------------------
;;; The "GtkFileChooserDefault::up-folder" signal
;;; 
;;; void user_function (GtkFileChooserDefault *chooser, gpointer user_data);
;;; 
;;; This is used to make the file chooser go to the parent of the current folder
;;; in the file hierarchy. By default this is bound to Backspace and Alt+Up (the
;;; Up key in the numeric keypad also works).
;;; 
;;; chooser :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "GtkFileChooserDefault::down-folder" signal
;;; 
;;; void user_function (GtkFileChooserDefault *chooser, gpointer user_data);
;;; 
;;; This is used to make the file chooser go to a child of the current folder in
;;; the file hierarchy. The subfolder that will be used is displayed in the path
;;; bar widget of the file chooser. For example, if the path bar is showing
;;; "/foo/bar/baz", then this will cause the file chooser to switch to the "baz"
;;; subfolder. By default this is bound to Alt+Down (the Down key in the numeric
;;; keypad also works).
;;; 
;;; chooser :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "GtkFileChooserDefault::home-folder" signal
;;; 
;;; void user_function (GtkFileChooserDefault *chooser, gpointer user_data);
;;; 
;;; This is used to make the file chooser show the user's home folder in the
;;; file list. By default this is bound to Alt+Home (the Home key in the numeric
;;; keypad also works).
;;; 
;;; chooser :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "GtkFileChooserDefault::desktop-folder" signal
;;; 
;;; void user_function (GtkFileChooserDefault *chooser, gpointer user_data);
;;; 
;;; This is used to make the file chooser show the user's Desktop folder in the
;;; file list. By default this is bound to Alt+D.
;;; 
;;; chooser :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "GtkFileChooserDefault::quick-bookmark" signal
;;; 
;;; void user_function (GtkFileChooserDefault *chooser,
;;;                     gint bookmark_index,
;;;                     gpointer user_data);
;;; 
;;; This is used to make the file chooser switch to the bookmark specified in
;;; the bookmark_index parameter. For example, if you have three bookmarks, you
;;; can pass 0, 1, 2 to this signal to switch to each of them, respectively. By
;;; default this is bound to Alt+1, Alt+2, etc. until Alt+0. Note that in the
;;; default binding, that Alt+1 is actually defined to switch to the bookmark at
;;; index 0, and so on successively; Alt+0 is defined to switch to the bookmark
;;; at index 10.
;;; 
;;; chooser :
;;;     the object which received the signal.
;;; 
;;; bookmark_indes :
;;;     index of the bookmark to switch to; the indices start at 0.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "action" property
;;; 
;;;   "action"                   GtkFileChooserAction  : Read / Write
;;; 
;;; The type of operation that the file selector is performing.
;;; 
;;; Default value: GTK_FILE_CHOOSER_ACTION_OPEN
;;;
;;; ----------------------------------------------------------------------------
;;; The "create-folders" property
;;; 
;;;   "create-folders"           gboolean              : Read / Write
;;; 
;;; Whether a file chooser not in GTK_FILE_CHOOSER_ACTION_OPEN mode will offer
;;; the user to create new folders.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "do-overwrite-confirmation" property
;;; 
;;;   "do-overwrite-confirmation" gboolean              : Read / Write
;;; 
;;; Whether a file chooser in GTK_FILE_CHOOSER_ACTION_SAVE mode will present an
;;; overwrite confirmation dialog if the user selects a file name that already
;;; exists.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "extra-widget" property
;;; 
;;;   "extra-widget"             GtkWidget*            : Read / Write
;;; 
;;; Application supplied widget for extra options.
;;;
;;; ----------------------------------------------------------------------------
;;; The "filter" property
;;; 
;;;   "filter"                   GtkFileFilter*        : Read / Write
;;; 
;;; The current filter for selecting which files are displayed.
;;;
;;; ----------------------------------------------------------------------------
;;; The "local-only" property
;;; 
;;;   "local-only"               gboolean              : Read / Write
;;; 
;;; Whether the selected file(s) should be limited to local file: URLs.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "preview-widget" property
;;; 
;;;   "preview-widget"           GtkWidget*            : Read / Write
;;; 
;;; Application supplied widget for custom previews.
;;;
;;; ----------------------------------------------------------------------------
;;; The "preview-widget-active" property
;;; 
;;;   "preview-widget-active"    gboolean              : Read / Write
;;; 
;;; Whether the application supplied widget for custom previews should be shown.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "select-multiple" property
;;; 
;;;   "select-multiple"          gboolean              : Read / Write
;;; 
;;; Whether to allow multiple files to be selected.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-hidden" property
;;; 
;;;   "show-hidden"              gboolean              : Read / Write
;;; 
;;; Whether the hidden files and folders should be displayed.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-preview-label" property
;;; 
;;;   "use-preview-label"        gboolean              : Read / Write
;;; 
;;; Whether to display a stock label with the name of the previewed file.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "confirm-overwrite" signal
;;; 
;;; GtkFileChooserConfirmationuser_function
;;;                                  (GtkFileChooser *chooser,
;;;                                   gpointer        user_data)      : Run Last
;;; 
;;; This signal gets emitted whenever it is appropriate to present a
;;; confirmation dialog when the user has selected a file name that already
;;; exists. The signal only gets emitted when the file chooser is in
;;; GTK_FILE_CHOOSER_ACTION_SAVE mode.
;;; 
;;; Most applications just need to turn on the "do-overwrite-confirmation"
;;; property (or call the gtk_file_chooser_set_do_overwrite_confirmation()
;;; function), and they will automatically get a stock confirmation dialog.
;;; Applications which need to customize this behavior should do that, and also
;;; connect to the "confirm-overwrite" signal.
;;; 
;;; A signal handler for this signal must return a GtkFileChooserConfirmation
;;; value, which indicates the action to take. If the handler determines that
;;; the user wants to select a different filename, it should return
;;; GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN. If it determines that the user
;;; is satisfied with his choice of file name, it should return
;;; GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME. On the other hand, if it
;;; determines that the stock confirmation dialog should be used, it should
;;; return GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM. The following example
;;; illustrates this.
;;; 
;;; Example 87. Custom confirmation
;;; 
;;; static GtkFileChooserConfirmation
;;; confirm_overwrite_callback (GtkFileChooser *chooser, gpointer data)
;;; {
;;;   char *uri;
;;;   uri = gtk_file_chooser_get_uri (chooser);
;;; 
;;;   if (is_uri_read_only (uri))
;;;     {
;;;       if (user_wants_to_replace_read_only_file (uri))
;;;         return GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME;
;;;       else
;;;         return GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN;
;;;     } else
;;;       // fall back to the default dialog
;;;       return GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM;
;;; }
;;; 
;;; ...
;;; 
;;; chooser = gtk_file_chooser_dialog_new (...);
;;; 
;;; gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog),
;;;                                                 TRUE);
;;; g_signal_connect (chooser, "confirm-overwrite",
;;;                   G_CALLBACK (confirm_overwrite_callback), NULL);
;;; 
;;; if (gtk_dialog_run (chooser) == GTK_RESPONSE_ACCEPT)
;;;         save_to_file (gtk_file_chooser_get_filename
;;;                                                (GTK_FILE_CHOOSER (chooser));
;;; 
;;; gtk_widget_destroy (chooser);
;;; 
;;; 
;;; chooser :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     a GtkFileChooserConfirmation value that indicates which action to take
;;;     after emitting the signal.
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "current-folder-changed" signal
;;; 
;;; void user_function (GtkFileChooser *chooser,
;;;                     gpointer        user_data)      : Run Last
;;; 
;;; This signal is emitted when the current folder in a GtkFileChooser changes.
;;; This can happen due to the user performing some action that changes folders,
;;; such as selecting a bookmark or visiting a folder on the file list. It can
;;; also happen as a result of calling a function to explicitly change the
;;; current folder in a file chooser.
;;; 
;;; Normally you do not need to connect to this signal, unless you need to keep
;;; track of which folder a file chooser is showing.
;;; 
;;; See also:
;;; gtk_file_chooser_set_current_folder(),
;;; gtk_file_chooser_get_current_folder(),
;;; gtk_file_chooser_set_current_folder_uri(),
;;; gtk_file_chooser_get_current_folder_uri().
;;; 
;;; chooser :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "file-activated" signal
;;; 
;;; void user_function (GtkFileChooser *chooser,
;;;                     gpointer        user_data)      : Run Last
;;; 
;;; This signal is emitted when the user "activates" a file in the file chooser.
;;; This can happen by double-clicking on a file in the file list, or by
;;; pressing Enter.
;;; 
;;; Normally you do not need to connect to this signal. It is used internally
;;; by GtkFileChooserDialog to know when to activate the default button in the
;;; dialog.
;;; 
;;; See also:
;;; gtk_file_chooser_get_filename(),
;;; gtk_file_chooser_get_filenames(),
;;; gtk_file_chooser_get_uri(),
;;; gtk_file_chooser_get_uris().
;;; 
;;; chooser :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-changed" signal
;;; 
;;; void user_function (GtkFileChooser *chooser,
;;;                     gpointer        user_data)      : Run Last
;;; 
;;; This signal is emitted when there is a change in the set of selected files
;;; in a GtkFileChooser. This can happen when the user modifies the selection
;;; with the mouse or the keyboard, or when explicitly calling functions to
;;; change the selection.
;;; 
;;; Normally you do not need to connect to this signal, as it is easier to wait
;;; for the file chooser to finish running, and then to get the list of selected
;;; files using the functions mentioned below.
;;; 
;;; See also:
;;; gtk_file_chooser_select_filename(),
;;; gtk_file_chooser_unselect_filename(),
;;; gtk_file_chooser_get_filename(),
;;; gtk_file_chooser_get_filenames(),
;;; gtk_file_chooser_select_uri(),
;;; gtk_file_chooser_unselect_uri(),
;;; gtk_file_chooser_get_uri(),
;;; gtk_file_chooser_get_uris().
;;; 
;;; chooser :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "update-preview" signal
;;; 
;;; void user_function (GtkFileChooser *chooser,
;;;                     gpointer        user_data)      : Run Last
;;; 
;;; This signal is emitted when the preview in a file chooser should be
;;; regenerated. For example, this can happen when the currently selected file
;;; changes. You should use this signal if you want your file chooser to have
;;; a preview widget.
;;; 
;;; Once you have installed a preview widget with
;;; gtk_file_chooser_set_preview_widget(), you should update it when this signal
;;; is emitted. You can use the functions
;;; gtk_file_chooser_get_preview_filename() or
;;; gtk_file_chooser_get_preview_uri() to get the name of the file to preview.
;;; Your widget may not be able to preview all kinds of files; your callback
;;; must call gtk_file_chooser_set_preview_widget_active() to inform the file
;;; chooser about whether the preview was generated successfully or not.
;;; 
;;; Please see the example code in the section called “Adding a Preview Widget”.
;;; 
;;; See also:
;;; gtk_file_chooser_set_preview_widget(),
;;; gtk_file_chooser_set_preview_widget_active(),
;;; gtk_file_chooser_set_use_preview_label(),
;;; gtk_file_chooser_get_preview_filename(),
;;; gtk_file_chooser_get_preview_uri().
;;; 
;;; chooser :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileChooser
;;; 
;;; typedef struct _GtkFileChooser GtkFileChooser;
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkFileChooser" gtk-file-chooser
  (:export t
   :type-initializer "gtk_file_chooser_get_type")
  (action
   gtk-file-chooser-action
   "action" "GtkFileChooserAction" t t)
  (do-overwrite-confirmation
   gtk-file-chooser-do-overwrite-confirmation
   "do-overwrite-confirmation" "gboolean" t t)
  (extra-widget
   gtk-file-chooser-extra-widget
   "extra-widget" "GtkWidget" t t)
  (file-system-backend
   gtk-file-chooser-file-system-backend
   "file-system-backend" "gchararray" nil nil)
  (filter
   gtk-file-chooser-filter
   "filter" "GtkFileFilter" t t)
  (local-only
   gtk-file-chooser-local-only
   "local-only" "gboolean" t t)
  (preview-widget
   gtk-file-chooser-preview-widget
   "preview-widget" "GtkWidget" t t)
  (preview-widget-active
   gtk-file-chooser-preview-widget-active
   "preview-widget-active" "gboolean" t t)
  (select-multiple
   gtk-file-chooser-select-multiple
   "select-multiple" "gboolean" t t)
  (show-hidden
   gtk-file-chooser-show-hidden
   "show-hidden" "gboolean" t t)
  (use-preview-label
   gtk-file-chooser-use-preview-label
   "use-preview-label" "gboolean" t t)
  (:cffi current-name
         gtk-file-chooser-current-name
         (:string :free-to-foreign t :encoding :utf-8) nil
         "gtk_file_chooser_set_current_name")
  #+win32
  (:cffi filename
         gtk-file-chooser-filename
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_filename_utf8"
         "gtk_file_chooser_set_filename_utf8")
  #-win32
  (:cffi filename
         gtk-file-chooser-filename
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_filename"
         "gtk_file_chooser_set_filename")
  #+win32
  (:cffi current-folder
         gtk-file-chooser-current-folder
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_current_folder_utf8"
         "gtk_file_chooser_set_current_folder_utf8")
  #-win32
  (:cffi current-folder
         gtk-file-chooser-current-folder
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_current_folder"
         "gtk_file_chooser_set_current_folder")
  (:cffi uri
         gtk-file-chooser-uri
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_uri" "gtk_file_chooser_set_uri")
  (:cffi current-folder-uri
         gtk-file-chooser-current-folder-uri
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_current_folder_uri"
         "gtk_file_chooser_set_current_folder_uri")
  #+win32
  (:cffi preview-filename
         gtk-file-chooser-preview-filename
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_preview_filename_utf8" nil)
  #-win32
  (:cffi preview-filename
         gtk-file-chooser-preview-filename
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_preview_filename" nil)
  (:cffi preview-uri
         gtk-file-chooser-preview-uri
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_preview_uri" nil))

;;; ----------------------------------------------------------------------------
;;; enum GtkFileChooserAction
;;; 
;;; typedef enum {
;;;   GTK_FILE_CHOOSER_ACTION_OPEN,
;;;   GTK_FILE_CHOOSER_ACTION_SAVE,
;;;   GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
;;;   GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER
;;; } GtkFileChooserAction;
;;; 
;;; Describes whether a GtkFileChooser is being used to open existing files or
;;; to save to a possibly new file.
;;; 
;;; GTK_FILE_CHOOSER_ACTION_OPEN
;;;     Indicates open mode. The file chooser will only let the user pick an
;;;     existing file.
;;; 
;;; GTK_FILE_CHOOSER_ACTION_SAVE
;;;     Indicates save mode. The file chooser will let the user pick an
;;;     existing file, or type in a new filename.
;;; 
;;; GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER
;;;     Indicates an Open mode for selecting folders. The file chooser will let
;;;     the user pick an existing folder.
;;; 
;;; GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER
;;;     Indicates a mode for creating a new folder. The file chooser will let
;;;     the user name an existing or new folder.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkFileChooserAction" gtk-file-chooser-action
  (:export t
   :type-initializer "gtk_file_chooser_action_get_type")
  (:open 0)
  (:save 1)
  (:select-folder 2)
  (:create-folder 3))

;;; ----------------------------------------------------------------------------
;;; enum GtkFileChooserConfirmation
;;; 
;;; typedef enum {
;;;   GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM,
;;;   GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME,
;;;   GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN
;;; } GtkFileChooserConfirmation;
;;; 
;;; Used as a return value of handlers for the "confirm-overwrite" signal of a
;;; GtkFileChooser. This value determines whether the file chooser will present
;;; the stock confirmation dialog, accept the user's choice of a filename, or
;;; let the user choose another filename.
;;; 
;;; GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM
;;;     The file chooser will present its stock dialog to confirm about
;;;     overwriting an existing file.
;;; 
;;; GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME
;;;     The file chooser will terminate and accept the user's choice of a file
;;;     name.
;;; 
;;; GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN
;;;     The file chooser will continue running, so as to let the user select
;;;     another file name.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkFileChooserConfirmation" gtk-file-chooser-confirmation
  (:export t
   :type-initializer "gtk_file_chooser_confirmation_get_type")
  (:confirm 0)
  (:accept-filename 1)
  (:select-again 2))

;;; ----------------------------------------------------------------------------
;;; GTK_FILE_CHOOSER_ERROR
;;; 
;;; #define GTK_FILE_CHOOSER_ERROR (gtk_file_chooser_error_quark ())
;;; 
;;; Used to get the GError quark for GtkFileChooser errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkFileChooserError
;;;
;;; typedef enum {
;;;   GTK_FILE_CHOOSER_ERROR_NONEXISTENT,
;;;   GTK_FILE_CHOOSER_ERROR_BAD_FILENAME,
;;;   GTK_FILE_CHOOSER_ERROR_ALREADY_EXISTS,
;;;   GTK_FILE_CHOOSER_ERROR_INCOMPLETE_HOSTNAME
;;; } GtkFileChooserError;
;;; 
;;; These identify the various errors that can occur while calling
;;; GtkFileChooser functions.
;;; 
;;; GTK_FILE_CHOOSER_ERROR_NONEXISTENT
;;;     Indicates that a file does not exist.
;;; 
;;; GTK_FILE_CHOOSER_ERROR_BAD_FILENAME
;;;     Indicates a malformed filename.
;;; 
;;; GTK_FILE_CHOOSER_ERROR_ALREADY_EXISTS
;;;     Indicates a duplicate path (e.g. when adding a bookmark).
;;; 
;;; GTK_FILE_CHOOSER_ERROR_INCOMPLETE_HOSTNAME
;;;     Indicates an incomplete hostname (e.g. "http://foo" without a slash
;;;     after that).
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkFileChooserError" gtk-file-chooser-error
  (:export t
   :type-initializer "gtk_file_chooser_error_get_type")
  (:nonexistent 0)
  (:bad-filename 1)
  (:already-exists 2)
  (:incomplete-hostname 3))

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_action ()
;;; 
;;; void gtk_file_chooser_set_action (GtkFileChooser *chooser,
;;;                                   GtkFileChooserAction action);
;;; 
;;; Sets the type of operation that the chooser is performing; the user
;;; interface is adapted to suit the selected action. For example, an option
;;; to create a new folder might be shown if the action is
;;; GTK_FILE_CHOOSER_ACTION_SAVE but not if the action is
;;; GTK_FILE_CHOOSER_ACTION_OPEN.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; action :
;;;     the action that the file selector is performing
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_action ()
;;; 
;;; GtkFileChooserAction gtk_file_chooser_get_action (GtkFileChooser *chooser)
;;; 
;;; Gets the type of operation that the file chooser is performing;
;;; see gtk_file_chooser_set_action().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     the action that the file selector is performing
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_local_only ()
;;; 
;;; void gtk_file_chooser_set_local_only (GtkFileChooser *chooser,
;;;                                       gboolean local_only);
;;; 
;;; Sets whether only local files can be selected in the file selector. If
;;; local_only is TRUE (the default), then the selected file are files are
;;; guaranteed to be accessible through the operating systems native file file
;;; system and therefore the application only needs to worry about the filename
;;; functions in GtkFileChooser, like gtk_file_chooser_get_filename(), rather
;;; than the URI functions like gtk_file_chooser_get_uri(),
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; local_only :
;;;     TRUE if only local files can be selected
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_local_only ()
;;; 
;;; gboolean gtk_file_chooser_get_local_only (GtkFileChooser *chooser);
;;; 
;;; Gets whether only local files can be selected in the file selector.
;;; See gtk_file_chooser_set_local_only()
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     TRUE if only local files can be selected.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_select_multiple ()
;;; 
;;; void gtk_file_chooser_set_select_multiple (GtkFileChooser *chooser,
;;;                                            gboolean select_multiple);
;;; 
;;; Sets whether multiple files can be selected in the file selector. This is
;;; only relevant if the action is set to be GTK_FILE_CHOOSER_ACTION_OPEN or
;;; GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; select_multiple :
;;;     TRUE if multiple files can be selected.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_select_multiple ()
;;; 
;;; gboolean gtk_file_chooser_get_select_multiple (GtkFileChooser *chooser);
;;; 
;;; Gets whether multiple files can be selected in the file selector. See
;;; gtk_file_chooser_set_select_multiple().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     TRUE if multiple files can be selected.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_show_hidden ()
;;; 
;;; void gtk_file_chooser_set_show_hidden (GtkFileChooser *chooser,
;;;                                        gboolean show_hidden);
;;; 
;;; Sets whether hidden files and folders are displayed in the file selector.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; show_hidden :
;;;     TRUE if hidden files and folders should be displayed.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_show_hidden ()
;;; 
;;; gboolean gtk_file_chooser_get_show_hidden (GtkFileChooser *chooser);
;;; 
;;; Gets whether hidden files and folders are displayed in the file selector.
;;; See gtk_file_chooser_set_show_hidden().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     TRUE if hidden files and folders are displayed.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_do_overwrite_confirmation ()
;;; 
;;; void gtk_file_chooser_set_do_overwrite_confirmation
;;;                                         (GtkFileChooser *chooser,
;;;                                          gboolean do_overwrite_confirmation)
;;; 
;;; Sets whether a file chooser in GTK_FILE_CHOOSER_ACTION_SAVE mode will
;;; present a confirmation dialog if the user types a file name that already
;;; exists. This is FALSE by default.
;;; 
;;; Regardless of this setting, the chooser will emit the "confirm-overwrite"
;;; signal when appropriate.
;;; 
;;; If all you need is the stock confirmation dialog, set this property to TRUE.
;;; You can override the way confirmation is done by actually handling the
;;; "confirm-overwrite" signal; please refer to its documentation for the
;;; details.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; do_overwrite_confirmation :
;;;     whether to confirm overwriting in save mode
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_do_overwrite_confirmation ()
;;; 
;;; gboolean gtk_file_chooser_get_do_overwrite_confirmation
;;;                                                    (GtkFileChooser *chooser)
;;; 
;;; Queries whether a file chooser is set to confirm for overwriting when the
;;; user types a file name that already exists.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     TRUE if the file chooser will present a confirmation dialog;
;;;     FALSE otherwise.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_create_folders ()
;;; 
;;; void gtk_file_chooser_set_create_folders (GtkFileChooser *chooser,
;;;                                           gboolean create_folders);
;;; 
;;; Sets whether file choser will offer to create new folders. This is only
;;; relevant if the action is not set to be GTK_FILE_CHOOSER_ACTION_OPEN.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; create_folders :
;;;     TRUE if the New Folder button should be displayed
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_create_folders ()
;;; 
;;; gboolean gtk_file_chooser_get_create_folders (GtkFileChooser *chooser);
;;; 
;;; Gets whether file choser will offer to create new folders.
;;; See gtk_file_chooser_set_create_folders().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     TRUE if the New Folder button should be displayed.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_current_name ()
;;; 
;;; void gtk_file_chooser_set_current_name (GtkFileChooser *chooser,
;;;                                         const gchar *name);
;;; 
;;; Sets the current name in the file selector, as if entered by the user. Note
;;; that the name passed in here is a UTF-8 string rather than a filename. This
;;; function is meant for such uses as a suggested name in a "Save As..."
;;; dialog. You can pass "Untitled.doc" or a similarly suitable suggestion for
;;; the name.
;;; 
;;; If you want to preselect a particular existing file, you should use
;;; gtk_file_chooser_set_filename() or gtk_file_chooser_set_uri() instead.
;;; Please see the documentation for those functions for an example of using
;;; gtk_file_chooser_set_current_name() as well.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; name :
;;;     the filename to use, as a UTF-8 string
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_filename ()
;;; 
;;; gchar * gtk_file_chooser_get_filename (GtkFileChooser *chooser);
;;; 
;;; Gets the filename for the currently selected file in the file selector. If
;;; multiple files are selected, one of the filenames will be returned at
;;; random.
;;; 
;;; If the file chooser is in folder mode, this function returns the selected
;;; folder.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     The currently selected filename, or NULL if no file is selected, or the
;;;     selected file can't be represented with a local filename. Free with
;;;     g_free().
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_filename ()
;;; 
;;; gboolean gtk_file_chooser_set_filename (GtkFileChooser *chooser,
;;;                                         const char *filename);
;;; 
;;; Sets filename as the current filename for the file chooser, by changing to
;;; the file's parent folder and actually selecting the file in list; all other
;;; files will be unselected. If the chooser is in GTK_FILE_CHOOSER_ACTION_SAVE
;;; mode, the file's base name will also appear in the dialog's file name entry.
;;; 
;;; Note that the file must exist, or nothing will be done except for the
;;; directory change.
;;; 
;;; You should use this function only when implementing a File/Save As... dialog
;;; for which you already have a file name to which the user may save. For
;;; example, when the user opens an existing file and then does File/Save As...
;;; on it to save a copy or a modified version. If you don't have a file name
;;; already — for example, if the user just created a new file and is saving it
;;; for the first time, do not call this function. Instead, use something
;;; similar to this:
;;; 
;;; if (document_is_new)
;;;   {
;;;     /* the user just created a new document */
;;;     gtk_file_chooser_set_current_name (chooser, "Untitled document");
;;;   }
;;; else
;;;   {
;;;     /* the user edited an existing document */ 
;;;     gtk_file_chooser_set_filename (chooser, existing_filename);
;;;   }
;;; 
;;; In the first case, the file chooser will present the user with useful
;;; suggestions as to where to save his new file. In the second case, the file's
;;; existing location is already known, so the file chooser will use it.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; filename :
;;;     the filename to set as current
;;; 
;;; Returns :
;;;     Not useful.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_filename ()
;;; 
;;; gboolean gtk_file_chooser_select_filename (GtkFileChooser *chooser,
;;;                                            const char *filename);
;;; 
;;; Selects a filename. If the file name isn't in the current folder of chooser,
;;; then the current folder of chooser will be changed to the folder containing
;;; filename.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; filename :
;;;     the filename to select
;;; 
;;; Returns :
;;;     Not useful. See also: gtk_file_chooser_set_filename()
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_filename ()
;;; 
;;; void gtk_file_chooser_unselect_filename (GtkFileChooser *chooser,
;;;                                          const char *filename);
;;; 
;;; Unselects a currently selected filename. If the filename is not in the
;;; current directory, does not exist, or is otherwise not currently selected,
;;; does nothing.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; filename :
;;;     the filename to unselect
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_all ()
;;; 
;;; void gtk_file_chooser_select_all (GtkFileChooser *chooser);
;;; 
;;; Selects all the files in the current folder of a file chooser.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_all ()
;;; 
;;; void gtk_file_chooser_unselect_all (GtkFileChooser *chooser);
;;; 
;;; Unselects all the files in the current folder of a file chooser.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_filenames ()
;;; 
;;; GSList * gtk_file_chooser_get_filenames (GtkFileChooser *chooser);
;;; 
;;; Lists all the selected files and subfolders in the current folder of
;;; chooser. The returned names are full absolute paths. If files in the current
;;; folder cannot be represented as local filenames they will be ignored.
;;; (See gtk_file_chooser_get_uris())
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     a GSList containing the filenames of all selected files and subfolders
;;;     in the current folder. Free the returned list with g_slist_free(), and
;;;     the filenames with g_free()
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_current_folder ()
;;; 
;;; gboolean gtk_file_chooser_set_current_folder (GtkFileChooser *chooser,
;;;                                               const gchar *filename);
;;; 
;;; Sets the current folder for chooser from a local filename. The user will be
;;; shown the full contents of the current folder, plus user interface elements
;;; for navigating to other folders.
;;; 
;;; In general, you should not use this function. See the section on setting up
;;; a file chooser dialog for the rationale behind this.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; filename :
;;;     the full path of the new current folder
;;; 
;;; Returns :
;;;     Not useful.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_folder ()
;;; 
;;; gchar * gtk_file_chooser_get_current_folder (GtkFileChooser *chooser);
;;; 
;;; Gets the current folder of chooser as a local filename. See
;;; gtk_file_chooser_set_current_folder().
;;; 
;;; Note that this is the folder that the file chooser is currently displaying
;;; (e.g. "/home/username/Documents"), which is not the same as the
;;; currently-selected folder if the chooser is in
;;; GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER mode (e.g.
;;; "/home/username/Documents/selected-folder/". To get the currently-selected
;;; folder in that mode, use gtk_file_chooser_get_uri() as the usual way to get
;;; the selection.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     the full path of the current folder, or NULL if the current path cannot
;;;     be represented as a local filename. Free with g_free(). This function
;;;     will also return NULL if the file chooser was unable to load the last
;;;     folder that was requested from it; for example, as would be for calling
;;;     gtk_file_chooser_set_current_folder() on a nonexistent folder
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_uri ()
;;; 
;;; gchar * gtk_file_chooser_get_uri (GtkFileChooser *chooser);
;;; 
;;; Gets the URI for the currently selected file in the file selector. If
;;; multiple files are selected, one of the filenames will be returned at
;;; random.
;;; 
;;; If the file chooser is in folder mode, this function returns the selected
;;; folder.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     The currently selected URI, or NULL if no file is selected. Free
;;;     with g_free()
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_uri ()
;;; 
;;; gboolean gtk_file_chooser_set_uri (GtkFileChooser *chooser, const char *uri)
;;; 
;;; Sets the file referred to by uri as the current file for the file chooser,
;;; by changing to the URI's parent folder and actually selecting the URI in the
;;; list. If the chooser is GTK_FILE_CHOOSER_ACTION_SAVE mode, the URI's base
;;; name will also appear in the dialog's file name entry.
;;; 
;;; Note that the URI must exist, or nothing will be done except for the
;;; directory change.
;;; 
;;; You should use this function only when implementing a File/Save As... dialog
;;; for which you already have a file name to which the user may save. For
;;; example, whenthe user opens an existing file and then does File/Save As...
;;; on it to save a copy or a modified version. If you don't have a file name
;;; already — for example, if the user just created a new file and is saving it
;;; for the first time, do not call this function. Instead, use something
;;; similar to this:
;;; 
;;; if (document_is_new)
;;;   {
;;;     /* the user just created a new document */
;;;     gtk_file_chooser_set_current_name (chooser, "Untitled document");
;;;   }
;;; else
;;;   {
;;;     /* the user edited an existing document */ 
;;;     gtk_file_chooser_set_uri (chooser, existing_uri);
;;;   }
;;; 
;;; In the first case, the file chooser will present the user with useful
;;; suggestions as to where to save his new file. In the second case, the file's
;;; existing location is already known, so the file chooser will use it.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; uri :
;;;     the URI to set as current
;;; 
;;; Returns :
;;;     Not useful.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_uri ()
;;; 
;;; gboolean gtk_file_chooser_select_uri (GtkFileChooser *chooser,
;;;                                       const char *uri);
;;; 
;;; Selects the file to by uri. If the URI doesn't refer to a file in the
;;; current folder of chooser, then the current folder of chooser will be
;;; changed to the folder containing filename.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; uri :
;;;     the URI to select
;;; 
;;; Returns :
;;;     Not useful.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_uri ()
;;; 
;;; void gtk_file_chooser_unselect_uri (GtkFileChooser *chooser,
;;;                                     const char *uri);
;;; 
;;; Unselects the file referred to by uri. If the file is not in the current
;;; directory, does not exist, or is otherwise not currently selected, does
;;; nothing.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; uri :
;;;     the URI to unselect
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_uris ()
;;; 
;;; GSList * gtk_file_chooser_get_uris (GtkFileChooser *chooser);
;;; 
;;; Lists all the selected files and subfolders in the current folder of
;;; chooser. The returned names are full absolute URIs.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     a GSList containing the URIs of all selected files and subfolders in
;;;     the current folder. Free the returned list with g_slist_free(), and the
;;;     filenames with g_free()
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_current_folder_uri ()
;;; 
;;; gboolean gtk_file_chooser_set_current_folder_uri (GtkFileChooser *chooser,
;;;                                                   const gchar *uri);
;;; 
;;; Sets the current folder for chooser from an URI. The user will be shown the
;;; full contents of the current folder, plus user interface elements for
;;; navigating to other folders.
;;; 
;;; In general, you should not use this function. See the section on setting up
;;; a file chooser dialog for the rationale behind this.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; uri :
;;;     the URI for the new current folder
;;; 
;;; Returns :
;;;     TRUE if the folder could be changed successfully, FALSE otherwise.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_folder_uri ()
;;; 
;;; gchar * gtk_file_chooser_get_current_folder_uri (GtkFileChooser *chooser);
;;; 
;;; Gets the current folder of chooser as an URI.
;;; See gtk_file_chooser_set_current_folder_uri().
;;; 
;;; Note that this is the folder that the file chooser is currently displaying
;;; (e.g. "file:///home/username/Documents"), which is not the same as the
;;; currently-selected folder if the chooser is in
;;; GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER mode (e.g.
;;; "file:///home/username/Documents/selected-folder/". To get the
;;; currently-selected folder in that mode, use gtk_file_chooser_get_uri() as
;;; the usual way to get the selection.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     the URI for the current folder. Free with g_free(). This function will
;;;     also return NULL if the file chooser was unable to load the last folder
;;;     that was requested from it; for example, as would be for calling
;;;     gtk_file_chooser_set_current_folder_uri() on a nonexistent folder
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_preview_widget ()
;;; 
;;; void gtk_file_chooser_set_preview_widget (GtkFileChooser *chooser,
;;;                                           GtkWidget *preview_widget);
;;; 
;;; Sets an application-supplied widget to use to display a custom preview of
;;; the currently selected file. To implement a preview, after setting the
;;; preview widget, you connect to the "update-preview" signal, and call
;;; gtk_file_chooser_get_preview_filename() or
;;; gtk_file_chooser_get_preview_uri() on each change. If you can display a
;;; preview of the new file, update your widget and set the preview active using
;;; gtk_file_chooser_set_preview_widget_active(). Otherwise, set the preview
;;; inactive.
;;; 
;;; When there is no application-supplied preview widget, or the
;;; application-supplied preview widget is not active, the file chooser may
;;; display an internally generated preview of the current file or it may
;;; display no preview at all.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; preview_widget :
;;;     widget for displaying preview.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_widget ()
;;; 
;;; GtkWidget * gtk_file_chooser_get_preview_widget (GtkFileChooser *chooser);
;;; 
;;; Gets the current preview widget; see gtk_file_chooser_set_preview_widget().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     the current preview widget, or NULL
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_preview_widget_active ()
;;; 
;;; void gtk_file_chooser_set_preview_widget_active (GtkFileChooser *chooser,
;;;                                                  gboolean active);
;;; 
;;; Sets whether the preview widget set by gtk_file_chooser_set_preview_widget()
;;; should be shown for the current filename. When active is set to false, the
;;; file chooser may display an internally generated preview of the current file
;;; or it may display no preview at all.
;;; See gtk_file_chooser_set_preview_widget() for more details.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; active :
;;;     whether to display the user-specified preview widget
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_widget_active ()
;;; 
;;; gboolean gtk_file_chooser_get_preview_widget_active
;;;                                                    (GtkFileChooser *chooser)
;;; 
;;; Gets whether the preview widget set by gtk_file_chooser_set_preview_widget()
;;; should be shown for the current filename.
;;; See gtk_file_chooser_set_preview_widget_active().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     TRUE if the preview widget is active for the current filename.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_use_preview_label ()
;;; 
;;; void gtk_file_chooser_set_use_preview_label (GtkFileChooser *chooser,
;;;                                              gboolean use_label);
;;; 
;;; Sets whether the file chooser should display a stock label with the name of
;;; the file that is being previewed; the default is TRUE. Applications that
;;; want to draw the whole preview area themselves should set this to FALSE and
;;; display the name themselves in their preview widget.
;;; 
;;; See also: gtk_file_chooser_set_preview_widget()
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; use_label :
;;;     whether to display a stock label with the name of the previewed file
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_use_preview_label ()
;;; 
;;; gboolean gtk_file_chooser_get_use_preview_label (GtkFileChooser *chooser);
;;; 
;;; Gets whether a stock label should be drawn with the name of the previewed
;;; file. See gtk_file_chooser_set_use_preview_label().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     TRUE if the file chooser is set to display a label with the name of the
;;;     previewed file, FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_filename ()
;;; 
;;; char * gtk_file_chooser_get_preview_filename (GtkFileChooser *chooser);
;;; 
;;; Gets the filename that should be previewed in a custom preview widget.
;;; See gtk_file_chooser_set_preview_widget().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     the filename to preview, or NULL if no file is selected, or if the
;;;     selected file cannot be represented as a local filename.
;;;     Free with g_free().
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_uri ()
;;; 
;;; char * gtk_file_chooser_get_preview_uri (GtkFileChooser *chooser);
;;; 
;;; Gets the URI that should be previewed in a custom preview widget.
;;; See gtk_file_chooser_set_preview_widget().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     the URI for the file to preview, or NULL if no file is selected.
;;;     Free with g_free().
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_extra_widget ()
;;; 
;;; void gtk_file_chooser_set_extra_widget (GtkFileChooser *chooser,
;;;                                         GtkWidget *extra_widget);
;;; 
;;; Sets an application-supplied widget to provide extra options to the user.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; extra_widget :
;;;     widget for extra options
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_extra_widget ()
;;; 
;;; GtkWidget * gtk_file_chooser_get_extra_widget (GtkFileChooser *chooser);
;;; 
;;; Gets the current preview widget; see gtk_file_chooser_set_extra_widget().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     the current extra widget, or NULL
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_filter ()
;;; 
;;; void gtk_file_chooser_add_filter (GtkFileChooser *chooser,
;;;                                   GtkFileFilter *filter);
;;; 
;;; Adds filter to the list of filters that the user can select between. When a
;;; filter is selected, only files that are passed by that filter are displayed.
;;; 
;;; Note that the chooser takes ownership of the filter, so you have to ref and
;;; sink it if you want to keep a reference.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; filter :
;;;     a GtkFileFilter
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_filter ()
;;; 
;;; void gtk_file_chooser_remove_filter (GtkFileChooser *chooser,
;;;                                      GtkFileFilter *filter);
;;; 
;;; Removes filter from the list of filters that the user can select between.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; filter :
;;;     a GtkFileFilter
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_list_filters ()
;;; 
;;; GSList * gtk_file_chooser_list_filters (GtkFileChooser *chooser);
;;; 
;;; Lists the current set of user-selectable filters; see
;;; gtk_file_chooser_add_filter(), gtk_file_chooser_remove_filter().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     a GSList containing the current set of user selectable filters. The
;;;     contents of the list are owned by GTK+, but you must free the list
;;;     itself with g_slist_free() when you are done with it.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_filter ()
;;; 
;;; void gtk_file_chooser_set_filter (GtkFileChooser *chooser,
;;;                                   GtkFileFilter *filter);
;;; 
;;; Sets the current filter; only the files that pass the filter will be
;;; displayed. If the user-selectable list of filters is non-empty, then the
;;; filter should be one of the filters in that list. Setting the current filter
;;; when the list of filters is empty is useful if you want to restrict the
;;; displayed set of files without letting the user change it.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; filter :
;;;     a GtkFileFilter
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_filter ()
;;; 
;;; GtkFileFilter * gtk_file_chooser_get_filter (GtkFileChooser *chooser);
;;; 
;;; Gets the current filter; see gtk_file_chooser_set_filter().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     the current filter, or NULL
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_shortcut_folder ()
;;; 
;;; gboolean gtk_file_chooser_add_shortcut_folder (GtkFileChooser *chooser,
;;;                                                const char *folder,
;;;                                                GError **error);
;;; 
;;; Adds a folder to be displayed with the shortcut folders in a file chooser.
;;; Note that shortcut folders do not get saved, as they are provided by the
;;; application. For example, you can use this to add a
;;; "/usr/share/mydrawprogram/Clipart" folder to the volume list.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; folder :
;;;     filename of the folder to add
;;; 
;;; error :
;;;     location to store error, or NULL
;;; 
;;; Returns :
;;;     TRUE if the folder could be added successfully, FALSE otherwise. In the
;;;     latter case, the error will be set as appropriate.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_shortcut_folder ()
;;; 
;;; gboolean gtk_file_chooser_remove_shortcut_folder (GtkFileChooser *chooser,
;;;                                                   const char *folder,
;;;                                                   GError **error);
;;; 
;;; Removes a folder from a file chooser's list of shortcut folders.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; folder :
;;;     filename of the folder to remove
;;; 
;;; error :
;;;     location to store error, or NULL
;;; 
;;; Returns :
;;;     TRUE if the operation succeeds, FALSE otherwise. In the latter case,
;;;     the error will be set as appropriate.
;;;     See also: gtk_file_chooser_add_shortcut_folder()
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_list_shortcut_folders ()
;;; 
;;; GSList * gtk_file_chooser_list_shortcut_folders (GtkFileChooser *chooser);
;;; 
;;; Queries the list of shortcut folders in the file chooser, as set by
;;; gtk_file_chooser_add_shortcut_folder().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     A list of folder filenames, or NULL if there are no shortcut folders.
;;;     Free the returned list with g_slist_free(), and the filenames with
;;;     g_free().
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_shortcut_folder_uri ()
;;; 
;;; gboolean gtk_file_chooser_add_shortcut_folder_uri (GtkFileChooser *chooser,
;;;                                                    const char *uri,
;;;                                                    GError **error);
;;; 
;;; Adds a folder URI to be displayed with the shortcut folders in a file
;;; chooser. Note that shortcut folders do not get saved, as they are provided
;;; by the application. For example, you can use this to add a
;;; "file:///usr/share/mydrawprogram/Clipart" folder to the volume list.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; uri :
;;;     URI of the folder to add
;;; 
;;; error :
;;;     location to store error, or NULL
;;; 
;;; Returns :
;;;     TRUE if the folder could be added successfully, FALSE otherwise. In the
;;;     latter case, the error will be set as appropriate.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_shortcut_folder_uri ()
;;; 
;;; gboolean gtk_file_chooser_remove_shortcut_folder_uri
;;;                                                    (GtkFileChooser *chooser,
;;;                                                     const char *uri,
;;;                                                     GError **error);
;;; 
;;; Removes a folder URI from a file chooser's list of shortcut folders.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; uri :
;;;     URI of the folder to remove
;;; 
;;; error :
;;;     location to store error, or NULL
;;; 
;;; Returns :
;;;     TRUE if the operation succeeds, FALSE otherwise. In the latter case,
;;;     the error will be set as appropriate.
;;;     See also: gtk_file_chooser_add_shortcut_folder_uri()
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_list_shortcut_folder_uris ()
;;; 
;;; GSList * gtk_file_chooser_list_shortcut_folder_uris
;;;                                                    (GtkFileChooser *chooser)
;;; 
;;; Queries the list of shortcut folders in the file chooser, as set by
;;; gtk_file_chooser_add_shortcut_folder_uri().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     A list of folder URIs, or NULL if there are no shortcut folders. Free
;;;     the returned list with g_slist_free(), and the URIs with g_free().
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_folder_file ()
;;; 
;;; GFile * gtk_file_chooser_get_current_folder_file (GtkFileChooser *chooser);
;;; 
;;; Gets the current folder of chooser as GFile.
;;; See gtk_file_chooser_get_current_folder_uri().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     the GFile for the current folder
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_file ()
;;; 
;;; GFile * gtk_file_chooser_get_file (GtkFileChooser *chooser);
;;; 
;;; Gets the GFile for the currently selected file in the file selector. If
;;; multiple files are selected, one of the files will be returned at random.
;;; 
;;; If the file chooser is in folder mode, this function returns the selected
;;; folder.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     a selected GFile. You own the returned file; use g_object_unref() to
;;;     release it
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_files ()
;;; 
;;; GSList * gtk_file_chooser_get_files (GtkFileChooser *chooser);
;;; 
;;; Lists all the selected files and subfolders in the current folder of chooser
;;; as GFile. An internal function, see gtk_file_chooser_get_uris().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     a GSList containing a GFile for each selected file and subfolder in the
;;;     current folder. Free the returned list with g_slist_free(), and the
;;;     files with g_object_unref()
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_file ()
;;; 
;;; GFile * gtk_file_chooser_get_preview_file (GtkFileChooser *chooser);
;;; 
;;; Gets the GFile that should be previewed in a custom preview Internal
;;; function, see gtk_file_chooser_get_preview_uri().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; Returns :
;;;     the GFile for the file to preview, or NULL if no file is selected.
;;;     Free with g_object_unref()
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_file ()
;;; 
;;; gboolean gtk_file_chooser_select_file (GtkFileChooser *chooser,
;;;                                        GFile *file,
;;;                                        GError **error);
;;; 
;;; Selects the file referred to by file. An internal function.
;;; See _gtk_file_chooser_select_uri().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; file :
;;;     the file to select
;;; 
;;; error :
;;;     location to store error, or NULL
;;; 
;;; Returns :
;;;     Not useful.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_current_folder_file ()
;;; 
;;; gboolean gtk_file_chooser_set_current_folder_file (GtkFileChooser *chooser,
;;;                                                    GFile *file,
;;;                                                    GError **error);
;;; 
;;; Sets the current folder for chooser from a GFile. Internal function,
;;; see gtk_file_chooser_set_current_folder_uri().
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; file :
;;;     the GFile for the new folder
;;; 
;;; error :
;;;     location to store error, or NULL
;;; 
;;; Returns :
;;;     TRUE if the folder could be changed successfully, FALSE otherwise
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_file ()
;;; 
;;; gboolean gtk_file_chooser_set_file (GtkFileChooser *chooser,
;;;                                     GFile *file,
;;;                                     GError **error);
;;; 
;;; Sets file as the current filename for the file chooser, by changing to the
;;; file's parent folder and actually selecting the file in list. If the chooser
;;; is in GTK_FILE_CHOOSER_ACTION_SAVE mode, the file's base name will also
;;; appear in the dialog's file name entry.
;;; 
;;; If the file name isn't in the current folder of chooser, then the current
;;; folder of chooser will be changed to the folder containing filename. This
;;; is equivalent to a sequence of gtk_file_chooser_unselect_all() followed by
;;; gtk_file_chooser_select_filename().
;;; 
;;; Note that the file must exist, or nothing will be done except for the
;; directory change.
;;; 
;;; If you are implementing a File/Save As... dialog, you should use this
;;; function if you already have a file name to which the user may save; for
;;; example, when the user opens an existing file and then does File/Save As...
;;; on it. If you don't have a file name already — for example, if the user just
;;; created a new file and is saving it for the first time, do not call this
;;; function. Instead, use something similar to this:
;;;  
;;; if (document_is_new)
;;;   {
;;;     /* the user just created a new document */
;;;     gtk_file_chooser_set_current_folder_file (chooser,
;;;                                               default_file_for_saving);
;;;     gtk_file_chooser_set_current_name (chooser, "Untitled document");
;;;   }
;;; else
;;;   {
;;;     /* the user edited an existing document */
;;;     gtk_file_chooser_set_file (chooser, existing_file);
;;;   }
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; file :
;;;     the GFile to set as current
;;; 
;;; error :
;;;     location to store the error, or NULL to ignore errors
;;; 
;;; Returns :
;;;     Not useful.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_file ()
;;; 
;;; void gtk_file_chooser_unselect_file (GtkFileChooser *chooser, GFile *file);
;;; 
;;; Unselects the file referred to by file. If the file is not in the current
;;; directory, does not exist, or is otherwise not currently selected, does
;;; nothing.
;;; 
;;; chooser :
;;;     a GtkFileChooser
;;; 
;;; file :
;;;     a GFile
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.file-chooser.lisp --------------------------------------
