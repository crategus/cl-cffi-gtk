;;; ----------------------------------------------------------------------------
;;; gtk.application.lisp
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;; GtkApplication
;;; 
;;; Application class
;;;     
;;; Synopsis
;;; 
;;;     GtkApplication
;;;
;;;     gtk_application_new
;;;     gtk_application_add_window
;;;     gtk_application_remove_window
;;;     gtk_application_get_windows
;;;     
;;;     GtkApplicationInhibitFlags
;;;
;;;     gtk_application_inhibit
;;;     gtk_application_uninhibit
;;;     gtk_application_is_inhibited
;;;     
;;;     gtk_application_get_app_menu
;;;     gtk_application_set_app_menu
;;;     gtk_application_get_menubar
;;;     gtk_application_set_menubar
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GApplication
;;;          +----GtkApplication
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkApplication implements GActionGroup and GActionMap.
;;;
;;; Properties
;;; 
;;;   "app-menu"                 GMenuModel*           : Read / Write
;;;   "menubar"                  GMenuModel*           : Read / Write
;;;   "register-session"         gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "window-added"                                   : Run First
;;;   "window-removed"                                 : Run First
;;; 
;;; Description
;;; 
;;; GtkApplication is a class that handles many important aspects of a GTK+
;;; application in a convenient fashion, without enforcing a one-size-fits-all
;;; application model.
;;; 
;;; Currently, GtkApplication handles GTK+ initialization, application
;;; uniqueness, session management, provides some basic scriptability and
;;; desktop shell integration by exporting actions and menus and manages a list
;;; of toplevel windows whose life-cycle is automatically tied to the life-cycle
;;; of your application.
;;; 
;;; While GtkApplication works fine with plain GtkWindows, it is recommended to
;;; use it together with GtkApplicationWindow.
;;; 
;;; When GDK threads are enabled, GtkApplication will acquire the GDK lock when
;;; invoking actions that arrive from other processes. The GDK lock is not
;;; touched for local action invocations. In order to have actions invoked in a
;;; predictable context it is therefore recommended that the GDK lock be held
;;; while invoking actions locally with g_action_group_activate_action(). The
;;; same applies to actions associated with GtkApplicationWindow and to the
;;; 'activate' and 'open' GApplication methods.
;;; 
;;; To set an application menu for a GtkApplication, use
;;; gtk_application_set_app_menu(). The GMenuModel that this function expects is
;;; usually constructed using GtkBuilder, as seen in the following example. To
;;; specify a menubar that will be shown by GApplicationWindows, use
;;; gtk_application_set_menubar(). Use the base GActionMap interface to add
;;; actions, to respond to the user selecting these menu items.
;;; 
;;; GTK+ displays these menus as expected, depending on the platform the
;;; application is running on.
;;; 
;;; Example 110. A simple application
;;; 
;;; #include <stdlib.h>
;;; #include <gtk/gtk.h>
;;; 
;;; static void
;;; activate_toggle (GSimpleAction *action,
;;;                  GVariant      *parameter,
;;;                  gpointer       user_data)
;;; {
;;;   GVariant *state;
;;; 
;;;   state = g_action_get_state (G_ACTION (action));
;;;   g_action_change_state
;;;                    (G_ACTION (action),
;;;                     g_variant_new_boolean (!g_variant_get_boolean (state)));
;;;   g_variant_unref (state);
;;; }
;;; 
;;; static void
;;; activate_radio (GSimpleAction *action,
;;;                 GVariant      *parameter,
;;;                 gpointer       user_data)
;;; {
;;;   g_action_change_state (G_ACTION (action), parameter);
;;; }
;;; 
;;; static void
;;; change_fullscreen_state (GSimpleAction *action,
;;;                          GVariant      *state,
;;;                          gpointer       user_data)
;;; {
;;;   if (g_variant_get_boolean (state))
;;;     gtk_window_fullscreen (user_data);
;;;   else
;;;     gtk_window_unfullscreen (user_data);
;;; 
;;;   g_simple_action_set_state (action, state);
;;; }
;;; 
;;; static void
;;; change_justify_state (GSimpleAction *action,
;;;                       GVariant      *state,
;;;                       gpointer       user_data)
;;; {
;;;   GtkTextView *text = g_object_get_data (user_data, "bloatpad-text");
;;;   const gchar *str;
;;; 
;;;   str = g_variant_get_string (state, NULL);
;;; 
;;;   if (g_str_equal (str, "left"))
;;;     gtk_text_view_set_justification (text, GTK_JUSTIFY_LEFT);
;;;   else if (g_str_equal (str, "center"))
;;;     gtk_text_view_set_justification (text, GTK_JUSTIFY_CENTER);
;;;   else if (g_str_equal (str, "right"))
;;;     gtk_text_view_set_justification (text, GTK_JUSTIFY_RIGHT);
;;;   else
;;;     /* ignore this attempted change */
;;;     return;
;;; 
;;;   g_simple_action_set_state (action, state);
;;; }
;;; 
;;; static GtkClipboard *
;;; get_clipboard (GtkWidget *widget)
;;; {
;;;   return gtk_widget_get_clipboard
;;;                               (widget,
;;;                                gdk_atom_intern_static_string ("CLIPBOARD"));
;;; }
;;; 
;;; static void
;;; window_copy (GSimpleAction *action,
;;;              GVariant      *parameter,
;;;              gpointer       user_data)
;;; {
;;;   GtkWindow *window = GTK_WINDOW (user_data);
;;;   GtkTextView *text = g_object_get_data ((GObject*)window, "bloatpad-text");
;;; 
;;;   gtk_text_buffer_copy_clipboard (gtk_text_view_get_buffer (text),
;;;                                   get_clipboard ((GtkWidget*) text));
;;; }
;;; 
;;; static void
;;; window_paste (GSimpleAction *action,
;;;               GVariant      *parameter,
;;;               gpointer       user_data)
;;; {
;;;   GtkWindow *window = GTK_WINDOW (user_data);
;;;   GtkTextView *text = g_object_get_data ((GObject*)window, "bloatpad-text");
;;;   
;;;   gtk_text_buffer_paste_clipboard (gtk_text_view_get_buffer (text),
;;;                                    get_clipboard ((GtkWidget*) text),
;;;                                    NULL,
;;;                                    TRUE);
;;; 
;;; }
;;; 
;;; static GActionEntry win_entries[] = {
;;;   { "copy", window_copy, NULL, NULL, NULL },
;;;   { "paste", window_paste, NULL, NULL, NULL },
;;;   { "fullscreen", activate_toggle, NULL, "false", change_fullscreen_state },
;;;   { "justify", activate_radio, "s", "'left'", change_justify_state }
;;; };
;;; 
;;; static void
;;; new_window (GApplication *app,
;;;             GFile        *file)
;;; {
;;;   GtkWidget *window, *grid, *scrolled, *view;
;;;   GtkWidget *toolbar;
;;;   GtkToolItem *button;
;;;   GtkWidget *sw, *box, *label;
;;; 
;;;   window = gtk_application_window_new (GTK_APPLICATION (app));
;;;   gtk_window_set_default_size ((GtkWindow*)window, 640, 480);
;;;   g_action_map_add_action_entries (G_ACTION_MAP (window), win_entries,
;;;                                    G_N_ELEMENTS (win_entries), window);
;;;   gtk_window_set_title (GTK_WINDOW (window), "Bloatpad");
;;; 
;;;   grid = gtk_grid_new ();
;;;   gtk_container_add (GTK_CONTAINER (window), grid);
;;; 
;;;   toolbar = gtk_toolbar_new ();
;;;   button = gtk_toggle_tool_button_new_from_stock (GTK_STOCK_JUSTIFY_LEFT);
;;;   gtk_actionable_set_detailed_action_name (GTK_ACTIONABLE (button),
;;;                                            "win.justify::left");
;;;   gtk_container_add (GTK_CONTAINER (toolbar), GTK_WIDGET (button));
;;; 
;;;   button = gtk_toggle_tool_button_new_from_stock (GTK_STOCK_JUSTIFY_CENTER);
;;;   gtk_actionable_set_detailed_action_name (GTK_ACTIONABLE (button),
;;;                                            "win.justify::center");
;;;   gtk_container_add (GTK_CONTAINER (toolbar), GTK_WIDGET (button));
;;; 
;;;   button = gtk_toggle_tool_button_new_from_stock (GTK_STOCK_JUSTIFY_RIGHT);
;;;   gtk_actionable_set_detailed_action_name (GTK_ACTIONABLE (button),
;;;                                            "win.justify::right");
;;;   gtk_container_add (GTK_CONTAINER (toolbar), GTK_WIDGET (button));
;;; 
;;;   button = gtk_separator_tool_item_new ();
;;;   gtk_separator_tool_item_set_draw (GTK_SEPARATOR_TOOL_ITEM (button),
;;;                                     FALSE);
;;;   gtk_tool_item_set_expand (GTK_TOOL_ITEM (button), TRUE);
;;;   gtk_container_add (GTK_CONTAINER (toolbar), GTK_WIDGET (button));
;;; 
;;;   button = gtk_tool_item_new ();
;;;   box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
;;;   gtk_container_add (GTK_CONTAINER (button), box);
;;;   label = gtk_label_new ("Fullscreen:");
;;;   gtk_container_add (GTK_CONTAINER (box), label);
;;;   sw = gtk_switch_new ();
;;;   gtk_actionable_set_action_name (GTK_ACTIONABLE (sw), "win.fullscreen");
;;;   gtk_container_add (GTK_CONTAINER (box), sw);
;;;   gtk_container_add (GTK_CONTAINER (toolbar), GTK_WIDGET (button));
;;; 
;;;   gtk_grid_attach (GTK_GRID (grid), toolbar, 0, 0, 1, 1);
;;; 
;;;   scrolled = gtk_scrolled_window_new (NULL, NULL);
;;;   gtk_widget_set_hexpand (scrolled, TRUE);
;;;   gtk_widget_set_vexpand (scrolled, TRUE);
;;;   view = gtk_text_view_new ();
;;; 
;;;   g_object_set_data ((GObject*)window, "bloatpad-text", view);
;;; 
;;;   gtk_container_add (GTK_CONTAINER (scrolled), view);
;;; 
;;;   gtk_grid_attach (GTK_GRID (grid), scrolled, 0, 1, 1, 1);
;;; 
;;;   if (file != NULL)
;;;     {
;;;       gchar *contents;
;;;       gsize length;
;;; 
;;;       if (g_file_load_contents (file, NULL, &contents, &length, NULL, NULL))
;;;         {
;;;           GtkTextBuffer *buffer;
;;; 
;;;           buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
;;;           gtk_text_buffer_set_text (buffer, contents, length);
;;;           g_free (contents);
;;;         }
;;;     }
;;; 
;;;   gtk_widget_show_all (GTK_WIDGET (window));
;;; }
;;; 
;;; static void
;;; bloat_pad_activate (GApplication *application)
;;; {
;;;   new_window (application, NULL);
;;; }
;;; 
;;; static void
;;; bloat_pad_open (GApplication  *application,
;;;                 GFile        **files,
;;;                 gint           n_files,
;;;                 const gchar   *hint)
;;; {
;;;   gint i;
;;; 
;;;   for (i = 0; i < n_files; i++)
;;;     new_window (application, files[i]);
;;; }
;;; 
;;; typedef GtkApplication BloatPad;
;;; typedef GtkApplicationClass BloatPadClass;
;;; 
;;; G_DEFINE_TYPE (BloatPad, bloat_pad, GTK_TYPE_APPLICATION)
;;; 
;;; static void
;;; bloat_pad_finalize (GObject *object)
;;; {
;;;   G_OBJECT_CLASS (bloat_pad_parent_class)->finalize (object);
;;; }
;;; 
;;; static void
;;; new_activated (GSimpleAction *action,
;;;                GVariant      *parameter,
;;;                gpointer       user_data)
;;; {
;;;   GApplication *app = user_data;
;;; 
;;;   g_application_activate (app);
;;; }
;;; 
;;; static void
;;; about_activated (GSimpleAction *action,
;;;                  GVariant      *parameter,
;;;                  gpointer       user_data)
;;; {
;;;   gtk_show_about_dialog (NULL,
;;;                          "program-name", "Bloatpad",
;;;                          "title", "About Bloatpad",
;;;                          "comments", "Not much to say, really.",
;;;                          NULL);
;;; }
;;; 
;;; static void
;;; quit_activated (GSimpleAction *action,
;;;                 GVariant      *parameter,
;;;                 gpointer       user_data)
;;; {
;;;   GApplication *app = user_data;
;;; 
;;;   g_application_quit (app);
;;; }
;;; 
;;; static GActionEntry app_entries[] = {
;;;   { "new", new_activated, NULL, NULL, NULL },
;;;   { "about", about_activated, NULL, NULL, NULL },
;;;   { "quit", quit_activated, NULL, NULL, NULL },
;;; };
;;; 
;;; static void
;;; bloat_pad_startup (GApplication *application)
;;; {
;;;   GtkBuilder *builder;
;;; 
;;;   G_APPLICATION_CLASS (bloat_pad_parent_class)
;;;     ->startup (application);
;;; 
;;;   g_action_map_add_action_entries (G_ACTION_MAP (application),
;;;                                    app_entries,
;;;                                    G_N_ELEMENTS (app_entries),
;;;                                    application);
;;; 
;;;   builder = gtk_builder_new ();
;;;   gtk_builder_add_from_string (builder,
;;;     "<interface>"
;;;     "  <menu id='app-menu'>"
;;;     "    <section>"
;;;     "      <item>"
;;;     "        <attribute name='label' translatable='yes'>_New Window</attribute>"
;;;     "        <attribute name='action'>app.new</attribute>"
;;;     "        <attribute name='accel'>&lt;Primary&gt;n</attribute>"
;;;     "      </item>"
;;;     "    </section>"
;;;     "    <section>"
;;;     "      <item>"
;;;     "        <attribute name='label' translatable='yes'>_About Bloatpad</attribute>"
;;;     "        <attribute name='action'>app.about</attribute>"
;;;     "      </item>"
;;;     "    </section>"
;;;     "    <section>"
;;;     "      <item>"
;;;     "        <attribute name='label' translatable='yes'>_Quit</attribute>"
;;;     "        <attribute name='action'>app.quit</attribute>"
;;;     "        <attribute name='accel'>&lt;Primary&gt;q</attribute>"
;;;     "      </item>"
;;;     "    </section>"
;;;     "  </menu>"
;;;     "  <menu id='menubar'>"
;;;     "    <submenu>"
;;;     "      <attribute name='label' translatable='yes'>_Edit</attribute>"
;;;     "      <section>"
;;;     "        <item>"
;;;     "          <attribute name='label' translatable='yes'>_Copy</attribute>"
;;;     "          <attribute name='action'>win.copy</attribute>"
;;;     "          <attribute name='accel'>&lt;Primary&gt;c</attribute>"
;;;     "        </item>"
;;;     "        <item>"
;;;     "          <attribute name='label' translatable='yes'>_Parse</attribute>"
;;;     "          <attribute name='action'>win.parse</attribute>"
;;;     "          <attribute name='accel'>&lt;Primary&gt;v</attribute>"
;;;     "        </item>"
;;;     "      </section>"
;;;     "    </submenu>"
;;;     "    <submenu>"
;;;     "      <attribute name='label' translatable='yes'>_View</attribute>"
;;;     "      <section>"
;;;     "        <item>"
;;;     "          <attribute name='label' translatable='yes'>_Fullscreen</attribute>"
;;;     "          <attribute name='action'>win.fullscreen</attribute>"
;;;     "          <attribute name='accel'>F11</attribute>"
;;;     "        </item>"
;;;     "      </section>"
;;;     "    </submenu>"
;;;     "  </menu>"
;;;     "</interface>", -1, NULL);
;;;   gtk_application_set_app_menu
;;;               (GTK_APPLICATION (application),
;;;                G_MENU_MODEL (gtk_builder_get_object (builder, "app-menu")));
;;;   gtk_application_set_menubar
;;;                (GTK_APPLICATION (application),
;;;                 G_MENU_MODEL (gtk_builder_get_object (builder, "menubar")));
;;;   g_object_unref (builder);
;;; }
;;; 
;;; static void
;;; bloat_pad_init (BloatPad *app)
;;; {
;;; }
;;; 
;;; static void
;;; bloat_pad_class_init (BloatPadClass *class)
;;; {
;;;   GApplicationClass *application_class = G_APPLICATION_CLASS (class);
;;;   GObjectClass *object_class = G_OBJECT_CLASS (class);
;;; 
;;;   application_class->startup = bloat_pad_startup;
;;;   application_class->activate = bloat_pad_activate;
;;;   application_class->open = bloat_pad_open;
;;; 
;;;   object_class->finalize = bloat_pad_finalize;
;;; 
;;; }
;;; 
;;; BloatPad *
;;; bloat_pad_new (void)
;;; {
;;;   GtkApplication *bloat_pad;
;;; 
;;;   g_type_init ();
;;; 
;;;   g_set_application_name ("Bloatpad");
;;; 
;;;   bloat_pad = g_object_new (bloat_pad_get_type (),
;;;                             "application-id", "org.gtk.Test.bloatpad",
;;;                             "flags", G_APPLICATION_HANDLES_OPEN,
;;;                             "inactivity-timeout", 30000,
;;;                             "register-session", TRUE,
;;;                             NULL);
;;; 
;;;   return bloat_pad;
;;; }
;;; 
;;; int
;;; main (int argc, char **argv)
;;; {
;;;   BloatPad *bloat_pad;
;;;   int status;
;;; 
;;;   bloat_pad = bloat_pad_new ();
;;; 
;;;   gtk_application_add_accelerator (GTK_APPLICATION (bloat_pad),
;;;                                    "F11", "win.fullscreen", NULL);
;;; 
;;;   status = g_application_run (G_APPLICATION (bloat_pad), argc, argv);
;;; 
;;;   g_object_unref (bloat_pad);
;;; 
;;;   return status;
;;; }
;;; 
;;; 
;;; GtkApplication optionally registers with a session manager of the users
;;; session (if you set the "register-session" property) and offers various
;;; functionality related to the session life-cycle.
;;; 
;;; An application can block various ways to end the session with the
;;; gtk_application_inhibit() function. Typical use cases for this kind of
;;; inhibiting are long-running, uninterruptible operations, such as burning a
;;; CD or performing a disk backup. The session manager may not honor the
;;; inhibitor, but it can be expected to inform the user about the negative
;;; consequences of ending the session while inhibitors are present.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "app-menu" property
;;; 
;;;   "app-menu"                 GMenuModel*           : Read / Write
;;; 
;;; The GMenuModel for the application menu.
;;;
;;; ----------------------------------------------------------------------------
;;; The "menubar" property
;;; 
;;;   "menubar"                  GMenuModel*           : Read / Write
;;; 
;;; The GMenuModel for the menubar.
;;;
;;; ----------------------------------------------------------------------------
;;; The "register-session" property
;;; 
;;;   "register-session"         gboolean              : Read / Write
;;; 
;;; Set this property to TRUE to register with the session manager.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.4
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "window-added" signal
;;; 
;;; void user_function (GtkApplication *application,
;;;                     GtkWindow      *window,
;;;                     gpointer        user_data)        : Run First
;;; 
;;; Emitted when a GtkWindow is added to application through
;;; gtk_application_add_window().
;;; 
;;; application :
;;;     the GtkApplication which emitted the signal
;;; 
;;; window :
;;;     the newly-added GtkWindow
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 3.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "window-removed" signal
;;; 
;;; void user_function (GtkApplication *application,
;;;                     GtkWindow      *window,
;;;                     gpointer        user_data)        : Run First
;;; 
;;; Emitted when a GtkWindow is removed from application, either as a
;;; side-effect of being destroyed or explicitly through
;;; gtk_application_remove_window().
;;; 
;;; application :
;;;     the GtkApplication which emitted the signal
;;; 
;;; window :
;;;     the GtkWindow that is being removed
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkApplication
;;; 
;;; struct GtkApplication;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkApplication" gtk-application
  (:superclass g-application
   :export t
   :interfaces ("GActionGroup"
                "GActionMap")
   :type-initializer "gtk_application_get_type")
   ((app-menu
     gtk-application-app-menu
     "app-menu" "GMenuModel" t t)
    (menubar
     gtk-application-menubar
     "menubar" "GMenuModel" t t)
    (register-session
     gtk-application-register-session
     "register-session" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_application_new ()
;;; 
;;; GtkApplication * gtk_application_new (const gchar *application_id,
;;;                                       GApplicationFlags flags);
;;; 
;;; Creates a new GtkApplication instance.
;;; 
;;; This function calls g_type_init() for you. gtk_init() is called as soon as
;; the application gets registered as the primary instance.
;;; 
;;; Concretely, gtk_init() is called in the default handler for the "startup"
;;; signal. Therefore, GtkApplication subclasses should chain up in their
;;; "startup" handler before using any GTK+ API.
;;; 
;;; Note that commandline arguments are not passed to gtk_init(). All GTK+
;;; functionality that is available via commandline arguments can also be
;;; achieved by setting suitable environment variables such as G_DEBUG, so this
;;; should not be a big problem. If you absolutely must support GTK+ commandline
;;; arguments, you can explicitly call gtk_init() before creating the
;;; application instance.
;;; 
;;; The application id must be valid. See g_application_id_is_valid().
;;; 
;;; application_id :
;;;     the application id
;;; 
;;; flags :
;;;     the application flags
;;; 
;;; Returns :
;;;     a new GtkApplication instance
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-new))

(defun gtk-application-new (application-id flags)
  (make-instance 'gtk-application
                 :application-id application-id
                 :flags flags))

(export 'gtk-application-new)

;;; ----------------------------------------------------------------------------
;;; gtk_application_add_window ()
;;; 
;;; void gtk_application_add_window (GtkApplication *application,
;;;                                  GtkWindow *window);
;;; 
;;; Adds a window to application.
;;; 
;;; This call is equivalent to setting the "application" property of window to
;;; application.
;;; 
;;; Normally, the connection between the application and the window will remain
;;; until the window is destroyed, but you can explicitly remove it with
;;; gtk_application_remove_window().
;;; 
;;; GTK+ will keep the application running as long as it has any windows.
;;; 
;;; application :
;;;     a GtkApplication
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_add_window" gtk-application-add-window) :void
  (application (g-object gtk-application))
  (window (g-object gtk-window)))

(export 'gtk-application-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_remove_window ()
;;; 
;;; void gtk_application_remove_window (GtkApplication *application,
;;;                                     GtkWindow *window);
;;; 
;;; Remove a window from application.
;;; 
;;; If window belongs to application then this call is equivalent to setting the
;;; "application" property of window to NULL.
;;; 
;;; The application may stop running as a result of a call to this function.
;;; 
;;; application :
;;;     a GtkApplication
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_remove_window" gtk-application-remove-window) :void
  (application (g-object gtk-application))
  (window (g-object gtk-window)))

(export 'gtk-application-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_windows ()
;;; 
;;; GList * gtk_application_get_windows (GtkApplication *application);
;;; 
;;; Gets a list of the GtkWindows associated with application.
;;; 
;;; The list is sorted by most recently focused window, such that the first
;;; element is the currently focused window. (Useful for choosing a parent for a
;;; transient window.)
;;; 
;;; The list that is returned should not be modified in any way. It will only
;;; remain valid until the next focus change or window creation or deletion.
;;; 
;;; application :
;;;     a GtkApplication
;;; 
;;; Returns :
;;;     a GList of GtkWindow
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_get_windows" gtk-application-get-windows)
    (g-list (g-object gtk-window))
  (application (g-object gtk-application)))

(export 'gtk-application-get-windows)

;;; ----------------------------------------------------------------------------
;;; enum GtkApplicationInhibitFlags
;;; 
;;; typedef enum {
;;;   GTK_APPLICATION_INHIBIT_LOGOUT  = (1 << 0),
;;;   GTK_APPLICATION_INHIBIT_SWITCH  = (1 << 1),
;;;   GTK_APPLICATION_INHIBIT_SUSPEND = (1 << 2),
;;;   GTK_APPLICATION_INHIBIT_IDLE    = (1 << 3)
;;; } GtkApplicationInhibitFlags;
;;; 
;;; Types of user actions that may be blocked by gtk_application_inhibit().
;;; 
;;; GTK_APPLICATION_INHIBIT_LOGOUT
;;;     Inhibit ending the user session by logging out or by shutting down the
;;;     computer
;;; 
;;; GTK_APPLICATION_INHIBIT_SWITCH
;;;     Inhibit user switching
;;; 
;;; GTK_APPLICATION_INHIBIT_SUSPEND
;;;     Inhibit suspending the session or computer
;;; 
;;; GTK_APPLICATION_INHIBIT_IDLE
;;;     Inhibit the session being marked as idle (and possibly locked)
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_application_inhibit ()
;;; 
;;; guint gtk_application_inhibit (GtkApplication *application,
;;;                                GtkWindow *window,
;;;                                GtkApplicationInhibitFlags flags,
;;;                                const gchar *reason);
;;; 
;;; Inform the session manager that certain types of actions should be
;;; inhibited. This is not guaranteed to work on all platforms and for all types
;;; of actions.
;;; 
;;; Applications should invoke this method when they begin an operation that
;;; should not be interrupted, such as creating a CD or DVD. The types of
;;; actions that may be blocked are specified by the flags parameter. When the
;;; application completes the operation it should call g_application_uninhibit()
;;; to remove the inhibitor. Note that an application can have multiple
;;; inhibitors, and all of the must be individually removed. Inhibitors are also
;;; cleared when the application exits.
;;; 
;;; Applications should not expect that they will always be able to block the
;;; action. In most cases, users will be given the option to force the action to
;;; take place.
;;; 
;;; Reasons should be short and to the point.
;;; 
;;; If window is given, the session manager may point the user to this window to
;;; find out more about why the action is inhibited.
;;; 
;;; application :
;;;     the GApplication
;;; 
;;; window :
;;;     a GtkWindow, or NULL
;;;
;;; flags :
;;;     what types of actions should be inhibited
;;; 
;;; reason :
;;;     a short, human-readable string that explains why these operations are
;;;     inhibited
;;; 
;;; Returns :
;;;     A non-zero cookie that is used to uniquely identify this request. It
;;;     should be used as an argument to g_application_uninhibit() in order to
;;;     remove the request. If the platform does not support inhibiting or the
;;;     request failed for some reason, 0 is returned.
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_application_uninhibit ()
;;; 
;;; void gtk_application_uninhibit (GtkApplication *application, guint cookie);
;;; 
;;; Removes an inhibitor that has been established with g_application_inhibit().
;;; Inhibitors are also cleared when the application exits.
;;; 
;;; application :
;;;     the GApplication
;;; 
;;; cookie :
;;;     a cookie that was returned by g_application_inhibit()
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_application_is_inhibited ()
;;; 
;;; gboolean gtk_application_is_inhibited (GtkApplication *application,
;;;                                        GtkApplicationInhibitFlags flags);
;;; 
;;; Determines if any of the actions specified in flags are currently inhibited
;;; (possibly by another application).
;;; 
;;; application :
;;;     the GApplication
;;; 
;;; flags :
;;;     what types of actions should be queried
;;; 
;;; Returns :
;;;     TRUE if any of the actions specified in flags are inhibited
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_app_menu ()
;;; 
;;; GMenuModel * gtk_application_get_app_menu (GtkApplication *application);
;;; 
;;; Returns the menu model that has been set with
;;; gtk_application_set_app_menu().
;;; 
;;; application :
;;;     a GtkApplication
;;; 
;;; Returns :
;;;     the application menu of application
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-get-app-menu))

(defun gtk-application-get-app-menu (application)
  (gtk-application-app-menu application))

(export 'gtk-application-get-app-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_application_set_app_menu ()
;;; 
;;; void gtk_application_set_app_menu (GtkApplication *application,
;;;                                    GMenuModel *app_menu);
;;; 
;;; Sets or unsets the application menu for application.
;;; 
;;; This can only be done in the primary instance of the application, after it
;;; has been registered. "startup" is a good place to call this.
;;; 
;;; The application menu is a single menu containing items that typically impact
;;; the application as a whole, rather than acting on a specific window or
;;; document. For example, you would expect to see "Preferences" or "Quit" in an
;;; application menu, but not "Save" or "Print".
;;; 
;;; If supported, the application menu will be rendered by the desktop
;;; environment.
;;; 
;;; Use the base GActionMap interface to add actions, to respond to the user
;;; selecting these menu items.
;;; 
;;; application :
;;;     a GtkApplication
;;; 
;;; app_menu :
;;;     a GMenuModel, or NULL
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-set-app-menu))

(defun gtk-application-set-app-menu (application app-menu)
  (setf (gtk-application-app-menu application) app-menu))

(export 'gtk-application-set-app-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_menubar ()
;;; 
;;; GMenuModel * gtk_application_get_menubar (GtkApplication *application);
;;; 
;;; Returns the menu model that has been set with gtk_application_set_menubar().
;;; 
;;; application :
;;;     a GtkApplication
;;; 
;;; Returns :
;;;     the menubar for windows of application
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-get-menubar))

(defun gtk-application-get-menubar (application)
  (gtk-application-menubar application))

(export 'gtk-application-get-menubar)

;;; ----------------------------------------------------------------------------
;;; gtk_application_set_menubar ()
;;; 
;;; void gtk_application_set_menubar (GtkApplication *application,
;;;                                   GMenuModel *menubar);
;;; 
;;; Sets or unsets the menubar for windows of application.
;;; 
;;; This is a menubar in the traditional sense.
;;; 
;;; This can only be done in the primary instance of the application, after it
;;; has been registered. "startup" is a good place to call this.
;;; 
;;; Depending on the desktop environment, this may appear at the top of each
;;; window, or at the top of the screen. In some environments, if both the
;;; application menu and the menubar are set, the application menu will be
;;; presented as if it were the first item of the menubar. Other environments
;;; treat the two as completely separate -- for example, the application menu
;;; may be rendered by the desktop shell while the menubar (if set) remains in
;;; each individual window.
;;; 
;;; Use the base GActionMap interface to add actions, to respond to the user
;;; selecting these menu items.
;;; 
;;; application :
;;;     a GtkApplication
;;; 
;;; menubar :
;;;     a GMenuModel, or NULL
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-set-menubar))

(defun gtk-application-set-menubar (application menubar)
  (setf (gtk-application-menubar application) menubar))

(export 'gtk-application-set-menubar)

;;; --- End of file gtk.application.lisp ---------------------------------------
