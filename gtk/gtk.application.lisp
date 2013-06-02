;;; ----------------------------------------------------------------------------
;;; gtk.application.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;;     gtk_application_get_window_by_id
;;;     gtk_application_get_active_window
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
;;;     gtk_application_add_accelerator
;;;     gtk_application_remove_accelerator
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkApplication
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkApplication" gtk-application
  (:superclass g-application
   :export t
   :interfaces ("GActionGroup"
                "GActionMap")
   :type-initializer "gtk_application_get_type")
   ((active-window
     gtk-application-active-window
     "active-window" "GtkWindow" t nil)
    (app-menu
     gtk-application-app-menu
     "app-menu" "GMenuModel" t t)
    (menubar
     gtk-application-menubar
     "menubar" "GMenuModel" t t)
    (register-session
     gtk-application-register-session
     "register-session" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-application 'type)
 "@version{2013-5-29}
  @begin{short}
    @sym{gtk-application} is a class that handles many important aspects of a
    GTK+ application in a convenient fashion, without enforcing a
    one-size-fits-all application model.
  @end{short}

  Currently, @sym{gtk-application} handles GTK+ initialization, application
  uniqueness, session management, provides some basic scriptability and
  desktop shell integration by exporting actions and menus and manages a list
  of toplevel windows whose life-cycle is automatically tied to the life-cycle
  of your application.

  While @sym{gtk-application} works fine with plain @class{gtk-window}'s, it is
  recommended to use it together with @class{gtk-application-window}.

  When GDK threads are enabled, @sym{gtk-application} will acquire the GDK lock
  when invoking actions that arrive from other processes. The GDK lock is not
  touched for local action invocations. In order to have actions invoked in a
  predictable context it is therefore recommended that the GDK lock be held
  while invoking actions locally with the function
  @fun{g-action-group-activate-action}. The same applies to actions associated
  with @class{gtk-application-window} and to the 'activate' and 'open'
  @class{g-application} methods.

  To set an application menu for a @sym{gtk-application}, use the function
  @fun{gtk-application-set-app-menu}. The @code{GMenuModel} that this function
  expects is usually constructed using @class{gtk-builder}, as seen in the
  following example. To specify a menubar that will be shown by
  @class{gtk-application-window}'s, use the function
  @fun{gtk-application-set-menubar}. Use the base @class{g-action-map} interface
  to add actions, to respond to the user selecting these menu items.

  GTK+ displays these menus as expected, depending on the platform the
  application is running on.

  @b{Example:} A simple application
  @begin{pre}
 #include <stdlib.h>
 #include <gtk/gtk.h>

 static void
 activate_toggle (GSimpleAction *action,
                  GVariant      *parameter,
                  gpointer       user_data)
 {
   GVariant *state;

   state = g_action_get_state (G_ACTION (action));
   g_action_change_state
                    (G_ACTION (action),
                     g_variant_new_boolean (!g_variant_get_boolean (state)));
   g_variant_unref (state);
 @}

 static void
 activate_radio (GSimpleAction *action,
                 GVariant      *parameter,
                 gpointer       user_data)
 {
   g_action_change_state (G_ACTION (action), parameter);
 @}

 static void
 change_fullscreen_state (GSimpleAction *action,
                          GVariant      *state,
                          gpointer       user_data)
 {
   if (g_variant_get_boolean (state))
     gtk_window_fullscreen (user_data);
   else
     gtk_window_unfullscreen (user_data);

   g_simple_action_set_state (action, state);
 @}

 static void
 change_justify_state (GSimpleAction *action,
                       GVariant      *state,
                       gpointer       user_data)
 {
   GtkTextView *text = g_object_get_data (user_data, \"bloatpad-text\");
   const gchar *str;

   str = g_variant_get_string (state, NULL);

   if (g_str_equal (str, \"left\"))
     gtk_text_view_set_justification (text, GTK_JUSTIFY_LEFT);
   else if (g_str_equal (str, \"center\"))
     gtk_text_view_set_justification (text, GTK_JUSTIFY_CENTER);
   else if (g_str_equal (str, \"right\"))
     gtk_text_view_set_justification (text, GTK_JUSTIFY_RIGHT);
   else
     /* ignore this attempted change */
     return;

   g_simple_action_set_state (action, state);
 @}

 static GtkClipboard *
 get_clipboard (GtkWidget *widget)
 {
   return gtk_widget_get_clipboard
                               (widget,
                                gdk_atom_intern_static_string (\"CLIPBOARD\"));
 @}

 static void
 window_copy (GSimpleAction *action,
              GVariant      *parameter,
              gpointer       user_data)
 {
   GtkWindow *window = GTK_WINDOW (user_data);
   GtkTextView *text = g_object_get_data ((GObject*)window, \"bloatpad-text\");

   gtk_text_buffer_copy_clipboard (gtk_text_view_get_buffer (text),
                                   get_clipboard ((GtkWidget*) text));
 @}

 static void
 window_paste (GSimpleAction *action,
               GVariant      *parameter,
               gpointer       user_data)
 {
   GtkWindow *window = GTK_WINDOW (user_data);
   GtkTextView *text = g_object_get_data ((GObject*)window, \"bloatpad-text\");

   gtk_text_buffer_paste_clipboard (gtk_text_view_get_buffer (text),
                                    get_clipboard ((GtkWidget*) text),
                                    NULL,
                                    TRUE);

 @}

 static GActionEntry win_entries[] = {
   { \"copy\", window_copy, NULL, NULL, NULL @},
   { \"paste\", window_paste, NULL, NULL, NULL @},
   { \"fullscreen\", activate_toggle, NULL, \"false\", change_fullscreen_state @},
   { \"justify\", activate_radio, \"s\", \"'left'\", change_justify_state @}
 @};

 static void
 new_window (GApplication *app,
             GFile        *file)
 {
   GtkWidget *window, *grid, *scrolled, *view;
   GtkWidget *toolbar;
   GtkToolItem *button;
   GtkWidget *sw, *box, *label;

   window = gtk_application_window_new (GTK_APPLICATION (app));
   gtk_window_set_default_size ((GtkWindow*)window, 640, 480);
   g_action_map_add_action_entries (G_ACTION_MAP (window), win_entries,
                                    G_N_ELEMENTS (win_entries), window);
   gtk_window_set_title (GTK_WINDOW (window), \"Bloatpad\");

   grid = gtk_grid_new ();
   gtk_container_add (GTK_CONTAINER (window), grid);

   toolbar = gtk_toolbar_new ();
   button = gtk_toggle_tool_button_new_from_stock (GTK_STOCK_JUSTIFY_LEFT);
   gtk_actionable_set_detailed_action_name (GTK_ACTIONABLE (button),
                                            \"win.justify::left\");
   gtk_container_add (GTK_CONTAINER (toolbar), GTK_WIDGET (button));

   button = gtk_toggle_tool_button_new_from_stock (GTK_STOCK_JUSTIFY_CENTER);
   gtk_actionable_set_detailed_action_name (GTK_ACTIONABLE (button),
                                            \"win.justify::center\");
   gtk_container_add (GTK_CONTAINER (toolbar), GTK_WIDGET (button));

   button = gtk_toggle_tool_button_new_from_stock (GTK_STOCK_JUSTIFY_RIGHT);
   gtk_actionable_set_detailed_action_name (GTK_ACTIONABLE (button),
                                            \"win.justify::right\");
   gtk_container_add (GTK_CONTAINER (toolbar), GTK_WIDGET (button));

   button = gtk_separator_tool_item_new ();
   gtk_separator_tool_item_set_draw (GTK_SEPARATOR_TOOL_ITEM (button),
                                     FALSE);
   gtk_tool_item_set_expand (GTK_TOOL_ITEM (button), TRUE);
   gtk_container_add (GTK_CONTAINER (toolbar), GTK_WIDGET (button));

   button = gtk_tool_item_new ();
   box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
   gtk_container_add (GTK_CONTAINER (button), box);
   label = gtk_label_new (\"Fullscreen:\");
   gtk_container_add (GTK_CONTAINER (box), label);
   sw = gtk_switch_new ();
   gtk_actionable_set_action_name (GTK_ACTIONABLE (sw), \"win.fullscreen\");
   gtk_container_add (GTK_CONTAINER (box), sw);
   gtk_container_add (GTK_CONTAINER (toolbar), GTK_WIDGET (button));

   gtk_grid_attach (GTK_GRID (grid), toolbar, 0, 0, 1, 1);

   scrolled = gtk_scrolled_window_new (NULL, NULL);
   gtk_widget_set_hexpand (scrolled, TRUE);
   gtk_widget_set_vexpand (scrolled, TRUE);
   view = gtk_text_view_new ();

   g_object_set_data ((GObject*)window, \"bloatpad-text\", view);

   gtk_container_add (GTK_CONTAINER (scrolled), view);

   gtk_grid_attach (GTK_GRID (grid), scrolled, 0, 1, 1, 1);

   if (file != NULL)
     {
       gchar *contents;
       gsize length;

       if (g_file_load_contents (file, NULL, &contents, &length, NULL, NULL))
         {
           GtkTextBuffer *buffer;

           buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
           gtk_text_buffer_set_text (buffer, contents, length);
           g_free (contents);
         @}
     @}

   gtk_widget_show_all (GTK_WIDGET (window));
 @}

 static void
 bloat_pad_activate (GApplication *application)
 {
   new_window (application, NULL);
 @}

 static void
 bloat_pad_open (GApplication  *application,
                 GFile        **files,
                 gint           n_files,
                 const gchar   *hint)
 {
   gint i;

   for (i = 0; i < n_files; i++)
     new_window (application, files[i]);
 @}

 typedef GtkApplication BloatPad;
 typedef GtkApplicationClass BloatPadClass;

 G_DEFINE_TYPE (BloatPad, bloat_pad, GTK_TYPE_APPLICATION)

 static void
 bloat_pad_finalize (GObject *object)
 {
   G_OBJECT_CLASS (bloat_pad_parent_class)->finalize (object);
 @}

 static void
 new_activated (GSimpleAction *action,
                GVariant      *parameter,
                gpointer       user_data)
 {
   GApplication *app = user_data;

   g_application_activate (app);
 @}

 static void
 about_activated (GSimpleAction *action,
                  GVariant      *parameter,
                  gpointer       user_data)
 {
   gtk_show_about_dialog (NULL,
                          \"program-name\", \"Bloatpad\",
                          \"title\", \"About Bloatpad\",
                          \"comments\", \"Not much to say, really.\",
                          NULL);
 @}

 static void
 quit_activated (GSimpleAction *action,
                 GVariant      *parameter,
                 gpointer       user_data)
 {
   GApplication *app = user_data;

   g_application_quit (app);
 @}

 static GActionEntry app_entries[] = {
   { \"new\", new_activated, NULL, NULL, NULL @},
   { \"about\", about_activated, NULL, NULL, NULL @},
   { \"quit\", quit_activated, NULL, NULL, NULL @},
 @};

 static void
 bloat_pad_startup (GApplication *application)
 {
   GtkBuilder *builder;

   G_APPLICATION_CLASS (bloat_pad_parent_class)
     ->startup (application);

   g_action_map_add_action_entries (G_ACTION_MAP (application),
                                    app_entries,
                                    G_N_ELEMENTS (app_entries),
                                    application);

   builder = gtk_builder_new ();
   gtk_builder_add_from_string (builder,
     \"<interface>\"
     \"  <menu id='app-menu'>\"
     \"    <section>\"
     \"      <item>\"
     \"        <attribute name='label' translatable='yes'>_New Window</attribute>\"
     \"        <attribute name='action'>app.new</attribute>\"
     \"        <attribute name='accel'>&lt;Primary&gt;n</attribute>\"
     \"      </item>\"
     \"    </section>\"
     \"    <section>\"
     \"      <item>\"
     \"        <attribute name='label' translatable='yes'>_About Bloatpad</attribute>\"
     \"        <attribute name='action'>app.about</attribute>\"
     \"      </item>\"
     \"    </section>\"
     \"    <section>\"
     \"      <item>\"
     \"        <attribute name='label' translatable='yes'>_Quit</attribute>\"
     \"        <attribute name='action'>app.quit</attribute>\"
     \"        <attribute name='accel'>&lt;Primary&gt;q</attribute>\"
     \"      </item>\"
     \"    </section>\"
     \"  </menu>\"
     \"  <menu id='menubar'>\"
     \"    <submenu>\"
     \"      <attribute name='label' translatable='yes'>_Edit</attribute>\"
     \"      <section>\"
     \"        <item>\"
     \"          <attribute name='label' translatable='yes'>_Copy</attribute>\"
     \"          <attribute name='action'>win.copy</attribute>\"
     \"          <attribute name='accel'>&lt;Primary&gt;c</attribute>\"
     \"        </item>\"
     \"        <item>\"
     \"          <attribute name='label' translatable='yes'>_Parse</attribute>\"
     \"          <attribute name='action'>win.parse</attribute>\"
     \"          <attribute name='accel'>&lt;Primary&gt;v</attribute>\"
     \"        </item>\"
     \"      </section>\"
     \"    </submenu>\"
     \"    <submenu>\"
     \"      <attribute name='label' translatable='yes'>_View</attribute>\"
     \"      <section>\"
     \"        <item>\"
     \"          <attribute name='label' translatable='yes'>_Fullscreen</attribute>\"
     \"          <attribute name='action'>win.fullscreen</attribute>\"
     \"          <attribute name='accel'>F11</attribute>\"
     \"        </item>\"
     \"      </section>\"
     \"    </submenu>\"
     \"  </menu>\"
     \"</interface>\", -1, NULL);
   gtk_application_set_app_menu
               (GTK_APPLICATION (application),
                G_MENU_MODEL (gtk_builder_get_object (builder, \"app-menu\")));
   gtk_application_set_menubar
                (GTK_APPLICATION (application),
                 G_MENU_MODEL (gtk_builder_get_object (builder, \"menubar\")));
   g_object_unref (builder);
 @}

 static void
 bloat_pad_init (BloatPad *app)
 {
 @}

 static void
 bloat_pad_class_init (BloatPadClass *class)
 {
   GApplicationClass *application_class = G_APPLICATION_CLASS (class);
   GObjectClass *object_class = G_OBJECT_CLASS (class);

   application_class->startup = bloat_pad_startup;
   application_class->activate = bloat_pad_activate;
   application_class->open = bloat_pad_open;

   object_class->finalize = bloat_pad_finalize;

 @}

 BloatPad *
 bloat_pad_new (void)
 {
   GtkApplication *bloat_pad;

   g_type_init ();

   g_set_application_name (\"Bloatpad\");

   bloat_pad = g_object_new (bloat_pad_get_type (),
                             \"application-id\", \"org.gtk.Test.bloatpad\",
                             \"flags\", G_APPLICATION_HANDLES_OPEN,
                             \"inactivity-timeout\", 30000,
                             \"register-session\", TRUE,
                             NULL);

   return bloat_pad;
 @}

 int
 main (int argc, char **argv)
 {
   BloatPad *bloat_pad;
   int status;

   bloat_pad = bloat_pad_new ();

   gtk_application_add_accelerator (GTK_APPLICATION (bloat_pad),
                                    \"F11\", \"win.fullscreen\", NULL);

   status = g_application_run (G_APPLICATION (bloat_pad), argc, argv);

   g_object_unref (bloat_pad);

   return status;
 @}
  @end{pre}
  @sym{gtk-application} optionally registers with a session manager of the
  users session (if you set the @code{\"register-session\"} property) and
  offers various functionality related to the session life-cycle.

  An application can block various ways to end the session with the
  @fun{gtk-application-inhibit} function. Typical use cases for this kind of
  inhibiting are long-running, uninterruptible operations, such as burning a
  CD or performing a disk backup. The session manager may not honor the
  inhibitor, but it can be expected to inform the user about the negative
  consequences of ending the session while inhibitors are present.
  @begin[Signal Details]{dictionary}
    @subheading{The \"window-added\" signal}
      @begin{pre}
 lambda (application window)   : Run First
      @end{pre}
      Emitted when a @class{gtk-window} is added to application through the
      @fun{gtk-application-add-window} function.
      @begin[code]{table}
        @entry[application]{The @sym{gtk-application} which emitted the signal.}
        @entry[window]{The newly added @class{gtk-window} widget.}
      @end{table}
      Since 3.2

    @subheading{The \"window-removed\" signal}
      @begin{pre}
 lambda (application window)   : Run First
      @end{pre}
      Emitted when a @class{gtk-window} is removed from application, either as
      a side-effect of being destroyed or explicitly through the
      @fun{gtk-application-remove-window} function.
      @begin[code]{table}
        @entry[application]{The @sym{gtk-application} which emitted the signal.}
        @entry[window]{The @class{gtk-window} that is being removed.}
      @end{table}
      Since 3.2
  @end{dictionary}
  @see-slot{gtk-application-active-window}
  @see-slot{gtk-application-app-menu}
  @see-slot{gtk-application-menubar}
  @see-slot{gtk-application-register-session}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active-window"
                                               'gtk-application) 't)
 "The @code{\"active-window\"} property of type @code{gtk-window} (Read) @br{}
  The window which most recently had focus.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "app-menu" 'gtk-application) 't)
 "The @code{\"app-menu\"} property of type @code{GMenuModel} (Read / Write)@br{}
  The GMenuModel for the application menu.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "menubar" 'gtk-application) 't)
 "The @code{\"menubar\"} property of type @code{GMenuModel} (Read / Write)@br{}
  The GMenuModel for the menubar.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "register-session"
                                               'gtk-application) 't)
 "The @code{\"register-session\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Set this property to TRUE to register with the session manager.@br{}
  Default value: @code{nil}@br{}
  Since 3.4")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-active-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-active-window 'function)
 "@version{2013-5-29}
  Accessor of the slot @code{\"active-window\"} of the @class{gtk-application}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-app-menu atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-app-menu 'function)
 "@version{2013-5-29}
  Accessor of the slot @code{\"app-menu\"} of the @class{gtk-application}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-menubar atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-menubar 'function)
 "@version{2013-5-29}
  Accessor of the slot @code{\"menubar\"} of the @class{gtk-application}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-register-session atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-register-session 'function)
 "@version{2013-5-29}
  Accessor of the slot @code{\"register-session\"} of the
  @class{gtk-application} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_application_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-new))

(defun gtk-application-new (application-id flags)
 "@version{2013-5-29}
  @argument[application-id]{the application id}
  @argument[flags]{the application flags}
  @return{A new @class{gtk-application} object.}
  @begin{short}
    Creates a new @class{gtk-application} object.
  @end{short}

  This function calls the @fun{g-type-init} function for you. The @fun{gtk-init}
  function is called as soon as the application gets registered as the primary
  instance.

  Concretely, the @fun{gtk-init} function is called in the default handler for
  the \"startup\" signal. Therefore, @class{gtk-application} subclasses should
  chain up in their \"startup\" handler before using any GTK+ API.

  Note that commandline arguments are not passed to the @fun{gtk-init} function.
  All GTK+ functionality that is available via commandline arguments can also be
  achieved by setting suitable environment variables such as @code{G_DEBUG}, so
  this should not be a big problem. If you absolutely must support GTK+
  commandline arguments, you can explicitly call the @fun{gtk-init} function
  before creating the application instance.

  The application id must be valid. See the @fun{g-application-id-is-valid}
  function.

  Since 3.0"
  (make-instance 'gtk-application
                 :application-id application-id
                 :flags flags))

(export 'gtk-application-new)

;;; ----------------------------------------------------------------------------
;;; gtk_application_add_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_add_window" gtk-application-add-window) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-29}
  @argument[application]{a @class{gtk-application} object}
  @argument[window]{a @class{gtk-window} object}
  @begin{short}
    Adds a window to application.
  @end{short}

  This call is equivalent to setting the @code{\"application\"} property of
  @arg{window} to @arg{application}.

  Normally, the connection between the application and the window will remain
  until the window is destroyed, but you can explicitly remove it with the
  @fun{gtk-application-remove-window}.

  GTK+ will keep the application running as long as it has any windows.

  Since 3.0"
  (application (g-object gtk-application))
  (window (g-object gtk-window)))

(export 'gtk-application-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_remove_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_remove_window" gtk-application-remove-window) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-29}
  @argument[application]{a @class{gtk-application} object}
  @argument[window]{a @class{gtk-window} object}
  @begin{short}
    Remove a window from application.
  @end{short}

  If window belongs to application then this call is equivalent to setting the
  @code{\"application\"} property of window to @code{nil}.

  The application may stop running as a result of a call to this function.

  Since 3.0"
  (application (g-object gtk-application))
  (window (g-object gtk-window)))

(export 'gtk-application-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_windows ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_get_windows" gtk-application-get-windows)
    (g-list (g-object gtk-window))
 #+cl-cffi-gtk-documentation
 "@version{2013-5-29}
  @argument[application]{a @class{gtk-application} object}
  @return{A list of @class{gkt-window} objects}
  @begin{short}
    Gets a list of the @class{gtk-window}'s associated with @arg{application}.
  @end{short}

  The list is sorted by most recently focused window, such that the first
  element is the currently focused window. (Useful for choosing a parent for a
  transient window.)

  The list that is returned should not be modified in any way. It will only
  remain valid until the next focus change or window creation or deletion.

  Since 3.0"
  (application (g-object gtk-application)))

(export 'gtk-application-get-windows)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_window_by_id ()
;;;
;;; GtkWindow * gtk_application_get_window_by_id (GtkApplication *application,
;;;                                               guint id);
;;;
;;; Returns the GtkApplicationWindow with the given ID.
;;;
;;; application :
;;;     a GtkApplication
;;;
;;; id :
;;;     an identifier number
;;;
;;; Returns :
;;;     The window with ID id, or NULL if there is no window with this ID.
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_active_window ()
;;;
;;; GtkWindow * gtk_application_get_active_window (GtkApplication *application);
;;;
;;; Gets the "active" window for the application.
;;;
;;; The active window is the one that was most recently focused (within the
;;; application). This window may not have the focus at the moment if another
;;; application has it - this is just the most recently-focused window within
;;; this application.
;;;
;;; application :
;;;     a GtkApplication
;;;
;;; Returns :
;;;     the active window
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

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
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-get-app-menu))

(defun gtk-application-get-app-menu (application)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-29}
  @argument[application]{a @class{gtk-application} object}
  @return{The application menu of @arg{application}.}
  @begin{short}
    Returns the menu model that has been set with the
    @fun{gtk-application-set-app-menu} function.
  @end{short}

  Since 3.4"
  (gtk-application-app-menu application))

(export 'gtk-application-get-app-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_application_set_app_menu ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-set-app-menu))

(defun gtk-application-set-app-menu (application app-menu)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-29}
  @argument[application]{a @class{gtk-application} object}
  @argument[app-menu]{a @code{GMenuModel}, or @code{nil}}
  @begin{short}
    Sets or unsets the application menu for @arg{application}.
  @end{short}

  This can only be done in the primary instance of the application, after it
  has been registered. \"startup\" is a good place to call this.

  The application menu is a single menu containing items that typically impact
  the application as a whole, rather than acting on a specific window or
  document. For example, you would expect to see \"Preferences\" or \"Quit\" in
  an application menu, but not \"Save\" or \"Print\".

  If supported, the application menu will be rendered by the desktop
  environment.

  Use the base @class{g-action-map} interface to add actions, to respond to the
  user selecting these menu items.

  Since 3.4"
  (setf (gtk-application-app-menu application) app-menu))

(export 'gtk-application-set-app-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_menubar ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-get-menubar))

(defun gtk-application-get-menubar (application)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-29}
  @argument[application]{a @class{gtk-application} object}
  @return{The menubar for windows of @arg{application}.}
  @begin{short}
    Returns the menu model that has been set with the
    @fun{gtk-application-set-menubar} function.
  @end{short}

  Since 3.4"
  (gtk-application-menubar application))

(export 'gtk-application-get-menubar)

;;; ----------------------------------------------------------------------------
;;; gtk_application_set_menubar ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-set-menubar))

(defun gtk-application-set-menubar (application menubar)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-29}
  @argument[application]{a @class{gtk-application} object}
  @argument[menubar]{a @code{GMenuModel}, or @code{nil}}
  @begin{short}
    Sets or unsets the menubar for windows of @arg{application}.
  @end{short}

  This is a menubar in the traditional sense.

  This can only be done in the primary instance of the application, after it
  has been registered. \"startup\" is a good place to call this.

  Depending on the desktop environment, this may appear at the top of each
  window, or at the top of the screen. In some environments, if both the
  application menu and the menubar are set, the application menu will be
  presented as if it were the first item of the menubar. Other environments
  treat the two as completely separate - for example, the application menu
  may be rendered by the desktop shell while the menubar (if set) remains in
  each individual window.

  Use the base @class{g-action-map} interface to add actions, to respond to the
  user selecting these menu items.

  Since 3.4"
  (setf (gtk-application-menubar application) menubar))

(export 'gtk-application-set-menubar)

;;; ----------------------------------------------------------------------------
;;; gtk_application_add_accelerator ()
;;;
;;; void gtk_application_add_accelerator (GtkApplication *application,
;;;                                       const gchar *accelerator,
;;;                                       const gchar *action_name,
;;;                                       GVariant *parameter);
;;;
;;; Installs an accelerator that will cause the named action to be activated
;;; when the key combination specificed by accelerator is pressed.
;;;
;;; accelerator must be a string that can be parsed by gtk_accelerator_parse(),
;;; e. g. "<Primary>q" or "<Control><Alt>p".
;;;
;;; action_name must be the name of an action as it would be used in the app
;;; menu, i.e. actions that have been added to the application are referred to
;;; with an "app." prefix, and window-specific actions with a "win." prefix.
;;;
;;; GtkApplication also extracts accelerators out of 'accel' attributes in the
;;; GMenuModels passed to gtk_application_set_app_menu() and
;;; gtk_application_set_menubar(), which is usually more convenient than calling
;;; this function for each accelerator.
;;;
;;; application :
;;;     a GtkApplication
;;;
;;; accelerator :
;;;     accelerator string
;;;
;;; action_name :
;;;     the name of the action to activate
;;;
;;; parameter :
;;;     parameter to pass when activating the action, or NULL if the action does
;;;     not accept an activation parameter
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_application_remove_accelerator ()
;;;
;;; void gtk_application_remove_accelerator (GtkApplication *application,
;;;                                          const gchar *action_name,
;;;                                          GVariant *parameter);
;;;
;;; Removes an accelerator that has been previously added with
;;; gtk_application_add_accelerator().
;;;
;;; application :
;;;     a GtkApplication
;;;
;;; action_name :
;;;     the name of the action to activate
;;;
;;; parameter :
;;;     parameter to pass when activating the action, or NULL if the action
;;;     does not accept an activation parameter
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.application.lisp ---------------------------------------
