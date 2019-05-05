;;; ----------------------------------------------------------------------------
;;; gtk.search-bar.lisp
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
;;; GtkSearchBar
;;;
;;;     A toolbar to integrate a search entry with
;;;
;;; Types and Values
;;;
;;;     GtkSearchBar
;;;
;;; Functions
;;;
;;;     gtk_search_bar_new
;;;     gtk_search_bar_connect_entry
;;;     gtk_search_bar_get_search_mode
;;;     gtk_search_bar_set_search_mode
;;;     gtk_search_bar_get_show_close_button               Accessor
;;;     gtk_search_bar_set_show_close_button               Accessor
;;;     gtk_search_bar_handle_event
;;;
;;; Properties
;;;
;;;     gboolean   search-mode-enabled    Read / Write
;;;     gboolean   show-close-button      Read / Write / Construct
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkSearchBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSearchBar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSearchBar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSearchBar" gtk-search-bar
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_search_bar_get_type")
  ((search-mode-enabled
    gtk-search-bar-search-mode-enabled
    "search-mode-enabled" "gboolean" t t)
   (show-close-button
    gtk-search-bar-show-close-button
    "show-close-button" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-search-bar 'type)
 "@version{2019-4-19}
  @begin{short}
    @class{gtk-search-bar} is a container made to have a search entry built-in,
    possibly with additional connex widgets, such as drop-down menus, or
    buttons.
  @end{short}
  The search bar would appear when a search is started through typing on the
  keyboard, or the application’s search mode is toggled on.

  @image[search-bar]{}

  For keyboard presses to start a search, events will need to be forwarded from
  the top-level window that contains the search bar. See the
  @func{gtk-search-bar-handle-event} function for example code. Common shortcuts
  such as Ctrl+F should be handled as an application action, or through the menu
  items.

  You will also need to tell the search bar about which entry you are using as
  your search entry using the @fun{gtk-search-bar-connect-entry} function. The
  following example shows you how to create a more complex search entry.
  @begin[CSS nodes]{dictionary}
    @sym{gtk-search-bar} has a single CSS node with name @code{searchbar}.
  @end{dictionary}
  @begin[Example]{dictionary}
    Creating a search bar.
    @begin{pre}
#include <gtk/gtk.h>

static gboolean
window_key_press_event_cb (GtkWidget *window,
    GdkEvent *event,
    GtkSearchBar *search_bar)
{
  return gtk_search_bar_handle_event (search_bar, event);
@}

static void
activate_cb (GtkApplication *app,
    gpointer user_data)
{
  GtkWidget *window;
  GtkWidget *search_bar;
  GtkWidget *box;
  GtkWidget *entry;
  GtkWidget *menu_button;

  window = gtk_application_window_new (app);
  gtk_widget_show (window);

  search_bar = gtk_search_bar_new ();
  gtk_container_add (GTK_CONTAINER (window), search_bar);
  gtk_widget_show (search_bar);

  box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
  gtk_container_add (GTK_CONTAINER (search_bar), box);
  gtk_widget_show (box);

  entry = gtk_search_entry_new ();
  gtk_box_pack_start (GTK_BOX (box), entry, TRUE, TRUE, 0);
  gtk_widget_show (entry);

  menu_button = gtk_menu_button_new ();
  gtk_box_pack_start (GTK_BOX (box), menu_button, FALSE, FALSE, 0);
  gtk_widget_show (menu_button);

  gtk_search_bar_connect_entry (GTK_SEARCH_BAR (search_bar), GTK_ENTRY (entry));

  g_signal_connect (window, \"key-press-event\",
      G_CALLBACK (window_key_press_event_cb), search_bar);
@}

gint
main (gint argc,
    gchar *argv[@])
{
  GtkApplication *app;

  app = gtk_application_new (\"org.gtk.Example.GtkSearchBar\",
      G_APPLICATION_FLAGS_NONE);
  g_signal_connect (app, \"activate\",
      G_CALLBACK (activate_cb), NULL);

  return g_application_run (G_APPLICATION (app), argc, argv);
@}
    @end{pre}
  @end{dictionary}
  @see-slot{gtk-search-bar-search-mode-enabled}
  @see-slot{gtk-search-bar-show-close-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-search-bar-search-mode-enabled -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "search-mode-enabled"
                                               'gtk-search-bar) 't)
 "The @code{search-mode-enabled} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the search mode is on and the search bar shown. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-search-bar-search-mode-enabled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-search-bar-search-mode-enabled 'function)
 "@version{2019-4-19}
  @begin{short}
    Accessor of the @slot[gtk-search-bar]{search-mode-enabled} slot of the
    @class{gtk-search-bar} class.
  @end{short}
  @see-class{gtk-search-bar}")

;;; --- gtk-search-bar-show-close-button ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-close-button"
                                               'gtk-search-bar) 't)
 "The @code{show-close-button} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether to show the close button in the toolbar. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-search-bar-show-close-button atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-search-bar-show-close-button 'function)
 "@version{2019-4-19}
  @begin{short}
    Accessor of the @slot[gtk-search-bar]{show-close-button} slot of the
    @class{gtk-search-bar} class.
  @end{short}
  @see-class{gtk-search-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_new ()
;;;
;;; GtkWidget * gtk_search_bar_new (void)
;;;
;;; Creates a GtkSearchBar. You will need to tell it about which widget is going
;;; to be your text entry using gtk_search_bar_connect_entry().
;;;
;;; Returns :
;;;     a new GtkSearchBar
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_connect_entry ()
;;;
;;; void
;;; gtk_search_bar_connect_entry (GtkSearchBar *bar, GtkEntry *entry);
;;;
;;; Connects the GtkEntry widget passed as the one to be used in this search
;;; bar. The entry should be a descendant of the search bar. This is only
;;; required if the entry isn’t the direct child of the search bar (as in our
;;; main example).
;;;
;;; bar :
;;;     a GtkSearchBar
;;;
;;; entry :
;;;     a GtkEntry
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_get_search_mode ()
;;;
;;; gboolean gtk_search_bar_get_search_mode (GtkSearchBar *bar);
;;;
;;; Returns whether the search mode is on or off.
;;;
;;; bar :
;;;     a GtkSearchBar
;;;
;;; Returns :
;;;     whether search mode is toggled on
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_set_search_mode ()
;;;
;;; void
;;; gtk_search_bar_set_search_mode (GtkSearchBar *bar, gboolean search_mode);
;;;
;;; Switches the search mode on or off.
;;;
;;; bar :
;;;     a GtkSearchBar
;;;
;;; search_mode :
;;;     the new state of the search mode
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_get_show_close_button ()
;;;
;;; gboolean gtk_search_bar_get_show_close_button (GtkSearchBar *bar);
;;;
;;; Returns whether the close button is shown.
;;;
;;; bar :
;;;     a GtkSearchBar
;;;
;;; Returns :
;;;     whether the close button is shown
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_set_show_close_button ()
;;;
;;; void
;;; gtk_search_bar_set_show_close_button (GtkSearchBar *bar,
;;;                                       gboolean visible);
;;;
;;; Shows or hides the close button. Applications that already have a “search”
;;; toggle button should not show a close button in their search bar, as it
;;; duplicates the role of the toggle button.
;;;
;;; bar :
;;;     a GtkSearchBar
;;;
;;; visible :
;;;     whether the close button will be shown or not
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_handle_event ()
;;;
;;; gboolean gtk_search_bar_handle_event (GtkSearchBar *bar, GdkEvent *event);
;;;
;;; This function should be called when the top-level window which contains the
;;; search bar received a key event.
;;;
;;; If the key event is handled by the search bar, the bar will be shown, the
;;; entry populated with the entered text and GDK_EVENT_STOP will be returned.
;;; The caller should ensure that events are not propagated further.
;;;
;;; If no entry has been connected to the search bar, using
;;; gtk_search_bar_connect_entry(), this function will return immediately with
;;; a warning.
;;;
;;; Showing the search bar on key presses
;;;
;;; static gboolean
;;; on_key_press_event (GtkWidget *widget,
;;;                     GdkEvent  *event,
;;;                     gpointer   user_data)
;;; {
;;;   GtkSearchBar *bar = GTK_SEARCH_BAR (user_data);
;;;   return gtk_search_bar_handle_event (bar, event);
;;; }
;;;
;;; static void
;;; create_toplevel (void)
;;; {
;;;   GtkWidget *window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;;   GtkWindow *search_bar = gtk_search_bar_new ();
;;;
;;;  // Add more widgets to the window...
;;;
;;;   g_signal_connect (window,
;;;                    "key-press-event",
;;;                     G_CALLBACK (on_key_press_event),
;;;                     search_bar);
;;; }
;;;
;;; bar :
;;;     a GtkSearchBar
;;;
;;; event :
;;;     a GdkEvent containing key press events
;;;
;;; Returns :
;;;     GDK_EVENT_STOP if the key press event resulted in text being entered in
;;;     the search entry (and revealing the search bar if necessary),
;;;     GDK_EVENT_PROPAGATE otherwise.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.search-bar.lisp ----------------------------------------
