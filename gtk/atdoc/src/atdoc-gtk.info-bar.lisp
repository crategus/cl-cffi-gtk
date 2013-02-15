;;; ----------------------------------------------------------------------------
;;; gtk.info-bar.lisp
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
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; 
;;; 
;;; 
;;; Style Properties
;;; 
;;;   "action-area-border"       gint                 : Read
;;;   "button-spacing"           gint                 : Read
;;;   "content-area-border"      gint                 : Read
;;;   "content-area-spacing"     gint                 : Read
;;; 
;;; Signals
;;; 
;;;   "close"                                         : Action
;;;   "response"                                      : Run Last
;;; 
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "action-area-border" style property
;;; 
;;;   "action-area-border"       gint                  : Read
;;; 
;;; Width of the border around the action area of the info bar.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 5
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "button-spacing" style property
;;; 
;;;   "button-spacing"           gint                  : Read
;;; 
;;; Spacing between buttons in the action area of the info bar.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 6
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "content-area-border" style property
;;; 
;;;   "content-area-border"      gint                  : Read
;;; 
;;; The width of the border around the content content area of the info bar.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 8
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "content-area-spacing" style property
;;; 
;;;   "content-area-spacing"     gint                  : Read
;;; 
;;; The default spacing used between elements of the content area of the info
;;; bar.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 16
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "close" signal
;;; 
;;; void user_function (GtkInfoBar *arg0,
;;;                     gpointer    user_data)      : Action
;;; 
;;; The ::close signal is a keybinding signal which gets emitted when the user
;;; uses a keybinding to dismiss the info bar.
;;; 
;;; The default binding for this signal is the Escape key.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "response" signal
;;; 
;;; void user_function (GtkInfoBar *info_bar,
;;;                     gint        response_id,
;;;                     gpointer    user_data)        : Run Last
;;; 
;;; Emitted when an action widget is clicked or the application programmer calls
;;; gtk_dialog_response(). The response_id depends on which action widget was
;;; clicked.
;;; 
;;; info_bar :
;;;     the object on which the signal is emitted
;;; 
;;; response_id :
;;;     the response ID
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; --- gtk-info-bar -----------------------------------------------------------

(setf (documentation 'gtk-info-bar 'type)
 "@version{2013-2-3}
  @begin{short}
    GtkInfoBar is a widget that can be used to show messages to the user without
    showing a dialog. It is often temporarily shown at the top or bottom of a
    document. In contrast to GtkDialog, which has a horizontal action area at
    the bottom, GtkInfoBar has a vertical action area at the side.
  @end{short}

  The API of GtkInfoBar is very similar to GtkDialog, allowing you to add
  buttons to the action area with gtk_info_bar_add_button() or
  gtk_info_bar_new_with_buttons(). The sensitivity of action widgets can be
  controlled with gtk_info_bar_set_response_sensitive(). To add widgets to the
  main content area of a GtkInfoBar, use gtk_info_bar_get_content_area() and
  add your widgets to the container.

  Similar to GtkMessageDialog, the contents of a GtkInfoBar can by classified
  as error message, warning, informational message, etc, by using
  gtk_info_bar_set_message_type(). GTK+ uses the message type to determine the
  background color of the message area.
 
  Example 52. Simple GtkInfoBar usage.
  @begin{pre}
 /* set up info bar */
 info_bar = gtk_info_bar_new ();
 gtk_widget_set_no_show_all (info_bar, TRUE);
 message_label = gtk_label_new (\"\");
 gtk_widget_show (message_label);
 content_area = gtk_info_bar_get_content_area (GTK_INFO_BAR (info_bar));
 gtk_container_add (GTK_CONTAINER (content_area), message_label);
 gtk_info_bar_add_button (GTK_INFO_BAR (info_bar),
                          GTK_STOCK_OK, GTK_RESPONSE_OK);
 g_signal_connect (info_bar, \"response\",
                   G_CALLBACK (gtk_widget_hide), NULL);
 gtk_grid_attach (GTK_GRID (grid),
                  info_bar,
                  0, 2, 1, 1);
 
 /* ... */
 
 /* show an error message */
 gtk_label_set_text (GTK_LABEL (message_label), error_message);
 gtk_info_bar_set_message_type (GTK_INFO_BAR (info_bar),
                                GTK_MESSAGE_ERROR);
 gtk_widget_show (info_bar);
  @end{pre}
  @heading{GtkInfoBar as GtkBuildable}
    The GtkInfoBar implementation of the GtkBuildable interface exposes the
    content area and action area as internal children with the names
    \"content_area\" and \"action_area\".

    GtkInfoBar supports a custom <action-widgets> element, which can contain
    multiple <action-widget> elements. The \"response\" attribute specifies a
    numeric response, and the content of the element is the id of widget (which
    should be a child of the dialogs action_area).
  @see-slot{gtk-info-bar-message-type}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "message-type" 'gtk-info-bar) 't)
 "The @code{\"message-type\"} property of type @symbol{gtk-message-type}
  (Read / Write / Construct)@br{}
  The type of the message is used to determine the colors to use in the info
  bar. The following symbolic color names can by used to customize these colors:
  \"info_fg_color\", \"info_bg_color\", \"warning_fg_color\",
  \"warning_bg_color\", \"question_fg_color\", \"question_bg_color\",
  \"error_fg_color\", \"error_bg_color\". \"other_fg_color\",
  \"other_bg_color\".@br{}
  If the type is GTK_MESSAGE_OTHER, no info bar is painted but the colors are
  still set.@br{}
  Default value: GTK_MESSAGE_INFO@br{}
  Since 2.18")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-info-bar-message-type ----------------------------------------------

(setf (gethash 'gtk-info-bar-message-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-info-bar-message-type 'function)
 "@version{2013-2-3}
  @begin{short}
    Accessor of the slot @code{\"message-type\"} of the @class{gtk-info-bar}
    class.
  @end{short}
  @see-function{gtk-info-bar-get-message-type}
  @see-function{gtk-info-bar-set-message-type}")

;;; --- End of file atdoc-gtk.info-bar.lisp ------------------------------------
