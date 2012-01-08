;;; ----------------------------------------------------------------------------
;;; gtk.button.lisp
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 2.2.2 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; ----------------------------------------------------------------------------
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
;;; GtkButton
;;; 
;;; A widget that creates a signal when clicked on
;;; 	
;;; Synopsis
;;; 
;;;     GtkButton
;;;     gtk_button_new
;;;     gtk_button_new_with_label
;;;     gtk_button_new_with_mnemonic
;;;     gtk_button_new_from_stock
;;;     gtk_button_pressed
;;;     gtk_button_released
;;;     gtk_button_clicked
;;;     gtk_button_enter
;;;     gtk_button_leave
;;;     gtk_button_set_relief
;;;     gtk_button_get_relief
;;;     gtk_button_get_label
;;;     gtk_button_set_label
;;;     gtk_button_get_use_stock
;;;     gtk_button_set_use_stock
;;;     gtk_button_get_use_underline
;;;     gtk_button_set_use_underline
;;;     gtk_button_set_focus_on_click
;;;     gtk_button_get_focus_on_click
;;;     gtk_button_set_alignment
;;;     gtk_button_get_alignment
;;;     gtk_button_set_image
;;;     gtk_button_get_image
;;;     gtk_button_set_image_position
;;;     gtk_button_get_image_position
;;;     gtk_button_get_event_window
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkButton
;;;                                  +----GtkToggleButton
;;;                                  +----GtkColorButton
;;;                                  +----GtkFontButton
;;;                                  +----GtkLinkButton
;;;                                  +----GtkLockButton
;;;                                  +----GtkScaleButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkButton implements AtkImplementorIface, GtkBuildable and GtkActivatable.
;;; Properties
;;; 
;;;   "focus-on-click"           gboolean             : Read / Write
;;;   "image"                    GtkWidget*           : Read / Write
;;;   "image-position"           GtkPositionType      : Read / Write
;;;   "label"                    gchar*               : Read / Write / Construct
;;;   "relief"                   GtkReliefStyle       : Read / Write
;;;   "use-stock"                gboolean             : Read / Write / Construct
;;;   "use-underline"            gboolean             : Read / Write / Construct
;;;   "xalign"                   gfloat               : Read / Write
;;;   "yalign"                   gfloat               : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "child-displacement-x"     gint                 : Read
;;;   "child-displacement-y"     gint                 : Read
;;;   "default-border"           GtkBorder*           : Read
;;;   "default-outside-border"   GtkBorder*           : Read
;;;   "displace-focus"           gboolean             : Read
;;;   "image-spacing"            gint                 : Read
;;;   "inner-border"             GtkBorder*           : Read
;;; 
;;; Signals
;;; 
;;;   "activate"                                      : Action
;;;   "clicked"                                       : Action
;;;   "enter"                                         : Run First
;;;   "leave"                                         : Run First
;;;   "pressed"                                       : Run First
;;;   "released"                                      : Run First
;;; 
;;; Description
;;; 
;;; The GtkButton widget is generally used to attach a function to that is
;;; called when the button is pressed. The various signals and how to use them
;;; are outlined below.
;;; 
;;; The GtkButton widget can hold any valid child widget. That is it can hold
;;; most any other standard GtkWidget. The most commonly used child is the
;;; GtkLabel.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkButton
;;; 
;;; struct GtkButton;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkButton" 'gtk-button))

(define-g-object-class "GtkButton" gtk-button
                       (:superclass bin
                        :export t
                        :interfaces ("AtkImplementorIface"
                                     "GtkActivatable"
                                     "GtkBuildable")
                        :type-initializer "gtk_button_get_type")
                       ((focus-on-click gtk-button-focus-on-click
                                        "focus-on-click" "gboolean" t t)
                        (image gtk-button-image "image" "GtkWidget" t t)
                        (image-position gtk-button-image-position
                                        "image-position" "GtkPositionType" t t)
                        (label gtk-button-label "label" "gchararray" t t)
                        (relief gtk-button-relief "relief" "GtkReliefStyle" t t)
                        (use-stock gtk-button-use-stock
                                   "use-stock" "gboolean" t t)
                        (use-underline gtk-button-use-underline
                                       "use-underline" "gboolean" t t)
                        (xalign gtk-button-xalign "xalign" "gfloat" t t)
                        (yalign gtk-button-yalign "yalign" "gfloat" t t)))

;;; ---------------------------------------------------------------------------- 
;;; gtk_button_new ()
;;; 
;;; GtkWidget * gtk_button_new (void)
;;; 
;;; Creates a new GtkButton widget. To add a child widget to the button, use
;;; gtk_container_add().
;;; 
;;; Returns :
;;; 	The newly created GtkButton widget.
;;; ----------------------------------------------------------------------------

(defun gtk-button-new ()
  (make-instance 'gtk-button))

(export 'gtk-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_with_label ()
;;; 
;;; GtkWidget * gtk_button_new_with_label (const gchar *label)
;;; 
;;; Creates a GtkButton widget with a GtkLabel child containing the given text.
;;; 
;;; label :
;;; 	The text you want the GtkLabel to hold.
;;; 
;;; Returns :
;;; 	The newly created GtkButton widget.
;;; ----------------------------------------------------------------------------

(defun gtk-button-new-with-label (label)
  (make-instance 'gtk-button :label label))

(export 'gtk-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_with_mnemonic ()
;;; 
;;; GtkWidget * gtk_button_new_with_mnemonic (const gchar *label)
;;; 
;;; Creates a new GtkButton containing a label. If characters in label are
;;; preceded by an underscore, they are underlined. If you need a literal
;;; underscore character in a label, use '__' (two underscores). The first
;;; underlined character represents a keyboard accelerator called a mnemonic.
;;; Pressing Alt and that key activates the button.
;;; 
;;; label :
;;; 	The text of the button, with an underscore in front of the mnemonic
;;;     character
;;; 
;;; Returns :
;;; 	a new GtkButton
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_from_stock ()
;;; 
;;; GtkWidget * gtk_button_new_from_stock (const gchar *stock_id)
;;; 
;;; Creates a new GtkButton containing the image and text from a stock item.
;;; Some stock ids have preprocessor macros like GTK_STOCK_OK and
;;; GTK_STOCK_APPLY.
;;; 
;;; If stock_id is unknown, then it will be treated as a mnemonic label (as for
;;; gtk_button_new_with_mnemonic()).
;;; 
;;; stock_id :
;;; 	the name of the stock item
;;; 
;;; Returns :
;;; 	a new GtkButton
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_pressed ()
;;; 
;;; void gtk_button_pressed (GtkButton *button)
;;; 
;;; Warning
;;; 
;;; gtk_button_pressed has been deprecated since version 2.20 and should not
;;; be used in newly-written code. Use the "button-press-event" signal.
;;; 
;;; Emits a "pressed" signal to the given GtkButton.
;;; 
;;; button :
;;; 	The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_released ()
;;; 
;;; void gtk_button_released (GtkButton *button)
;;; 
;;; Warning
;;; 
;;; gtk_button_released has been deprecated since version 2.20 and should not
;;; be used in newly-written code. Use the "button-release-event" signal.
;;; 
;;; Emits a "released" signal to the given GtkButton.
;;; 
;;; button :
;;; 	The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_clicked ()
;;; 
;;; void gtk_button_clicked (GtkButton *button)
;;; 
;;; Emits a "clicked" signal to the given GtkButton.
;;; 
;;; button :
;;; 	The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_enter ()
;;; 
;;; void gtk_button_enter (GtkButton *button)
;;; 
;;; Warning
;;; 
;;; gtk_button_enter has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Use the "enter-notify-event" signal.
;;; 
;;; Emits a "enter" signal to the given GtkButton.
;;; 
;;; button :
;;; 	The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_leave ()
;;; 
;;; void gtk_button_leave (GtkButton *button)
;;; 
;;; Warning
;;; 
;;; gtk_button_leave has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Use the "leave-notify-event" signal.
;;; 
;;; Emits a "leave" signal to the given GtkButton.
;;; 
;;; button :
;;; 	The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_relief ()
;;; 
;;; void gtk_button_set_relief (GtkButton *button, GtkReliefStyle newstyle)
;;; 
;;; Sets the relief style of the edges of the given GtkButton widget. Three
;;; styles exist, GTK_RELIEF_NORMAL, GTK_RELIEF_HALF, GTK_RELIEF_NONE. The
;;; default style is, as one can guess, GTK_RELIEF_NORMAL.
;;; 
;;; button :
;;; 	The GtkButton you want to set relief styles of.
;;; 
;;; newstyle :
;;; 	The GtkReliefStyle as described above.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_relief ()
;;; 
;;; GtkReliefStyle gtk_button_get_relief (GtkButton *button)
;;; 
;;; Returns the current relief style of the given GtkButton.
;;; 
;;; button :
;;; 	The GtkButton you want the GtkReliefStyle from.
;;; 
;;; Returns :
;;; 	The current GtkReliefStyle
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_label ()
;;; 
;;; const gchar * gtk_button_get_label (GtkButton *button)
;;; 
;;; Fetches the text from the label of the button, as set by
;;; gtk_button_set_label(). If the label text has not been set the return value
;;; will be NULL. This will be the case if you create an empty button with
;;; gtk_button_new() to use as a container.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; Returns :
;;; 	The text of the label widget. This string is owned by the widget and
;;;     must not be modified or freed.
;;; ----------------------------------------------------------------------------

(defun gtk-button-get-label (button)
  (gtk-button-label button))

(export 'gtk-button-get-label)
  
;;; ----------------------------------------------------------------------------
;;; gtk_button_set_label ()
;;; 
;;; void gtk_button_set_label (GtkButton *button, const gchar *label)
;;; 
;;; Sets the text of the label of the button to str. This text is also used to
;;; select the stock item if gtk_button_set_use_stock() is used.
;;; 
;;; This will also clear any previously set labels.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; label :
;;; 	a string
;;; ----------------------------------------------------------------------------

(defun gtk-button-set-label (button label)
  (setf (gtk-button-label button) label))

(export 'gtk-button-set-label)
  
;;; ----------------------------------------------------------------------------
;;; gtk_button_get_use_stock ()
;;; 
;;; gboolean gtk_button_get_use_stock (GtkButton *button)
;;; 
;;; Returns whether the button label is a stock item.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; Returns :
;;; 	TRUE if the button label is used to select a stock item instead of
;;;     being used directly as the label text.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_use_stock ()
;;; 
;;; void gtk_button_set_use_stock (GtkButton *button, gboolean use_stock)
;;; 
;;; If TRUE, the label set on the button is used as a stock id to select the
;;; stock item for the button.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; use_stock :
;;; 	TRUE if the button should use a stock item
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_use_underline ()
;;; 
;;; gboolean gtk_button_get_use_underline (GtkButton *button)
;;; 
;;; Returns whether an embedded underline in the button label indicates a
;;; mnemonic. See gtk_button_set_use_underline().
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; Returns :
;;; 	TRUE if an embedded underline in the button label indicates the
;;;     mnemonic accelerator keys.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_use_underline ()
;;; 
;;; void gtk_button_set_use_underline (GtkButton *button,
;;;                                    gboolean use_underline);
;;; 
;;; If true, an underline in the text of the button label indicates the next
;;; character should be used for the mnemonic accelerator key.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; use_underline :
;;; 	TRUE if underlines in the text indicate mnemonics
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_focus_on_click ()
;;; 
;;; void gtk_button_set_focus_on_click (GtkButton *button,
;;;                                     gboolean focus_on_click);
;;; 
;;; Sets whether the button will grab focus when it is clicked with the mouse.
;;; Making mouse clicks not grab focus is useful in places like toolbars where
;;; you don't want the keyboard focus removed from the main area of the
;;; application.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; focus_on_click :
;;; 	whether the button grabs focus when clicked with the mouse
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_focus_on_click ()
;;; 
;;; gboolean gtk_button_get_focus_on_click (GtkButton *button);
;;; 
;;; Returns whether the button grabs focus when it is clicked with the mouse.
;;; See gtk_button_set_focus_on_click().
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; Returns :
;;; 	TRUE if the button grabs focus when it is clicked with the mouse.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_alignment ()
;;; 
;;; void gtk_button_set_alignment (GtkButton *button,
;;;                                gfloat xalign,
;;;                                gfloat yalign);
;;; 
;;; Sets the alignment of the child. This property has no effect unless the
;;; child is a GtkMisc or a GtkAlignment.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; xalign :
;;; 	the horizontal position of the child, 0.0 is left aligned, 1.0 is
;;;     right aligned
;;; 
;;; yalign :
;;; 	the vertical position of the child, 0.0 is top aligned, 1.0 is
;;;     bottom aligned
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_alignment ()
;;; 
;;; void gtk_button_get_alignment (GtkButton *button,
;;;                                gfloat *xalign,
;;;                                gfloat *yalign);
;;; 
;;; Gets the alignment of the child in the button.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; xalign :
;;; 	return location for horizontal alignment.
;;; 
;;; yalign :
;;; 	return location for vertical alignment.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_image ()
;;; 
;;; void gtk_button_set_image (GtkButton *button, GtkWidget *image);
;;; 
;;; Set the image of button to the given widget. Note that it depends on the
;;; "gtk-button-images" setting whether the image will be displayed or not, you
;;; don't have to call gtk_widget_show() on image yourself.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; image :
;;; 	a widget to set as the image for the button
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_image ()
;;; 
;;; GtkWidget * gtk_button_get_image (GtkButton *button);
;;; 
;;; Gets the widget that is currenty set as the image of button. This may have
;;; been explicitly set by gtk_button_set_image() or constructed by
;;; gtk_button_new_from_stock().
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; Returns :
;;; 	a GtkWidget or NULL in case there is no image.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_image_position ()
;;; 
;;; void gtk_button_set_image_position (GtkButton *button,
;;;                                     GtkPositionType position);
;;; 
;;; Sets the position of the image relative to the text inside the button.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; position :
;;; 	the position
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_image_position ()
;;; 
;;; GtkPositionType gtk_button_get_image_position (GtkButton *button);
;;; 
;;; Gets the position of the image relative to the text inside the button.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; Returns :
;;; 	the position
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_event_window ()
;;; 
;;; GdkWindow * gtk_button_get_event_window (GtkButton *button);
;;; 
;;; Returns the button's event window if it is realized, NULL otherwise. This
;;; function should be rarely needed.
;;; 
;;; button :
;;; 	a GtkButton
;;; 
;;; Returns :
;;; 	button's event window.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-on-click" property
;;; 
;;;   "focus-on-click" gboolean              : Read / Write
;;; 
;;; Whether the button grabs focus when it is clicked with the mouse.
;;; 
;;; Default value: TRUE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "image" property
;;; 
;;;   "image" GtkWidget*            : Read / Write
;;; 
;;; The child widget to appear next to the button text.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "image-position" property
;;; 
;;;   "image-position" GtkPositionType       : Read / Write
;;; 
;;; The position of the image relative to the text inside the button.
;;; 
;;; Default value: GTK_POS_LEFT
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "label" property
;;; 
;;;   "label" gchar*               : Read / Write / Construct
;;; 
;;; Text of the label widget inside the button, if the button contains a label
;;; widget.
;;; 
;;; Default value: NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "relief" property
;;; 
;;;   "relief" GtkReliefStyle        : Read / Write
;;; 
;;; The border relief style.
;;; 
;;; Default value: GTK_RELIEF_NORMAL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "use-stock" property
;;; 
;;;   "use-stock" gboolean             : Read / Write / Construct
;;; 
;;; If set, the label is used to pick a stock item instead of being displayed.
;;; 
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "use-underline" property
;;; 
;;;   "use-underline" gboolean             : Read / Write / Construct
;;; 
;;; If set, an underline in the text indicates the next character should be
;;; used for the mnemonic accelerator key.
;;; 
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "xalign" property
;;; 
;;;   "xalign" gfloat                : Read / Write
;;; 
;;; If the child of the button is a GtkMisc or GtkAlignment, this property can
;;; be used to control its horizontal alignment. 0.0 is left aligned, 1.0 is
;;; right aligned.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "yalign" property
;;; 
;;;   "yalign" gfloat                : Read / Write
;;; 
;;; If the child of the button is a GtkMisc or GtkAlignment, this property can
;;; be used to control its vertical alignment. 0.0 is top aligned, 1.0 is
;;; bottom aligned.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "child-displacement-x" style property
;;; 
;;;   "child-displacement-x"     gint                  : Read
;;; 
;;; How far in the x direction to move the child when the button is depressed.
;;; 
;;; Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "child-displacement-y" style property
;;; 
;;;   "child-displacement-y"     gint                  : Read
;;; 
;;; How far in the y direction to move the child when the button is depressed.
;;; 
;;; Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "default-border" style property
;;; 
;;;   "default-border"           GtkBorder*            : Read
;;; 
;;; The "default-border" style property defines the extra space to add around a
;;; button that can become the default widget of its window. For more
;;; information about default widgets, see gtk_widget_grab_default().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "default-outside-border" style property
;;; 
;;;   "default-outside-border"   GtkBorder*            : Read
;;; 
;;; The "default-outside-border" style property defines the extra outside space
;;; to add around a button that can become the default widget of its window.
;;; Extra outside space is always drawn outside the button border. For more
;;; information about default widgets, see gtk_widget_grab_default().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "displace-focus" style property
;;; 
;;;   "displace-focus"           gboolean              : Read
;;; 
;;; Whether the child_displacement_x/child_displacement_y properties should
;;; also affect the focus rectangle.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "image-spacing" style property
;;; 
;;;   "image-spacing"            gint                  : Read
;;; 
;;; Spacing in pixels between the image and label.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "inner-border" style property
;;; 
;;;   "inner-border"             GtkBorder*            : Read
;;; 
;;; Sets the border between the button edges and child.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate" signal
;;; 
;;; void user_function (GtkButton *widget, gpointer user_data)      : Action
;;; 
;;; The ::activate signal on GtkButton is an action signal and emitting it
;;; causes the button to animate press then release. Applications should never
;;; connect to this signal, but use the "clicked" signal.
;;; 
;;; widget :
;;; 	the object which received the signal.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "clicked" signal
;;; 
;;; void user_function (GtkButton *button, gpointer user_data)      : Action
;;; 
;;; Emitted when the button has been activated (pressed and released).
;;; 
;;; button :
;;; 	the object that received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "enter" signal
;;; 
;;; void user_function (GtkButton *button, gpointer user_data)     : Run First
;;; 
;;; Warning
;;; 
;;; GtkButton::enter has been deprecated since version 2.8 and should not be
;;; used in newly-written code. Use the "enter-notify-event" signal.
;;; 
;;; Emitted when the pointer enters the button.
;;; 
;;; button :
;;; 	the object that received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "leave" signal
;;; 
;;; void user_function (GtkButton *button, gpointer user_data)    : Run First
;;; 
;;; Warning
;;; 
;;; GtkButton::leave has been deprecated since version 2.8 and should not be
;;; used in newly-written code. Use the "leave-notify-event" signal.
;;; 
;;; Emitted when the pointer leaves the button.
;;; 
;;; button :
;;; 	the object that received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "pressed" signal
;;; 
;;; void user_function (GtkButton *button, gpointer user_data)      : Run First
;;; 
;;; Warning
;;; 
;;; GtkButton::pressed has been deprecated since version 2.8 and should not be
;;; used in newly-written code. Use the "button-press-event" signal.
;;; 
;;; Emitted when the button is pressed.
;;; 
;;; button :
;;; 	the object that received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "released" signal
;;; 
;;; void user_function (GtkButton *button, gpointer user_data)      : Run First
;;; 
;;; Warning
;;; 
;;; GtkButton::released has been deprecated since version 2.8 and should not be
;;; used in newly-written code. Use the "button-release-event" signal.
;;; 
;;; Emitted when the button is released.
;;; 
;;; button :
;;; 	the object that received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.button.lisp --------------------------------------------
