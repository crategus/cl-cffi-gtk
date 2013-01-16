;;; ----------------------------------------------------------------------------
;;; gtk.tooltip.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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

(in-package :gtk)

;;; --- gtk-tooltip ------------------------------------------------------------

(setf (documentation 'gtk-tooltip 'type)
 "@version{2013-1-13}
  @short{Add tips to your widgets.}

  Basic tooltips can be realized simply by using
  @fun{gtk-widget-set-tooltip-text} or @fun{gtk-widget-set-tooltip-markup}
  without any explicit tooltip object.

  When you need a tooltip with a little more fancy contents, like adding an
  image, or you want the tooltip to have different contents per
  @class{gtk-tree-view} row or cell, you will have to do a little more work:

  @begin{indent}
    Set the @arg{\"has-tooltip\"} property to @arg{true}, this will make GTK+
    monitor the widget for motion and related events which are needed to
    determine when and where to show a tooltip.

    Connect to the @code{\"query-tooltip\"} signal. This signal will be emitted
    when a tooltip is supposed to be shown. One of the arguments passed to the
    signal handler is a @sym{gtk-tooltip} object. This is the object that we are
    about to display as a tooltip, and can be manipulated in your callback
    using functions like @fun{gtk-tooltip-set-icon}. There are functions for
    setting the tooltip's markup, setting an image from a stock icon, or
    even putting in a custom widget.

    Return @arg{true} from your query-tooltip handler. This causes the tooltip
    to be show. If you return @code{nil}, it will not be shown.
  @end{indent}

  In the probably rare case where you want to have even more control over the
  tooltip that is about to be shown, you can set your own @class{gtk-window}
  which will be used as tooltip window. This works as follows:

  @begin{indent}
    Set @arg{\"has-tooltip\"} and connect to @code{\"query-tooltip\"} as
    before.

    Use @fun{gtk-widget-set-tooltip-window} to set a @class{gtk-window} created
    by you as tooltip window.

    In the @code{\"query-tooltip\"} callback you can access your window using
    @fun{gtk-widget-get-tooltip-window} and manipulate as you wish. The
    semantics of the return value are exactly as before, return @arg{true} to
    show the window, @code{nil} to not show it.
  @end{indent}")

;;; --- gtk-tooltip-set-markup -------------------------------------------------

(setf (documentation 'gtk-tooltip-set-markup 'function)
 "@version{2013-1-13}
  @argument[tooltip]{a @class{gtk-tooltip} instance}
  @argument[markup]{a markup string (see Pango markup format) or @code{nil}}
  @begin{short}
    Sets the text of the @arg{tooltip} to be @arg{markup}, which is marked up
    with the Pango text markup language.
  @end{short}
  If @arg{markup} is @code{nil}, the label will be hidden.

  Since 2.12
  @see-function{gtk-tooltip-set-text}")

;;; --- gtk-tooltip-set-text ---------------------------------------------------

(setf (documentation 'gtk-tooltip-set-text 'function)
 "@version{2013-1-13}
  @argument[tooltip]{a @class{gtk-tooltip} instance}
  @argument[text]{a text string or @code{nil}}
  @begin{short}
    Sets the text of the @arg{tooltip} to be @arg{text}.
  @end{short}
  If @arg{text} is @code{nil}, the label will be hidden. See also
  @fun{gtk-tooltip-set-markup}.

  Since 2.12
  @see-function{gtk-tooltip-set-markup}")

#|
;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon ()
;;;
;;; void gtk_tooltip_set_icon (GtkTooltip *tooltip, GdkPixbuf *pixbuf);
;;;
;;; Sets the icon of the tooltip (which is in front of the text) to be pixbuf.
;;; If pixbuf is NULL, the image will be hidden.
;;;
;;; tooltip :
;;;     a GtkTooltip
;;;
;;; pixbuf :
;;;     a GdkPixbuf, or NULL
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_icon" gtk-tooltip-set-icon) :void
  (tooltip g-object)
  (pixbuf g-object))

(export 'gtk-tooltip-set-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon_from_stock ()
;;;
;;; void gtk_tooltip_set_icon_from_stock (GtkTooltip *tooltip,
;;;                                       const gchar *stock_id,
;;;                                       GtkIconSize size);
;;;
;;; Sets the icon of the tooltip (which is in front of the text) to be the stock
;;; item indicated by stock_id with the size indicated by size. If stock_id is
;;; NULL, the image will be hidden.
;;;
;;; tooltip :
;;;     a GtkTooltip
;;;
;;; stock_id :
;;;     a stock id, or NULL
;;;
;;; size :
;;;     a stock icon size
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_icon_from_stock" gtk-tooltip-set-icon-from-stock)
    :void
  (tooltip g-object)
  (stock-id :string)
  (icon-size gtk-icon-size))

(export 'gtk-tooltip-set-icon-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon_from_icon_name ()
;;;
;;; void gtk_tooltip_set_icon_from_icon_name (GtkTooltip *tooltip,
;;;                                           const gchar *icon_name,
;;;                                           GtkIconSize size);
;;;
;;; Sets the icon of the tooltip (which is in front of the text) to be the icon
;;; indicated by icon_name with the size indicated by size. If icon_name is
;;; NULL, the image will be hidden.
;;;
;;; tooltip :
;;;     a GtkTooltip
;;;
;;; icon_name :
;;;     an icon name, or NULL
;;;
;;; size :
;;;     a stock icon size
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_icon_from_icon_name"
           gtk-tooltip-set-icon-from-icon-name) :void
  (tooltip g-object)
  (icon-name :string)
  (icon-size gtk-icon-size))

(export 'gtk-tooltip-set-icon-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon_from_gicon ()
;;;
;;; void gtk_tooltip_set_icon_from_gicon (GtkTooltip *tooltip,
;;;                                       GIcon *gicon,
;;;                                       GtkIconSize size);
;;;
;;; Sets the icon of the tooltip (which is in front of the text) to be the icon
;;; indicated by gicon with the size indicated by size. If gicon is NULL, the
;;; image will be hidden.
;;;
;;; tooltip :
;;;     a GtkTooltip
;;;
;;; gicon :
;;;     a GIcon representing the icon, or NULL
;;;
;;; size :
;;;     a stock icon size
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_custom ()
;;;
;;; void gtk_tooltip_set_custom (GtkTooltip *tooltip, GtkWidget *custom_widget);
;;;
;;; Replaces the widget packed into the tooltip with custom_widget.
;;; custom_widget does not get destroyed when the tooltip goes away. By default
;;; a box with a GtkImage and GtkLabel is embedded in the tooltip, which can be
;;; configured using gtk_tooltip_set_markup() and gtk_tooltip_set_icon().
;;;
;;; tooltip :
;;;     a GtkTooltip
;;;
;;; custom_widget :
;;;     a GtkWidget, or NULL to unset the old custom widget
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_custom" gtk-tooltip-set-custom) :void
  (tooltip g-object)
  (custom-widget g-object))

(export 'gtk-tooltip-set-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_trigger_tooltip_query ()
;;;
;;; void gtk_tooltip_trigger_tooltip_query (GdkDisplay *display);
;;;
;;; Triggers a new tooltip query on display, in order to update the current
;;; visible tooltip, or to show/hide the current tooltip. This function is
;;; useful to call when, for example, the state of the widget changed by a key
;;; press.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_trigger_tooltip_query" gtk-tooltip-trigger-tooltip-query)
    :void
  (display g-object))

(export 'gtk-tooltip-trigger-tooltip-query)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_tip_area ()
;;;
;;; void gtk_tooltip_set_tip_area (GtkTooltip *tooltip,
;;;                                const GdkRectangle *rect);
;;;
;;; Sets the area of the widget, where the contents of this tooltip apply, to be
;;; rect (in widget coordinates). This is especially useful for properly setting
;;; tooltips on GtkTreeView rows and cells, GtkIconViews, etc.
;;;
;;; For setting tooltips on GtkTreeView, please refer to the convenience
;;; functions for this: gtk_tree_view_set_tooltip_row() and
;;; gtk_tree_view_set_tooltip_cell().
;;;
;;; tooltip :
;;;     a GtkTooltip
;;;
;;; rect :
;;;     a GdkRectangle
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_tip_area" gtk-tooltip-set-tip-area) :void
  (tooltip g-object)
  (rectangle (g-boxed-foreign gdk-rectangle)))

(export 'gtk-tooltip-set-tip-area)
|#

;;; ---- End of file atdoc-gtk.tooltip.lisp ------------------------------------
