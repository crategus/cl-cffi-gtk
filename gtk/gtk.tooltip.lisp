;;; ----------------------------------------------------------------------------
;;; gtk.tooltip.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkTooltip
;;; 
;;; Add tips to your widgets
;;; 
;;; Synopsis
;;; 
;;;     GtkTooltip
;;;     
;;;     gtk_tooltip_set_markup
;;;     gtk_tooltip_set_text
;;;     gtk_tooltip_set_icon
;;;     gtk_tooltip_set_icon_from_stock
;;;     gtk_tooltip_set_icon_from_icon_name
;;;     gtk_tooltip_set_icon_from_gicon
;;;     gtk_tooltip_set_custom
;;;     gtk_tooltip_trigger_tooltip_query
;;;     gtk_tooltip_set_tip_area
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkTooltip
;;; 
;;; Description
;;; 
;;; Basic tooltips can be realized simply by using gtk_widget_set_tooltip_text()
;;; or gtk_widget_set_tooltip_markup() without any explicit tooltip object.
;;; 
;;; When you need a tooltip with a little more fancy contents, like adding an
;;; image, or you want the tooltip to have different contents per GtkTreeView
;;; row or cell, you will have to do a little more work:
;;; 
;;;     Set the "has-tooltip" property to TRUE, this will make GTK+ monitor the
;;;     widget for motion and related events which are needed to determine when
;;;     and where to show a tooltip.
;;; 
;;;     Connect to the "query-tooltip" signal. This signal will be emitted when
;;;     a tooltip is supposed to be shown. One of the arguments passed to the
;;;     signal handler is a GtkTooltip object. This is the object that we are
;;;     about to display as a tooltip, and can be manipulated in your callback
;;;     using functions like gtk_tooltip_set_icon(). There are functions for
;;;     setting the tooltip's markup, setting an image from a stock icon, or
;;;     even putting in a custom widget.
;;; 
;;;     Return TRUE from your query-tooltip handler. This causes the tooltip to
;;;     be show. If you return FALSE, it will not be shown.
;;; 
;;; In the probably rare case where you want to have even more control over the
;;; tooltip that is about to be shown, you can set your own GtkWindow which will
;;; be used as tooltip window. This works as follows:
;;; 
;;;     Set "has-tooltip" and connect to "query-tooltip" as before.
;;; 
;;;     Use gtk_widget_set_tooltip_window() to set a GtkWindow created by you
;;;     as tooltip window.
;;; 
;;;     In the "query-tooltip" callback you can access your window using
;;;     gtk_widget_get_tooltip_window() and manipulate as you wish. The
;;;     semantics of the return value are exactly as before, return TRUE to
;;;     show the window, FALSE to not show it.
;;; 
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTooltip
;;; 
;;; typedef struct _GtkTooltip GtkTooltip;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTooltip" gtk-tooltip
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_tooltip_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_markup ()
;;; 
;;; void gtk_tooltip_set_markup (GtkTooltip *tooltip, const gchar *markup);
;;; 
;;; Sets the text of the tooltip to be markup, which is marked up with the Pango
;;; text markup language. If markup is NULL, the label will be hidden.
;;; 
;;; tooltip :
;;;     a GtkTooltip
;;; 
;;; markup :
;;;     a markup string (see Pango markup format) or NULL
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_markup" gtk-tooltip-set-markup) :void
  (tooltip g-object)
  (markup :string))

(defun (setf gtk-tooltip-markup) (new-value tooltip)
  (gtk-tooltip-set-markup tooltip new-value))

(export 'gtk-tooltip-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_text ()
;;; 
;;; void gtk_tooltip_set_text (GtkTooltip *tooltip, const gchar *text);
;;; 
;;; Sets the text of the tooltip to be text. If text is NULL, the label will
;;; be hidden. See also gtk_tooltip_set_markup().
;;; 
;;; tooltip :
;;;     a GtkTooltip
;;; 
;;; text :
;;;     a text string or NULL
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_text" gtk-tooltip-set-text) :void
  (tooltip g-object)
  (text :string))

(defun (setf gtk-tooltip-text) (new-value tooltip)
  (gtk-tooltip-set-text tooltip new-value))

(export 'gtk-tooltip-text)

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

(defun (setf gtk-tooltip-icon) (new-value tooltip)
  (gtk-tooltip-set-icon tooltip new-value))

(export 'gtk-tooltip-icon)

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

;;; --- End of file gtk.tooltip.lisp -------------------------------------------
