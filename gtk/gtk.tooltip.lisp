;;; ----------------------------------------------------------------------------
;;; gtk.tooltip.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     Add tips to your widgets
;;;
;;; Types and Values
;;;
;;;     GtkTooltip
;;;
;;; Functions
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
;;;     GObject
;;;     ╰── GtkTooltip
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTooltip
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTooltip" gtk-tooltip
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_tooltip_get_type")
  nil)

(setf (documentation 'gtk-tooltip 'type)
 "@version{2020-6-3}
  @short{Add tips to your widgets.}

  Basic tooltips can be realized simply by using the functions
  @fun{gtk-widget-tooltip-text} or @fun{gtk-widget-tooltip-markup}
  without any explicit tooltip object.

  When you need a tooltip with a little more fancy contents, like adding an
  image, or you want the tooltip to have different contents per
  @class{gtk-tree-view} row or cell, you will have to do a little more work:
  @begin{itemize}
    @begin{item}
      Set the @slot[gtk-widget]{has-tooltip} property to @em{true}, this will
      make GTK+ monitor the widget for motion and related events which are
      needed to determine when and where to show a tooltip.
    @end{item}
    @begin{item}
      Connect to the \"query-tooltip\" signal. This signal will be emitted when
      a tooltip is supposed to be shown. One of the arguments passed to the
      signal handler is a @sym{gtk-tooltip} object. This is the object that we
      are about to display as a tooltip, and can be manipulated in your callback
      using functions like the function @fun{gtk-tooltip-set-icon}. There are
      functions for setting the tooltip's markup, setting an image from a stock
      icon, or even putting in a custom widget.
    @end{item}
    @begin{item}
      Return @em{true} from your query-tooltip handler. This causes the tooltip
      to be show. If you return @em{false}, it will not be shown.
    @end{item}
  @end{itemize}
  In the probably rare case where you want to have even more control over the
  tooltip that is about to be shown, you can set your own @class{gtk-window}
  which will be used as tooltip window. This works as follows:
  @begin{itemize}
    @begin{item}
      Set the @slot[gtk-widget]{has-tooltip} property and connect to the
      \"query-tooltip\" signal as before.
    @end{item}
    @begin{item}
      Use the function @fun{gtk-widget-tooltip-window} to set a
      @class{gtk-window} created by you as tooltip window.
    @end{item}
    @begin{item}
      In the \"query-tooltip\" callback you can access your window using the
       function @fun{gtk-widget-tooltip-window} and manipulate as you wish.
      The semantics of the return value are exactly as before, return @em{true}
      to show the window, @em{false} to not show it.
    @end{item}
  @end{itemize}
  @see-class{gtk-tree-view}
  @see-class{gtk-window}
  @see-function{gtk-widget-tooltip-text}
  @see-function{gtk-widget-tooltip-markup}
  @see-function{gtk-widget-tooltip-window}
  @see-function{gtk-tooltip-set-icon}")

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_markup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_markup" gtk-tooltip-set-markup) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-3}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[markup]{a markup string, see Pango markup format, or @code{nil}}
  @begin{short}
    Sets the text of the tooltip to be @arg{markup}, which is marked up
    with the Pango text markup language.
  @end{short}
  If @arg{markup} is @code{nil}, the label will be hidden.
  @see-class{gtk-tooltip}
  @see-function{gtk-tooltip-set-text}"
  (tooltip (g-object gtk-tooltip))
  (markup :string))

(export 'gtk-tooltip-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_text" gtk-tooltip-set-text) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-3}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[text]{a text string or @code{nil}}
  @begin{short}
    Sets the text of the tooltip to be @arg{text}.
  @end{short}
  If @arg{text} is @code{nil}, the label will be hidden. See also the function
  @fun{gtk-tooltip-set-markup}.
  @see-class{gtk-tooltip}
  @see-function{gtk-tooltip-set-markup}"
  (tooltip (g-object gtk-tooltip))
  (text :string))

(export 'gtk-tooltip-set-text)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_icon" gtk-tooltip-set-icon) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-21}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[pixbuf]{a @class{gdk-pixbuf} structure, or @code{nil}}
  @begin{short}
    Sets the icon of the tooltip, which is in front of the text, to be
    @arg{pixbuf}.
  @end{short}
  If @arg{pixbuf} is @code{nil}, the image will be hidden.
  @see-class{gtk-tooltip}"
  (tooltip (g-object gtk-tooltip))
  (pixbuf (g-object gkd-pixbuf)))

(export 'gtk-tooltip-set-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_icon_from_stock" gtk-tooltip-set-icon-from-stock)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-3}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[stock-id]{a string with the stock ID, or @code{nil}}
  @argument[icon-size]{a icon size of type @symbol{gtk-icon-size}}
  @begin{short}
    Sets the icon of the tooltip, which is in front of the text, to be the
    stock item indicated by @arg{stock-id} with the size indicated by
    @arg{icon-size}.
  @end{short}
  If @arg{stock-id} is @code{nil}, the image will be hidden.
  @begin[Warning]{dictionary}
    The function @sym{gtk-tooltip-set-icon-from-stock} has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-tooltip-set-icon-from-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-tooltip}"
  (tooltip (g-object gtk-tooltip))
  (stock-id :string)
  (icon-size gtk-icon-size))

(export 'gtk-tooltip-set-icon-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon_from_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_icon_from_icon_name"
           gtk-tooltip-set-icon-from-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-3}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[icon-name]{a string with the icon name, or @code{nil}}
  @argument[icon-size]{a icon size of type @symbol{gtk-icon-size}}
  @begin{short}
    Sets the icon of the tooltip, which is in front of the text, to be the icon
    indicated by @arg{icon-name} with the size indicated by @arg{icon-size}.
  @end{short}
  If @arg{icon-name} is @code{nil}, the image will be hidden.
  @see-class{gtk-tooltip}"
  (tooltip (g-object gtk-tooltip))
  (icon-name :string)
  (icon-size gtk-icon-size))

(export 'gtk-tooltip-set-icon-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon_from_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_icon_from_gicon" gtk-tooltip-set-icon-from-gicon)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-3}
  @argument[tooltip]{a @class{gtk-tooltip} widget}
  @argument[gicon]{a @class{g-icon} representing the icon, or @code{nil}}
  @argument[icon-size]{a icon size of type @symbol{gtk-icon-size}}
  @begin{short}
    Sets the icon of the tooltip, which is in front of the text, to be the
    icon indicated by @arg{gicon} with the size indicated by @arg{icon-size}.
  @end{short}
  If @arg{gicon} is @code{nil}, the image will be hidden.
  @see-class{gtk-tooltip}
  @see-class{g-icon}
  @see-symbol{gtk-icon-size}"
  (tooltip (g-object gtk-tooltip))
  (gicon (g-object g-icon))
  (size gtk-icon-size))

(export 'gtk-tooltip-set-icon-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_custom ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_custom" gtk-tooltip-set-custom) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-3}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[custom-widget]{a @class{gtk-widget}, or @code{nil} to unset the old
    custom widget}
  @begin{short}
    Replaces the widget packed into the tooltip with @arg{custom-widget}.
  @end{short}
  @arg{custom-widget} does not get destroyed when the @arg{tooltip} goes away.
  By default a box with a @class{gtk-image} and @class{gtk-label} is embedded
  in the tooltip, which can be configured using the functions
  @fun{gtk-tooltip-set-markup} and @fun{gtk-tooltip-set-icon}.
  @see-class{gtk-tooltip}
  @see-function{gtk-tooltip-set-markup}
  @see-function{gtk-tooltip-set-icon}"
  (tooltip (g-object gtk-tooltip))
  (custom-widget (g-object gtk-widget)))

(export 'gtk-tooltip-set-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_trigger_tooltip_query ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_trigger_tooltip_query" gtk-tooltip-trigger-tooltip-query)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-3}
  @argument[display]{a @class{gdk-display} object}
  @begin{short}
    Triggers a new tooltip query on @arg{display}, in order to update the
    current visible tooltip, or to show/hide the current tooltip.
  @end{short}
  This function is useful to call when, for example, the state of the widget
  changed by a key press.
  @see-class{gtk-tooltip}"
  (display (g-object gdk-display)))

(export 'gtk-tooltip-trigger-tooltip-query)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_tip_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_set_tip_area" gtk-tooltip-set-tip-area) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-3}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[rectangle]{a @class{gdk-rectangle} object}
  @begin{short}
    Sets the area of the widget, where the contents of the tooltip apply, to be
    @arg{rectangle} in widget coordinates.
  @end{short}
  This is especially useful for properly setting tooltips on
  @class{gtk-tree-view} rows and cells, @class{gtk-icon-view} widgets, etc.

  For setting tooltips on @class{gtk-tree-view}, please refer to the
  convenience functions for this: @fun{gtk-tree-view-set-tooltip-row} and
  @fun{gtk-tree-view-set-tooltip-cell}.
  @see-class{gtk-tooltip}
  @see-function{gtk-tree-view-set-tooltip-row}
  @see-function{gtk-tree-view-set-tooltip-cell}"
  (tooltip (g-object gtk-tooltip))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(export 'gtk-tooltip-set-tip-area)

;;; ---- End of file gtk.tooltip.lisp ------------------------------------------
