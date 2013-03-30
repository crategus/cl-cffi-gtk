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



;;; ---- End of file atdoc-gtk.tooltip.lisp ------------------------------------
