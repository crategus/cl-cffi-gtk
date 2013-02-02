;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.separator.lisp
;;;
;;; Documentation strings for the library GTK+.
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

;;; --- gtk-separator ----------------------------------------------------------

(setf (documentation 'gtk-separator 'type)
 "@version{2013-1-29}
  @begin{short}
    @sym{gtk-separator} is a horizontal or vertical separator widget, depending
    on the value of the @code{\"orientation\"} property of the interface
    @class{gtk-orientable}, used to group the widgets within a window.
  @end{short}
  It displays a line with a shadow to make it appear sunken into the interface.
")

;;; --- gtk-separator-new ------------------------------------------------------

(setf (documentation 'gtk-separator-new 'function)
 "@version{2013-1-29}
  @argument[orientation]{the separator's orientation}
  @return{A new @class{gtk-separator} widget.}
  @begin{short}
    Creates a new @class{gtk-separator} with the given @arg{orientation}
    @code{:horizontal} or @code{:vertical} of type @symbol{gtk-orientation}.
  @end{short}
  See also the interface @class{gtk-orientable}.

  Since 3.0
  @see-class{gtk-orientable}
  @see-symbol{gtk-orientation}")

;;; --- gtk-hseparator ---------------------------------------------------------

(setf (documentation 'gtk-hseparator 'type)
 "@version{2013-1-29}
  @begin{short}
    The @sym{gtk-hseparator} widget is a horizontal separator, used to group
    the widgets within a window. It displays a horizontal line with a shadow to
    make it appear sunken into the interface.
  @end{short}

  @subheading{Note}
  The @sym{gtk-hseparator} widget is not used as a separator within menus. To
  create a separator in a menu create an empty @class{gtk-separator-menu-item}
  widget using @fun{gtk-separator-menu-item-new} and add it to the menu with
  @fun{gtk-menu-shell-append}.

  @subheading{Warning}
  @sym{gtk-hseparator} has been deprecated, use @class{gtk-separator}
  with an orientation @code{:horizontal} instead.")

;;; --- gtk-hseparator-new -----------------------------------------------------

(setf (documentation 'gtk-hseparator-new 'function)
 "@version{2013-1-29}
  @return{A new @class{gtk-hseparator} widget.}
  @begin{short}
    Creates a new @class{gtk-hseparator} widget.
  @end{short}

  @subheading{Warning}
  @sym{gtk-hseparator-new} has been deprecated since version 3.2 and should not
  be used in newly-written code. Use @fun{gtk-separator-new} with an orientation
  @code{:horizontal} instead.
  @see-function{gtk-separator-new}")

;;; --- gtk-vseparator ---------------------------------------------------------

(setf (documentation 'gtk-vseparator 'type)
 "@version{2013-1-29}
  @begin{short}
    The @sym{gtk-vseparator} widget is a vertical separator, used to group the
    widgets within a window. It displays a vertical line with a shadow to make
    it appear sunken into the interface.
  @end{short}

  @subheading{Warning}
  @sym{gtk-vseparator} has been deprecated, use @class{gtk-separator} with
  orientation @code{:vertical} instead.
  @see-class{gtk-separator}")

;;; --- gtk-vseparator-new -----------------------------------------------------

(setf (documentation 'gtk-vseparator-new 'function)
 "@version{2013-1-29}
  @return{A new @class{gtk-vseparator} widget.}
  @begin{short}
    Creates a new @class{gtk-vseparator} widget.
  @end{short}

  @subheading{Warning}
  @sym{gtk-vseparator-new} has been deprecated since version 3.2 and should not
  be used in newly-written code. Use @fun{gtk-separator-new} with the
  orientation @code{:vertical} instead.
  @see-function{gtk-separator-new}")

;;; --- End of file atdoc-gtk.separator.lisp -----------------------------------
