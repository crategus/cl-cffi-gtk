;;; ----------------------------------------------------------------------------
;;; gtk.scrollbar.lisp
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
;;; GtkScrollbar
;;;
;;;     A Scrollbar
;;;
;;; Types and Values
;;;
;;;     GtkScrollbar
;;;
;;; Functions
;;;
;;;     gtk_scrollbar_new
;;;
;;; Style Properties
;;;
;;;     gboolean    fixed-slider-length               Read
;;;     gboolean    has-backward-stepper              Read
;;;     gboolean    has-forward-stepper               Read
;;;     gboolean    has-secondary-backward-stepper    Read
;;;     gboolean    has-secondary-forward-stepper     Read
;;;         gint    min-slider-length                 Read
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkRange
;;;                 ╰── GtkScrollbar
;;;                     ├── GtkHScrollbar
;;;                     ╰── GtkVScrollbar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkScrollbar implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkScrollbar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkScrollbar" gtk-scrollbar
  (:superclass gtk-range
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_scrollbar_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-scrollbar 'type)
 "@version{2020-7-20}
  @begin{short}
    The @sym{gtk-scrollbar} widget is a horizontal or vertical scrollbar,
    depending on the value of the @slot[gtk-orientable]{orientation} property.
  @end{short}

  @image[scrollbar]{}

  The position of the thumb in a scrollbar is controlled by the scroll
  adjustments. See @class{gtk-adjustment} for the properties in an adjustment -
  for @sym{gtk-scrollbar}, the @code{value} property represents the position
  of the scrollbar, which must be between the @code{lower} property and
  @code{upper} - @code{page-size}. The @code{page-size} property
  represents the size of the visible scrollable area. The
  @code{step-increment} and @code{page-increment} properties are
  used when the user asks to step down, using the small stepper arrows, or
  page down, using for example the PageDown key.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 scrollbar[.fine-tune]
 ╰── contents
     ├── [button.up]
     ├── [button.down]
     ├── trough
     │   ╰── slider
     ├── [button.up]
     ╰── [button.down]
    @end{pre}
    @sym{gtk-scrollbar} has a main CSS node with name @code{scrollbar} and a
    subnode for its contents, with subnodes named @code{trough} and
    @code{slider}.

    The main node gets the style class @code{.fine-tune} added when the
    scrollbar is in \"fine-tuning\" mode.

    If steppers are enabled, they are represented by up to four additional
    subnodes with name @code{button}. These get the style classes @code{.up}
    and @code{.down} to indicate in which direction they are moving.

    Other style classes that may be added to scrollbars inside
    @class{gtk-scrolled-window} include the positional classes @code{.left},
    @code{.right}, @code{.top}, @code{.bottom} and style classes related to
    overlay scrolling @code{.overlay-indicator}, @code{.dragging},
    @code{.hovering}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[fixed-slider-length]{entry}
        The @code{fixed-slider-length} style property of type @code{:boolean}
        (Read) @br{}
        Don't change slider size, just lock it to the minimum length. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[has-backward-stepper]{entry}
        The @code{has-backward-stepper} style property of type @code{:boolean}
        (Read)@br{}
        Display the standard backward arrow button. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[has-forward-stepper]{entry}
        The @code{has-forward-stepper} style property of type @code{:boolean}
        (Read) @br{}
        Display the standard forward arrow button. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[has-secondary-backward-stepper]{entry}
        The @code{has-secondary-backward-stepper} style property of type
        @code{:boolean} (Read) @br{}
        Display a second backward arrow button on the opposite end of the
        scrollbar. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[has-secondary-forward-stepper]{entry}
        The @code{has-secondary-forward-stepper} style property of type
        @code{:boolean} (Read) @br{}
        Display a second forward arrow button on the opposite end of the
        scrollbar. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[min-slider-length]{entry}
        The @code{min-slider-length} style property of type @code{:int} (Read)
        @br{}
        Minimum length of scrollbar slider. @br{}
        @em{Warning:} The @code{min-slider-length} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use min-height/min-width CSS properties on the slider element
        instead. The value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 21
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-class{gtk-adjustment}
  @see-class{gtk-scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; gtk_scrollbar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollbar-new))

(defun gtk-scrollbar-new (orientation adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-20}
  @argument[orientation]{the scrollbar's orientation of type
    @symbol{gtk-orientation}}
  @argument[adjustment]{the @class{gtk-adjustment} to use, or @code{nil} to
    create a new adjustment}
  @return{The new @class{gtk-scrollbar} widget.}
  @short{Creates a new scrollbar with the given @arg{orientation}.}
  @see-class{gtk-scrollbar}
  @see-class{gtk-adjustment}
  @see-symbol{gtk-orientation}"
  (make-instance 'gtk-scrollbar
                 :orientation orientation
                 :adjustment adjustment))

(export 'gtk-scrollbar-new)

;;; ----------------------------------------------------------------------------
;;; GtkHScrollbar
;;;
;;; A horizontal scrollbar
;;;
;;; Synopsis
;;;
;;;     GtkHScrollbar
;;;
;;;     gtk_hscrollbar_new
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHScrollbar
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkHScrollbar" 'gtk-hscrollbar)
  (setf *lisp-name-exceptions*
        (append '(("GtkHScrollbar" GTK-HSCROLLBAR)) *lisp-name-exceptions*)))

(define-g-object-class "GtkHScrollbar" gtk-hscrollbar
  (:superclass gtk-scrollbar
   :export nil
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_hscrollbar_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-hscrollbar 'type)
 "@version{2013-5-20}
  @begin{short}
    The @sym{gtk-hscrollbar} widget is a widget arranged horizontally creating
    a scrollbar. See @class{gtk-scrollbar} for details on scrollbars.
  @end{short}
  @class{gtk-adjustment} pointers may be added to handle the adjustment of the
  scrollbar or it may be left @code{nil} in which case one will be created for
  you. See @class{gtk-scrollbar} for a description of what the fields in an
  adjustment represent for a scrollbar.
  @begin[Warning]{dictionary}
    @sym{gtk-hscrollbar} has been deprecated, use @class{gtk-scrollbar}
    instead.
  @end{dictionary}
  @see-class{gtk-scrollbar}")

;;; ----------------------------------------------------------------------------
;;; gtk_hscrollbar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hscrollbar-new))

(defun gtk-hscrollbar-new (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[adjustment]{the @class{gtk-adjustment} to use, or @code{nil} to
    create a new adjustment}
  @return{The new @class{gtk-hscrollbar} widget.}
  @short{Creates a new horizontal scrollbar.}
  @begin[Warning]{dictionary}
    @sym{gtk-hscrollbar-new} has been deprecated since version 3.2 and should
    not be used in newly-written code. Use the @fun{gtk-scrollbar-new} function
    with @code{:horizontal} instead.
  @end{dictionary}
  @see-class{scrollbar}
  @see-function{gtk-scrollbar-new}"
  (make-instance 'gtk-scrollbar
                 :orientation :horizontal
                 :adjustment adjustment))

;;; ----------------------------------------------------------------------------
;;; GtkVScrollbar
;;;
;;; A vertical scrollbar
;;;
;;; Synopsis
;;;
;;;     GtkVScrollbar
;;;
;;;     gtk_vscrollbar_new
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVScrollbar
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkVScrollbar" 'gtk-vscrollbar)
  (setf *lisp-name-exceptions*
        (append '(("GtkVScrollbar" GTK-VSCROLLBAR)) *lisp-name-exceptions*)))

(define-g-object-class "GtkVScrollbar" gtk-vscrollbar
  (:superclass gtk-scrollbar
   :export nil
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_vscrollbar_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-vscrollbar 'type)
 "@version{2013-5-20}
  @begin{short}
    The @sym{gtk-vscrollbar} widget is a widget arranged vertically creating a
    scrollbar. See @class{gtk-scrollbar} for details on scrollbars.
    @class{gtk-adjustment} pointers may be added to handle the adjustment of the
    scrollbar or it may be left @code{nil} in which case one will be created
    for you. See @class{gtk-scrollbar} for a description of what the fields in
    an adjustment represent for a scrollbar.
  @end{short}
  @begin[Warning]{dictionary}
    @sym{gtk-vscrollbar} has been deprecated, use @class{gtk-scrollbar}
    instead.
  @end{dictionary}
  @see-class{gtk-scrollbar}")

;;; ----------------------------------------------------------------------------
;;; gtk_vscrollbar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-vscrollbar-new))

(defun gtk-vscrollbar-new (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[adjustment]{the @class{gtk-adjustment} to use, or @code{nil} to
    create a new adjustment}
  @return{The new @class{gtk-vscrollbar} widget.}
  @short{Creates a new vertical scrollbar.}
  @begin[Warning]{dictionary}
    @sym{gtk-vscrollbar-new} has been deprecated since version 3.2 and should
    not be used in newly-written code. Use the @fun{gtk-scrollbar-new} function
    with @code{:vertical} instead.
  @end{dictionary}
  @see-class{gtk-scrollbar}
  @see-function{gtk-scrollbar-new}"
  (make-instance 'gtk-scrollbar
                 :orientation :vertical
                 :adjustment adjustment))

;;; --- End of file gtk.scrollbar.lisp -----------------------------------------
