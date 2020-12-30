;;; ----------------------------------------------------------------------------
;;; gtk.overlay.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkOverlay
;;;
;;;     A container which overlays widgets on top of each other
;;;
;;; Types and Values
;;;
;;;     GtkOverlay
;;;
;;; Functions
;;;
;;;     gtk_overlay_new
;;;     gtk_overlay_add_overlay
;;;     gtk_overlay_reorder_overlay
;;;     gtk_overlay_get_overlay_pass_through   -> gtk-overlay-child-pass-through
;;;     gtk_overlay_set_overlay_pass_through   -> gtk-overlay-child-pass-through
;;;
;;; Child Properties
;;;
;;;         gint    index                 Read / Write
;;;     gboolean    pass-through          Read / Write
;;;
;;; Signals
;;;
;;;     gboolean    get-child-position    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkOverlay
;;;
;;; Implemented Interfaces
;;;
;;;     GtkOverlay implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkOverlay
;;; ----------------------------------------------------------------------------

;; TODO: The "get-child-position" signal handler has an return location
;; of type GdkRectangle. Check the Lisp implementation of a handler.

(define-g-object-class "GtkOverlay" gtk-overlay
  (:superclass gtk-bin
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_overlay_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-overlay 'type)
 "@version{2020-5-9}
  @begin{short}
    @sym{gtk-overlay} is a container which contains a single main child, on top
    of which it can place overlay widgets.
  @end{short}
  The position of each overlay widget is determined by its
  @slot[gtk-widget]{halign} and @slot[gtk-widget]{valign} properties. E.g. a
  widget with both alignments set to @code{:start} will be placed at the top
  left corner of the main widget, whereas an overlay with
  @slot[gtk-widget]{halign} set to @code{:center} and @slot[gtk-widget]{valign}
  set to @code{:end} will be placed a the bottom edge of the main widget,
  horizontally centered. The position can be adjusted by setting the margin
  properties of the child to non-zero values.

  More complicated placement of overlays is possible by connecting to the
  \"get-child-position\" signal.
  @begin[GtkOverlay as GtkBuildable]{dictionary}
    The @sym{gtk-overlay} implementation of the @class{gtk-buildable} interface
    supports placing a child as an overlay by specifying @code{\"overlay\"} as
    the @code{\"type\"} attribute of a @code{<child>} element.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @sym{gtk-overlay} has a single CSS node with the name @code{overlay}.
    Overlay children whose alignments cause them to be positioned at an edge get
    the style classes @code{.left}, @code{.right}, @code{.top}, and/or
    @code{.bottom} according to their position.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[index]{entry}
        The @code{index} child property of type @code{:int} (Read / Write) @br{}
        The index of the overlay in the parent, -1 for the main child.
        Since 3.18 @br{}
        Allowed values: >= -1 @br{}
        Default value: 0
      @end{entry}
      @begin[pass-through]{entry}
        The @code{pass-through} child property of type @code{:boolean}
        (Read / Write) @br{}
        Pass through input, does not affect main child. Since 3.18 @br{}
        Default value: @em{false}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"get-child-position\" signal}
      @begin{pre}
 lambda (overlay widget allocation)    : Run Last
      @end{pre}
      The \"get-child-position\" signal is emitted to determine the position
      and size of any overlay child widgets. A handler for this signal should
      fill allocation with the desired position and size for widget, relative
      to the 'main' child of overlay.
      The default handler for this signal uses the widget's
      @slot[gtk-widget]{halign} and @slot[gtk-widget]{valign} properties to
      determine the position and gives the widget its natural size (except that
      an alignment of @code{:fill} will cause the overlay to be
      full-width/height). If the main child is a @class{gtk-scrolled-window},
      the overlays are placed relative to its contents.
      @begin[code]{table}
        @entry[overlay]{The @class{gtk-overlay} container which emitted the
          signal.}
        @entry[widget]{The @class{gtk-widget} child widget to position.}
        @entry[allocation]{Return location of type @class{gdk-rectangle} for
          the allocation.}
        @entry[Return]{@em{True} if the allocation has been filled.}
      @end{table}
  @end{dictionary}
  @see-class{gtk-buildable}
  @see-class{gtk-scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-overlay-child-index ------------------------------------------------

(define-child-property "GtkOverlay"
                       gtk-overlay-child-index
                       "index" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-overlay-child-index atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-overlay-child-index 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-overlay-child-index object) => index)}
  @syntax[]{(setf (gtk-overlay-child-index object) index)}
  @argument[container]{a @class{gtk-overlay} container}
  @argument[child]{the @class{gtk-widget} child widget}
  @argument[index]{an integer with the index of the child widget in the parent}
  @begin{short}
    Accessor of the @code{index} child property of the
    @class{gtk-overlay} class.
  @end{short}

  The index of the child widget in the parent, -1 for the main child.

  Since 3.18
  @see-class{gtk-overlay}
  @see-function{gtk-overlay-add-overlay}
  @see-function{gtk-overlay-reorder-overlay}")

;;; --- gtk-overlay-child-pass-through -----------------------------------------

(define-child-property "GtkOverlay"
                       gtk-overlay-child-pass-through
                       "pass-through" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-overlay-child-pass-through atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-overlay-child-pass-through 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-overlay-child-index container child) => pass-through)}
  @syntax[]{(setf (gtk-overlay-child-index container child) pass-through)}
  @argument[container]{a @class{gtk-overlay} container}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[pass-through]{a boolean wether to pass through input}
  @begin{short}
    Accessor of the @code{pass-through} child property of the
    @class{gtk-overlay} class.
  @end{short}

  Pass through input, does not affect main child.

  Since 3.18
  @see-class{gtk-overlay}")

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-overlay-new))

(defun gtk-overlay-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @return{A new @class{gtk-overlay} container.}
  @short{Creates a new overlay container.}

  Since 3.2
  @see-class{gtk-overlay}"
  (make-instance 'gtk-overlay))

(export 'gtk-overlay-new)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_add_overlay ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_overlay_add_overlay" gtk-overlay-add-overlay) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @argument[overlay]{a @class{gtk-overlay} container}
  @argument[widget]{a @class{gtk-widget} child widget to be added to the
    container}
  @short{Adds a child widget to the overlay container.}

  The child widget will be stacked on top of the main widget added with the
  function @fun{gtk-container-add}.

  The position at which the child widget is placed is determined from its
  @slot[gtk-widget]{halign} and @slot[gtk-widget]{valign} properties.

  Since 3.2
  @see-class{gtk-overlay}"
  (overlay (g-object gtk-overlay))
  (widget (g-object gtk-widget)))

(export 'gtk-overlay-add-overlay)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_reorder_overlay ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_overlay_reorder_overlay" gtk-overlay-reorder-overlay) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @argument[overlay]{a @class{gtk-overlay} container}
  @argument[child]{the overlaid @class{gtk-widget} child widget to move}
  @argument[position]{the new index for the child widget in the list of overlay
    children of the overlay container, starting from 0, if negative, indicates
    the end of the list}
  @begin{short}
    Moves a child widget to a new index in the list of overlay children.
  @end{short}
  The list contains overlays in the order that these were added to the overlay
  container.

  An index of the widget in the overlay children list determines which order
  the children are drawn if they overlap. The first child is drawn at the
  bottom. It also affects the default focus chain order.

  Since 3.18
  @see-class{gtk-overlay}"
  (overlay (g-object gtk-overlay))
  (child (g-object gtk-widget))
  (position :int))

(export 'gtk-overlay-reorder-overlay)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_get_overlay_pass_through ()
;;; gtk_overlay_set_overlay_pass_through ()
;;; ----------------------------------------------------------------------------

;; Implemented as the child accessor gtk-overlay-child-pass-through

;;; --- End of file gtk.overlay.lisp -------------------------------------------
