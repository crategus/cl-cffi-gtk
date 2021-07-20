;;; ----------------------------------------------------------------------------
;;; gtk.handle-box.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; GtkHandleBox
;;;
;;;     A widget for detachable window portions
;;;
;;; Types and Values
;;;
;;;     GtkHandleBox
;;;
;;; Functions
;;;
;;;     gtk_handle_box_new
;;;     gtk_handle_box_set_shadow_type                     Accessor
;;;     gtk_handle_box_set_handle_position                 Accessor
;;;     gtk_handle_box_set_snap_edge                       Accessor
;;;     gtk_handle_box_get_handle_position                 Accessor
;;;     gtk_handle_box_get_shadow_type                     Accessor
;;;     gtk_handle_box_get_snap_edge                       Accessor
;;;     gtk_handle_box_get_child_detached                  Accessor
;;;
;;; Properties
;;;
;;;            gboolean    child-detached     Read
;;;     GtkPositionType    handle-position    Read / Write
;;;       GtkShadowType    shadow-type        Read / Write
;;;     GtkPositionType    snap-edge          Read / Write
;;;            gboolean    snap-edge-set      Read / Write
;;;
;;; Signals
;;;
;;;                void    child-attached     Run First
;;;                void    child-detached     Run First
;;;
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkHandleBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkHandleBox implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkHandleBox
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkHandleBox" gtk-handle-box
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_handle_box_get_type")
  ((child-detached
    gtk-handle-box-child-detached
    "child-detached" "gboolean" t nil)
   (handle-position
    gtk-handle-box-handle-position
    "handle-position" "GtkPositionType" t t)
   (shadow-type
    gtk-handle-box-shadow-type
    "shadow-type" "GtkShadowType" t t)
   (snap-edge
    gtk-handle-box-snap-edge
    "snap-edge" "GtkPositionType" t t)
   (snap-edge-set
    gtk-handle-box-snap-edge-set
    "snap-edge-set" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-handle-box 'type)
 "@version{2021-5-25}
  @begin{short}
    The @sym{gtk-handle-box} widget allows a portion of a window to be
    \"torn off\".
  @end{short}
  It is a bin widget which displays its child and a handle that the user can
  drag to tear off a separate window, the float window, containing the child
  widget. A thin ghost is drawn in the original location of the handle box. By
  dragging the separate window back to its original location, it can be
  reattached.

  When reattaching, the ghost and float window must be aligned along one of
  the edges, the snap edge. This either can be specified by the application
  programmer explicitely, or GTK will pick a reasonable default based on the
  handle position.

  To make detaching and reattaching the handle box as minimally confusing as
  possible to the user, it is important to set the snap edge so that the snap
  edge does not move when the handle box is deattached. For instance, if the
  handle box is packed at the bottom of a vertical @class{gtk-box} widget, then
  when the handle box is detached, the bottom edge of the allocation of the
  handle box will remain fixed as the height of the handle box shrinks, so the
  snap edge should be set to @code{:bottom}.
  @begin[Warning]{dictionary}
    The @sym{gtk-handle-box} widget has been deprecated since GTK 3.4. It is
    very specialized, lacks features to make it useful and most importantly
    does not fit well into modern application design. Do not use it. There is
    no replacement.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"child-attached\" signal}
      @begin{pre}
 lambda (handlebox widget)    :run-first
      @end{pre}
      The signal is emitted when the contents of the handle box are reattached
      to the main window.
      @begin[code]{table}
        @entry[handlebox]{The @sym{gtk-handle-box} widget which received the
          signal.}
        @entry[widget]{The @class{gtk-widget} child widget of the handle box.
          This argument provides no extra information and is here only for
          backwards compatibility.}
      @end{table}
    @subheading{The \"child-detached\" signal}
      @begin{pre}
 lambda (handlebox widget)    :run-first
      @end{pre}
      The signal is emitted when the contents of the handle box are detached
      from the main window.
      @begin[code]{table}
        @entry[handlebox]{The @sym{gtk-handle-box} widget which received the
          signal.}
        @entry[widget]{The @class{gtk-widget} child widget of the handle box.
          This argument provides no extra information and is here only for
          backwards compatibility.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-handle-box-child-detached}
  @see-slot{gtk-handle-box-handle-position}
  @see-slot{gtk-handle-box-shadow-type}
  @see-slot{gtk-handle-box-snap-edge}
  @see-slot{gtk-handle-box-snap-edge-set}
  @see-class{gtk-bin}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-handle-box-child-detached ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "child-detached"
                                               'gtk-handle-box) 't)
 "The @code{child-detached} property of type @code{:boolean} (Read) @br{}
  Whether the child of the handle box is attached or detached. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-handle-box-child-detached atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-handle-box-child-detached 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-handle-box-child-detached object) => detached}
  @argument[object]{a @class{gtk-handle-box} widget}
  @argument[detached]{a boolean whether the child is detached}
  @begin{short}
    Accessor of the @slot[gtk-handle-box]{child-detached} slot of the
    @class{gtk-handle-box} class.
  @end{short}

  Returns whether the child of the handle box is currently detached.
  @begin[Warning]{dictionary}
    The function @sym{gtk-handle-box-child-detached} has been deprecated since
    version 3.4 and should not be used in newly written code. The
    @class{gtk-handle-box} widget has been deprecated.
  @end{dictionary}
  @see-class{gtk-handle-box}")

;;; --- gtk-handle-box-handle-position -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "handle-position"
                                               'gtk-handle-box) 't)
 "The @code{handle-position} property of type @symbol{gtk-position-type}
  (Read / Write) @br{}
  Position of the handle relative to the child widget. @br{}
  Default value: @code{:left}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-handle-box-handle-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-handle-box-handle-position 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-handle-box-handle-position object) => position}
  @syntax[]{(setf (gtk-handle-box-handle-position object) position)}
  @argument[object]{a @class{gtk-handle-box} widget}
  @argument[position]{a value of the @symbol{gtk-position-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-handle-box]{handle-position} slot of the
    @class{gtk-handle-box} class.
  @end{short}

  The slot access function @sym{gtk-handle-box-handle-position} gets the side
  of the handle box where the handle should be drawn. The slot access function
  @sym{(setf gtk-handle-box-handle-position)} sets the handle position.
  @begin[Warning]{dictionary}
    The function @sym{gtk-handle-box-handle-position} has been deprecated since
    version 3.4 and should not be used in newly written code. The
    @class{gtk-handle-box} widget has been deprecated.
  @end{dictionary}
  @see-class{gtk-handle-box}
  @see-symbol{gtk-position-type}")

;;; --- gtk-handle-box-shadow-type ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type"
                                               'gtk-handle-box) 't)
 "The @code{shadow-type} property of type @symbol{gtk-shadow-type}
  (Read / Write) @br{}
  Appearance of the shadow that surrounds the handle box. @br{}
  Default value: @code{:out}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-handle-box-shadow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-handle-box-shadow-type 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-handle-box-shadow-type object) => shadow-type}
  @syntax[]{(setf (gtk-handle-box-shadow-type object) shadow-type)}
  @argument[object]{a @class{gtk-handle-box} widget}
  @argument[shadow-type]{a value of the @symbol{gtk-shadow-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-handle-box]{shadow-type} slot of the
    @class{gtk-handle-box} class.
  @end{short}

  The function @sym{gtk-handle-box-shadow-type} gets the type of shadow drawn
  around the handle box. The function @sym{(setf gtk-handle-box-shadow-type)}
  sets the type of shadow.
  @begin[Warning]{dictionary}
    The function @sym{gtk-handle-box-shadow-type} has been deprecated since
    version 3.4 and should not be used in newly written code. The
    @class{gtk-handle-box} widget has been deprecated.
  @end{dictionary}
  @see-class{gtk-handle-box}
  @see-symbol{gtk-shadow-type}")

;;; --- gtk-handle-box-snap-edge -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "snap-edge" 'gtk-handle-box) 't)
 "The @code{snap-edge} property of type @symbol{gtk-position-type}
  (Read / Write) @br{}
  Side of the handle box that is lined up with the docking point to dock the
  handle box. @br{}
  Default value: @code{:top}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-handle-box-snap-edge atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-handle-box-snap-edge 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-handle-box-snap-edge object) => edge}
  @syntax[]{(setf (gtk-handle-box-snap-edge object) edge)}
  @argument[object]{a @class{gtk-handle-box} widget}
  @argument[edge]{a value of the @symbol{gtk-position-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-handle-box]{snap-edge} slot of the
    @class{gtk-handle-box} class.
  @end{short}

  The slot access function @sym{gtk-handle-box-snap-edge} gets the edge used
  for determining reattachment of the handle box. The slot access function
  @sym{(setf gtk-handle-box-snap-edge)} sets the snap edge.

  The snap edge is the edge of the detached child that must be aligned with the
  corresponding edge of the \"ghost\" left behind when the child was detached
  to reattach the torn-off window. Usually, the snap edge should be chosen so
  that it stays in the same place on the screen when the handle box is torn off.

  If the snap edge is not set, then an appropriate value will be guessed from
  the handle position. If the handle position is @code{:right} or @code{:left},
  then the snap edge will be @code{:top}, otherwise it will be @code{:left}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-handle-box-snap-edge} has been deprecated since
    version 3.4 and should not be used in newly written code. The
    @class{gtk-handle-box} widget has been deprecated.
  @end{dictionary}
  @see-class{gtk-handle-box}
  @see-symbol{gtk-position-type}")

;;; --- gtk-handle-box-snap-edge-set -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "snap-edge-set"
                                               'gtk-handle-box) 't)
 "The @code{snap-edge-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether to use the value from the @code{snap-edge} property or a value
  derived from the @code{handle-position} property. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-handle-box-snap-edge-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-handle-box-snap-edge-set 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-handle-box-snap-edge-set object) => setting}
  @syntax[]{(setf (gtk-handle-box-snap-edge-set object) setting)}
  @argument[object]{a @class{gtk-handle-box} widget}
  @argument[setting]{a boolean whether to use the value from the
    @slot[gtk-handle-box]{snap-edge} property}
  @begin{short}
    Accessor of the @slot[gtk-handle-box]{snap-edge-set} slot of the
    @class{gtk-handle-box} class.
  @end{short}

  Whether to use the value from the @slot[gtk-handle-box]{snap-edge} property
  or a value derived from the @slot[gtk-handle-box]{handle-position} property.
  @begin[Warning]{dictionary}
    The function @sym{gtk-handle-box-snap-edge-set} has been deprecated since
    version 3.4 and should not be used in newly written code. The
    @class{gtk-handle-box} widget has been deprecated.
  @end{dictionary}
  @see-class{gtk-handle-box}
  @see-function{gtk-handle-box-snap-edge}
  @see-function{gtk-handle-box-handle-position}")

;;; ----------------------------------------------------------------------------
;;; gtk_handle_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-handle-box-new))

(defun gtk-handle-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @return{A new @class{gtk-handle-box} widget.}
  @begin{short}
    Create a new handle box.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-handle-box-new} has been deprecated since version 3.4
    and should not be used in newly written code. The @class{gtk-handle-box}
    widget has been deprecated.
  @end{dictionary}
  @see-class{gtk-handle-box}"
  (make-instance 'gtk-handle-box))

(export 'gtk-handle-box-new)

;;; --- gtk.handle-box.lisp ----------------------------------------------------
