;;; ----------------------------------------------------------------------------
;;; gtk.handle-box.lisp
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
 "@version{2020-1-20}
  @begin{short}
    The @sym{gtk-handle-box} widget allows a portion of a window to be
    \"torn off\".
  @end{short}
  It is a bin widget which displays its child and a handle that the user can
  drag to tear off a separate window (the float window) containing the child
  widget. A thin ghost is drawn in the original location of the handlebox. By
  dragging the separate window back to its original location, it can be
  reattached.

  When reattaching, the ghost and float window must be aligned along one of
  the edges, the snap edge. This either can be specified by the application
  programmer explicitely, or GTK+ will pick a reasonable default based on the
  handle position.

  To make detaching and reattaching the handlebox as minimally confusing as
  possible to the user, it is important to set the snap edge so that the snap
  edge does not move when the handlebox is deattached. For instance, if the
  handlebox is packed at the bottom of a vertical @class{gtk-box}, then when
  the handlebox is detached, the bottom edge of the handlebox's allocation will
  remain fixed as the height of the handlebox shrinks, so the snap edge should
  be set to @code{:bottom}.
  @begin[Warning]{dictionary}
    @sym{gtk-handle-box} has been deprecated since GTK+ 3.4. It is very
    specialized, lacks features to make it useful and most importantly does not
    fit well into modern application design. Do not use it. There is no
    replacement.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"child-attached\" signal}
      @begin{pre}
 lambda (handlebox widget)    : Run First
      @end{pre}
      This signal is emitted when the contents of the handle box are reattached
      to the main window.
      @begin[code]{table}
        @entry[handlebox]{The object which received the signal.}
        @entry[widget]{The child widget of the handlebox. This argument provides
          no extra information and is here only for backwards-compatibility.}
      @end{table}
    @subheading{The \"child-detached\" signal}
      @begin{pre}
 lambda (handlebox widget)    : Run First
      @end{pre}
      This signal is emitted when the contents of the handle box are detached
      from the main window.
      @begin[code]{table}
        @entry[handlebox]{The object which received the signal.}
        @entry[widget]{The child widget of the handlebox. This argument provides
          no extra information and is here only for backwards-compatibility.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-handle-box-child-detached}
  @see-slot{gtk-handle-box-handle-position}
  @see-slot{gtk-handle-box-shadow-type}
  @see-slot{gtk-handle-box-snap-edge}
  @see-slot{gtk-handle-box-snap-edge-set}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-handle-box-child-detached ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "child-detached"
                                               'gtk-handle-box) 't)
 "The @code{child-detached} property of type @code{:boolean} (Read) @br{}
  A boolean value indicating whether the handlebox's child is attached or
  detached. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-handle-box-child-detached atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-handle-box-child-detached 'function)
 "@version{2020-1-20}
  @begin{short}
    Accessor of the @slot[gtk-handle-box]{child-detached} slot of the
    @class{gtk-handle-box} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-handle-box-child-detached} has been deprecated since
    version 3.4 and should not be used in newly-written code.
    @class{gtk-handle-box} has been deprecated.
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
 "@version{2020-1-20}
  @begin{short}
    Accessor of the @slot[gtk-handle-box]{handle-position} slot of the
    @class{gtk-handle-box} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-handle-box-handle-position} has been deprecated since
    version 3.4 and should not be used in newly-written code.
    @class{gtk-handle-box} has been deprecated.
  @end{dictionary}
  @see-class{gtk-handle-box}")

;;; --- gtk-handle-box-shadow-type ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type"
                                               'gtk-handle-box) 't)
 "The @code{shadow-type} property of type @symbol{gtk-shadow-type}
  (Read / Write) @br{}
  Appearance of the shadow that surrounds the container. @br{}
  Default value: @code{:out}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-handle-box-shadow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-handle-box-shadow-type 'function)
 "@version{2020-1-20}
  @begin{short}
    Accessor of the @slot[gtk-handle-box]{shadow-type} slot of the
    @class{gtk-handle-box} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-handle-box-shadow-type} has been deprecated since
    version 3.4 and should not be used in newly-written code.
    @class{gtk-handle-box} has been deprecated.
  @end{dictionary}
  @see-class{gtk-handle-box}")

;;; --- gtk-handle-box-snap-edge -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "snap-edge" 'gtk-handle-box) 't)
 "The @code{snap-edge} property of type @symbol{gtk-position-type}
  (Read / Write) @br{}
  Side of the handlebox that's lined up with the docking point to dock the
  handlebox. @br{}
  Default value: @code{:top}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-handle-box-snap-edge atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-handle-box-snap-edge 'function)
 "@version{2020-1-20}
  @begin{short}
    Accessor of the @slot[gtk-handle-box]{snap-edge} slot of the
    @class{gtk-handle-box} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-handle-box-snap-edge} has been deprecated since
    version 3.4 and should not be used in newly-written code.
    @class{gtk-handle-box} has been deprecated.
  @end{dictionary}
  @see-class{gtk-handle-box}")

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
 "@version{2020-1-20}
  @begin{short}
    Accessor of the @slot[gtk-handle-box]{snap-edge-set} slot of the
    @class{gtk-handle-box} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-handle-box-snap-edge-set} has been deprecated since
    version 3.4 and should not be used in newly-written code.
    @class{gtk-handle-box} has been deprecated.
  @end{dictionary}
  @see-class{gtk-handle-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_handle_box_new ()
;;;
;;; GtkWidget * gtk_handle_box_new (void);
;;;
;;; Warning
;;;
;;; gtk_handle_box_new has been deprecated since version 3.4 and should not be
;;; used in newly-written code. GtkHandleBox has been deprecated.
;;;
;;; Create a new handle box.
;;;
;;; Returns :
;;;     a new GtkHandleBox.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_handle_box_set_shadow_type ()
;;;
;;; void gtk_handle_box_set_shadow_type (GtkHandleBox *handle_box,
;;;                                      GtkShadowType type);
;;;
;;; Warning
;;;
;;; gtk_handle_box_set_shadow_type has been deprecated since version 3.4 and
;;; should not be used in newly-written code. GtkHandleBox has been deprecated.
;;;
;;; Sets the type of shadow to be drawn around the border of the handle box.
;;;
;;; handle_box :
;;;     a GtkHandleBox
;;;
;;; type :
;;;     the shadow type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_handle_box_set_handle_position ()
;;;
;;; void gtk_handle_box_set_handle_position (GtkHandleBox *handle_box,
;;;                                          GtkPositionType position);
;;;
;;; Warning
;;;
;;; gtk_handle_box_set_handle_position has been deprecated since version 3.4 and
;;; should not be used in newly-written code. GtkHandleBox has been deprecated.
;;;
;;; Sets the side of the handlebox where the handle is drawn.
;;;
;;; handle_box :
;;;     a GtkHandleBox
;;;
;;; position :
;;;     the side of the handlebox where the handle should be drawn.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_handle_box_set_snap_edge ()
;;;
;;; void gtk_handle_box_set_snap_edge (GtkHandleBox *handle_box,
;;;                                    GtkPositionType edge);
;;;
;;; Warning
;;;
;;; gtk_handle_box_set_snap_edge has been deprecated since version 3.4 and
;;; should not be used in newly-written code. GtkHandleBox has been deprecated.
;;;
;;; Sets the snap edge of a handlebox. The snap edge is the edge of the detached
;;; child that must be aligned with the corresponding edge of the "ghost" left
;;; behind when the child was detached to reattach the torn-off window. Usually,
;;; the snap edge should be chosen so that it stays in the same place on the
;;; screen when the handlebox is torn off.
;;;
;;; If the snap edge is not set, then an appropriate value will be guessed from
;;; the handle position. If the handle position is GTK_POS_RIGHT or
;;; GTK_POS_LEFT, then the snap edge will be GTK_POS_TOP, otherwise it will be
;;; GTK_POS_LEFT.
;;;
;;; handle_box :
;;;     a GtkHandleBox
;;;
;;; edge :
;;;     the snap edge, or -1 to unset the value; in which case GTK+ will try to
;;;     guess an appropriate value in the future.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_handle_box_get_handle_position ()
;;;
;;; GtkPositionType gtk_handle_box_get_handle_position
;;;                                                  (GtkHandleBox *handle_box);
;;;
;;; Warning
;;;
;;; gtk_handle_box_get_handle_position has been deprecated since version 3.4 and
;;; should not be used in newly-written code. GtkHandleBox has been deprecated.
;;;
;;; Gets the handle position of the handle box. See
;;; gtk_handle_box_set_handle_position().
;;;
;;; handle_box :
;;;     a GtkHandleBox
;;;
;;; Returns :
;;;     the current handle position.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_handle_box_get_shadow_type ()
;;;
;;; GtkShadowType gtk_handle_box_get_shadow_type (GtkHandleBox *handle_box);
;;;
;;; Warning
;;;
;;; gtk_handle_box_get_shadow_type has been deprecated since version 3.4 and
;;; should not be used in newly-written code. GtkHandleBox has been deprecated.
;;;
;;; Gets the type of shadow drawn around the handle box. See
;;; gtk_handle_box_set_shadow_type().
;;;
;;; handle_box :
;;;     a GtkHandleBox
;;;
;;; Returns :
;;;     the type of shadow currently drawn around the handle box.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_handle_box_get_snap_edge ()
;;;
;;; GtkPositionType gtk_handle_box_get_snap_edge (GtkHandleBox *handle_box);
;;;
;;; Warning
;;;
;;; gtk_handle_box_get_snap_edge has been deprecated since version 3.4 and
;;; should not be used in newly-written code. GtkHandleBox has been deprecated.
;;;
;;; Gets the edge used for determining reattachment of the handle box. See
;;; gtk_handle_box_set_snap_edge().
;;;
;;; handle_box :
;;;     a GtkHandleBox
;;;
;;; Returns :
;;;     the edge used for determining reattachment, or (GtkPositionType)-1 if
;;;     this is determined (as per default) from the handle position.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_handle_box_get_child_detached ()
;;;
;;; gboolean gtk_handle_box_get_child_detached (GtkHandleBox *handle_box);
;;;
;;; Warning
;;;
;;; gtk_handle_box_get_child_detached has been deprecated since version 3.4 and
;;; should not be used in newly-written code. GtkHandleBox has been deprecated.
;;;
;;; Whether the handlebox's child is currently detached.
;;;
;;; handle_box :
;;;     a GtkHandleBox
;;;
;;; Returns :
;;;     TRUE if the child is currently detached, otherwise FALSE
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; --- gtk.handle-box.lisp ----------------------------------------------------
