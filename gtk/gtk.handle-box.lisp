;;; ----------------------------------------------------------------------------
;;; gtk.handle-box.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;;﻿
;;; GtkHandleBox
;;;
;;; A widget for detachable window portions
;;;
;;; Synopsis
;;;
;;;     GtkHandleBox
;;;
;;;     gtk_handle_box_new
;;;     gtk_handle_box_set_shadow_type
;;;     gtk_handle_box_set_handle_position
;;;     gtk_handle_box_set_snap_edge
;;;     gtk_handle_box_get_handle_position
;;;     gtk_handle_box_get_shadow_type
;;;     gtk_handle_box_get_snap_edge
;;;     gtk_handle_box_get_child_detached
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkHandleBox
;;;
;;; Implemented Interfaces
;;;
;;; GtkHandleBox implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;;
;;;   "child-detached"           gboolean              : Read
;;;   "handle-position"          GtkPositionType       : Read / Write
;;;   "shadow-type"              GtkShadowType         : Read / Write
;;;   "snap-edge"                GtkPositionType       : Read / Write
;;;   "snap-edge-set"            gboolean              : Read / Write
;;;
;;; Signals
;;;
;;;   "child-attached"                                 : Run First
;;;   "child-detached"                                 : Run First
;;;
;;; Description
;;;
;;; The GtkHandleBox widget allows a portion of a window to be "torn off". It is
;;; a bin widget which displays its child and a handle that the user can drag to
;;; tear off a separate window (the float window) containing the child widget. A
;;; thin ghost is drawn in the original location of the handlebox. By dragging
;;; the separate window back to its original location, it can be reattached.
;;;
;;; When reattaching, the ghost and float window, must be aligned along one of
;;; the edges, the snap edge. This either can be specified by the application
;;; programmer explicitely, or GTK+ will pick a reasonable default based on the
;;; handle position.
;;;
;;; To make detaching and reattaching the handlebox as minimally confusing as
;;; possible to the user, it is important to set the snap edge so that the snap
;;; edge does not move when the handlebox is deattached. For instance, if the
;;; handlebox is packed at the bottom of a VBox, then when the handlebox is
;;; detached, the bottom edge of the handlebox's allocation will remain fixed as
;;; the height of the handlebox shrinks, so the snap edge should be set to
;;; GTK_POS_BOTTOM.
;;;
;;; Note
;;;
;;; GtkHandleBox has been deprecated. It is very specialized, lacks features to
;;; make it useful and most importantly does not fit well into modern
;;; application design. Do not use it. There is no replacement.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "child-detached" property
;;;
;;;   "child-detached"           gboolean              : Read
;;;
;;; A boolean value indicating whether the handlebox's child is attached or
;;; detached.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "handle-position" property
;;;
;;;   "handle-position"          GtkPositionType       : Read / Write
;;;
;;; Position of the handle relative to the child widget.
;;;
;;; Default value: GTK_POS_LEFT
;;;
;;; ----------------------------------------------------------------------------
;;; The "shadow-type" property
;;;
;;;   "shadow-type"              GtkShadowType         : Read / Write
;;;
;;; Appearance of the shadow that surrounds the container.
;;;
;;; Default value: GTK_SHADOW_OUT
;;;
;;; ----------------------------------------------------------------------------
;;; The "snap-edge" property
;;;
;;;   "snap-edge"                GtkPositionType       : Read / Write
;;;
;;; Side of the handlebox that's lined up with the docking point to dock the
;;; handlebox.
;;;
;;; Default value: GTK_POS_TOP
;;;
;;; ----------------------------------------------------------------------------
;;; The "snap-edge-set" property
;;;
;;;   "snap-edge-set"            gboolean              : Read / Write
;;;
;;; Whether to use the value from the snap_edge property or a value derived from
;;; handle_position.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "child-attached" signal
;;;
;;; void user_function (GtkHandleBox *handlebox,
;;;                     GtkWidget    *widget,
;;;                     gpointer      user_data)      : Run First
;;;
;;; Warning
;;;
;;; GtkHandleBox::child-attached has been deprecated since version 3.4 and
;;; should not be used in newly-written code. GtkHandleBox has been deprecated.
;;;
;;; This signal is emitted when the contents of the handlebox are reattached to
;;; the main window.
;;;
;;; handlebox :
;;;     the object which received the signal.
;;;
;;; widget :
;;;     the child widget of the handlebox. (this argument provides no extra
;;;     information and is here only for backwards-compatibility)
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "child-detached" signal
;;;
;;; void user_function (GtkHandleBox *handlebox,
;;;                     GtkWidget    *widget,
;;;                     gpointer      user_data)      : Run First
;;;
;;; Warning
;;;
;;; GtkHandleBox::child-detached has been deprecated since version 3.4 and
;;; should not be used in newly-written code. GtkHandleBox has been deprecated.
;;;
;;; This signal is emitted when the contents of the handlebox are detached from
;;; the main window.
;;;
;;; handlebox :
;;;     the object which received the signal.
;;;
;;; widget :
;;;     the child widget of the handlebox. (this argument provides no extra
;;;     information and is here only for backwards-compatibility)
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkHandleBox
;;;
;;; struct GtkHandleBox;
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
   (shadow
    gtk-handle-box-shadow
    "shadow" "GtkShadowType" t t)
   (shadow-type
    gtk-handle-box-shadow-type
    "shadow-type" "GtkShadowType" t t)
   (snap-edge
    gtk-handle-box-snap-edge
    "snap-edge" "GtkPositionType" t t)
   (snap-edge-set
    gtk-handle-box-snap-edge-set
    "snap-edge-set" "gboolean" t t)))

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
