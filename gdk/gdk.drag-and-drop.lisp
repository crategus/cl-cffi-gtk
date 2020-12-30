;;; ----------------------------------------------------------------------------
;;; gdk.drag-and-drop.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
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
;;; Drag And Drop
;;;
;;;     Functions for controlling drag and drop handling
;;;
;;; Types and Values
;;;
;;;     GdkDragContext
;;;     GdkDragCancelReason
;;;     GdkDragProtocol
;;;     GdkDragAction
;;;
;;; Functions
;;;
;;;     gdk_drag_get_selection
;;;     gdk_drag_abort
;;;     gdk_drop_reply
;;;     gdk_drag_drop
;;;     gdk_drag_drop_done
;;;     gdk_drag_find_window_for_screen
;;;     gdk_drag_begin
;;;     gdk_drag_begin_for_device
;;;     gdk_drag_begin_from_point
;;;     gdk_drag_motion
;;;     gdk_drop_finish
;;;     gdk_drag_status
;;;     gdk_drag_drop_succeeded
;;;
;;;     gdk_window_get_drag_protocol
;;;
;;;     gdk_drag_context_get_actions
;;;     gdk_drag_context_get_suggested_action
;;;     gdk_drag_context_get_selected_action
;;;     gdk_drag_context_list_targets
;;;     gdk_drag_context_get_device
;;;     gdk_drag_context_set_device
;;;     gdk_drag_context_get_source_window
;;;     gdk_drag_context_get_dest_window
;;;     gdk_drag_context_get_protocol
;;;     gdk_drag_context_get_drag_window
;;;     gdk_drag_context_set_hotspot
;;;     gdk_drag_context_manage_dnd
;;;
;;; Signals
;;;
;;;     void  action-changed  Run Last
;;;     void  cancel          Run Last
;;;     void  dnd-finished    Run Last
;;;     void  drop-performed  Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDragContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkDragCancelReason
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(define-g-enum "GdkDragCancelReason" gdk-drag-cancel-reason
  (:export t
   :type-initializer "gdk_drag_cancel_reason_get_type")
  :no-target
  :user-cancelled
  :error)

#+(and gdk-3-20 cl-cffi-gtk-documentation)
(setf (gethash 'gdk-drag-cancel-reason atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-drag-cancel-reason atdoc:*external-symbols*)
 "@version{2019-3-25}
  @begin{short}
    Used in @class{gdk-drag-context} to the reason of a cancelled DND operation.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkDragCancelReason\" gdk-drag-cancel-reason
  (:export t
   :type-initializer \"gdk_drag_cancel_reason_get_type\")
  :no-target
  :user-cancelled
  :error)
  @end{pre}
  @begin[code]{table}
    @entry[:no-target]{There is no suitable drop target.}
    @entry[:user-cancelled]{Drag cancelled by the user.}
    @entry[:error]{Unspecified error.}
  @end{table}
  Since 3.20
  @see-class{gdk-drag-context}")

;;; ----------------------------------------------------------------------------
;;; enum GdkDragProtocol
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkDragProtocol" gdk-drag-protocol
  (:export t
   :type-initializer "gdk_drag_protocol_get_type")
  (:none 0)
  (:motif 1)
  (:xdnd 2)
  (:rootwin 3)
  (:win32-dropfiles 4)
  (:ole2 5)
  (:local 6))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-drag-protocol atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-drag-protocol atdoc:*external-symbols*)
 "@version{2013-11-16}
  @begin{short}
    Used in @class{gdk-drag-context} to indicate the protocol according to
    which DND is done.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkDragProtocol\" gdk-drag-protocol
  (:export t
   :type-initializer \"gdk_drag_protocol_get_type\")
  (:none 0)
  (:motif 1)
  (:xdnd 2)
  (:rootwin 3)
  (:win32-dropfiles 4)
  (:ole2 5)
  (:local 6))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No protocol.}
    @entry[:motif]{The Motif DND protocol.}
    @entry[:xdnd]{The Xdnd protocol.}
    @entry[:rootwin]{An extension to the Xdnd protocol for unclaimed root
      window drops.}
    @entry[:win32-dropfiles]{The simple @code{WM_DROPFILES} protocol.}
    @entry[:ole2]{The complex OLE2 DND protocol (not implemented).}
    @entry[:local]{Intra-application DND.}
  @end{table}
  @see-class{gdk-drag-context}")

;;; ----------------------------------------------------------------------------
;;; enum GdkDragAction
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkDragAction" gdk-drag-action
  (:export t
   :type-initializer "gdk_drag_action_get_type")
  (:default 1)
  (:copy 2)
  (:move 4)
  (:link 8)
  (:private 16)
  (:ask 32))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-drag-action atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-drag-action atdoc:*external-symbols*)
 "@version{2013-10-20}
  @begin{short}
    Used in @class{gdk-drag-context} to indicate what the destination should do
    with the dropped data.
  @end{short}
  @begin{pre}
(define-g-flags \"GdkDragAction\" gdk-drag-action
  (:export t
   :type-initializer \"gdk_drag_action_get_type\")
  (:default 1)
  (:copy 2)
  (:move 4)
  (:link 8)
  (:private 16)
  (:ask 32))
  @end{pre}
  @begin[code]{table}
    @entry[:default]{Means nothing, and should not be used.}
    @entry[:copy]{Copy the data.}
    @entry[:move]{Move the data, i.e. first copy it, then delete it from the
      source using the \"DELETE\" target of the X selection protocol.}
    @entry[:link]{Add a link to the data. Note that this is only useful if
      source and destination agree on what it means.}
    @entry[:private]{Special action which tells the source that the destination
      will do something that the source does not understand.}
    @entry[:ask]{Ask the user what to do with the data.}
  @end{table}
  @see-class{gdk-drag-context}")

;;; ----------------------------------------------------------------------------
;;; GdkDragContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDragContext" gdk-drag-context
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_drag_context_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-drag-context 'type)
 "@version{2013-6-30}
  @begin{short}
    These functions provide a low level interface for drag and drop.
  @end{short}
  The X backend of GDK supports both the Xdnd and Motif drag and drop protocols
  transparently, the Win32 backend supports the @code{WM_DROPFILES} protocol.

  GTK+ provides a higher level abstraction based on top of these functions,
  and so they are not normally needed in GTK+ applications. See the Drag and
  Drop section of the GTK+ documentation for more information.

  @begin[Signal Details]{dictionary}
    @subheading{The \"action-changed\" signal}
    @begin{pre}
 lambda (context action)    : Run Last
    @end{pre}
    A new action is being chosen for the drag and drop operation.

    This signal will only be emitted if the @sym{gtk-drag-context} manages the
    drag and drop operation. See the function @fun{gdk-drag-context-manage-dnd}
    for more information.
    @begin[code]{table}
      @entry[context]{The object on which the signal is emitted.}
      @entry[action]{The action currently chosen.}
    @end{table}
    Since 3.20

    @subheading{The \"cancel\" signal}
    @begin{pre}
 lambda (context reason)    : Run Last
    @end{pre}
    The drag and drop operation was cancelled.

    This signal will only be emitted if the @class{gdk-drag-context} manages the
    drag and drop operation. See the function @fun{gdk-drag-context-manage-dnd}
    for more information.
    @begin[code]{table}
      @entry[context]{The object on which the signal is emitted.}
      @entry[reason]{The reason the context was cancelled.}
    @end{table}
    Since 3.20

    @subheading{The \"dnd-finished\" signal}
    @begin{pre}
 lambda (context)    : Run Last
    @end{pre}
    The drag and drop operation was finished, the drag destination finished
    reading all data. The drag source can now free all miscellaneous data.

    This signal will only be emitted if the @class{gdk-drag-context} manages the
    drag and drop operation. See the function @fun{gdk-drag-context-manage-dnd}
    for more information.
    @begin[code]{table}
      @entry[context]{The object on which the signal is emitted.}
    @end{table}
    Since 3.20

    @subheading{The \"drop-performed\" signal}
    @begin{pre}
 lambda (context time)    : Run Last
    @end{pre}
    The drag and drop operation was performed on an accepting client.

    This signal will only be emitted if the @sym{gdk-drag-context} manages the
    drag and drop operation. See the function @fun{gdk-drag-context-manage-dnd}
    for more information.
    @begin[code]{table}
      @entry[context]{The object on which the signal is emitted.}
      @entry[time]{The time at which the drop happened.}
    @end{table}
    Since 3.20
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_selection ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_get_selection" gdk-drag-get-selection) gdk-atom-as-string
 #+cl-cffi-gtk-documentation
 "@version{2013-4-7}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{The selection atom, or @code{:none}.}
  Returns the selection atom for the current source window."
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-get-selection)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_abort ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_abort" gdk-drag-abort) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-7}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[time]{the timestamp for this operation}
  @begin{short}
    Aborts a drag without dropping.
  @end{short}

  This function is called by the drag source."
  (context (g-object gdk-drag-context))
  (time :uint32))

(export 'gdk-drag-abort)

;;; ----------------------------------------------------------------------------
;;; gdk_drop_reply ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drop_reply" gdk-drop-reply) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-7}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[accepted]{@em{true} if the drop is accepted}
  @argument[time]{the timestamp for this operation}
  @begin{short}
    Accepts or rejects a drop.
  @end{short}

  This function is called by the drag destination in response to a drop
  initiated by the drag source."
  (context (g-object gdk-drag-context))
  (accepted :boolean)
  (time :uint32))

(export 'gdk-drop-reply)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_drop ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_drop" gdk-drag-drop) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-7}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[time]{the timestamp for this operation}
  @begin{short}
    Drops on the current destination.
  @end{short}

  This function is called by the drag source."
  (context (g-object gdk-drag-context))
  (time :uint32))

(export 'gdk-drag-drop)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_drop_done ()
;;;
;;; void
;;; gdk_drag_drop_done (GdkDragContext *context,
;;;                     gboolean success);
;;;
;;; Inform GDK if the drop ended successfully. Passing FALSE for success may
;;; trigger a drag cancellation animation.
;;;
;;; This function is called by the drag source, and should be the last call
;;; before dropping the reference to the context .
;;;
;;; The GdkDragContext will only take the first gdk_drag_drop_done() call as
;;; effective, if this function is called multiple times, all subsequent calls
;;; will be ignored.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; success :
;;;     whether the drag was ultimatively successful
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_find_window_for_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_find_window_for_screen" %gdk-drag-find-window-for-screen)
    :void
  (context (g-object gdk-drag-context))
  (window (g-object gdk-window))
  (screen (g-object gdk-screen))
  (x-root :int)
  (y-root :int)
  (dest-window (:pointer (g-object gdk-window)))
  (protocol (:pointer gdk-drag-protocol)))

(defun gdk-drag-find-window-for-screen (context window screen x-root y-root)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[drag-window]{a window which may be at the pointer position, but
    should be ignored, since it is put up by the drag source as an icon}
  @argument[screen]{the screen where the destination window is sought}
  @argument[x-root]{the x position of the pointer in root coordinates}
  @argument[y-root]{the y position of the pointer in root coordinates}
  @begin{return}
    @code{dest-window} -- the destination window @br{}
    @code{protocol} -- the DND protocol
  @end{return}
  @begin{short}
    Finds the destination window and DND protocol to use at the given pointer
    position.
  @end{short}

  This function is called by the drag source to obtain the @arg{dest-window}
  and protocol parameters for the function @fun{gdk-drag-motion}.
  @see-function{gdk-drag-motion}"
  (with-foreign-objects ((dest-window :pointer) (protocol 'gdk-drag-protocol))
    (%gdk-drag-find-window-for-screen context
                                      window
                                      screen
                                      x-root
                                      y-root
                                      dest-window
                                      protocol)
    (values (mem-ref dest-window '(g-object gdk-window))
            (mem-ref protocol 'gdk-drag-protocol))))

(export 'gdk-drag-find-window-for-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_begin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_begin" gdk-drag-begin)
    (g-object gdk-drag-context :already-referenced)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[window]{the source window for this drag}
  @argument[targets]{the offered targets, as list of @symbol{gdk-atom}'s}
  @return{A newly created @class{gdk-drag-context} object.}
  @begin{short}
    Starts a drag and creates a new drag context for it.
  @end{short}
  This function assumes that the drag is controlled by the client pointer
  device, use the function @fun{gdk-drag-begin-for-device} to begin a drag with
  a different device.

  This function is called by the drag source.
  @see-function{gdk-drag-begin-for-device}"
  (window (g-object gdk-window))
  (targets (g-list gdk-atom-as-string)))

(export 'gdk-drag-begin)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_begin_for_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_begin_for_device" gdk-drag-begin-for-device)
    (g-object gdk-drag-context :already-referenced)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-12}
  @argument[window]{the source window for this drag}
  @argument[device]{the device that controls this drag}
  @argument[targets]{the offered targets, as list of @symbol{gdk-atom}'s}
  @return{A newly created @class{gdk-drag-context} object.}
  @short{Starts a drag and creates a new drag context for it.}

  This function is called by the drag source."
  (window (g-object gdk-window))
  (device (g-object gdk-device))
  (targets (g-list gdk-atom-as-string)))

(export 'gdk-drag-begin-for-device)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_begin_from_point ()
;;;
;;; GdkDragContext *
;;; gdk_drag_begin_from_point (GdkWindow *window,
;;;                            GdkDevice *device,
;;;                            GList *targets,
;;;                            gint x_root,
;;;                            gint y_root);
;;;
;;; Starts a drag and creates a new drag context for it.
;;;
;;; This function is called by the drag source.
;;;
;;; window :
;;;     the source window for this drag
;;;
;;; device :
;;;     the device that controls this drag
;;;
;;; targets :
;;;     the offered targets, as list of GdkAtoms.
;;;
;;; x_root :
;;;     the x coordinate where the drag nominally started
;;;
;;; y_root :
;;;     the y coordinate where the drag nominally started
;;;
;;; Returns :
;;;     a newly created GdkDragContext.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_motion ()
;;; ----------------------------------------------------------------------------

;; TODO: The return value gboolean is not documented.

(defcfun ("gdk_drag_motion" gdk-drag-motion) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-7}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[dest-window]{the new destination window, obtained by
    @fun{gdk-drag-find-window-for-screen}}
  @argument[protocol]{the DND protocol in use, obtained by
    @fun{gdk-drag-find-window-for-screen}}
  @argument[x-root]{the x position of the pointer in root coordinates}
  @argument[y-root]{the y position of the pointer in root coordinates}
  @argument[suggested-action]{the suggested action}
  @argument[possible-actions]{the possible actions}
  @argument[time]{the timestamp for this operation}
  @begin{short}
    Updates the drag context when the pointer moves or the set of actions
    changes.
  @end{short}

  This function is called by the drag source.
  @see-function{gdk-drag-find-window-for-screen}"
  (context (g-object gdk-drag-context))
  (dest-window (g-object gdk-window))
  (protocol gdk-drag-protocol)
  (x-root :int)
  (y-root :int)
  (suggested-action gdk-drag-action)
  (possible-actions gdk-drag-action)
  (time :uint32))

(export 'gdk-drag-motion)

;;; ----------------------------------------------------------------------------
;;; gdk_drop_finish ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drop_finish" gdk-drop-finish) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[success]{@em{true} if the data was successfully received}
  @argument[time]{the timestamp for this operation}
  @begin{short}
    Ends the drag operation after a drop.
  @end{short}

  This function is called by the drag destination.
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context))
  (success :boolean)
  (time :uint32))

(export 'gdk-drop-finish)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_status ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_status" gdk-drag-status) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[action]{the selected action which will be taken when a drop happens,
    or 0 to indicate that a drop will not be accepted}
  @argument[time]{the timestamp for this operation}
  @begin{short}
    Selects one of the actions offered by the drag source.
  @end{short}

  This function is called by the drag destination in response to the function
  @fun{gdk-drag-motion} called by the drag source.
  @see-function{gdk-drag-motion}"
  (context (g-object gdk-drag-context))
  (action gdk-drag-action)
  (time :uint32))

(export 'gdk-drag-status)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_drop_succeeded ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_drop_succeeded" gdk-drag-drop-succeeded) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{@em{True} if the drop was successful.}
  @begin{short}
    Returns whether the dropped data has been successfully transferred.
  @end{short}
  This function is intended to be used while handling a @code{:drop-finished}
  event, its return value is meaningless at other times.
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-drop-succeeded)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_drag_protocol ()
;;;
;;; GdkDragProtocol gdk_window_get_drag_protocol (GdkWindow *window,
;;;                                               GdkWindow **target);
;;;
;;; Finds out the DND protocol supported by a window.
;;;
;;; window :
;;;     the destination window
;;;
;;; target :
;;;     location of the window where the drop should happen. This may be window
;;;     or a proxy window, or NULL if window does not support Drag and Drop
;;;
;;; Returns :
;;;     the supported DND protocol.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_actions ()
;;;
;;; GdkDragAction gdk_drag_context_get_actions (GdkDragContext *context);
;;;
;;; Determines the bitmask of actions proposed by the source if
;;; gdk_drag_context_get_suggested_action() returns GDK_ACTION_ASK.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; Returns :
;;;     the GdkDragAction flags
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_suggested_action ()
;;;
;;; GdkDragAction gdk_drag_context_get_suggested_action
;;;                                                   (GdkDragContext *context);
;;;
;;; Determines the suggested drag action of the context.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; Returns :
;;;     a GdkDragAction value
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_selected_action ()
;;;
;;; GdkDragAction gdk_drag_context_get_selected_action (GdkDragContext *context)
;;;
;;; Determines the action chosen by the drag destination.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; Returns :
;;;     a GdkDragAction value
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_list_targets ()
;;;
;;; GList * gdk_drag_context_list_targets (GdkDragContext *context);
;;;
;;; Retrieves the list of targets of the context.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; Returns :
;;;     a GList of targets
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_device ()
;;;
;;; GdkDevice * gdk_drag_context_get_device (GdkDragContext *context);
;;;
;;; Returns the GdkDevice associated to the drag context.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; Returns :
;;;     The GdkDevice associated to context
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_set_device ()
;;;
;;; void gdk_drag_context_set_device (GdkDragContext *context,
;;;                                   GdkDevice *device);
;;;
;;; Associates a GdkDevice to context, so all Drag and Drop events for context
;;; are emitted as if they came from this device.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; device :
;;;     a GdkDevice
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_source_window ()
;;;
;;; GdkWindow * gdk_drag_context_get_source_window (GdkDragContext *context);
;;;
;;; Returns the GdkWindow where the DND operation started.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; Returns :
;;;     a GdkWindow
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_dest_window ()
;;;
;;; GdkWindow * gdk_drag_context_get_dest_window (GdkDragContext *context);
;;;
;;; Returns the destination windw for the DND operation.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; Returns :
;;;     a GdkWindow
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_protocol ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_context_get_protocol" gdk-drag-context-get-protocol)
    gdk-drag-protocol
 #+cl-cffi-gtk-documentation
 "@version{2013-11-19}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{The drag protocol of type @symbol{gdk-drag-protocol}.}
  @begin{short}
    Returns the drag protocol thats used by this context.
  @end{short}

  Since 3.0
  @see-class{gdk-drag-context}
  @see-symbol{gdk-drag-protocol}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-context-get-protocol)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_drag_window ()
;;;
;;; GdkWindow *
;;; gdk_drag_context_get_drag_window (GdkDragContext *context);
;;;
;;; Returns the window on which the drag icon should be rendered during the drag
;;; operation. Note that the window may not be available until the drag
;;; operation has begun. GDK will move the window in accordance with the ongoing
;;; drag operation. The window is owned by context and will be destroyed when
;;; the drag operation is over.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; Returns :
;;;     the drag window, or NULL.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_set_hotspot ()
;;;
;;; void
;;; gdk_drag_context_set_hotspot (GdkDragContext *context,
;;;                               gint hot_x,
;;;                               gint hot_y);
;;;
;;; Sets the position of the drag window that will be kept under the cursor
;;; hotspot. Initially, the hotspot is at the top left corner of the drag
;;; window.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; hot_x :
;;;     x coordinate of the drag window hotspot
;;;
;;; hot_y :
;;;     y coordinate of the drag window hotspot
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_manage_dnd ()
;;;
;;; gboolean
;;; gdk_drag_context_manage_dnd (GdkDragContext *context,
;;;                              GdkWindow *ipc_window,
;;;                              GdkDragAction actions);
;;;
;;; Requests the drag and drop operation to be managed by context . When a drag
;;; and drop operation becomes managed, the GdkDragContext will internally
;;; handle all input and source-side GdkEventDND events as required by the
;;; windowing system.
;;;
;;; Once the drag and drop operation is managed, the drag context will emit the
;;; following signals:
;;;
;;; The “action-changed” signal whenever the final action to be performed by the
;;; drag and drop operation changes.
;;;
;;; The “drop-performed” signal after the user performs the drag and drop
;;; gesture (typically by releasing the mouse button).
;;;
;;; The “dnd-finished” signal after the drag and drop operation concludes (after
;;; all GdkSelection transfers happen).
;;;
;;; The “cancel” signal if the drag and drop operation is finished but does not
;;; happen over an accepting destination, or is cancelled through other means.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; ipc_window :
;;;     Window to use for IPC messaging/events
;;;
;;; actions :
;;;     the actions supported by the drag source
;;;
;;; Returns :
;;;     TRUE if the drag and drop operation is managed.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.drag-and-drop.lisp -------------------------------------
