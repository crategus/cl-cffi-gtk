;;; ----------------------------------------------------------------------------
;;; gdk.drag-and-drop.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; Functions for controlling drag and drop handling
;;;
;;; Synopsis
;;;
;;;     GdkDragProtocol
;;;     GdkDragAction
;;;     GdkDragContext
;;;
;;;     gdk_drag_get_selection
;;;     gdk_drag_abort
;;;     gdk_drop_reply
;;;     gdk_drag_drop
;;;     gdk_drag_find_window_for_screen
;;;     gdk_drag_begin
;;;     gdk_drag_begin_for_device
;;;     gdk_drag_motion
;;;     gdk_drop_finish
;;;
;;;     gdk_drag_status
;;;     gdk_drag_drop_succeeded
;;;     gdk_window_get_drag_protocol
;;;     gdk_drag_context_get_actions
;;;     gdk_drag_context_get_suggested_action
;;;     gdk_drag_context_get_selected_action
;;;     gdk_drag_context_list_targets
;;;     gdk_drag_context_get_device
;;;     gdk_drag_context_set_device
;;;     gdk_drag_context_get_source_window
;;;     gdk_drag_context_get_dest_window
;;;     gdk_drag_context_get_protocol
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GdkDragContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

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
 "@version{2013-6-30}
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
    @entry{:rootwin]{An extension to the Xdnd protocol for unclaimed root
      window drops.}
    @entry[:win32-dropfiles]{The simple @code{WM_DROPFILES} protocol.}
    @entry[:ole2]{The complex OLE2 DND protocol (not implemented).}
    @entry[:local]{Intra-application DND.}
  @end{table}")

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
    @entry[:move]{Move the data, i. e. first copy it, then delete it from the
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
    These functions provide a low level interface for drag and drop. The X
    backend of GDK supports both the Xdnd and Motif drag and drop protocols
    transparently, the Win32 backend supports the @code{WM_DROPFILES} protocol.
  @end{short}

  GTK+ provides a higher level abstraction based on top of these functions,
  and so they are not normally needed in GTK+ applications. See the Drag and
  Drop section of the GTK+ documentation for more information.")

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

  Since 2.2
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

  Since 2.6"
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
;;;
;;; GdkDragProtocol gdk_drag_context_get_protocol (GdkDragContext *context);
;;;
;;; Returns the drag protocol thats used by this context.
;;;
;;; context :
;;;     a GdkDragContext
;;;
;;; Returns :
;;;     the drag protocol
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.drag-and-drop.lisp -------------------------------------
