;;; ----------------------------------------------------------------------------
;;; gdk.drag-and-drop.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
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
;;;     void    action-changed    Run Last
;;;     void    cancel            Run Last
;;;     void    dnd-finished      Run Last
;;;     void    drop-performed    Run Last
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
(setf (gethash 'gdk-drag-cancel-reason atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gdk-drag-cancel-reason atdoc:*external-symbols*)
 "@version{2021-10-3}
  @begin{short}
    Used in the @class{gdk-drag-context} object to indicate the reason of a
    cancelled DND operation.
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
(setf (gethash 'gdk-drag-protocol atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gdk-drag-protocol atdoc:*external-symbols*)
 "@version{2021-10-3}
  @begin{short}
    Used in the @class{gdk-drag-context} object to indicate the protocol
    according to which DND is done.
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
    @entry[:win32-dropfiles]{The simple WM_DROPFILES protocol.}
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
  (:default #.(ash 1 0))
  (:copy #.(ash 1 1))
  (:move #.(ash 1 2))
  (:link #.(ash 1 3))
  (:private #.(ash 1 4))
  (:ask #.(ash 1 5)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-drag-action atdoc:*symbol-name-alias*)
      "GFlags"
      (gethash 'gdk-drag-action atdoc:*external-symbols*)
 "@version{2021-10-3}
  @begin{short}
    Used in the @class{gdk-drag-context} object to indicate what the
    destination should do with the dropped data.
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
 "@version{2021-10-3}
  @begin{short}
    The @class{gdk-drag-context} class and the correspondig functions provide a
    low level interface for drag and drop.
  @end{short}
  The X backend of GDK supports both the Xdnd and Motif drag and drop protocols
  transparently, the Win32 backend supports the WM_DROPFILES protocol.

  GTK provides a higher level abstraction based on top of these functions,
  and so they are not normally needed in GTK applications. See the Drag and
  Drop section of the GTK documentation for more information.
  @begin[Signal Details]{dictionary}
    @subheading{The \"action-changed\" signal}
    @begin{pre}
 lambda (context action)    :run-last
    @end{pre}
    A new action is being chosen for the drag and drop operation. The signal
    will only be emitted if the @sym{gtk-drag-context} object manages the drag
    and drop operation. See the @fun{gdk-drag-context-manage-dnd} function for
    more information. Since 3.20
    @begin[code]{table}
      @entry[context]{The @sym{gdk-drag-context} object on which the signal is
        emitted.}
      @entry[action]{The @symbol{gdk-drag-action} value currently chosen.}
    @end{table}
    @subheading{The \"cancel\" signal}
    @begin{pre}
 lambda (context reason)    :run-last
    @end{pre}
    The drag and drop operation was cancelled. The signal will only be emitted
    if the @sym{gdk-drag-context} object manages the drag and drop operation.
    See the @fun{gdk-drag-context-manage-dnd} function for more information.
    Since 3.20
    @begin[code]{table}
      @entry[context]{The @sym{gdk-drag-context} object on which the signal is
        emitted.}
      @entry[reason]{The @symbol{gdk-drag-cancel-reason} value the drag context
        was cancelled.}
    @end{table}
    @subheading{The \"dnd-finished\" signal}
    @begin{pre}
 lambda (context)    :run-last
    @end{pre}
    The drag and drop operation was finished, the drag destination finished
    reading all data. The drag source can now free all miscellaneous data. This
    signal will only be emitted if the @sym{gdk-drag-context} manages the
    drag and drop operation. See the @fun{gdk-drag-context-manage-dnd} function
    for more information. Since 3.20
    @begin[code]{table}
      @entry[context]{The @sym{gdk-drag-context} object on which the signal is
        emitted.}
    @end{table}
    @subheading{The \"drop-performed\" signal}
    @begin{pre}
 lambda (context time)    :run-last
    @end{pre}
    The drag and drop operation was performed on an accepting client. This
    signal will only be emitted if the @sym{gdk-drag-context} manages the
    drag and drop operation. See the @fun{gdk-drag-context-manage-dnd} function
    for more information. Since 3.20
    @begin[code]{table}
      @entry[context]{The @sym{gdk-drag-context} object on which the signal is
        emitted.}
      @entry[time]{An integer with the time at which the drop happened.}
    @end{table}
  @end{dictionary}
  @see-symbol{gdk-drag-cancel-reason}
  @see-symbol{gdk-drag-protocol}
  @see-symbol{gdk-drag-action}
  @see-function{gdk-drag-context-manage-dnd}")

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_selection () -> gdk-drag-selection
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_get_selection" gdk-drag-selection) gdk-atom-as-string
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{A @symbol{gdk-atom} as a string for the selection, or \"NONE\".}
  @begin{short}
    Returns the selection atom for the current source window.
  @end{short}
  @see-class{gdk-drag-context}
  @see-symbol{gdk-atom}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-selection)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_abort ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_abort" gdk-drag-abort) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[time]{an unsigned integer with the timestamp for this operation}
  @begin{short}
    Aborts a drag without dropping.
  @end{short}
  This function is called by the drag source.
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context))
  (time :uint32))

(export 'gdk-drag-abort)

;;; ----------------------------------------------------------------------------
;;; gdk_drop_reply ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drop_reply" gdk-drop-reply) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[accepted]{@em{true} if the drop is accepted}
  @argument[time]{an unsigned integer with the timestamp for this operation}
  @begin{short}
    Accepts or rejects a drop.
  @end{short}
  This function is called by the drag destination in response to a drop
  initiated by the drag source.
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context))
  (accepted :boolean)
  (time :uint32))

(export 'gdk-drop-reply)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_drop ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_drop" gdk-drag-drop) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[time]{an unsigned integer with the timestamp for this operation}
  @begin{short}
    Drops on the current destination.
  @end{short}
  This function is called by the drag source.
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context))
  (time :uint32))

(export 'gdk-drag-drop)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_drop_done ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun ("gdk_drag_drop_done" gdk-drag-drop-done) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[sucess]{a boolean whether the drag was ultimatively succesful}
  @begin{short}
    Inform GDK if the drop ended successfully.
  @end{short}
  Passing @em{false} for @@arg{success 0 may trigger a drag cancellation
  animation. This function is called by the drag source, and should be the last
  call before dropping the reference to the drag context. The
  @class{gdk-drag-context} object will only take the first
  @sym{gdk-drag-drop-done} function call as effective, if this function is
  called multiple times, all subsequent calls will be ignored.

  Since 3.20
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context))
  (success :boolean))

#+gdk-3-20
(export 'gdk-drag-drop-done)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_find_window_for_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_find_window_for_screen" %gdk-drag-find-window-for-screen)
    :void
  (context (g-object gdk-drag-context))
  (window (g-object gdk-window))
  (screen (g-object gdk-screen))
  (x :int)
  (y :int)
  (dest (:pointer (g-object gdk-window)))
  (protocol (:pointer gdk-drag-protocol)))

(defun gdk-drag-find-window-for-screen (context window screen x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[window]{a @class{gdk-window} object which may be at the pointer
    position, but should be ignored, since it is put up by the drag source as
    an icon}
  @argument[screen]{the @class{gdk-screen} object where the destination window
    is sought}
  @argument[x]{an integer with the x position of the pointer in root
    coordinates}
  @argument[y]{an integer with the y position of the pointer in root
    coordinates}
  @begin{return}
    @code{dest} -- the  @class{gdk-window} destination window @br{}
    @code{protocol} -- the @symbol{gdk-drag-protocol} DND protocol
  @end{return}
  @begin{short}
    Finds the destination window and DND protocol to use at the given pointer
    position.
  @end{short}
  This function is called by the drag source to obtain the destination window
  and protocol parameters for the @fun{gdk-drag-motion} function.
  @see-class{gdk-drag-context}
  @see-class{gdk-window}
  @see-class{gdk-screen}
  @see-symbol{gdk-drag-protocol}
  @see-function{gdk-drag-motion}"
  (with-foreign-objects ((dest :pointer) (protocol 'gdk-drag-protocol))
    (%gdk-drag-find-window-for-screen context
                                      window
                                      screen
                                      x
                                      y
                                      dest
                                      protocol)
    (values (mem-ref dest '(g-object gdk-window))
            (mem-ref protocol 'gdk-drag-protocol))))

(export 'gdk-drag-find-window-for-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_begin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_begin" %gdk-drag-begin)
    (g-object gdk-drag-context :already-referenced)
  (window (g-object gdk-window))
  (targets (g-list :pointer)))

(defun gdk-drag-begin (window targets)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[window]{a @class{gdk-window} source window for this drag}
  @argument[targets]{a list of @symbol{gdk-atom} as strings with the offered
    targets}
  @return{A newly created @class{gdk-drag-context} object.}
  @begin{short}
    Starts a drag and creates a new drag context for it.
  @end{short}
  This function assumes that the drag is controlled by the client pointer
  device, use the @fun{gdk-drag-begin-for-device} function to begin a drag with
  a different device. This function is called by the drag source.
  @see-class{gdk-drag-context}
  @see-class{gdk-window}
  @see-symbol{gdk-atom}
  @see-function{gdk-drag-begin-for-device}"
  (%gdk-drag-begin window
                   (mapcar #'gdk-atom-intern targets)))

(export 'gdk-drag-begin)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_begin_for_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_begin_for_device" %gdk-drag-begin-for-device)
    (g-object gdk-drag-context :already-referenced)
  (window (g-object gdk-window))
  (device (g-object gdk-device))
  (targets (g-list :pointer)))

(defun gdk-drag-begin-for-device (window device targets)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[window]{a @class{gdk-window} source window for this drag}
  @argument[device]{a @class{gdk-device} object that controls this drag}
  @argument[targets]{a list of @symbol{gdk-atom} as strings with the offered
    targets}
  @return{A newly created @class{gdk-drag-context} object.}
  @begin{short}
    Starts a drag and creates a new drag context for it.
  @end{short}
  This function is called by the drag source.
  @see-class{gdk-drag-context}
  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-symbol{gdk-atom}"
  (%gdk-drag-begin-for-device window
                              device
                              (mapcar #'gdk-atom-intern targets)))

(export 'gdk-drag-begin-for-device)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_begin_from_point ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun ("gdk_drag_begin_from_point" %gdk-drag-begin-from-point)
    (g-object gdk-drag-context)
  (window (g-object gdk-window))
  (device (g-object gdk-device))
  (targets (g-list :pointer))
  (x :int)
  (y :int))

#+gdk-3-20
(defun gdk-drag-begin-from-point (window device targets x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[window]{a @class{gdk-window} source window for this drag}
  @argument[device]{a @class{gdk-device} object that controls this drag}
  @argument[targets]{a list of @symbol{gdk-atom} as strings with the offered
    targets}
  @argument[x]{an integer with the x coordinate where the drag nominally
    started}
  @argument[y]{an integer with the y coordinate where the drag nominally
    started}
  @return{A newly created @class{gdk-drag-context} object.}
  @begin{short}
    Starts a drag and creates a new drag context for it.
  @end{short}
  This function is called by the drag source.

  Since 3.20
  @see-class{gdk-drag-context}
  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-symbol{gdk-atom}"
  (%gdk-drag-begin-from-point window
                              device
                              (mapcar #'gdk-atom-intern targets)
                              x
                              y))

#+gdk-3-20
(export 'gdk-drag-begin-from-point)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_motion ()
;;; ----------------------------------------------------------------------------

;; TODO: The return value gboolean is not documented.

(defcfun ("gdk_drag_motion" gdk-drag-motion) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[window]{a new @class{gdk-window} destination window, obtained by
    the @fun{gdk-drag-find-window-for-screen} function}
  @argument[protocol]{a @symbol{gdk-drag-protocol} DND protocol in use,
    obtained by the @fun{gdk-drag-find-window-for-screen} function}
  @argument[x]{an integer with the x position of the pointer in root
    coordinates}
  @argument[y]{an integer with the the y position of the pointer in root
    coordinates}
  @argument[suggested]{a @symbol{gdk-drag-action} value with the suggested
    action}
  @argument[possible]{a @symbol{gdk-drag-action} value with the possible
    actions}
  @argument[time]{an unsigned integer with the timestamp for this operation}
  @begin{short}
    Updates the drag context when the pointer moves or the set of actions
    changes.
  @end{short}
  This function is called by the drag source.
  @see-class{gdk-drag-context}
  @see-class{gdk-window}
  @see-symbol{gdk-drag-protocol}
  @see-symbol{gdk-drag-action}
  @see-function{gdk-drag-find-window-for-screen}"
  (context (g-object gdk-drag-context))
  (window (g-object gdk-window))
  (protocol gdk-drag-protocol)
  (x :int)
  (y :int)
  (suggested gdk-drag-action)
  (possible gdk-drag-action)
  (time :uint32))

(export 'gdk-drag-motion)

;;; ----------------------------------------------------------------------------
;;; gdk_drop_finish ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drop_finish" gdk-drop-finish) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[success]{@em{true} if the data was successfully received}
  @argument[time]{an unsigned integer with the timestamp for this operation}
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
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[action]{a selected @symbol{gdk-drag-action} value which will be
    taken when a drop happens, or 0 to indicate that a drop will not be
    accepted}
  @argument[time]{an integer with the timestamp for this operation}
  @begin{short}
    Selects one of the actions offered by the drag source.
  @end{short}
  This function is called by the drag destination in response to the
  @fun{gdk-drag-motion} function called by the drag source.
  @see-class{gdk-drag-context}
  @see-symbol{gdk-drag-action}
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
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{@em{True} if the drop was successful.}
  @begin{short}
    Returns whether the dropped data has been successfully transferred.
  @end{short}
  This function is intended to be used while handling a @code{:drop-finished}
  event, its return value is meaningless at other times.
  @see-class{gdk-drag-context}
  @see-class{gdk-event-dnd}
  @see-symbol{gdk-event-type}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-drop-succeeded)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_drag_protocol ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdK_window_get_drag_protocol" gdk-window-drag-protocol)
    gdk-drag-protocol
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[window]{a @class{gdk-window} destination window}
  @argument[target]{a @class{gdk-window} object where the drop should happen,
    this may be @arg{window} or a proxy window, or @code{nil} if @arg{window}
    does not support drag and drop}
  @return{The supported @symbol{gdk-drag-protocol} DND protocol.}
  @begin{short}
    Finds out the DND protocol supported by a window.
  @end{short}
  @see-class{gdk-drag-context}
  @see-class{gdk-window}
  @see-symbol{gdk-drag-protocol}"
  (window (g-object gdk-window))
  (target (g-object gdk-window)))

(export 'gdk-window-drag-protocol)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_actions () -> gdk-drag-context-actions
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_context_get_actions" gdk-drag-context-actions)
    gdk-drag-action
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{The @symbol{gdk-drag-action} flags.}
  @begin{short}
    Determines the bitmask of actions proposed by the source if the
    @fun{gdk-drag-context-suggested-action} function returns the @code{:ask}
    value.
  @end{short}
  @see-class{gdk-drag-context}
  @see-symbol{gdk-drag-action}
  @see-function{gdk-drag-context-suggest-action}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-context-actions)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_suggested_action () -> gdk-drag-context-suggest-action
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_context_get_suggested_action"
          gdk-drag-context-suggested-action) gdk-drag-action
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{The @symbol{gdk-drag-action} flags.}
  @begin{short}
    Determines the suggested drag action of the drag context.
  @end{short}
  @see-class{gdk-drag-context}
  @see-symbol{gdk-drag-action}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-context-suggested-action)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_selected_action () -> gdk-drag-context-selected-action
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_context_get_selected_action"
           gdk-drag-context-selected-action) gdk-drag-action
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{The @symbol{gdk-drag-action} flags.}
  @begin{short}
    Determines the action chosen by the drag destination.
  @end{short}
  @see-class{gdk-drag-context}
  @see-symbol{gdk-drag-action}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-context-selected-action)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_list_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_context_list_targets" gdk-drag-context-list-targets)
    (g-list gdk-atom-as-string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{A list of @symbol{gdk-atom} as strings.}
  @begin{short}
    Retrieves the list of targets of the drag context.
  @end{short}
  @see-class{gdk-drag-context}
  @see-symbol{gdk-atom}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-context-list-targets)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_device ()
;;; gdk_drag_context_set_device () -> gdk-drag-context-device
;;; ----------------------------------------------------------------------------

(defun (setf gdk-drag-context-device) (device context)
  (foreign-funcall "gdk_drag_context_set_device"
                   (g-object gdk-drag-context) context
                   (g-object gdk-device) device
                   :void)
  device)

(defcfun ("gdk_drag_context_get_device" gdk-drag-context-device)
    (g-object gdk-device)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @syntax[]{(gdk-drag-context-device context) => device}
  @syntax[]{(setf (gdk-drag-context-device context) device)}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[device]{a @class{gdk-device} object associated to @arg{context}}
  @begin{short}
    Associates a device to the drag context, so all drag and drop events for
    the drag context are emitted as if they came from this device.
  @end{short}
  @see-class{gdk-drag-context}
  @see-class{gdk-device}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-context-device)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_source_window () -> gdk-drag-context-source-window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_context_get_source_window" gdk-drag-context-source-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{A @class{gdk-window} object.}
  @begin{short}
    Returns the window where the DND operation started.
  @end{short}
  @see-class{gdk-drag-context}
  @see-class{gdk-window}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-context-source-window)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_dest_window () -> gdk-drag-context-dest-window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_context_get_dest_window" gdk-drag-context-dest-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{A @class{gdk-window} object.}
  @begin{short}
    Returns the destination windw for the DND operation.
  @end{short}
  @see-class{gdk-drag-context}
  @see-class{gdk-window}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-context-dest-window)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_protocol () -> gdk-drag-context-protocol
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_context_get_protocol" gdk-drag-context-protocol)
    gdk-drag-protocol
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{The drag protocol of type @symbol{gdk-drag-protocol}.}
  @begin{short}
    Returns the drag protocol that is used by this drag context.
  @end{short}
  @see-class{gdk-drag-context}
  @see-symbol{gdk-drag-protocol}"
  (context (g-object gdk-drag-context)))

(export 'gdk-drag-context-protocol)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_drag_window () -> gdk-drag-context-drag-window
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun ("gdk_drag_context_get_drag_window" gdk-drag-context-drag-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @return{The @class{gdk-window} drag window, or @code{nil}.}
  @begin{short}
    Returns the window on which the drag icon should be rendered during the
    drag operation.
  @end{short}
  Note that the window may not be available until the drag operation has begun.
  GDK will move the window in accordance with the ongoing drag operation. The
  window is owned by context and will be destroyed when the drag operation is
  over.

  Since 3.20
  @see-class{gdk-drag-context}
  @see-class{gdk-window}"
  (context (g-object gdk-drag-context)))

#+gdk-3-20
(export 'gdk-drag-context-drag-window)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_set_hotspot ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun ("gdk_drag_context_set_hotspot" gdk-drag-context-set-hotspot) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[x]{an integer with the x coordinate of the drag window hotspot}
  @argument[y]{an integer with the y coordinate of the drag window hotspot}
  @begin{short}
    Sets the position of the drag window that will be kept under the cursor
    hotspot.
  @end{short}
  Initially, the hotspot is at the top left corner of the drag window.

  Since 3.20
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context))
  (x :int)
  (y :int))

#+gdk-3-20
(export 'gdk-drag-context-set-hotspot)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_manage_dnd ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun ("gdk_drag_context_manage_dnd" gdk-drag-context-manage-dnd) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[window]{a @class{gdk-window} object to use for IPC messaging/events}
  @argument[actions]{the @symbol{gdk-drag-action} flags supported by the drag
    source}
  @return{@em{True} if the drag and drop operation is managed.}
  @begin{short}
    Requests the drag and drop operation to be managed by @arg{context}.
  @end{short}
  When a drag and drop operation becomes managed, the @class{gdk-drag-context}
  object will internally handle all input and source-side @class{gdk-event-dnd}
  events as required by the windowing system.

  Once the drag and drop operation is managed, the drag context will emit the
  following signals:
  @begin{itemize}
    @begin{item}
      The \"action-changed\" signal whenever the final action to be performed
      by the drag and drop operation changes.
    @end{item}
    @begin{item}
      The \"drop-performed\" signal after the user performs the drag and drop
      gesture, typically by releasing the mouse button.
    @end{item}
    @begin{item}
      The \"dnd-finished\" signal after the drag and drop operation concludes,
      after all GDK selection transfers happen.
    @end{item}
    @begin{item}
      The \"cancel\" signal if the drag and drop operation is finished but does
      not happen over an accepting destination, or is cancelled through other
      means.
    @end{item}
  @end{itemize}
  Since 3.20
  @see-class{gdk-drag-context}
  @see-class{gdk-window}
  @see-symbol{gdk-drag-action}
  @see-class{gdk-event-dnd}"
  (context (g-object gdk-drag-context))
  (window (g-object gdk-window))
  (actions gdk-drag-action))

#+gdk-3-20
(export 'gdk-drag-context-manage-dnd)

;;; --- End of file gdk.drag-and-drop.lisp -------------------------------------
