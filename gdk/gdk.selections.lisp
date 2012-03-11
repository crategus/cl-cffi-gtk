;;; ----------------------------------------------------------------------------
;;; gdk.selections.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; Version 2.24.10. See http://www.gtk.org.
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
;;;
;;; Selections
;;; 
;;; Functions for transfering data via the X selection mechanism
;;; 
;;; Synopsis
;;; 
;;;     GdkSelection;
;;;     GdkSelectionType;
;;;     GdkTarget;
;;;     GDK_SELECTION_PRIMARY
;;;     GDK_SELECTION_SECONDARY
;;;     GDK_SELECTION_CLIPBOARD
;;;     GDK_TARGET_BITMAP
;;;     GDK_TARGET_COLORMAP
;;;     GDK_TARGET_DRAWABLE
;;;     GDK_TARGET_PIXMAP
;;;     GDK_TARGET_STRING
;;;     GDK_SELECTION_TYPE_ATOM
;;;     GDK_SELECTION_TYPE_BITMAP
;;;     GDK_SELECTION_TYPE_COLORMAP
;;;     GDK_SELECTION_TYPE_DRAWABLE
;;;     GDK_SELECTION_TYPE_INTEGER
;;;     GDK_SELECTION_TYPE_PIXMAP
;;;     GDK_SELECTION_TYPE_WINDOW
;;;     GDK_SELECTION_TYPE_STRING
;;;     gdk_selection_owner_set
;;;     gdk_selection_owner_set_for_display
;;;     gdk_selection_owner_get
;;;     gdk_selection_owner_get_for_display
;;;     gdk_selection_convert
;;;     gdk_selection_property_get
;;;     gdk_selection_send_notify
;;;     gdk_selection_send_notify_for_display
;;; 
;;; Description
;;; 
;;; The X selection mechanism provides a way to transfer arbitrary chunks of
;;; data between programs. A selection is a essentially a named clipboard,
;;; identified by a string interned as a GdkAtom. By claiming ownership of a
;;; selection, an application indicates that it will be responsible for
;;; supplying its contents. The most common selections are PRIMARY and
;;; CLIPBOARD.
;;; 
;;; The contents of a selection can be represented in a number of formats,
;;; called targets. Each target is identified by an atom. A list of all possible
;;; targets supported by the selection owner can be retrieved by requesting the
;;; special target TARGETS. When a selection is retrieved, the data is
;;; accompanied by a type (an atom), and a format (an integer, representing the
;;; number of bits per item). See Properties and Atoms for more information.
;;; 
;;; The functions in this section only contain the lowlevel parts of the
;;; selection protocol. A considerably more complicated implementation is needed
;;; on top of this. GTK+ contains such an implementation in the functions in
;;; gtkselection.h and programmers should use those functions instead of the
;;; ones presented here. If you plan to implement selection handling directly
;;; on top of the functions here, you should refer to the X Inter-client
;;; Communication Conventions Manual (ICCCM).
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ---------------------------------------------------------------------------- 
;;; typedef GdkAtom GdkSelection;
;;; 
;;; Warning
;;; 
;;; GdkSelection is deprecated and should not be used in newly-written code.
;;; 
;;; The GdkSelection enumeration contains predefined atom values for several
;;; common selections.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkSelectionType
;;; 
;;; typedef GdkAtom GdkSelectionType;
;;; 
;;; Warning
;;; 
;;; GdkSelectionType is deprecated and should not be used in newly-written code.
;;; 
;;; The GdkSelectionType enumeration contains predefined atom values used to
;;; represent the types of data transferred in response to a request for a
;;; target. See the ICCCM for details about what data should be transferred for
;;; each of these types. Other atoms can be used, and the recommended practice
;;; for GTK+ is to to use mime types for this purpose. However, supporting these
;;; types may be useful for compatibility with older programs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkTarget
;;; 
;;; typedef GdkAtom GdkTarget;
;;; 
;;; Warning
;;; 
;;; GdkTarget is deprecated and should not be used in newly-written code.
;;; 
;;; The GdkTarget enumeration contains predefined atom values which are used to
;;; describe possible targets for a selection. Other atoms can be used, and the
;;; recommended practice for GTK+ is to to use mime types for this purpose.
;;; However, supporting these types may be useful for compatibility with older
;;; programs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_PRIMARY
;;; 
;;; #define GDK_SELECTION_PRIMARY         _GDK_MAKE_ATOM (1)
;;; 
;;; A GdkAtom representing the PRIMARY selection.
;;; ----------------------------------------------------------------------------

(defparameter *selection-primary* "PRIMARY")

(export '*selection-primary*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_SECONDARY
;;; 
;;; #define GDK_SELECTION_SECONDARY     _GDK_MAKE_ATOM (2)
;;; 
;;; A GdkAtom representing the SECONDARY selection.
;;; ----------------------------------------------------------------------------

(defparameter *selection-secondary* "SECONDARY")

(export '*selection-secondary*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_CLIPBOARD
;;; 
;;; #define GDK_SELECTION_CLIPBOARD     _GDK_MAKE_ATOM (69)
;;; 
;;; A GdkAtom representing the CLIPBOARD selection.
;;; ----------------------------------------------------------------------------

(defparameter *selection-clipboard* "CLIPBOARD")

(export '*selection-clipboard*)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_BITMAP
;;; 
;;; #define GDK_TARGET_BITMAP         _GDK_MAKE_ATOM (5)
;;; 
;;; A GdkAtom representing the BITMAP selection target.
;;; ----------------------------------------------------------------------------

(defparameter *target-bitmap* "BITMAP")
(export '*target-bitmap*)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_COLORMAP
;;; 
;;; #define GDK_TARGET_COLORMAP         _GDK_MAKE_ATOM (7)
;;; 
;;; A GdkAtom representing the COLORMAP selection target.
;;; ----------------------------------------------------------------------------

(defparameter *target-colormap* "COLORMAP")
(export '*target-colormap*)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_DRAWABLE
;;; 
;;; #define GDK_TARGET_DRAWABLE         _GDK_MAKE_ATOM (17)
;;; 
;;; A GdkAtom representing the DRAWABLE selection target.
;;; ----------------------------------------------------------------------------

(defparameter *target-drawable* "DRAWABLE")
(export '*target-drawable*)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_PIXMAP
;;; 
;;; #define GDK_TARGET_PIXMAP         _GDK_MAKE_ATOM (20)
;;; 
;;; A GdkAtom representing the PIXMAP selection target.
;;; ----------------------------------------------------------------------------

(defparameter *target-pixmap* "PIXMAP")

(export '*target-pixmap*)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_STRING
;;; 
;;; #define GDK_TARGET_STRING         _GDK_MAKE_ATOM (31)
;;; 
;;; A GdkAtom representing the STRING selection target.
;;; ----------------------------------------------------------------------------

(defparameter *target-string* "STRING")

(export '*target-string*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_ATOM
;;; 
;;; #define GDK_SELECTION_TYPE_ATOM     _GDK_MAKE_ATOM (4)
;;; 
;;; A GdkAtom representing the ATOM selection type.
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-atom* "ATOM")

(export '*selection-type-atom*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_BITMAP
;;; 
;;; #define GDK_SELECTION_TYPE_BITMAP     _GDK_MAKE_ATOM (5)
;;; 
;;; A GdkAtom representing the BITMAP selection type.
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-bitmap* "BITMAP")

(export '*selection-type-bitmap*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_COLORMAP
;;; 
;;; #define GDK_SELECTION_TYPE_COLORMAP     _GDK_MAKE_ATOM (7)
;;; 
;;; A GdkAtom representing the COLORMAP selection type.
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-colormap* "COLORMAP")

(export '*selection-type-colormap*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_DRAWABLE
;;; 
;;; #define GDK_SELECTION_TYPE_DRAWABLE     _GDK_MAKE_ATOM (17)
;;; 
;;; A GdkAtom representing the DRAWABLE selection type.
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-drawable* "DRAWABLE")

(export '*selection-type-drawable*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_INTEGER
;;; 
;;; #define GDK_SELECTION_TYPE_INTEGER     _GDK_MAKE_ATOM (19)
;;; 
;;; A GdkAtom representing the INTEGER selection type.
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-integer* "INTEGER")

(export '*selection-type-integer*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_PIXMAP
;;; 
;;; #define GDK_SELECTION_TYPE_PIXMAP     _GDK_MAKE_ATOM (20)
;;; 
;;; A GdkAtom representing the PIXMAP selection type.
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-pixmap* "PIXMAP")

(export '*selection-type-pixmap*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_WINDOW
;;; 
;;; #define GDK_SELECTION_TYPE_WINDOW     _GDK_MAKE_ATOM (33)
;;; 
;;; A GdkAtom representing the WINDOW selection type.
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-window* "WINDOW")

(export '*selection-type-window*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_STRING
;;; 
;;; #define GDK_SELECTION_TYPE_STRING     _GDK_MAKE_ATOM (31)
;;; 
;;; A GdkAtom representing the STRING selection type.
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-string* "STRING")

(export '*selection-type-string*)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_set ()
;;; 
;;; gboolean gdk_selection_owner_set (GdkWindow *owner,
;;;                                   GdkAtom selection,
;;;                                   guint32 time_,
;;;                                   gboolean send_event);
;;; 
;;; Sets the owner of the given selection.
;;; 
;;; owner :
;;;     a GdkWindow or NULL to indicate that the the owner for the given should
;;;     be unset.
;;; 
;;; selection :
;;;     an atom identifying a selection.
;;; 
;;; time_ :
;;;     timestamp to use when setting the selection. If this is older than the
;;;     timestamp given last time the owner was set for the given selection,
;;;     the request will be ignored.
;;; 
;;; send_event :
;;;     if TRUE, and the new owner is different from the current owner, the
;;;     current owner will be sent a SelectionClear event.
;;; 
;;; Returns :
;;;     TRUE if the selection owner was successfully changed to owner,
;;;     otherwise FALSE.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_set" gdk-selection-owner-set) :boolean
  (owner (g-object gdk-window))
  (selection gdk-atom-as-string)
  (time :uint32)
  (send-event :boolean))

(export 'gdk-selection-owner-set)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_set_for_display ()
;;; 
;;; gboolean gdk_selection_owner_set_for_display (GdkDisplay *display,
;;;                                               GdkWindow *owner,
;;;                                               GdkAtom selection,
;;;                                               guint32 time_,
;;;                                               gboolean send_event);
;;; 
;;; Sets the GdkWindow owner as the current owner of the selection selection.
;;; 
;;; display :
;;;     the GdkDisplay.
;;; 
;;; owner :
;;;     a GdkWindow or NULL to indicate that the owner for the given should
;;;     be unset.
;;; 
;;; selection :
;;;     an atom identifying a selection.
;;; 
;;; time_ :
;;;     timestamp to use when setting the selection. If this is older than the
;;;     timestamp given last time the owner was set for the given selection,
;;;     the request will be ignored.
;;; 
;;; send_event :
;;;     if TRUE, and the new owner is different from the current owner, the
;;;     current owner will be sent a SelectionClear event.
;;; 
;;; Returns :
;;;     TRUE if the selection owner was successfully changed to owner,
;;;     otherwise FALSE.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_set_for_display"
          gdk-selection-owner-set-for-display) :boolean
  (display (g-object gdk-display))
  (owner (g-object gdk-window))
  (selection gdk-atom-as-string)
  (time :uint32)
  (send-event :boolean))

(export 'gdk-selection-owner-set-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_get ()
;;; 
;;; GdkWindow * gdk_selection_owner_get (GdkAtom selection);
;;; 
;;; Determines the owner of the given selection.
;;; 
;;; selection :
;;;     an atom indentifying a selection.
;;; 
;;; Returns :
;;;     if there is a selection owner for this window, and it is a window known
;;;     to the current process, the GdkWindow that owns the selection, otherwise
;;;     NULL. Note that the return value may be owned by a different process if
;;;     a foreign window was previously created for that window, but a new
;;;     foreign window will never be created by this call.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_get" gdk-selection-owner-get)
    (g-object gdk-window)
  (selection gdk-atom-as-string))

(export 'gdk-selection-owner-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_get_for_display ()
;;; 
;;; GdkWindow * gdk_selection_owner_get_for_display (GdkDisplay *display,
;;;                                                  GdkAtom selection);
;;; 
;;; Determine the owner of the given selection.
;;; 
;;; Note that the return value may be owned by a different process if a foreign
;;; window was previously created for that window, but a new foreign window will
;;; never be created by this call.
;;; 
;;; display :
;;;     a GdkDisplay.
;;; 
;;; selection :
;;;     an atom indentifying a selection.
;;; 
;;; Returns :
;;;     if there is a selection owner for this window, and it is a window
;;;     known to the current process, the GdkWindow that owns the selection,
;;;     otherwise NULL.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_get_for_display"
          gdk-selection-owner-get-for-display) (g-object gdk-window)
  (display (g-object gdk-display))
  (selection gdk-atom-as-string))

(export 'gdk-selection-owner-get-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_convert ()
;;; 
;;; void gdk_selection_convert (GdkWindow *requestor,
;;;                             GdkAtom selection,
;;;                             GdkAtom target,
;;;                             guint32 time_);
;;; 
;;; Retrieves the contents of a selection in a given form.
;;; 
;;; requestor :
;;;     a GdkWindow.
;;; 
;;; selection :
;;;     an atom identifying the selection to get the contents of.
;;; 
;;; target :
;;;     the form in which to retrieve the selection.
;;; 
;;; time_ :
;;;     the timestamp to use when retrieving the selection. The selection owner
;;;     may refuse the request if it did not own the selection at the time
;;;     indicated by the timestamp.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_convert" gdk-selection-convert) :void
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-convert)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_property_get ()
;;; 
;;; gint gdk_selection_property_get (GdkWindow *requestor,
;;;                                  guchar **data,
;;;                                  GdkAtom *prop_type,
;;;                                  gint *prop_format);
;;; 
;;; Retrieves selection data that was stored by the selection data in response
;;; to a call to gdk_selection_convert(). This function will not be used by
;;; applications, who should use the GtkClipboard API instead.
;;; 
;;; requestor :
;;;     the window on which the data is stored
;;; 
;;; data :
;;;     location to store a pointer to the retrieved data. If the retrieval
;;;     failed, NULL we be stored here, otherwise, it will be non-NULL and the
;;;     returned data should be freed with g_free() when you are finished using
;;;     it. The length of the allocated memory is one more than the length of
;;;     the returned data, and the final byte will always be zero, to ensure
;;;     nul-termination of strings.
;;; 
;;; prop_type :
;;;     location to store the type of the property.
;;; 
;;; prop_format :
;;;     location to store the format of the property.
;;; 
;;; Returns :
;;;     the length of the retrieved data.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_property_get" gdk-selection-property-get) :int
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-property-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_send_notify ()
;;; 
;;; void gdk_selection_send_notify (GdkNativeWindow requestor,
;;;                                 GdkAtom selection,
;;;                                 GdkAtom target,
;;;                                 GdkAtom property,
;;;                                 guint32 time_);
;;; 
;;; Sends a response to SelectionRequest event.
;;; 
;;; requestor :
;;;     window to which to deliver response.
;;; 
;;; selection :
;;;     selection that was requested.
;;; 
;;; target :
;;;     target that was selected.
;;; 
;;; property :
;;;     property in which the selection owner stored the data, or GDK_NONE
;;;     to indicate that the request was rejected.
;;; 
;;; time_ :
;;;     timestamp.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_send_notify" gdk-selection-send-notify) :void
  (requestor gdk-native-window)
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (property gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-send-notify)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_send_notify_for_display ()
;;; 
;;; void gdk_selection_send_notify_for_display (GdkDisplay *display,
;;;                                             GdkNativeWindow requestor,
;;;                                             GdkAtom selection,
;;;                                             GdkAtom target,
;;;                                             GdkAtom property,
;;;                                             guint32 time_);
;;; 
;;; Send a response to SelectionRequest event.
;;; 
;;; display :
;;;     the GdkDisplay where requestor is realized
;;; 
;;; requestor :
;;;     window to which to deliver response.
;;; 
;;; selection :
;;;     selection that was requested.
;;; 
;;; target :
;;;     target that was selected.
;;; 
;;; property :
;;;     property in which the selection owner stored the data, or GDK_NONE to
;;;     indicate that the request was rejected.
;;; 
;;; time_ :
;;;     timestamp.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_send_notify_for_display"
          gdk-selection-send-notify-for-display) :void
  (display (g-object gdk-display))
  (requestor gdk-native-window)
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (property gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-send-notify-for-display)

;;; --- End of file gdk.selections.lisp ----------------------------------------
