;;; ----------------------------------------------------------------------------
;;; gdk.selections.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
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
;;;
;;; Selections
;;;
;;; Functions for transfering data via the X selection mechanism
;;;
;;; Synopsis
;;;
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
;;;
;;;     gdk_selection_owner_set
;;;     gdk_selection_owner_set_for_display
;;;     gdk_selection_owner_get
;;;     gdk_selection_owner_get_for_display
;;;     gdk_selection_convert
;;;     gdk_selection_property_get
;;;     gdk_selection_send_notify
;;;     gdk_selection_send_notify_for_display
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_PRIMARY
;;; ----------------------------------------------------------------------------

(defparameter *selection-primary* "PRIMARY"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"PRIMARY\"}
  @short{A GdkAtom representing the PRIMARY selection.}")

(export '*selection-primary*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_SECONDARY
;;; ----------------------------------------------------------------------------

(defparameter *selection-secondary* "SECONDARY"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"SECONDARY\"}
  @short{A GdkAtom representing the \"SECONDARY\" selection.}")

(export '*selection-secondary*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_CLIPBOARD
;;; ----------------------------------------------------------------------------

(defparameter *selection-clipboard* "CLIPBOARD"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"CLIPBOARD\"}
  @short{A GdkAtom representing the CLIPBOARD selection.}")

(export '*selection-clipboard*)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_BITMAP
;;; ----------------------------------------------------------------------------

(defparameter *target-bitmap* "BITMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"BITMAP\"}
  @short{A GdkAtom representing the BITMAP selection target.}")

(export '*target-bitmap*)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_COLORMAP
;;; ----------------------------------------------------------------------------

(defparameter *target-colormap* "COLORMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"COLORMAP\"}
  @short{A GdkAtom representing the COLORMAP selection target.}")

(export '*target-colormap*)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_DRAWABLE
;;; ----------------------------------------------------------------------------

(defparameter *target-drawable* "DRAWABLE"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"DRAWABLE\"}
  @short{A GdkAtom representing the DRAWABLE selection target.}")

(export '*target-drawable*)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_PIXMAP
;;; ----------------------------------------------------------------------------

(defparameter *target-pixmap* "PIXMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"PIXMAP\"}
  @short{A GdkAtom representing the PIXMAP selection target.}")

(export '*target-pixmap*)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_STRING
;;; ----------------------------------------------------------------------------

(defparameter *target-string* "STRING"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"STRING\"}
  @short{A GdkAtom representing the STRING selection target.}")

(export '*target-string*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_ATOM
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-atom* "ATOM"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"ATOM\"}
  @short{A GdkAtom representing the ATOM selection type.}")

(export '*selection-type-atom*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_BITMAP
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-bitmap* "BITMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"BITMAP\"}
  @short{A GdkAtom representing the BITMAP selection type.}")

(export '*selection-type-bitmap*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_COLORMAP
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-colormap* "COLORMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"COLORMAP\"}
  @short{A GdkAtom representing the COLORMAP selection type.}")

(export '*selection-type-colormap*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_DRAWABLE
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-drawable* "DRAWABLE"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"DRAWABLE\"}
  @short{A GdkAtom representing the DRAWABLE selection type.}")

(export '*selection-type-drawable*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_INTEGER
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-integer* "INTEGER"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"INTEGER\"}
  @short{A GdkAtom representing the INTEGER selection type.}")

(export '*selection-type-integer*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_PIXMAP
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-pixmap* "PIXMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"PIXMAP\"}
  @short{A GdkAtom representing the PIXMAP selection type.}")

(export '*selection-type-pixmap*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_WINDOW
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-window* "WINDOW"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"WINDOW\"}
  @short{A GdkAtom representing the WINDOW selection type.}")

(export '*selection-type-window*)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_STRING
;;; ----------------------------------------------------------------------------

(defparameter *selection-type-string* "STRING"
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @variable-value{\"WINDOW\"}
  @short{A GdkAtom representing the STRING selection type.}")

(export '*selection-type-string*)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_set" gdk-selection-owner-set) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @argument[owner]{a @class{gdk-window} or @code{nil} to indicate that the the
    owner for the given should be unset.}
  @argument[selection]{an atom identifying a selection.}
  @argument[time]{timestamp to use when setting the selection. If this is older
    than the timestamp given last time the owner was set for the given
    selection, the request will be ignored.}
  @argument[send-event]{if @arg{true}, and the new owner is different from the
    current owner, the current owner will be sent a @code{SelectionClear}
    event.}
  @return{@arg{true} if the selection owner was successfully changed to owner,
    otherwise @arg{nil}.}
  @short{Sets the owner of the given selection.}"
  (owner (g-object gdk-window))
  (selection gdk-atom-as-string)
  (time :uint32)
  (send-event :boolean))

(export 'gdk-selection-owner-set)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_set_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_set_for_display"
          gdk-selection-owner-set-for-display) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @argument[display]{the GdkDisplay}
  @argument[owner]{a GdkWindow or NULL to indicate that the owner for the given
    should be unset}
  @argument[selection]{an atom identifying a selection}
  @argument[time_]{timestamp to use when setting the selection If this is older
    than the timestamp given last time the owner was set for the given
    selection, the request will be ignored}
  @argument[send_event]{if TRUE, and the new owner is different from the current
    owner, the current owner will be sent a SelectionClear event}
  @return{TRUE if the selection owner was successfully changed to owner,
    otherwise FALSE.}
  @short{Sets the GdkWindow owner as the current owner of the selection
    selection.}

  Since 2.2"
  (display (g-object gdk-display))
  (owner (g-object gdk-window))
  (selection gdk-atom-as-string)
  (time :uint32)
  (send-event :boolean))

(export 'gdk-selection-owner-set-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_get" gdk-selection-owner-get)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @argument[selection]{an atom indentifying a selection.}
  @return{if there is a selection owner for this window, and it is a window
    known to the current process, the GdkWindow that owns the selection,
    otherwise NULL. Note that the return value may be owned by a different
    process if a foreign window was previously created for that window, but a
    new foreign window will never be created by this call.}
  @short{Determines the owner of the given selection.}"
  (selection gdk-atom-as-string))

(export 'gdk-selection-owner-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_get_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_get_for_display"
          gdk-selection-owner-get-for-display) (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @argument[display]{a GdkDisplay}
  @argument[selection]{an atom indentifying a selection}
  @return{if there is a selection owner for this window, and it is a window
    known to the current process, the GdkWindow that owns the selection,
    otherwise NULL.}
  @short{Determine the owner of the given selection.}

  Note that the return value may be owned by a different process if a foreign
  window was previously created for that window, but a new foreign window will
  never be created by this call.

  Since 2.2"
  (display (g-object gdk-display))
  (selection gdk-atom-as-string))

(export 'gdk-selection-owner-get-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_convert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_convert" gdk-selection-convert) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @argument[requestor]{a GdkWindow.}
  @argument[selection]{an atom identifying the selection to get the contents
    of.}
  @argument[target]{the form in which to retrieve the selection.}
  @argument[time_]{the timestamp to use when retrieving the selection. The
    selection owner may refuse the request if it did not own the selection at
    the time indicated by the timestamp.}
  @short{Retrieves the contents of a selection in a given form.}"
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-convert)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_property_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_property_get" gdk-selection-property-get) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @argument[requestor]{the window on which the data is stored}
  @argument[data]{location to store a pointer to the retrieved data. If the
    retrieval failed, NULL we be stored here, otherwise, it will be non-NULL and
    the returned data should be freed with g_free() when you are finished using
    it. The length of the allocated memory is one more than the length of
    the returned data, and the final byte will always be zero, to ensure
    nul-termination of strings}
  @argument[prop_type]{location to store the type of the property}
  @argument[prop_format]{location to store the format of the property}
  @return{the length of the retrieved data.}
  @begin{short}
    Retrieves selection data that was stored by the selection data in response
    to a call to gdk_selection_convert().
  @end{short}
  This function will not be used by applications, who should use the
  GtkClipboard API instead."
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-property-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_send_notify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_send_notify" gdk-selection-send-notify) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @argument[requestor]{window to which to deliver response.}
  @argument[selection]{selection that was requested.}
  @argument[target]{target that was selected.}
  @argument[property]{property in which the selection owner stored the data, or
    GDK_NONE to indicate that the request was rejected.}
  @argument[time_]{timestamp.}
  @short{Sends a response to SelectionRequest event.}"
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (property gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-send-notify)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_send_notify_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_send_notify_for_display"
          gdk-selection-send-notify-for-display) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @argument[display]{the GdkDisplay where requestor is realized}
  @argument[requestor]{window to which to deliver response}
  @argument[selection]{selection that was requested}
  @argument[target]{target that was selected}
  @argument[property]{property in which the selection owner stored the data, or
    GDK_NONE to indicate that the request was rejected}
  @argument[time_]{timestamp}
  @short{Send a response to SelectionRequest event.}

  Since 2.2"
  (display (g-object gdk-display))
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (property gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-send-notify-for-display)

;;; --- End of file gdk.selections.lisp ----------------------------------------
