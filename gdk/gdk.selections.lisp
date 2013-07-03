;;; ----------------------------------------------------------------------------
;;; gdk.selections.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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

(defparameter +gdk-selection-primary+ "PRIMARY"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"PRIMARY\"}
  A @symbol{gdk-atom} representing the @code{\"PRIMARY\"} selection.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-primary+ atdoc:*variable-name-alias*) "Constant")

(export '+gdk-selection-primary+)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_SECONDARY
;;; ----------------------------------------------------------------------------

(defparameter +gdk-selection-secondary+ "SECONDARY"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"SECONDARY\"}
  A @symbol{gdk-atom} representing the @code{\"SECONDARY\"} selection.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-secondary+ atdoc:*variable-name-alias*)
      "Constant")

(export '+gdk-selection-secondary+)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_CLIPBOARD
;;; ----------------------------------------------------------------------------

(defparameter +gdk-selection-clipboard+ "CLIPBOARD"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"CLIPBOARD\"}
  A @symbol{gdk-atom} representing the @code{\"CLIPBOARD\"} selection.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-clipboard+ atdoc:*variable-name-alias*)
      "Constant")

(export '+gdk-selection-clipboard+)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_BITMAP
;;; ----------------------------------------------------------------------------

(defparameter +gdk-target-bitmap+ "BITMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"BITMAP\"}
  A @symbol{gdk-atom} representing the @code{\"BITMAP\"} selection target.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-target-bitmap+ atdoc:*variable-name-alias*) "Constant")

(export '+gdk-target-bitmap+)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_COLORMAP
;;; ----------------------------------------------------------------------------

(defparameter +gdk-target-colormap+ "COLORMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"COLORMAP\"}
  A @symbol{gdk-atom} representing the @code{\"COLORMAP\"} selection target.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-target-colormap+ atdoc:*variable-name-alias*) "Constant")

(export '+gdk-target-colormap+)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_DRAWABLE
;;; ----------------------------------------------------------------------------

(defparameter +gdk-target-drawable+ "DRAWABLE"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"DRAWABLE\"}
  A @symbol{gdk-atom} representing the @code{\"DRAWABLE\"} selection target.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-target-drawable+ atdoc:*variable-name-alias*) "Constant")

(export '+gdk-target-drawable+)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_PIXMAP
;;; ----------------------------------------------------------------------------

(defparameter +gdk-target-pixmap+ "PIXMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"PIXMAP\"}
  A @symbol{gdk-atom} representing the @code{\"PIXMAP\"} selection target.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-target-pixmap+ atdoc:*variable-name-alias*) "Constant")

(export '+gdk-target-pixmap+)

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_STRING
;;; ----------------------------------------------------------------------------

(defparameter +gdk-target-string+ "STRING"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"STRING\"}
  A @symbol{gdk-atom} representing the @code{\"STRING\"} selection target.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-target-string+ atdoc:*variable-name-alias*) "Constant")

(export '+gdk-target-string+)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_ATOM
;;; ----------------------------------------------------------------------------

(defparameter +gdk-selection-type-atom+ "ATOM"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"ATOM\"}
  A @symbol{gdk-atom} representing the @code{\"ATOM\"} selection type.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-atom+ atdoc:*variable-name-alias*)
      "Constant")

(export '+gdk-selection-type-atom+)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_BITMAP
;;; ----------------------------------------------------------------------------

(defparameter +gdk-selection-type-bitmap+ "BITMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"BITMAP\"}
  A @symbol{gdk-atom} representing the @code{\"BITMAP\"} selection type.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-bitmap+ atdoc:*variable-name-alias*)
      "Constant")

(export '+gdk-selection-type-bitmap+)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_COLORMAP
;;; ----------------------------------------------------------------------------

(defparameter +gdk-selection-type-colormap+ "COLORMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"COLORMAP\"}
  A @symbol{gdk-atom} representing the @code{\"COLORMAP\"} selection type.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-colormap+ atdoc:*variable-name-alias*)
      "Constant")

(export '+gdk-selection-type-colormap+)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_DRAWABLE
;;; ----------------------------------------------------------------------------

(defparameter +gdk-selection-type-drawable+ "DRAWABLE"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"DRAWABLE\"}
  A @symbol{gdk-atom} representing the @code{\"DRAWABLE\"} selection type.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-drawable+ atdoc:*variable-name-alias*)
      "Constant")

(export '+gdk-selection-type-drawable+)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_INTEGER
;;; ----------------------------------------------------------------------------

(defparameter +gdk-selection-type-integer+ "INTEGER"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"INTEGER\"}
  A @symbol{gdk-atom} representing the @code{\"INTEGER\"} selection type.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-integer+ atdoc:*variable-name-alias*)
      "Constant")

(export '+gdk-selection-type-integer+)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_PIXMAP
;;; ----------------------------------------------------------------------------

(defparameter +gdk-selection-type-pixmap+ "PIXMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"PIXMAP\"}
  A @symbol{gdk-atom} representing the @code{\"PIXMAP\"} selection type.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-pixmap+ atdoc:*variable-name-alias*) "Constant")

(export '+gdk-selection-type-pixmap+)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_WINDOW
;;; ----------------------------------------------------------------------------

(defparameter +gdk-selection-type-window+ "WINDOW"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"WINDOW\"}
  A @symbol{gdk-atom} representing the @code{\"WINDOW\"} selection type.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-window+ atdoc:*variable-name-alias*)
      "Constant")

(export '+gdk-selection-type-window+)

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_STRING
;;; ----------------------------------------------------------------------------

(defparameter +gdk-selection-type-string+ "STRING"
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @variable-value{\"STRING\"}
  A @symbol{gdk-atom} representing the @code{\"STRING\"} selection type.")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-string+ atdoc:*variable-name-alias*)
      "Constant")

(export '+gdk-selection-type-string+)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_set" gdk-selection-owner-set) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @argument[owner]{a @class{gdk-window} or @code{nil} to indicate that the
    owner for the given @arg{selection} should be unset}
  @argument[selection]{an atom identifying a selection}
  @argument[time]{timestamp to use when setting the @arg{selection}. If this is
    older than the timestamp given last time the owner was set for the given
    selection, the request will be ignored.}
  @argument[send-event]{if @arg{true}, and the new owner is different from the
    current owner, the current owner will be sent a \"selection-clear\" event}
  @return{@arg{True} if the selection owner was successfully changed to
    @arg{owner}, otherwise @arg{nil}.}
  Sets the owner of the given selection."
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
 "@version{2013-6-28}
  @argument[display]{the @class{gdk-display} object}
  @argument[owner]{a @class{gdk-window} object or @code{nil} to indicate that
    the owner for the given should be unset}
  @argument[selection]{an atom identifying a selection}
  @argument[time]{timestamp to use when setting the selection. If this is older
    than the timestamp given last time the owner was set for the given
    selection, the request will be ignored.}
  @argument[send-event]{if @em{true}, and the new owner is different from the
    current owner, the current owner will be sent a \"selection-clear\" event}
  @return{@em{True} if the selection owner was successfully changed to owner,
    otherwise @code{nil}.}
  @begin{short}
    Sets the @class{gdk-window} owner as the current owner of the selection
    selection.
  @end{short}

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
 "@version{2013-6-28}
  @argument[selection]{an atom indentifying a selection}
  @begin{return}
    If there is a selection owner for this window, and it is a window
    known to the current process, the @class{gdk-window} that owns the
    selection, otherwise @code{nil}.
  @end{return}
  @begin{short}
    Determines the owner of the given @arg{selection}.
  @end{short}

  Note that the return value may be owned by a different process if a foreign
  window was previously created for that window, but a new foreign window will
  never be created by this call."
  (selection gdk-atom-as-string))

(export 'gdk-selection-owner-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_get_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_get_for_display"
          gdk-selection-owner-get-for-display) (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @argument[display]{a @class{gdk-display} object}
  @argument[selection]{an atom indentifying a selection}
  @return{if there is a selection owner for this window, and it is a window
    known to the current process, the @class{gdk-window} that owns the
    selection, otherwise @code{nil}}
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
 "@version{2013-6-28}
  @argument[requestor]{a @class{gdk-window} object}
  @argument[selection]{an atom identifying the selection to get the contents of}
  @argument[target]{the form in which to retrieve the selection}
  @argument[time]{the timestamp to use when retrieving the selection. The
    selection owner may refuse the request if it did not own the selection at
    the time indicated by the timestamp.}
  Retrieves the contents of a selection in a given form."
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-convert)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_property_get ()
;;; ----------------------------------------------------------------------------

;; TODO: This implementation is wrong. We do not export the function.

(defcfun ("gdk_selection_property_get" gdk-selection-property-get) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @argument[requestor]{the window on which the data is stored}

  @argument[data]{location to store a pointer to the retrieved data. If the
    retrieval failed, @code{nil} we be stored here, otherwise, it will be
    non-@code{nil} and the returned data should be freed with @code{g_free()}
    when you are finished using it. The length of the allocated memory is one
    more than the length of the returned data, and the final byte will always be
    zero, to ensure nul-termination of strings}
  @argument[prop-type]{location to store the type of the property}
  @argument[prop-format]{location to store the format of the property}

  @return{The length of the retrieved data.}


  @begin{short}
    Retrieves selection data that was stored by the selection data in response
    to a call to gdk_selection_convert().
  @end{short}

  This function will not be used by applications, who should use the
  @class{gtk-clipboard} API instead."
  (requestor (g-object gdk-window))

  (data :pointer)

  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (time :uint32))

;(export 'gdk-selection-property-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_send_notify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_send_notify" gdk-selection-send-notify) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-28}
  @argument[requestor]{window to which to deliver response}
  @argument[selection]{selection that was requested}
  @argument[target]{target that was selected}
  @argument[property]{property in which the selection owner stored the data, or
    @var{+gdk-none+} to indicate that the request was rejected}
  @argument[time]{timestamp}
  Sends a response to the \"selection-request\" event."
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
 "@version{2013-6-28}
  @argument[display]{the @class{gdk-display} object where requestor is realized}
  @argument[requestor]{window to which to deliver response}
  @argument[selection]{selection that was requested}
  @argument[target]{target that was selected}
  @argument[property]{property in which the selection owner stored the data, or
    @var{+gdk-none+} to indicate that the request was rejected}
  @argument[time]{timestamp}
  @short{Send a response to the \"selection-request\" event.}

  Since 2.2"
  (display (g-object gdk-display))
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (property gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-send-notify-for-display)

;;; --- End of file gdk.selections.lisp ----------------------------------------
