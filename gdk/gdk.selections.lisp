;;; ----------------------------------------------------------------------------
;;; gdk.selections.lisp
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
;;; Selections
;;;
;;;     Functions for transfering data via the X selection mechanism
;;;
;;; Types and Values
;;;
;;;     GDK_SELECTION_PRIMARY
;;;     GDK_SELECTION_SECONDARY
;;;     GDK_SELECTION_CLIPBOARD
;;;
;;;     GDK_TARGET_BITMAP
;;;     GDK_TARGET_COLORMAP
;;;     GDK_TARGET_DRAWABLE
;;;     GDK_TARGET_PIXMAP
;;;     GDK_TARGET_STRING
;;;
;;;     GDK_SELECTION_TYPE_ATOM
;;;     GDK_SELECTION_TYPE_BITMAP
;;;     GDK_SELECTION_TYPE_COLORMAP
;;;     GDK_SELECTION_TYPE_DRAWABLE
;;;     GDK_SELECTION_TYPE_INTEGER
;;;     GDK_SELECTION_TYPE_PIXMAP
;;;     GDK_SELECTION_TYPE_WINDOW
;;;     GDK_SELECTION_TYPE_STRING
;;;
;;; Functions
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

#+nil
(defparameter +gdk-selection-primary+ "PRIMARY"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"PRIMARY\"}
  A @symbol{gdk-atom} representing the @code{\"PRIMARY\"} selection.
  @see-symbol{gdk-atom}
  @see-variable{+gdk-selection-secondary+}
  @see-variable{+gdk-selection-clipboard+}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-primary+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_SECONDARY
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-secondary+ "SECONDARY"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"SECONDARY\"}
  A @symbol{gdk-atom} representing the @code{\"SECONDARY\"} selection.
  @see-symbol{gdk-atom}
  @see-variable{+gdk-selection-primary+}
  @see-variable{+gdk-selection-clipboard+}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-secondary+ atdoc:*variable-name-alias*)
      "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_CLIPBOARD
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-clipboard+ "CLIPBOARD"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"CLIPBOARD\"}
  A @symbol{gdk-atom} representing the @code{\"CLIPBOARD\"} selection.
  @see-symbol{gdk-atom}
  @see-variable{+gdk-selection-primary+}
  @see-variable{+gdk-selection-secondary+}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-clipboard+ atdoc:*variable-name-alias*)
      "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_BITMAP
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-target-bitmap+ "BITMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"BITMAP\"}
  A @symbol{gdk-atom} representing the @code{\"BITMAP\"} selection target.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-target-bitmap+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_COLORMAP
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-target-colormap+ "COLORMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"COLORMAP\"}
  A @symbol{gdk-atom} representing the @code{\"COLORMAP\"} selection target.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-target-colormap+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_DRAWABLE
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-target-drawable+ "DRAWABLE"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"DRAWABLE\"}
  A @symbol{gdk-atom} representing the @code{\"DRAWABLE\"} selection target.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-target-drawable+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_PIXMAP
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-target-pixmap+ "PIXMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"PIXMAP\"}
  A @symbol{gdk-atom} representing the @code{\"PIXMAP\"} selection target.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-target-pixmap+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_STRING
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-target-string+ "STRING"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"STRING\"}
  A @symbol{gdk-atom} representing the @code{\"STRING\"} selection target.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-target-string+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_ATOM
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-atom+ "ATOM"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"ATOM\"}
  A @symbol{gdk-atom} representing the @code{\"ATOM\"} selection type.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-atom+ atdoc:*variable-name-alias*)
      "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_BITMAP
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-bitmap+ "BITMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"BITMAP\"}
  A @symbol{gdk-atom} representing the @code{\"BITMAP\"} selection type.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-bitmap+ atdoc:*variable-name-alias*)
      "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_COLORMAP
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-colormap+ "COLORMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"COLORMAP\"}
  A @symbol{gdk-atom} representing the @code{\"COLORMAP\"} selection type.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-colormap+ atdoc:*variable-name-alias*)
      "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_DRAWABLE
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-drawable+ "DRAWABLE"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"DRAWABLE\"}
  A @symbol{gdk-atom} representing the @code{\"DRAWABLE\"} selection type.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-drawable+ atdoc:*variable-name-alias*)
      "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_INTEGER
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-integer+ "INTEGER"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"INTEGER\"}
  A @symbol{gdk-atom} representing the @code{\"INTEGER\"} selection type.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-integer+ atdoc:*variable-name-alias*)
      "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_PIXMAP
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-pixmap+ "PIXMAP"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"PIXMAP\"}
  A @symbol{gdk-atom} representing the @code{\"PIXMAP\"} selection type.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-pixmap+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_WINDOW
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-window+ "WINDOW"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"WINDOW\"}
  A @symbol{gdk-atom} representing the @code{\"WINDOW\"} selection type.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-window+ atdoc:*variable-name-alias*)
      "Constant")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_STRING
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-string+ "STRING"
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @variable-value{\"STRING\"}
  A @symbol{gdk-atom} representing the @code{\"STRING\"} selection type.
  @see-symbol{gdk-atom}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-selection-type-string+ atdoc:*variable-name-alias*)
      "Constant")

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_set" gdk-selection-owner-set) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-27}
  @argument[owner]{a @class{gdk-window} object or @code{nil} to indicate that
    the owner for the given @arg{selection} should be unset}
  @argument[selection]{a @symbol{gdk-atom} as a string identifying a selection}
  @argument[time]{an unsigned integer with the timestamp to use when setting
    the selection}
  @argument[send]{if @em{true}, and the new owner is different from the current
    owner, the current owner will be sent a @code{:selection-clear} event}
  @return{@em{True} if the selection owner was successfully changed to
    @arg{owner}, otherwise @em{false}.}
  @begin{short}
    Sets the owner of the given selection.
  @end{short}
  If @arg{time} is older than the timestamp given last time the owner was set
  for the given selection, the request will be ignored.
  @see-class{gdk-window}
  @see-symbol{gdk-atom}"
  (owner (g-object gdk-window))
  (selection gdk-atom-as-string)
  (time :uint32)
  (send :boolean))

(export 'gdk-selection-owner-set)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_set_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_set_for_display"
          gdk-selection-owner-set-for-display) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-27}
  @argument[display]{the @class{gdk-display} object}
  @argument[owner]{a @class{gdk-window} object or @code{nil} to indicate that
    the owner for the given should be unset}
  @argument[selection]{a @symbol{gdk-atom} as as string identifying a
    selection}
  @argument[time]{an unsigned integer with the timestamp to use when setting
    the selection}
  @argument[send]{if @em{true}, and the new owner is different from the current
    owner, the current owner will be sent a @code{:selection-clear} event}
  @return{@em{True} if the selection owner was successfully changed to
    @arg{owner}, otherwise @code{nil}.}
  @begin{short}
    Sets the owner as the current owner of the selection selection.
  @end{short}
  If @arg{time} is older than the timestamp given last time the owner was set
  for the given selection, the request will be ignored.
  @see-class{gdk-display}
  @see-class{gdk-window}
  @see-symbol{gdk-atom}"
  (display (g-object gdk-display))
  (owner (g-object gdk-window))
  (selection gdk-atom-as-string)
  (time :uint32)
  (send :boolean))

(export 'gdk-selection-owner-set-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_get" gdk-selection-owner-get)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-27}
  @argument[selection]{a @symbol{gdk-atom} as a string indentifying a selection}
  @begin{return}
    If there is a selection owner for this window, and it is a window
    known to the current process, the @class{gdk-window} object that owns the
    selection, otherwise @code{nil}.
  @end{return}
  @begin{short}
    Determines the owner of the given selection.
  @end{short}
  Note that the return value may be owned by a different process if a foreign
  window was previously created for that window, but a new foreign window will
  never be created by this call.
  @see-class{gdk-window}
  @see-symbol{gdk-atom}"
  (selection gdk-atom-as-string))

(export 'gdk-selection-owner-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_get_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_get_for_display"
           gdk-selection-owner-get-for-display) (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-27}
  @argument[display]{a @class{gdk-display} object}
  @argument[selection]{a @symbol{gdk-atom} as a string indentifying a selection}
  @return{If there is a selection owner for this window, and it is a window
    known to the current process, the @class{gdk-window} object that owns the
    selection, otherwise @code{nil}.}
  @short{Determine the owner of the given selection.}
  Note that the return value may be owned by a different process if a foreign
  window was previously created for that window, but a new foreign window will
  never be created by this call.
  @see-class{gdk-display}
  @see-class{gdk-window}
  @see-symbol{gdk-atom}"
  (display (g-object gdk-display))
  (selection gdk-atom-as-string))

(export 'gdk-selection-owner-get-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_convert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_convert" gdk-selection-convert) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-27}
  @argument[requestor]{a @class{gdk-window} object}
  @argument[selection]{a @symbol{gdk-atom} as a string identifying the selection
    to get the contents of}
  @argument[target]{a @symbol{gdk-atom} as a string with the form in which to
    retrieve the selection}
  @argument[time]{an unsigned integer with the timestamp to use when retrieving
    the selection}
  @begin{short}
    Retrieves the contents of a selection in a given form.
  @end{short}
  The selection owner may refuse the request if it did not own the selection at
  the time indicated by the timestamp.
  @see-class{gdk-window}
  @see-symbol{gdk-atom}"
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-convert)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_property_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_property_get" %gdk-selection-property-get) :int
  (requestor (g-object gdk-window))
  (data :pointer)
  (prop-type :pointer)
  (prop-format :pointer))

(defun gdk-selection-property-get (requestor)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-27}
  @argument[requestor]{the @class{gdk-window} object on which the data is
    stored}
  @begin{return}
    @code{length} -- an integer with the length of the retrieved data @br{}
    @code{data} -- a pointer to the retrieved data @br{}
    @code{type} -- a @symbol{gdk-atom} as as string with the type of the
    property @br{}
    @code{format} -- an integer with the format of the property
  @end{return}
  @begin{short}
    Retrieves selection data that was stored in response to a call to the
    function @fun{gdk-selection-convert}.
  @end{short}
  This function will not be used by applications, who should use the
  @class{gtk-clipboard} API instead.

  If the retrieval failed, @code{nil} will be returned, otherwise, it will be
  non-@code{nil} and the returned data should be freed with @code{g_free()}
  when you are finished using it. The length of the allocated memory is one
  more than the length of the returned data, and the final byte will always be
  zero, to ensure nul-termination of strings.
  @see-class{gdk-window}
  @see-class{gtk-clipboard}
  @see-symbol{gdk-atom}
  @see-function{gdk-selection-convert}"
  (with-foreign-objects ((data :pointer)
                         (prop-type 'gdk-atom)
                         (prop-format :int))
    (let ((length (%gdk-selection-property-get requestor
                                               data
                                               prop-type
                                               prop-format)))
      (values length
              data
              (mem-ref prop-type 'gdk-atom-as-string)
              (mem-ref prop-format :int)))))

(export 'gdk-selection-property-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_send_notify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_send_notify" gdk-selection-send-notify) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-25}
  @argument[requestor]{a @class{gdk-window} object to which to deliver response}
  @argument[selection]{a @symbol{gdk-atom} as a string with the selection that
    was requested}
  @argument[target]{a @symbol{gdk-atom} as a string with the target that was
    selected}
  @argument[property]{a @symbol{gdk-atom} as a string with the property in
    which the selection owner stored the data, or \"NONE\" to indicate that
    the request was rejected}
  @argument[time]{an unsigned integer with the timestamp}
  @begin{short}
    Sends a response to the @code{:selection-request} event.
  @end{short}
  @see-class{gdk-window}
  @see-symbol{gdk-atom}"
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
 "@version{2021-3-25}
  @argument[display]{a @class{gdk-display} object where @arg{requestor} is
    realized}
  @argument[requestor]{a @class{gdk-window} object to which to deliver response}
  @argument[selection]{a @symbol{gdk-atom} as a string with the selection that
    was requested}
  @argument[target]{a @symbol{gdk-atom} as a string with the target that was
    selected}
  @argument[property]{a @symbol{gdk-atom} as a string with the property in
    which the selection owner stored the data, or \"NONE\" to indicate that the
    request was rejected}
  @argument[time]{an unsigned integer with the timestamp}
  @short{Send a response to the @code{:selection-request} event.}
  @see-class{gdk-display}
  @see-class{gdk-window}"
  (display (g-object gdk-display))
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (property gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-send-notify-for-display)

;;; --- End of file gdk.selections.lisp ----------------------------------------
