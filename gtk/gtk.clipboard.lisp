;;; ----------------------------------------------------------------------------
;;; gtk.clipboard.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
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
;;; Clipboards
;;;
;;;     Storing data on clipboards
;;;
;;; Types and Values
;;;
;;;     GtkClipboard
;;;
;;; Functions
;;;
;;;     GtkClipboardReceivedFunc
;;;     GtkClipboardTextReceivedFunc
;;;     GtkClipboardImageReceivedFunc
;;;     GtkClipboardTargetsReceivedFunc
;;;     GtkClipboardRichTextReceivedFunc
;;;     GtkClipboardURIReceivedFunc
;;;     GtkClipboardGetFunc
;;;     GtkClipboardClearFunc
;;;
;;;     gtk_clipboard_get
;;;     gtk_clipboard_get_for_display
;;;     gtk_clipboard_get_display
;;;     gtk_clipboard_get_default ()
;;;     gtk_clipboard_set_with_data
;;;     gtk_clipboard_set_with_owner
;;;     gtk_clipboard_get_owner
;;;     gtk_clipboard_clear
;;;     gtk_clipboard_set_text
;;;     gtk_clipboard_set_image
;;;     gtk_clipboard_request_contents
;;;     gtk_clipboard_request_text
;;;     gtk_clipboard_request_image
;;;     gtk_clipboard_request_targets
;;;     gtk_clipboard_request_rich_text
;;;     gtk_clipboard_request_uris
;;;     gtk_clipboard_wait_for_contents
;;;     gtk_clipboard_wait_for_text
;;;     gtk_clipboard_wait_for_image
;;;     gtk_clipboard_wait_for_rich_text
;;;     gtk_clipboard_wait_for_uris
;;;     gtk_clipboard_wait_is_text_available
;;;     gtk_clipboard_wait_is_image_available
;;;     gtk_clipboard_wait_is_rich_text_available
;;;     gtk_clipboard_wait_is_uris_available
;;;     gtk_clipboard_wait_for_targets
;;;     gtk_clipboard_wait_is_target_available
;;;     gtk_clipboard_set_can_store
;;;     gtk_clipboard_store
;;;     gtk_clipboard_get_selection
;;;
;;; Signals
;;;
;;;     void    owner-change    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkClipboard
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkClipboard
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkClipboard" gtk-clipboard
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_clipboard_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-clipboard 'type)
 "@version{2021-3-24}
  @begin{short}
    The @sym{gtk-clipboard} object represents a clipboard of data shared between
    different processes or between different widgets in the same process.
  @end{short}
  Each clipboard is identified by a name encoded as a @symbol{gdk-atom} type.
  Conversion to and from strings can be done with the functions
  @fun{gdk-atom-intern} and @fun{gdk-atom-name}. The default clipboard
  corresponds to the @code{\"CLIPBOARD\"} atom. Another commonly used clipboard
  is the @code{\"PRIMARY\"} clipboard, which, in X, traditionally contains the
  currently selected text.

  To support having a number of different formats on the clipboard at the same
  time, the clipboard mechanism allows providing callbacks instead of the
  actual data. When you set the contents of the clipboard, you can either
  supply the data directly via functions like @fun{gtk-clipboard-set-text},
  or you can supply a callback to be called at a later time when the data is
  needed via the functions @fun{gtk-clipboard-set-with-data} or
  @fun{gtk-clipboard-set-with-owner}. Providing a callback also avoids having
  to make copies of the data when it is not needed.

  The functions @fun{gtk-clipboard-set-with-data} and
  @fun{gtk-clipboard-set-with-owner} are quite similar. The choice between the
  two depends mostly on which is more convenient in a particular situation. The
  former is most useful when you want to have a blob of data with callbacks to
  convert it into the various data types that you advertise. When the
  @code{clear_func} you provided is called, you simply free the data blob. The
  latter is more useful when the contents of clipboard reflect the internal
  state of a GObject. As an example, for the @code{PRIMARY} clipboard, when an
  entry widget provides the clipboard's contents the contents are simply the
  text within the selected region. If the contents change, the entry widget
  can call the function @fun{gtk-clipboard-set-with-owner} to update the
  timestamp for clipboard ownership, without having to worry about
  @code{clear_func} being called.

  Requesting the data from the clipboard is essentially asynchronous. If the
  contents of the clipboard are provided within the same process, then a
  direct function call will be made to retrieve the data, but if they are
  provided by another process, then the data needs to be retrieved from the
  other process, which may take some time. To avoid blocking the user
  interface, the call to request the selection,
  @fun{gtk-clipboard-request-contents} takes a callback that will be called
  when the contents are received or when the request fails. If you do not want
  to deal with providing a separate callback, you can also use the function
  @fun{gtk-clipboard-wait-for-contents}. What this does is run the GLib main
  loop recursively waiting for the contents. This can simplify the code flow,
  but you still have to be aware that other callbacks in your program can be
  called while this recursive mainloop is running.

  Along with the functions to get the clipboard contents as an arbitrary data
  chunk, there are also functions to retrieve it as text,
  @fun{gtk-clipboard-request-text} and @fun{gtk-clipboard-wait-for-text}. These
  functions take care of determining which formats are advertised by the
  clipboard provider, asking for the clipboard in the best available format
  and converting the results into the UTF-8 encoding, which is the standard
  form for representing strings in GTK+.
  @begin[Signal Details]{dictionary}
    @subheading{The \"owner-change\" signal}
      @begin{pre}
 lambda (clipboard event)    :run-first
      @end{pre}
      The signal is emitted when GTK+ receives an event that indicates that the
      ownership of the selection associated with @arg{clipboard} has changed.
      @begin[arg]{table}
        @entry[clipboard]{The @sym{gtk-clipboard} object on which the signal is
          emitted.}
        @entry[event]{The @class{gdk-event-owner-change} event.}
      @end{table}
  @end{dictionary}
  @see-class{gtk-selection-data}")

;;; ----------------------------------------------------------------------------
;;; GtkClipboardReceivedFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-clipboard-received-func :void
    ((clipboard (g-object gtk-clipboard))
     (selection (g-boxed-foreign gtk-selection-data))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn clipboard selection)
      (return-from-gtk-clipboard-received-func () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-clipboard-received-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-clipboard-received-func atdoc:*external-symbols*)
 "@version{2021-3-25}
  @begin{short}
    A callback function to be called when the results of the function
    @fun{gtk-clipboard-request-contents} are received, or when the request
    fails.
  @end{short}
  @begin{pre}
 lambda (clipboard selection)
  @end{pre}
  @begin[code]{table}
    @entry[clipboard]{A @class{gtk-clipboard} object.}
    @entry[selection]{A @class{gtk-selection-data} instance containing the data
      that was received. If retrieving the data failed, then then length field
      of selection_data will be negative.}
  @end{table}
  @see-class{gtk-clipboard}
  @see-class{gtk-selection-data}
  @see-function{gtk-clipboard-request-contents}")

(export 'gtk-clipboard-received-func)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardTextReceivedFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-clipboard-text-received-func :void
    ((clipboard (g-object gtk-clipboard))
     (text :string)
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn clipboard text)
      (return-from-gtk-clipboard-text-received-func () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-clipboard-text-received-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-clipboard-text-received-func atdoc:*external-symbols*)
 "@version{2021-3-25}
  @begin{short}
    A function to be called when the results of the function
    @fun{gtk-clipboard-request-text} are received, or when the request fails.
  @end{short}
  @begin{pre}
 lambda (clipboard text)
  @end{pre}
  @begin[code]{table}
    @entry[clipboard]{A @class{gtk-clipboard} object.}
    @entry[text]{The text received, as a UTF-8 encoded string, or @code{nil} if
      retrieving the data failed.}
  @end{table}
  @see-class{gtk-clipboard}
  @see-function{gtk-clipboard-request-text}")

(export 'gtk-clipboard-text-received-func)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardImageReceivedFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-clipboard-image-received-func :void
    ((clipboard (g-object gtk-clipboard))
     (pixbuf (g-object gdk-pixbuf))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn clipboard pixbuf)
      (return-from-gtk-clipboard-image-received-func () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-clipboard-image-received-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-clipboard-image-received-func atdoc:*external-symbols*)
 "@version{2021-3-25}
  @begin{short}
    A callback function to be called when the results of the function
    @fun{gtk-clipboard-request-image} are received, or when the request fails.
  @end{short}
  @begin{pre}
 lambda (clipboard pixbuf)
  @end{pre}
  @begin[code]{table}
    @entry[clipboard]{A @class{gtk-clipboard} object.}
    @entry[pixbuf]{The text received @class{gdk-pixbuf} object.}
  @end{table}
  @see-class{gtk-clipboard}
  @see-class{gdk-pixbuf}
  @see-function{gtk-clipboard-request-image}")

(export 'gtk-clipboard-image-received-func)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardTargetsReceivedFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-clipboard-targets-received-func :void
    ((clipboard (g-object gtk-clipboard))
     (atoms :pointer)
     (n-atoms :int)
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn clipboard atoms n-atoms)
      (return-from-gtk-clipboard-targets-received-func () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-clipboard-targets-received-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-clipboard-targets-received-func atdoc:*external-symbols*)
 "@version{2021-3-27}
  @begin{short}
    A callback function to be called when the results of the function
    @fun{gtk-clipboard-request-targets} are received, or when the request fails.
  @end{short}
  @begin{pre}
 lambda (clipboard atoms n-atoms)
  @end{pre}
  @begin[code]{table}
    @entry[clipboard]{A @class{gtk-clipboard} object.}
    @entry[atoms]{A pointer to an array of @symbol{gdk-atom} atoms.}
    @entry[n-atoms]{An integer with the length of the atoms array.}
  @end{table}
  @see-class{gtk-clipboard}
  @see-class{gdk-atom}
  @see-function{gtk-clipboard-request-targets}")

(export 'gtk-clipboard-targets-received-func)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardRichTextReceivedFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-clipboard-rich-text-received-func :void
    ((clipboard (g-object gtk-clipboard))
     (format gdk-atom)
     (text :uint8)
     (length g-size)
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn clipboard format text length)
      (return-from-gtk-clipboard-rich-text-received-func () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-clipboard-rich-text-received-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-clipboard-rich-text-received-func atdoc:*external-symbols*)
 "@version{2021-3-27}
  @begin{short}
    A callback function to be called when the results of the function
    @fun{gtk-clipboard-request-rich-text} are received, or when the request
    fails.
  @end{short}
  @begin{pre}
 lambda (clipboard format text length)
  @end{pre}
  @begin[code]{table}
    @entry[clipboard]{A @class{gtk-clipboard} object.}
    @entry[format]{The @symbol{gdk-atom} for the format of the rich text.}
    @entry[text]{The richt text received, as a UTF-8 encoded string, or
      @code{null-pointer} if retrieving the data failed.}
    @entry[length]{An integer with the length of the text.}
  @end{table}
  @see-class{gtk-clipboard}
  @see-class{gdk-atom}
  @see-function{gtk-clipboard-request-rich-text}")

(export 'gtk-clipboard-rich-text-received-func)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardURIReceivedFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-clipboard-uri-received-func :void
    ((clipboard (g-object gtk-clipboard))
     (uris :pointer)
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn clipboard uris)
      (return-from-gtk-clipboard-uri-received-func () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-clipboard-uri-received-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-clipboard-uri-received-func atdoc:*external-symbols*)
 "@version{2021-3-27}
  @begin{short}
    A callback function to be called when the results of the function
    @fun{gtk-clipboard-request-uris} are received, or when the request fails.
  @end{short}
  @begin{pre}
 lambda (clipboard uris)
  @end{pre}
  @begin[code]{table}
    @entry[clipboard]{A @class{gtk-clipboard} object.}
    @entry[uris]{A pointer to the list of received URIs.}
  @end{table}
  @see-class{gtk-clipboard}
  @see-class{gdk-atom}
  @see-function{gtk-clipboard-request-uris}")

(export 'gtk-clipboard-uri-received-func)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardGetFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-clipboard-get-func :void
    ((clipboard (g-object gtk-clipboard))
     (selection (g-boxed-foreign gtk-selection-data))
     (info :uint)
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn clipboard selection info)
      (return-from-gtk-clipboard-get-func () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-clipboard-get-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-clipboard-get-func atdoc:*external-symbols*)
 "@version{2021-10-3}
  @begin{short}
    A function that will be called to provide the contents of the selection.
  @end{short}
  If multiple types of data were advertised, the requested type can be
  determined from the info parameter or by checking the target field of the
  @arg{selection} argument. If the data could successfully be converted into
  then it should be stored into the selection data object by calling the
  @fun{gtk-selection-data-set}  function, or related functions such as the
  @fun{gtk-selection-data-set-text} function. If no data is set, the requestor
  will be informed that the attempt to get the data failed.
  @begin{pre}
 lambda (clipboard selection info)
  @end{pre}
  @begin[code]{table}
    @entry[clipboard]{A @class{gtk-clipboard} object.}
    @entry[selection]{A @class{gtk-selection-data} instance in which the
      requested data should be stored.}
    @entry[info]{The info field corresponding to the requested target from the
      target entries passed to the @fun{gtk-clipboard-set-with-data} or
      @fun{gtk-clipboard-set-with-owner} functions.}
  @end{table}
  @see-class{gtk-clipboard}
  @see-class{gtk-selection-data}
  @see-function{gtk-clipboard-set-with-data}")

(export 'gtk-clipboard-get-func)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardClearFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-clipboard-clear-func :void
    ((clipboard (g-object gtk-clipboard))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn clipboard)
      (return-from-gtk-clipboard-clear-func () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-clipboard-clear-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-clipboard-clear-func atdoc:*external-symbols*)
 "@version{2021-3-27}
  @begin{short}
    A function that will be called when the contents of the clipboard are
    changed or cleared.
  @end{short}
  @begin{pre}
 lambda (clipboard)
  @end{pre}
  @begin[code]{table}
    @entry[clipboard]{A @class{gtk-clipboard} object.}
  @end{table}
  @see-class{gtk-clipboard}")

(export 'gtk-clipboard-clear-func)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_get" gtk-clipboard-get)
    (g-object gtk-clipboard :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-25}
  @argument[selection]{a @symbol{gdk-atom} as a string which identifies the
    clipboard to use}
  @begin{return}
    The appropriate @class{gtk-clipboard} object. If no clipboard already
    exists, a new one will be created. Once a clipboard object has been
    created, it is persistent.
  @end{return}
  @short{Returns the clipboard object for the given selection.}
  See the function @fun{gtk-clipboard-for-display} for complete details.
  @see-class{gtk-clipboard}
  @see-symbol{gdk-atom}
  @see-function{gtk-clipboard-for-display}"
  (selection gdk-atom-as-string))

(export 'gtk-clipboard-get)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get_for_display () -> gtk-clipboard-for-display
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_get_for_display" gtk-clipboard-for-display)
    (g-object gtk-clipboard :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-25}
  @argument[display]{the @class{gdk-display} object for which the clipboard is
    to be retrieved or created}
  @argument[selection]{a @symbol{gdk-atom} as a string which identifies the
    clipboard to use}
  @begin{return}
    The appropriate clipboard object. If no clipboard already exists, a new
    one will be created. Once a clipboard object has been created, it is
    persistent.
  @end{return}
  @begin{short}
    Returns the clipboard object for the given selection.
  @end{short}
  Cut/copy/paste menu items and keyboard shortcuts should use the default
  clipboard, returned by the atom @code{\"CLIPBOARD\"} for @arg{selection}.
  The atom @code{\"NONE\"} is supported as a synonym for the atom
  @code{\"CLIPBOARD\"} for backwards compatibility reasons. The
  currently-selected object or text should be provided on the clipboard
  identified by the atom @code{\"PRIMARY\"}. Cut/copy/paste menu items
  conceptually copy the contents of the atom @code{\"PRIMARY\"} clipboard to
  the default clipboard, i.e. they copy the selection to what the user sees as
  the clipboard.

  It is possible to have arbitrary named clipboards. If you do invent new
  clipboards, you should prefix the selection name with an underscore, because
  the ICCCM requires that nonstandard atoms are underscore-prefixed, and
  namespace it as well. For example, if your application called \"Foo\" has a
  special-purpose clipboard, you might call it \"_FOO_SPECIAL_CLIPBOARD\".
  @see-class{gtk-clipboard}
  @see-symbol{gdk-atom}
  @see-function{gtk-clipboard-get}"
  (display (g-object gdk-display))
  (selection gdk-atom-as-string))

(export 'gtk-clipboard-for-display)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get_display () -> gtk-clipboard-display
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_get_display" gtk-clipboard-display)
    (g-object gdk-display)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-25}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @return{The @class{gdk-display} object associated with @arg{clipboard}.}
  @begin{short}
    Gets the display associated with the clipboard.
  @end{short}
  @see-class{gtk-clipboard}"
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-clipboard-display)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get_default () -> gtk-clipboard-default
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_get_default" gtk-clipboard-default)
    (g-object gtk-clipboard)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-25}
  @argument[display]{a @class{gdk-display} object for which the clipboard is
    to be retrieved}
  @return{The default @class{gtk-clipboard} object.}
  @begin{short}
    Returns the default clipboard object for use with cut/copy/paste menu items
    and keyboard shortcuts.
  @end{short}
  @see-class{gtk-clipboard}"
  (display (g-object gdk-display)))

(export 'gtk-clipboard-default)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_set_with_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_set_with_data" %gtk-clipboard-set-with-data) :boolean
  (clipboard (g-object gtk-clipboard))
  (targets :pointer)
  (n-targets :int)
  (func :pointer)
  (clear-func :pointer)
  (data :pointer))

(defun gtk-clipboard-set-with-data (clipboard targets func)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[targets]{a list of target entries containing information about the
    available forms for the clipboard data}
  @argument[func]{a @symbol{gtk-clipboard-get-func} callback function to call
    to get the actual clipboard data}
  @return{@em{True} if setting the clipboard data succeeded. If setting the
    clipboard data failed the provided callback functions will be ignored.}
  @begin{short}
    Virtually sets the contents of the specified clipboard by providing a list
    of supported formats for the clipboard data and a function to call to get
    the actual data when it is requested.
  @end{short}
  @see-class{gtk-clipboard}
  @see-symbol{gtk-clipboard-get-func}"
  (let ((n-targets (length targets)))
    (with-foreign-object (targets-ptr '(:struct %gtk-target-entry) n-targets)
      (loop for i from 0 below n-targets
            for target-ptr = (mem-aptr targets-ptr
                                       '(:struct %gtk-target-entry) i)
            for entry = (pop targets)
            do (with-foreign-slots ((target flags info)
                                    target-ptr
                                    (:struct %gtk-target-entry))
                 (setf target (first entry))
                 (setf flags (second entry))
                 (setf info (third entry))))
      (%gtk-clipboard-set-with-data clipboard
                                    targets-ptr
                                    n-targets
                                    (callback gtk-clipboard-get-func)
                                    (callback stable-pointer-destroy-notify-cb)
                                    (allocate-stable-pointer func)))))

(export 'gtk-clipboard-set-with-data)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_set_with_owner ()
;;;
;;; gboolean gtk_clipboard_set_with_owner (GtkClipboard *clipboard,
;;;                                        const GtkTargetEntry *targets,
;;;                                        guint n_targets,
;;;                                        GtkClipboardGetFunc get_func,
;;;                                        GtkClipboardClearFunc clear_func,
;;;                                        GObject *owner);
;;;
;;; Virtually sets the contents of the specified clipboard by providing a list
;;; of supported formats for the clipboard data and a function to call to get
;;; the actual data when it is requested.
;;;
;;; The difference between this function and gtk_clipboard_set_with_data() is
;;; that instead of an generic user_data pointer, a GObject is passed in.
;;;
;;; clipboard :
;;;     a GtkClipboard
;;;
;;; targets :
;;;     array containing information about the available forms for the clipboard
;;;     data
;;;
;;; n_targets :
;;;     number of elements in targets
;;;
;;; get_func :
;;;     function to call to get the actual clipboard data
;;;
;;; clear_func :
;;;     when the clipboard contents are set again, this function will be called,
;;;     and get_func will not be subsequently called
;;;
;;; owner :
;;;     an object that "owns" the data. This object will be passed to the
;;;     callbacks when called
;;;
;;; Returns :
;;;     TRUE if setting the clipboard data succeeded. If setting the clipboard
;;;     data failed the provided callback functions will be ignored.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get_owner ()
;;;
;;; GObject * gtk_clipboard_get_owner (GtkClipboard *clipboard);
;;;
;;; If the clipboard contents callbacks were set with
;;; gtk_clipboard_set_with_owner(), and the gtk_clipboard_set_with_data() or
;;; gtk_clipboard_clear() has not subsequently called, returns the owner set by
;;; gtk_clipboard_set_with_owner().
;;;
;;; clipboard :
;;;     a GtkClipboard
;;;
;;; Returns :
;;;     the owner of the clipboard, if any; otherwise NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_clear" gtk-clipboard-clear) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-25}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @begin{short}
    Clears the contents of the clipboard.
  @end{short}
  Generally this should only be called between the time you call the functions
  @fun{gtk-clipboard-set-with-owner} or @fun{gtk-clipboard-set-with-data}, and
  when the callback function @symbol{gtk-clipboard-clear-func} you supplied is
  called. Otherwise, the clipboard may be owned by someone else.
  @see-class{gtk-clipboard}
  @see-symbol{gtk-clipboard-clear-func}
  @see-function{gtk-clipboard-set-with-owner}
  @see-function{gtk-clipboard-set-with-data}"
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-clipboard-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_set_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_set_text" %gtk-clipboard-set-text) :void
  (clipboard (g-object gtk-clipboard))
  (text :string)
  (len :int))

(defun gtk-clipboard-set-text (clipboard text)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-25}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[text]{a UTF-8 string}
  @begin{short}
    Sets the contents of the clipboard to the given UTF-8 string
   @arg{text}.
  @end{short}
  GTK+ will make a copy of @arg{text} and take responsibility for responding
  for requests for the text, and for converting the text into the requested
  format.
  @see-class{gtk-clipboard}
  @see-function{gtk-clipboard-request-text}"
  (%gtk-clipboard-set-text clipboard text -1))

(export 'gtk-clipboard-set-text)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_set_image ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_set_image" gtk-clipboard-set-image) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @begin{short}
    Sets the contents of the clipboard to the given pixbuf.
  @end{short}
  GTK+ will take responsibility for responding for requests for the image, and
  for converting the image into the requested format.
  @see-class{gtk-clipboard}
  @see-class{gdk-pixbuf}"
  (clipboard (g-object gtk-clipboard))
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-clipboard-set-image)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_contents ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_request_contents" %gtk-clipboard-request-contents)
    :void
  (clipboard (g-object gtk-clipboard))
  (target gdk-atom-as-string)
  (func :pointer)
  (data :pointer))

(defun gtk-clipboard-request-contents (clipboard target func)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[target]{a @symbol{gdk-atom} as a string representing the form into
    which the clipboard owner should convert the selection}
  @argument[func]{a @symbol{gtk-clipboard-received-func} callback function to
    call when the results are received (or the retrieval fails). If the
    retrieval fails the length field of the @class{gtk-selection-data} instance
    will be negative.}
  @begin{short}
    Requests the contents of clipboard as the given target.
  @end{short}
  When the results of the result are later received the supplied callback will
  be called.
  @see-class{gtk-clipboard}"
  (%gtk-clipboard-request-contents clipboard
                                   target
                                   (callback gtk-clipboard-received-func)
                                   (allocate-stable-pointer func)))

(export 'gtk-clipboard-request-contents)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_request_text" %gtk-clipboard-request-text) :void
  (clipboard (g-object gtk-clipboard))
  (func :pointer)
  (data :pointer))

(defun gtk-clipboard-request-text (clipboard func)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-25}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[func]{a @symbol{gtk-clipboard-text-received-func} callback function
    to call when the text is received, or the retrieval fails, it will always
    be called one way or the other}
  @begin{short}
    Requests the contents of the clipboard as text.
  @end{short}
  When the text is later received, it will be converted to UTF-8 if necessary,
  and callback will be called.

  The @arg{text} parameter to callback will contain the resulting text if the
  request succeeded, or @code{nil} if it failed. This could happen for various
  reasons, in particular if the clipboard was empty or if the contents of the
  clipboard could not be converted into text form.
  @see-class{gtk-clipboard}
  @see-symbol{gtk-clipboard-text-received-func}
  @see-function{gtk-clipboard-set-text}"
  (%gtk-clipboard-request-text clipboard
                               (callback gtk-clipboard-text-received-func)
                               (allocate-stable-pointer func)))

(export 'gtk-clipboard-request-text)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_image ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_request_image" %gtk-clipboard-request-image) :void
  (clipboard (g-object gtk-clipboard))
  (func :pointer)
  (data :pointer))

(defun gtk-clipboard-request-image (clipboard func)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[func]{a @symbol{gtk-clipboard-image-receibed-func} callback function
    to call when the image is received, or the retrieval fails. (It will always
    be called one way or the other.)}
  @begin{short}
    Requests the contents of the clipboard as image.
  @end{short}
  When the image is later received, it will be converted to a GdkPixbuf, and
  callback will be called.

  The pixbuf parameter to callback will contain the resulting GdkPixbuf if the
  request succeeded, or @code{nil} if it failed. This could happen for various
  reasons, in particular if the clipboard was empty or if the contents of the
  clipboard could not be converted into an image.
  @see-class{gtk-clipboard}
  @see-symbol{gtk-clipboard-image-receibed-func}"
  (%gtk-clipboard-request-image clipboard
                                (callback gtk-clipboard-image-received-func)
                                (allocate-stable-pointer func)))

(export 'gtk-clipboard-request-image)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_request_targets" %gtk-clipboard-request-targets) :void
  (clipboard (g-object gtk-clipboard))
  (func :pointer)
  (data :pointer))

(defun gtk-clipboard-request-targets (clipboard func)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[func]{a @symbol{gtk-clipboard-targets-received-func} callback
    function to call when the targets are received, or the retrieval fails}
  @begin{short}
    Requests the contents of the clipboard as list of supported targets.
  @end{short}
  When the list is later received, the callback function will be called.

  The @arg{atoms} parameter of the callback function will contain the
  resulting targets if the request succeeded, or @code{nil} if it failed.
  @see-class{gtk-clipboard}
  @see-symbol{gtk-clipboard-targets-received-func}"
  (%gtk-clipboard-request-targets clipboard
                                  (callback gtk-clipboard-targets-received-func)
                                  (allocate-stable-pointer func)))

(export 'gtk-clipboard-request-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_rich_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_request_rich_text" %gtk-clipboard-request-rich-text)
    :void
  (clipboard (g-object gtk-clipboard))
  (buffer (g-object gtk-buffer))
  (func :pointer)
  (data :pointer))

(defun gtk-clipboard-request-rich-text (clipboard buffer func)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[func]{a @symbol{gtk-clipboard-rich-text-received-func} callback
    function to call when the text is received, or the retrieval fails. (It
    will always be called one way or the other.)}
  @begin{short}
    Requests the contents of the clipboard as rich text.
  @end{short}
  When the rich text is later received, callback will be called.

  The text parameter to callback will contain the resulting rich text if the
  request succeeded, or @code{nil} if it failed. The length parameter will
  contain text's length. This function can fail for various reasons, in
  particular if the clipboard was empty or if the contents of the clipboard
  could not be converted into rich text form.
  @see-class{gtk-clipboard}
  @see-symbol{gtk-clipboard-rich-text-received-func}"
  (%gtk-clipboard-request-rich-text
                                clipboard
                                buffer
                                (callback gtk-clipboard-rich-text-received-func)
                                (allocate-stable-pointer func)))

(export 'gtk-clipboard-request-rich-text)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_uris ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_request_uris" %gtk-clipboard-request-uris) :void
  (clipboard (g-object gtk-clipboard))
  (func :pointer)
  (data :pointer))

(defun gtk-clipboard-request-uris (clipboard func)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[func]{a @symbol{gtk-clipboard-uri-received-func} callback function
    to call when the URIs are received, or the retrieval fails. (It will always
    be called one way or the other.)}
  @begin{short}
    Requests the contents of the clipboard as URIs.
  @end{short}
  When the URIs are later received callback will be called.

  The uris parameter to callback will contain the resulting array of URIs if
  the request succeeded, or @code{nil} if it failed. This could happen for
  various reasons, in particular if the clipboard was empty or if the contents
  of the clipboard could not be converted into URI form.
  @see-class{gtk-clipboard}
  @see-symbol{gtk-clipboard-uri-received-func}"
  (%gtk-clipboard-request-uris clipboard
                               (callback gtk-clipboard-uri-received-func)
                               (allocate-stable-pointer func)))

(export 'gtk-clipboard-request-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_contents ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_wait_for_contents" gtk-clipboard-wait-for-contents)
    (g-boxed-foreign gtk-selection-data)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[target]{a @symbol{gdk-atom} as a string representing the form into
    which the clipboard owner should convert the selection}
  @return{a newly-allocated @class{gtk-selection-data} object or @code{nil} if
    retrieving the given target failed.}
  @begin{short}
    Requests the contents of the clipboard using the given target.
  @end{short}
  This function waits for the data to be received using the main loop, so
  events, timeouts, etc, may be dispatched during the wait.
  @see-class{gtk-clipboard}
  @see-class{gtk-selection-data}
  @see-symbol{gdk-atom}"
  (clipboard (g-object gtk-clipboard))
  (target gdk-atom-as-string))

(export 'gtk-clipboard-wait-for-contents)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_wait_for_text" gtk-clipboard-wait-for-text) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @return{a newly-allocated UTF-8 string, or @code{nil} if retrieving the
    selection data failed. (This could happen for various reasons, in particular
    if the clipboard was empty or if the contents of the clipboard could not be
    converted into text form.)}
  @begin{short}
    Requests the contents of the clipboard as text and converts the result to
    UTF-8 if necessary.
  @end{short}
  This function waits for the data to be received using the main loop, so
  events, timeouts, etc, may be dispatched during the wait.
  @see-class{gtk-clipboard}"
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-clipboard-wait-for-text)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_image ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_wait_for_image" gtk-clipboard-wait-for-image)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @return{a newly-allocated @class{gdk-pixbuf} object, or @code{nil} if
    retrieving the selection data failed. (This could happen for various
    reasons, in particular if the clipboard was empty or if the contents of the
    clipboard could not be converted into an image.)}
  @begin{short}
    Requests the contents of the clipboard as image and converts the result to
    a @class{gdk-pixbuf} object.
  @end{short}
  This function waits for the data to be received using the main loop, so
  events, timeouts, etc, may be dispatched during the wait.
  @see-class{gtk-clipboard}
  @see-class{gdk-pixbuf}"
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-clipboard-wait-for-image)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_rich_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_wait_for_rich_text" %gtk-clipboard-wait-for-rich-text)
    :uint8
  (clipboard (g-object gtk-clipboard))
  (buffer (g-object gtk-text-buffer))
  (format :pointer)
  (length :pointer))

(defun gtk-clipboard-wait-for-rich-text (clipboard buffer)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{return}
    @code{data} -- a newly-allocated binary block of data which must be freed
    with @code{g_free()}, or @code{nil} if retrieving the selection data failed.
    (This could happen for various reasons, in particular if the clipboard was
    empty or if the contents of the clipboard could not be converted into text
    form.) @br{}
    @code{format} -- a @symbol{gdk-atom} as a string with the format of the
      returned data @br{}
    @code{length} -- an integer with the length of the returned data
  @end{return}
  @begin{short}
    Requests the contents of the clipboard as rich text.
  @end{short}
  This function waits for the data to be received using the main loop, so
  events, timeouts, etc, may be dispatched during the wait.
  @see-class{gtk-clipboard}
  @see-class{gtk-buffer}
  @see-symbol{gdk-atom}"
  (with-foreign-objects ((format 'gdk-atom) (length :int))
    (let ((data (%gtk-clipboard-wait-for-rich-text clipboard
                                                   buffer
                                                   format
                                                   length)))
      (when data
        (values data
                (mem-ref format :pointer) ; gdk-atom
                (mem-ref length :int))))))

(export 'gtk-clipboard-wait-for-rich-text)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_uris ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_wait_for_uris" gtk-clipboard-wait-for-uris) g-strv
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @return{A newly-allocated array of strings which must be freed with
    @code{g_strfreev(}), or @code{nil} if retrieving the selection data failed.
    (This could happen for various reasons, in particular if the clipboard
    was empty or if the contents of the clipboard could not be converted into
    URI form.)}
  @begin{short}
    Requests the contents of the clipboard as URIs.
  @end{short}
  This function waits for the data to be received using the main loop, so
  events, timeouts, etc, may be dispatched during the wait.
  @see-class{gtk-clipboard}"
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-clipboard-wait-for-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_is_text_available ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_wait_is_text_available"
           gtk-clipboard-wait-is-text-available) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @return{@em{True} is there is text available, @em{false} otherwise.}
  @begin{short}
    Test to see if there is text available to be pasted.
  @end{short}
  This is done by requesting the \"TARGETS\" atom and checking if it contains
  any of the supported text targets. This function waits for the data to be
  received using the main loop, so events, timeouts, etc, may be dispatched
  during the wait.

  This function is a little faster than calling the function
  @fun{gtk-clipboard-wait-for-text} since it does not need to retrieve the
  actual text.
  @see-class{gtk-clipboard}
  @see-function{gtk-clipboard-wait-for-text}"
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-clipboard-wait-is-text-available)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_is_image_available ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_wait_is_image_available"
           gtk-clipboard-wait-is-image-available) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @return{@em{True} is there is an image available, @em{false} otherwise.}
  @begin{short}
    Test to see if there is an image available to be pasted.
  @end{short}
  This is done by requesting the \"TARGETS\" atom and checking if it contains
  any of the supported image targets. This function waits for the data to be
  received using the main loop, so events, timeouts, etc, may be dispatched
  during the wait.

  This function is a little faster than calling the function
  @fun{gtk-clipboard-wait-for-image} since it does not need to retrieve the
  actual image data.
  @see-class{gtk-clipboard}
  @see-function{gtk-clipboard-wait-for-image}"
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-clipboard-wait-is-image-available)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_is_rich_text_available ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_wait_is_rich_text_available"
           gtk-clipboard-wait-is-rich-text-available) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{@em{True} is there is rich text available, @em{false} otherwise.}
  @begin{short}
    Test to see if there is rich text available to be pasted.
  @end{short}
  This is done by requesting the \"TARGETS\" atom and checking if it contains
  any of the supported rich text targets. This function waits for the data to be
  received using the main loop, so events, timeouts, etc, may be dispatched
  during the wait.

  This function is a little faster than calling the function
  @fun{gtk-clipboard-wait-for-rich-text} since it does not need to retrieve the
  actual text.
  @see-class{gtk-clipboard}
  @see-function{gtk-clipboard-wait-for-rich-text}"
  (clipboard (g-object gtk-clipboard))
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-clipboard-wait-is-rich-text-available)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_is_uris_available ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_wait_is_uris_available"
           gtk-clipboard-wait-is-uris-available) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @return{@em{True} is there is an URI list available, @em{false} otherwise.}
  @begin{short}
    Test to see if there is a list of URIs available to be pasted.
  @end{short}
  This is done by requesting the \"TARGETS\" atom and checking if it contains
  the URI targets. This function waits for the data to be received using the
  main loop, so events, timeouts, etc, may be dispatched during the wait.

  This function is a little faster than calling the function
  @fun{gtk-clipboard-wait-for-uris} since it does not need to retrieve the
  actual URI data.
  @see-class{gtk-clipboard}
  @see-function{gtk-clipboard-wait-for-uris}"
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-clipboard-wait-is-uris-available)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_wait_for_targets" gtk-clipboard-wait-for-targets)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[targets]{location to store an array of targets. The result stored
    here must be freed with g_free()}
  @argument[n-targets]{location to store number of items in targets.}
  @return{@em{True} if any targets are present on the clipboard, otherwise
    @em{false}.}
  @begin{short}
    Returns a list of targets that are present on the clipboard, or @code{nil}
    if there are not any targets available.
  @end{short}
  The returned list must be freed with g_free(). This function waits for the
  data to be received using the main loop, so events, timeouts, etc, may be
  dispatched during the wait.
  @see-class{gtk-clipboard}"
  (clipboard (g-object gtk-clipboard))
  (targets :pointer)
  (n-targets :int))

(export 'gtk-clipboard-wait-for-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_is_target_available ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_wait_is_target_available"
           gtk-clipboard-wait-is-target-available) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[target]{a @symbol{gdk-atom} as a string indicating which target to
    look for}
  @return{@em{True} if the target is available, @em{false} otherwise.}
  @begin{short}
    Checks if a clipboard supports pasting data of a given type.
  @end{short}
  This function can be used to determine if a \"Paste\" menu item should be
  insensitive or not.

  If you want to see if there is text available on the clipboard, use the
  function @fun{gtk-clipboard-wait-is-text-available} instead.
  @see-class{gtk-clipboard}
  @see-function{gtk-clipboard-wait-is-text-available}"
  (clipboard (g-object gtk-clipboard))
  (target gdk-atom-as-string))

(export 'gtk-clipboard-wait-is-target-available)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_set_can_store ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_set_can_store" gtk-clipboard-set-can-store) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @argument[targets]{array containing information about which forms should be
    stored or NULL to indicate that all forms should be stored}
  @argument[n-targets]{number of elements in targets}
  @begin{short}
    Hints that the clipboard data should be stored somewhere when the
    application exits or when the function @fun{gtk-clipboard-store} is called.
  @end{short}

  This value is reset when the clipboard owner changes. Where the clipboard
  data is stored is platform dependent, see the function
  @fun{gdk-display-store-clipboard} for more information.
  @see-class{gtk-clipboard}
  @see-function{gtk-clipboard-store}
  @see-function{gdk-display-store-clipboard}"
  (clipboard (g-object gtk-clipboard))
  (targets :pointer)
  (n-targets :int))

(export 'gtk-clipboard-set-can-store)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_store ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_clipboard_store" gtk-clipboard-store) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @begin{short}
    Stores the current clipboard data somewhere so that it will stay around
    after the application has quit.
  @end{short}
  @see-class{gtk-clipboard}"
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-clipboard-store)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get_selection ()
;;; ----------------------------------------------------------------------------

#+gtk-3-22
(defcfun ("gtk_clipboard_get_selection" gtk-clipboard-selection)
    gdk-atom-as-string
 #+cl-cffi-gtk-documentation
 "@version{2021-3-28}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @return{The @symbol{gdk-atom} as a string with the selection.}
  @begin{short}
    Gets the selection that this clipboard is for.
  @end{short}
  Since 3.22
  @see-class{gtk-clipboard}"
  (clipboard (g-object gtk-clipboard)))

#+gtk-3-22
(export 'gtk-clipboard-selection)

;;; --- End of file gtk.clipboard.lisp -----------------------------------------
