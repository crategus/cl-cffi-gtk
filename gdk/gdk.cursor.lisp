;;; ----------------------------------------------------------------------------
;;; gdk.cursor.lisp
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
;;; Cursors
;;;
;;; Standard and pixmap cursors
;;;
;;; Synopsis
;;;
;;;     GdkCursor
;;;     GdkCursorType
;;;
;;;     gdk_cursor_new
;;;     gdk_cursor_new_from_pixbuf
;;;     gdk_cursor_new_from_name
;;;     gdk_cursor_new_for_display
;;;     gdk_cursor_get_display
;;;     gdk_cursor_get_image
;;;     gdk_cursor_get_cursor_type
;;;     gdk_cursor_ref                           * deprecated *
;;;     gdk_cursor_unref                         * deprecated *
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GdkCursor
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkCursor
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkCursor" gdk-cursor
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_cursor_get_type")
  ((cursor-type
    gdk-cursor-cursor-type
    "cursor-type" "GdkCursorType" t t)
   (display
    gdk-cursor-display
    "display" "GdkDisplay" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-cursor 'type)
 "@version{2013-7-29}
  @begin{short}
    These functions are used to create and destroy cursors. There is a number
    of standard cursors, but it is also possible to construct new cursors from
    pixbufs. There may be limitations as to what kinds of cursors can be
    constructed on a given display, see the functions
    @fun{gdk-display-supports-cursor-alpha},
    @fun{gdk-display-supports-cursor-color},
    @fun{gdk-display-get-default-cursor-size} and
    @fun{gdk-display-get-maximal-cursor-size}.
  @end{short}

  Cursors by themselves are not very interesting, they must be be bound to a
  window for users to see them. This is done with the function
  @fun{gdk-window-set-cursor} or by setting the cursor member of the
  @class{gdk-window-attr} structure passed to the function @fun{gdk-window-new}.
  @see-slot{gdk-cursor-cursor-type}
  @see-slot{gdk-cursor-display}
  @see-function{gdk-display-supports-cursor-alpha}
  @see-function{gdk-display-supports-cursor-color}
  @see-function{gdk-display-get-default-cursor-size}
  @see-function{gdk-display-get-maximal-cursor-size}
  @see-function{gdk-window-set-cursor}
  @see-function{gdk-window-new}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cursor-type" 'gdk-cursor) 't)
 "The @code{\"cursor-type\"} property of type @symbol{gdk-cursor-type}
  (Read / Write / Construct) @br{}
  Standard cursor type. @br{}
  Default value: @code{:x-cursor}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "display" 'gdk-cursor) 't)
 "The @code{\"display\"} property of type @class{gdk-display}
  (Read / Write / Construct) @br{}
  Display of this cursor.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-cursor-cursor-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-cursor-cursor-type 'function)
 "@version{2013-7-29}
  Accessor of the slot @code{\"cursor-type\"} of the @class{gdk-cursor} class.
  @see-function{gdk-cursor-get-cursor-type}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-cursor-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-cursor-display 'function)
 "@version{2013-7-29}
  Accessor of the slot @code{\"display\"} of the @class{gdk-cursor} class.
  @see-function{gdk-cursor-get-display}")

;;; ----------------------------------------------------------------------------
;;; enum GdkCursorType
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkCursorType" gdk-cursor-type
  (:export t
   :type-initializer "gdk_cursor_type_get_type")
  (:x-cursor 0)
  (:arrow 2)
  (:based-arrow-down 4)
  (:based-arrow-up 6)
  (:boat 8)
  (:bogosity 10)
  (:bottom-left-corner 12)
  (:bottom-right-corner 14)
  (:bottom-side 16)
  (:bottom-tee 18)
  (:box-spiral 20)
  (:center-ptr 22)
  (:circle 24)
  (:clock 26)
  (:coffee-mug 28)
  (:cross 30)
  (:cross-reverse 32)
  (:crosshair 34)
  (:diamond-cross 36)
  (:dot 38)
  (:dotbox 40)
  (:double-arrow 42)
  (:draft-large 44)
  (:draft-small 46)
  (:draped-box 48)
  (:exchange 50)
  (:fleur 52)
  (:gobbler 54)
  (:gumby 56)
  (:hand1 58)
  (:hand2 60)
  (:heart 62)
  (:icon 64)
  (:iron-cross 66)
  (:left-ptr 68)
  (:left-side 70)
  (:left-tee 72)
  (:leftbutton 74)
  (:ll-angle 76)
  (:lr-angle 78)
  (:man 80)
  (:middlebutton 82)
  (:mouse 84)
  (:pencil 86)
  (:pirate 88)
  (:plus 90)
  (:question-arrow 92)
  (:right-ptr 94)
  (:right-side 96)
  (:right-tee 98)
  (:rightbutton 100)
  (:rtl-logo 102)
  (:sailboat 104)
  (:sb-down-arrow 106)
  (:sb-h-double-arrow 108)
  (:sb-left-arrow 110)
  (:sb-right-arrow 112)
  (:sb-up-arrow 114)
  (:sb-v-double-arrow 116)
  (:shuttle 118)
  (:sizing 120)
  (:spider 122)
  (:spraycan 124)
  (:star 126)
  (:target 128)
  (:tcross 130)
  (:top-left-arrow 132)
  (:top-left-corner 134)
  (:top-right-corner 136)
  (:top-side 138)
  (:top-tee 140)
  (:trek 142)
  (:ul-angle 144)
  (:umbrella 146)
  (:ur-angle 148)
  (:watch 150)
  (:xterm 152)
  (:last-cursor 153)
  (:blank-cursor -2)
  (:cursor-is-pixmap -1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-cursor-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-cursor-type atdoc:*external-symbols*)
 "@version{2013-7-29}
  @begin{short}
    The standard cursors available.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkCursorType\" gdk-cursor-type
  (:export t
   :type-initializer \"gdk_cursor_type_get_type\")
  (:x-cursor 0)
  (:arrow 2)
  (:based-arrow-down 4)
  (:based-arrow-up 6)
  (:boat 8)
  (:bogosity 10)
  (:bottom-left-corner 12)
  (:bottom-right-corner 14)
  (:bottom-side 16)
  (:bottom-tee 18)
  (:box-spiral 20)
  (:center-ptr 22)
  (:circle 24)
  (:clock 26)
  (:coffee-mug 28)
  (:cross 30)
  (:cross-reverse 32)
  (:crosshair 34)
  (:diamond-cross 36)
  (:dot 38)
  (:dotbox 40)
  (:double-arrow 42)
  (:draft-large 44)
  (:draft-small 46)
  (:draped-box 48)
  (:exchange 50)
  (:fleur 52)
  (:gobbler 54)
  (:gumby 56)
  (:hand1 58)
  (:hand2 60)
  (:heart 62)
  (:icon 64)
  (:iron-cross 66)
  (:left-ptr 68)
  (:left-side 70)
  (:left-tee 72)
  (:leftbutton 74)
  (:ll-angle 76)
  (:lr-angle 78)
  (:man 80)
  (:middlebutton 82)
  (:mouse 84)
  (:pencil 86)
  (:pirate 88)
  (:plus 90)
  (:question-arrow 92)
  (:right-ptr 94)
  (:right-side 96)
  (:right-tee 98)
  (:rightbutton 100)
  (:rtl-logo 102)
  (:sailboat 104)
  (:sb-down-arrow 106)
  (:sb-h-double-arrow 108)
  (:sb-left-arrow 110)
  (:sb-right-arrow 112)
  (:sb-up-arrow 114)
  (:sb-v-double-arrow 116)
  (:shuttle 118)
  (:sizing 120)
  (:spider 122)
  (:spraycan 124)
  (:star 126)
  (:target 128)
  (:tcross 130)
  (:top-left-arrow 132)
  (:top-left-corner 134)
  (:top-right-corner 136)
  (:top-side 138)
  (:top-tee 140)
  (:trek 142)
  (:ul-angle 144)
  (:umbrella 146)
  (:ur-angle 148)
  (:watch 150)
  (:xterm 152)
  (:last-cursor 153)
  (:blank-cursor -2)
  (:cursor-is-pixmap -1))
  @end{pre}
  @begin[code]{table}
    @entry[:last-cursor]{Last cursor type.}
    @entry[:blank-cursor]{Blank cursor. Since 2.16.}
    @entry[:cursor-is-pixmap]{Type of cursors constructed with
      @fun{gdk-cursor-new-from-pixbuf}.}
  @end{table}
  @see-class{gdk-cursor}
  @see-function{gdk-cursor-new}
  @see-function{gdk-cursor-new-for-display}
  @see-function{gdk-cursor-get-cursor-type}")

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_new" gdk-cursor-new) (g-object gdk-cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[cursor-type]{cursor to create}
  @return{A new @class{gdk-cursor} object.}
  @begin{short}
    Creates a new cursor from the set of builtin cursors for the default
    display. See the function @fun{gdk-cursor-new-for-display}.
  @end{short}

  To make the cursor invisible, use the value @code{:blank-cursor} of the
  @symbol{gdk-cursor-type} enumeration.
  @see-class{gdk-cursor}
  @see-symbol{gdk-cursor-type}
  @see-function{gdk-cursor-new-for-display}"
  (cursor-type gdk-cursor-type))

(export 'gdk-cursor-new)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_new_from_pixbuf" gdk-cursor-new-from-pixbuf)
    (g-object gdk-cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[display]{the @class{gdk-display} object for which the cursor will
    be created}
  @argument[pixbuf]{the @class{gdk-pixbuf} object containing the cursor image}
  @argument[x]{the horizontal offset of the 'hotspot' of the cursor}
  @argument[y]{the vertical offset of the 'hotspot' of the cursor}
  @return{A new @class{gdk-cursor} object.}
  @begin{short}
    Creates a new cursor from a @arg{pixbuf}.
  @end{short}

  Not all GDK backends support RGBA cursors. If they are not supported, a
  monochrome approximation will be displayed. The functions
  @fun{gdk-display-supports-cursor-alpha} and
  @fun{gdk-display-supports-cursor-color} can be used to determine whether RGBA
  cursors are supported; the functions @fun{gdk-display-get-default-cursor-size}
  and @fun{gdk-display-get-maximal-cursor-size} give information about cursor
  sizes.

  If @arg{x} or @arg{y} are -1, the @arg{pixbuf} must have options named
  \"x_hot\" and \"y_hot\", resp., containing integer values between 0 and the
  width resp. height of the pixbuf. Since 3.0.

  On the X backend, support for RGBA cursors requires a sufficently new
  version of the X Render extension.

  Since 2.4
  @see-class{gdk-cursor}
  @see-class{gdk-display}
  @see-class{gdk-display}
  @see-function{gdk-display-supports-cursor-alpha}
  @see-function{gdk-display-supports-cursor-color}
  @see-function{gdk-display-get-default-cursor-size}
  @see-function{gdk-display-get-maximal-cursor-size}"
  (display (g-object gdk-display))
  (pixbuf (g-object gdk-pixbuf))
  (x :int)
  (y :int))

(export 'gdk-cursor-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_from_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_new_from_name" gdk-cursor-new-from-name)
    (g-object gdk-cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[display]{the @class{gdk-display} object for which the cursor will
    be created}
  @argument[name]{the name of the cursor}
  @return{A new @class{gdk-cursor} object, or @code{nil} if there is no cursor
    with the given @arg{name}.}
  @begin{short}
    Creates a new cursor by looking up @arg{name} in the current cursor theme.
  @end{short}

  Since 2.8
  @see-class{gdk-cursor}
  @see-class{gdk-display}"
  (display (g-object gdk-display))
  (name :string))

(export 'gdk-cursor-new-from-name)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_new_for_display" gdk-cursor-new-for-display)
    (g-object gdk-cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[display]{the @class{gdk-display} object for which the cursor will
    be created}
  @argument[cursor-type]{cursor to create from a value of the
    @symbol{gdk-cursor-type} enumeration}
  @return{A new @class{gdk-cursor} object.}
  @begin{short}
    Creates a new cursor from the set of builtin cursors.
  @end{short}
  Some useful ones are:
  @begin{pre}
     :right-ptr (right-facing arrow)
     :crosshair (crosshair)
     :xterm (I-beam)
     :watch (busy)
     :fleur (for moving objects)
     :hand1 (a right-pointing hand)
     :hand2 (a left-pointing hand)
     :left-side (resize left side)
     :right-side (resize right side)
     :top-left-corner (resize northwest corner)
     :top-right-corner (resize northeast corner)
     :bottom-left-corner (resize southwest corner)
     :bottom-right-corner (resize southeast corner)
     :top-side (resize top side)
     :bottom-side (resize bottom side)
     :sb-h-double-arrow (move vertical splitter)
     :sb-v-double-arrow (move horizontal splitter)
     :blank-cursor (Blank cursor. Since 2.16)
  @end{pre}
  Since 2.2
  @see-class{gdk-cursor}
  @see-class{gdk-display}
  @see-symbol{gdk-cursor-type}"
  (display (g-object gdk-display))
  (cursor-type gdk-cursor-type))

(export 'gdk-cursor-new-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_display ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-cursor-get-display))

(defun gdk-cursor-get-display (cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[cursor]{a @class{gdk-cursor} object}
  @return{The @class{gdk-display} associated to @arg{cursor}.}
  @begin{short}
    Returns the display on which the @arg{cursor} is defined.
  @end{short}

  Since 2.2
  @see-class{gdk-cursor}
  @see-class{gdk-display}"
  (gdk-cursor-display cursor))

(export 'gdk-cursor-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_image ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_get_image" gdk-cursor-get-image) (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[cursor]{a @class{gdk-cursor} object}
  @return{A @class{gdk-pixbuf} representing @arg{cursor}, or @code{nil}.}
  @begin{short}
    Returns a @class{gdk-pixbuf} object with the image used to display the
    @arg{cursor}.
  @end{short}

  Note that depending on the capabilities of the windowing system and on the
  @arg{cursor}, GDK may not be able to obtain the image data. In this case,
  @code{nil} is returned.

  Since 2.8
  @see-class{gdk-cursor}
  @see-class{gdk-pixbuf}"
  (cursor (g-object gdk-cursor)))

(export 'gdk-cursor-get-image)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_cursor_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-cursor-get-cursor-type))

(defun gdk-cursor-get-cursor-type (cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[cursor]{a @class{gdk-cursor} object}
  @return{A value from the @symbol{gdk-cursor-type} enumeration.}
  @begin{short}
    Returns the cursor type for this @arg{cursor}.
  @end{short}

  Since 2.22
  @see-class{gdk-cursor}
  @see-symbol{gdk-cursor-type}"
  (gdk-cursor-cursor-type cursor))

(export 'gdk-cursor-get-cursor-type)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_ref ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-cursor-ref))

(defun gdk-cursor-ref (cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[cursor]{a @class{gdk-cursor} object}
  @return{Same @arg{cursor} that was passed in.}
  @subheading{Warning}
    @sym{gdk-cursor-ref} has been deprecated since version 3.0 and should not
    be used in newly-written code. Use the function @fun{g-object-ref} instead.

  @short{Adds a reference to @arg{cursor}.}
  @see-class{gdk-cursor}
  @see-function{g-object-ref}
  @see-function{gdk-cursor-unref}"
  (g-object-ref cursor))

(export 'gdk-cursor-ref)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_unref ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-cursor-unref))

(defun gdk-cursor-unref (cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[cursor]{a @class{gdk-cursor} object}
  @subheading{Warning}
    @sym{gdk-cursor-unref} has been deprecated since version 3.0 and should not
    be used in newly-written code. Use the function @fun{g-object-unref}
    instead.

  @begin{short}
    Removes a reference from @arg{cursor}, deallocating the @arg{cursor} if no
    references remain.
  @end{short}
  @see-class{gdk-cursor}
  @see-function{g-object-unref}
  @see-function{gdk-cursor-ref}"
  (g-object-unref cursor))

(export 'gdk-cursor-unref)

;;; --- End of file gdk.cursor.lisp --------------------------------------------
