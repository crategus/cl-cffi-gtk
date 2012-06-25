;;; ----------------------------------------------------------------------------
;;; gdk.cursor.lisp
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
;;;     gdk_cursor_ref                   * deprecated *
;;;     gdk_cursor_unref                 * deprecated *
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GdkCursor
;;;
;;; Properties
;;;
;;;   "cursor-type"              GdkCursorType        : Read / Write / Construct
;;;   "display"                  GdkDisplay*          : Read / Write / Construct
;;;
;;; Description
;;;
;;; These functions are used to create and destroy cursors. There is a number of
;;; standard cursors, but it is also possible to construct new cursors from
;;; pixbufs. There may be limitations as to what kinds of cursors can be
;;; constructed on a given display, see gdk_display_supports_cursor_alpha(),
;;; gdk_display_supports_cursor_color(), gdk_display_get_default_cursor_size()
;;; and gdk_display_get_maximal_cursor_size().
;;;
;;; Cursors by themselves are not very interesting, they must be be bound to a
;;; window for users to see them. This is done with gdk_window_set_cursor() or
;;; by setting the cursor member of the GdkWindowAttr struct passed to
;;; gdk_window_new().
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "cursor-type" property
;;;
;;;   "cursor-type"              GdkCursorType        : Read / Write / Construct
;;;
;;; Standard cursor type.
;;;
;;; Default value: GDK_X_CURSOR
;;;
;;; ----------------------------------------------------------------------------
;;; The "display" property
;;;
;;;   "display"                  GdkDisplay*          : Read / Write / Construct
;;;
;;; Display of this cursor.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkCursor
;;;
;;; typedef struct _GdkCursor GdkCursor;
;;;
;;; The GdkCursor structure represents a cursor. Its contents are private.
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque gdk-cursor "GdkCursor"
  :alloc (error "GdkCursor can not be created from Lisp side"))

(export (boxed-related-symbols 'gdk-cursor))

;;; ----------------------------------------------------------------------------
;;; enum GdkCursorType
;;;
;;; typedef enum {
;;;   GDK_X_CURSOR            = 0,
;;;   GDK_ARROW               = 2,
;;;   GDK_BASED_ARROW_DOWN    = 4,
;;;   GDK_BASED_ARROW_UP      = 6,
;;;   GDK_BOAT                = 8,
;;;   GDK_BOGOSITY            = 10,
;;;   GDK_BOTTOM_LEFT_CORNER  = 12,
;;;   GDK_BOTTOM_RIGHT_CORNER = 14,
;;;   GDK_BOTTOM_SIDE         = 16,
;;;   GDK_BOTTOM_TEE          = 18,
;;;   GDK_BOX_SPIRAL          = 20,
;;;   GDK_CENTER_PTR          = 22,
;;;   GDK_CIRCLE              = 24,
;;;   GDK_CLOCK               = 26,
;;;   GDK_COFFEE_MUG          = 28,
;;;   GDK_CROSS               = 30,
;;;   GDK_CROSS_REVERSE       = 32,
;;;   GDK_CROSSHAIR           = 34,
;;;   GDK_DIAMOND_CROSS       = 36,
;;;   GDK_DOT                 = 38,
;;;   GDK_DOTBOX              = 40,
;;;   GDK_DOUBLE_ARROW        = 42,
;;;   GDK_DRAFT_LARGE         = 44,
;;;   GDK_DRAFT_SMALL         = 46,
;;;   GDK_DRAPED_BOX          = 48,
;;;   GDK_EXCHANGE            = 50,
;;;   GDK_FLEUR               = 52,
;;;   GDK_GOBBLER             = 54,
;;;   GDK_GUMBY               = 56,
;;;   GDK_HAND1               = 58,
;;;   GDK_HAND2               = 60,
;;;   GDK_HEART               = 62,
;;;   GDK_ICON                = 64,
;;;   GDK_IRON_CROSS          = 66,
;;;   GDK_LEFT_PTR            = 68,
;;;   GDK_LEFT_SIDE           = 70,
;;;   GDK_LEFT_TEE            = 72,
;;;   GDK_LEFTBUTTON          = 74,
;;;   GDK_LL_ANGLE            = 76,
;;;   GDK_LR_ANGLE            = 78,
;;;   GDK_MAN                 = 80,
;;;   GDK_MIDDLEBUTTON        = 82,
;;;   GDK_MOUSE               = 84,
;;;   GDK_PENCIL              = 86,
;;;   GDK_PIRATE              = 88,
;;;   GDK_PLUS                = 90,
;;;   GDK_QUESTION_ARROW      = 92,
;;;   GDK_RIGHT_PTR           = 94,
;;;   GDK_RIGHT_SIDE          = 96,
;;;   GDK_RIGHT_TEE           = 98,
;;;   GDK_RIGHTBUTTON         = 100,
;;;   GDK_RTL_LOGO            = 102,
;;;   GDK_SAILBOAT            = 104,
;;;   GDK_SB_DOWN_ARROW       = 106,
;;;   GDK_SB_H_DOUBLE_ARROW   = 108,
;;;   GDK_SB_LEFT_ARROW       = 110,
;;;   GDK_SB_RIGHT_ARROW      = 112,
;;;   GDK_SB_UP_ARROW         = 114,
;;;   GDK_SB_V_DOUBLE_ARROW   = 116,
;;;   GDK_SHUTTLE             = 118,
;;;   GDK_SIZING              = 120,
;;;   GDK_SPIDER              = 122,
;;;   GDK_SPRAYCAN            = 124,
;;;   GDK_STAR                = 126,
;;;   GDK_TARGET              = 128,
;;;   GDK_TCROSS              = 130,
;;;   GDK_TOP_LEFT_ARROW      = 132,
;;;   GDK_TOP_LEFT_CORNER     = 134,
;;;   GDK_TOP_RIGHT_CORNER    = 136,
;;;   GDK_TOP_SIDE            = 138,
;;;   GDK_TOP_TEE             = 140,
;;;   GDK_TREK                = 142,
;;;   GDK_UL_ANGLE            = 144,
;;;   GDK_UMBRELLA            = 146,
;;;   GDK_UR_ANGLE            = 148,
;;;   GDK_WATCH               = 150,
;;;   GDK_XTERM               = 152,
;;;   GDK_LAST_CURSOR,
;;;   GDK_BLANK_CURSOR        = -2,
;;;   GDK_CURSOR_IS_PIXMAP    = -1
;;; } GdkCursorType;
;;;
;;; The standard cursors available.
;;;
;;; GDK_LAST_CURSOR
;;;     last cursor type
;;;
;;; GDK_BLANK_CURSOR
;;;     Blank cursor. Since 2.16
;;;
;;; GDK_CURSOR_IS_PIXMAP
;;;     type of cursors constructed with gdk_cursor_new_from_pixbuf()
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

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new ()
;;;
;;; GdkCursor * gdk_cursor_new (GdkCursorType cursor_type);
;;;
;;; Creates a new cursor from the set of builtin cursors for the default
;;; display. See gdk_cursor_new_for_display().
;;;
;;; To make the cursor invisible, use GDK_BLANK_CURSOR.
;;;
;;; cursor_type :
;;;     cursor to create
;;;
;;; Returns :
;;;     a new GdkCursor
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_new" gdk-cursor-new) (g-boxed-foreign gdk-cursor :return)
  (cursor-type gdk-cursor-type))

(export 'gdk-cursor-new)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_from_pixbuf ()
;;;
;;; GdkCursor * gdk_cursor_new_from_pixbuf (GdkDisplay *display,
;;;                                         GdkPixbuf *pixbuf,
;;;                                         gint x,
;;;                                         gint y);
;;;
;;; Creates a new cursor from a pixbuf.
;;;
;;; Not all GDK backends support RGBA cursors. If they are not supported, a
;;; monochrome approximation will be displayed. The functions
;;; gdk_display_supports_cursor_alpha() and gdk_display_supports_cursor_color()
;;; can be used to determine whether RGBA cursors are supported;
;;; gdk_display_get_default_cursor_size() and
;;; gdk_display_get_maximal_cursor_size() give information about cursor sizes.
;;;
;;; If x or y are -1, the pixbuf must have options named "x_hot" and "y_hot",
;;; resp., containing integer values between 0 and the width resp. height of the
;;; pixbuf. (Since: 3.0)
;;;
;;; On the X backend, support for RGBA cursors requires a sufficently new
;;; version of the X Render extension.
;;;
;;; display :
;;;     the GdkDisplay for which the cursor will be created
;;;
;;; pixbuf :
;;;     the GdkPixbuf containing the cursor image
;;;
;;; x :
;;;     the horizontal offset of the 'hotspot' of the cursor.
;;;
;;; y :
;;;     the vertical offset of the 'hotspot' of the cursor.
;;;
;;; Returns :
;;;     a new GdkCursor.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_new_from_pixbuf" gdk-cursor-new-from-pixbuf)
    (g-boxed-foreign gdk-cursor :return)
  (display (g-object gdk-display))
  (pixbuf (g-object gdk-pixbuf))
  (x :int)
  (y :int))

(export 'gdk-cursor-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_from_name ()
;;;
;;; GdkCursor * gdk_cursor_new_from_name (GdkDisplay *display,
;;;                                       const gchar *name);
;;;
;;; Creates a new cursor by looking up name in the current cursor theme.
;;;
;;; display :
;;;     the GdkDisplay for which the cursor will be created
;;;
;;; name :
;;;     the name of the cursor
;;;
;;; Returns :
;;;     a new GdkCursor, or NULL if there is no cursor with the given name
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_new_from_name" gdk-cursor-new-from-name)
    (g-boxed-foreign gdk-cursor :return)
  (display (g-object gdk-display))
  (name :string))

(export 'gdk-cursor-new-from-name)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_for_display ()
;;;
;;; GdkCursor * gdk_cursor_new_for_display (GdkDisplay *display,
;;;                                         GdkCursorType cursor_type);
;;;
;;; Creates a new cursor from the set of builtin cursors. Some useful ones are:
;;;
;;;     GDK_RIGHT_PTR (right-facing arrow)
;;;     GDK_CROSSHAIR (crosshair)
;;;     GDK_XTERM (I-beam)
;;;     GDK_WATCH (busy)
;;;     GDK_FLEUR (for moving objects)
;;;     GDK_HAND1 (a right-pointing hand)
;;;     GDK_HAND2 (a left-pointing hand)
;;;     GDK_LEFT_SIDE (resize left side)
;;;     GDK_RIGHT_SIDE (resize right side)
;;;     GDK_TOP_LEFT_CORNER (resize northwest corner)
;;;     GDK_TOP_RIGHT_CORNER (resize northeast corner)
;;;     GDK_BOTTOM_LEFT_CORNER (resize southwest corner)
;;;     GDK_BOTTOM_RIGHT_CORNER (resize southeast corner)
;;;     GDK_TOP_SIDE (resize top side)
;;;     GDK_BOTTOM_SIDE (resize bottom side)
;;;     GDK_SB_H_DOUBLE_ARROW (move vertical splitter)
;;;     GDK_SB_V_DOUBLE_ARROW (move horizontal splitter)
;;;     GDK_BLANK_CURSOR (Blank cursor). Since 2.16
;;;
;;; display :
;;;     the GdkDisplay for which the cursor will be created
;;;
;;; cursor_type :
;;;     cursor to create
;;;
;;; Returns :
;;;     a new GdkCursor
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_new_for_display" gdk-cursor-new-for-display)
    (g-boxed-foreign gdk-cursor :return)
  (display (g-object gdk-display))
  (cursor-type gdk-cursor-type))

(export 'gdk-cursor-new-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_display ()
;;;
;;; GdkDisplay * gdk_cursor_get_display (GdkCursor *cursor);
;;;
;;; Returns the display on which the GdkCursor is defined.
;;;
;;; cursor :
;;;     a GdkCursor.
;;;
;;; Returns :
;;;     the GdkDisplay associated to cursor
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gdk-cursor gdk-cursor-get-display
  :type (g-object gdk-display)
  :reader "gdk_cursor_get_display")

(export 'gdk-cursor-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_image ()
;;;
;;; GdkPixbuf * gdk_cursor_get_image (GdkCursor *cursor);
;;;
;;; Returns a GdkPixbuf with the image used to display the cursor.
;;;
;;; Note that depending on the capabilities of the windowing system and on the
;;; cursor, GDK may not be able to obtain the image data. In this case, NULL is
;;; returned.
;;;
;;; cursor :
;;;     a GdkCursor
;;;
;;; Returns :
;;;     a GdkPixbuf representing cursor, or NULL
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_get_image" gdk-cursor-get-image) (g-object gdk-pixbuf)
  (cursor (g-boxed-foreign gdk-cursor)))

(export 'gdk-cursor-get-image)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_cursor_type ()
;;;
;;; GdkCursorType gdk_cursor_get_cursor_type (GdkCursor *cursor);
;;;
;;; Returns the cursor type for this cursor.
;;;
;;; cursor :
;;;     a GdkCursor
;;;
;;; Returns :
;;;     a GdkCursorType
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gdk-cursor gdk-cursor-get-cursor-type
  :type gdk-cursor-type
  :reader "gdk_cursor_get_cursor_type")

(export 'gdk-cursor-get-cursor-type)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_ref ()
;;;
;;; GdkCursor * gdk_cursor_ref (GdkCursor *cursor);
;;;
;;; Warning
;;;
;;; gdk_cursor_ref has been deprecated since version 3.0 and should not be used
;;; in newly-written code. Use g_object_ref() instead
;;;
;;; Adds a reference to cursor.
;;;
;;; cursor :
;;;     a GdkCursor
;;;
;;; Returns :
;;;     Same cursor that was passed in
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_unref ()
;;;
;;; void gdk_cursor_unref (GdkCursor *cursor);
;;;
;;; Warning
;;;
;;; gdk_cursor_unref has been deprecated since version 3.0 and should not be
;;; used in newly-written code. Use g_object_unref() instead
;;;
;;; Removes a reference from cursor, deallocating the cursor if no references
;;; remain.
;;;
;;; cursor :
;;;     a GdkCursor
;;; ----------------------------------------------------------------------------


;;; --- End of file gdk.cursor.lisp --------------------------------------------
