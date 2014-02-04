;;; ----------------------------------------------------------------------------
;;; cairo.device.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.12.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2014 Dieter Kaiser
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
;;; cairo_font_face_t
;;;
;;; Base class for font faces
;;;
;;; Synopsis
;;;
;;;     cairo_font_face_t
;;;
;;;     cairo_font_face_reference
;;;     cairo_font_face_destroy
;;;     cairo_font_face_status
;;;
;;;     cairo_font_type_t
;;;
;;;     cairo_font_face_get_type
;;;     cairo_font_face_get_reference_count
;;;     cairo_font_face_set_user_data
;;;     cairo_font_face_get_user_data
;;;
;;; Description
;;;
;;; cairo_font_face_t represents a particular font at a particular weight,
;;; slant, and other characteristic but no size, transformation, or size.
;;;
;;; Font faces are created using font-backend-specific constructors, typically
;;; of the form cairo_backend_font_face_create(), or implicitly using the toy
;;; text API by way of cairo_select_font_face(). The resulting face can be
;;; accessed using cairo_get_font_face().
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-font-face-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-font-face-t atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'cairo-font-face-t atdoc:*external-symbols*)
 "@version{2014-2-4}
  @begin{short}
    A @sym{cairo-font-face-t} specifies all aspects of a font other than the
    size or font matrix (a font matrix is used to distort a font by sheering it
    or scaling it unequally in the two directions). A font face can be set on a
    @symbol{cairo-t} by using the function @fun{cairo-set-font-face}; the size
    and font matrix are set with the functions @fun{cairo-set-font-size} and
    @fun{cairo-set-font-matrix}.
  @end{short}

  There are various types of font faces, depending on the font backend they
  use. The type of a font face can be queried using the function
  @fun{cairo-font-face-get-type}.

  Memory management of @sym{cairo-font-face-t} is done with the functions
  @fun{cairo-font-face-reference} and @fun{cairo-font-face-destroy}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-fun{cairo-set-font-face}
  @see-function{cairo-set-font-size}
  @see-function{cairo-set-font-matrix}
  @see-function{cairo-font-face-get-type}")

(export 'cairo-font-face-t)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_reference ()
;;;
;;; cairo_font_face_t * cairo_font_face_reference (cairo_font_face_t *font_face)
;;;
;;; Increases the reference count on font_face by one. This prevents font_face
;;; from being destroyed until a matching call to cairo_font_face_destroy() is
;;; made.
;;;
;;; The number of references to a cairo_font_face_t can be get using
;;; cairo_font_face_get_reference_count().
;;;
;;; font_face :
;;;     a cairo_font_face_t, (may be NULL in which case this function does
;;;     nothing).
;;;
;;; Returns :
;;;     the referenced cairo_font_face_t.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_destroy ()
;;;
;;; void cairo_font_face_destroy (cairo_font_face_t *font_face);
;;;
;;; Decreases the reference count on font_face by one. If the result is zero,
;;; then font_face and all associated resources are freed. See
;;; cairo_font_face_reference().
;;;
;;; font_face :
;;;     a cairo_font_face_t
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_status ()
;;;
;;; cairo_status_t cairo_font_face_status (cairo_font_face_t *font_face);
;;;
;;; Checks whether an error has previously occurred for this font face
;;;
;;; font_face :
;;;     a cairo_font_face_t
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or another error such as CAIRO_STATUS_NO_MEMORY.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_font_type_t
;;;
;;; typedef enum {
;;;     CAIRO_FONT_TYPE_TOY,
;;;     CAIRO_FONT_TYPE_FT,
;;;     CAIRO_FONT_TYPE_WIN32,
;;;     CAIRO_FONT_TYPE_QUARTZ,
;;;     CAIRO_FONT_TYPE_USER
;;; } cairo_font_type_t;
;;;
;;; cairo_font_type_t is used to describe the type of a given font face or
;;; scaled font. The font types are also known as "font backends" within cairo.
;;;
;;; The type of a font face is determined by the function used to create it,
;;; which will generally be of the form cairo_type_font_face_create(). The font
;;; face type can be queried with cairo_font_face_get_type()
;;;
;;; The various cairo_font_face_t functions can be used with a font face of any
;;; type.
;;;
;;; The type of a scaled font is determined by the type of the font face passed
;;; to cairo_scaled_font_create(). The scaled font type can be queried with
;;; cairo_scaled_font_get_type()
;;;
;;; The various cairo_scaled_font_t functions can be used with scaled fonts of
;;; any type, but some font backends also provide type-specific functions that
;;; must only be called with a scaled font of the appropriate type. These
;;; functions have names that begin with cairo_type_scaled_font() such as
;;; cairo_ft_scaled_font_lock_face().
;;;
;;; The behavior of calling a type-specific function with a scaled font of the
;;; wrong type is undefined.
;;;
;;; New entries may be added in future versions.
;;;
;;; CAIRO_FONT_TYPE_TOY
;;;     The font was created using cairo's toy font api (Since: 1.2)
;;;
;;; CAIRO_FONT_TYPE_FT
;;;     The font is of type FreeType (Since: 1.2)
;;;
;;; CAIRO_FONT_TYPE_WIN32
;;;     The font is of type Win32 (Since: 1.2)
;;;
;;; CAIRO_FONT_TYPE_QUARTZ
;;;     The font is of type Quartz (Since: 1.6, in 1.2 and 1.4 it was named
;;;     CAIRO_FONT_TYPE_ATSUI)
;;;
;;; CAIRO_FONT_TYPE_USER
;;;     The font was create using cairo's user font api (Since: 1.8)
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_get_type ()
;;;
;;; cairo_font_type_t cairo_font_face_get_type (cairo_font_face_t *font_face);
;;;
;;; This function returns the type of the backend used to create a font face.
;;; See cairo_font_type_t for available types.
;;;
;;; font_face :
;;;     a font face
;;;
;;; Returns :
;;;     The type of font_face.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_get_reference_count ()
;;;
;;; unsigned int cairo_font_face_get_reference_count
;;;                                              (cairo_font_face_t *font_face);
;;;
;;; Returns the current reference count of font_face.
;;;
;;; font_face :
;;;     a cairo_font_face_t
;;;
;;; Returns :
;;;     the current reference count of font_face. If the object is a nil object,
;;;     0 will be returned.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_set_user_data ()
;;;
;;; cairo_status_t cairo_font_face_set_user_data
;;;                                           (cairo_font_face_t *font_face,
;;;                                            const cairo_user_data_key_t *key,
;;;                                            void *user_data,
;;;                                            cairo_destroy_func_t destroy);
;;;
;;; Attach user data to font_face. To remove user data from a font face, call
;;; this function with the key that was used to set it and NULL for data.
;;;
;;; font_face :
;;;     a cairo_font_face_t
;;;
;;; key :
;;;     the address of a cairo_user_data_key_t to attach the user data to
;;;
;;; user_data :
;;;     the user data to attach to the font face
;;;
;;; destroy :
;;;     a cairo_destroy_func_t which will be called when the font face is
;;;     destroyed or when new user data is attached using the same key.
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a slot could not be
;;;     allocated for the user data.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_get_user_data ()
;;;
;;; void * cairo_font_face_get_user_data (cairo_font_face_t *font_face,
;;;                                       const cairo_user_data_key_t *key);
;;;
;;; Return user data previously attached to font_face using the specified key.
;;; If no user data has been attached with the given key this function returns
;;; NULL.
;;;
;;; font_face :
;;;     a cairo_font_face_t
;;;
;;; key :
;;;     the address of the cairo_user_data_key_t the user data was attached to
;;;
;;; Returns :
;;;     the user data previously attached or NULL.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.font-face.lisp ---------------------------------------
