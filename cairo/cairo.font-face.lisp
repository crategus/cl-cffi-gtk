;;; ----------------------------------------------------------------------------
;;; cairo.font-face.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2014 - 2020 Dieter Kaiser
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
;;;     Base class for font faces
;;;
;;; Types and Values
;;;
;;;     cairo_font_face_t
;;;     cairo_font_type_t
;;;
;;; Functions
;;;
;;;     cairo_font_face_reference
;;;     cairo_font_face_destroy
;;;     cairo_font_face_status
;;;
;;;     cairo_font_face_get_type
;;;     cairo_font_face_get_reference_count
;;;     cairo_font_face_set_user_data
;;;     cairo_font_face_get_user_data
;;;
;;; Description
;;;
;;;     cairo_font_face_t represents a particular font at a particular weight,
;;;     slant, and other characteristic but no size, transformation, or size.
;;;
;;;     Font faces are created using font-backend-specific constructors,
;;;     typically of the form cairo_backend_font_face_create(), or implicitly
;;;     using the toy text API by way of cairo_select_font_face(). The resulting
;;;     face can be accessed using cairo_get_font_face().
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-font-face-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-font-face-t atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'cairo-font-face-t atdoc:*external-symbols*)
 "@version{2020-12-15}
  @begin{short}
    A @sym{cairo-font-face-t} structure specifies all aspects of a font other
    than the size or font matrix (a font matrix is used to distort a font by
    sheering it or scaling it unequally in the two directions).
  @end{short}
  A font face can be set on a Cairo context by using the function
  @fun{cairo-set-font-face}; the size and font matrix are set with the
  functions @fun{cairo-set-font-size} and @fun{cairo-set-font-matrix}.

  There are various types of font faces, depending on the font backend they
  use. The type of a font face can be queried using the function
  @fun{cairo-font-face-get-type}.

  Memory management of the @sym{cairo-font-face-t} structure is done with the
  functions @fun{cairo-font-face-reference} and @fun{cairo-font-face-destroy}.
  @see-symbol{cairo-t}
  @see-fun{cairo-set-font-face}
  @see-function{cairo-set-font-size}
  @see-function{cairo-set-font-matrix}
  @see-function{cairo-font-face-get-type}")

(export 'cairo-font-face-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_font_type_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-font-type-t
  :toy
  :ft
  :win32
  :quartz
  :user)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-font-type-t atdoc:*symbol-name-alias*)
      "CEnum"
      (gethash 'cairo-font-type-t atdoc:*external-symbols*)
 "@version{2020-12-28}
  @begin{short}
    The @sym{cairo-font-type-t} enumeration is used to describe the type of a
    given font face or scaled font.
  @end{short}
  The font types are also known as \"font backends\" within Cairo.

  The type of a font face is determined by the function used to create it,
  which will generally be of the form @code{cairo-type-font-face-create}. The
  font face type can be queried with the function
  @fun{cairo-font-face-get-type}.

  The various @code{cairo-font-face-t} functions can be used with a font face
  of any type.

  The type of a scaled font is determined by the type of the font face passed
  to the function @fun{cairo-scaled-font-create}. The scaled font type can be
  queried with the function @fun{cairo-scaled-font-get-type}.

  The various @code{cairo-scaled-font-t} functions can be used with scaled fonts
  of any type, but some font backends also provide type-specific functions that
  must only be called with a scaled font of the appropriate type. These
  functions have names that begin with @code{cairo-type-scaled-font} such as the
  function @fun{cairo-ft-scaled-font-lock-face}.

  The behavior of calling a type-specific function with a scaled font of the
  wrong type is undefined.

  New entries may be added in future versions.
  @begin{pre}
(defcenum cairo-font-type-t
  :toy
  :ft
  :win32
  :quartz
  :user)
  @end{pre}
  @begin[code]{table}
    @entry[:toy]{The font was created using Cairo's toy font api.}
    @entry[:ft]{The font is of type FreeType.}
    @entry[:win32]{The font is of type Win32.}
    @entry{:quartz]{The font is of type Quartz.}
    @entry[:user]{The font was create using Cairo's user font api.}
  @end{table}
  @see-symbol{cairo-font-face-t}
  @see-function{cairo-font-face-get-type}
  @see-function{cairo-scaled-font-create}
  @see-function{cairo-scaled-font-get-type}")

(export 'cairo-font-type-t)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_reference ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_face_reference" cairo-font-face-reference)
    (:pointer (:struct cairo-font-face-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[font-face]{a @symbol{cairo-font-face-t} instance, may be NULL in
    which case this function does nothing}
  @return{The referenced @symbol{cairo-font-face-t} instance.}
  @begin{short}
    Increases the reference count on @arg{font-face} by one.
  @end{short}
  This prevents @arg{font-face} from being destroyed until a matching call to
  the function @fun{cairo-font-face-destroy} is made.

  The number of references to a @symbol{cairo-font-face-t} instance can be get
  using the function @fun{cairo-font-face-get-reference-count}.
  @see-symbol{cairo-font-face-t}
  @see-function{cairo-font-face-destroy}
  @see-function{cairo-font-face-get-reference-count}"
  (font-face (:pointer (:struct cairo-font-face-t))))

(export 'cairo-font-face-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_face_destroy" cairo-font-face-destroy) :void
  #+cl-cffi-gtk-documentation
  "@version{2020-12-28}
  @argument[font-face]{a @symbol{cairo-font-face-t} instance}
  @begin{short}
    Decreases the reference count on @arg{font-face} by one.
  @end{short}
  If the result is zero, then @arg{font-face} and all associated resources are
  freed. See the function @fun{cairo-font-face-reference}.
  @see-symbol{cairo-font-face-t}
  @see-function{cairo-font-face-reference}"
  (font-face (:pointer (:struct cairo-font-face-t))))

(export 'cairo-font-face-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_status ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_face_status" cairo-font-face-status) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[font-face]{a @symbol{cairo-font-face-t} instance}
  @begin{return}
    @code{:success} or another error such as @code{:no-memory}.
  @end{return}
  @begin{short}
    Checks whether an error has previously occurred for this font face.
  @end{short}
  @see-symbol{cairo-font-face-t}"
  (font-face (:pointer (:struct cairo-font-face-t))))

(export 'cairo-font-face-status)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_get_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_face_get_type" cairo-font-face-get-type) cairo-font-type-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[font-face]{a @symbol{cairo-font-face-t} font face}
  @return{The @symbol{cairo-font-type-t} type of @arg{font-face}.}
  @begin{short}
    This function returns the type of the backend used to create a font face.
  @end{short}
  See the @symbol{cairo-font-type-t} enumeration for available types.
  @see-symbol{cairo-font-face-t}"
  (font-face (:pointer (:struct cairo-font-face-t))))

(export 'cairo-font-face-get-type)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_get_reference_count ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_face_get_reference_count"
           cairo-font-face-get-reference-count) :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[font-face]{a @symbol{cairo-font-face-t} instance}
  @begin{return}
    The current reference count of @arg{font-face}. If the object is a nil
    object, 0 will be returned.
  @end{return}
  @begin{short}
    Returns the current reference count of @arg{font-face}.
  @end{short}
  @see-symbol{cairo-font-face-t}"
  (font-face (:pointer (:struct cairo-font-face-t))))

(export 'cairo-font-face-get-reference-count)

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
