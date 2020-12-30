;;; ----------------------------------------------------------------------------
;;; cairo.freetype-font.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 Dieter Kaiser
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
;;; FreeType Fonts
;;;
;;;     Font support for FreeType
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_FT_FONT
;;;     CAIRO_HAS_FC_FONT
;;;     cairo_ft_synthesize_t
;;;
;;; Functions
;;;
;;;     cairo_ft_font_face_create_for_ft_face
;;;     cairo_ft_font_face_create_for_pattern
;;;     cairo_ft_font_options_substitute
;;;     cairo_ft_scaled_font_lock_face
;;;     cairo_ft_scaled_font_unlock_face
;;;     cairo_ft_font_face_get_synthesize
;;;     cairo_ft_font_face_set_synthesize
;;;     cairo_ft_font_face_unset_synthesize
;;;
;;; Description
;;;
;;; The FreeType font backend is primarily used to render text on GNU/Linux
;;; systems, but can be used on other platforms too.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_FT_FONT
;;;
;;; #define CAIRO_HAS_FT_FONT 1
;;;
;;; Defined if the FreeType font backend is available. This macro can be used
;;; to conditionally compile backend-specific code.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_FC_FONT
;;;
;;; #define CAIRO_HAS_FC_FONT 1
;;;
;;; Defined if the Fontconfig-specific functions of the FreeType font backend
;;; are available. This macro can be used to conditionally compile
;;; backend-specific code.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_ft_synthesize_t
;;;
;;; A set of synthesis options to control how FreeType renders the glyphs for a
;;; particular font face.
;;;
;;; Individual synthesis features of a cairo_ft_font_face_t can be set using
;;; cairo_ft_font_face_set_synthesize(), or disabled using
;;; cairo_ft_font_face_unset_synthesize(). The currently enabled set of
;;; synthesis options can be queried with cairo_ft_font_face_get_synthesize().
;;;
;;; Note: that when synthesizing glyphs, the font metrics returned will only be
;;; estimates.
;;;
;;; CAIRO_FT_SYNTHESIZE_BOLD :
;;;     Embolden the glyphs (redraw with a pixel offset)
;;;
;;; CAIRO_FT_SYNTHESIZE_OBLIQUE :
;;;     Slant the glyph outline by 12 degrees to the right.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ft_font_face_create_for_ft_face ()
;;;
;;; cairo_font_face_t *
;;; cairo_ft_font_face_create_for_ft_face (FT_Face face,
;;;                                        int load_flags);
;;;
;;; Creates a new font face for the FreeType font backend from a pre-opened
;;; FreeType face. This font can then be used with cairo_set_font_face() or
;;; cairo_scaled_font_create(). The cairo_scaled_font_t returned from
;;; cairo_scaled_font_create() is also for the FreeType backend and can be used
;;; with functions such as cairo_ft_scaled_font_lock_face(). Note that Cairo may
;;; keep a reference to the FT_Face alive in a font-cache and the exact lifetime
;;; of the reference depends highly upon the exact usage pattern and is subject
;;; to external factors. You must not call FT_Done_Face() before the last
;;; reference to the cairo_font_face_t has been dropped.
;;;
;;; As an example, below is how one might correctly couple the lifetime of the
;;; FreeType face object to the cairo_font_face_t.
;;;
;;; static const cairo_user_data_key_t key;
;;;
;;; font_face = cairo_ft_font_face_create_for_ft_face (ft_face, 0);
;;; status = cairo_font_face_set_user_data (font_face, &key,
;;;                                ft_face, (cairo_destroy_func_t) FT_Done_Face)
;;; if (status) {
;;;    cairo_font_face_destroy (font_face);
;;;    FT_Done_Face (ft_face);
;;;    return ERROR;
;;; }
;;;
;;; face :
;;;     A FreeType face object, already opened. This must be kept around until
;;;     the face's ref_count drops to zero and it is freed. Since the face may
;;;     be referenced internally to Cairo, the best way to determine when it is
;;;     safe to free the face is to pass a cairo_destroy_func_t to
;;;     cairo_font_face_set_user_data()
;;;
;;; load_flags :
;;;     flags to pass to FT_Load_Glyph when loading glyphs from the font. These
;;;     flags are OR'ed together with the flags derived from the
;;;     cairo_font_options_t passed to cairo_scaled_font_create(), so only a few
;;;     values such as FT_LOAD_VERTICAL_LAYOUT, and FT_LOAD_FORCE_AUTOHINT are
;;;     useful. You should not pass any of the flags affecting the load target,
;;;     such as FT_LOAD_TARGET_LIGHT.
;;;
;;; Returns :
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ft_font_face_create_for_pattern ()
;;;
;;; cairo_font_face_t *
;;; cairo_ft_font_face_create_for_pattern (FcPattern *pattern);
;;;
;;; Creates a new font face for the FreeType font backend based on a fontconfig
;;; pattern. This font can then be used with cairo_set_font_face() or
;;; cairo_scaled_font_create(). The cairo_scaled_font_t returned from
;;; cairo_scaled_font_create() is also for the FreeType backend and can be used
;;; with functions such as cairo_ft_scaled_font_lock_face().
;;;
;;; Font rendering options are represented both here and when you call
;;; cairo_scaled_font_create(). Font options that have a representation in a
;;; FcPattern must be passed in here; to modify FcPattern appropriately to
;;; reflect the options in a cairo_font_options_t, call
;;; cairo_ft_font_options_substitute().
;;;
;;; The pattern's FC_FT_FACE element is inspected first and if that is set, that
;;; will be the FreeType font face associated with the returned cairo font face.
;;; Otherwise the FC_FILE element is checked. If it's set, that and the value of
;;; the FC_INDEX element (defaults to zero) of pattern are used to load a font
;;; face from file.
;;;
;;; If both steps from the previous paragraph fails, pattern will be passed to
;;; FcConfigSubstitute, FcDefaultSubstitute, and finally FcFontMatch, and the
;;; resulting font pattern is used.
;;;
;;; If the FC_FT_FACE element of pattern is set, the user is responsible for
;;; making sure that the referenced FT_Face remains valid for the life time of
;;; the returned cairo_font_face_t. See cairo_ft_font_face_create_for_ft_face()
;;; for an example of how to couple the life time of the FT_Face to that of the
;;; cairo font-face.
;;;
;;; pattern :
;;;     A fontconfig pattern. Cairo makes a copy of the pattern if it needs to.
;;;     You are free to modify or free pattern after this call.
;;;
;;; Returns :
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ft_font_options_substitute ()
;;;
;;; void
;;; cairo_ft_font_options_substitute (const cairo_font_options_t *options,
;;;                                   FcPattern *pattern);
;;;
;;; Add options to a FcPattern based on a cairo_font_options_t font options
;;; object. Options that are already in the pattern, are not overridden, so you
;;; should call this function after calling FcConfigSubstitute() (the user's
;;; settings should override options based on the surface type), but before
;;; calling FcDefaultSubstitute().
;;;
;;; options :
;;;     a cairo_font_options_t object
;;;
;;; pattern :
;;;     an existing FcPattern
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ft_scaled_font_lock_face ()
;;;
;;; FT_Face
;;; cairo_ft_scaled_font_lock_face (cairo_scaled_font_t *scaled_font);
;;;
;;; cairo_ft_scaled_font_lock_face() gets the FT_Face object from a FreeType
;;; backend font and scales it appropriately for the font and applies OpenType
;;; font variations if applicable. You must release the face with
;;; cairo_ft_scaled_font_unlock_face() when you are done using it. Since the
;;; FT_Face object can be shared between multiple cairo_scaled_font_t objects,
;;; you must not lock any other font objects until you unlock this one. A count
;;; is kept of the number of times cairo_ft_scaled_font_lock_face() is called.
;;; cairo_ft_scaled_font_unlock_face() must be called the same number of times.
;;;
;;; You must be careful when using this function in a library or in a threaded
;;; application, because freetype's design makes it unsafe to call freetype
;;; functions simultaneously from multiple threads, (even if using distinct
;;; FT_Face objects). Because of this, application code that acquires an FT_Face
;;; object with this call must add its own locking to protect any use of that
;;; object, (and which also must protect any other calls into cairo as almost
;;; any cairo function might result in a call into the freetype library).
;;;
;;; scaled_font :
;;;     A cairo_scaled_font_t from the FreeType font backend. Such an object can
;;;     be created by calling cairo_scaled_font_create() on a FreeType backend
;;;     font face (see cairo_ft_font_face_create_for_pattern(),
;;;     cairo_ft_font_face_create_for_ft_face()).
;;;
;;; Returns :
;;;     The FT_Face object for font , scaled appropriately, or NULL if
;;;     scaled_font is in an error state (see cairo_scaled_font_status()) or
;;;     there is insufficient memory.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ft_scaled_font_unlock_face ()
;;;
;;; void
;;; cairo_ft_scaled_font_unlock_face (cairo_scaled_font_t *scaled_font);
;;;
;;; Releases a face obtained with cairo_ft_scaled_font_lock_face().
;;;
;;; scaled_font :
;;;     A cairo_scaled_font_t from the FreeType font backend. Such an object
;;;     can be created by calling cairo_scaled_font_create() on a FreeType
;;;     backend font face (see cairo_ft_font_face_create_for_pattern(),
;;;     cairo_ft_font_face_create_for_ft_face()).
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ft_font_face_get_synthesize ()
;;;
;;; unsigned int
;;; cairo_ft_font_face_get_synthesize (cairo_font_face_t *font_face);
;;;
;;; See cairo_ft_synthesize_t.
;;;
;;; font_face :
;;;     The cairo_ft_font_face_t object to query
;;;
;;; Returns :
;;;     the current set of synthesis options.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ft_font_face_set_synthesize ()
;;;
;;; void
;;; cairo_ft_font_face_set_synthesize (cairo_font_face_t *font_face,
;;;                                    unsigned int synth_flags);
;;;
;;; FreeType provides the ability to synthesize different glyphs from a base
;;; font, which is useful if you lack those glyphs from a true bold or oblique
;;; font. See also cairo_ft_synthesize_t.
;;;
;;; font_face :
;;;     The cairo_ft_font_face_t object to modify
;;;
;;; synth_flags :
;;;     the set of synthesis options to enable
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ft_font_face_unset_synthesize ()
;;;
;;; void
;;; cairo_ft_font_face_unset_synthesize (cairo_font_face_t *font_face,
;;;                                      unsigned int synth_flags);
;;;
;;; See cairo_ft_font_face_set_synthesize().
;;;
;;; font_face :
;;;     The cairo_ft_font_face_t object to modify
;;;
;;; synth_flags :
;;;     the set of synthesis options to disable
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.freetype-font.lisp -----------------------------------
