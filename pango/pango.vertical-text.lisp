;;; ----------------------------------------------------------------------------
;;; pango.vertical-text.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.48 and modified to document the Lisp binding to the Pango library.
;;; See <http://www.pango.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Vertical Text
;;;
;;;     Laying text out in vertical directions
;;;
;;; Types and Values
;;;
;;;     PangoGravity
;;;     PangoGravityHint
;;;
;;; Functions
;;;
;;;     PANGO_GRAVITY_IS_IMPROPER
;;;     PANGO_GRAVITY_IS_VERTICAL
;;;     pango_gravity_get_for_matrix
;;;     pango_gravity_get_for_script
;;;     pango_gravity_get_for_script_and_width
;;;     pango_gravity_to_rotation
;;;
;;; Object Hierarchy
;;;
;;;     GEnum
;;;     ├── PangoGravity
;;;     ╰── PangoGravityHint
;;;
;;; Description
;;;
;;; Since 1.16, Pango is able to correctly lay vertical text out. In fact, it
;;; can set layouts of mixed vertical and non-vertical text. This section
;;; describes the types used for setting vertical text parameters.
;;;
;;; The way this is implemented is through the concept of gravity. Gravity of
;;; normal Latin text is south. A gravity value of east means that glyphs will
;;; be rotated ninety degrees counterclockwise. So, to render vertical text one
;;; needs to set the gravity and rotate the layout using the matrix machinery
;;; already in place. This has the huge advantage that most algorithms working
;;; on a PangoLayout do not need any change as the assumption that lines run in
;;; the X direction and stack in the Y direction holds even for vertical text
;;; layouts.
;;;
;;; Applications should only need to set base gravity on PangoContext in use,
;;; and let Pango decide the gravity assigned to each run of text. This
;;; automatically handles text with mixed scripts. A very common use is to set
;;; the context base gravity to auto using pango_context_set_base_gravity() and
;;; rotate the layout normally. Pango will make sure that Asian languages take
;;; the right form, while other scripts are rotated normally.
;;;
;;; The correct way to set gravity on a layout is to set it on the context
;;; associated with it using pango_context_set_base_gravity(). The context of a
;;; layout can be accessed using pango_layout_get_context(). The currently set
;;; base gravity of the context can be accessed using
;;; pango_context_get_base_gravity() and the resolved gravity of it using
;;; pango_context_get_gravity(). The resolved gravity is the same as the base
;;; gravity for the most part, except that if the base gravity is set to
;;; PANGO_GRAVITY_AUTO, the resolved gravity will depend on the current matrix
;;; set on context, and is derived using pango_gravity_get_for_matrix().
;;;
;;; The next thing an application may want to set on the context is the gravity
;;; hint. A PangoGravityHint instructs how different scripts should react to
;;; the set base gravity.
;;;
;;; Font descriptions have a gravity property too, that can be set using
;;; pango_font_description_set_gravity() and accessed using
;;; pango_font_description_get_gravity(). However, those are rarely useful from
;;; application code and are mainly used by PangoLayout internally.
;;;
;;; Last but not least, one can create PangoAttributes for gravity and gravity
;;; hint using pango_attr_gravity_new() and pango_attr_gravity_hint_new().
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; enum PangoGravity
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoGravity" pango-gravity
  (:export t
   :type-initializer "pango_gravity_get_type")
  (:south 0)
  (:east 1)
  (:north 2)
  (:west 3)
  (:auto 4))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-gravity atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'pango-gravity atdoc:*external-symbols*)
 "@version{2021-1-5}
  @begin{short}
    The @sym{pango-gravity} enumeration represents the orientation of glyphs in
    a segment of text.
  @end{short}
  This is useful when rendering vertical text layouts. In those situations, the
  layout is rotated using a non-identity @symbol{pango-matrix}, and then glyph
  orientation is controlled using @sym{pango-gravity}. Not every value in this
  enumeration makes sense for every usage of @sym{pango-gravity}. For example,
  @code{:auto} only can be passed to the function
  @fun{pango-context-base-gravity} and can only be returned by the function
  @fun{pango-context-base-gravity}.
  @begin{pre}
(define-g-enum \"PangoGravity\" pango-gravity
  (:export t
   :type-initializer \"pango_gravity_get_type\")
  (:south 0)
  (:east 1)
  (:north 2)
  (:west 3)
  (:auto 4))
  @end{pre}
  @begin[code]{table}
    @entry[:south]{Glyphs stand upright (default).}
    @entry[:east]{Glyphs are rotated 90 degrees clockwise.}
    @entry[:north]{Glyphs are upside-down.}
    @entry[:west]{Glyphs are rotated 90 degrees counter-clockwise.}
    @entry[:auto]{Gravity is resolved from the context matrix.}
  @end{table}
  @see-symbol{pango-gravity-hint}
  @see-class{pango-matrix}
  @see-function{pango-context-base-gravity}")

;;; ----------------------------------------------------------------------------
;;; enum PangoGravityHint
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoGravityHint" pango-gravity-hint
  (:export t
   :type-initializer "pango_gravity_hint_get_type")
  (:natural 0)
  (:strong 1)
  (:line 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-gravity-hint atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'pango-gravity-hint atdoc:*external-symbols*)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-5}
  @begin{short}
    The @sym{pango-gravity-hint} enumeration defines how horizontal scripts
    should behave in a vertical context.
  @end{short}
  That is, English excerpt in a vertical paragraph for example.
  @begin{pre}
(define-g-enum \"PangoGravityHint\" pango-gravity-hint
  (:export t
   :type-initializer \"pango_gravity_hint_get_type\")
  (:natural 0)
  (:strong 1)
  (:line 2))
  @end{pre}
  @begin[code]{table}
    @entry[:natural]{Scripts will take their natural gravity based on the base
      gravity and the script. This is the default.}
    @entry[:strong]{Always use the base gravity set, regardless of the script.}
    @entry[:line]{For scripts not in their natural direction (e.g. Latin in East
      gravity), choose per-script gravity such that every script respects the
      line progression. This means, Latin and Arabic will take opposite
      gravities and both flow top-to-bottom for example.}
  @end{table}
  @see-symbol{pango-gravity}")

;;; ----------------------------------------------------------------------------
;;; PANGO_GRAVITY_IS_IMPROPER()
;;;
;;; #define PANGO_GRAVITY_IS_IMPROPER(gravity)
;;;
;;; Whether a PangoGravity represents a gravity that results in reversal of
;;; text direction.
;;;
;;; gravity :
;;;     the PangoGravity to check
;;;
;;; Returns :
;;;     TRUE if gravity is PANGO_GRAVITY_WEST or PANGO_GRAVITY_NORTH, FALSE
;;;     otherwise.
;;;
;;; Since 1.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_GRAVITY_IS_VERTICAL()
;;;
;;; #define PANGO_GRAVITY_IS_VERTICAL(gravity)
;;;
;;; Whether a PangoGravity represents vertical writing directions.
;;;
;;; gravity :
;;;     the PangoGravity to check
;;;
;;; Returns :
;;;     TRUE if gravity is PANGO_GRAVITY_EAST or PANGO_GRAVITY_WEST, FALSE
;;;     otherwise.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_gravity_get_for_matrix () -> pango-gravity-for-matrix
;;; ----------------------------------------------------------------------------

(defcfun ("pango_gravity_get_for_matrix" pango-gravity-for-matrix) pango-gravity
 #+cl-cffi-gtk-documentation
 "@version{2021-1-5}
  @argument[matrix]{a @class{pango-matrix} instance}
  @begin{return}
    The gravity of @arg{matrix}, which will never be @code{:auto}, or
    @code{:south} if @arg{matrix} is @code{nil}.
  @end{return}
  @begin{short}
    Finds the gravity that best matches the rotation component in a
    @class{pango-matrix} instance.
  @end{short}
  @see-class{pango-matrix}"
  (matrix (g-boxed-foreign pango-matrix)))

(export 'pango-gravity-for-matrix)

;;; ----------------------------------------------------------------------------
;;; pango_gravity_get_for_script () -> pango-gravity-for-script
;;; ----------------------------------------------------------------------------

(defcfun ("pango_gravity_get_for_script" pango-gravity-for-script) pango-gravity
 #+cl-cffi-gtk-documentation
 "@version{2021-1-5}
  @argument[script]{a @symbol{pango-script} value to query}
  @argument[base-gravity]{a base @symbol{pango-gravity} value of the paragraph}
  @argument[hint]{a @symbol{pango-gravity-hint} orientation hint}
  @begin{return}
    Resolved @symbol{pango-gravity} value suitable to use for a run of text
    with @arg{script}.
  @end{return}
  @begin{short}
    Based on the script, base gravity, and hint, returns actual gravity to use
    in laying out a single PangoItem.
  @end{short}

  If @arg{base-gravity} is @code{:auto}, it is first replaced with the
  preferred gravity of @arg{script}. To get the preferred gravity of a script,
  pass @code{:auto} and @code{:strong} in.
  @see-symbol{pango-script}
  @see-symbol{pango-gravity}
  @see-symbol{pango-gravity-hint}"
  (script pango-script)
  (base-gravity pango-gravity)
  (hint pango-gravity-hint))

(export 'pango-gravity-for-script)

;;; ----------------------------------------------------------------------------
;;; pango_gravity_get_for_script_and_width ()
;;;   -> pango-gravity-for-script-and-width
;;; ----------------------------------------------------------------------------

(defcfun ("pango_gravity_get_for_script_and_width"
           pango-gravity-for-script-and-width) pango-gravity
 #+cl-cffi-gtk-documentation
 "@version{2021-1-5}
  @argument[script]{a @symbol{pango-script} value to query}
  @argument[wide]{@em{true} for wide characters as returned by
    @code{g_unichar_iswide()}}
  @argument[base-gravity]{a base @symbol{pango-gravity} value of the paragraph}
  @argument[hint]{a @symbol{pango-gravity-hint} orientation hint}
  @begin{return}
    Resolved @symbol{pango-gravity} value suitable to use for a run of text
    with @arg{script} and @arg{wide}.
  @end{return}
  @begin{short}
    Based on the script, East Asian width, base gravity, and hint, returns
    actual gravity to use in laying out a single character or PangoItem.
  @end{short}

  This function is similar to the function @fun{pango-gravity-for-script}
  except that this function makes a distinction between narrow/half-width and
  wide/full-width characters also. Wide/full-width characters always stand
  upright, that is, they always take the base gravity, whereas narrow/full-width
  characters are always rotated in vertical context.

  If @arg{base-gravity} is @code{:auto}, it is first replaced with the
  preferred gravity of @arg{script}.
  @see-symbol{pango-script}
  @see-symbol{pango-gravity}
  @see-symbol{pango-gravity-hint}
  @see-function{pango-gravity-for-script}"
  (script pango-script)
  (wide :boolean)
  (base-gravity pango-gravity)
  (hint pango-gravity-hint))

(export 'pango-gravity-for-script-and-width)

;;; ----------------------------------------------------------------------------
;;; pango_gravity_to_rotation ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_gravity_to_rotation" pango-gravity-to-rotation) :double
 #+cl-cffi-gtk-documentation
 "@version{2021-1-5}
  @argument[script]{a @symbol{pango-script} value to query}
  @begin{return}
    A double with the rotation value corresponding to @arg{gravity}.
  @end{return}
  @begin{short}
    Converts a @symbol{pangogravity} value to its natural rotation in radians.
  @end{short}
  @arg{gravity} should not be @code{:auto}.

  Note that the function @fun{pango-matrix-rotate} takes angle in degrees, not
  radians. So, to call the function @fun{pango-matrix-rotate} with the output
  of this function you should multiply it by (180 / pi).
  @see-symbol{pango-gravity}"
  (gravity pango-gravity))

(export 'pango-gravity-to-rotation)

;;; --- End of file pango.vertical-text.lisp -----------------------------------
