;;; ----------------------------------------------------------------------------
;;; atdoc-cairo.package.lisp
;;;
;;; Documentation strings for the library Cairo.
;;;
;;; The documentation has been copied from the Cairo Reference Manual
;;; for Cairo 1.12.2. See http://cairographics.org
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(in-package :cairo)

(setf (documentation (find-package :cairo) t)
 "@em{cairo} is a software library used to provide a vector graphics-based,
  device-independent API for software developers. It is designed to provide
  primitives for 2-dimensional drawing across a number of different backends.
  @em{cairo} is designed to use hardware acceleration when available.

  This is the API documentation of a Lisp binding to @em{cairo}. Only a few
  types and functions are implemented which are needed to compile the
  Lisp bindung to GTK+.
  @begin[Drawing - cairo-t]{section}
    The @em{cairo} drawing context.

    Description

    @symbol{cairo-t} is the main object used when drawing with @em{cairo}. To
    draw with @em{cairo}, you create a @symbol{cairo-t}, set the target surface,
    and drawing options for the @symbol{cairo-t}, create shapes with functions
    like @code{cairo_move_to()} and @code{cairo_line_to()}, and then draw shapes
    with @code{cairo_stroke()} or @fun{cairo-fill}.
 
    @symbol{cairo-t}'s can be pushed to a stack via @code{cairo_save()}. They
    may then safely be changed, without losing the current state. Use
    @code{cairo_restore()} to restore to the saved state.
  
    @about-symbol{cairo-t}
    @about-function{cairo-create}
    @about-function{cairo-reference}
    @about-function{cairo-destroy}
    @about-function{cairo-status}
    @about-function{cairo-save}
    @about-function{cairo-restore}
    @about-function{cairo-get-target}
    @about-function{cairo-push-group}
    @about-function{cairo-push-group-with-content}
    @about-function{cairo-pop-group}
    @about-function{cairo-pop-group-to-source}
    @about-function{cairo-get-group-target}
    @about-function{cairo-set-source-rgb}
    @about-function{cairo-set-source-rgba}
    @about-function{cairo-set-source}
    @about-function{cairo-set-source-surface}
    @about-function{cairo-get-source}
    @about-symbol{cairo-antialias-t}
    @about-function{cairo-set-antialias}
    @about-function{cairo-get-antialias}
    @about-function{cairo-set-dash}
    @about-function{cairo-get-dash-count}
    @about-function{cairo-get-dash}
    @about-symbol{cairo-fill-rule-t}
    @about-function{cairo-set-fill-rule}
    @about-function{cairo-get-fill-rule}
    @about-symbol{cairo-line-cap-t}
    @about-function{cairo-set-line-cap}
    @about-function{cairo-get-line-cap}
    @about-symbol{cairo-line-join-t}
    @about-function{cairo-set-line-join}
    @about-function{cairo-get-line-join}
    @about-function{cairo-set-line-width}
    @about-function{cairo-get-line-width}
    @about-function{cairo-set-miter-limit}
    @about-function{cairo-get-miter-limit}
    @about-symbol{cairo-operator-t}
    @about-function{cairo-set-operator}
    @about-function{cairo-get-operator}
    @about-function{cairo-set-tolerance}
    @about-function{cairo-get-tolerance}
    @about-function{cairo-clip}
    @about-function{cairo-clip-preserve}
    @about-function{cairo-clip-extents}
    @about-function{cairo-in-clip}
    @about-function{cairo-reset-clip}
    @about-function{cairo-rectangle-t}
    @about-function{cairo-rectangle-list-t}
    @about-function{cairo-rectangle-list-destroy}
    @about-function{cairo-copy-clip-rectangle-list}
    @about-function{cairo-fill}
    @about-function{cairo-fill-preserve}
    @about-function{cairo-fill-extents}
    @about-function{cairo-in-fill}
    @about-function{cairo-mask}
    @about-function{cairo-mask-surface}
    @about-function{cairo-paint}
    @about-function{cairo-paint-with-alpha}
    @about-function{cairo-stroke}
    @about-function{cairo-stroke-preserve}
    @about-function{cairo-stroke-extents}
    @about-function{cairo-in-stroke}
    @about-function{cairo-copy-page}
    @about-function{cairo-show-page}
    @about-function{cairo-get-reference-count}
    @about-function{cairo-set-user-data}
    @about-function{cairo-get-user-data}
  @end{section}
 ")

;;; --- End of file atdoc-cairo.package.lisp -----------------------------------
