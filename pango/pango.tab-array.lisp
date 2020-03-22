;;; ----------------------------------------------------------------------------
;;; pango.tab-array.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.32.6 and modified to document the Lisp binding to the Pango
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;; Tab Stops
;;;
;;; Structures for storing tab stops
;;;
;;; Synopsis
;;;
;;;     PangoTabArray
;;;     PANGO_TYPE_TAB_ARRAY
;;;
;;;     PangoTabAlign
;;;     PANGO_TYPE_TAB_ALIGN
;;;
;;;     pango_tab_array_new
;;;     pango_tab_array_new_with_positions
;;;     pango_tab_array_copy
;;;     pango_tab_array_free
;;;     pango_tab_array_get_size
;;;     pango_tab_array_resize
;;;     pango_tab_array_set_tab
;;;     pango_tab_array_get_tab
;;;     pango_tab_array_get_tabs
;;;     pango_tab_array_get_positions_in_pixels
;;;
;;; Object Hierarchy
;;;
;;;   GBoxed
;;;    +----PangoTabArray
;;;
;;;   GEnum
;;;    +----PangoTabAlign
;;;
;;; Description
;;;
;;; Functions in this section are used to deal with PangoTabArray objects that
;;; can be used to set tab stop positions in a PangoLayout.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoTabArray
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque pango-tab-array "PangoTabArray"
  :alloc (%pango-tab-array-new 0 nil))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-tab-array atdoc:*class-name-alias*) "CStruct"
      (documentation 'pango-tab-array 'type)
 "@version{2013-8-10}
  @begin{short}
    A @sym{pango-tab-array} structure contains an array of tab stops. Each tab
    stop has an alignment and a position.
  @end{short}
  @begin{pre}
(define-g-boxed-opaque pango-tab-array \"PangoTabArray\"
  :alloc (%pango-tab-array-new))
  @end{pre}")

(export (boxed-related-symbols 'pango-tab-array))

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_TAB_ARRAY
;;;
;;; #define PANGO_TYPE_TAB_ARRAY (pango_tab_array_get_type ())
;;;
;;; The GObject type for PangoTabArray.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum PangoTabAlign
;;;
;;; typedef enum {
;;;   PANGO_TAB_LEFT
;;;
;;;   /* These are not supported now, but may be in the
;;;    * future.
;;;    *
;;;    *  PANGO_TAB_RIGHT,
;;;    *  PANGO_TAB_CENTER,
;;;    *  PANGO_TAB_NUMERIC
;;;    */
;;; } PangoTabAlign;
;;;
;;; A PangoTabAlign specifies where a tab stop appears relative to the text.
;;;
;;; PANGO_TAB_LEFT
;;;     the tab stop appears to the left of the text.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_TAB_ALIGN
;;;
;;; #define PANGO_TYPE_TAB_ALIGN (pango_tab_align_get_type ())
;;;
;;; The GObject type for PangoTabAlign.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_new ()
;;;
;;; PangoTabArray * pango_tab_array_new (gint initial_size,
;;;                                      gboolean positions_in_pixels);
;;;
;;; Creates an array of initial_size tab stops. Tab stops are specified in pixel
;;; units if positions_in_pixels is TRUE, otherwise in Pango units. All stops
;;; are initially at position 0.
;;;
;;; initial_size :
;;;     Initial number of tab stops to allocate, can be 0
;;;
;;; positions_in_pixels :
;;;     whether positions are in pixel units
;;;
;;; Returns :
;;;     the newly allocated PangoTabArray, which should be freed with
;;;     pango_tab_array_free().
;;; ----------------------------------------------------------------------------

(defcfun ("pango_tab_array_new" %pango-tab-array-new)
    (g-boxed-foreign pango-tab-array)
  (initial-size :int)
  (positions-in-pixels :boolean))

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_new_with_positions ()
;;;
;;; PangoTabArray * pango_tab_array_new_with_positions
;;;                                              (gint size,
;;;                                               gboolean positions_in_pixels,
;;;                                               PangoTabAlign first_alignment,
;;;                                               gint first_position,
;;;                                               ...);
;;;
;;; This is a convenience function that creates a PangoTabArray and allows you
;;; to specify the alignment and position of each tab stop. You must provide an
;;; alignment and position for size tab stops.
;;;
;;; size :
;;;     number of tab stops in the array
;;;
;;; positions_in_pixels :
;;;     whether positions are in pixel units
;;;
;;; first_alignment :
;;;     alignment of first tab stop
;;;
;;; first_position :
;;;     position of first tab stop
;;;
;;; ... :
;;;     additional alignment/position pairs
;;;
;;; Returns :
;;;     the newly allocated PangoTabArray, which should be freed with
;;;     pango_tab_array_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_copy ()
;;;
;;; PangoTabArray * pango_tab_array_copy (PangoTabArray *src);
;;;
;;; Copies a PangoTabArray
;;;
;;; src :
;;;     PangoTabArray to copy
;;;
;;; Returns :
;;;     the newly allocated PangoTabArray, which should be freed with
;;;     pango_tab_array_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_free ()
;;;
;;; void pango_tab_array_free (PangoTabArray *tab_array);
;;;
;;; Frees a tab array and associated resources.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_size ()
;;;
;;; gint pango_tab_array_get_size (PangoTabArray *tab_array);
;;;
;;; Gets the number of tab stops in tab_array.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; Returns :
;;;     the number of tab stops in the array.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_resize ()
;;;
;;; void pango_tab_array_resize (PangoTabArray *tab_array, gint new_size);
;;;
;;; Resizes a tab array. You must subsequently initialize any tabs that were
;;; added as a result of growing the array.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; new_size :
;;;     new size of the array
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_set_tab ()
;;;
;;; void pango_tab_array_set_tab (PangoTabArray *tab_array,
;;;                               gint tab_index,
;;;                               PangoTabAlign alignment,
;;;                               gint location);
;;;
;;; Sets the alignment and location of a tab stop. alignment must always be
;;; PANGO_TAB_LEFT in the current implementation.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; tab_index :
;;;     the index of a tab stop
;;;
;;; alignment :
;;;     tab alignment
;;;
;;; location :
;;;     tab location in Pango units
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_tab ()
;;;
;;; void pango_tab_array_get_tab (PangoTabArray *tab_array,
;;;                               gint tab_index,
;;;                               PangoTabAlign *alignment,
;;;                               gint *location);
;;;
;;; Gets the alignment and position of a tab stop.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; tab_index :
;;;     tab stop index
;;;
;;; alignment :
;;;     location to store alignment, or NULL. [out][allow-none]
;;;
;;; location :
;;;     location to store tab position, or NULL. [out][allow-none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_tabs ()
;;;
;;; void pango_tab_array_get_tabs (PangoTabArray *tab_array,
;;;                                PangoTabAlign **alignments,
;;;                                gint **locations);
;;;
;;; If non-NULL, alignments and locations are filled with allocated arrays of
;;; length pango_tab_array_get_size(). You must free the returned array.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; alignments :
;;;     location to store an array of tab stop alignments, or NULL
;;;
;;; locations :
;;;     location to store an array of tab positions, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_positions_in_pixels ()
;;;
;;; gboolean pango_tab_array_get_positions_in_pixels (PangoTabArray *tab_array)
;;;
;;; Returns TRUE if the tab positions are in pixels, FALSE if they are in Pango
;;; units.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; Returns :
;;;     whether positions are in pixels.
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.tab-array.lisp ---------------------------------------
