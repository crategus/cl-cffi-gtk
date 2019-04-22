;;; ----------------------------------------------------------------------------
;;; gtk.print-settings.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkPrintSettings
;;;
;;;     Stores print settings
;;;
;;; Types and Values
;;;
;;;     GtkPrintSettings
;;;
;;;     GTK_PRINT_SETTINGS_PRINTER
;;;
;;;     GtkPageOrientation
;;;     GTK_PRINT_SETTINGS_ORIENTATION
;;;     GTK_PRINT_SETTINGS_PAPER_FORMAT
;;;     GTK_PRINT_SETTINGS_PAPER_WIDTH
;;;     GTK_PRINT_SETTINGS_PAPER_HEIGHT
;;;     GTK_PRINT_SETTINGS_USE_COLOR
;;;     GTK_PRINT_SETTINGS_COLLATE
;;;     GTK_PRINT_SETTINGS_REVERSE
;;;
;;;     GtkPrintDuplex
;;;     GTK_PRINT_SETTINGS_DUPLEX
;;;
;;;     GtkPrintQuality
;;;     GTK_PRINT_SETTINGS_QUALITY
;;;     GTK_PRINT_SETTINGS_N_COPIES
;;;     GTK_PRINT_SETTINGS_NUMBER_UP
;;;
;;;     GtkNumberUpLayout
;;;     GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT
;;;     GTK_PRINT_SETTINGS_RESOLUTION
;;;     GTK_PRINT_SETTINGS_RESOLUTION_X
;;;     GTK_PRINT_SETTINGS_RESOLUTION_Y
;;;     GTK_PRINT_SETTINGS_PRINTER_LPI
;;;     GTK_PRINT_SETTINGS_SCALE
;;;
;;;     GtkPrintPages
;;;     GTK_PRINT_SETTINGS_PRINT_PAGES
;;;
;;;     GtkPageRange
;;;     GTK_PRINT_SETTINGS_PAGE_RANGES
;;;d
;;;     GtkPageSet
;;;     GTK_PRINT_SETTINGS_PAGE_SET
;;;     GTK_PRINT_SETTINGS_DEFAULT_SOURCE
;;;     GTK_PRINT_SETTINGS_MEDIA_TYPE
;;;     GTK_PRINT_SETTINGS_DITHER
;;;     GTK_PRINT_SETTINGS_FINISHINGS
;;;     GTK_PRINT_SETTINGS_OUTPUT_BIN
;;;     GTK_PRINT_SETTINGS_OUTPUT_DIR
;;;     GTK_PRINT_SETTINGS_OUTPUT_BASENAME
;;;     GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT
;;;     GTK_PRINT_SETTINGS_OUTPUT_URI
;;;     GTK_PRINT_SETTINGS_WIN32_DRIVER_EXTRA
;;;     GTK_PRINT_SETTINGS_WIN32_DRIVER_VERSION
;;;
;;; Functions
;;;
;;;     GtkPrintSettingsFunc
;;;
;;;     gtk_print_settings_new
;;;     gtk_print_settings_copy
;;;     gtk_print_settings_has_key
;;;     gtk_print_settings_get
;;;     gtk_print_settings_set
;;;     gtk_print_settings_unset
;;;     gtk_print_settings_foreach
;;;     gtk_print_settings_get_bool
;;;     gtk_print_settings_set_bool
;;;     gtk_print_settings_get_double
;;;     gtk_print_settings_get_double_with_default
;;;     gtk_print_settings_set_double
;;;     gtk_print_settings_get_length
;;;     gtk_print_settings_set_length
;;;     gtk_print_settings_get_int
;;;     gtk_print_settings_get_int_with_default
;;;     gtk_print_settings_set_int
;;;
;;;     gtk_print_settings_get_printer
;;;     gtk_print_settings_set_printer
;;;     gtk_print_settings_get_orientation
;;;     gtk_print_settings_set_orientation
;;;     gtk_print_settings_get_paper_size
;;;     gtk_print_settings_set_paper_size
;;;     gtk_print_settings_get_paper_width
;;;     gtk_print_settings_set_paper_width
;;;     gtk_print_settings_get_paper_height
;;;     gtk_print_settings_set_paper_height
;;;     gtk_print_settings_get_use_color
;;;     gtk_print_settings_set_use_color
;;;     gtk_print_settings_get_collate
;;;     gtk_print_settings_set_collate
;;;     gtk_print_settings_get_reverse
;;;     gtk_print_settings_set_reverse
;;;     gtk_print_settings_get_duplex
;;;     gtk_print_settings_set_duplex
;;;     gtk_print_settings_get_quality
;;;     gtk_print_settings_set_quality
;;;     gtk_print_settings_get_n_copies
;;;     gtk_print_settings_set_n_copies
;;;     gtk_print_settings_get_number_up
;;;     gtk_print_settings_set_number_up
;;;     gtk_print_settings_get_number_up_layout
;;;     gtk_print_settings_set_number_up_layout
;;;     gtk_print_settings_get_resolution
;;;     gtk_print_settings_set_resolution
;;;     gtk_print_settings_set_resolution_xy
;;;     gtk_print_settings_get_resolution_x
;;;     gtk_print_settings_get_resolution_y
;;;     gtk_print_settings_get_printer_lpi
;;;     gtk_print_settings_set_printer_lpi
;;;     gtk_print_settings_get_scale
;;;     gtk_print_settings_set_scale
;;;     gtk_print_settings_get_print_pages
;;;     gtk_print_settings_set_print_pages
;;;     gtk_print_settings_get_page_ranges
;;;     gtk_print_settings_set_page_ranges
;;;     gtk_print_settings_get_page_set
;;;     gtk_print_settings_set_page_set
;;;     gtk_print_settings_get_default_source
;;;     gtk_print_settings_set_default_source
;;;     gtk_print_settings_get_media_type
;;;     gtk_print_settings_set_media_type
;;;     gtk_print_settings_get_dither
;;;     gtk_print_settings_set_dither
;;;     gtk_print_settings_get_finishings
;;;     gtk_print_settings_set_finishings
;;;     gtk_print_settings_get_output_bin
;;;     gtk_print_settings_set_output_bin
;;;
;;;     gtk_print_settings_new_from_file
;;;     gtk_print_settings_new_from_key_file
;;;     gtk_print_settings_new_from_gvariant
;;;     gtk_print_settings_load_file
;;;     gtk_print_settings_load_key_file
;;;     gtk_print_settings_to_file
;;;     gtk_print_settings_to_key_file
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkPrintSettings
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintSettings
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPrintSettings" gtk-print-settings
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_print_settings_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-print-settings 'type)
 "@version{2013-5-30}
  @begin{short}
    A @sym{gtk-print-settings} object represents the settings of a print dialog
    in a system-independent way. The main use for this object is that once you
    have printed you can get a settings object that represents the settings the
    user chose, and the next time you print you can pass that object in so that
    the user does not have to re-set all his settings.
  @end{short}

  Its also possible to enumerate the settings so that you can easily save the
  settings for the next time your app runs, or even store them in a document.
  The predefined keys try to use shared values as much as possible so that
  moving such a document between systems still works.

  Printing support was added in GTK+ 2.10.")

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-settings-new))

(defun gtk-print-settings-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @return{A new @class{gtk-print-settings} object.}
  @short{Creates a new @class{gtk-print-settings} object.}

  Since 2.10
  @see-class{gtk-print-settings}"
  (make-instance 'gtk-print-settings))

(export 'gtk-print-settings-new)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_copy" gtk-print-settings-copy)
    (g-object gtk-print-settings)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[other]{a @class{gtk-print-settings} object}
  @return{A newly allocated copy of @arg{other}.}
  @short{Copies a @class{gtk-print-settings} object.}

  Since 2.10
  @see-class{gtk-print-settins}"
  (other (g-object gtk-print-settings)))

(export 'gtk-print-settings-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_has_key ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_has_key" gtk-print-settings-has-key) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @return{@em{True}, if @arg{key} has a value.}
  @short{Returns @em{true}, if a value is associated with @arg{key}.}

  Since 2.10
  @see-class{gtk-print-settings}"
  (settings (g-object gtk-print-settings))
  (key :string))

(export 'gtk-print-settings-has-key)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get" gtk-print-settings-get) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @return{The string value for @arg{key}.}
  @short{Looks up the string value associated with @arg{key}.}

  Since 2.10
  @see-class{gtk-print-settings}"
  (settings (g-object gtk-print-settings))
  (key :string))

(export 'gtk-print-settings-get)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set" gtk-print-settings-set) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @argument[value]{a string value, or @code{nil}}
  @short{Associates value with @arg{key}.}

  Since 2.10
  @see-class{gtk-print-settings}"
  (settings (g-object gtk-print-settings))
  (key :string)
  (value :string))

(export 'gtk-print-settings-set)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_unset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_unset" gtk-print-settings-unset) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @begin{short}
    Removes any value associated with @arg{key}.
  @end{short}
  This has the same effect as setting the value to @code{nil}.

  Since 2.10
  @see-class{gtk-print-settings}"
  (settings (g-object gtk-print-settings))
  (key :string))

(export 'gtk-print-settings-unset)

;;; ----------------------------------------------------------------------------
;;; GtkPrintSettingsFunc ()
;;;
;;; void (*GtkPrintSettingsFunc) (const gchar *key,
;;;                               const gchar *value,
;;;                               gpointer user_data);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_foreach ()
;;;
;;; void gtk_print_settings_foreach (GtkPrintSettings *settings,
;;;                                  GtkPrintSettingsFunc func,
;;;                                  gpointer user_data);
;;;
;;; Calls func for each key-value pair of settings.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; func :
;;;     the function to call
;;;
;;; user_data :
;;;     user data for func
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_bool ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_bool" gtk-print-settings-get-bool) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @return{@em{True}, if @arg{key} maps to a true value.}
  @begin{short}
    Returns the boolean represented by the value that is associated with
    @arg{key}.
  @end{short}

  The string \"true\" represents @em{true}, any other string @code{nil}.

  Since 2.10
  @see-class{gtk-print-settings}"
  (settings (g-object gtk-print-settings))
  (key :string))

(export 'gtk-print-settings-get-bool)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_bool ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_bool" gtk-print-settings-set-bool) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @argument[value]{a boolean}
  @short{Sets @arg{key} to a boolean value.}

  Since 2.10
  @see-class{gtk-print-settings}"
  (settings (g-object gtk-print-settings))
  (key :string)
  (value :boolean))

(export 'gtk-print-settings-set-bool)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_double ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_double" gtk-print-settings-get-double) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @return{The double value of @arg{key}.}
  @short{Returns the double value associated with @arg{key}, or 0.}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-set-double}
  @see-function{gtk-print-settings-get-double-with-default}"
  (settings (g-object gtk-print-settings))
  (key :string))

(export 'gtk-print-settings-get-double)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_double_with_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_double_with_default"
           gtk-print-settings-get-double-with-default) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @argument[default]{the default value}
  @return{The floating point number associated with @arg{key}.}
  @begin{short}
    Returns the floating point number represented by the value that is
    associated with @arg{key}, or @arg{default} if the value does not represent
    a floating point number.
  @end{short}

  Floating point numbers are parsed with @code{g_ascii_strtod()}.

  Since 2.10
  @see-class{gtk-print-settings}"
  (settings (g-object gtk-print-settings))
  (key :string)
  (default :double))

(export 'gtk-print-settings-get-double-with-default)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_double ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_double" gtk-print-settings-set-double) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @argument[value]{a double value}
  @short{Sets key to a double value.}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-double}
  @see-function{gtk-print-settings-get-double-with-default}"
  (settings (g-object gtk-print-settings))
  (key :string)
  (value :double))

(export 'gtk-print-settings-set-double)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_length ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_length" gtk-print-settings-get-length) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @argument[unit]{the unit of the return value}
  @return{The length value of @arg{key}, converted to @arg{unit}.}
  @begin{return}
    Returns the value associated with @arg{key}, interpreted as a length.
  @end{return}
  The returned value is converted to @arg{unit}.

  Since 2.10
  @see-class{gtk-print-settings}
  @see-symbol{gtk-unit}
  @see-class{gtk-print-settings-set-length}"
  (settings (g-object gtk-print-settings))
  (key :string)
  (unit gtk-unit))

(export 'gtk-print-settings-get-length)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_length ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_length" gtk-print-settings-set-length) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @argument[value]{a length}
  @argument[unit]{the unit of length}
  @short{Associates a length in units of @arg{unit} with @arg{key}.}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-symbol{gtk-unit}
  @see-function{gtk-print-settings-get-length}"
  (settings (g-object gtk-print-settings))
  (key :string)
  (value :double)
  (unit gtk-unit))

(export 'gtk-print-settings-set-length)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_int ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_int" gtk-print-settings-get-int) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @return{The integer value of @arg{key}.}
  @short{Returns the integer value of @arg{key}, or 0.}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-class{gtk-print-settings-set-int}
  @see-class{gtk-print-settings-get-int-with-default}"
  (settings (g-object gtk-print-settings))
  (key :string))

(export 'gtk-print-settings-get-int)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_int_with_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_int_with_default"
           gtk-print-settings-get-int-with-default) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @argument[default]{the default value}
  @return{the integer value of @arg{key}}
  @begin{short}
    Returns the value of @arg{key}, interpreted as an integer, or the default
    value.
  @end{short}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-int}
  @see-function{gtk-print-settings-set-int}"
  (settings (g-object gtk-print-settings))
  (key :string)
  (default :int))

(export 'gtk-print-settings-get-int-with-default)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_int ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_int" gtk-print-settings-set-int) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[key]{a key}
  @argument[value]{an integer}
  @short{Sets key to an integer value.}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-int}
  @see-function{gtk-print-settings-get-int-with-default}"
  (settings (g-object gtk-print-settings))
  (key :string)
  (value :int))

(export 'gtk-print-settings-set-int)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PRINTER
;;;
;;; #define GTK_PRINT_SETTINGS_PRINTER          "printer"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_printer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_printer" gtk-print-settings-get-printer)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @return{The printer name.}
  @begin{short}
    Convenience function to obtain the value of \"printer\".
  @end{short}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-set-printer}"
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-printer)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_printer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_printer" gtk-print-settings-set-printer) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[printer]{the printer name}
  @begin{short}
    Convenience function to set \"printer\" to @arg{printer}.
  @end{short}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-printer}"
  (settings (g-object gtk-print-settings))
  (printer :string))

(export 'gtk-print-settings-set-printer)

;;; ----------------------------------------------------------------------------
;;; enum GtkPageOrientation
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPageOrienation" gtk-page-orientation
  (:export t
   :type-initializer "gtk_page_orientation_get_type")
  :portrait
  :landscape
  :reverse-portrait
  :reverse-landscape)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-page-orientation atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-page-orientation atdoc:*external-symbols*)
 "@version{2013-11-17}
  @short{}
  @begin{pre}
(define-g-enum \"GtkPageOrienation\" gtk-page-orientation
  (:export t
   :type-initializer \"gtk_page_orientation_get_type\")
  :portrait
  :landscape
  :reverse-portrait
  :reverse-landscape)
  @end{pre}
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-orientation}
  @see-function{gtk-print-settings-set-orientation}")

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_ORIENTATION
;;;
;;; #define GTK_PRINT_SETTINGS_ORIENTATION      "orientation"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_orientation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_orientation"
           gtk-print-settings-get-orientation) gtk-page-orientation
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @return{The orientation.}
  @begin{short}
    Get the value of \"orientation\", converted to a
    @symbol{gtk-page-orientation}.
  @end{short}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-symbol{gtk-page-orientation}
  @see-function{gtk-print-settings-set-orientation}"
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_orientation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_orientation"
           gtk-print-settings-set-orientation) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[orientation]{a page orientation}
  @short{Sets the value of \"orientation\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-symbol{gtk-page-orientation}
  @see-function{gtk-print-settings-get-orientation}"
  (settings (g-object gtk-print-settings))
  (orientation gtk-page-orientation))

(export 'gtk-print-settings-set-orientation)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PAPER_FORMAT
;;;
;;; #define GTK_PRINT_SETTINGS_PAPER_FORMAT     "paper-format"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_paper_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_paper_size" gtk-print-settings-get-paper-size)
    (g-boxed-foreign gtk-paper-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @return{The paper size.}
  @begin{short}
    Gets the value of \"paper-format\", converted to a @class{gtk-paper-size}.
  @end{short}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-class{gtk-paper-size}
  @see-function{gtk-print-settings-set-paper-size}"
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-paper-size)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_paper_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_paper_size" gtk-print-settings-set-paper-size)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[paper-size]{a paper size}
  @begin{short}
    Sets the value of \"paper-format\", \"paper-width\", and \"paper-height\".
  @end{short}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-class{gtk-paper-size}
  @see-function{gtk-print-settings-get-paper-size}"
  (settings (g-object gtk-print-settings))
  (paper-size (g-boxed-foreign gtk-paper-size)))

(export 'gtk-print-settings-set-paper-size)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PAPER_WIDTH
;;;
;;; #define GTK_PRINT_SETTINGS_PAPER_WIDTH      "paper-width"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_paper_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_paper_width"
           gtk-print-settings-get-paper-width) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[unit]{the unit for the return value}
  @return{The paper width, in units of @arg{unit}.}
  @begin{short}
    Gets the value of \"paper-width\" converted to @arg{unit}.
  @end{short}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-symbol{gtk-unit}
  @see-function{gtk-print-settings-set-paper-width}"
  (settings (g-object gtk-print-settings))
  (unit gtk-unit))

(export 'gtk-print-settings-get-paper-width)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_paper_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_paper_width"
           gtk-print-settings-set-paper-width) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[width]{the paper width}
  @argument[unit]{the units of width}
  @short{Sets the value of \"paper-width\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-symbol{gtk-unit}
  @see-function{gtk-print-settings-get-paper-width}"
  (settings (g-object gtk-print-settings))
  (width :double)
  (unit gtk-unit))

(export 'gtk-print-settings-set-paper-width)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PAPER_HEIGHT
;;;
;;; #define GTK_PRINT_SETTINGS_PAPER_HEIGHT     "paper-height"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_paper_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_paper_height"
           gtk-print-settings-get-paper-height) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[unit]{the unit for the return value}
  @return{The paper height, in units of @arg{unit}.}
  @short{Gets the value of \"paper-height\", converted to @arg{unit}.}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-symbol{gtk-unit}
  @see-function{gtk-print-settings-set-paper-height}"
  (settings (g-object gtk-print-settings))
  (unit gtk-unit))

(export 'gtk-print-settings-get-paper-height)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_paper_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_paper_height"
           gtk-print-settings-set-paper-height) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[height]{the paper height}
  @argument[unit]{the units of height}
  @short{Sets the value of \"paper-height\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-symbol{gtk-unit}
  @see-function{gtk-print-settings-get-paper-height}"
  (settings (g-object gtk-print-settings))
  (height :double)
  (unit gtk-unit))

(export 'gtk-print-settings-set-paper-height)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_USE_COLOR
;;;
;;; #define GTK_PRINT_SETTINGS_USE_COLOR        "use-color"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_use_color ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_use_color" gtk-print-settings-get-use-color)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gtk-print-settings} object}
  @return{Whether to use color.}
  @short{Gets the value of \"use-color\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-set-use-color}"
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-use-color)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_use_color ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_use_color" gtk-print-settings-set-use-color)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[settings]{a @class{gt-print-settings} object}
  @argument[use-color]{whether to use color}
  @short{Sets the value of \"use-color\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-use-color}"
  (settings (g-object gtk-print-settings))
  (use-color :boolean))

(export 'gtk-print-settings-set-use-color)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_COLLATE
;;;
;;; #define GTK_PRINT_SETTINGS_COLLATE          "collate"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_collate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_collate" gtk-print-settings-get-collate)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[settings]{a @class{gtk-print-settings} object}
  @return{Whether to collate the printed pages.}
  @short{Gets the value of \"collate\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-set-collate}"
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-collate)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_collate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_collate" gtk-print-settings-set-collate) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[collate]{whether to collate the output}
  @short{Sets the value of \"collate\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-collate}"
  (settings (g-object gtk-print-settings))
  (collate :boolean))

(export 'gtk-print-settings-set-collate)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_REVERSE
;;;
;;; #define GTK_PRINT_SETTINGS_REVERSE          "reverse"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_reverse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_reverse" gtk-print-settings-get-reverse)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[settings]{a @class{gtk-print-settings} object}
  @return{Whether to reverse the order of the printed pages.}
  @begin{short}
    Gets the value of \"reverse\".
  @end{short}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-set-reverse}"
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-reverse)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_reverse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_reverse" gtk-print-settings-set-reverse)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[reverse]{whether to reverse the output}
  @short{Sets the value of \"reserve\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-reverse}"
  (settings (g-object gtk-print-settings))
  (reverse :boolean))

(export 'gtk-print-settings-set-reverse)

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintDuplex
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintDuplex" gtk-print-duplex
  (:export t
   :type-initializer "gtk_print_duplex_get_type")
  :simplex
  :horizontal
  :vertical)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-duplex atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-print-duplex atdoc:*external-symbols*)
 "@version{2013-12-3}
  @short{}
  @begin{pre}
(define-g-enum \"GtkPrintDuplex\" gtk-print-duplex
  (:export t
   :type-initializer \"gtk_print_duplex_get_type\")
  :simplex
  :horizontal
  :vertical)
  @end{pre}
  @see-class{gtk-print-settings}")

;;; ----------------------------------------------------------------------------
;;; #define GTK_PRINT_SETTINGS_DUPLEX           "duplex"
;;;
;;; gtk_print_settings_get_duplex ()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkPrintDuplex gtk_print_settings_get_duplex ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_duplex" gtk-print-settings-get-duplex)
    gtk-print-duplex
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[settings]{a @class{gtk-print-settings} object}
  @return{Whether to print the output in duplex.}
  @short{Gets the value of \"duplex\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-set-duplex}"
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-duplex)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_duplex ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_duplex" gtk-print-settings-set-duplex) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[duplex]{a @class{gtk-print-duplex} value}
  @short{Sets the value of \"duplex\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-duplex}"
  (settings (g-object gtk-print-settings))
  (duplex gtk-print-duplex))

(export 'gtk-print-settings-set-duplex)

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintQuality
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintQuality" gtk-print-quality
  (:export t
   :type-initializer "gtk_print_quality_get_type")
  :low
  :normal
  :high
  :draft)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-quality atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-print-quality atdoc:*external-symbols*)
 "@version{2013-12-3}
  @short{}
  @begin{pre}
(define-g-enum \"GtkPrintQuality\" gtk-print-quality
  (:export t
   :type-initializer \"gtk_print_quality_get_type\")
  :low
  :normal
  :high
  :draft)
  @end{pre}
  @see-class{gtk-print-settings}")

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_QUALITY
;;;
;;; #define GTK_PRINT_SETTINGS_QUALITY          "quality"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_quality ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_quality" gtk-print-settings-get-quality)
    gtk-print-quality
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[settings]{a @class{gtk-print-settings} object}
  @return{The print quality.}
  @short{Gets the value of \"quality\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-set-quality}"
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-quality)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_quality ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_quality" gtk-print-settings-set-quality) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[quality]{a @class{gtk-print-quality} value}
  @short{Sets the value of \"quality\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-quality}"
  (settings (g-object gtk-print-settings))
  (quality gtk-print-quality))

(export 'gtk-print-settings-set-quality)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_N_COPIES
;;;
;;; #define GTK_PRINT_SETTINGS_N_COPIES         "n-copies"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_n_copies ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_n_copies" gtk-print-copies-get-n-copies) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[settings]{a @class{gtk-print-settings} object}
  @return{The number of copies to print.}
  @short{Gets the value of \"n-copies\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-n-copies}"
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-n-copies)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_n_copies ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_n_copies" gtk-print-settings-set-n-copies)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[n-copies]{the number of copies}
  @return{Sets the value of \"n-copies\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-n-copies}"
  (settings (g-object gtk-print-settings))
  (n-copies :int))

(export 'gtk-print-settings-set-n-copies)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_NUMBER_UP
;;;
;;; #define GTK_PRINT_SETTINGS_NUMBER_UP        "number-up"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_number_up ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_number_up" gtk-print-settings-get-number-up)
    :int
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[settings]{a @class{gtk-print-settings} object}
  @return{The number of pages per sheet.}
  @short{Gets the value of \"number-up\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-set-number-up}"
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-number-up)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_number_up ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_number_up" gtk-print-settings-set-number-up)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[settings]{a @class{gtk-print-settings} object}
  @argument[number-up]{the number of pages per sheet}
  @short{Sets the value of \"number-up\".}

  Since 2.10
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-number-up}"
  (settings (g-object gtk-print-settings))
  (number-up :int))

(export 'gtk-print-settings-set-number-up)

;;; ----------------------------------------------------------------------------
;;; enum GtkNumberUpLayout
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkNubmerUpLayout" gtk-number-up-layout
  (:export t
   :type-initializer "gtk_number_up_layout_get_type")
  (:left-to-right-top-to-bottom 0)
  (:left-to-right-bottom-to-top 1)
  (:right-to-left-bottom-to-top 2)
  (:right-to-left-top-to-bottom 3)
  (:top-to-bottom-left-to-right 4)
  (:top-to-bottom-right-to-left 5)
  (:bottom-to-top-left-to-right 6)
  (:bottom-to-top-right-to-left 7))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-number-up-layout atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-number-up-layout atdoc:*external-symbols*)
 "@version{2013-10-23}
  @begin{short}
    Used to determine the layout of pages on a sheet when printing multiple
    pages per sheet.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkNubmerUpLayout\" gtk-number-up-layout
  (:export t
   :type-initializer \"gtk_number_up_layout_get_type\")
  (:left-to-right-top-to-bottom 0)
  (:left-to-right-bottom-to-top 1)
  (:right-to-left-bottom-to-top 2)
  (:right-to-left-top-to-bottom 3)
  (:top-to-bottom-left-to-right 4)
  (:top-to-bottom-right-to-left 5)
  (:bottom-to-top-left-to-right 6)
  (:bottom-to-top-right-to-left 7))
  @end{pre}
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-number-up-layout}
  @see-function{gtk-print-settings-set-number-up-layout}")

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT
;;;
;;; #define GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT "number-up-layout"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_number_up_layout ()
;;;
;;; GtkNumberUpLayout gtk_print_settings_get_number_up_layout
;;;                                                (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     layout of page in number-up mode
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_number_up_layout"
           gtk-print-settings-get-number-up-layout) gtk-number-up-layout
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-number-up-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_number_up_layout ()
;;;
;;; void gtk_print_settings_set_number_up_layout
;;;                                        (GtkPrintSettings *settings,
;;;                                         GtkNumberUpLayout number_up_layout);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; number_up_layout :
;;;     a GtkNumberUpLayout value
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_number_up_layout"
           gtk-print-settings-set-number-up-layout) :void
  (settings (g-object gtk-print-settings))
  (number-up-layout gtk-number-up-layout))

(export 'gtk-print-settings-set-number-up-layout)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_RESOLUTION
;;;
;;; #define GTK_PRINT_SETTINGS_RESOLUTION       "resolution"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_resolution ()
;;;
;;; gint gtk_print_settings_get_resolution (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_RESOLUTION.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the resolution in dpi
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_resolution" gtk-print-settings-get-resolution)
    :int
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-resolution)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_resolution ()
;;;
;;; void gtk_print_settings_set_resolution (GtkPrintSettings *settings,
;;;                                         gint resolution);
;;;
;;; Sets the values of GTK_PRINT_SETTINGS_RESOLUTION,
;;; GTK_PRINT_SETTINGS_RESOLUTION_X and GTK_PRINT_SETTINGS_RESOLUTION_Y.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; resolution :
;;;     the resolution in dpi
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_resolution" gtk-print-settings-set-resolution)
    :void
  (settings (g-object gtk-print-settings))
  (resolution :int))

(export 'gtk-print-settings-set-resolution)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_resolution_xy ()
;;;
;;; void gtk_print_settings_set_resolution_xy (GtkPrintSettings *settings,
;;;                                            gint resolution_x,
;;;                                            gint resolution_y);
;;;
;;; Sets the values of GTK_PRINT_SETTINGS_RESOLUTION,
;;; GTK_PRINT_SETTINGS_RESOLUTION_X and GTK_PRINT_SETTINGS_RESOLUTION_Y.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; resolution_x :
;;;     the horizontal resolution in dpi
;;;
;;; resolution_y :
;;;     the vertical resolution in dpi
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_resolution_xy"
           gtk-print-settings-set-resolution-xy) :void
  (settings (g-object gtk-print-settings))
  (resolution-x :int)
  (resolution-y :int))

(export 'gtk-print-settings-set-resolution-xy)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_RESOLUTION_X
;;;
;;; #define GTK_PRINT_SETTINGS_RESOLUTION_X     "resolution-x"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_resolution_x ()
;;;
;;; gint gtk_print_settings_get_resolution_x (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_RESOLUTION_X.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the horizontal resolution in dpi
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_resolution_x"
           gtk-print-settings-get-resolution-x) :int
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-resolution-x)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_RESOLUTION_Y
;;;
;;; #define GTK_PRINT_SETTINGS_RESOLUTION_Y     "resolution-y"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_resolution_y ()
;;;
;;; gint gtk_print_settings_get_resolution_y (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_RESOLUTION_Y.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the vertical resolution in dpi
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_resolution_y"
           gtk-print-settings-get-resolution-y) :int
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-resolution-y)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PRINTER_LPI
;;;
;;; #define GTK_PRINT_SETTINGS_PRINTER_LPI      "printer-lpi"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_printer_lpi ()
;;;
;;; gdouble gtk_print_settings_get_printer_lpi (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_PRINTER_LPI.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the resolution in lpi (lines per inch)
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_printer_lpi"
           gtk-print-settings-get-printer-lpi) :double
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-printer-lpi)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_printer_lpi ()
;;;
;;; void gtk_print_settings_set_printer_lpi (GtkPrintSettings *settings,
;;;                                          gdouble lpi);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_PRINTER_LPI.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; lpi :
;;;     the resolution in lpi (lines per inch)
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_printer_lpi"
           gtk-print-settings-set-printer-lpi) :void
  (settings (g-object gtk-print-settings))
  (lpi :double))

(export 'gtk-print-settings-set-printer-lpi)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_SCALE
;;;
;;; #define GTK_PRINT_SETTINGS_SCALE            "scale"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_scale ()
;;;
;;; gdouble gtk_print_settings_get_scale (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_SCALE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the scale in percent
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_scale" gtk-print-settings-get-scale) :double
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_scale ()
;;;
;;; void gtk_print_settings_set_scale (GtkPrintSettings *settings,
;;;                                    gdouble scale);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_SCALE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; scale :
;;;     the scale in percent
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_scale" gtk-print-settings-set-scale) :void
  (settings (g-object gtk-print-settings))
  (scale :double))

(export 'gtk-print-settings-set-scale)

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintPages
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintPages" gtk-print-pages
  (:export t
   :type-initializer "gtk_print_pages_get_type")
  (:all 0)
  (:current 1)
  (:ranges 2)
  (:selection 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-pages atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-print-pages atdoc:*external-symbols*)
 "@version{2013-10-21}
  @begin{pre}
(define-g-enum \"GtkPrintPages\" gtk-print-pages
  (:export t
   :type-initializer \"gtk_print_pages_get_type\")
  (:all 0)
  (:current 1)
  (:ranges 2)
  (:selection 3))
  @end{pre}
  @see-class{gtk-print-settings}
  @see-function{gtk-print-settings-get-print-pages}
  @see-function{gtk-print-settings-set-print-pages}")

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PRINT_PAGES
;;;
;;; #define GTK_PRINT_SETTINGS_PRINT_PAGES      "print-pages"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_print_pages ()
;;;
;;; GtkPrintPages gtk_print_settings_get_print_pages
;;;                                                (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_PRINT_PAGES.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     which pages to print
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_print_pages"
           gtk-print-settings-get-print-pages) gtk-print-pages
  (settings (g-object gtk-print-pages)))

(export 'gtk-print-settings-get-print-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_print_pages ()
;;;
;;; void gtk_print_settings_set_print_pages (GtkPrintSettings *settings,
;;;                                          GtkPrintPages pages);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_PRINT_PAGES.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; pages :
;;;     a GtkPrintPages value
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_print_pages"
           gtk-print-settings-set-print-pages) :void
  (settings (g-object gtk-print-settings))
  (pages gtk-print-pages))

(export 'gtk-print-settings-set-print-pages)

;;; ----------------------------------------------------------------------------
;;; struct GtkPageRange
;;;
;;; struct GtkPageRange {
;;;   gint start;
;;;   gint end;
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PAGE_RANGES
;;;
;;; #define GTK_PRINT_SETTINGS_PAGE_RANGES "page-ranges"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_page_ranges ()
;;;
;;; GtkPageRange * gtk_print_settings_get_page_ranges
;;;                                                 (GtkPrintSettings *settings,
;;;                                                  gint *num_ranges);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_PAGE_RANGES.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; num_ranges :
;;;     return location for the length of the returned array
;;;
;;; Returns :
;;;     an array of GtkPageRanges. Use g_free() to free the array when it is no
;;;     longer needed
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_page_ranges ()
;;;
;;; void gtk_print_settings_set_page_ranges (GtkPrintSettings *settings,
;;;                                          GtkPageRange *page_ranges,
;;;                                          gint num_ranges);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_PAGE_RANGES.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; page_ranges :
;;;     an array of GtkPageRanges
;;;
;;; num_ranges :
;;;     the length of page_ranges
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkPageSet
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPageSet" gtk-page-set
  (:export t
   :type-initializer "gtk_page_set_get_type")
  (:all 0)
  (:even 1)
  (:odd 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-page-set atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-page-set atdoc:*external-symbols*)
 "@version{2013-10-22}
  @begin{pre}
(define-g-enum \"GtkPageSet\" gtk-page-set
  (:export t
   :type-initializer \"gtk_page_set_get_type\")
  (:all 0)
  (:even 1)
  (:odd 2))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PAGE_SET
;;;
;;; #define GTK_PRINT_SETTINGS_PAGE_SET "page-set"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_page_set ()
;;;
;;; GtkPageSet gtk_print_settings_get_page_set (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_PAGE_SET.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the set of pages to print
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_page_set" gtk-print-settings-get-page-set)
    gtk-page-set
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-page-set)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_page_set ()
;;;
;;; void gtk_print_settings_set_page_set (GtkPrintSettings *settings,
;;;                                       GtkPageSet page_set);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_PAGE_SET.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; page_set :
;;;     a GtkPageSet value
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_page_set" gtk-print-settings-set-page-set)
    :void
  (settings (g-object gtk-print-settings))
  (page-set gtk-page-set))

(export 'gtk-print-settings-set-page-set)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_DEFAULT_SOURCE
;;;
;;; #define GTK_PRINT_SETTINGS_DEFAULT_SOURCE   "default-source"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_default_source ()
;;;
;;; const gchar * gtk_print_settings_get_default_source
;;;                                                (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_DEFAULT_SOURCE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the default source
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_default_source"
           gtk-print-settings-get-default-source) :string
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-default-source)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_default_source ()
;;;
;;; void gtk_print_settings_set_default_source (GtkPrintSettings *settings,
;;;                                             const gchar *default_source);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_DEFAULT_SOURCE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; default_source :
;;;     the default source
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_default_source"
           gtk-print-settings-set-default-source) :void
  (settings (g-object gtk-print-settings))
  (source :string))

(export 'gtk-print-settings-set-default-source)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_MEDIA_TYPE
;;;
;;; #define GTK_PRINT_SETTINGS_MEDIA_TYPE       "media-type"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_media_type ()
;;;
;;; const gchar * gtk_print_settings_get_media_type (GtkPrintSettings *settings)
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_MEDIA_TYPE.
;;;
;;; The set of media types is defined in PWG 5101.1-2002 PWG.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the media type
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_media_type"
           gtk-print-settings-get-media-type) :string
  (settings (g-object gtk-print-settings)))

(export 'gtk-print-settings-get-media-type)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_media_type ()
;;;
;;; void gtk_print_settings_set_media_type (GtkPrintSettings *settings,
;;;                                         const gchar *media_type);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_MEDIA_TYPE.
;;;
;;; The set of media types is defined in PWG 5101.1-2002 PWG.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; media_type :
;;;     the media type
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set_media_type"
           gtk-print-settings-set-media-type) :void
  (settings (g-object gtk-print-settings))
  (media-type :string))

(export 'gtk-print-settings-set-media-type)

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_DITHER
;;;
;;; #define GTK_PRINT_SETTINGS_DITHER           "dither"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_dither ()
;;;
;;; const gchar * gtk_print_settings_get_dither (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_DITHER.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the dithering that is used
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_dither ()
;;;
;;; void gtk_print_settings_set_dither (GtkPrintSettings *settings,
;;;                                     const gchar *dither);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_DITHER.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; dither :
;;;     the dithering that is used
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_FINISHINGS
;;;
;;; #define GTK_PRINT_SETTINGS_FINISHINGS       "finishings"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_finishings ()
;;;
;;; const gchar * gtk_print_settings_get_finishings (GtkPrintSettings *settings)
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_FINISHINGS.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the finishings
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_finishings ()
;;;
;;; void gtk_print_settings_set_finishings (GtkPrintSettings *settings,
;;;                                         const gchar *finishings);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_FINISHINGS.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; finishings :
;;;     the finishings
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_OUTPUT_BIN
;;;
;;; #define GTK_PRINT_SETTINGS_OUTPUT_BIN       "output-bin"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_output_bin ()
;;;
;;; const gchar * gtk_print_settings_get_output_bin (GtkPrintSettings *settings)
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_OUTPUT_BIN.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the output bin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_output_bin ()
;;;
;;; void gtk_print_settings_set_output_bin (GtkPrintSettings *settings,
;;;                                         const gchar *output_bin);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_OUTPUT_BIN.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; output_bin :
;;;     the output bin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_OUTPUT_DIR
;;;
;;; #define GTK_PRINT_SETTINGS_OUTPUT_DIR       "output-dir"
;;;
;;; The key used by the “Print to file” printer to store the directory to which
;;; the output should be written.
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_OUTPUT_BASENAME
;;;
;;; #define GTK_PRINT_SETTINGS_OUTPUT_BASENAME  "output-basename"
;;;
;;; The key used by the “Print to file” printer to store the file name of the
;;; output without the path to the directory and the file extension.
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT
;;;
;;; #define GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT  "output-file-format"
;;;
;;; The key used by the "Print to file" printer to store the format of the
;;; output. The supported values are "PS" and "PDF".
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_OUTPUT_URI
;;;
;;; #define GTK_PRINT_SETTINGS_OUTPUT_URI          "output-uri"
;;;
;;; The key used by the "Print to file" printer to store the URI to which the
;;; output should be written. GTK+ itself supports only "file://" URIs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_WIN32_DRIVER_EXTRA
;;;
;;; #define GTK_PRINT_SETTINGS_WIN32_DRIVER_EXTRA   "win32-driver-extra"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_WIN32_DRIVER_VERSION
;;;
;;; #define GTK_PRINT_SETTINGS_WIN32_DRIVER_VERSION "win32-driver-version"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_new_from_file ()
;;;
;;; GtkPrintSettings * gtk_print_settings_new_from_file (const gchar *file_name,
;;;                                                      GError **error);
;;;
;;; Reads the print settings from file_name. Returns a new GtkPrintSettings
;;; object with the restored settings, or NULL if an error occurred. If the file
;;; could not be loaded then error is set to either a GFileError or
;;; GKeyFileError. See gtk_print_settings_to_file().
;;;
;;; file_name :
;;;     the filename to read the settings from
;;;
;;; error :
;;;     return location for errors, or NULL
;;;
;;; Returns :
;;;     the restored GtkPrintSettings
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_new_from_key_file ()
;;;
;;; GtkPrintSettings * gtk_print_settings_new_from_key_file
;;;                                                    (GKeyFile *key_file,
;;;                                                     const gchar *group_name,
;;;                                                     GError **error);
;;;
;;; Reads the print settings from the group group_name in key_file. Returns a
;;; new GtkPrintSettings object with the restored settings, or NULL if an error
;;; occurred. If the file could not be loaded then error is set to either a
;;; GFileError or GKeyFileError.
;;;
;;; key_file :
;;;     the GKeyFile to retrieve the settings from
;;;
;;; group_name :
;;;     the name of the group to use, or NULL to use the default
;;;     "Print Settings"
;;;
;;; error :
;;;     return location for errors, or NULL
;;;
;;; Returns :
;;;     the restored GtkPrintSettings
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_new_from_gvariant ()
;;;
;;; GtkPrintSettings *
;;; gtk_print_settings_new_from_gvariant (GVariant *variant);
;;;
;;; Deserialize print settings from an a{sv} variant in the format produced by
;;; gtk_print_settings_to_gvariant().
;;;
;;; variant :
;;;     an a{sv} GVariant
;;;
;;; Returns :
;;;     a new GtkPrintSettings object.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_load_file ()
;;;
;;; gboolean gtk_print_settings_load_file (GtkPrintSettings *settings,
;;;                                        const gchar *file_name,
;;;                                        GError **error);
;;;
;;; Reads the print settings from file_name. If the file could not be loaded
;;; then error is set to either a GFileError or GKeyFileError. See
;;; gtk_print_settings_to_file().
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; file_name :
;;;     the filename to read the settings from
;;;
;;; error :
;;;     return location for errors, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_load_key_file ()
;;;
;;; gboolean gtk_print_settings_load_key_file (GtkPrintSettings *settings,
;;;                                            GKeyFile *key_file,
;;;                                            const gchar *group_name,
;;;                                            GError **error);
;;;
;;; Reads the print settings from the group group_name in key_file. If the file
;;; could not be loaded then error is set to either a GFileError or
;;; GKeyFileError.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key_file :
;;;     the GKeyFile to retrieve the settings from
;;;
;;; group_name :
;;;     the name of the group to use, or NULL to use the default
;;;     "Print Settings"
;;;
;;; error :
;;;     return location for errors, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_to_file ()
;;;
;;; gboolean gtk_print_settings_to_file (GtkPrintSettings *settings,
;;;                                      const gchar *file_name,
;;;                                      GError **error);
;;;
;;; This function saves the print settings from settings to file_name. If the
;;; file could not be loaded then error is set to either a GFileError or
;;; GKeyFileError.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; file_name :
;;;     the file to save to
;;;
;;; error :
;;;     return location for errors, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_to_key_file ()
;;;
;;; void gtk_print_settings_to_key_file (GtkPrintSettings *settings,
;;;                                      GKeyFile *key_file,
;;;                                      const gchar *group_name);
;;;
;;; This function adds the print settings from settings to key_file.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key_file :
;;;     the GKeyFile to save the print settings to
;;;
;;; group_name :
;;;     the group to add the settings to in key_file, or NULL to use the default
;;;     "Print Settings"
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_to_gvariant ()
;;;
;;; GVariant *
;;; gtk_print_settings_to_gvariant (GtkPrintSettings *settings);
;;;
;;; Serialize print settings to an a{sv} variant.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     a new, floating, GVariant.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.print-settings.lisp ------------------------------------
