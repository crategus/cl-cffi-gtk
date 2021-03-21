;;; ----------------------------------------------------------------------------
;;; gtk.page-setup.lisp
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
;;; GtkPageSetup
;;;
;;;     Stores page setup information
;;;
;;; Types and Values
;;;
;;;     GtkPageSetup
;;;
;;; Functions
;;;
;;;     gtk_page_setup_new
;;;     gtk_page_setup_copy
;;;     gtk_page_setup_get_orientation
;;;     gtk_page_setup_set_orientation
;;;     gtk_page_setup_get_paper_size
;;;     gtk_page_setup_set_paper_size
;;;     gtk_page_setup_get_top_margin
;;;     gtk_page_setup_set_top_margin
;;;     gtk_page_setup_get_bottom_margin
;;;     gtk_page_setup_set_bottom_margin
;;;     gtk_page_setup_get_left_margin
;;;     gtk_page_setup_set_left_margin
;;;     gtk_page_setup_get_right_margin
;;;     gtk_page_setup_set_right_margin
;;;     gtk_page_setup_set_paper_size_and_default_margins
;;;     gtk_page_setup_get_paper_width
;;;     gtk_page_setup_get_paper_height
;;;     gtk_page_setup_get_page_width
;;;     gtk_page_setup_get_page_height
;;;
;;;     gtk_page_setup_new_from_file
;;;     gtk_page_setup_new_from_key_file
;;;     gtk_page_setup_new_from_gvariant
;;;     gtk_page_setup_load_file
;;;     gtk_page_setup_load_key_file
;;;     gtk_page_setup_to_file
;;;     gtk_page_setup_to_key_file
;;;     gtk_page_setup_to_gvariant
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkPageSetup
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPageSetup
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPageSetup" gtk-page-setup
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_page_setup_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-page-setup 'type)
 "@version{2021-3-17}
  @begin{short}
    A @sym{gtk-page-setup} object stores the page size, orientation and margins.
  @end{short}
  The idea is that you can get one of these from the page setup dialog and then
  pass it to the @class{gtk-print-operation} object when printing. The benefit
  of splitting this out of the @class{gtk-print-settings} object is that these
  affect the actual layout of the page, and thus need to be set long before
  user prints.

  The margins specified in this object are the \"print margins\", i.e. the
  parts of the page that the printer cannot print on. These are different from
  the layout margins that a word processor uses. They are typically used to
  determine the minimal size for the layout margins.

  To obtain a @sym{gtk-page-setup} object use the function
  @fun{gtk-page-setup-new} to get the defaults, or use the function
  @fun{gtk-print-run-page-setup-dialog} to show the page setup dialog and
  receive the resulting page setup.
  @begin[Example]{dictionary}
    A page setup dialog.
    @begin{pre}
(defun do-page-setup (settings page-setup)
  (when (not settings)
    ;; Set default print settings
    (setf settings (gtk-print-settings-new)))
  ;; Return the new page setup from the dialog
  (gtk-print-run-page-setup-dialog window page-setup settings))
    @end{pre}
  @end{dictionary}
  @see-class{gtk-print-operation}
  @see-class{gtk-print-settings}")

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-page-setup-new))

(defun gtk-page-setup-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @return{A new @class{gtk-page-setup} object.}
  @short{Creates a new page setup object.}
  @see-class{gtk-page-setup}"
  (make-instance 'gtk-page-setup))

(export 'gtk-page-setup-new)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_copy" gtk-page-setup-copy) (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[setup]{the @class{gtk-page-setup} object to copy}
  @return{A copy of @arg{setup}.}
  @short{Copies a page setup object.}
  @see-class{gtk-page-setup}"
  (setup (g-object gtk-page-setup)))

(export 'gtk-page-setup-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_orientation ()
;;; gtk_page_setup_set_orientation ()
;;; ----------------------------------------------------------------------------

(defun (setf gtk-page-setup-orientation) (orientation setup)
  (foreign-funcall "gtk_page_setup_set_orientation"
                   (g-object gtk-page-setup) setup
                   gtk-page-orientation orientation
                   :void)
  orientation)

(defcfun ("gtk_page_setup_get_orientation" gtk-page-setup-orientation)
    gtk-page-orientation
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @syntax[]{(gtk-page-setup-orientation setup) => orientation}
  @syntax[]{(setf (gtk-page-setup-orientation setup) orientation)}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[orientation]{a @symbol{gtk-page-orientation} value}
  @begin{short}
    Accessor for the page orientation of a page setup object.
  @end{short}

  The function @sym{gtk-page-setup-orientation} gets the page orientation of
  the page setup object. The function @sym{(setf gtk-page-setup-orientation)}
  sets the page orientation.

  Possible values are @code{:portrait} and @code{:landscape}.
  @begin[Example]{dictionary}
    Get the default page orientation.
    @begin{pre}
(let ((setup (gtk-page-setup-new)))
  (gtk-page-setup-orientation setup))
=> :PORTRAIT
    @end{pre}
  @end{dictionary}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-page-orientation}"
  (setup (g-object gtk-page-setup)))

(export 'gtk-page-setup-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_size ()
;;; gtk_page_setup_set_paper_size () -> gtk-page-setup-paper-size
;;; ----------------------------------------------------------------------------

(defun (setf gtk-page-setup-paper-size) (size setup)
  (foreign-funcall "gtk_page_setup_set_paper_size"
                   (g-object gtk-page-setup) setup
                   (g-boxed-foreign gtk-paper-size) size
                   :void)
  size)

(defcfun ("gtk_page_setup_get_paper_size" gtk-page-setup-paper-size)
    (g-boxed-foreign gtk-paper-size)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @syntax[]{(gtk-page-setup-paper-size setup) => size}
  @syntax[]{(setf (gtk-page-setup-paper-size setup) size)}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[size]{a @class{gtk-paper-size} instance}
  @begin{short}
    Accessor for the paper size of a page setup object.
  @end{short}

  The function @sym{gtk-page-setup-paper-size} gets the paper size of the page
  setup object. The function @sym{(setf gtk-page-setup-paper-size)} sets the
  paper size without changing the margins. See the function
  @fun{gtk-page-setup-set-paper-size-and-default-margins}.
  @see-class{gtk-page-setup}
  @see-class{gtk-paper-size}
  @see-function{gtk-page-setup-set-paper-size-and-default-margins}"
  (setup (g-object gtk-page-setup)))

(export 'gtk-page-setup-paper-size)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_top_margin ()
;;; gtk_page_setup_set_top_margin () -> gtk-page-setup-top-margin
;;; ----------------------------------------------------------------------------

(defun (setf gtk-page-setup-top-margin) (margin setup unit)
  (foreign-funcall "gtk_page_setup_set_top_margin"
                   (g-object gtk-page-setup) setup
                   :double (coerce margin 'double-float)
                   gtk-unit unit
                   :void)
  margin)

(defcfun ("gtk_page_setup_get_top_margin" gtk-page-setup-top-margin) :double
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @syntax[]{(gtk-page-setup-top-margin setup unit) => margin}
  @syntax[]{(setf (gtk-page-setup-top-margin setup unit) margin)}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{a @symbol{gtk-unit} value}
  @argument[margin]{a double float with the top margin in units of @arg{unit}}
  @begin{short}
    Accessor of the top margin of the page setup in units of @arg{unit}.
  @end{short}

  The function @sym{gtk-page-setup-top-margin} gets the top margin of
  the page setup in units of @arg{unit}. The function
  @sym{(setf gtk-page-setup-top-margin)} sets the top margin.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-top-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_bottom_margin ()
;;; gtk_page_setup_set_bottom_margin () -> gtk-page-setup-bottom-margin
;;; ----------------------------------------------------------------------------

(defun (setf gtk-page-setup-bottom-margin) (margin setup unit)
  (foreign-funcall "gtk_page_setup_set_bottom_margin"
                   (g-object gtk-page-setup) setup
                   :double (coerce margin 'double-float)
                   gtk-unit unit
                   :void)
  margin)

(defcfun ("gtk_page_setup_get_bottom_margin" gtk-page-setup-bottom-margin)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @syntax[]{(gtk-page-setup-bottom-margin setup unit) => margin}
  @syntax[]{(setf gtk-page-setup-bottom-margin setup unit) margin)}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value}
  @argument[margin]{a double float with the bottom margin in units of
    @arg{unit}}
  @begin{short}
    Accessor of the bottom margin of the page setup in units of @arg{unit}.
  @end{short}

  The function @sym{gtk-page-setup-bottom-margin} gets the bottom margin of
  the page setup in units of @arg{unit}. The function
  @sym{(setf gtk-page-setup-bottom-margin)} sets the bottom margin.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-bottom-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_left_margin ()
;;; gtk_page_setup_set_left_margin () -> gtk-page-setup-left-margin
;;; ----------------------------------------------------------------------------

(defun (setf gtk-page-setup-left-margin) (margin setup unit)
  (foreign-funcall "gtk_page_setup_set_left_margin"
                   (g-object gtk-page-setup) setup
                   :double (coerce margin 'double-float)
                   gtk-unit unit
                   :void)
  margin)

(defcfun ("gtk_page_setup_get_left_margin" gtk-page-setup-left-margin) :double
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @syntax[]{(gtk-page-setup-left-margin setup unit) => margin}
  @syntax[]{(setf gtk-page-setup-left-margin setup unit) margin)}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value}
  @argument[margin]{a double float with the left margin in units of @arg{unit}}
  @begin{short}
    Accessor of the left margin of the page setup in units of @arg{unit}.
  @end{short}

  The function @sym{gtk-page-setup-left-margin} gets the left margin of
  the page setup in units of @arg{unit}. The function
  @sym{(setf gtk-page-setup-left-margin)} sets the left margin.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-left-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_right_margin ()
;;; gtk_page_setup_set_right_margin () -> gtk-page-setup-right-margin
;;; ----------------------------------------------------------------------------

(defun (setf gtk-page-setup-right-margin) (margin setup unit)
  (foreign-funcall "gtk_page_setup_set_right_margin"
                   (g-object gtk-page-setup) setup
                   :double (coerce margin 'double-float)
                   gtk-unit unit
                   :void)
  margin)

(defcfun ("gtk_page_setup_get_right_margin" gtk-page-setup-right-margin) :double
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @syntax[]{(gtk-page-setup-right-margin setup unit) => margin}
  @syntax[]{(setf gtk-page-setup-right-margin setup unit) margin)}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value}
  @argument[margin]{a double float with the right margin in units of @arg{unit}}
  @begin{short}
    Accessor of the right margin of the page setup in units of @arg{unit}.
  @end{short}

  The function @sym{gtk-page-setup-right-margin} gets the right margin of
  the page setup in units of @arg{unit}. The function
  @sym{(setf gtk-page-setup-right-margin)} sets the right margin.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-right-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_paper_size_and_default_margins ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_set_paper_size_and_default_margins"
           gtk-page-setup-set-paper-size-and-default-margins) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[size]{a @class{gtk-paper-size} instance}
  @begin{short}
    Sets the paper size of the page setup and modifies the margins according
    to the new paper size.
  @end{short}
  @see-class{gtk-page-setup}
  @see-class{gtk-paper-size}"
  (setup (g-object gtk-page-setup))
  (size (g-boxed-foreign gtk-paper-size)))

(export 'gtk-page-setup-set-paper-size-and-default-margins)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_width () -> gtk-page-setup-paper-width
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_paper_width" gtk-page-setup-paper-width) :double
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value for the return value}
  @return{A double float with the paper width.}
  @short{Returns the paper width of the page setup in units of @arg{unit}.}

  Note that this function takes orientation, but not margins into consideration.
  See the function @fun{gtk-page-setup-page-width}.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}
  @see-function{gtk-page-setup-page-width}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-paper-width)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_height () -> gtk-page-setup-paper-height
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_paper_height" gtk-page-setup-paper-height) :double
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value for the return value}
  @return{A double float with the paper height.}
  @short{Returns the paper height of the page setup in units of @arg{unit}.}

  Note that this function takes orientation, but not margins into consideration.
  See the function @fun{gtk-page-setup-page-height}.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}
  @see-function{gtk-page-setup-page-height}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-paper-height)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_page_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_page_width" gtk-page-setup-page-width) :double
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value for the return value}
  @return{A double float with the page width.}
  @short{Returns the page width of the page setup in units of @arg{unit}.}

  Note that this function takes orientation and margins into consideration.
  See the function @fun{gtk-page-setup-paper-width}.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}
  @see-function{gtk-page-setup-paper-width}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-page-width)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_page_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_page_height" gtk-page-setup-page-height) :double
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value for the return value}
  @return{A double float with the page height.}
  @short{Returns the page height of the page setup in units of @arg{unit}.}

  Note that this function takes orientation and margins into consideration.
  See the function @fun{gtk-page-setup-paper-height}.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}
  @see-function{gtk-page-setup-paper-height}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-page-height)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_new_from_file" %gtk-page-setup-new-from-file)
    (g-object gtk-page-setup)
  (filename :string)
  (err :pointer))

(defun gtk-page-setup-new-from-file (filename)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[filename]{a string with the filename to read the page setup from}
  @return{The restored @class{gtk-page-setup} object.}
  @begin{short}
    Reads the page setup from a file.
  @end{short}
  Returns a new @class{gtk-page-setup} object with the restored page setup,
  or @code{nil} if an error occurred. See the function
  @fun{gtk-page-setup-to-file}.
  @see-class{gtk-page-setup}
  @see-function{gtk-page-setup-to-file}"
  (with-ignore-g-error (err)
    (%gtk-page-setup-new-from-file filename err)))

(export 'gtk-page-setup-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_new_from_key_file" %gtk-page-setup-new-from-key-file)
    (g-object gtk-page-setup)
  (keyfile (:pointer (:struct g-key-file)))
  (groupname :string)
  (err :pointer))

(defun gtk-page-setup-new-from-key-file (keyfile groupname)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[keyfile]{a @type{g-key-file} instance to retrieve the page setup
    from}
  @argument[groupname]{a string with the name of the group in the key file to
    read, or @code{nil} to use the default name \"Page Setup\"}
  @return{The restored @class{gtk-page-setup} object.}
  @begin{short}
    Reads the page setup from the group @arg{groupname} in the key file.
  @end{short}
  Returns a new @class{gtk-page-setup} object with the restored page setup, or
  @code{nil} if an error occurred.
  @see-class{gtk-page-setup}
  @see-type{g-key-file}"
  (with-ignore-g-error (err)
    (%gtk-page-setup-new-from-key-file keyfile groupname err)))

(export 'gtk-page-setup-new-from-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_gvariant ()
;;; ----------------------------------------------------------------------------

#+gtk-3-22
(defcfun ("gtk_page_setup_new_from_gvariant" gtk-page-setup-new-from-gvariant)
    (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[variant]{a @code{a{sv@}} @type{g-variant} instance}
  @return{A new @class{gtk-page-setup} object.}
  @begin{short}
    Desrialize a page setup from an @code{a{sv@}} variant in the format
    produced by the function @fun{gtk-page-setup-to-gvariant}.
  @end{short}

  Since 3.22
  @see-class{gtk-page-setup}
  @see-type{g-variant}
  @see-function{gtk-page-setup-to-gvariant}"
  (variant (:pointer (:struct g-variant))))

#+gtk-3-22
(export 'gtk-page-setup-new-from-gvariant)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_load_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_load_file" %gtk-page-setup-load-file) :boolean
  (page-setup (g-object gtk-page-setup))
  (filename g-string)
  (err :pointer))

(defun gtk-page-setup-load-file (setup filename)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[filename]{a string with the filename to read the page setup from}
  @return{@em{True} on sucess.}
  @begin{short}
    Reads the page setup from a file.
  @end{short}
  See the function @fun{gtk-page-setup-to-file}.
  @see-class{gtk-page-setup}
  @see-function{gtk-page-setup-to-file}"
  (with-ignore-g-error (err)
    (%gtk-page-setup-load-file setup filename err)))

(export 'gtk-page-setup-load-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_load_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_load_key_file" %gtk-page-setup-load-key-file) :boolean
  (setup (g-object gtk-page-setup))
  (keyfile (:pointer (:struct g-key-file)))
  (groupname :string)
  (err :pointer))

(defun gtk-page-setup-load-key-file (setup keyfile groupname)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[keyfile]{the @type{g-key-file} instance to retrieve the page
    setup from}
  @argument[groupname]{a string with the name of the group in the key file to
    read, or @code{nil} to use the default name \"Page Setup\"}
  @return{@em{True} on sucess.}
  @begin{short}
    Reads the page setup from the group @arg{groupname} in the key file.
  @end{short}
  @see-class{gtk-page-setup}
  @see-type{g-key-file}"
  (with-ignore-g-error (err)
    (%gtk-page-setup-load-key-file setup keyfile groupname err)))

(export 'gtk-page-setup-load-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_to_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_to_file" %gtk-page-setup-to-file) :boolean
  (setup (g-object gtk-page-setup))
  (filename :string)
  (err :pointer))

(defun gtk-page-setup-to-file (setup filename)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[filename]{a string with the file to save to}
  @return{@em{True} on sucess.}
  @begin{short}
    The function saves the information from the page setup to a file.
  @end{short}
  @see-class{gtk-page-setup}"
  (with-ignore-g-error (err)
    (%gtk-page-setup-to-file setup filename err)))

(export 'gtk-page-setup-to-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_to_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_to_key_file" %gtk-page-setup-to-key-file) :void
  (setup (g-object gtk-page-setup))
  (keyfile (:pointer (:struct g-key-file)))
  (groupname :string))

(defun gtk-page-setup-to-key-file (setup keyfile groupname)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[keyfile]{the @type{g-key-file} instance to save the page setup to}
  @argument[groupname]{a string with the group to add the settings to in the key
    file, or @code{nil} to use the default name \"Page Setup\"}
  @begin{short}
    The function adds the page setup from the page setup to a key file.
  @end{short}
  @see-class{gtk-page-setup}
  @see-type{g-key-file}"
  (%gtk-page-setup-to-key-file setup
                               keyfile
                               (if groupname groupname (null-pointer))))

(export 'gtk-page-setup-to-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_to_gvariant ()
;;; ----------------------------------------------------------------------------

#+gtk-3-22
(defcfun ("gtk_page_setup_to_gvariant" gtk-page-setup-to-gvariant)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2021-3-17}
  @argument[setup]{a @class{gtk-page-setup} object}
  @return{A new @type{g-variant} instance.}
  @begin{short}
    Serialize the page setup to an @code{a{sv@}} variant.
  @end{short}

  Since 3.22
  @see-class{gtk-page-setup}
  @see-type{g-variant}"
  (setup (g-object gtk-page-setup)))

#+gtk-3-22
(export 'gtk-page-setup-to-gvariant)

;;; ---- End of file gtk.page-setup.lisp ---------------------------------------
