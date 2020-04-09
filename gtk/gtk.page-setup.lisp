;;; ----------------------------------------------------------------------------
;;; gtk.page-setup.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
 "@version{2013-11-17}
  @begin{short}
    A @sym{gtk-page-setup} object stores the page size, orientation and margins.
  @end{short}
  The idea is that you can get one of these from the page setup dialog and then
  pass it to the @class{gtk-print-operation} object when printing. The benefit
  of splitting this out of the @class{gtk-print-settings} object is that these
  affect the actual layout of the page, and thus need to be set long before user
  prints.

  The margins specified in this object are the \"print margins\", i. e. the
  parts of the page that the printer cannot print on. These are different from
  the layout margins that a word processor uses; they are typically used to
  determine the minimal size for the layout margins.

  To obtain a @sym{gtk-page-setup} object use the function
  @fun{gtk-page-setup-new} to get the defaults, or use the function
  @fun{gtk-print-run-page-setup-dialog}  to show the page setup dialog and
  receive the resulting page setup.
  @begin[Example]{dictionary}
    A page setup dialog.
    @begin{pre}
   static GtkPrintSettings *settings = NULL;
   static GtkPageSetup *page_setup = NULL;

   static void
   do_page_setup (void)
   {
     GtkPageSetup *new_page_setup;

     if (settings == NULL)
       settings = gtk_print_settings_new ();

     new_page_setup = gtk_print_run_page_setup_dialog
                                                   (GTK_WINDOW (main_window),
                                                    page_setup, settings);

     if (page_setup)
       g_object_unref (page_setup);

     page_setup = new_page_setup;
   @}
    @end{pre}
  @end{dictionary}
  @see-class{gtk-print-operation}
  @see-class{gtk-print-settings}
  @see-function{gtk-page-setup-new}
  @see-function{gtk-print-run-page-setup-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-page-setup-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @return{A new @class{gtk-page-setup} object.}
  @short{Creates a new @class{gtk-page-setup} object.}
  @see-class{gtk-page-setup}"
  (make-instance 'gtk-page-setup))

(export 'gtk-page-setup-new)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_copy" gtk-page-setup-copy) (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[other]{the @class{gtk-page-setup} object to copy}
  @return{A copy of @arg{other}.}
  @short{Copies a page setup object.}
  @see-class{gtk-page-setup}"
  (other (g-object gtk-page-setup)))

(export 'gtk-page-setup-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_orientation ()
;;; gtk_page_setup_set_orientation ()
;;; ----------------------------------------------------------------------------

(defun (setf gtk-page-setup-orientation) (orientation page-setup)
  (foreign-funcall "gtk_page_setup_set_orientation"
                   (g-object gtk-page-setup) page-setup
                   gtk-page-orientation orientation
                   :void)
  orientation)

(defcfun ("gtk_page_setup_get_orientation" gtk-page-setup-orientation)
    gtk-page-orientation
 #+cl-cffi-gtk-documentation
 "@version{2020-3-28}
  @syntax[]{(gtk-page-setup-orientation page-setup) => orientation}
  @syntax[]{(setf (gtk-page-setup-orientation page-setup) orientation)}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[orientation]{a @symbol{gtk-page-orientation} value}
  @begin{short}
    Accessor for the page orientation of a page setup object.
  @end{short}

  The function @sym{gtk-page-setup-orientation} gets the page orientation of
  the page setup object. The function @sym{(setf gtk-page-setup-orientation)}
  sets the page orientation of the page setup object.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-page-orientation}"
  (page-setup (g-object gtk-page-setup)))

(export 'gtk-page-setup-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_size ()
;;; gtk_page_setup_set_paper_size ()
;;; ----------------------------------------------------------------------------

(defun (setf gtk-page-setup-paper-size) (paper-size page-setup)
  (foreign-funcall "gtk_page_setup_set_paper_size"
                   (g-object gtk-page-setup) page-setup
                   (g-boxed-foreign gtk-paper-size) paper-size
                   :void)
  paper-size)

(defcfun ("gtk_page_setup_get_paper_size" gtk-page-setup-paper-size)
    (g-boxed-foreign gtk-paper-size)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-28}
  @syntax[]{(gtk-page-setup-paper-size page-setup) => paper-size}
  @syntax[]{(setf (gtk-page-setup-paper-size page-setup) paper-size)}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[paper-size]{a @class{gtk-paper-size} structure}
  @begin{short}
    Accessor for the paper size of a page setup object.
  @end{short}

  The function @sym{gtk-page-setup-paper-size} gets the paper size of the page
  setup object. The function @sym{(setf gtk-page-setup-paper-size)} sets the
  paper size of the page setup object without changing the margins. See the
  function @fun{gtk-page-setup-paper-size-and-default-margins}.
  @see-class{gtk-page-setup}
  @see-class{gtk-paper-size}
  @see-function{gtk-page-setup-paper-size-and-default-margins}"
  (page-setup (g-object gtk-page-setup)))

(export 'gtk-page-setup-paper-size)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_top_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_top_margin" gtk-page-setup-get-top-margin) :double
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[unit]{a @symbol{gtk-unit} value for the return value}
  @return{A @code{:double} with the top margin.}
  @short{Gets the top margin of the page setup in units of @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (page-setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-top-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_top_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_set_top_margin" %gtk-page-setup-set-top-margin) :void
  (setup (g-object gtk-page-setup))
  (margin :double)
  (unit gtk-unit))

(defun gtk-page-setup-set-top-margin (page-setup margin unit)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[margin]{a number with the new top margin in units of @arg{unit}}
  @argument[unit]{a @symbol{gtk-unit} value}
  @short{Sets the top margin of the page setup in units of @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (%gtk-page-setup-set-top-margin page-setup
                                  (coerce margin 'double-float)
                                  unit))

(export 'gtk-page-setup-set-top-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_bottom_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_bottom_margin" gtk-page-setup-get-bottom-margin)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value for the return value}
  @return{A @code{:double} with the bottom margin.}
  @short{Gets the bottom margin of the page setup in units of @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (page-setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-bottom-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_bottom_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_set_bottom_margin" %gtk-page-setup-set-bottom-margin)
    :void
  (page-setup (g-object gtk-page-setup))
  (margin :double)
  (unit gtk-unit))

(defun gtk-page-setup-set-bottom-margin (page-setup margin unit)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[margin]{a number with the new bottom margin in units of @arg{unit}}
  @argument[unit]{a @symbol{gtk-unit} value}
  @short{Sets the bottom margin of the page setup in units of @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (%gtk-page-setup-set-bottom-margin page-setup
                                     (coerce margin 'double-float)
                                     unit))

(export 'gtk-page-setup-set-bottom-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_left_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_left_margin" gtk-page-setup-get-left-margin)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the unit for the return value}
  @return{A @code{:double} with the left margin.}
  @short{Gets the left margin of the page setup in units of @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (page-setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-left-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_left_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_set_left_margin" %gtk-page-setup-set-left-margin)
    :void
  (setup (g-object gtk-page-setup))
  (margin :double)
  (unit gtk-unit))

(defun gtk-page-setup-set-left-margin (page-setup margin unit)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[margin]{a number with the new left margin in units of @arg{unit}}
  @argument[unit]{a @symbol{gtk-unit} value}
  @short{Sets the left margin of the page setup in units of @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (%gtk-page-setup-set-left-margin page-setup
                                   (coerce margin 'double-float)
                                   unit))

(export 'gtk-page-setup-set-left-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_right_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_right_margin" gtk-page-setup-get-right-margin)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value for the return value}
  @return{A @code{:double} with the right margin.}
  @short{Gets the right margin of the page setup in units of @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (page-setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-right-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_right_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_set_right_margin" %gtk-page-setup-set-right-margin)
    :void
  (setup (g-object gtk-page-setup))
  (margin :double)
  (unit gtk-unit))

(defun gtk-page-setup-set-right-margin (page-setup margin unit)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[margin]{a number with the new right margin in units of @arg{unit}}
  @argument[unit]{a @symbol{gtk-unit} value}
  @short{Sets the right margin of the page setup in units of @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (%gtk-page-setup-set-right-margin page-setup
                                    (coerce margin 'double-float)
                                    unit))

(export 'gtk-page-setup-set-right-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_paper_size_and_default_margins ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_set_paper_size_and_default_margins"
           gtk-page-setup-set-paper-size-and-default-margins) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[paper-size]{a @class{gtk-paper-size} structure}
  @begin{short}
    Sets the paper size of the page setup and modifies the margins according
    to the new paper size.
  @end{short}
  @see-class{gtk-page-setup}
  @see-class{gtk-paper-size}"
  (page-setup (g-object gtk-page-setup))
  (size (g-boxed-foreign gtk-paper-size)))

(export 'gtk-page-setup-set-paper-size-and-default-margins)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_paper_width" gtk-page-setup-get-paper-width)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value for the return value}
  @return{A @code{:double} with the paper width.}
  @short{Returns the paper width of the page setup in units of @arg{unit}.}

  Note that this function takes orientation, but not margins into consideration.
  See the function @fun{gtk-page-setup-get-page-width}.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}
  @see-function{gtk-page-setup-get-page-width}"
  (page-setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-paper-width)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_paper_height" gtk-page-setup-get-paper-height)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value for the return value}
  @return{A @code{:double} with the paper height.}
  @short{Returns the paper height of the page setup in units of @arg{unit}.}

  Note that this function takes orientation, but not margins into consideration.
  See the function @fun{gtk-page-setup-get-page-height}.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}
  @see-function{gtk-page-setup-get-page-height}"
  (page-setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-paper-height)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_page_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_page_width" gtk-page-setup-get-page-width)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value for the return value}
  @return{A @code{:double} with the page width.}
  @short{Returns the page width of the page setup in units of @arg{unit}.}

  Note that this function takes orientation and margins into consideration.
  See the function @fun{gtk-page-setup-get-paper-width}.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}
  @see-function{gtk-page-setup-get-paper-width}"
  (page-setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-page-width)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_page_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_page_height" gtk-page-setup-get-page-height)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the @symbol{gtk-unit} value for the return value}
  @return{A @code{:double} with the page height.}
  @short{Returns the page height of the page setup in units of @arg{unit}.}

  Note that this function takes orientation and margins into consideration.
  See the function @fun{gtk-page-setup-get-paper-height}.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}
  @see-function{gtk-page-setup-get-paper-height}"
  (page-setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-page-height)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_new_from_file" %gtk-page-setup-new-from-file)
    (g-object gtk-page-setup)
  (filename g-string)
  (error :pointer))

(defun gtk-page-setup-new-from-file (filename)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[filename]{a string with the filename to read the page setup from}
  @return{the restored @class{gtk-page-setup} object}
  @begin{short}
    Reads the page setup from a file.
  @end{short}
  Returns a new @class{gtk-page-setup} object with the restored page setup, or
  @code{nil} if an error occurred. See the function
  @fun{gtk-page-setup-to-file}.
  @see-class{gtk-page-setup}
  @see-function{gtk-page-setup-to-file}"
  (with-g-error (err)
    (%gtk-page-setup-new-from-file filename err)))

(export 'gtk-page-setup-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_new_from_key_file" %gtk-page-setup-new-from-key-file)
    (g-object gtk-page-setup)
  (key-file (:pointer (:struct g-key-file)))
  (group-name g-string)
  (error :pointer))

(defun gtk-page-setup-new-from-key-file (key-file group-name)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[key-file]{a @type{g-key-file} structure to retrieve the page setup
    from}
  @argument[group-name]{a string with the name of the group in the key file to
    read, or @code{nil} to use the default name \"Page Setup\"}
  @return{the restored @class{gtk-page-setup} object}
  @begin{short}
    Reads the page setup from the group @arg{group-name} in the key file.
  @end{short}
  Returns a new @class{gtk-page-setup} object with the restored page setup, or
  @code{nil} if an error occurred.
  @see-class{gtk-page-setup}"
  (with-g-error (err)
    (%gtk-page-setup-new-from-key-file key-file group-name err)))

(export 'gtk-page-setup-new-from-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_gvariant ()
;;; ----------------------------------------------------------------------------

#+gtk-3-22
(defcfun ("gtk_page_setup_new_from_gvariant" gtk-page-setup-new-from-gvariant)
    (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[variant]{an @code{a{sv@}} @symbol{g-variant} structure}
  @return{A new @class{gtk-page-setup} object.}
  @begin{short}
    Desrialize a page setup from an @code{a{sv@}} variant in the format
    produced by the function @fun{gtk-page-setup-to-gvariant}.
  @end{short}

  Since 3.22
  @see-class{gtk-page-setup}
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
  (error :pointer))

(defun gtk-page-setup-load-file (page-setup filename)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[filename]{a string with the filename to read the page setup from}
  @return{@em{True} on sucess.}
  @begin{short}
    Reads the page setup from a file.
  @end{short}
  See the function @fun{gtk-page-setup-to-file}.
  @see-class{gtk-page-setup}
  @see-function{gtk-page-setup-to-file}"
  (with-g-error (err)
    (%gtk-page-setup-load-file page-setup filename err)))

(export 'gtk-page-setup-load-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_load_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_load_key_file" %gtk-page-setup-load-key-file) :boolean
  (page-setup (g-object gtk-page-setup))
  (key-file (:pointer (:struct g-key-file)))
  (group-name g-string)
  (error :pointer))

(defun gtk-page-setup-load-key-file (page-setup key-file group-name)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[key-file]{the @type{g-key-file} structure to retrieve the page
    setup from}
  @argument[group-name]{a string with the name of the group in the key file to
    read, or @code{nil} to use the default name \"Page Setup\"}
  @return{@em{True} on sucess.}
  @begin{short}
    Reads the page setup from the group @arg{group-name} in the key file.
  @end{short}
  @see-class{gtk-page-setup}"
  (with-g-error (err)
    (%gtk-page-setup-load-key-file page-setup key-file group-name err)))

(export 'gtk-page-setup-load-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_to_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_to_file" %gtk-page-setup-to-file) :boolean
  (page-setup (g-object gtk-page-setup))
  (filename g-string)
  (error :pointer))

(defun gtk-page-setup-to-file (page-setup filename)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[filename]{a string with the file to save to}
  @return{@em{True} on sucess.}
  @begin{short}
    The function saves the information from the page setup to a file.
  @end{short}
  @see-class{gtk-page-setup}"
  (with-g-error (err)
    (%gtk-page-setup-to-file page-setup filename err)))

(export 'gtk-page-setup-to-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_to_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_to_key_file" gtk-page-setup-to-key-file) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @argument[key-file]{the @type{g-key-file} structure to save the page setup to}
  @argument[group-name]{the group to add the settings to in the key file, or
    @code{nil} to use the default name \"Page Setup\"}
  @begin{short}
    The function adds the page setup from the page setup to a key file.
  @end{short}
  @see-class{gtk-page-setup}"
  (page-setup (g-object gtk-page-setup))
  (key-file (:pointer (:struct g-key-file)))
  (group-name g-string))

(export 'gtk-page-setup-to-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_to_gvariant ()
;;; ----------------------------------------------------------------------------

#+gtk-3-22
(defcfun ("gtk_page_setup_to_gvariant" gtk-page-setup-to-gvariant)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-3-30}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @return{A new @type{g-variant} structure.}
  @begin{short}
    Serialize the page setup to an @code{a{sv@}} variant.
  @end{short}

  Since 3.22
  @see-class{gtk-page-setup}"
  (page-setup (g-object gtk-page-setup)))

#+gtk-3-22
(export 'gtk-page-setup-to-gvariant)

;;; ---- End of file gtk.page-setup.lisp ---------------------------------------
