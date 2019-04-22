;;; ----------------------------------------------------------------------------
;;; gtk.print-unix-dialog.lisp
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
;;; GtkPrintUnixDialog
;;;
;;;     A print dialog
;;;
;;; Types and Values
;;;
;;;     GtkPrintUnixDialog
;;;     GtkPrintCapabilities
;;;
;;; Functions
;;;
;;;     gtk_print_unix_dialog_new
;;;     gtk_print_unix_dialog_set_page_setup
;;;     gtk_print_unix_dialog_get_page_setup
;;;     gtk_print_unix_dialog_set_current_page
;;;     gtk_print_unix_dialog_get_current_page
;;;     gtk_print_unix_dialog_set_settings
;;;     gtk_print_unix_dialog_get_settings
;;;     gtk_print_unix_dialog_get_selected_printer
;;;     gtk_print_unix_dialog_add_custom_tab
;;;     gtk_print_unix_dialog_set_support_selection
;;;     gtk_print_unix_dialog_get_support_selection
;;;     gtk_print_unix_dialog_set_has_selection
;;;     gtk_print_unix_dialog_get_has_selection
;;;     gtk_print_unix_dialog_set_embed_page_setup
;;;     gtk_print_unix_dialog_get_embed_page_setup
;;;     gtk_print_unix_dialog_get_page_setup_set
;;;
;;;     gtk_print_unix_dialog_set_manual_capabilities
;;;     gtk_print_unix_dialog_get_manual_capabilities
;;;
;;; Properties
;;;
;;;                 gint   current-page           Read / Write
;;;             gboolean   embed-page-setup       Read / Write
;;;             gboolean   has-selection          Read / Write
;;; GtkPrintCapabilities   manual-capabilities    Read / Write
;;;         GtkPageSetup*  page-setup             Read / Write
;;;     GtkPrintSettings*  print-settings         Read / Write
;;;           GtkPrinter*  selected-printer       Read
;;;             gboolean   support-selection      Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkDialog
;;;                             ╰── GtkPrintUnixDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPrintUnixDialog implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPrintUnixDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPrintUnixDialog" gtk-print-unix-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_print_unix_dialog_get_type")
  ((current-page
    gtk-print-unix-dialog-current-page
    "current-page" "gint" t t)
   (embed-page-setup
    gtk-print-unix-dialog-embed-page-setup
    "embed-page-setup" "gboolean" t t)
   (has-selection
    gtk-print-unix-dialog-has-selection
    "has-selection" "gboolean" t t)
   (manual-capabilities
    gtk-print-unix-dialog-manual-capabilities
    "manual-capabilities" "GtkPrintCapabilities" t t)
   (page-setup
    gtk-print-unix-dialog-page-setup
    "page-setup" "GtkPageSetup" t t)
   (print-settings
    gtk-print-unix-dialog-print-settings
    "print-settings" "GtkPrintSettings" t t)
   (selected-printer
    gtk-print-unix-dialog-selected-printer
    "selected-printer" "GtkPrinter" t nil)
   (support-selection
    gtk-print-unix-dialog-selected-printer
    "support-selection" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-print-unix-dialog 'type)
 "@version{2013-5-30}
  @begin{short}
    @sym{gtk-print-unix-dialog} implements a print dialog for platforms which
    do not provide a native print dialog, like Unix. It can be used very much
    like any other GTK+ dialog, at the cost of the portability offered by the
    high-level printing API.
  @end{short}

  In order to print something with @sym{gtk-print-unix-dialog}, you need to use
  the @fun{gtk-print-unix-dialog-get-selected-printer} function to obtain a
  @class{gtk-printer} object and use it to construct a @class{gtk-print-job}
  using the @fun{gtk-print-job-new} function.

  @sym{gtk-print-unix-dialog} uses the following response values:
  @begin[code]{table}
    @entry[:ok]{for the \"Print\" button}
    @entry[:apply]{for the \"Preview\" button}
    @entry[:cancel]{for the \"Cancel\" button}
  @end{table}
  Printing support was added in GTK+ 2.10.

  @subheading{@sym{gtk-print-unix-dialog} as @class{gtk-buildable}}
    The @sym{gtk-print-unix-dialog} implementation of the @class{gtk-buildable}
    interface exposes its notebook internal children with the name \"notebook\".

    @b{Example:} A @sym{gtk-print-unix-dialog} UI definition fragment.
    @begin{pre}
   <object class=\"GtkPrintUnixDialog\" id=\"dialog1\">
     <child internal-child=\"notebook\">
       <object class=\"GtkNotebook\" id=\"notebook\">
         <child>
           <object class=\"GtkLabel\" id=\"tabcontent\">
           <property name=\"label\">Content on notebook tab</property>
           </object>
         </child>
         <child type=\"tab\">
           <object class=\"GtkLabel\" id=\"tablabel\">
             <property name=\"label\">Tab label</property>
           </object>
           <packing>
             <property name=\"tab_expand\">False</property>
             <property name=\"tab_fill\">False</property>
           </packing>
         </child>
       </object>
     </child>
   </object>
    @end{pre}
  @see-slot{gtk-print-unix-dialog-current-page}
  @see-slot{gtk-print-unix-dialog-embed-page-setup}
  @see-slot{gtk-print-unix-dialog-has-selection}
  @see-slot{gtk-print-unix-dialog-manual-capabilities}
  @see-slot{gtk-print-unix-dialog-page-setup}
  @see-slot{gtk-print-unix-dialog-print-settings}
  @see-slot{gtk-print-unix-dialog-selected-printer}
  @see-slot{gtk-print-unix-dialog-support-selection}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "current-page"
                                               'gtk-print-unix-dialog) 't)
 "The @code{\"current-page\"} property of type @code{:int} (Read / Write) @br{}
  The current page in the document. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "embed-page-setup"
                                               'gtk-print-unix-dialog) 't)
 "The @code{\"embed-page-setup\"} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if page setup combos are embedded in @sym{gtk-print-unix-dialog}.
  @br{}
  Default value: @code{nil} @br{}")


#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-selection"
                                               'gtk-print-unix-dialog) 't)
 "The @code{\"has-selection\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the application has a selection. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "manual-capabilities"
                                               'gtk-print-unix-dialog) 't)
 "The @code{\"manual-capabilities\"} property of type
  @symbol{gtk-print-capabilities} (Read / Write) @br{}
  Capabilities the application can handle.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page-setup"
                                               'gtk-print-unix-dialog) 't)
 "The @code{\"page-setup\"} property of type @class{gtk-page-setup}
  (Read / Write) @br{}
  The @class{gtk-page-setup} to use.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "print-settings"
                                               'gtk-print-unix-dialog) 't)
 "The @code{\"print-settings\"} property of type @class{gtk-print-settings}
  (Read / Write) @br{}
  The @class{gtk-print-settings} used for initializing the dialog.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selected-printer"
                                               'gtk-print-unix-dialog) 't)
 "The @code{\"selected-printer\"} property of type @class{gtk-printer}
  (Read) @br{}
  The @class{gtk-printer} which is selected.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "support-selection"
                                               'gtk-print-unix-dialog) 't)
 "The @code{\"support-selection\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the dialog supports selection. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-unix-dialog-current-page atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-unix-dialog-current-page 'function)
 "@version{2013-3-30}
  Accessor of the slot @arg{\"current-page\"} of the
  @class{gtk-print-unix-dialog} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-unix-dialog-embed-page-setup
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-unix-dialog-embed-page-setup 'function)
 "@version{2013-3-30}
  Accessor of the slot @arg{\"embed-page-setup\"} of the
  @class{gtk-print-unix-dialog} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-unix-dialog-has-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-unix-dialog-has-selection 'function)
 "@version{2013-3-30}
  Accessor of the slot @arg{\"has-selection\"} of the
  @class{gtk-print-unix-dialog} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-unix-dialog-manual-capabilities
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-unix-dialog-manual-capabilities 'function)
 "@version{2013-3-30}
  Accessor of the slot @arg{\"manual-capabilities\"} of the
  @class{gtk-print-unix-dialog} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-unix-dialog-page-setup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-unix-dialog-page-setup 'function)
 "@version{2013-3-30}
  Accessor of the slot @arg{\"page-setup\"} of the
  @class{gtk-print-unix-dialog} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-unix-dialog-print-settings
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-unix-dialog-print-settings 'function)
 "@version{2013-3-30}
  Accessor of the slot @arg{\"print-settings\"} of the
  @class{gtk-print-unix-dialog} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-unix-dialog-selected-printer
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-unix-dialog-selected-printer 'function)
 "@version{2013-3-30}
  Accessor of the slot @arg{\"selected-printer\"} of the
  @class{gtk-print-unix-dialog} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-unix-dialog-support-selection
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-print-unix-dialog-support-selection 'function)
 "@version{2013-3-30}
  Accessor of the slot @arg{\"support-selection\"} of the
  @class{gtk-print-unix-dialog} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-new))

(defun gtk-print-unix-dialog-new (title parent)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[title]{title of the dialog, or @code{nil}}
  @argument[parent]{transient parent of the dialog, or @code{nil}}
  @return{A new @class{gtk-print-unix-dialog} widget.}
  @short{Creates a new @class{gtk-print-unix-dialog} widget.}

  Since 2.10"
  (make-instance 'gtk-print-unix-dialog
                 :title title
                 :parent parent))

(export 'gtk-print-unix-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_page_setup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-set-page-setup))

(defun gtk-print-unix-dialog-set-page-setup (dialog page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @argument[page-setup]{a @class{gtk-page-setup} object}
  @begin{short}
    Sets the page setup of the @class{gtk-print-unix-dialog} widget.
  @end{short}

  Since 2.10"
  (setf (gtk-print-unix-dialog-page-setup dialog) page-setup))

(export 'gtk-print-unix-dialog-set-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_page_setup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-get-page-setup))

(defun gtk-print-unix-dialog-get-page-setup (dialog)
 #+cl-cffi-gtk-documentation
 "@version{2014-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @return{The page setup of @arg{dialog}.}
  @begin{short}
    Gets the page setup that is used by the @class{gtk-print-unix-dialog}
    widget.
  @end{short}

  Since 2.10"
  (gtk-print-unix-dialog-page-setup dialog))

(export 'gtk-print-unix-dialog-get-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_current_page ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-set-current-page))

(defun gtk-print-unix-dialog-set-current-page (dialog current-page)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @argument[current-page]{the current page number}
  @begin{short}
    Sets the current page number. If @arg{current-page} is not -1, this enables
    the current page choice for the range of pages to print.
  @end{short}

  Since 2.10"
  (setf (gtk-print-unix-dialog-current-page dialog) current-page))

(export 'gtk-print-unix-dialog-set-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_current_page ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-get-current-page))

(defun gtk-print-unix-dialog-get-current-page (dialog)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @return{The current page of @arg{dialog}.}
  @begin{short}
    Gets the current page of the @class{gtk-print-unix-dialog} widget.
  @end{short}

  Since 2.10"
  (gtk-print-unix-dialog-current-page dialog))

(export 'gtk-print-unix-dialog-get-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_settings ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-set-settings))

(defun gtk-print-unix-dialog-set-settings (dialog settings)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @argument[settings]{a @class{gtk-print-settings}, or @code{nil}}
  @begin{short}
    Sets the @class{gtk-print-settings} for the @class{gtk-print-unix-dialog}.
    Typically, this is used to restore saved print settings from a previous
    print operation before the print dialog is shown.
  @end{short}

  Since 2.10"
  (setf (gtk-print-unix-dialog-print-settings dialog) settings))

(export 'gtk-print-unix-dialog-set-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_settings ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-get-settings))

(defun gtk-print-unix-dialog-get-settings (dialog)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @return{A new @class{gtk-print-settings} object with the values from
    @arg{dialog}.}
  @begin{short}
    Gets a new @class{gtk-print-settings} object that represents the current
    values in the print dialog. Note that this creates a new object, and you
    need to unref it if don't want to keep it.
  @end{short}

  Since 2.10"
  (gtk-print-unix-dialog-print-settings dialog))

(export 'gtk-print-unix-dialog-get-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_selected_printer ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-get-selected-printer))

(defun gtk-print-unix-dialog-get-selected-printer (dialog)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @return{The currently selected printer.}
  @short{Gets the currently selected printer.}

  Since 2.10"
  (gtk-print-unix-dialog-selected-printer dialog))

(export 'gtk-print-unix-dialog-get-selected-printer)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_add_custom_tab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_unix_dialog_add_custom_tab"
           gtk-print-unix-dialog-add-custom-tab) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @argument[child]{the widget to put in the custom tab}
  @argument[tab-label]{the widget to use as tab label}
  @begin{short}
    Adds a custom tab to the print dialog.
  @end{short}

  Since 2.10"
  (dialog (g-object gtk-print-unix-dialog))
  (child (g-object gtk-widget))
  (tab-label (g-object gtk-widget)))

(export 'gtk-print-unix-dialog-add-custom-tab)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_support_selection ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-set-support-selection))

(defun gtk-print-unix-dialog-set-support-selection (dialog support-selection)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @argument[support-selection]{@em{true} to allow print selection}
  @begin{short}
    Sets whether the print dialog allows user to print a selection.
  @end{short}

  Since 2.18"
  (setf (gtk-print-unix-dialog-support-selection dialog) support-selection))

(export 'gtk-print-unix-dialog-set-support-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_support_selection ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-get-support-selection))

(defun gtk-print-unix-dialog-get-support-selection (dialog)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @return{Whether the application supports print of selection.}
  @begin{short}
    Gets the value of the @code{\"support-selection\"} property.
  @end{short}

  Since 2.18"
  (gtk-print-unix-dialog-support-selection dialog))

(export 'gtk-print-unix-dialog-get-support-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_has_selection ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-set-has-selection))

(defun gtk-print-unix-dialog-set-has-selection (dialog has-selection)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @argument[has-selection]{@em{true} indicates that a selection exists}
  @begin{short}
    Sets whether a selection exists.
  @end{short}

  Since 2.18"
  (setf (gtk-print-unix-dialog-has-selection dialog) has-selection))

(export 'gtk-print-unix-dialog-set-has-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_has_selection ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-get-has-selection))

(defun gtk-print-unix-dialog-get-has-selection (dialog)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @return{Whether there is a selection.}
  @begin{short}
    Gets the value of the @code{\"has-selection\"} property.
  @end{short}

  Since 2.18"
  (gtk-print-unix-dialog-has-selection dialog))

(export 'gtk-print-unix-dialog-get-has-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_embed_page_setup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-set-embed-page-setup))

(defun gtk-print-unix-dialog-set-embed-page-setup (dialog embed)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @argument[embed]{embed page setup selection}
  @begin{short}
    Embed page size combo box and orientation combo box into page setup page.
  @end{short}

  Since 2.18"
  (setf (gtk-print-unix-dialog-embed-page-setup dialog) embed))

(export 'gtk-print-unix-dialog-set-embed-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_embed_page_setup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-get-embed-page-setup))

(defun gtk-print-unix-dialog-get-embed-page-setup (dialog)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @return{Whether there is a selection.}
  @begin{short}
    Gets the value of the @code{\"embed-page-setup\"} property.
  @end{short}

  Since 2.18"
  (gtk-print-unix-dialog-embed-page-setup dialog))

(export 'gtk-print-unix-dialog-get-embed-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_page_setup_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_unix_dialog_get_page_setup_set"
           gtk-print-unix-dialog-get-page-setup-set) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @return{Whether a page setup was set by user.}
  @begin{short}
    Gets the page setup that is used by the @class{gtk-print-unix-dialog}
    widget.
  @end{short}

  Since 2.18"
  (dialog (g-object gtk-print-unix-dialog)))

(export 'gtk-print-unix-dialog-get-page-setup-set)

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintCapabilities
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintCapabilities" gtk-print-capabilities
  (:export t
   :type-initializer "gtk_print_capabilities_get_type")
  (:page-set         #.(ash 1 0))
  (:copies           #.(ash 1 1))
  (:collate          #.(ash 1 2))
  (:reverse          #.(ash 1 3))
  (:scale            #.(ash 1 4))
  (:generate-pdf     #.(ash 1 5))
  (:generate-ps      #.(ash 1 6))
  (:preview          #.(ash 1 7))
  (:number-up        #.(ash 1 8))
  (:number-up-layout #.(ash 1 9)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-capabilities atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-print-capabilities atdoc:*external-symbols*)
 "@version{2013-5-30}
  @begin{short}
    An enum for specifying which features the print dialog should offer. If
    neither @code{:generate-pdf} nor @code{:generate-ps} is specified, GTK+
    assumes that all formats are supported.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPrintCapabilities\" gtk-print-capabilities
  (:export t
   :type-initializer \"gtk_print_capabilities_get_type\")
  (:page-set         #.(ash 1 0))
  (:copies           #.(ash 1 1))
  (:collate          #.(ash 1 2))
  (:reverse          #.(ash 1 3))
  (:scale            #.(ash 1 4))
  (:generate-pdf     #.(ash 1 5))
  (:generate-ps      #.(ash 1 6))
  (:preview          #.(ash 1 7))
  (:number-up        #.(ash 1 8))
  (:number-up-layout #.(ash 1 9)))
  @end{pre}
  @begin[code]{table}
    @entry[:page-set]{Print dialog will offer printing even/odd pages.}
    @entry[:copies]{Print dialog will allow to print multiple copies.}
    @entry[:collate]{Print dialog will allow to collate multiple copies.}
    @entry[:reverse]{Print dialog will allow to print pages in reverse order.}
    @entry[:scale]{Print dialog will allow to scale the output.}
    @entry[:generate-pdf]{The program will send the document to the printer in
      PDF format.}
    @entry[:generate-ps]{The program will send the document to the printer in
      Postscript format.}
    @entry[:preview]{Print dialog will offer a preview.}
    @entry[:number-up]{Print dialog will offer printing multiple pages per
      sheet. Since 2.12.}
    @entry[:up-layout]{Print dialog will allow to rearrange pages when printing
      multiple pages per sheet. Since 2.14.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_set_manual_capabilities ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-set-manual-capabilities))

(defun gtk-print-unix-dialog-set-manual-capabilities (dialog capabilities)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @argument[capabilities]{the printing capabilities of your application}
  @begin{short}
    This lets you specify the printing capabilities your application supports.
    For instance, if you can handle scaling the output then you pass
    @code{:scale}. If you do not pass that, then the dialog will only let you
    select the scale if the printing system automatically handles scaling.
  @end{short}

  Since 2.10"
  (setf (gtk-print-unix-dialog-manual-capabilities dialog) capabilities))

(export 'gtk-print-unix-dialog-set-manual-capabilities)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_manual_capabilities ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-print-unix-dialog-get-manual-capabilities))

(defun gtk-print-unix-dialog-get-manual-capabilities (dialog)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-30}
  @argument[dialog]{a @class{gtk-print-unix-dialog} widget}
  @return{The printing capabilities.}
  @begin{short}
    Gets the value of the @code{\"manual-capabilities\"} property.
  @end{short}

  Since 2.18"
  (gtk-print-unix-dialog-manual-capabilities dialog))

(export 'gtk-print-unix-dialog-get-manual-capabilities)

;;; --- End of file gtk.print-unix-dialog.lisp ---------------------------------
