;;; ----------------------------------------------------------------------------
;;; gtk.app-chooser-widget.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkAppChooserWidget
;;;
;;; Application chooser widget that can be embedded in other widgets
;;;
;;; Synopsis
;;;
;;;     GtkAppChooserWidget
;;;
;;;     gtk_app_chooser_widget_new
;;;     gtk_app_chooser_widget_set_show_default
;;;     gtk_app_chooser_widget_get_show_default
;;;     gtk_app_chooser_widget_set_show_recommended
;;;     gtk_app_chooser_widget_get_show_recommended
;;;     gtk_app_chooser_widget_set_show_fallback
;;;     gtk_app_chooser_widget_get_show_fallback
;;;     gtk_app_chooser_widget_set_show_other
;;;     gtk_app_chooser_widget_get_show_other
;;;     gtk_app_chooser_widget_set_show_all
;;;     gtk_app_chooser_widget_get_show_all
;;;     gtk_app_chooser_widget_set_default_text
;;;     gtk_app_chooser_widget_get_default_text
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkAppChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;; GtkAppChooserWidget implements AtkImplementorIface, GtkBuildable,
;;; GtkOrientable and GtkAppChooser.
;;;
;;; Properties
;;;
;;;   "default-text"             gchar*               : Read / Write
;;;   "show-all"                 gboolean             : Read / Write / Construct
;;;   "show-default"             gboolean             : Read / Write / Construct
;;;   "show-fallback"            gboolean             : Read / Write / Construct
;;;   "show-other"               gboolean             : Read / Write / Construct
;;;   "show-recommended"         gboolean             : Read / Write / Construct
;;;
;;; Signals
;;;
;;;   "application-activated"                         : Run First
;;;   "application-selected"                          : Run First
;;;   "populate-popup"                                : Run First
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAppChooserWidget
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAppChooserWidget" gtk-app-chooser-widget
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkAppChooser")
   :type-initializer "gtk_app_chooser_widget_get_type")
  ((default-text
    gtk-app-chooser-widget-default-text
    "default-text" "gchararray" t t)
   (show-all
    gtk-app-chooser-widget-show-all
    "show-all" "gboolean" t t)
   (show-default
    gtk-app-chooser-widget-show-default
    "show-default" "gboolean" t t)
   (show-fallback
    gtk-app-chooser-widget-show-fallback
    "show-fallback" "gboolean" t t)
   (show-other
    gtk-app-chooser-widget-show-other
    "show-other" "gboolean" t t)
   (show-recommended
    gtk-app-chooser-widget-show-recommended
    "show-recommended" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-app-chooser-widget 'type)
 "@version{2013-11-1}
  @begin{short}
    @sym{gtk-app-chooser-widget} is a widget for selecting applications. It is
    the main building block for @class{gtk-app-chooser-dialog}. Most
    applications only need to use the latter; but you can use this widget as
    part of a larger widget if you have special needs.
  @end{short}

  @sym{gtk-app-chooser-widget} offers detailed control over what applications
  are shown, using the @code{\"show-default\"}, @code{\"show-recommended\"},
  @code{\"show-fallback\"}, @code{\"show-other\"} and @code{\"show-all\"}
  properties. See the @class{gtk-app-chooser} documentation for more information
  about these groups of applications.

  To keep track of the selected application, use the \"application-selected\"
  and \"application-activated\" signals.
  @begin[Signal Details]{dictionary}
    @subheading{The \"application-activated\" signal}
      @begin{pre}
 lambda (self application)   : Run First
      @end{pre}
      Emitted when an application item is activated from the widget's list.

      This usually happens when the user double clicks an item, or an item is
      selected and the user presses one of the keys Space, Shift+Space, Return
      or Enter.
      @begin[code]{table}
        @entry[self]{The object which received the signal.}
        @entry[application]{The activated @class{g-app-info} object.}
      @end{table}
    @subheading{The \"application-selected\" signal}
      @begin{pre}
 lambda (self application)   : Run First
      @end{pre}
      Emitted when an application item is selected from the widget's list.
      @begin[code]{table}
        @entry[self]{The object which received the signal.}
        @entry[application]{The selected @class{g-app-info} object.}
      @end{table}
    @subheading{The \"populate-popup\" signal}
      @begin{pre}
 lambda (self menu application)    : Run First
      @end{pre}
      Emitted when a context menu is about to popup over an application item.
      Clients can insert menu items into the provided @class{gtk-menu} object in
      the callback of this signal; the context menu will be shown over the item
      if at least one item has been added to the menu.
      @begin[code]{table}
        @entry[self]{The object which received the signal.}
        @entry[menu]{The @class{gtk-menu} to populate.}
        @entry[application]{The current @class{g-app-info}.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-app-chooser-widget-default-text}
  @see-slot{gtk-app-chooser-widget-show-all}
  @see-slot{gtk-app-chooser-widget-show-default}
  @see-slot{gtk-app-chooser-widget-show-fallback}
  @see-slot{gtk-app-chooser-widget-show-other}
  @see-slot{gtk-app-chooser-widget-show-recommended}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "default-text"
                                               'gtk-app-chooser-widget) 't)
 "The @code{\"default-text\"} property of type @code{:string}
  (Read / Write) @br{}
  The @code{\"default-text\"} property determines the text that appears in the
  widget when there are no applications for the given content type. See also
  the function @fun{gtk-app-chooser-widget-set-default-text}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-all"
                                               'gtk-app-chooser-widget) 't)
 "The @code{\"show-all\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If the @code{\"show-all\"} property is @em{true}, the app chooser presents all
  applications in a single list, without subsections for default, recommended
  or related applications. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-default"
                                               'gtk-app-chooser-widget) 't)
 "The @code{\"show-default\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{\"show-default\"} property determines whether the app chooser should
  show the default handler for the content type in a separate section. If
  @code{nil}, the default handler is listed among the recommended
  applications. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-fallback"
                                               'gtk-app-chooser-widget) 't)
 "The @code{\"show-fallback\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{\"show-fallback\"} property determines whether the app chooser
  should show a section for fallback applications. If @code{nil}, the fallback
  applications are listed among the other applications. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-other"
                                               'gtk-app-chooser-widget) 't)
 "The @code{\"show-other\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{\"show-other\"} property determines whether the app chooser should
  show a section for other applications. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-recommended"
                                               'gtk-app-chooser-widget) 't)
 "The @code{\"show-recommended\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{\"show-recommended\"} property determines whether the app chooser
  should show a section for recommended applications. If @code{nil}, the
  recommended applications are listed among the other applications. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-default-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-default-text 'function)
 "@version{2013-11-1}
  Accessor of the slot @code{\"default-text\"} of the
  @class{gtk-app-chooser-widget} class.
  @see-class{gtk-app-chooser-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-show-all atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-show-all 'function)
 "@version{2013-11-1}
  Accessor of the slot @code{\"show-all\"} of the
  @class{gtk-app-chooser-widget} class.
  @see-class{gtk-app-chooser-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-show-default atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-show-default 'function)
 "@version{2013-11-1}
  Accessor of the slot @code{\"show-default\"} of the
  @class{gtk-app-chooser-widget} class.
  @see-class{gtk-app-chooser-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-show-fallback
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-show-fallback 'function)
 "@version{2013-11-1}
  Accessor of the slot @code{\"show-fallback\"} of the
  @class{gtk-app-chooser-widget} class.
  @see-class{gtk-app-chooser-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-show-other atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-show-other 'function)
 "@version{2013-11-1}
  Accessor of the slot @code{\"show-other\"} of the
  @class{gtk-app-chooser-widget} class.
  @see-class{gtk-app-chooser-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-show-recommended
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-show-recommended 'function)
 "@version{2013-11-1}
  Accessor of the slot @code{\"show-recommended\"} of the
  @class{gtk-app-chooser-widget} class.
  @see-class{gtk-app-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-new))

(defun gtk-app-chooser-widget-new (content-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[content-type]{the content type to show applications for}
  @return{A newly created @class{gtk-app-chooser-widget} widget.}
  @begin{short}
    Creates a new @class{gtk-app-chooser-widget} for applications that can
    handle content of the given type.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}"
  (make-instance 'gtk-app-chooser-widget
                 :content-type content-type))

(export 'gtk-app-chooser-widget-new)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_set_show_default ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-set-show-default))

(defun gtk-app-chooser-widget-set-show-default (self setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @argument[setting]{the new value for the @code{\"show-default\"} property}
  @begin{short}
    Sets whether the app chooser should show the default handler for the content
    type in a separate section.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-get-show-default}"
  (setf (gtk-app-chooser-widget-show-default self) setting))

(export 'gtk-app-chooser-widget-set-show-default)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_get_show_default ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-get-show-default))

(defun gtk-app-chooser-widget-get-show-default (self)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @begin{return}
    The value of the @code{\"show-default\"} property.
  @end{return}
  @begin{short}
    Returns the current value of the @code{\"show-default\"} property.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-set-show-default}"
  (gtk-app-chooser-widget-show-default self))

(export 'gtk-app-chooser-widget-get-show-default)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_set_show_recommended ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-set-show-recommended))

(defun gtk-app-chooser-widget-set-show-recommended (self setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @argument[setting]{the new value for the @code{\"show-recommended\"} property}
  @begin{short}
    Sets whether the app chooser should show recommended applications for the
    content type in a separate section.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-get-show-recommended}"
  (setf (gtk-app-chooser-widget-show-recommended self) setting))

(export 'gtk-app-chooser-widget-set-show-recommended)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_get_show_recommended ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-get-show-recommended))

(defun gtk-app-chooser-widget-get-show-recommended (self)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @begin{return}
    The value of the @code{\"show-recommended\"} property.
  @end{return}
  @begin{short}
    Returns the current value of the @code{\"show-recommended\"} property.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-set-show-recommended}"
  (gtk-app-chooser-widget-show-recommended self))

(export 'gtk-app-chooser-widget-get-show-recommended)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_set_show_fallback ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-set-show-fallback))

(defun gtk-app-chooser-widget-set-show-fallback (self setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @argument[setting]{the new value for the @code{\"show-fallback\"} property}
  @begin{short}
    Sets whether the app chooser should show related applications for the
    content type in a separate section.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-get-show-fallback}"
  (setf (gtk-app-chooser-widget-show-fallback self) setting))

(export 'gtk-app-chooser-widget-set-show-fallback)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_get_show_fallback ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-get-show-fallback))

(defun gtk-app-chooser-widget-get-show-fallback (self)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @return{The value of the @code{\"show-fallback\"} property.}
  @begin{short}
    Returns the current value of the @code{\"show-fallback\"} property.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-set-show-fallback}"
  (gtk-app-chooser-widget-show-fallback self))

(export 'gtk-app-chooser-widget-get-show-fallback)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_set_show_other ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-set-show-other))

(defun gtk-app-chooser-widget-set-show-other (self setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @argument[setting]{the new value for the @code{\"show-other\"} property}
  @begin{short}
    Sets whether the app chooser should show applications which are unrelated to
    the content type.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-get-show-other}"
  (setf (gtk-app-chooser-widget-show-other self) setting))

(export 'gtk-app-chooser-widget-set-show-other)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_get_show_other ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-get-show-other))

(defun gtk-app-chooser-widget-get-show-other (self)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @return{The value of the @code{\"show-other\"} property.}
  @begin{short}
    Returns the current value of the @code{\"show-other\"} property.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-set-show-all}"
  (gtk-app-chooser-widget-show-other self))

(export 'gtk-app-chooser-widget-get-show-other)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_set_show_all ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-set-show-all))

(defun gtk-app-chooser-widget-set-show-all (self setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @argument[setting]{the new value for the @code{\"show-all\"} property}
  @begin{short}
    Sets whether the app chooser should show all applications in a flat list.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-get-show-all}"
  (setf (gtk-app-chooser-widget-show-all self) setting))

(export 'gtk-app-chooser-widget-set-show-all)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_get_show_all ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-get-show-all))

(defun gtk-app-chooser-widget-get-show-all (self)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @return{The value of the @code{\"show-all\"} property.}
  @begin{short}
    Returns the current value of the @code{\"show-all\"} property.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-set-show-all}"
  (gtk-app-chooser-widget-show-all self))

(export 'gtk-app-chooser-widget-get-show-all)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_set_default_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-set-default-text))

(defun gtk-app-chooser-widget-set-default-text (self text)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @argument[text]{the new value for the @code{\"default-text\"} property}
  @begin{short}
    Sets the text that is shown if there are not applications that can handle
    the content type.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-get-default-text}"
  (setf (gtk-app-chooser-widget-default-text self) text))

(export 'gtk-app-chooser-widget-set-default-text)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_get_default_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-get-default-text))

(defun gtk-app-chooser-widget-get-default-text (self)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @argument[self]{a @class{gtk-app-chooser-widget} widget}
  @return{The value of the @code{\"default-text\"} property.}
  @begin{short}
    Returns the text that is shown if there are not applications that can handle
    the content type.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-widget}
  @see-function{gtk-app-chooser-widget-set-default-text}"
  (gtk-app-chooser-widget-default-text self))

(export 'gtk-app-chooser-widget-get-default-text)

;;; --- End of file gtk.app-chooser-widget.lisp --------------------------------
