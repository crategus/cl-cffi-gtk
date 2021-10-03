;;; ----------------------------------------------------------------------------
;;; gtk.app-chooser-widget.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2020 Dieter Kaiser
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
;;;     Application chooser widget that can be embedded in other widgets
;;;
;;; Types and Values
;;;
;;;     GtkAppChooserWidget
;;;
;;; Functions
;;;
;;;     gtk_app_chooser_widget_new
;;;     gtk_app_chooser_widget_set_show_default            Accessor
;;;     gtk_app_chooser_widget_get_show_default            Accessor
;;;     gtk_app_chooser_widget_set_show_recommended        Accessor
;;;     gtk_app_chooser_widget_get_show_recommended        Accessor
;;;     gtk_app_chooser_widget_set_show_fallback           Accessor
;;;     gtk_app_chooser_widget_get_show_fallback           Accessor
;;;     gtk_app_chooser_widget_set_show_other              Accessor
;;;     gtk_app_chooser_widget_get_show_other              Accessor
;;;     gtk_app_chooser_widget_set_show_all                Accessor
;;;     gtk_app_chooser_widget_get_show_all                Accessor
;;;     gtk_app_chooser_widget_set_default_text            Accessor
;;;     gtk_app_chooser_widget_get_default_text            Accessor
;;;
;;; Properties
;;;
;;;        gchar*   default-text             Read / Write
;;;     gboolean    show-all                 Read / Write / Construct
;;;     gboolean    show-default             Read / Write / Construct
;;;     gboolean    show-fallback            Read / Write / Construct
;;;     gboolean    show-other               Read / Write / Construct
;;;     gboolean    show-recommended         Read / Write / Construct
;;;
;;; Signals
;;;
;;;         void    application-activated    Run First
;;;         void    application-selected     Run First
;;;         void    populate-popup           Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkAppChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAppChooserWidget implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable and GtkAppChooser.
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
 "@version{2020-5-21}
  @begin{short}
    @sym{gtk-app-chooser-widget} is a widget for selecting applications. It is
    the main building block for @class{gtk-app-chooser-dialog}. Most
    applications only need to use the latter. But you can use this widget as
    part of a larger widget if you have special needs.
  @end{short}

  @sym{gtk-app-chooser-widget} offers detailed control over what applications
  are shown, using the @code{show-default}, @code{show-recommended},
  @code{show-fallback}, @code{show-other} and @code{show-all} properties. See
  the @class{gtk-app-chooser} documentation for more information
  about these groups of applications.

  To keep track of the selected application, use the \"application-selected\"
  and \"application-activated\" signals.
  @begin[CSS nodes]{dictionary}
    @sym{gtk-app-chooser-widget} has a single CSS node with name
    @code{appchooser}.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"application-activated\" signal}
      @begin{pre}
 lambda (widget application)    :run-first
      @end{pre}
      Emitted when an application item is activated from the list of the widget.
      This usually happens when the user double clicks an item, or an item is
      selected and the user presses one of the @kbd{Space}, @kbd{Shift+Space},
      @kbd{Return} or @kbd{Enter} keys.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-app-chooser-widget} widget which received
          the signal.}
        @entry[application]{The activated @class{g-app-info} object.}
      @end{table}
    @subheading{The \"application-selected\" signal}
      @begin{pre}
 lambda (widget application)    : Run First
      @end{pre}
      Emitted when an application item is selected from the widget's list.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-app-chooser-widget} widget which received
          the signal.}
        @entry[application]{The selected @class{g-app-info} object.}
      @end{table}
    @subheading{The \"populate-popup\" signal}
      @begin{pre}
 lambda (self menu application)    : Run First
      @end{pre}
      Emitted when a context menu is about to popup over an application item.
      Clients can insert menu items into the provided @class{gtk-menu} object in
      the callback of this signal. The context menu will be shown over the item
      if at least one item has been added to the menu.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-app-chooser-widget} widget which received
          the signal.}
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
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-app-chooser-widget-default-text ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "default-text"
                                               'gtk-app-chooser-widget) 't)
 "The @code{default-text} property of type @code{g-string} (Read / Write) @br{}
  The @code{default-text} property determines the text that appears in the
  widget when there are no applications for the given content type. See also
  the function @fun{gtk-app-chooser-widget-default-text}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-default-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-default-text 'function)
 "@version{2020-5-21}
  @syntax[]{(gtk-app-chooser-widget-default-text object) => text}
  @syntax[]{(setf (gtk-app-chooser-widget-default-text object) text)}
  @argument[object]{a @class{gtk-app-chooser-widget} widget}
  @argument[text]{a string with the text that appears in the widget}
  @begin{short}
    Accessor of the @slot[gtk-app-chooser-widget]{default-text} slot of the
    @class{gtk-app-chooser-widget} class.
  @end{short}

  The slot access function @sym{gtk-app-chooser-widger-default-text} returns
  the text that is shown if there are not applications that can handle the
  content type. The slot access function
  @sym{(setf gtk-app-chooser-widget-default-text)} sets the text that is shown
  if there are not applications that can handle the content type.
  @see-class{gtk-app-chooser-widget}")

;;; --- gtk-app-chooser-widget-show-all ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-all"
                                               'gtk-app-chooser-widget) 't)
 "The @code{show-all} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If the @code{show-all} property is @em{true}, the application chooser
  presents all applications in a single list, without subsections for default,
  recommended or related applications. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-show-all atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-show-all 'function)
 "@version{2020-5-21}
  @syntax[]{(gtk-app-chooser-widget-show-all object) => setting}
  @syntax[]{(setf (gtk-app-chooser-widget-show-all object) setting)}
  @argument[object]{a @class{gtk-app-chooser-widget} widget}
  @argument[setting]{a boolean whether the application chooser presents all
    applications in a single list}
  @begin{short}
    Accessor of the @slot[gtk-app-chooser-widget]{show-all} slot of the
    @class{gtk-app-chooser-widget} class.
  @end{short}

  The slot access function @sym{gtk-app-chooser-widget-show-all} sets whether
  the application chooser should show all applications in a flat list.
  The slot access function @sym{(setf gtk-app-chooser-widget-show-all)} sets
  whether the app chooser should show all applications in a flat list.
  @see-class{gtk-app-chooser-widget}")

;;; --- gtk-app-chooser-widget-show-default ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-default"
                                               'gtk-app-chooser-widget) 't)
 "The @code{show-default} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{show-default} property determines whether the application chooser
  should show the default handler for the content type in a separate section.
  If @em{false}, the default handler is listed among the recommended
  applications. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-show-default atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-show-default 'function)
 "@version{2020-5-21}
  @syntax[]{(gtk-app-chooser-widget-show-default object) => setting}
  @syntax[]{(setf (gtk-app-chooser-widget-show-default object) setting)}
  @argument[object]{a @class{gtk-app-chooser-widget} widget}
  @argument[setting]{a boolean whether the application chooser should show the
    default handler}
  @begin{short}
    Accessor of the @slot[gtk-app-chooser-widget]{show-default} slot of the
    @class{gtk-app-chooser-widget} class.
  @end{short}

  The slot access function @sym{gtk-app-chooser-widget-show-default} returns
  whether the application chooser should the default handler for the content
  type in a separate section. The slott access function
  @sym{(setf gtk-app-chooser-widget-show-default)} sets whether the application
  chooser should show the default handler.
  @see-class{gtk-app-chooser-widget}")

;;; --- gtk-app-chooser-widget-show-fallback -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-fallback"
                                               'gtk-app-chooser-widget) 't)
 "The @code{show-fallback} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{show-fallback} property determines whether the application chooser
  should show a section for fallback applications. If @em{false}, the fallback
  applications are listed among the other applications. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-show-fallback
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-show-fallback 'function)
 "@version{2020-5-21}
  @syntax[]{(gtk-app-chooser-widget-show-fallback object) => setting}
  @syntax[]{(setf (gtk-app-chooser-widget-show-fallback object) setting)}
  @argument[object]{a @class{gtk-app-chooser-widget} widget}
  @argument[setting]{a boolean whether the application chooser should show a
    section for fallback applications}
  @begin{short}
    Accessor of the @slot[gtk-app-chooser-widget]{show-fallback} slot of the
    @class{gtk-app-chooser-widget} class.
  @end{short}

  The slot access function @sym{gtk-app-chooser-widget-show-fallback} returns
  whether the application chooser should show a section for fallback
  applications. The slot access function
  @sym{(setf gtk-app-chooser-widget-show-fallback)} sets whether the application
  chooser should show related applications for the content type in a separate
  section.
  @see-class{gtk-app-chooser-widget}")

;;; --- gtk-app-chooser-widget-show-other --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-other"
                                               'gtk-app-chooser-widget) 't)
 "The @code{show-other} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{show-other} property determines whether the application chooser
  should show a section for other applications. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-show-other atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-show-other 'function)
 "@version{2020-5-21}
  @syntax[]{(gtk-app-chooser-widget-show-other object) => setting}
  @syntax[]{(setf (gtk-app-chooser-widget-show-other object) setting)}
  @argument[object]{a @class{gtk-app-chooser-widget} widget}
  @argument[setting]{a boolean whether the application chooser should show a
    section for other applications}
  @begin{short}
    Accessor of the @slot[gtk-app-chooser-widget]{show-other} slot of the
    @class{gtk-app-chooser-widget} class.
  @end{short}

  The slot access function @sym{gtk-app-chooser-widget-show-other} returns
  whether the application chooser should show a section for other applications.
  The slot access function @sym{(gtk-app-chooser-widget-show-other)} sets
  whether the app chooser should show applications which are unrelated to
  the content type.
  @see-class{gtk-app-chooser-widget}")

;;; --- gtk-app-chooser-widget-show-recommended --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-recommended"
                                               'gtk-app-chooser-widget) 't)
 "The @code{show-recommended} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{show-recommended} property determines whether the application
  chooser should show a section for recommended applications. If @em{false},
  the recommended applications are listed among the other applications. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-widget-show-recommended
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-widget-show-recommended 'function)
 "@version{2020-5-21}
  @syntax[]{(gtk-app-chooser-widget-show-recommended object) => setting}
  @syntax[]{(setf (gtk-app-chooser-widget-show-recommended object) setting)}
  @argument[object]{a @class{gtk-app-chooser-widget} widget}
  @argument[setting]{a boolean whether the application chooser shuld show a
    section for recommended applications}
  @begin{short}
    Accessor of the @slot[gtk-app-chooser-widget]{show-recommended} slot of
    the @class{gtk-app-chooser-widget} class.
  @end{short}

  The slot access function @sym{gtk-app-chooser-widget-show-recommended}
  returns whether the application chooser should recommended applications. The
  slot access function @sym{(setf gtk-app-chooser-widget-show-recommended)}
  sets whether the app chooser should show recommended applications for the
  content type in a separate section.
  @see-class{gtk-app-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-widget-new))

(defun gtk-app-chooser-widget-new (content-type)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-21}
  @argument[content-type]{a string with the content type to show applications
    for}
  @return{A newly created @class{gtk-app-chooser-widget} widget.}
  @begin{short}
    Creates a new application chooser widget for applications that can
    handle content of the given type.
  @end{short}
  @see-class{gtk-app-chooser-widget}"
  (make-instance 'gtk-app-chooser-widget
                 :content-type content-type))

(export 'gtk-app-chooser-widget-new)

;;; --- End of file gtk.app-chooser-widget.lisp --------------------------------
