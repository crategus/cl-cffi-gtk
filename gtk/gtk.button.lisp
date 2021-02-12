;;; ----------------------------------------------------------------------------
;;; gtk.button.lisp
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
;;; GtkButton
;;;
;;;     A widget that emits a signal when clicked on.
;;;
;;; Types and Values
;;;
;;;     GtkButton
;;;
;;; Functions
;;;
;;;     gtk_button_new
;;;     gtk_button_new_with_label
;;;     gtk_button_new_with_mnemonic
;;;     gtk_button_new_from_icon_name ()
;;;     gtk_button_new_from_stock
;;;     gtk_button_pressed                               * deprecated
;;;     gtk_button_released                              * deprecated
;;;     gtk_button_clicked
;;;     gtk_button_enter                                 * deprecated
;;;     gtk_button_leave                                 * deprecated
;;;     gtk_button_set_relief                              Accessor
;;;     gtk_button_get_relief                              Accessor
;;;     gtk_button_get_label                               Accessor
;;;     gtk_button_set_label                               Accessor
;;;     gtk_button_get_use_stock                           Accessor
;;;     gtk_button_set_use_stock                           Accessor
;;;     gtk_button_get_use_underline                       Accessor
;;;     gtk_button_set_use_underline                       Accessor
;;;     gtk_button_set_focus_on_click                      Accessor
;;;     gtk_button_get_focus_on_click                      Accessor
;;;     gtk_button_set_alignment                         * deprecated
;;;     gtk_button_get_alignment                         * deprecated
;;;     gtk_button_set_image                               Accessor
;;;     gtk_button_get_image                               Accessor
;;;     gtk_button_set_image_position                      Accessor
;;;     gtk_button_get_image_position                      Accessor
;;;     gtk_button_set_always_show_image                   Accessor
;;;     gtk_button_get_always_show_image                   Accessor
;;;     gtk_button_get_event_window
;;;
;;; Properties
;;;
;;;            gboolean   always-show-image    Read / Write / Construct
;;;           GtkWidget*  image                Read / Write
;;;     GtkPositionType   image-position       Read / Write
;;;               gchar*  label                Read / Write / Construct
;;;      GtkReliefStyle   relief               Read / Write
;;;            gboolean   use-stock            Read / Write / Construct
;;;            gboolean   use-underline        Read / Write / Construct
;;;              gfloat   xalign               Read / Write
;;;              gfloat   yalign               Read / Write
;;;
;;; Style Properties
;;;
;;;          gint   child-displacement-x      Read
;;;          gint   child-displacement-y      Read
;;;     GtkBorder*  default-border            Read
;;;     GtkBorder*  default-outside-border    Read
;;;      gboolean   displace-focus            Read
;;;          gint   image-spacing             Read
;;;     GtkBorder*  inner-border              Read
;;;
;;; Signals
;;;
;;;     void  activate    Action
;;;     void  clicked     Action
;;;     void  enter       Run First
;;;     void  leave       Run First
;;;     void  pressed     Run First
;;;     void  released    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ├── GtkToggleButton
;;;                         ├── GtkColorButton
;;;                         ├── GtkFontButton
;;;                         ├── GtkLinkButton
;;;                         ├── GtkLockButton
;;;                         ├── GtkModelButton
;;;                         ╰── GtkScaleButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkButton implements AtkImplementorIface, GtkBuildable, GtkActionable
;;;     and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkButton
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkButton" 'gtk-button))

(define-g-object-class "GtkButton" gtk-button
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_button_get_type")
  ((always-show-image
    gtk-button-always-show-image
    "always-show-image" "gboolean" t t)
   (focus-on-click
    gtk-button-focus-on-click
    "focus-on-click" "gboolean" t t)
   (image
    gtk-button-image
    "image" "GtkWidget" t t)
   (image-position
    gtk-button-image-position
    "image-position" "GtkPositionType" t t)
   (label
    gtk-button-label
    "label" "gchararray" t t)
   (relief
    gtk-button-relief
    "relief" "GtkReliefStyle" t t)
   (use-stock
    gtk-button-use-stock
    "use-stock" "gboolean" t t)
   (use-underline
    gtk-button-use-underline
    "use-underline" "gboolean" t t)
   (xalign
    gtk-button-xalign
    "xalign" "gfloat" t t)
   (yalign
    gtk-button-yalign
    "yalign" "gfloat" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-button 'type)
 "@version{*2021-2-9}
  @short{A widget that emits a signal when clicked on.}

  @image[button]{}

  The @sym{gtk-button} widget is generally used to trigger a callback function
  that is called when the button is pressed. The various signals and how to use
  them are outlined below.

  The @sym{gtk-button} widget can hold any valid child widget. That is, it can
  hold almost any other standard @class{gtk-widget} object. The most commonly
  used child is the @class{gtk-label} widget.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-button} widget has a single CSS node with name @code{button}.
    The node will get the style classes @code{.image-button} or
    @code{.text-button}, if the content is just an image or label, respectively.
    It may also receive the @code{.flat} style class.

    Other style classes that are commonly used with the @sym{gtk-button} widget
    include @code{.suggested-action} and @code{.destructive-action}. In special
    cases, buttons can be made round by adding the @code{.circular} style class.

    Button-like widgets like @class{gtk-toggle-button}, @class{gtk-menu-button},
    @class{gtk-volume-button}, @class{gtk-lock-button},
    @class{gtk-color-button}, @class{gtk-font-button} or
    @class{gtk-file-chooser-button} use style classes such as @code{.toggle},
    @code{.popup}, @code{.scale}, @code{.lock}, @code{.color}, @code{.font},
    @code{.file} to differentiate themselves from a plain @sym{gtk-button}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[child-displacement-x]{entry}
        The @code{child-displacement-x} style property of type @code{:int}
        (Read) @br{}
        How far in the x direction to move the child when the button is
        depressed. @br{}
        @em{Warning:} The @code{child-displacement-x} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use CSS margins and padding instead. The value of this style
        property is ignored. @br{}
        Default value: 0
      @end{entry}
      @begin[child-displacement-y]{entry}
        The @code{child-displacement-y} style property of type @code{:int}
        (Read) @br{}
        How far in the y direction to move the child when the button is
        depressed. @br{}
        @em{Warning:} The @code{child-displacement-x} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use CSS margins and padding instead. The value of this style
        property is ignored. @br{}
        Default value: 0
      @end{entry}
      @begin[default-border]{entry}
        The @code{default-border} style property of type @class{gtk-border}
        (Read) @br{}
        The @code{default-border} style property defines the extra space to add
        around a button that can become the default widget of its window. For
        more information about default widgets, see the function
        @fun{gtk-widget-grab-default}. @br{}
        @em{Warning:} The @code{default-border} style property has been
        deprecated since version 3.14 and should not be used in newly-written
        code. Use CSS margins and padding instead. The value of this style
        property is ignored.
      @end{entry}
      @begin[default-outside-border]{entry}
        The @code{default-outside-border} style property of type
        @class{gtk-border} (Read) @br{}
        The @code{default-outside-border} style property defines the extra
        outside space to add around a button that can become the default widget
        of its window. Extra outside space is always drawn outside the button
        border. For more information about default widgets, see the function
        @fun{gtk-widget-grab-default}. @br{}
        @em{Warning:} The @code{default-outside-border} style property has been
        deprecated since version 3.14 and should not be used in newly-written
        code. Use CSS margins and padding instead. The value of this style
        property is ignored.
      @end{entry}
      @begin[displace-focus]{entry}
        The @code{displace-focus} style property of type @code{:boolean}
        (Read) @br{}
        Whether the @code{child-displacement-x} or @code{child-displacement-y}
        properties should also affect the focus rectangle. @br{}
        @em{Warning:} The @code{displace-focus} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use CSS margins and padding instead. The value of this style
        property is ignored. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[image-spacing]{entry}
        The @code{image-spacing} style property of type @code{:int} (Read) @br{}
        Spacing in pixels between the image and label. @br{}
        Allowed values: >= 0 @br{}
        Default value: 2
      @end{entry}
      @begin[inner-border]{entry}
        The @code{inner-border} style property of type @class{gtk-border}
        (Read) @br{}
        Sets the border between the button edges and child. @br{}
        @em{Warning:} The @code{inner-border} style property has been
        deprecated since version 3.4 and should not be used in newly written
        code. Use the standard border and padding CSS properties. The value
        of this style property is ignored. @br{}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (button)    : Action
      @end{pre}
      The \"activate\" signal on @sym{gtk-button} is an action signal and
      emitting it causes the button to animate press then release. Applications
      should never connect to this signal, but use the \"clicked\" signal.
      @begin[code]{table}
        @entry[button]{The @sym{gtk-button} widget which received the signal.}
      @end{table}
    @subheading{The \"clicked\" signal}
      @begin{pre}
 lambda (button)    : Action
      @end{pre}
      Emitted when the button has been activated (pressed and released).
      @begin[code]{table}
        @entry[button]{The @sym{gtk-button} widget which received the signal.}
      @end{table}
    @subheading{The \"enter\" signal}
      @begin{pre}
 lambda (button)    : Run First
      @end{pre}
      Emitted when the pointer enters the button. @br{}
      @em{Warning:} The \"enter\" signal has been deprecated since version 2.8
      and should not be used in newly written code. Use the
      \"enter-notify-event\" signal of the @class{gtk-widget} class.
      @begin[code]{table}
        @entry[button]{The @sym{gtk-button} widget which received the signal.}
      @end{table}
    @subheading{The \"leave\" signal}
      @begin{pre}
 lambda (button)    : Run First
      @end{pre}
      Emitted when the pointer leaves the button. @br{}
      @em{Warning:} The \"leave\" signal has been deprecated since version 2.8
      and should not be used in newly written code. Use the
      \"leave-notify-event\" signal of the @class{gtk-widget} class.
      @begin[code]{table}
        @entry[button]{The @sym{gtk-button} widget which received the signal.}
      @end{table}
    @subheading{The \"pressed\" signal}
      @begin{pre}
 lambda (button)    : Run First
      @end{pre}
      Emitted when the button is pressed. @br{}
      @em{Warning:} The \"pressed\" signal has been deprecated since version
      2.8 and should not be used in newly-written code. Use the
      \"button-press-event\" signal of the @class{gtk-widget} class.
      @begin[code]{table}
        @entry[button]{The @sym{gtk-button} widget which received the signal.}
      @end{table}
    @subheading{The \"released\" signal}
      @begin{pre}
 lambda (button)    : Run First
      @end{pre}
      Emitted when the button is released. @br{}
      @em{Warning:} The \"released\" signal has been deprecated since version
      2.8 and should not be used in newly written code. Use the
      \"button-release-event\" signal of the @class{gtk-widget} class.
      @begin[code]{table}
        @entry[button]{The @sym{gtk-button} widget which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-button-always-show-image}
  @see-slot{gtk-button-focus-on-click}
  @see-slot{gtk-button-image}
  @see-slot{gtk-button-image-position}
  @see-slot{gtk-button-label}
  @see-slot{gtk-button-relief}
  @see-slot{gtk-button-use-stock}
  @see-slot{gtk-button-use-underline}
  @see-slot{gtk-button-xalign}
  @see-slot{gtk-button-yalign}
  @see-class{gtk-widget}
  @see-class{gtk-label}
  @see-class{gtk-border}
  @see-symbol{gtk-position-type}
  @see-symbol{gtk-relief-style}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-button-always-show-image -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "always-show-image"
                                               'gtk-button) t)
 "The @code{always-show-image} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the button will ignore the
  @slot[gtk-settings]{gtk-button-images} setting and always show the image, if
  available. Use this property if the button would be useless or hard to use
  without the image. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-always-show-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-always-show-image 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-button-always-show-image object) => always-show}
  @syntax[]{(setf (gtk-button-always-show-image object) always-show)}
  @argument[object]{a @class{gtk-button} widget}
  @argument[always-show]{@em{true} if the button should always show the image}
  @begin{short}
    Accessor of the @slot[gtk-button]{always-show-image} slot of the
    @class{gtk-button} class.
  @end{short}

  The slot access function @sym{gtk-button-always-show-image} returns whether
  the button will ignore the @slot[gtk-settings]{gtk-button-images} setting
  and always show the image, if available. The slot access function
  @sym{(setf gtk-button-always-show-image)} sets the
  @slot[gtk-button]{always-show-image} property.

  Use this property if the button would be useless or hard to use without
  the image.
  @see-class{gtk-button}
  @see-class{gtk-settings}
  @see-function{gtk-settings-gtk-button-images}")

;;; --- gtk-button-focus-on-click ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-on-click" 'gtk-button) 't)
 "The @code{focus-on-click} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the button grabs focus when it is clicked with the mouse. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-focus-on-click atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-focus-on-click 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-button-focus-on-click object) => focus-on-click}
  @syntax[]{(setf (gtk-button-focus-on-click object) focus-on-click)}
  @argument[object]{a @class{gtk-button} widget}
  @argument[focus-on-click]{a boolean whether the button grabs focus when
    clicked with the mouse}
  @begin{short}
    Accessor of the @slot[gtk-button]{focus-on-click} slot of the
    @class{gtk-button} class.
  @end{short}

  The slot access function @sym{gtk-button-focus-on-click} returns whether
  the button grabs focus when it is clicked with the mouse. The slot access
  function @sym{(setf gtk-button-focus-on-click)} sets whether the button will
  grab focus when it is clicked with the mouse.

  Making mouse clicks not grab focus is useful in places like toolbars where you
  do not want the keyboard focus removed from the main area of the application.
  @begin[Warning]{dictionary}
    The function @sym{gtk-button-focus-on-click} has been deprecated since
    version 3.20 and should not be used in newly-written code. Use the function
    @fun{gtk-widget-focus-on-click} instead.
  @end{dictionary}
  @see-class{gtk-button}")

;;; --- gtk-button-image -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "image" 'gtk-button) 't)
 "The @code{image} property of type @class{gtk-widget} (Read / Write) @br{}
  The child widget to appear next to the button text.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-image atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-image 'function)
 "@version{2020-11-20}
  @syntax[]{(gtk-button-image object) => image}
  @syntax[]{(setf (gtk-button-image object) image)}
  @argument[object]{a @class{gtk-button} widget}
  @argument[image]{a @class{gtk-widget} to set as the image for the button}
  @begin{short}
    Accessor of the @slot[gtk-button]{image} slot of the @class{gtk-button}
    class.
  @end{short}

  The slot access function @sym{gtk-button-image} gets the widget that is
  currently set as the image of the button. This may have been explicitly set
  by the slot access function @sym{(setf gtk-button-image)} or constructed by
  the function @fun{gtk-button-new-from-icon-name}.

  Note that it depends on the @slot[gtk-button]{always-show-image} property
  whether the image will allways be displayed or not. You do not have to call
  the function @fun{gtk-widget-show} on the image yourself.
  @see-class{gtk-button}
  @see-function{gtk-widget-show}
  @see-function{gtk-button-new-from-icon-name}
  @see-function{gtk-button-always-show-image}")

;;; --- gtk-button-image-position ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "image-position" 'gtk-button) 't)
 "The @code{image-position} property of type @symbol{gtk-position-type}
  (Read / Write) @br{}
  The position of the image relative to the text inside the button. @br{}
  Default value: @code{:left}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-image-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-image-position 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-button-image-position object) => position}
  @syntax[]{(setf (gtk-button-image-position object) position)}
  @argument[object]{a @class{gtk-button} widget}
  @argument[position]{the position of type @symbol{gtk-position-type}}
  @begin{short}
    Accessor of the @slot[gtk-button]{image-position} slot of the
    @class{gtk-button} class.
  @end{short}

  The slot access function @sym{gtk-button-image-position} gets the position of
  the image relative to the text inside the button. The slot access function
  @sym{(setf gtk-button-image-position} sets the position of the image relative
  to the text inside the button.
  @see-class{gtk-button}")

;;; --- gtk-button-label -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-button) 't)
 "The @code{label} property of type @code{:string} (Read / Write / Construct)
  @br{}
  Text of the label widget inside the button, if the button contains a
  label widget. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-label atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-label 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-button-label object) => label}
  @syntax[]{(setf (gtk-button-label object) label)}
  @argument[object]{a @class{gtk-button} widget}
  @argument[label]{a string with the label widget inside the button}
  @begin{short}
    Accessor of the @slot[gtk-button]{label} slot of the @class{gtk-button}
    class.
  @end{short}

  The slot access function @sym{gtk-button-label} fetches the text from the
  label of the button. The slot access function @sym{(setf gtk-button-label)}
  sets the text of the label of the button.

  If the label text has not been set the return value will be @code{nil}. This
  will be the case if you create an empty button with the function
  @fun{gtk-button-new} to use as a container.

  This text is also used to select the stock item if the slot access function
  @fun{gtk-button-use-stock} is used.
  @see-class{gtk-button}
  @see-function{gtk-button-new}
  @see-function{gtk-button-use-stock}")

;;; --- gtk-button-relief ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "relief" 'gtk-button) 't)
 "The @code{relief} property of type @symbol{gtk-relief-style}
  (Read / Write) @br{}
  The border relief style. @br{}
  Default value: @code{:normal}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-relief atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-relief 'function)
 "@version{2020-5-10}
  @syntax[]{(gtk-button-relief object) => relief-style}
  @syntax[]{(setf (gtk-button-relief object) relief-style)}
  @argument[object]{the @class{gtk-button} widget you want to set relief
    styles of}
  @argument[relief-style]{a value of the @symbol{gtk-relief-style} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-button]{relief} slot of the @class{gtk-button}
    class.
  @end{short}

  The slot access function @sym{gtk-button-relief} returns the current relief
  style of the given @class{gtk-button} widget. The slot access function
  @sym{(setf gtk-button-relief} sets the relief style of the edges of the given
  @class{gtk-button} widget.

  Three styles exist, @code{:normal}, @code{:half}, @code{:none}. The default
  style is @code{:normal}.
  @see-class{gtk-button}
  @see-symbol{gtk-relief-style}")

;;; --- gtk-button-use-stock ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-stock" 'gtk-button) 't)
 "The @code{use-stock} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If set, the label is used to pick a stock item instead of being
  displayed. @br{}
  @em{Warning:} The @code{use-stock} property has been deprecated since
  version 3.10 and should not be used in newly-written code. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-use-stock atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-use-stock 'function)
 "@version{2020-5-10}
  @syntax[]{(gtk-button-use-stock object) => use-stock}
  @syntax[]{(setf (gtk-button-use-stock object) use-stock)}
  @argument[object]{a @class{gtk-button} widget}
  @argument[use-stock]{@em{true} if the button should use a stock item}
  @begin{short}
    Accessor of the @slot[gtk-button]{use-stock} slot of the @class{gtk-button}
    class.
  @end{short}

  The slot access function @sym{gtk-button-use-stock} returns whether the
  button label is a stock item.

  If @em{true}, the label set on the button is used as a stock ID to
  select the stock item for the button.
  @begin[Warning]{dictionary}
    The function @sym{gtk-button-use-stock} has been deprecated since version
    3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-button}")

;;; --- gtk-button-use-underline -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-underline" 'gtk-button) 't)
 "The @code{use-underline} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-use-underline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-button-use-underline 'function)
 "@version{2020-5-10}
  @syntax[]{(gtk-button-use-underline object) => use-underline}
  @syntax[]{(setf (gtk-button-use-underline object) use-underline)}
  @argument[object]{a @class{gtk-button} widget}
  @argument[use-underline]{@em{true} if underlines in the text indicate
    mnemonics}
  @begin{short}
    Accessor of the @slot[gtk-button]{use-underline} slot of the
    @class{gtk-button} class.
  @end{short}

  The slot access function @sym{gtk-button-use-underline} returns whether an
  embedded underline in the button label indicates a mnemonic.

  If @em{true}, an underline in the text of the button label indicates the
  next character should be used for the mnemonic accelerator key.
  @see-class{gtk-button}")

;;; --- gtk-button-xalign ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-button) 't)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  If the child of the button is a @class{gtk-misc} or @class{gtk-alignment}
  widget, this property can be used to control its horizontal alignment. The
  value 0.0 is left aligned, 1.0 is right aligned. @br{}
  @em{Warning:} The @code{xalign} property has been deprecated since version
  3.14 and should not be used in newly-written code. Access the child widget
  directly if you need to control its alignment. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-xalign atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-xalign 'function)
 "@version{2020-11-19}
  @syntax[]{(gtk-button-xalign object) => xalign}
  @syntax[]{(setf (gtk-button-xalign object) xalign)}
  @argument[object]{a @class{gtk-button} widget}
  @argument[xalign]{a float for the horizontal alignment}
  @begin{short}
    Accessor of the @slot[gtk-button]{xalign} slot of the @class{gtk-button}
    class.
  @end{short}

  If the child of the button is a @class{gtk-misc} or @class{gtk-alignment}
  widget, this property can be used to control its horizontal alignment. The
  value 0.0 is left aligned, 1.0 is right aligned.
  @begin[Warning]{dictionary}
    The function @sym{gtk-button-xalign} property has been deprecated since
    version 3.14 and should not be used in newly-written code. Access the child
    widget directly if you need to control its alignment.
  @end{dictionary}
  @see-class{gtk-button}
  @see-function{gtk-button-alignment}")

;;; --- gtk-button-yalign ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-button) 't)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  If the child of the button is a @class{gtk-misc} or @class{gtk-alignment}
  widget, this property can be used to control its vertical alignment. The
  value 0.0 is top aligned, 1.0 is bottom aligned. @br{}
  @em{Warning:} The @code{yalign} property has been deprecated since version
  3.14 and should not be used in newly-written code. Access the child widget
  directly if you need to control its alignment. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-yalign atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-yalign 'function)
 "@version{2020-11-19}
  @syntax[]{(gtk-button-xalign object) => yalign}
  @syntax[]{(setf (gtk-button-xalign object) yalign)}
  @argument[object]{a @class{gtk-button} widget}
  @argument[xalign]{a float for the vertical alignment}
  @begin{short}
    Accessor of the @slot[gtk-button]{yalign} slot of the @class{gtk-button}
    class.
  @end{short}

  If the child of the button is a @class{gtk-misc} or @class{gtk-alignment}
  widget, this property can be used to control its vertical alignment. The
  value 0.0 is top aligned, 1.0 is bottom aligned.
  @begin[Warning]{dictionary}
    The @code{yalign} property has been deprecated since version 3.14 and
    should not be used in newly-written code. Access the child widget directly
    if you need to control its alignment.
  @end{dictionary}
  @see-class{gtk-button}
  @see-function{gtk-button-alignment}")

;;; ----------------------------------------------------------------------------
;;; gtk_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-new))

(defun gtk-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-11-20}
  @return{The newly created @class{gtk-button} widget.}
  @begin{short}
    Creates a new button widget.
  @end{short}
  To add a child widget to the button, use the function @fun{gtk-container-add}.
  @see-class{gtk-button}
  @see-function{gtk-button-new-with-label}
  @see-function{gtk-button-new-with-mnemonic}
  @see-function{gtk-button-new-from-icon-name}
  @see-function{gtk-container-add}"
  (make-instance 'gtk-button))

(export 'gtk-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_with_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-new-with-label))

(defun gtk-button-new-with-label (label)
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-9}
  @argument[label]{a string with the text you want the @class{gtk-label} child
    widget to hold}
  @return{The newly created @class{gtk-button} widget.}
  @begin{short}
    Creates a button widget with a label child widget containing the given text
    in @arg{label}.
  @end{short}
  @see-class{gtk-button}
  @see-class{gtk-label}
  @see-function{gtk-button-new}
  @see-function{gtk-button-new-with-mnemonic}
  @see-function{gtk-button-new-from-icon-name}"
  (make-instance 'gtk-button
                 :label label))

(export 'gtk-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-new-with-mnemonic))

(defun gtk-button-new-with-mnemonic (label)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-20}
  @argument[label]{a string with the text of the button, with an underscore in
    front of the mnemonic character}
  @return{A new @class{gtk-button} widget.}
  @begin{short}
    Creates a new button widget containing a label with a mnemonic.
  @end{short}
  If characters in label are preceded by an underscore, they are underlined.
  If you need a literal underscore character in a label, use '__' (two
  underscores). The first underlined character represents a keyboard accelerator
  called a mnemonic. Pressing Alt and that key activates the button.
  @see-class{gtk-button}
  @see-function{gtk-button-new}
  @see-function{gtk-button-new-with-label}
  @see-function{gtk-button-new-from-icon-name}"
  (make-instance 'gtk-button
                 :label label
                 :use-underline t))

(export 'gtk-button-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_from_icon_name ()
;;; ----------------------------------------------------------------------------

;; TODO: Consider to implement this function with make-instance

(defcfun ("gtk_button_new_from_icon_name" gtk-button-new-from-icon-name)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-10}
  @argument[button]{a @class{gtk-button} widget}
  @argument[icon-name]{a string with the icon name}
  @argument[icon-size]{an icon size of type @symbol{gtk-icon-size}}
  @return{A new @class{gtk-button} widget displaying the themed icon.}
  @begin{short}
    Creates a new button containing an icon from the current icon theme.
  @end{short}

  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.

  This function is a convenience wrapper around @fun{gtk-button-new} and
  @fun{gtk-button-image}.
  @see-class{gtk-button}
  @see-symbol{gtk-icon-size}
  @see-function{gtk-button-new}
  @see-function{gtk-button-image}"
  (icon-name g-string)
  (size gtk-icon-size))

(export 'gtk-button-new-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defun gtk-button-new-from-stock (stock-id)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-10}
  @argument[stock-id]{a string with the name of the stock item}
  @return{A new @class{gtk-button} widget.}
  @begin{short}
    Creates a new button widget containing the image and text from a stock item.
  @end{short}

  If @arg{stock-id} is unknown, then it will be treated as a mnemonic label
  as for the function @fun{gtk-button-new-with-mnemonic}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-button-new-from-stock} has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-button-new-with-label} instead.
  @end{dictionary}
  @see-class{gtk-button}
  @see-function{gtk-button-new}
  @see-function{gtk-button-new-with-label}
  @see-function{gtk-button-new-with-mnemonic}"
  (make-instance 'gtk-button
                 :label stock-id
                 :use-underline t
                 :use-stock t))

(export 'gtk-button-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_button_pressed ()
;;;
;;; void gtk_button_pressed (GtkButton *button);
;;;
;;; Warning
;;;
;;; gtk_button_pressed has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Use the "button-press-event" signal.
;;;
;;; Emits a "pressed" signal to the given GtkButton.
;;;
;;; button :
;;;     The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; * deprecated *

;;; ----------------------------------------------------------------------------
;;; gtk_button_released ()
;;;
;;; void gtk_button_released (GtkButton *button);
;;;
;;; Warning
;;;
;;; gtk_button_released has been deprecated since version 2.20 and should not
;;; be used in newly-written code. Use the "button-release-event" signal.
;;;
;;; Emits a "released" signal to the given GtkButton.
;;;
;;; button :
;;;     The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; * deprecated *

;;; ----------------------------------------------------------------------------
;;; gtk_button_clicked ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_button_clicked" gtk-button-clicked) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @argument[button]{the @class{gtk-button} widget you want to send the signal
    to}
  @begin{short}
    Emits a \"clicked\" signal to the given @class{gtk-button} widget.
  @end{short}
  @see-class{gtk-button}"
  (button (g-object gtk-button)))

(export 'gtk-button-clicked)

;;; ----------------------------------------------------------------------------
;;; gtk_button_enter ()
;;;
;;; void gtk_button_enter (GtkButton *button);
;;;
;;; Warning
;;;
;;; gtk_button_enter has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Use the "enter-notify-event" signal.
;;;
;;; Emits a "enter" signal to the given GtkButton.
;;;
;;; button :
;;;     The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; * deprecated *

;;; ----------------------------------------------------------------------------
;;; gtk_button_leave ()
;;;
;;; void gtk_button_leave (GtkButton *button);
;;;
;;; Warning
;;;
;;; gtk_button_leave has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Use the "leave-notify-event" signal.
;;;
;;; Emits a "leave" signal to the given GtkButton.
;;;
;;; button :
;;;     The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; * deprecated *

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_alignment ()
;;; gtk_button_set_alignment () -> gtk-button-alignment
;;; ----------------------------------------------------------------------------

(defun (setf gtk-button-alignment) (alignment button)
  (setf (gtk-button-xalign button) (first alignment)
        (gtk-button-yalign button) (second alignment))
  alignment)

(defun gtk-button-alignment (button)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @syntax[]{(gtk-button-alignment button) => (list xalign yalign)}
  @syntax[]{(setf (gtk-button-alignment button) (list xalign yalign))}
  @argument[button]{a @class{gtk-button} widget}
  @argument[xalign]{a float with the horizontal position of the child, the
    value 0.0 is left aligned, 1.0 is right aligned}
  @argument[yalign]{a float with the vertical position of the child, the value
    0.0 is top aligned, 1.0 is bottom aligned}
  @begin{short}
    Accessor of the alignment of the button.
  @end{short}

  The function @sym{gtk-button-alignment} gets the alignment of the child in
  the button. The function @sym{(setf gtk-button-alignment)} sets the alignment
  of the child.

  This property has no effect unless the child is a @class{gtk-misc} or a
  @class{gtk-alignment}.
  @begin[Warning:]{dictionary}
    The function @sym{gtk-button-alignment} has been deprecated since version
    3.14 and should not be used in newly-written code. Access the child widget
    directly if you need to control its alignment.
  @end{dictionary}
  @see-class{gtk-button}"
  (list (gtk-button-xalign button)
        (gtk-button-yalign button)))

(export 'gtk-button-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_event_window () -> gtk-button-event-window
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_button_get_event_window" gtk-button-event-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @argument[button]{a @class{gtk-button} widget}
  @return{The @class{gdk-window} event window of the button.}
  @begin{short}
    Returns the event window of the button if it is realized, @code{nil}
    otherwise.
  @end{short}
  This function should be rarely needed.
  @see-class{gtk-button}
  @see-class{gdk-window}"
  (button (g-object gtk-button)))

(export 'gtk-button-event-window)

;;; --- End of file gtk.button.lisp --------------------------------------------
