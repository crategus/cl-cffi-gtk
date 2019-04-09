;;; ----------------------------------------------------------------------------
;;; gtk.application.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2019 Dieter Kaiser
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
;;; GtkApplication
;;;
;;;     Application class
;;;
;;; Types and Values
;;;
;;;     GtkApplication
;;;     GtkApplicationInhibitFlags
;;;
;;; Functions
;;;
;;;     gtk_application_new
;;;     gtk_application_add_window
;;;     gtk_application_remove_window
;;;     gtk_application_get_windows
;;;     gtk_application_get_window_by_id
;;;     gtk_application_get_active_window                  Accessor
;;;     gtk_application_inhibit
;;;     gtk_application_uninhibit
;;;     gtk_application_is_inhibited
;;;     gtk_application_prefers_app_menu
;;;     gtk_application_get_app_menu                       Accessor
;;;     gtk_application_set_app_menu                       Accessor
;;;     gtk_application_get_menubar                        Accessor
;;;     gtk_application_set_menubar                        Accessor
;;;     gtk_application_get_menu_by_id
;;;     gtk_application_add_accelerator
;;;     gtk_application_remove_accelerator
;;;     gtk_application_list_action_descriptions
;;;     gtk_application_get_accels_for_action
;;;     gtk_application_set_accels_for_action
;;;     gtk_application_get_actions_for_accel
;;;
;;; Properties
;;;
;;;      GtkWindow*  active-window         Read
;;;     GMenuModel*  app-menu              Read / Write
;;;     GMenuModel*  menubar               Read / Write
;;;       gboolean   register-session      Read / Write
;;;       gboolean   screensaver-active    Read
;;;
;;; Signals
;;;
;;;           void   window-added          Run First
;;;           void   window-removed        Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GApplication
;;;         ╰── GtkApplication
;;;
;;; Implemented Interfaces
;;;
;;;     GtkApplication implements GActionGroup and GActionMap.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkApplication
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkApplication" gtk-application
  (:superclass g-application
   :export t
   :interfaces ("GActionGroup"
                "GActionMap")
   :type-initializer "gtk_application_get_type")
   (#+gtk-3-6
    (active-window
     gtk-application-active-window
     "active-window" "GtkWindow" t nil)
    (app-menu
     gtk-application-app-menu
     "app-menu" "GMenuModel" t t)
    (menubar
     gtk-application-menubar
     "menubar" "GMenuModel" t t)
    (register-session
     gtk-application-register-session
     "register-session" "gboolean" t t)
   #+gtk-3-24
   (screensaver-active
    gtk-application-screensaver-active
    "screensaver-active" "gboolean" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-application 'type)
 "@version{2013-8-8}
  @begin{short}
    @sym{gtk-application} is a class that handles many important aspects of a
    GTK+ application in a convenient fashion, without enforcing a
    one-size-fits-all application model.
  @end{short}

  Currently, @sym{gtk-application} handles GTK+ initialization, application
  uniqueness, session management, provides some basic scriptability and desktop
  shell integration by exporting actions and menus and manages a list of
  toplevel windows whose life-cycle is automatically tied to the life-cycle of
 your application.

  While @sym{gtk-application} works fine with plain @class{gtk-window} widgets,
  it is recommended to use it together with @class{gtk-application-window}.

  When GDK threads are enabled, @sym{gtk-application} will acquire the GDK lock
  when invoking actions that arrive from other processes. The GDK lock is not
  touched for local action invocations. In order to have actions invoked in a
  predictable context it is therefore recommended that the GDK lock be held
  while invoking actions locally with the @fun{g-action-group-activate-action}
  function. The same applies to actions associated with
  @class{gtk-application-window} and to the \"activate\" and \"open\"
  @class{g-application} methods.

  To set an application menu for a @sym{gtk-application}, use the
  @fun{gtk-application-app-menu} slot access function. The @class{g-menu-model}
  object that this function expects is usually constructed using
  @class{gtk-builder}, as seen in the following example. To specify a menubar
  that will be shown by @class{gtk-application-window} widgets, use the
  @fun{gtk-application-menubar} slot access function. Use the base
  @class{g-action-map} interface to add actions, to respond to the user
  selecting these menu items.

  GTK+ displays these menus as expected, depending on the platform the
  application is running on.

  @b{Figure Menu integration in OS X.}

  @image[bloatpad-osx]{}

  @b{Figure Menu integration in GNOME.}

  @image[bloatpad-gnome]{}

  @b{Figure Menu integration in Xfce.}

  @image[bloatpad-xfce]{}

  @sym{gtk-application} optionally registers with a session manager of the
  users session, if you set the @code{register-session} property, and offers
  various functionality related to the session life-cycle.

  An application can block various ways to end the session with the
  @fun{gtk-application-inhibit} function. Typical use cases for this kind of
  inhibiting are long-running, uninterruptible operations, such as burning a
  CD or performing a disk backup. The session manager may not honor the
  inhibitor, but it can be expected to inform the user about the negative
  consequences of ending the session while inhibitors are present.

  @begin[Example]{dictionary}
    A simple application
    @begin{pre}
(defclass bloat-pad (gtk-application)
  ()
  (:metaclass gobject-class)
  (:g-type-name . \"BloatPad\"))t

(register-object-type-implementation \"BloatPad\"
                                     bloat-pad
                                     \"GtkApplication\"
                                     nil
                                     nil)

(defun new-window (application file)
  (declare (ignore file))
    (let (;; Create the application window
          (window (make-instance 'gtk-application-window
                                 :application application
                                 :title \"Bloatpad\"
                                 :border-width 12
                                 :default-width 500
                                 :default-height 400))
          (grid (make-instance 'gtk-grid))
          (toolbar (make-instance 'gtk-toolbar)))

      ;; Connect signal \"destroy\" to the application window
      (g-signal-connect window \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)
                          (if (zerop gtk::*main-thread-level*)
                              (g-application-quit application))))

      ;; Add action \"copy\" to the application window
      (let ((action (g-simple-action-new \"copy\" nil)))
        (g-action-map-add-action window action)
        (g-signal-connect action \"activate\"
           (lambda (action parameter)
             (declare (ignore action parameter))
             (let ((view (gobject::get-g-object-for-pointer
                           (g-object-get-data window \"bloatpad-text\"))))
               (gtk-text-buffer-copy-clipboard
                                  (gtk-text-view-buffer view)
                                  (gtk-widget-get-clipboard view
                                                            \"CLIPBOARD\"))))))

      ;; Add action \"paste\" to the application window
      (let ((action (g-simple-action-new \"paste\" nil)))
        (g-action-map-add-action window action)
        (g-signal-connect action \"activate\"
           (lambda (action parameter)
             (declare (ignore action parameter))
             (let ((view (gobject::get-g-object-for-pointer
                           (g-object-get-data window \"bloatpad-text\"))))
               (gtk-text-buffer-paste-clipboard
                                       (gtk-text-view-buffer view)
                                       (gtk-widget-get-clipboard view
                                                                 \"CLIPBOARD\")
                                       :default-editable t)))))

      ;; Add action \"fullscreen\" to the application window
      (let ((action (g-simple-action-new-stateful
                                               \"fullscreen\"
                                               nil
                                               (g-variant-new-boolean nil))))
        (g-action-map-add-action window action)
        (g-signal-connect action \"activate\"
           (lambda (action parameter)
             (declare (ignore parameter))
             (let* ((state (g-action-state action))
                    (value (g-variant-get-boolean state)))
               (g-action-change-state action
                                      (g-variant-new-boolean (not value))))))
        (g-signal-connect action \"change-state\"
           (lambda (action parameter)
             (if (g-variant-get-boolean parameter)
                 (gtk-window-fullscreen window)
                 (gtk-window-unfullscreen window))
             (setf (g-simple-action-state action) parameter))))

      ;; Add action \"justify\" to the application window
      (let ((action (g-simple-action-new-stateful
                                             \"justify\"
                                             (g-variant-type-new \"s\")
                                             (g-variant-new-string \"left\"))))
        (g-action-map-add-action window action)
        (g-signal-connect action \"activate\"
           (lambda (action parameter)
             (g-action-change-state action parameter)))
        (g-signal-connect action \"change-state\"
           (lambda (action parameter)
             (let ((view (gobject::get-g-object-for-pointer
                           (g-object-get-data window \"bloatpad-text\")))
                   (str (g-variant-get-string parameter)))
               (cond ((equal str \"left\")
                      (setf (gtk-text-view-justification view) :left))
                     ((equal str \"center\")
                      (setf (gtk-text-view-justification view) :center))
                     (t
                      (setf (gtk-text-view-justification view) :right)))
               (g-simple-action-set-state action parameter)))))

      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :stock-id \"gtk-justify-left\")))
        (gtk-actionable-set-detailed-action-name button \"win.justify::left\")
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :stock-id \"gtk-justify-center\")))
        (gtk-actionable-set-detailed-action-name button \"win.justify::center\")
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :stock-id \"gtk-justify-right\")))
        (gtk-actionable-set-detailed-action-name button \"win.justify::right\")
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-separator-tool-item
                                   :draw nil)))
        (gtk-tool-item-set-expand button t)
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-tool-item))
            (box (make-instance 'gtk-box
                                :orientation :horizontal
                                :spacing 6))
            (label (make-instance 'gtk-label
                                  :label \"Fullscreen:\"))
            (switch (make-instance 'gtk-switch)))
        (setf (gtk-actionable-action-name switch) \"win.fullscreen\")
        (gtk-container-add box label)
        (gtk-container-add box switch)
        (gtk-container-add button box)
        (gtk-container-add toolbar button))
      (gtk-grid-attach grid toolbar 0 0 1 1)
      (let ((scrolled (make-instance 'gtk-scrolled-window
                                     :hexpand t
                                     :vexpand t))
            (view (make-instance 'gtk-text-view)))
        (g-object-set-data window \"bloatpad-text\" (pointer view))
        (gtk-container-add scrolled view)
        (gtk-grid-attach grid scrolled 0 1 1 1))
      (gtk-container-add window grid)
      (gtk-widget-show-all window)))

(defun bloat-pad-activate (application)
  ;; Start a main loop and create an application window
  (within-main-loop
    (new-window application nil))
  ;; Wait until the main loop has finished
  (join-gtk-main))

(defun create-about-dialog ()
  (let (;; Create an about dialog
        (dialog (make-instance 'gtk-about-dialog
                               :program-name \"Example Dialog\"
                               :version \"0.00\"
                               :copyright \"(c) Dieter Kaiser\"
                               :website
                               \"github.com/crategus/cl-cffi-gtk\"
                               :website-label \"Project web site\"
                               :license \"LLGPL\"
                               :authors '(\"Dieter Kaiser\")
                               :documenters '(\"Dieter Kaiser\")
                               :artists '(\"None\")
                               :logo-icon-name
                               \"applications-development\"
                               :wrap-license t)))
    ;; Run the about dialog
    (gtk-dialog-run dialog)
    ;; Destroy the about dialog
    (gtk-widget-destroy dialog)))

(defvar *menu*
  \"<interface>
    <menu id='app-menu'>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_New Window</attribute>
       <attribute name='action'>app.new</attribute>
       <attribute name='accel'>&lt;Primary&gt;n</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_About Bloatpad</attribute>
       <attribute name='action'>app.about</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_Quit</attribute>
       <attribute name='action'>app.quit</attribute>
       <attribute name='accel'>&lt;Primary&gt;q</attribute>
      </item>
     </section>
     </menu>
    <menu id='menubar'>
     <submenu>
      <attribute name='label' translatable='yes'>_Edit</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Copy</attribute>
        <attribute name='action'>win.copy</attribute>
        <attribute name='accel'>&lt;Primary&gt;c</attribute>
       </item>
       <item>
        <attribute name='label' translatable='yes'>_Paste</attribute>
        <attribute name='action'>win.paste</attribute>
        <attribute name='accel'>&lt;Primary&gt;v</attribute>
       </item>
      </section>
     </submenu>
     <submenu>
      <attribute name='label' translatable='yes'>_View</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Fullscreen</attribute>
        <attribute name='action'>win.fullscreen</attribute>
        <attribute name='accel'>F11</attribute>
       </item>
      </section>
     </submenu>
    </menu>
   </interface>\")

(defun bloat-pad-startup (application)
  ;; Add action \"new\" to the application
  (let ((action (g-simple-action-new \"new\" nil)))
    ;; Connect a handler to the signal \"activate\"
    (g-signal-connect action \"activate\"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; ensure-gtk-main increases the thread level for the new window
         (ensure-gtk-main)
         (new-window application nil)))
    ;; Add the action to the action map of the application
    (g-action-map-add-action application action))

  ;; Add action \"about\" to the application
  (let ((action (g-simple-action-new \"about\" nil)))
    ;; Connect a handler to the signal \"activate\"
    (g-signal-connect action \"activate\"
       (lambda (action parameter)
         (declare (ignore action parameter))
         (create-about-dialog)))
    ;; Add the action to the action map of the application
    (g-action-map-add-action application action))

  ;; Add action \"quit\" to the application
  (let ((action (g-simple-action-new \"quit\" nil)))
    ;; Connect a handler to the signal activate
    (g-signal-connect action \"activate\"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; Destroy all windows of the application
         (dolist (window (gtk-application-get-windows application))
           (gtk-widget-destroy window))
         ;; Quit the main loop
         (leave-gtk-main)
         ;; Quit the application
         (g-application-quit application)))
    ;; Add the action to action map of the application
    (g-action-map-add-action application action))

  ;; Intitialize the application menu and the menubar
  (let ((builder (make-instance 'gtk-builder)))
    ;; Read the menus from a string
    (gtk-builder-add-from-string builder *menu*)
    ;; Set the application menu
    (setf (gtk-application-app-menu application)
          (gtk-builder-get-object builder \"app-menu\"))
    ;; Set the menubar
    (setf (gtk-application-menubar application)
          (gtk-builder-get-object builder \"menubar\"))))

(defun bloat-pad-open (application)
  (declare (ignore application))
  ;; Executed when the application is opened
  nil)

(defun bloat-pad-shutdown (application)
  (declare (ignore application))
  ;; Executed when the application is shut down
  nil)

(defmethod initialize-instance :after
    ((app bloat-pad) &key &allow-other-keys)
  (g-signal-connect app \"activate\" #'bloat-pad-activate)
  (g-signal-connect app \"startup\" #'bloat-pad-startup)
  (g-signal-connect app \"open\" #'bloat-pad-open)
  (g-signal-connect app \"shutdown\" #'bloat-pad-shutdown))

(defun bloat-pad-new ()
  (g-set-application-name \"Bloatpad\")
  (setf (gtk-settings-gtk-shell-shows-app-menu (gtk-settings-get-default))
        t)
  (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-get-default))
        t)
  (make-instance 'bloat-pad
                 :application-id \"org.gtk.Test.bloatpad\"
                 :flags :handles-open
                 :inactivity-timeout 30000
                 :register-session t))

(defun example-application (&optional (argc 0) (argv (null-pointer)))
  (let (;; Create an instance of the application Bloat Pad
        (bloat-pad (bloat-pad-new)))
    ;; Run the application
    (g-application-run bloat-pad argc argv)
    ;; Destroy the application
    (g-object-unref (pointer bloat-pad))))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"window-added\" signal}
      @begin{pre}
 lambda (application window)    : Run First
      @end{pre}
      Emitted when a @class{gtk-window} widget is added to @arg{application}
      through the @fun{gtk-application-add-window} function.
      @begin[code]{table}
        @entry[application]{The @sym{gtk-application} object which emitted the
          signal.}
        @entry[window]{The newly added @class{gtk-window} widget.}
      @end{table}
    @subheading{The \"window-removed\" signal}
      @begin{pre}
 lambda (application window)    : Run First
      @end{pre}
      Emitted when a @class{gtk-window} widget is removed from
      @arg{application}, either as a side-effect of being destroyed or
      explicitly through the @fun{gtk-application-remove-window} function.
      @begin[code]{table}
        @entry[application]{The @sym{gtk-application} object which emitted the
          signal.}
        @entry[window]{The @class{gtk-window} widget that is being removed.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-application-active-window}
  @see-slot{gtk-application-app-menu}
  @see-slot{gtk-application-menubar}
  @see-slot{gtk-application-register-session}
  @see-slot{gtk-application-screensaver-active}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-application-active-window ------------------------------------------

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "active-window"
                                               'gtk-application) 't)
 "The @code{active-window} property of type @class{gtk-window} (Read) @br{}
  The window which most recently had focus. @br{}
  Since 3.6")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-application-active-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-active-window 'function)
 "@version{2014-7-19}
  @argument[object]{a @class{gtk-application} object}
  @begin{short}
    Accessor of the slot @slot[gtk-application]{active-window} of the
    @class{gtk-application} class.
  @end{short}

  The @sym{gtk-application-active-window} slot access function
  gets the \"active\" window for the application.

  The active window is the one that was most recently focused within the
  application. This window may not have the focus at the moment if another
  application has it - this is just the most recently-focused window within
  this application.

  Since 3.6
  @see-class{gtk-application}")

;;; --- gtk-application-app-menu -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "app-menu" 'gtk-application) 't)
 "The @code{app-menu} property of type @class{g-menu-model}
  (Read / Write) @br{}
  The @class{g-menu-model} object for the application menu.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-app-menu atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-app-menu 'function)
 "@version{2014-10-18}
  @argument[object]{a @class{gtk-application} object}
  @argument[app-menu]{a @class{g-menu-model}, or @code{nil}}
  @syntax[]{(gtk-application-app-menu object) => app-menu}
  @syntax[]{(setf (gtk-application-app-menu object) app-menu)}
  @begin{short}
    Accessor of the slot @slot[gtk-application]{app-menu} of the
    @class{gtk-application} class.
  @end{short}

  The @sym{gtk-application-app-menu} slot access function
  returns the menu model that has been set.

  The @sym{(setf gtk-application-app-menu)} slot access function
  sets the application menu for the application.

  This can only be done in the primary instance of the application, after it
  has been registered. The handler for the \"startup\" signal is a good place
  to call this.

  The application menu is a single menu containing items that typically impact
  the application as a whole, rather than acting on a specific window or
  document. For example, you would expect to see \"Preferences\" or \"Quit\" in
  an application menu, but not \"Save\" or \"Print\".

  If supported, the application menu will be rendered by the desktop
  environment.

  Use the base @class{g-action-map} interface to add actions, to respond to the
  user selecting these menu items.
  @see-class{gtk-application}
  @see-class{g-menu-model}
  @see-class{g-action-map}")

;;; --- gtk-application-menubar ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "menubar" 'gtk-application) 't)
 "The @code{menubar} property of type @class{g-menu-model}
  (Read / Write) @br{}
  The @class{g-menu-model} object for the menubar.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-menubar atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-menubar 'function)
 "@version{2014-4-20}
  @argument[object]{a @class{gtk-application} object}
  @argument[menubar]{a @class{g-menu-model}, or @code{nil}}
  @syntax[]{(gtk-application-menubar object) => menubar}
  @syntax[]{(setf (gtk-application-menubar object) menubar)}
  @begin{short}
    Accessor of the slot @slot[gtk-application]{menubar} of the
    @class{gtk-application} class.
  @end{short}

  The generic function @sym{gtk-application-menubar} returns the menu model
  that has been set with the generic function
  @sym{(setf gtk-application-menubar)}.

  The generic function @sym{(setf gtk-application-menubar)} sets or unsets the
  menubar for windows of the application.

  This is a menubar in the traditional sense.

  This can only be done in the primary instance of the application, after it
  has been registered. \"startup\" is a good place to call this.

  Depending on the desktop environment, this may appear at the top of each
  window, or at the top of the screen. In some environments, if both the
  application menu and the menubar are set, the application menu will be
  presented as if it were the first item of the menubar. Other environments
  treat the two as completely separate - for example, the application menu
  may be rendered by the desktop shell while the menubar, if set, remains in
  each individual window.

  Use the base @class{g-action-map} interface to add actions, to respond to the
  user selecting these menu items.
  @see-class{gtk-application}
  @see-class{g-menu-model}
  @see-class{g-action-map}")

;;; --- gtk-application-register-session ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "register-session"
                                               'gtk-application) 't)
 "The @code{register-session} property of type @code{:boolean}
  (Read / Write) @br{}
  Set this property to @em{true} to register with the session manager. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-register-session atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-register-session 'function)
 "@version{2014-4-20}
  Accessor of the slot @slot[gtk-application]{register-session} of the
  @class{gtk-application} class.
  @see-class{gtk-application}")

;;; gtk-application-screensaver-active -----------------------------------------

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "screensaver-active"
                                               'gtk-application) 't)
 "The @code{screensaver-active} property of type @code{:boolean} (Read) @br{}
  This property is @em{true} if GTK+ believes that the screensaver is currently
  active. GTK+ only tracks session state (including this) when
  @code{register-session} is set to @em{true}.
  Tracking the screensaver state is supported on Linux. @br{}
  Default value: @code{nil} @br{}
  Since 3.24")

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-application-screensaver-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-screensaver-active 'function)
 "@version{2019-4-9}
  @begin{short}
    Accessor of the slot @slot[gtk-application]{screensaver-active} of the
    @class{gtk-application} class.
  @end{short}

  Since 3.24
  @see-class{gtk-application}")

;;; ----------------------------------------------------------------------------
;;; gtk_application_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-new))

(defun gtk-application-new (application-id flags)
 "@version{2013-7-25}
  @argument[application-id]{the application ID of type @code{:string}}
  @argument[flags]{the application flags of type @symbol{g-application-flags}}
  @return{A new @class{gtk-application} object.}
  @begin{short}
    Creates a new @class{gtk-application} object.
  @end{short}

  The @code{gtk_init()} function is called as soon as the application gets
  registered as the primary instance.

  Concretely, the @code{gtk_init()} function is called in the default handler
  for the \"startup\" signal. Therefore, @class{gtk-application} subclasses
  should chain up in their \"startup\" handler before using any GTK+ API.

  Note that commandline arguments are not passed to the function
  @code{gtk_init()}. All GTK+ functionality that is available via commandline
  arguments can also be achieved by setting suitable environment variables such
  as @code{G_DEBUG}, so this should not be a big problem. If you absolutely must
  support GTK+ commandline arguments, you can explicitly call the function
  @code{gtk_init} before creating the application instance.

  The application ID must be valid. See the  @fun{g-application-id-is-valid}
  function.
  @see-class{gtk-application}
  @see-symbol{g-application-flags}
  @see-function{g-application-id-is-valid}"
  (make-instance 'gtk-application
                 :application-id application-id
                 :flags flags))

(export 'gtk-application-new)

;;; ----------------------------------------------------------------------------
;;; gtk_application_add_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_add_window" gtk-application-add-window) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-8}
  @argument[application]{a @class{gtk-application} object}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Adds a window to the application.
  @end{short}
  This call is equivalent to setting the @slot[gtk-window]{application}
  property of @arg{window} to @arg{application}.

  Normally, the connection between the application and the window will remain
  until the window is destroyed, but you can explicitly remove it with the
  @fun{gtk-application-remove-window} function.

  GTK+ will keep the application running as long as it has any windows.
  @see-class{gtk-application}
  @see-class{gtk-window}
  @see-function{gtk-application-remove-window}"
  (application (g-object gtk-application))
  (window (g-object gtk-window)))

(export 'gtk-application-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_remove_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_remove_window" gtk-application-remove-window) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-18}
  @argument[application]{a @class{gtk-application} object}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Remove a window from the application.
  @end{short}
  If @arg{window} belongs to @arg{application} then this call is equivalent to
  setting the @slot[gtk-window]{application} property of window to @code{nil}.

  The application may stop running as a result of a call to this function.
  @see-class{gtk-application}
  @see-class{gtk-window}
  @see-function{gtk-application-add-window}"
  (application (g-object gtk-application))
  (window (g-object gtk-window)))

(export 'gtk-application-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_windows ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_get_windows" gtk-application-get-windows)
    (g-list (g-object gtk-window))
 #+cl-cffi-gtk-documentation
 "@version{2014-10-18}
  @argument[application]{a @class{gtk-application} object}
  @return{A list of @class{gtk-window} widgets.}
  @begin{short}
    Gets a list of the windows associated with the application.
  @end{short}
  The list is sorted by most recently focused window, such that the first
  element is the currently focused window. This is useful for choosing a parent
  for a transient window.

  The list that is returned should not be modified in any way. It will only
  remain valid until the next focus change or window creation or deletion.
  @see-class{gtk-application}
  @see-class{gtk-window}
  @see-function{gtk-application-active-window}
  @see-function{gtk-application-get-window-by-id}"
  (application (g-object gtk-application)))

(export 'gtk-application-get-windows)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_window_by_id ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(defcfun ("gtk_application_get_window_by_id" gtk-application-get-window-by-id)
    (g-object gtk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-8}
  @argument[application]{a @class{gtk-application} object}
  @argument[id]{an identifier number of type @code{:uint}}
  @return{The window with ID @arg{id}, or @code{nil} if there is no window with
    this ID.}
  @begin{short}
    Returns the @class{gtk-application-window} widget with the given ID.
  @end{short}

  Since 3.6
  @see-class{gtk-application}
  @see-class{gtk-application-window}
  @see-function{gtk-application-active-window}
  @see-function{gtk-application-get-windows}"
  (application (g-object gtk-application))
  (id :uint))

#+gtk-3-6
(export 'gtk-application-get-window-by-id)

;;; ----------------------------------------------------------------------------
;;; enum GtkApplicationInhibitFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkApplicationInhibitFlags" gtk-application-inhibit-flags
  (:export t
   :type-initializer "gtk_application_inhibit_flags_get_type")
  (:logout  #.(ash 1 0))
  (:switch  #.(ash 1 1))
  (:suspend #.(ash 1 2))
  (:idle    #.(ash 1 3)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-inhibit-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-application-inhibit-flags atdoc:*external-symbols*)
 "@version{2013-8-8}
  @begin{short}
    Types of user actions that may be blocked by the
    @fun{gtk-application-inhibit} function.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkApplicationInhibitFlags\" gtk-application-inhibit-flags
  (:export t
   :type-initializer \"gtk_application_inhibit_flags_get_type\")
  (:logout  #.(ash 1 0))
  (:switch  #.(ash 1 1))
  (:suspend #.(ash 1 2))
  (:idle    #.(ash 1 3)))
  @end{pre}
  @begin[code]{table}
    @entry[:logout]{Inhibit ending the user session by logging out or by
      shutting down the computer.}
    @entry[:switch]{Inhibit user switching.}
    @entry[:suspend]{Inhibit suspending the session or computer.}
    @entry[:idle]{Inhibit the session being marked as idle and possibly locked.}
  @end{table}
  @see-function{gtk-application-inhibit}")

;;; ----------------------------------------------------------------------------
;;; gtk_application_inhibit ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_inhibit" gtk-application-inhibit) :uint
 #+cl-cffi-gtk-documentation
 "@version{2014-10-18}
  @argument[application]{the @class{gtk-application} object}
  @argument[window]{a @class{gtk-window} widget, or @code{nil}}
  @argument[flags]{what @symbol{gtk-application-inhibit-flags} types of user
    actions should be inhibited}
  @argument[reason]{a short, human-readable string that explains why these
    operations are inhibited}
  @begin{return}
    A non-zero cookie of type @code{:uint} that is used to uniquely identify
    this request. It should be used as an argument to the
    @fun{gtk-application-uninhibit} function in order to remove the request. If
    the platform does not support inhibiting or the request failed for some
    reason, 0 is returned.
  @end{return}
  @begin{short}
    Inform the session manager that certain types of actions should be
    inhibited. This is not guaranteed to work on all platforms and for all types
    of actions.
  @end{short}

  Applications should invoke this method when they begin an operation that
  should not be interrupted, such as creating a CD or DVD. The types of
  actions that may be blocked are specified by the flags parameter. When the
  application completes the operation it should call the
  @fun{gtk-application-uninhibit} function to remove the inhibitor. Note that an
  application can have multiple inhibitors, and all of the must be individually
  removed. Inhibitors are also cleared when the application exits.

  Applications should not expect that they will always be able to block the
  action. In most cases, users will be given the option to force the action to
  take place.

  Reasons should be short and to the point.

  If @arg{window} is given, the session manager may point the user to this
  window to find out more about why the action is inhibited.
  @see-class{gtk-application}
  @see-class{gtk-window}
  @see-symbol{gtk-application-inhibit-flags}
  @see-function{gtk-application-uninhibit}"
  (application (g-object gtk-application))
  (window (g-object gtk-window))
  (flags gtk-application-inhibit-flags)
  (reason :string))

(export 'gtk-application-inhibit)

;;; ----------------------------------------------------------------------------
;;; gtk_application_uninhibit ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_uninhibit" gtk-application-uninhibit) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-8}
  @argument[application]{the @class{gtk-application} object}
  @argument[cookie]{a cookie of type @code{:uint} that was returned by the
    @fun{gtk-application-inhibit} function}
  @begin{short}
    Removes an inhibitor that has been established with the
    @fun{gtk-application-inhibit} function. Inhibitors are also cleared when the
    application exits.
  @end{short}
  @see-class{gtk-application}
  @see-function{gtk-application-inhibit}"
  (application (g-object gtk-application))
  (cookie :uint))

(export 'gtk-application-uninhibit)

;;; ----------------------------------------------------------------------------
;;; gtk_application_is_inhibited ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_is_inhibited" gtk-application-is-inhibited) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-8}
  @argument[application]{the @class{gtk-application} object}
  @argument[flags]{what @symbol{gtk-application-inhibit} types of actions should
    be queried}
  @return{@em{True} if any of the actions specified in @arg{flags} are
    inhibited.}
  @begin{short}
    Determines if any of the actions specified in @arg{flags} are currently
    inhibited, possibly by another application.
  @end{short}
  @see-class{gtk-application}
  @see-function{gtk-application-inhibit}"
  (application (g-object gtk-application))
  (flags gtk-application-inhibit-flags))

(export 'gtk-application-is-inhibited)

;;; ----------------------------------------------------------------------------
;;; gtk_application_prefers_app_menu ()
;;; ----------------------------------------------------------------------------

#+gtk-3-14
(defcfun ("gtk_application_prefers_app_menu" gtk-application-prefers-app-menu)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2019-4-9}
  @argument[application]{a @class{gtk-application} object}
  @return{a @code{:boolean} that is @em{True} if you should set an app menu}
  @begin{short}
    Determines if the desktop environment in which the application is running
    would prefer an application menu be shown.
  @end{short}

  If this function returns @em{true} then the application should call the
  @fun{gtk-application-app-menu} slot access function with the contents of an
  application menu, which will be shown by the desktop environment. If it
  returns @code{nil} then you should consider using an alternate approach, such
  as a menubar.

  The value returned by this function is purely advisory and you are free to
  ignore it. If you call the @fun{gtk-application-app-menu} slot access function
  even if the desktop environment doesn't support app menus, then a fallback
  will be provided.

  Applications are similarly free not to set an app menu even if the desktop
  environment wants to show one. In that case, a fallback will also be created
  by the desktop environment. GNOME, for example, uses a menu with only a
  \"Quit\" item in it.

  The value returned by this function never changes. Once it returns a
  particular value, it is guaranteed to always return the same value.

  You may only call this function after the application has been registered
  and after the base startup handler has run. You're most likely to want to
  use this from your own startup handler. It may also make sense to consult
  this function while constructing UI, in activate, open or an action
  activation handler, in order to determine if you should show a gear menu
  or not.

  This function will return @code{nil} on Mac OS and a default app menu will be
  created automatically with the \"usual\" contents of that menu typical to most
  Mac OS applications. If you call the @fun{gtk-application-app-menu} slot
  access function anyway, then this menu will be replaced with your own.
  @see-class{gtk-application}
  @see-function{gtk-application-app-menu}"
  (application (g-object gtk-application)))

#+gtk-3-14
(export 'gtk-application-prefers-app-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_menu_by_id ()
;;; ----------------------------------------------------------------------------

#+gkt-3-14
(defcfun ("gtk_application_get_menu_by_id" gtk-application-get-menu-by-id)
    (g-object g-menu)
 #+cl-cffi-gtk-documentation
 "@version{2019-4-9}
  @argument[application]{a @class{gtk-application} object}
  @argument[id]{a @code{:string} with the ID of the menu to look up}
  @return{Gets the menu with the given @arg{id} from the automatically loaded
    resources.}
  @begin{short}
    Gets a menu from automatically loaded resources.
  @end{short}
  See Automatic resources for more information.

  Since 3.14
  @see-class{gtk-application}
  @see-function{gtk-application-add-accelerator}"
  (application (g-object gtk-application))
  (id :string))

#+3-14
(export 'gtk-application-get-menu-by-id)

;;; ----------------------------------------------------------------------------
;;; gtk_application_add_accelerator ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_add_accelerator" gtk-application-add-accelerator)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-8}
  @argument[application]{a @class{gtk-application} object}
  @argument[accelerator]{accelerator string}
  @argument[action-name]{a string with the name of the action to activate}
  @argument[parameter]{a @type{g-variant} parameter to pass when activating
    the action, or @code{nil} if the action does not accept an activation
    parameter}
  @begin{short}
    Installs an accelerator that will cause the named action to be activated
    when the key combination specificed by accelerator is pressed.
  @end{short}

  @arg{accelerator} must be a string that can be parsed by the function
  @fun{gtk-accelerator-parse}, e. g. \"<Primary>q\" or \"<Control><Alt>p\".

  @arg{action-name} must be the name of an action as it would be used in the app
  menu, i. e. actions that have been added to the application are referred to
  with an \"app.\" prefix, and window-specific actions with a \"win.\" prefix.

  @class{gtk-application} also extracts accelerators out of 'accel' attributes
  in the @class{g-menu-model}s passed to the functions
  @fun{gtk-application-app-menu} and @fun{gtk-application-menubar},
  which is usually more convenient than calling this function for each
  accelerator.
  @begin[Warning]{dictionary}
    The @sym{gtk-application-add-accelerator} function has been deprecated since
    version 3.14 and should not be used in newly-written code. Use the
    @fun{gtk-application-set-accels-for-action} function instead.
  @end{dictionary}
  @see-class{gtk-application}
  @see-function{gtk-accelerator-parse}
  @see-function{gtk-application-app-menu}
  @see-function{gtk-application-menubar}
  @see-function{gtk-application-remove-accelerator}"
  (application (g-object gtk-application))
  (accelerator :string)
  (action-name :string)
  (parameter (:pointer (:struct g-variant))))

(export 'gtk-application-add-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_application_remove_accelerator ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_remove_accelerator"
           gtk-application-remove-accelerator) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-8}
  @argument[application]{a @class{gtk-application} object}
  @argument[action-name]{a string with the name of the action to activate}
  @argument[parameter]{a @type{g-variant} parameter to pass when activating the
    action, or @code{nil} if the action does not accept an activation parameter}
  @begin{short}
    Removes an accelerator that has been previously added with the function
    @fun{gtk-application-add-accelerator}.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-application-remove-accelerator} function has been deprecated
    since version 3.14 and should not be used in newly-written code. Use the
    @fun{gtk-application-set-accels-for-action} function instead.
  @end{dictionary}
  @see-class{gtk-application}
  @see-function{gtk-application-add-accelerator}"
  (application (g-object gtk-application))
  (action-name :string)
  (parameter (:pointer (:struct g-variant))))

(export 'gtk-application-remove-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_application_list_action_descriptions ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun ("gtk_application_list_action_desriptions"
           gtk-application-list-action-descriptions) g-strv
 #+cl-cffi-gtk-documentation
 "@version{2013-8-8}
  @argument[application]{a @class{gtk-application} object}
  @return{A list of strings.}
  @begin{short}
    Lists the detailed action names which have associated accelerators.
  @end{short}
  See the @fun{gtk-application-set-accels-for-action} function.

  Since 3.12
  @see-class{gtk-application}"
 (application (g-object gtk-application)))

#+gtk-3-12
(export 'gtk-application-list-action-descriptions)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_accels_for_action ()
;;;
;;; gchar ** gtk_application_get_accels_for_action (GtkApplication *application,
;;;                                          const gchar *detailed_action_name);
;;;
;;; Gets the accelerators that are currently associated with the given action.
;;;
;;; application
;;;     a GtkApplication
;;;
;;; detailed_action_name
;;;     a detailed action name, specifying an action and target to obtain
;;;     accelerators for
;;;
;;; Returns
;;;     accelerators for detailed_action_name , as a NULL-terminated array.
;;;     Free with g_strfreev() when no longer needed.
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_application_set_accels_for_action ()
;;;
;;; void gtk_application_set_accels_for_action (GtkApplication *application,
;;;                                           const gchar *detailed_action_name,
;;;                                                const gchar * const *accels);
;;;
;;; Sets zero or more keyboard accelerators that will trigger the given action.
;;; The first item in accels will be the primary accelerator, which may be
;;; displayed in the UI.
;;;
;;; To remove all accelerators for an action, use an empty, zero-terminated
;;; array for accels .
;;;
;;; For the detailed_action_name , see g_action_parse_detailed_name() and
;;; g_action_print_detailed_name().
;;;
;;; application
;;;     a GtkApplication
;;;
;;; detailed_action_name
;;;     a detailed action name, specifying an action and target to associate
;;;     accelerators with
;;;
;;; accels
;;;     a list of accelerators in the format understood by
;;;     gtk_accelerator_parse().
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_actions_for_accel ()
;;;
;;; gchar ** gtk_application_get_actions_for_accel (GtkApplication *application,
;;;                                                 const gchar *accel);
;;;
;;; Returns the list of actions (possibly empty) that accel maps to. Each item
;;; in the list is a detailed action name in the usual form.
;;;
;;; This might be useful to discover if an accel already exists in order to
;;; prevent installation of a conflicting accelerator (from an accelerator
;;; editor or a plugin system, for example). Note that having more than one
;;; action per accelerator may not be a bad thing and might make sense in cases
;;; where the actions never appear in the same context.
;;;
;;; In case there are no actions for a given accelerator, an empty array is
;;; returned. NULL is never returned.
;;;
;;; It is a programmer error to pass an invalid accelerator string. If you are
;;; unsure, check it with gtk_accelerator_parse() first.
;;;
;;; application
;;;     a GtkApplication
;;;
;;; accel
;;;     an accelerator that can be parsed by gtk_accelerator_parse()
;;;
;;; Returns
;;;     a NULL-terminated array of actions for accel .
;;;
;;; Since: 3.14
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.application.lisp ---------------------------------------
