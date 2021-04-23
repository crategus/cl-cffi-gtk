;;; ----------------------------------------------------------------------------
;;; gtk.application.lisp
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
;;;      GtkWindow*   active-window         Read
;;;     GMenuModel*   app-menu              Read / Write
;;;     GMenuModel*   menubar               Read / Write
;;;       gboolean    register-session      Read / Write
;;;       gboolean    screensaver-active    Read
;;;
;;; Signals
;;;
;;;           void    query-end             Run First
;;;           void    window-added          Run First
;;;           void    window-removed        Run First
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
 "@version{*2020-5-11}
  @begin{short}
    Types of user actions that may be blocked by the function
    @fun{gtk-application-inhibit}.
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
  @see-class{gtk-application}
  @see-function{gtk-application-inhibit}")

;;; ----------------------------------------------------------------------------
;;; struct GtkApplication
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkApplication" gtk-application
  (:superclass g-application
   :export t
   :interfaces ("GActionGroup"
                "GActionMap")
   :type-initializer "gtk_application_get_type")
   ((active-window
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
    "screensaver-active" "gboolean" t nil)
   ))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-application 'type)
 "@version{*2020-5-13}
  @begin{short}
    @sym{gtk-application} is a class that handles many important aspects of a
    GTK+ application in a convenient fashion, without enforcing a
    one-size-fits-all application model.
  @end{short}

  Currently, @sym{gtk-application} handles GTK+ initialization, application
  uniqueness, session management, provides some basic scriptability and desktop
  shell integration by exporting actions and menus and manages a list of
  toplevel windows whose life-cycle is automatically tied to the life-cycle of
  the application.

  While @sym{gtk-application} works fine with plain @class{gtk-window} widgets,
  it is recommended to use it together with @class{gtk-application-window}
  widgets.

  When GDK threads are enabled, @sym{gtk-application} will acquire the GDK lock
  when invoking actions that arrive from other processes. The GDK lock is not
  touched for local action invocations. In order to have actions invoked in a
  predictable context it is therefore recommended that the GDK lock be held
  while invoking actions locally with the function
  @fun{g-action-group-activate-action}. The same applies to actions associated
  with @class{gtk-application-window} widgets and to the \"activate\" and
  \"open\" signals of the @class{g-application} class.

  @sym{gtk-application} will automatically load menus from the
  @class{gtk-builder} resource located at @file{\"gtk/menus.ui\"}, relative to
  the application's resource base path, see the function
  @fun{g-application-resource-base-path}. The menu with the ID \"app-menu\"
  is taken as the application's app menu and the menu with the ID \"menubar\"
  is taken as the application's menubar. Additional menus, most interesting
  submenus, can be named and accessed via the function
  @fun{gtk-application-menu-by-id} which allows for dynamic population of
  a part of the menu structure.

  If the resources @file{\"gtk/menus-appmenu.ui\"} or
  @file{\"gtk/menus-traditional.ui\"} are present then these files will be used
  in preference, depending on the value of the function
  @fun{gtk-application-prefers-app-menu}. If the resource
  @file{\"gtk/menus-common.ui\"} is present it will be loaded as well. This is
  useful for storing items that are referenced from both
  @file{\"gtk/menus-appmenu.ui\"} and @file{\"gtk/menus-traditional.ui\"}.

  It is also possible to provide the menus manually using the slot access
  functions @fun{gtk-application-app-menu} and @fun{gtk-application-menubar}.

  @sym{gtk-application} will also automatically setup an icon search path for
  the default icon theme by appending @file{\"icons\"} to the resource base
  path. This allows your application to easily store its icons as resources.
  See the function @fun{gtk-icon-theme-add-resource-path} for more information.

  If there is a resource located at @file{\"gtk/help-overlay.ui\"} which
  defines a @class{gtk-shortcuts-window} widget with ID @code{help_overlay} then
  @sym{gtk-application} associates an instance of this shortcuts window with
  each @class{gtk-application-window} widget and sets up keyboard accelerators
  (@kbd{Control-F1} and @kbd{Control-?}) to open it. To create a menu item that
  displays the shortcuts window, associate the item with the action
  @code{win.show-help-overlay}.

  @sym{gtk-application} optionally registers with a session manager of the
  users session, if you set the @code{register-session} property, and offers
  various functionality related to the session life-cycle.

  An application can block various ways to end the session with the function
  @fun{gtk-application-inhibit}. Typical use cases for this kind of inhibiting
  are long-running, uninterruptible operations, such as burning a CD or
  performing a disk backup. The session manager may not honor the inhibitor,
  but it can be expected to inform the user about the negative consequences of
  ending the session while inhibitors are present.
  @begin[Example]{dictionary}
    A simple application.
    @begin{pre}
(defun simple-application (&optional (argv nil))
  (within-main-loop
    (let (;; Create an application
          (app (make-instance 'gtk-application
                              :application-id \"com.crategus.simple-application\"
                              :flags :none)))

      ;; Connect signal \"activate\" to the applicaton
      (g-signal-connect app \"activate\"
          (lambda (application)
            ;; Create an application window
            (let ((window (make-instance 'gtk-application-window
                                         :application application
                                         :title \"Simple Application\"
                                         :default-width 500
                                         :default-height 300)))
              ;; Connect signal \"destroy\" to the application window
              (g-signal-connect window \"destroy\"
                                (lambda (widget)
                                  (declare (ignore widget))
                                  ;; Quit the application
                                  (g-application-quit app)))
              ;; Show the application window
              (gtk-widget-show-all window))))

      ;; Connect signal \"shutdown\" to the application
      (g-signal-connect app \"shutdown\"
          (lambda (application)
            (declare (ignore application))
            ;; Leave the main loop on shutdown
            (leave-gtk-main)))

      ;; Run the application
      (g-application-run app argv)))
      (join-gtk-main))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"query-end\" signal}
      @begin{pre}
 lambda (application)    : Run First
      @end{pre}
      Emitted when the session manager is about to end the session, only if the
      @code{register-session} property is @arg{true}. Applications can connect
      to this signal and call the function @fun{gtk-application-inhibit} with
      the value @code{:logout} of type @symbol{gtk-application-inhibit-flags} to
      delay the end of the session until the state has been saved. Since 3.24
      @begin[code]{table}
        @entry[application]{The @sym{gtk-application} object which emitted the
          signal.}
      @end{table}
    @subheading{The \"window-added\" signal}
      @begin{pre}
 lambda (application window)    : Run First
      @end{pre}
      Emitted when a @class{gtk-window} widget is added to the application
      through the function @fun{gtk-application-add-window}.
      @begin[code]{table}
        @entry[application]{The @sym{gtk-application} object which emitted the
          signal.}
        @entry[window]{The newly added @class{gtk-window} widget.}
      @end{table}
    @subheading{The \"window-removed\" signal}
      @begin{pre}
 lambda (application window)    : Run First
      @end{pre}
      Emitted when a @class{gtk-window} widget is removed from the application,
      either as a side-effect of being destroyed or explicitly through the
      function @fun{gtk-application-remove-window}.
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
  @see-slot{gtk-application-screensaver-active}
  @see-class{gtk-application-window}
  @see-class{gtk-builder}
  @see-class{gtk-shortcuts-window}
  @see-class{gtk-window}
  @see-class{g-application}
  @see-class{g-menu-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-application-active-window ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active-window"
                                               'gtk-application) 't)
 "The @code{active-window} property of type @class{gtk-window} (Read) @br{}
  The window which most recently had focus.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-active-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-active-window 'function)
 "@version{*2020-5-11}
  @syntax[]{(gtk-application-active-window object) => window}
  @argument[object]{a @class{gtk-application} object}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Accessor of the @slot[gtk-application]{active-window} slot of the
    @class{gtk-application} class.
  @end{short}

  The slot access function @sym{gtk-application-active-window} gets the
  active window for the application.

  The active window is the one that was most recently focused within the
  application. This window may not have the focus at the moment if another
  application has it - this is just the most recently focused window within
  this application.
  @see-class{gtk-application}
  @see-class{gtk-window}")

;;; --- gtk-application-app-menu -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "app-menu" 'gtk-application) 't)
 "The @code{app-menu} property of type @class{g-menu-model} (Read / Write) @br{}
  The menu model for the application menu.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-app-menu atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-app-menu 'function)
 "@version{*2020-5-11}
  @syntax[]{(gtk-application-app-menu object) => app-menu}
  @syntax[]{(setf (gtk-application-app-menu object) app-menu)}
  @argument[object]{a @class{gtk-application} object}
  @argument[app-menu]{a @class{g-menu-model} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-application]{app-menu} slot of the
    @class{gtk-application} class.
  @end{short}

  The slot access function @sym{gtk-application-app-menu} returns the menu model
  that has been set. The slot access function
  @sym{(setf gtk-application-app-menu)} sets the application menu.

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
 "The @code{menubar} property of type @class{g-menu-model} (Read / Write) @br{}
  The menu model for the menubar.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-menubar atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-menubar 'function)
 "@version{*2020-5-11}
  @syntax[]{(gtk-application-menubar object) => menubar}
  @syntax[]{(setf (gtk-application-menubar object) menubar)}
  @argument[object]{a @class{gtk-application} object}
  @argument[menubar]{a @class{g-menu-model} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-application]{menubar} slot of the
    @class{gtk-application} class.
  @end{short}

  The slot access function @sym{gtk-application-menubar} returns the menubar.
  The slot access function @sym{(setf gtk-application-menubar)} sets or unsets
  the menubar for windows of the application.

  This is a menubar in the traditional sense.

  This can only be done in the primary instance of the application, after it
  has been registered. The handler for the \"startup\" signal is a good place
  to call this.

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
 "The @code{register-session} property of type @code{:boolean} (Read / Write)
  @br{}
  Set this property to @em{true} to register with the session manager. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-register-session atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-register-session 'function)
 "@version{*2020-5-11}
  @syntax[]{(gtk-application-register-session object) => register-session}
  @syntax[]{(setf (gtk-application-register-session object) register-session)}
  @argument[object]{a @class{gtk-application} object}
  @argument[register-session]{a boolean whether to register with the session
    manager}
  @begin{short}
    Accessor of the @slot[gtk-application]{register-session} slot of the
    @class{gtk-application} class.
  @end{short}

  Set the @code{register-session} property to @em{true} to register with the
  session mananger.
  @see-class{gtk-application}")

;;; gtk-application-screensaver-active -----------------------------------------

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "screensaver-active"
                                               'gtk-application) 't)
 "The @code{screensaver-active} property of type @code{:boolean} (Read) @br{}
  This property is @em{true} if GTK+ believes that the screensaver is currently
  active. GTK+ only tracks session state, including this, when the
  @code{register-session} property is set to @em{true}. Tracking the screensaver
  state is supported on Linux. Since 3.24 @br{}
  Default value: @em{false}")

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-application-screensaver-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-screensaver-active 'function)
 "@version{*2020-5-11}
  @syntax[]{(gtk-application-screensaver-active object) => screensaver-active}
  @argument[object]{a @class{gtk-application} object}
  @argument[screensaver-active]{a boolean whether the screensaver is active}
  @begin{short}
    Accessor of the @slot[gtk-application]{screensaver-active} slot of the
    @class{gtk-application} class.
  @end{short}

  The @slot[gtk-application]{screensaver-active} property is @em{true} if GTK+
  believes that the screensaver is currently active. GTK+ only tracks session
  state, including this, when the @slot[gtk-application]{register-session}
  property is set to @em{true}. Tracking the screensaver state is supported on
  Linux.

  Since 3.24
  @see-class{gtk-application}
  @see-function{gtk-application-register-session}")

;;; ----------------------------------------------------------------------------
;;; gtk_application_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-new))

(defun gtk-application-new (application-id flags)
 "@version{*2020-5-14}
  @argument[application-id]{a string with the application ID, or @code{nil} for
    no application ID}
  @argument[flags]{the application flags of type @symbol{g-application-flags}}
  @return{A new @class{gtk-application} object.}
  @begin{short}
    Creates a new application.
  @end{short}

  The function @code{gtk_init()} is called as soon as the application gets
  registered as the primary instance. Concretely, the function @code{gtk_init()}
  is called in the default handler for the \"startup\" signal. Therefore,
  @class{gtk-application} subclasses should chain up in their \"startup\"
  handler before using any GTK+ API.

  Note that commandline arguments are not passed to the function
  @code{gtk_init()}. All GTK+ functionality that is available via commandline
  arguments can also be achieved by setting suitable environment variables such
  as @code{G_DEBUG}, so this should not be a big problem. If you absolutely must
  support GTK+ commandline arguments, you can explicitly call the function
  @code{gtk_init()} before creating the application instance.

  The application ID must be valid. See the function
  @fun{g-application-id-is-valid}.

  If no application ID is given then some features, most notably application
  uniqueness, will be disabled. No application ID is only allowed with
  GTK+ 3.6 or later.
  @see-class{gtk-application}
  @see-symbol{g-application-flags}
  @see-function{g-application-id-is-valid}"
  (make-instance 'gtk-application
                 :application-id (if application-id
                                     application-id
                                     (null-pointer))
                 :flags flags))

(export 'gtk-application-new)

;;; ----------------------------------------------------------------------------
;;; gtk_application_add_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_add_window" gtk-application-add-window) :void
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-12}
  @argument[application]{a @class{gtk-application} object}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Adds a window to the application.
  @end{short}
  This call is equivalent to setting the @slot[gtk-window]{application}
  property of @arg{window} to @arg{application}.

  Normally, the connection between the application and the window will remain
  until the window is destroyed, but you can explicitly remove it with the
  function @fun{gtk-application-remove-window}.

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
 "@version{*2020-5-12}
  @argument[application]{a @class{gtk-application} object}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Remove a window from the application.
  @end{short}
  If @arg{window} belongs to @arg{application} then this call is equivalent to
  setting the @slot[gtk-window]{application} property of @arg{window} to
  @code{nil}.

  The application may stop running as a result of a call to this function.
  @see-class{gtk-application}
  @see-class{gtk-window}
  @see-function{gtk-application-add-window}"
  (application (g-object gtk-application))
  (window (g-object gtk-window)))

(export 'gtk-application-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_windows () -> gtk-application-windows
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_get_windows" gtk-application-windows)
    (g-list (g-object gtk-window) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-14}
  @argument[application]{a @class{gtk-application} object}
  @return{A list of @class{gtk-window} widgets.}
  @begin{short}
    Gets a list of the windows associated with the application.
  @end{short}
  The list is sorted by most recently focused windows, such that the first
  element is the currently focused window. This is useful for choosing a parent
  for a transient window.
  @see-class{gtk-application}
  @see-class{gtk-window}"
  (application (g-object gtk-application)))

(export 'gtk-application-windows)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_window_by_id () -> gtk-application-window-by-id
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_get_window_by_id" gtk-application-window-by-id)
    (g-object gtk-window)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-14}
  @argument[application]{a @class{gtk-application} object}
  @argument[window-id]{an identifier number of type @code{:uint}}
  @return{The @class{gtk-application-window} widget with ID @arg{window-id},
    or @code{nil} if there is no window with this ID.}
  @begin{short}
    Returns the application window with the given ID.
  @end{short}
  The ID of an application window can be retrieved with the function
  @fun{gtk-application-window-id}.
  @see-class{gtk-application}
  @see-class{gtk-application-window}
  @see-function{gtk-application-window-id}"
  (application (g-object gtk-application))
  (window-id :uint))

(export 'gtk-application-window-by-id)

;;; ----------------------------------------------------------------------------
;;; gtk_application_inhibit ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_inhibit" gtk-application-inhibit) :uint
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-12}
  @argument[application]{the @class{gtk-application} object}
  @argument[window]{a @class{gtk-window} widget, or @code{nil}}
  @argument[flags]{what @symbol{gtk-application-inhibit-flags} types of user
    actions should be inhibited}
  @argument[reason]{a short, human-readable string that explains why these
    operations are inhibited}
  @begin{return}
    A non-zero cookie of type @code{:uint} that is used to uniquely identify
    this request. It should be used as an argument to the function
    @fun{gtk-application-uninhibit} in order to remove the request. If the
    platform does not support inhibiting or the request failed for some reason,
    0 is returned.
  @end{return}
  @begin{short}
    Inform the session manager that certain types of actions should be
    inhibited.
  @end{short}
  This is not guaranteed to work on all platforms and for all types of actions.

  Applications should invoke this method when they begin an operation that
  should not be interrupted, such as creating a CD or DVD. The types of
  actions that may be blocked are specified by the flags parameter. When the
  application completes the operation it should call the function
  @fun{gtk-application-uninhibit} to remove the inhibitor. Note that an
  application can have multiple inhibitors, and all of the must be individually
  removed. Inhibitors are also cleared when the application exits.

  Applications should not expect that they will always be able to block the
  action. In most cases, users will be given the option to force the action to
  take place.

  Reasons should be short and to the point.

  If the argument @arg{window} is given, the session manager may point the user
  to this window to find out more about why the action is inhibited.
  @see-class{gtk-application}
  @see-class{gtk-window}
  @see-symbol{gtk-application-inhibit-flags}
  @see-function{gtk-application-uninhibit}
  @see-function{gtk-application-is-inhibited}"
  (application (g-object gtk-application))
  (window (g-object gtk-window))
  (flags gtk-application-inhibit-flags)
  (reason g-string))

(export 'gtk-application-inhibit)

;;; ----------------------------------------------------------------------------
;;; gtk_application_uninhibit ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_uninhibit" gtk-application-uninhibit) :void
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-14}
  @argument[application]{the @class{gtk-application} object}
  @argument[cookie]{a cookie of type @code{:uint} that was returned by the
    function @fun{gtk-application-inhibit}}
  @begin{short}
    Removes an inhibitor that has been established with the function
    @fun{gtk-application-inhibit}.
  @end{short}
  Inhibitors are also cleared when the application exits.
  @see-class{gtk-application}
  @see-function{gtk-application-inhibit}
  @see-function{gtk-application-is-inhibited}"
  (application (g-object gtk-application))
  (cookie :uint))

(export 'gtk-application-uninhibit)

;;; ----------------------------------------------------------------------------
;;; gtk_application_is_inhibited ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_is_inhibited" gtk-application-is-inhibited) :boolean
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-14}
  @argument[application]{the @class{gtk-application} object}
  @argument[flags]{what @symbol{gtk-application-inhibit-flags} types of actions
    should be queried}
  @return{A boolean that is @arg{true} if any of the actions specified in
    the argument @arg{flags} are inhibited.}
  @begin{short}
    Determines if any of the actions specified in the argument @arg{flags} are
    currently inhibited, possibly by another application.
  @end{short}
  @see-class{gtk-application}
  @see-symbol{gtk-application-inhibit-flags}
  @see-function{gtk-application-inhibit}
  @see-function{gtk-application-uninhibit}"
  (application (g-object gtk-application))
  (flags gtk-application-inhibit-flags))

(export 'gtk-application-is-inhibited)

;;; ----------------------------------------------------------------------------
;;; gtk_application_prefers_app_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_prefers_app_menu" gtk-application-prefers-app-menu)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-14}
  @argument[application]{a @class{gtk-application} object}
  @return{A boolean that is @arg{true} if the desktop enviroment would prefer
    an application menu be shown.}
  @begin{short}
    Determines if the desktop environment in which the application is running
    would prefer an application menu be shown.
  @end{short}

  If this function returns @arg{true} then the application should call the
  function @fun{gtk-application-app-menu} with the contents of an application
  menu, which will be shown by the desktop environment. If it returns @code{nil}
  then you should consider using an alternate approach, such as a menubar.

  The value returned by this function is purely advisory and you are free to
  ignore it. If you call the function @fun{gtk-application-app-menu} even if
  the desktop environment does not support application menus, then a fallback
  will be provided.

  Applications are similarly free not to set an application menu even if the
  desktop environment wants to show one. In that case, a fallback will also be
  created by the desktop environment. GNOME, for example, uses a menu with only
  a \"Quit\" item in it.

  The value returned by this function never changes. Once it returns a
  particular value, it is guaranteed to always return the same value.

  You may only call this function after the application has been registered
  and after the base \"startup\" handler has run. You are most likely to want
  to use this from your own \"startup\" handler. It may also make sense to
  consult this function in an \"activate\", \"open\" or an action activation
  handler, while constructing the UI in order to determine if you should show
  a gear menu or not.

  This function will return @code{nil} on Mac OS and a default application menu
  will be created automatically with the contents of that menu typical to most
  Mac OS applications. If you call the function @fun{gtk-application-app-menu}
  anyway, then this menu will be replaced with your own.
  @see-class{gtk-application}
  @see-function{gtk-application-app-menu}"
  (application (g-object gtk-application)))

(export 'gtk-application-prefers-app-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_menu_by_id () -> gtk-application-menu-by-id
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_get_menu_by_id" gtk-application-menu-by-id)
    (g-object g-menu)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-11}
  @argument[application]{a @class{gtk-application} object}
  @argument[menu-id]{a string with the ID of the menu to look up}
  @return{Gets the menu of type @class{g-menu} with the given @arg{menu-id}
    from the automatically loaded resources.}
  @begin{short}
    Gets a menu from automatically loaded resources.
  @end{short}
  @see-class{gtk-application}
  @see-class{g-menu}"
  (application (g-object gtk-application))
  (menu-id g-string))

(export 'gtk-application-menu-by-id)

;;; ----------------------------------------------------------------------------
;;; gtk_application_add_accelerator ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_add_accelerator" %gtk-application-add-accelerator)
    :void
  (application (g-object gtk-application))
  (accel g-string)
  (action-name g-string)
  (parameter (:pointer (:struct g-variant))))

(defun gtk-application-add-accelerator (application accel action-name parameter)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-14}
  @argument[application]{a @class{gtk-application} object}
  @argument[accel]{a string representing an accelerator}
  @argument[action-name]{a string with the name of the action to activate}
  @argument[parameter]{a @type{g-variant} parameter to pass when activating
    the action, or @code{nil} if the action does not accept an activation
    parameter}
  @begin{short}
    Installs an accelerator that will cause the named action to be activated
    when the key combination specificed by the accelerator is pressed.
  @end{short}

  The argument @arg{accelerator} must be a string that can be parsed by the
  function @fun{gtk-accelerator-parse}, e.g. \"<Primary>q\" or
  \"<Control><Alt>p\".

  The argument @arg{action-name} must be the name of an action as it would be
  used in the application menu, i.e. actions that have been added to the
  application are referred to with an \"app.\" prefix, and window-specific
  actions with a \"win.\" prefix.

  @class{gtk-application} also extracts accelerators out of 'accel' attributes
  in the @class{g-menu-model} objects passed to the functions
  @fun{gtk-application-app-menu} and @fun{gtk-application-menubar}, which is
  usually more convenient than calling this function for each accelerator.
  @begin[Warning]{dictionary}
    The function @sym{gtk-application-add-accelerator} has been deprecated since
    version 3.14 and should not be used in newly-written code. Use the function
    @fun{gtk-application-accels-for-action} instead.
  @end{dictionary}
  @see-class{gtk-application}
  @see-function{gtk-accelerator-parse}
  @see-function{gtk-application-app-menu}
  @see-function{gtk-application-menubar}
  @see-function{gtk-application-remove-accelerator}
  @see-function{gtk-application-accels-for-action}"
  (%gtk-application-add-accelerator application
                                    accel
                                    action-name
                                    (if parameter parameter (null-pointer))))

(export 'gtk-application-add-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_application_remove_accelerator ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_remove_accelerator"
          %gtk-application-remove-accelerator) :void
  (application (g-object gtk-application))
  (action-name g-string)
  (parameter (:pointer (:struct g-variant))))

(defun gtk-application-remove-accelerator (application action-name parameter)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-14}
  @argument[application]{a @class{gtk-application} object}
  @argument[action-name]{a string with the name of the action to activate}
  @argument[parameter]{a @type{g-variant} parameter to pass when activating the
    action, or @code{nil} if the action does not accept an activation parameter}
  @begin{short}
    Removes an accelerator that has been previously added with the function
    @fun{gtk-application-add-accelerator}.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-application-remove-accelerator} has been deprecated
    since version 3.14 and should not be used in newly-written code. Use the
    function @fun{gtk-application-accels-for-action} instead.
  @end{dictionary}
  @see-class{gtk-application}
  @see-function{gtk-application-add-accelerator}
  @see-function{gtk-application-accels-for-action}"
  (%gtk-application-remove-accelerator application
                                       action-name
                                       (if parameter parameter (null-pointer))))

(export 'gtk-application-remove-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_application_list_action_descriptions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_list_action_descriptions"
           gtk-application-list-action-descriptions) g-strv
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-14}
  @argument[application]{a @class{gtk-application} object}
  @return{A list of strings with the detailed action names.}
  @begin{short}
    Lists the detailed action names which have associated accelerators.
  @end{short}
  See the function @fun{gtk-application-accels-for-action}.
  @see-class{gtk-application}
  @see-function{gtk-applicaton-accels-for-action}"
  (application (g-object gtk-application)))

(export 'gtk-application-list-action-descriptions)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_accels_for_action ()
;;; gtk_application_set_accels_for_action () -> gtk-application-accels-for-action
;;; ----------------------------------------------------------------------------

(defun (setf gtk-application-accels-for-action) (accels application action-name)
  (foreign-funcall "gtk_application_set_accels_for_action"
                   (g-object gtk-application) application
                   g-string action-name
                   g-strv accels
                   :void)
  accels)

(defcfun ("gtk_application_get_accels_for_action"
           gtk-application-accels-for-action) g-strv
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-14}
  @syntax[]{(gtk-application-accels-for-action application) => accels}
  @syntax[]{(setf (gtk-application-accels-for-action application) accels)}
  @argument[application]{a @class{gtk-application} object}
  @argument[action-name]{a string with a detailed action name, specifying an
    action and target}
  @argument[accels]{a list of strings of accelerators in the format understood
    by the function @fun{gtk-accelerator-parse}}
  @begin{short}
    Accessor of the accelerators that are associated with the given action.
  @end{short}

  The function @sym{gtk-application-accels-for-action} gets the keyboard
  accelerators that will trigger the given action. The function
  @sym{(setf gtk-application-accels-for-action)} sets zero or more keyboard
  accelerators that will trigger the given action.

  The first item in the list of accelerators will be the primary accelerator,
  which may be displayed in the UI. To remove all accelerators for an action,
  use an empty list.

  For the detailed action name, see the functions
  @fun{g-action-parse-detailed-name} and @fun{g-action-print-detailed-name}.
  @see-class{gtk-application}
  @see-function{g-action-parse-detailed-name}
  @see-function{g-action-print-detailed-name}"
  (application (g-object gtk-application))
  (action-name g-string))

(export 'gtk-application-accels-for-action)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_actions_for_accel () -> gtk-application-actions-for-accel
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_get_actions_for_accel"
           gtk-application-actions-for-accel) g-strv
 "@version{*2020-5-14}
  @argument[application]{a @class{gtk-application} object}
  @argument[accel]{a string with an accelerator that can be parsed by the
    function @fun{gtk-accelerator-parse}}
  @return{A list of strings of actions for the argument @arg{accel}.}
  @begin{short}
    Returns the list of actions, possibly empty, that the given accelerator
    maps to.
  @end{short}
  Each item in the list is a detailed action name in the usual form.

  This might be useful to discover if an accelerator already exists in order
  to prevent installation of a conflicting accelerator, from an accelerator
  editor or a plugin system, for example. Note that having more than one
  action per accelerator may not be a bad thing and might make sense in cases
  where the actions never appear in the same context.

  In case there are no actions for a given accelerator, an empty list is
  returned.

  It is a programmer error to pass an invalid accelerator string. If you are
  unsure, check it with the function @fun{gtk-accelerator-parse} first.
  @see-class{gtk-application}
  @see-function{gtk-accelerator-parse}"
  (application (g-object gtk-application))
  (accel g-string))

(export 'gtk-application-actions-for-accel)

;;; --- End of file gtk.application.lisp ---------------------------------------
