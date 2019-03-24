;;; ----------------------------------------------------------------------------
;;; gtk.pad-controller.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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
;;; General Public License. If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkPadController
;;;
;;;     Controller for drawing tablet pads
;;;
;;; Types and Values
;;;
;;;     GtkPadController
;;;     GtkPadActionType
;;;     GtkPadActionEntry
;;;
;;; Functions
;;;
;;;     gtk_pad_controller_new
;;;     gtk_pad_controller_set_action_entries
;;;     gtk_pad_controller_set_action
;;;
;;; Properties
;;;
;;;     GActionGroup *  action-group  Read / Write / Construct Only
;;;     GdkDevice *     pad           Read / Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkPadController
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkPadActionType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPadActionType" gtk-pad-action-type
  (:export t
   :type-initializer "gtk_pad_action_type_get_type")
  :button
  :ring
  :strip)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-pad-action-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-pad-action-type atdoc:*external-symbols*)
 "@version{2019-3-24}
  @begin{short}
    The type of a pad action.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPadActionType\" gtk-pad-action-type
  (:export t
   :type-initializer \"gtk_pad_action_type_get_type\")
  :button
  :ring
  :strip)
  @end{pre}
  @begin[code]{table}
    @entry[:button]{Action is triggered by a pad button.}
    @entry[:ring]{Action is triggered by a pad ring.}
    @entry[:strip]{Action is triggered by a pad strip.}
  @end{table}
  @see-class{gtk-pad-controller}")

;;; ----------------------------------------------------------------------------
;;; struct GtkPadActionEntry
;;; ----------------------------------------------------------------------------

(defcstruct gtk-pad-action-entry
  (type gtk-pad-action-type)
  (index :int)
  (mode :int)
  (label :string)
  (action-name :string))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-pad-action-entry atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'gtk-pad-action-entry atdoc:*external-symbols*)
 "@version{2019-3-24}
  @begin{short}
    Struct defining a pad action entry.
  @end{short}
  @begin{pre}
(defcstruct gtk-pad-action-entry
  (type gtk-pad-action-type)
  (index :int)
  (mode :int)
  (label :string)
  (action-name :string))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The type of pad feature that will trigger this action entry.}
    @entry[index]{The 0-indexed button/ring/strip number that will trigger this
      action entry.}
    @entry[mode]{The mode that will trigger this action entry, or -1 for all
      modes.}
    @entry[label]{Human readable description of this action entry, this string
      should be deemed user-visible.}
    @entry[action-name]{Action name that will be activated in the
      @class{g-action-group}.}
  @end{table}
  @see-class{gtk-pad-controller}
  @see-symbol{gtk-pad-action-type}")

(export 'gtk-pad-action-entry)

;;; ----------------------------------------------------------------------------
;;; struct GtkPadController
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPadController" gtk-pad-controller
  (:superclass gtk-event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_pad_controller_get_type")
  ((action-group
    gtk-pad-controller-action-group
    "action-group" "GActionGroup" t t)
   (pad
    gtk-pad-controller-pad
    "pad" "GdkDevice" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-pad-controller 'type)
 "@version{2019-3-24}
  @begin{short}
    @sym{gtk-pad-controller} is an event controller for the pads found in
    drawing tablets (The collection of buttons and tactile sensors often found
    around the stylus-sensitive area).
  @end{short}

  These buttons and sensors have no implicit meaning, and by default they
  perform no action, this event controller is provided to map those to
  @class{g-action} objects, thus letting the application give those a more
  semantic meaning.

  Buttons and sensors are not constrained to triggering a single action, some
  @code{:tablet-pad} devices feature multiple \"modes\", all these input
  elements have one current mode, which may determine the final action being
  triggered. Pad devices often divide buttons and sensors into groups, all
  elements in a group share the same current mode, but different groups may have
  different modes. See the functions @fun{gdk-device-pad-get-n-groups} and
  @fun{gdk-device-pad-get-group-n-modes}.

  Each of the actions that a given button/strip/ring performs for a given mode
  is defined by @symbol{gtk-pad-action-entry}, it contains an action name that
  will be looked up in the given @class{g-action-group} and activated whenever
  the specified input element and mode are triggered.

  A simple example of @sym{gtk-pad-controller} usage, assigning button 1 in all
  modes and pad devices to an \"invert-selection\" action:
  @begin{pre}
GtkPadActionEntry *pad_actions = {
  { GTK_PAD_ACTION_BUTTON, 1, -1, \"Invert selection\", \"pad-actions.invert-selection\" @},
  ...
@};

...
action_group = g_simple_action_group_new ();
action = g_simple_action_new (\"pad-actions.invert-selection\", NULL);
g_signal_connect (action, \"activate\", on_invert_selection_activated, NULL);
g_action_map_add_action (G_ACTION_MAP (action_group), action);
...
pad_controller = gtk_pad_controller_new (window, action_group, NULL);
  @end{pre}
  The actions belonging to rings/strips will be activated with a parameter of
  type @var{+g-variant-type-double+} bearing the value of the given axis, it is
  required that those are made stateful and accepting this
  @symbol{g-variant-type}.

  Since 3.22
  @see-slot{gtk-pad-controller-action-group}
  @see-slot{gtk-pad-controller-pad}
  @see-class{gdk-event-conroller}
  @see-class{gdk-device-pad}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-pad-controller-action-group ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "action-group"
                                               'gtk-pad-controller) 't)
 "The @code{action-group} property of type @class{g-action-group}
  (Read / Write / Construct Only) @br{}
  Action group to launch actions from. @br{}
  Since 3.22 @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-pad-controller-action-group
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-pad-controller-action-group 'function)
 "@version{2019-3-24}
  @syntax[]{(gtk-pad-controller-action-group object) => group)}
  @syntax[]{(setf (gtk-pad-controller-action-group object) group)}
  @argument[object]{a @class{gtk-pad-controller} object}
  @argument[group]{the @class{g-action-group}}
  @begin{short}
    Accessor of the slot @slot[gtk-pad-controller]{action-group} of the
    @class{gtk-pad-controller} class.
  @end{short}

  Since 3.22
  @see-class{gtk-pad-controller}")

;;; --- gtk-pad-controller-pad -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pad"
                                               'gtk-pad-controller) 't)
 "The @code{pad} property of type @class{gdk-device}
  (Read / Write / Construct Only) @br{}
  Pad device to control. @br{}
  Since 3.22 @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-pad-controller-pad
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-pad-controller-pad 'function)
 "@version{2019-3-24}
  @syntax[]{(gtk-pad-controller-pad object) => pad)}
  @syntax[]{(setf (gtk-pad-controller-pad object) pad)}
  @argument[object]{a @class{gtk-pad-controller} object}
  @argument[pad]{the @class{gdk-device} object}
  @begin{short}
    Accessor of the slot @slot[gtk-pad-controller]{pad} of the
    @class{gtk-pad-controller} class.
  @end{short}

  Since 3.22
  @see-class{gtk-pad-controller}")

;;; ----------------------------------------------------------------------------
;;; gtk_pad_controller_new ()
;;;
;;; GtkPadController *
;;; gtk_pad_controller_new (GtkWindow *window,
;;;                         GActionGroup *group,
;;;                         GdkDevice *pad);
;;;
;;; Creates a new GtkPadController that will associate events from pad to
;;; actions. A NULL pad may be provided so the controller manages all pad
;;; devices generically, it is discouraged to mix GtkPadController objects with
;;; NULL and non-NULL pad argument on the same window , as execution order is
;;; not guaranteed.
;;;
;;; The GtkPadController is created with no mapped actions. In order to map pad
;;; events to actions, use gtk_pad_controller_set_action_entries() or
;;; gtk_pad_controller_set_action().
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; group :
;;;     GActionGroup to trigger actions from
;;;
;;; pad :
;;;     A GDK_SOURCE_TABLET_PAD device, or NULL to handle all pads.
;;;
;;; Returns :
;;;     A newly created GtkPadController
;;;
;;; Since: 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_pad_controller_set_action_entries ()
;;;
;;; void
;;; gtk_pad_controller_set_action_entries (GtkPadController *controller,
;;;                                        const GtkPadActionEntry *entries,
;;;                                        gint n_entries);
;;;
;;; This is a convenience function to add a group of action entries on
;;; controller . See GtkPadActionEntry and gtk_pad_controller_set_action().
;;;
;;; controller :
;;;     a GtkPadController
;;;
;;; entries :
;;;     the action entries to set on controller .
;;;
;;; n_entries :
;;;     the number of elements in entries
;;;
;;; Since: 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_pad_controller_set_action ()
;;;
;;; void
;;; gtk_pad_controller_set_action (GtkPadController *controller,
;;;                                GtkPadActionType type,
;;;                                gint index,
;;;                                gint mode,
;;;                                const gchar *label,
;;;                                const gchar *action_name);
;;;
;;; Adds an individual action to controller . This action will only be activated
;;; if the given button/ring/strip number in index is interacted while the
;;; current mode is mode . -1 may be used for simple cases, so the action is
;;; triggered on all modes.
;;;
;;; The given label should be considered user-visible, so internationalization
;;; rules apply. Some windowing systems may be able to use those for user
;;; feedback.
;;;
;;; controller :
;;;     a GtkPadController
;;;
;;; type :
;;;     the type of pad feature that will trigger this action
;;;
;;; index :
;;;     the 0-indexed button/ring/strip number that will trigger this action
;;;
;;; mode :
;;;     the mode that will trigger this action, or -1 for all modes.
;;;
;;; label :
;;;     Human readable description of this action, this string should be deemed
;;;     user-visible.
;;;
;;; action_name :
;;;     action name that will be activated in the GActionGroup
;;;
;;; Since: 3.22
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.pad-controller.lisp ------------------------------------
