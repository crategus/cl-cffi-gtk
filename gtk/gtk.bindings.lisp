;;; ----------------------------------------------------------------------------
;;; gtk.bindings.lisp
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
;;; Bindings
;;;
;;;     Key bindings for individual widgets
;;;
;;; Types and Values
;;;
;;;     GtkBindingSet
;;;     GtkBindingEntry
;;;     GtkBindingSignal
;;;     GtkBindingArg
;;;
;;; Functions
;;;
;;;     gtk_binding_entry_add_signall
;;;     gtk_binding_set_new
;;;     gtk_binding_set_by_class
;;;     gtk_binding_set_find
;;;     gtk_bindings_activate
;;;     gtk_bindings_activate_event
;;;     gtk_binding_set_activate
;;;     gtk_binding_entry_add_signal
;;;     gtk_binding_entry_add_signal_from_string
;;;     gtk_binding_entry_skip
;;;     gtk_binding_entry_remove
;;;     gtk_binding_set_add_path
;;;
;;; Description
;;;
;;; GtkBindingSet provides a mechanism for configuring GTK+ key bindings through
;;; CSS files. This eases key binding adjustments for application developers as
;;; well as users and provides GTK+ users or administrators with high key
;;; binding configurability which requires no application or toolkit side
;;; changes.
;;;
;;; Installing a key binding
;;;
;;; A CSS file binding consists of a 'binding-set' definition and a match
;;; statement to apply the binding set to specific widget types. Details on the
;;; matching mechanism are described under Selectors in the GtkCssProvider
;;; documentation. Inside the binding set definition, key combinations are bound
;;; to one or more specific signal emissions on the target widget. Key
;;; combinations are strings consisting of an optional GdkModifierType name and
;;; key names such as those defined in <gdk/gdkkeysyms.h> or returned from
;;; gdk_keyval_name(), they have to be parsable by gtk_accelerator_parse().
;;; Specifications of signal emissions consist of a string identifying the
;;; signal name, and a list of signal specific arguments in parenthesis.
;;;
;;; For example for binding Control and the left or right cursor keys of a
;;; GtkEntry widget to the "move-cursor" signal (so movement occurs in
;;; 3-character steps), the following binding can be used:
;;;
;;; @binding-set MoveCursor3
;;; {
;;;   bind "<Control>Right" { "move-cursor" (visual-positions, 3, 0) };
;;;   bind "<Control>Left" { "move-cursor" (visual-positions, -3, 0) };
;;; };
;;; GtkEntry
;;; {
;;;   gtk-key-bindings: MoveCursor3
;;; }
;;;
;;; Unbinding existing key bindings
;;;
;;; GTK+ already defines a number of useful bindings for the widgets it
;;; provides. Because custom bindings set up in CSS files take precedence over
;;; the default bindings shipped with GTK+, overriding existing bindings as
;;; demonstrated in Installing a key binding works as expected. The same
;;; mechanism can not be used to "unbind" existing bindings, however.
;;;
;;; @binding-set MoveCursor3
;;; {
;;;   bind "<Control>Right" {  };
;;;   bind "<Control>Left" {  };
;;; };
;;; GtkEntry
;;; {
;;;   gtk-key-bindings: MoveCursor3
;;; }
;;;
;;; The above example will not have the desired effect of causing
;;; "<Control>Right" and "<Control>Left" key presses to be ignored by GTK+.
;;; Instead, it just causes any existing bindings from the bindings set
;;; "MoveCursor3" to be deleted, so when "<Control>Right" or "<Control>Left" are
;;; pressed, no binding for these keys is found in binding set "MoveCursor3".
;;; GTK+ will thus continue to search for matching key bindings, and will
;;; eventually lookup and find the default GTK+ bindings for entries which
;;; implement word movement. To keep GTK+ from activating its default bindings,
;;; the "unbind" keyword can be used like this:
;;;
;;; @binding-set MoveCursor3
;;; {
;;;   unbind "<Control>Right";
;;;   unbind "<Control>Left";
;;; };
;;; GtkEntry
;;; {
;;;   gtk-key-bindings: MoveCursor3
;;; }
;;;
;;; Now, GTK+ will find a match when looking up "<Control>Right" and
;;; "<Control>Left" key presses before it resorts to its default bindings, and
;;; the match instructs it to abort ("unbind") the search, so the key presses
;;; are not consumed by this widget. As usual, further processing of the key
;;; presses, e.g. by an entry's parent widget, is now possible.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkBindingSet
;;;
;;; struct GtkBindingSet {
;;;   gchar           *set_name;
;;;   gint             priority;
;;;   GSList          *widget_path_pspecs;
;;;   GSList          *widget_class_pspecs;
;;;   GSList          *class_branch_pspecs;
;;;   GtkBindingEntry *entries;
;;;   GtkBindingEntry *current;
;;;   guint            parsed : 1;
;;; };
;;;
;;; A binding set maintains a list of activatable key bindings. A single binding
;;; set can match multiple types of widgets. Similar to style contexts, can be
;;; matched by any information contained in a widgets GtkWidgetPath. When a
;;; binding within a set is matched upon activation, an action signal is emitted
;;; on the target widget to carry out the actual activation.
;;;
;;; gchar *set_name;
;;;     unique name of this binding set
;;;
;;; gint priority;
;;;     unused
;;;
;;; GSList *widget_path_pspecs;
;;;     unused
;;;
;;; GSList *widget_class_pspecs;
;;;     unused
;;;
;;; GSList *class_branch_pspecs;
;;;     unused
;;;
;;; GtkBindingEntry *entries;
;;;     the key binding entries in this binding set
;;;
;;; GtkBindingEntry *current;
;;;     implementation detail
;;;
;;; guint parsed : 1;
;;;     whether this binding set stems from a CSS file and is reset upon theme
;;;     changes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkBindingEntry
;;;
;;; struct GtkBindingEntry {
;;;   /* key portion */
;;;   guint             keyval;
;;;   GdkModifierType   modifiers;
;;;
;;;   GtkBindingSet    *binding_set;
;;;   guint             destroyed     : 1;
;;;   guint             in_emission   : 1;
;;;   guint             marks_unbound : 1;
;;;   GtkBindingEntry  *set_next;
;;;   GtkBindingEntry  *hash_next;
;;;   GtkBindingSignal *signals;
;;; };
;;;
;;; Each key binding element of a binding sets binding list is represented by a GtkBindingEntry.
;;;
;;; guint keyval;
;;;     key value to match
;;;
;;; GdkModifierType modifiers;
;;;     key modifiers to match
;;;
;;; GtkBindingSet *binding_set;
;;;     binding set this entry belongs to
;;;
;;; guint destroyed : 1;
;;;     implementation detail
;;;
;;; guint in_emission : 1;
;;;     implementation detail
;;;
;;; guint marks_unbound : 1;
;;;     implementation detail
;;;
;;; GtkBindingEntry *set_next;
;;;     linked list of entries maintained by binding set
;;;
;;; GtkBindingEntry *hash_next;
;;;     implementation detail
;;;
;;; GtkBindingSignal *signals;
;;;     action signals of this entry
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkBindingSignal
;;;
;;; struct GtkBindingSignal {
;;;   GtkBindingSignal *next;
;;;   gchar            *signal_name;
;;;   guint             n_args;
;;;   GtkBindingArg    *args;
;;; };
;;;
;;; A GtkBindingSignal stores the necessary information to activate a widget in
;;; response to a key press via a signal emission.
;;;
;;; GtkBindingSignal *next;
;;;     implementation detail
;;;
;;; gchar *signal_name;
;;;     the action signal to be emitted
;;;
;;; guint n_args;
;;;     number of arguments specified for the signal
;;;
;;; GtkBindingArg *args;
;;;     the arguments specified for the signal
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkBindingArg
;;;
;;; struct GtkBindingArg {
;;;   GType      arg_type;
;;;   union {
;;;     glong    long_data;
;;;     gdouble  double_data;
;;;     gchar   *string_data;
;;;   } d;
;;; };
;;;
;;; A GtkBindingArg holds the data associated with an argument for a key binding
;;; signal emission as stored in GtkBindingSignal.
;;;
;;; GType arg_type;
;;;     implementation detail
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_binding_entry_add_signall ()
;;;
;;; void gtk_binding_entry_add_signall (GtkBindingSet *binding_set,
;;;                                     guint keyval,
;;;                                     GdkModifierType modifiers,
;;;                                     const gchar *signal_name,
;;;                                     GSList *binding_args);
;;;
;;; Override or install a new key binding for keyval with modifiers on
;;; binding_set.
;;;
;;; binding_set :
;;;     a GtkBindingSet to add a signal to
;;;
;;; keyval :
;;;     key value
;;;
;;; modifiers :
;;;     key modifier
;;;
;;; signal_name :
;;;     signal name to be bound
;;;
;;; binding_args :
;;;     list of GtkBindingArg signal arguments
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_binding_set_new ()
;;;
;;; GtkBindingSet * gtk_binding_set_new (const gchar *set_name);
;;;
;;; GTK+ maintains a global list of binding sets. Each binding set has a unique
;;; name which needs to be specified upon creation.
;;;
;;; set_name :
;;;     unique name of this binding set
;;;
;;; Returns :
;;;     new binding set.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_binding_set_by_class ()
;;;
;;; GtkBindingSet * gtk_binding_set_by_class (gpointer object_class);
;;;
;;; This function returns the binding set named after the type name of the
;;; passed in class structure. New binding sets are created on demand by this
;;; function.
;;;
;;; object_class :
;;;     a valid GObject class
;;;
;;; Returns :
;;;     the binding set corresponding to object_class. [transfer full]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_binding_set_find ()
;;;
;;; GtkBindingSet * gtk_binding_set_find (const gchar *set_name);
;;;
;;; Find a binding set by its globally unique name.
;;;
;;; The set_name can either be a name used for gtk_binding_set_new() or the type
;;; name of a class used in gtk_binding_set_by_class().
;;;
;;; set_name :
;;;     unique binding set name
;;;
;;; Returns :
;;;     NULL or the specified binding set. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_bindings_activate ()
;;;
;;; gboolean gtk_bindings_activate (GObject *object,
;;;                                 guint keyval,
;;;                                 GdkModifierType modifiers);
;;;
;;; Find a key binding matching keyval and modifiers and activate the binding
;;; on object.
;;;
;;; object :
;;;     object to activate when binding found
;;;
;;; keyval :
;;;     key value of the binding
;;;
;;; modifiers :
;;;     key modifier of the binding
;;;
;;; Returns :
;;;     TRUE if a binding was found and activated
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_bindings_activate_event ()
;;;
;;; gboolean gtk_bindings_activate_event (GObject *object, GdkEventKey *event);
;;;
;;; Looks up key bindings for object to find one matching event, and if one was
;;; found, activate it.
;;;
;;; object :
;;;     a GObject (generally must be a widget)
;;;
;;; event :
;;;     a GdkEventKey
;;;
;;; Returns :
;;;     TRUE if a matching key binding was found
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_binding_set_activate ()
;;;
;;; gboolean gtk_binding_set_activate (GtkBindingSet *binding_set,
;;;                                    guint keyval,
;;;                                    GdkModifierType modifiers,
;;;                                    GObject *object);
;;;
;;; Find a key binding matching keyval and modifiers within binding_set and
;;; activate the binding on object.
;;;
;;; binding_set :
;;;     a GtkBindingSet set to activate
;;;
;;; keyval :
;;;     key value of the binding
;;;
;;; modifiers :
;;;     key modifier of the binding
;;;
;;; object :
;;;     object to activate when binding found
;;;
;;; Returns :
;;;     TRUE if a binding was found and activated
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_binding_entry_add_signal ()
;;;
;;; void gtk_binding_entry_add_signal (GtkBindingSet *binding_set,
;;;                                    guint keyval,
;;;                                    GdkModifierType modifiers,
;;;                                    const gchar *signal_name,
;;;                                    guint n_args,
;;;                                    ...);
;;;
;;; Override or install a new key binding for keyval with modifiers on
;;; binding_set. When the binding is activated, signal_name will be emitted on
;;; the target widget, with n_args Varargs used as arguments.
;;;
;;; binding_set :
;;;     a GtkBindingSet to install an entry for
;;;
;;; keyval :
;;;     key value of binding to install
;;;
;;; modifiers :
;;;     key modifier of binding to install
;;;
;;; signal_name :
;;;     signal to execute upon activation
;;;
;;; n_args :
;;;     number of arguments to signal_name
;;;
;;; ... :
;;;     arguments to signal_name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_binding_entry_add_signal_from_string ()
;;;
;;; GTokenType gtk_binding_entry_add_signal_from_string
;;;                                                 (GtkBindingSet *binding_set,
;;;                                                  const gchar *signal_desc);
;;;
;;; Parses a signal description from signal_desc and incorporates it into
;;; binding_set.
;;;
;;; Signal descriptions may either bind a key combination to one or more
;;; signals:
;;;
;;; bind "key" {
;;;   "signalname" (param, ...)
;;;   ...
;;; }
;;;
;;; Or they may also unbind a key combination:
;;;
;;; unbind "key"
;;;
;;; Key combinations must be in a format that can be parsed by
;;; gtk_accelerator_parse().
;;;
;;; binding_set :
;;;     a GtkBindingSet
;;;
;;; signal_desc :
;;;     a signal description
;;;
;;; Returns :
;;;     G_TOKEN_NONE if the signal was successfully parsed and added, the
;;;     expected token otherwise
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_binding_entry_skip ()
;;;
;;; void gtk_binding_entry_skip (GtkBindingSet *binding_set,
;;;                              guint keyval,
;;;                              GdkModifierType modifiers);
;;;
;;; Install a binding on binding_set which causes key lookups to be aborted, to
;;; prevent bindings from lower priority sets to be activated.
;;;
;;; binding_set :
;;;     a GtkBindingSet to skip an entry of
;;;
;;; keyval :
;;;     key value of binding to skip
;;;
;;; modifiers :
;;;     key modifier of binding to skip
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_binding_entry_remove ()
;;;
;;; void gtk_binding_entry_remove (GtkBindingSet *binding_set,
;;;                                guint keyval,
;;;                                GdkModifierType modifiers);
;;;
;;; Remove a binding previously installed via gtk_binding_entry_add_signal() on
;;; binding_set.
;;;
;;; binding_set :
;;;     a GtkBindingSet to remove an entry of
;;;
;;; keyval :
;;;     key value of binding to remove
;;;
;;; modifiers :
;;;     key modifier of binding to remove
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_binding_set_add_path ()
;;;
;;; void gtk_binding_set_add_path (GtkBindingSet *binding_set,
;;;                                GtkPathType path_type,
;;;                                const gchar *path_pattern,
;;;                                GtkPathPriorityType priority);
;;;
;;; Warning
;;;
;;; gtk_binding_set_add_path is deprecated and should not be used in
;;; newly written code. 3.0
;;;
;;; This function was used internally by the GtkRC parsing mechanism to assign
;;; match patterns to GtkBindingSet structures.
;;;
;;; In GTK+ 3, these match patterns are unused.
;;;
;;; binding_set :
;;;     a GtkBindingSet to add a path to
;;;
;;; path_type :
;;;     path type the pattern applies to
;;;
;;; path_pattern :
;;;     the actual match pattern
;;;
;;; priority :
;;;     binding priority
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.bindings.lisp ------------------------------------------
