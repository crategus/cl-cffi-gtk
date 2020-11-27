;;; ----------------------------------------------------------------------------
;;; gio.package.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.66 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2020 Dieter Kaiser
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

(defpackage :gio
  (:use :glib :glib-init :gobject :cl :cffi))

#+cl-cffi-gtk-documentation
(setf (documentation (find-package :gio) t)
 "This is the API documentation of a Lisp binding to GIO.
  GIO is striving to provide a modern, easy-to-use VFS API that sits at the
  right level in the library stack, as well as other generally useful APIs for
  desktop applications (such as networking and D-Bus support). The goal is to
  overcome the shortcomings of GnomeVFS and provide an API that is so good that
  developers prefer it over raw POSIX calls. Among other things that means using
  GObject. It also means not cloning the POSIX API, but providing higher-level,
  document-centric interfaces.
  @begin[Files types and applications]{section}
    @begin[GContentType]{subsection}
      Platform-specific content typing.

      A content type is a platform specific string that defines the type of a
      file. On UNIX it is a mime type like \"text/plain\" or \"image/png\". On
      Win32 it is an extension string like \".doc\", \".txt\" or a perceived
      string like \"audio\". Such strings can be looked up in the registry at
      @code{HKEY_CLASSES_ROOT}.

      @about-function{g-content-type-equals}
      @about-function{g-content-type-is-a}
      @about-function{g-content-type-is-unknown}
      @about-function{g-content-type-get-description}
      @about-function{g-content-type-get-mime-type}
      @about-function{g-content-type-get-icon}
      @about-function{g-content-type-get-symbolic-icon}
      @about-function{g-content-type-get-generic-icon-name}
      @about-function{g-content-type-can-be-executable}
      @about-function{g-content-type-from-mime-type}
      @about-function{g-content-type-guess}
      @about-function{g-content-type-guess-for-tree}
      @about-function{g-content-types-get-registered}
    @end{subsection}
    @begin[GAppInfo]{subsection}
      Application information and launch contexts.

        @about-symbol{g-app-info-create-flags}
        @about-class{g-app-info}
        @about-symbol{g-app-info-iface}
        @about-class{g-app-launch-context}
        @about-function{g-app-info-create-from-commandline}
        @about-function{g-app-info-dup}
        @about-function{g-app-info-equal}
        @about-function{g-app-info-get-id}
        @about-function{g-app-info-get-name}
        @about-function{g-app-info-get-display-name}
        @about-function{g-app-info-get-description}
        @about-function{g-app-info-get-executable}
        @about-function{g-app-info-get-commandline}
        @about-function{g-app-info-get-icon}
        @about-function{g-app-info-launch}
        @about-function{g-app-info-supports-files}
        @about-function{g-app-info-supports-uris}
        @about-function{g-app-info-launch-uris}
        @about-function{g-app-info-should-show}
        @about-function{g-app-info-can-delete}
        @about-function{g-app-info-delete}
        @about-function{g-app-info-reset-type-associations}
        @about-function{g-app-info-set-as-default-for-type}
        @about-function{g-app-info-set-as-default-for-extension}
        @about-function{g-app-info-set-as-last-used-for-type}
        @about-function{g-app-info-add-supports-type}
        @about-function{g-app-info-can-remove-supports-type}
        @about-function{g-app-info-remove-supports-type}
        @about-function{g-app-info-get-supported-types}
        @about-function{g-app-info-get-all}
        @about-function{g-app-info-get-all-for-type}
        @about-function{g-app-info-get-default-for-type}
        @about-function{g-app-info-get-default-for-uri-scheme}
        @about-function{g-app-info-get-fallback-for-type}
        @about-function{g-app-info-get-recommended-for-type}
        @about-function{g-app-info-launch-default-for-uri}
        @about-function{g-app-launch-context-setenv}
        @about-function{g-app-launch-context-unsetenv}
        @about-function{g-app-launch-context-get-environment}
        @about-function{g-app-launch-context-get-display}
        @about-function{g-app-launch-context-get-startup-notify-id}
        @about-function{g-app-launch-context-launch-failed}
        @about-function{g-app-launch-context-new}
    @end{subsection}
  @end{section}
  @begin[Icons]{section}
    @begin[GIcon]{subsection}
      Interface for icons.

      @about-class{g-icon}
      @about-class{g-icon-iface}
      @about-function{g-icon-hash}
      @about-function{g-icon-equal}
      @about-function{g-icon-to-string}
      @about-function{g-icon-new-for-string}
    @end{subsection}
    @begin[GThemedIcon]{subsection}
      Icon theming support.

      @about-class{g-themed-icon}
      @about-generic{g-themed-icon-name}
      @about-generic{g-themed-icon-names}
      @about-generic{g-themed-icon-use-default-fallbacks}
      @about-function{g-themed-icon-new}
      @about-function{g-themed-icon-new-from-names}
      @about-function{g-themed-icon-new-witiconh-default-fallbacks}
      @about-function{g-themed-icon-prepend-name}
      @about-function{g-themed-icon-append-name}
    @end{subsection}
    @begin[GEmblemedIcon]{subsection}
      An implementation of GIcon for icons with emblems.

      @about-class{g-emblemed-icon}
      @about-function{g-emblemed-icon-new}
      @about-function{g-emblemed-icon-get-icon}
      @about-function{g-emblemed-icon-get-emblems}
      @about-function{g-emblemed-icon-add-emblem}
      @about-function{g-emblemed-icon-clear-emblems}
    @end{subsection}
    @begin[GEmblem]{subsection}
      An object for emblems.

      @about-class{g-emblem}
      @about-symbol{g-emblem-origin}
      @about-generic{g-emblem-icon}
      @about-generic{g-emblem-origin}
      @about-function{g-emblem-new}
      @about-function{g-emblem-new-with-origin}
    @end{subsection}
  @end{section}
  @begin[Resources]{section}
    Resource framework

    @about-symbol{g-resource-flags}
    @about-symbol{g-resource-lookup-flags}
    @about-symbol{g-static-resource}
    @about-symbol{g-resource-error}
    @about-class{g-resource}
    @about-function{g-resource-load}
    @about-function{g-resource-new-from-data}
    @about-function{g-resource-ref}
    @about-function{g-resource-unref}
    @about-function{g-resource-lookup-data}
    @about-function{g-resource-open-stream}
    @about-function{g-resource-enumerate-children}
    @about-function{g-resource-get-info}
    @about-function{g-static-resource-init}
    @about-function{g-static-resource-fini}
    @about-function{g-static-resource-get.resource}
    @about-function{g-resources-register}
    @about-function{g-resources-unregister}
    @about-function{g-resources-lookup-data}
    @about-function{g-resources-open-stream}
    @about-function{g-resources-enumerate-children}
    @about-function{g-resources-get-info}
  @end{section}
  @begin[Application support]{section}
    @begin[GApplication]{subsection}
      Core application class.

      @about-symbol{g-application-flags}
      @about-class{g-application}
      @about-generic{g-application-action-group}
      @about-generic{g-application-application-id}
      @about-generic{g-application-flags}
      @about-generic{g-application-inactivity-timeout}
      @about-generic{g-application-is-busy}
      @about-generic{g-application-is-registered}
      @about-generic{g-application-is-remote}
      @about-generic{g-application-resource-base-path}
      @about-function{g-application-id-is-valid}
      @about-function{g-application-new}
      @about-function{g-application-get-dbus-connection}
      @about-function{g-application-get-dbus-object-path}
      @about-function{g-application-set-action-group}
      @about-function{g-application-register}
      @about-function{g-application-hold}
      @about-function{g-application-release}
      @about-function{g-application-quit}
      @about-function{g-application-activate}
      @about-function{g-application-open}
      @about-function{g-application-run}
      @about-function{g-application-send-notification}
      @about-function{g-application-withdraw_notification}
      @about-function{g-application-add-main-option-entries}
      @about-function{g-application-add-main-option}
      @about-function{g-application-add-option-group}
      @about-function{g-application-set-option-context-parameter-string}
      @about-function{g-application-set-option-context-summary}
      @about-function{g-application-set-option-context-description}
      @about-function{g-application-default}
      @about-function{g-application-mark-busy}
      @about-function{g-application-unmark-busy}
      @about-function{g-application-bind-busy-property}
      @about-function{g-application-unbind-busy-property}
    @end{subsection}
    @begin[GApplicationCommandLine]{subsection}
      A command-line invocation of an application.

      @about-class{g-application-command-line}
      @about-generic{g-application-command-line-arguments}
      @about-generic{g-application-command-line-is-remote}
      @about-generic{g-application-command-line-options}
      @about-generic{g-application-command-line-platform-data}

      @about-function{g-application-command-line-get-arguments}
      @about-function{g-application-command-line-get-cwd}
      @about-function{g-application-command-line-get-environ}
      @about-function{g-application-command-line-get-options-dict}
      @about-function{g-application-command-line-get-stdin}
      @about-function{g-application-command-line-create-file-for-arg}
      @about-function{g-application-command-line-getenv}
      @about-function{g-application-command-line-get-is-remote}
      @about-function{g-application-command-line-get-platform-data}
      @about-function{g-application-command-line-set-exit-status}
      @about-function{g-application-command-line-get-exit-status}
      @about-function{g-application-command-line-print}
      @about-function{g-application-command-line-printerr}
    @end{subsection}
    @begin[GActionGroup]{subsection}
      A group of actions.

      @about-class{g-action-group}
      @about-function{g-action-group-list-actions}
      @about-function{g-action-group-query-action}
      @about-function{g-action-group-has-action}
      @about-function{g-action-group-get-action-enabled}
      @about-function{g-action-group-get-action-parameter-type}
      @about-function{g-action-group-get-action-state-type}
      @about-function{g-action-group-get-action-state-hint}
      @about-function{g-action-group-get-action-state}
      @about-function{g-action-group-change-action-state}
      @about-function{g-action-group-activate-action}
      @about-function{g-action-group-action-added}
      @about-function{g-action-group-action-removed}
      @about-function{g-action-group-action-enabled-changed}
      @about-function{g-action-group-action-state-changed}
    @end{subsection}
    @begin[GActionMap]{subsection}
      Interface for action containers.

      @about-class{g-action-map}
      @about-function{g-action-map-lookup-action}
      @about-function{g-action-map-add-action-entries}
      @about-function{g-action-map-add-action}
      @about-function{g-action-map-remove-action}
    @end{subsection}
    @begin[GSimpleActionGroup]{subsection}
      A simple @class{g-action-group} implementation.

      @about-class{g-simple-action-group}
      @about-function{g-simple-action-group-new}
      @about-function{g-simple-action-group-lookup}
      @about-function{g-simple-action-group-insert}
      @about-function{g-simple-action-group-remove}
      @about-function{g-simple-action-group-add-entries}
    @end{subsection}
    @begin[GAction]{subsection}
      An action interface.

      @about-class{g-action}
      @about-generic{g-action-enabled}
      @about-generic{g-action-name}
      @about-generic{g-action-parameter-type}
      @about-generic{g-action-state}
      @about-generic{g-action-state-type}
      @about-function{g-action-name-is-valid}
      @about-function{g-action-get-state-hint}
      @about-function{g-action-change-state}
      @about-function{g-action-activate}
      @about-function{g-action-parse-detailed-name}
      @about-function{g-action-print-detailed-name}
    @end{subsection}
    @begin[GSimpleAction]{subsection}
      A simple GAction implementation.

      @about-class{g-simple-action}
      @about-generic{g-simple-action-enabled}
      @about-generic{g-simple-action-name}
      @about-generic{g-simple-action-parameter-type}
      @about-generic{g-simple-action-state}
      @about-generic{g-simple-action-state-type}
      @about-function{g-simple-action-new}
      @about-function{g-simple-action-new-stateful}
      @about-function{g-simple-action-set-state-hint}
    @end{subsection}
    @begin[GPropertyAction]{subsection}
      A GAction reflecting a GObject property.

      @about-class{g-property-action}
      @about-generic{g-property-action-enabled}
      @about-generic{g-property-action-invert-boolean}
      @about-generic{g-property-action-name}
      @about-generic{g-property-action-object}
      @about-generic{g-property-action-parameter-type}
      @about-generic{g-property-action-property-name}
      @about-generic{g-property-action-state}
      @about-generic{g-property-action-state-type}
      @about-function{g-property-action-new}
    @end{subsection}
    @begin[GMenuModel]{subsection}
      An abstract class representing the contents of a menu.

      @about-class{g-menu-model}
      @about-function{g-menu-model-is-mutable}
      @about-function{g-menu-model-get-n-items}
      @about-variable{+g-menu-attribute-action+}
      @about-variable{+g-menu-attribute-label+}
      @about-variable{+g-menu-attribute-target+}
      @about-variable{+g-menu-link-section+}
      @about-variable{+g-menu-link-submenu+}
      @about-function{g-menu-model-get-item-attribute-value}
      @about-function{g-menu-model-get-item-attribute}
      @about-function{g-menu-model-get-item-link}
      @about-function{g-menu-model-iterate-item-attributes}
      @about-function{g-menu-model-iterate-item-links}
      @about-function{g-menu-model-items-changed}
      @about-symbol{g-menu-attribute-iter}
      @about-function{g-menu-attribute-iter-get-next}
      @about-function{g-menu-attribute-iter-get-name}
      @about-function{g-menu-attribute-iter-get-value}
      @about-function{g-menu-attribute-iter-next}
      @about-function{g-menu-link-iter}
      @about-function{g-menu-link-iter-get-name}
      @about-function{g-menu-link-iter-get-next}
      @about-function{g-menu-link-iter-get-value}
      @about-function{g-menu-link-iter-next}
    @end{subsection}
  @end{section}")

;;; --- End of file gio.package.lisp -------------------------------------------
