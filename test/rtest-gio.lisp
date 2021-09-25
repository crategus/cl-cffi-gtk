(in-package :gtk-testsuite)

(def-suite gio-suite :in gtk-testsuite)
(in-suite gio-suite)

;;; File Operations

(load "rtest-gio-file.lisp")

;;; File types and applications

(load "rtest-gio-content-type.lisp")
;  gio.app-info.lisp
;  not implemented              - GDesktopAppInfo

;;; Icons

(load "rtest-gio-icon.lisp")
;  not implemented              - GFileIcon
;  not implemented              - GLoadableIcon

(load "rtest-gio-themed-icon.lisp")
;  gio.emblemed-icon.lisp
;  gio.emblem.lisp

;;; Resources

(load "rtest-gio-resource.lisp")

;;; Application support

(load "rtest-gio-application.lisp")
(load "rtest-gio-application-command-line.lisp")
(load "rtest-gio-action-group.lisp")
(load "rtest-gio-action-map.lisp")
(load "rtest-gio-simple-action-group.lisp")
(load "rtest-gio-action.lisp")
(load "rtest-gio-simple-action.lisp")
(load "rtest-gio-property-action.lisp")
;  not implemented              - GRemoteActionGroup
;  not implemented              - GActionGroup exporter
;  not implemented              - GDBusActionGroup
;  not implemented              - GMemoryMonitor
;  gio.menu-model-lisp          - GMenuModel
(load "rtest-gio-menu.lisp")
;  not implemented              - GMenuModel exporter
;  not implemented              - GDBusMenuModel
;  not implemented              - GNotification

;;; 2021-9-8
