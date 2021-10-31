(in-package :gtk-testsuite)

(def-suite cairo-suite :in gtk-testsuite)
(in-suite cairo-suite)

;;; Drawing

(load "rtest-cairo-context.lisp")
(load "rtest-cairo-path.lisp")
(load "rtest-cairo-pattern.lisp")

; cairo.regionslisp - Representing a pixel-aligned area
; cairo.transformationslisp - Transformations

(load "rtest-cairo-text.lisp")

; Raster Sources - Supplying arbitrary image data
; Tags and Links — Hyperlinks and document structure

;;; Fonts

; cairo.font-face.lisp - Base class for font faces

(load "rtest-cairo-scaled-font.lisp")
(load "rtest-cairo-font-options.lisp")

; FreeType Fonts - Font support for FreeType
; Win32 Fonts - Font support for Microsoft Windows
; Quartz Fonts - Font support via CGFont on OS X
; User Fonts - Font support with font data provided by the user

;;; Surfaces

; cairo.device.lisp - interface to underlying rendering system
; cairo.surface.lisp - Base class for surfaces

(load "rtest-cairo-image-surface.lisp")

; cairo-pdf-surface.lisp - Rendering PDF documents
; cairo.png-surface.lisp - Reading and writing PNG images

; PostScript Surfaces - Rendering PostScript documents
; Recording Surfaces - Records all drawing operations
; Win32 Surfaces - Microsoft Windows surface support
; SVG Surfaces - Rendering SVG documents
; Quartz Surfaces - Rendering to Quartz surfaces
; XCB Surfaces - X Window System rendering using the XCB library
; XLib Surfaces - X Window System rendering using XLib
; XLib-XRender Backend - X Window System rendering using the X Render extension
; Script Surfaces - Rendering to replayable scripts

;;; Utilities

(load "rtest-cairo-matrix.lisp")
(load "rtest-cairo-status.lisp")

; cairo.version.lisp - Compile-time and run-time version checks
; cairo.types.lisp - Generic data types

;;; 2020-12-25
