(in-package :gtk-testsuite)

(def-suite pango-suite :in gtk-testsuite)
(in-suite pango-suite)

;;; Basic Pango Interfaces

; pango.rendering.lisp - Functions to run the rendering pipeline  - Pango 1.48 %

(load "rtest-pango-fonts.lisp")

; pango.glyph.lisp - Storing information about glyphs             - Pango 1.48 %
; pango.attributes.lisp - Attributes for annotating text          - Pango 1.48 %
; pango.markup.lisp - Markup language for text with attributes    - Pango 1.48 %

(load "rtest-pango-layout.lisp")

; pango.script.lisp - Scripts and Languages                       - Pango 1.48 %
; pango.biderectional.lisp - Handling bidirectional text          - Pango 1.48 %

(load "rtest-pango-vertical-text.lisp")

;;; Rendering with Pango

(load "rtest-pango-cairo-rendering.lisp")

; Win32 Fonts and Rendering — Font handling and rendering on Windows
; CoreText Fonts and Rendering — Font handling and rendering on OS X
; FreeType Fonts and Rendering — Font handling and rendering with FreeType
; Xft Fonts and Rendering — Font handling and rendering with the Xft backend

;;; Low Level Functionality

(load "rtest-pango-context.lisp")

; pango.tab-array.lisp - Structures for storing tab stops         - Pango 1.48 %
; pango.coverage.lisp - Unicode character range coverage storage  - Pango 1.48 %
; pango.renderer.lisp - Rendering driver base class               - Pango 1.48 %

; PangoFcFontMap — Base fontmap class for Fontconfig-based backends
; PangoFcFont — Base font class for Fontconfig-based backends
; PangoFcDecoder — Custom font encoding handling
; Miscellaneous Utilities — Various convenience and utility functions

; pango.version.lisp -Version Checking                            - Pango 1.48

;;; Deprecated APIs

; OpenType Font Handling — Obtaining information from OpenType tables
; Engines — Language-specific and rendering-system-specific processing
; PangoEngineLang — Rendering-system independent script engines
; PangoEngineShape — Rendering-system dependent script engines
; Modules — Support for loadable modules

;;; 2021-1-2
