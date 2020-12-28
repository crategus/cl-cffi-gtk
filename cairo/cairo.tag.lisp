;;; ----------------------------------------------------------------------------
;;; cairo.tag.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 Dieter Kaiser
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
;;; Tags and Links
;;;
;;;     Hyperlinks and document structure
;;;
;;; Types and Values
;;;
;;;     CAIRO_TAG_DEST
;;;     CAIRO_TAG_LINK
;;;
;;; Functions
;;;
;;;     cairo_tag_begin
;;;     cairo_tag_end
;;;
;;; Description
;;;
;;; The tag functions provide the ability to specify hyperlinks and document
;;; logical structure on supported backends. The following tags are supported:
;;;
;;;     Link - Create a hyperlink
;;;     Destinations - Create a hyperlink destination
;;;     Document Structure Tags - Create PDF Document Structure
;;;
;;; Link Tags
;;;
;;; A hyperlink is specified by enclosing the hyperlink text with the
;;; CAIRO_TAG_LINK tag.
;;;
;;; For example:
;;;
;;; cairo_tag_begin (cr, CAIRO_TAG_LINK, "uri='https://cairographics.org'");
;;; cairo_move_to (cr, 50, 50);
;;; cairo_show_text (cr, "This is a link to the cairo website.");
;;; cairo_tag_end (cr, CAIRO_TAG_LINK);
;;;
;;; The PDF backend uses one or more rectangles to define the clickable area of
;;; the link. By default cairo will use the extents of the drawing operations
;;; enclosed by the begin/end link tags to define the clickable area. In some
;;; cases, such as a link split across two lines, the default rectangle is
;;; undesirable.
;;;
;;;     rect : [optional] The "rect" attribute allows the application to specify
;;;     one or more rectangles that form the clickable region. The value of this
;;;     attribute is an array of floats. Each rectangle is specified by four
;;;     elements in the array: x, y, width, height. The array size must be a
;;;     multiple of four.
;;;
;;; An example of creating a link with user specified clickable region:
;;;
;;; cairo_font_extents_t font_extents;
;;; cairo_text_extents_t text1_extents;
;;; cairo_text_extents_t text2_extents;
;;; char attribs[100];
;;; const char *text1 = "This link is split";
;;; const char *text2 = "across two lines";
;;;
;;; cairo_font_extents (cr, &font_extents);
;;; cairo_move_to (cr, 450, 50);
;;; cairo_text_extents (cr, text1, &text1_extents);
;;; cairo_move_to (cr, 50, 70);
;;; cairo_text_extents (cr, text2, &text2_extents);
;;; sprintf (attribs,
;;;          "rect=[%f %f %f %f %f %f %f %f] uri='https://cairographics.org'",
;;;          text1_extents.x_bearing,
;;;          text1_extents.y_bearing,
;;;          text1_extents.width,
;;;          text1_extents.height,
;;;          text2_extents.x_bearing,
;;;          text2_extents.y_bearing,
;;;          text2_extents.width,
;;;          text2_extents.height);
;;;
;;; cairo_tag_begin (cr, CAIRO_TAG_LINK, attribs);
;;; cairo_show_text (cr, "This is a link to the cairo website");
;;; cairo_move_to (cr, 450, 50);
;;; cairo_show_text (cr, text1);
;;; cairo_move_to (cr, 50, 70);
;;; cairo_show_text (cr, text2);
;;; cairo_tag_end (cr, CAIRO_TAG_LINK);
;;;
;;; There are three types of links. Each type has its own attributes as
;;; detailed below.
;;;
;;;     Internal Links - A link to a location in the same document
;;;     URI Links - A link to a Uniform resource identifier
;;;     File Links - A link to a location in another document
;;;
;;; Internal Links
;;;
;;; An internal link is a link to a location in the same document. The
;;; destination is specified with either:
;;;
;;;     dest : a UTF-8 string specifying the destination in the PDF file to
;;;     link to. Destinations are created with the CAIRO_TAG_DEST tag.
;;;
;;; or the two attributes:
;;;
;;;     page : An integer specifying the page number in the PDF file to link to.
;;;
;;;     pos : [optional] An array of two floats specifying the x,y position on
;;;     the page.
;;;
;;; An example of the link attributes to link to a page and x,y position:
;;;
;;;     "page=3 pos=[3.1 6.2]"
;;;
;;; URI Links
;;;
;;; A URI link is a link to a Uniform Resource Identifier (RFC 2396).
;;;
;;; A URI is specified with the following attribute:
;;;
;;;     uri : An ASCII string specifying the URI.
;;;
;;; An example of the link attributes to the cairo website:
;;;
;;;     "uri='https://cairographics.org'"
;;;
;;; File Links
;;;
;;; A file link is a link a location in another PDF file.
;;;
;;; The file attribute (required) specifies the name of the PDF file:
;;;
;;;     file : File name of PDF file to link to.
;;;
;;; The position is specified by either:
;;;
;;;     dest : a UTF-8 string specifying the named destination in the PDF file.
;;;
;;; or
;;;
;;;     page : An integer specifying the page number in the PDF file.
;;;
;;;     pos : [optional] An array of two floats specifying the x,y position on
;;;     the page. Position coordinates in external files are in PDF coordinates
;;;     (0,0 at bottom left).
;;;
;;; An example of the link attributes to PDF file:
;;;
;;;     "file='document.pdf' page=16 pos=[25 40]"
;;;
;;; Destination Tags
;;;
;;; A destination is specified by enclosing the destination drawing operations
;;; with the CAIRO_TAG_DEST tag.
;;;
;;;     name : [required] A UTF-8 string specifying the name of this
;;;     destination.
;;;
;;;     x : [optional] A float specifying the x coordinate of destination
;;;     position on this page. If not specified the default x coordinate is
;;;     the left side of the extents of the operations enclosed by the
;;;     CAIRO_TAG_DEST begin/end tags. If no operations are enclosed, the x
;;;     coordidate is 0.
;;;
;;;     y : [optional] A float specifying the y coordinate of destination
;;;     position on this page. If not specified the default y coordinate is
;;;     the top of the extents of the operations enclosed by the
;;;     CAIRO_TAG_DEST begin/end tags. If no operations are enclosed, the y
;;;     coordidate is 0.
;;;
;;;     internal : A boolean that if true, the destination name may be omitted
;;;     from PDF where possible. In this case, links refer directly to the page
;;;     and position instead of via the named destination table. Note that if
;;;     this destination is referenced by another PDF (see File Links), this
;;;     attribute must be false. Default is false.
;;;
;;; /* Create a hyperlink */
;;; cairo_tag_begin (cr, CAIRO_TAG_LINK, "dest='mydest' internal");
;;; cairo_move_to (cr, 50, 50);
;;; cairo_show_text (cr, "This is a hyperlink.");
;;; cairo_tag_end (cr, CAIRO_TAG_LINK);
;;;
;;; /* Create a destination */
;;; cairo_tag_begin (cr, CAIRO_TAG_DEST, "name='mydest'");
;;; cairo_move_to (cr, 50, 250);
;;; cairo_show_text (cr, "This paragraph is the destination of the above link.");
;;; cairo_tag_end (cr, CAIRO_TAG_DEST);
;;;
;;; Document Structure (PDF)
;;;
;;; The document structure tags provide a means of specifying structural
;;; information such as headers, paragraphs, tables, and figures. The inclusion
;;; of structural information facilitates:
;;;
;;;     Extraction of text and graphics for copy and paste
;;;     Reflow of text and graphics in the viewer
;;;     Processing text eg searching and indexing
;;;     Conversion to other formats
;;;     Accessability support
;;;
;;; The list of structure types is specified in section 14.8.4 of the PDF
;;; Reference.
;;;
;;; Note the PDF "Link" structure tag is the same as the cairo CAIRO_TAG_LINK
;;; tag.
;;;
;;; The following example creates a document structure for a document containing
;;; two section, each with a header and a paragraph.
;;;
;;; cairo_tag_begin (cr, "Document", NULL);
;;;
;;; cairo_tag_begin (cr, "Sect", NULL);
;;; cairo_tag_begin (cr, "H1", NULL);
;;; cairo_show_text (cr, "Heading 1");
;;; cairo_tag_end (cr, "H1");
;;;
;;; cairo_tag_begin (cr, "P", NULL);
;;; cairo_show_text (cr, "Paragraph 1");
;;; cairo_tag_end (cr, "P");
;;; cairo_tag_end (cr, "Sect");
;;;
;;; cairo_tag_begin (cr, "Sect", NULL);
;;; cairo_tag_begin (cr, "H1", NULL);
;;; cairo_show_text (cr, "Heading 2");
;;; cairo_tag_end (cr, "H1");
;;;
;;; cairo_tag_begin (cr, "P", NULL);
;;; cairo_show_text (cr, "Paragraph 2");
;;; cairo_tag_end (cr, "P");
;;; cairo_tag_end (cr, "Sect");
;;;
;;; cairo_tag_end (cr, "Document");
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_TAG_DEST
;;;
;;; #define CAIRO_TAG_DEST "cairo.dest"
;;;
;;; Create a destination for a hyperlink. Destination tag attributes are
;;; detailed at Destinations.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_TAG_LINK
;;;
;;; #define CAIRO_TAG_LINK "Link"
;;;
;;; Create hyperlink. Link tag attributes are detailed at Links.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_tag_begin ()
;;;
;;; void
;;; cairo_tag_begin (cairo_t *cr,
;;;                  const char *tag_name,
;;;                  const char *attributes);
;;;
;;; Marks the beginning of the tag_name structure. Call cairo_tag_end() with
;;; the same tag_name to mark the end of the structure.
;;;
;;; The attributes string is of the form "key1=value2 key2=value2 ...". Values
;;; may be boolean (true/false or 1/0), integer, float, string, or an array.
;;;
;;; String values are enclosed in single quotes ('). Single quotes and
;;; backslashes inside the string should be escaped with a backslash.
;;;
;;; Boolean values may be set to true by only specifying the key. eg the
;;; attribute string "key" is the equivalent to "key=true".
;;;
;;; Arrays are enclosed in '[]'. eg "rect=[1.2 4.3 2.0 3.0]".
;;;
;;; If no attributes are required, attributes can be an empty string or NULL.
;;;
;;; See Tags and Links Description for the list of tags and attributes.
;;;
;;; Invalid nesting of tags or invalid attributes will cause cr to shutdown
;;; with a status of CAIRO_STATUS_TAG_ERROR.
;;;
;;; See cairo_tag_end().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; tag_name :
;;;     tag name
;;;
;;; attributes :
;;;     tag attributes
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;cairo_tag_end ()
;;;
;;; void
;;; cairo_tag_end (cairo_t *cr, const char *tag_name);
;;;
;;; Marks the end of the tag_name structure.
;;;
;;; Invalid nesting of tags will cause cr to shutdown with a status of
;;; CAIRO_STATUS_TAG_ERROR.
;;;
;;; See cairo_tag_begin().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; tag_name :
;;;     tag name
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.tag.lisp ---------------------------------------------
