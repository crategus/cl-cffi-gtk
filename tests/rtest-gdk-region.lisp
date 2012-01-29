;;; ----------------------------------------------------------------------------
;;; rtest-gdk-region.lisp
;;;
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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

(in-package :gdk-tests)

(define-test gdk-point
  (let ((point (make-gdk-point :x 1 :y 2)))
    (assert-eql 'gdk-point (type-of point))  
    (assert-eql 1 (gdk-point-x point))
    (assert-eql 2 (gdk-point-y point))))

(define-test gdk-rectangle
  (let* ((rect1 (make-gdk-rectangle :width 20 :height 10))
         (rect2 (make-gdk-rectangle :x 10 :y 5 :width 20 :height 10))
         (sect  (gdk-rectangle-intersect rect1 rect2))
         (union (gdk-rectangle-union rect1 rect2)))
    
    (assert-eql 'gdk-rectangle (type-of rect1))
    (assert-eql 'gdk-rectangle (type-of rect2))
    (assert-eql 'gdk-rectangle (type-of sect))
    (assert-eql 'gdk-rectangle (type-of union))
    
    (assert-eql  0 (gdk-rectangle-x rect1))
    (assert-eql  0 (gdk-rectangle-y rect1))
    (assert-eql 20 (gdk-rectangle-width rect1))
    (assert-eql 10 (gdk-rectangle-height rect1))
    
    (assert-eql 10 (gdk-rectangle-x sect))
    (assert-eql  5 (gdk-rectangle-y sect))
    (assert-eql 10 (gdk-rectangle-width sect))
    (assert-eql  5 (gdk-rectangle-height sect))
    
    (assert-eql  0 (gdk-rectangle-x union))
    (assert-eql  0 (gdk-rectangle-y union))
    (assert-eql 30 (gdk-rectangle-width union))
    (assert-eql 15 (gdk-rectangle-height union))))

(define-test gdk-region
  (let* ((rect nil)
         (rect1   (make-gdk-rectangle :width 20 :height 10))
         (rect2   (make-gdk-rectangle :x 10 :y 5 :width 20 :height 10))
         (region1 (gdk-region-new))
         (region2 (gdk-region-rectangle rect1))
         (clipbox (gdk-region-get-clipbox region2)))
    
    (assert-eql 'gdk-region (type-of region1))
    (assert-eql 'gdk-region (type-of region2))
    (assert-eql 'gdk-rectangle (type-of clipbox))
    
    (assert-true   (gdk-region-is-empty region1))
    (assert-false  (gdk-region-is-empty region2))
    
    (assert-true   (gdk-region-equal region1 region1))
    (assert-true   (gdk-region-equal region2 region2))
    (assert-false  (gdk-region-equal region1 region2))
    
    (assert-false  (gdk-region-point-in region1 10 5))
    (assert-true   (gdk-region-point-in region2 10 5))
    (assert-false  (gdk-region-point-in region2 25 25))
    
    (assert-eql  0 (gdk-rectangle-x clipbox))
    (assert-eql  0 (gdk-rectangle-y clipbox))
    (assert-eql 20 (gdk-rectangle-width clipbox))
    (assert-eql 10 (gdk-rectangle-height clipbox))
    
    (setq rect (first (gdk-region-get-rectangles region2)))
    (assert-true (gdk-region-equal region2 (gdk-region-rectangle rect1)))
    
    (assert-eq :in (gdk-region-rect-in region2 rect1))
    (gdk-region-offset region2 10 5)
    (assert-eq :part (gdk-region-rect-in region2 rect1))
    (gdk-region-offset region2 15 10)
    (assert-eq :out (gdk-region-rect-in region2 rect1))
    
    (gdk-region-union-with-rect region2 rect1)
    (assert-eq :in (gdk-region-rect-in region2 rect1))
    
    (gdk-region-intersect (setq region1 (gdk-region-rectangle rect1))
                          (setq region2 (gdk-region-rectangle rect2)))
    (assert-eq :part (gdk-region-rect-in region1 rect1))
    (assert-eq :part (gdk-region-rect-in region1 rect2))
    (assert-eq :part (gdk-region-rect-in region2 rect1))
    (assert-eq :in   (gdk-region-rect-in region2 rect2)) ; ???
    
    (gdk-region-union (setq region1 (gdk-region-rectangle rect1))
                      (setq region2 (gdk-region-rectangle rect2)))
    (assert-eq :in   (gdk-region-rect-in region1 rect1))
    (assert-eq :in   (gdk-region-rect-in region1 rect2))
    (assert-eq :part (gdk-region-rect-in region2 rect1)) ; ???
    (assert-eq :in   (gdk-region-rect-in region2 rect2))
    
    (gdk-region-subtract (setq region1 (gdk-region-rectangle rect1))
                         (setq region2 (gdk-region-rectangle rect2)))
    (assert-eq :part (gdk-region-rect-in region1 rect1))
    (assert-eq :out   (gdk-region-rect-in region1 rect2))
    (assert-eq :part (gdk-region-rect-in region2 rect1)) ; ???
    (assert-eq :in   (gdk-region-rect-in region2 rect2))
    
    (gdk-region-xor (setq region1 (gdk-region-rectangle rect1))
                    (setq region2 (gdk-region-rectangle rect2)))
    (assert-eq :part (gdk-region-rect-in region1 rect1))
    (assert-eq :part  (gdk-region-rect-in region1 rect2))
    (assert-eq :part (gdk-region-rect-in region2 rect1)) ; ???
    (assert-eq :in   (gdk-region-rect-in region2 rect2))
    ))

;;; --- End of file rtest-gdk-region.lisp --------------------------------------
