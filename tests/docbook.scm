;; guile-lib
;; Copyright (C) 2007 Andy Wingo <wingo at pobox dot com>

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; Unit tests for (texinfo docbook).
;;
;;; Code:

(use-modules (oop goops)
             (unit-test)
             (texinfo docbook))

(define-class <test-texinfo-docbook> (<test-case>))

(define-method (test-flatten (self <test-texinfo-docbook>))
  (assert-equal
   (sdocbook-flatten '(refsect1 (refsect2 (para "foo"))))
   '((refsect1) (refsect2) (para "foo")))
  )

(exit-with-summary (run-all-defined-test-cases))
