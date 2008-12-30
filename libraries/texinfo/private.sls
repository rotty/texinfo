;;; internals.sls --- library providing the texinfo internals

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This mainly intended for the testsuite, and not for public
;; consumption.

;;; Code:
#!r6rs

(library (texinfo private)
  (export
   ;; Public, reexported by (texinfo)
   texi-command-specs
   texi-command-depth
   texi-fragment->stexi
   texi->stexi
   stexi->sxml
   
   ;; Private, for the test suite
   read-verbatim-body
   read-arguments
   complete-start-command
   read-char-data
   )
  (import (except (rnrs base) error string-copy string-for-each string->list)
          (rnrs conditions)
          (rnrs exceptions)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs lists)
          (except (rnrs unicode) string-titlecase string-upcase string-downcase)
          (xitomatl srfi let-values)
          (prefix (xitomatl strings) x:)
          (spells lists)
          (spells strings)
          (spells char-set)
          (only (spells error) make-error-signaller)
          (spells filesys)
          (spells alist)
          (spells misc)
          (spells condition)
          (spells include)
          (spells tracing)
          (sxml simple)
          (sxml transform)
          (sxml ssax input-parse)
          (sxml ssax util)
          )

  (define ascii->char integer->char)
  (define open-input-string open-string-input-port)

  (define error (make-error-signaller "texinfo library"))

  (define (string-split s c)
    (x:string-split s (string c) #t))
  
  (define-syntax and=>
    (syntax-rules ()
      ((and=> expr receiver)
       (cond (expr => receiver)
             (else #f)))))
  
  (include-file ((texinfo scheme) stexi))

  )
