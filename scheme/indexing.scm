;; guile-lib
;; Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>

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
;;@c texinfo formatting
;;Given a piece of stexi, return an index of a specified variety.
;;
;;Note that currently, @code{stexi-extract-index} doesn't differentiate
;;between different kinds of index entries. That's a bug ;)
;;; Code:

(define-module (texinfo indexing)
  #:use-module (sxml simple)
  #:use-module (scheme documentation)
  #:use-module (srfi srfi-13)
  #:export (stexi-extract-index))

(define (def-name def)
  (cadr (assq 'name (cdadr def))))

(define defines
  '(deftp defcv defivar deftypeivar defop deftypeop defmethod
    deftypemethod defopt defvr defvar deftypevr deftypevar deffn
    deftypefn defspec defmac defun deftypefun))

(define indices
  '(cindex findex vindex kindex pindex tindex))

(define (stexi-extract-index tree manual-name kind)
  "Given an stexi tree @var{tree}, index all of the entries of type
@var{kind}. @var{kind} can be one of the predefined texinfo indices
(@code{concept}, @code{variable}, @code{function}, @code{key},
@code{program}, @code{type}) or one of the special symbols @code{auto} 
or @code{all}. @code{auto} will scan the stext for a @code{(printindex)}
statement, and @code{all} will generate an index from all entries,
regardless of type.

The returned index is a list of pairs, the @sc{car} of which is the
entry (a string) and the @sc{cdr} of which is a node name (a string)."
  (let loop ((in tree) (entries '()))
    (cond
     ((null? in)
      entries)
     ((pair? (car in))
      (cond
       ((and (pair? (cdr in)) (pair? (cadr in))
             (eq? (caar in) 'anchor) (memq (caadr in) defines))
        (loop (cddr in) (acons (cadr (assq 'name (cdr (cadadr in))))
                               (cadr (assq 'name (cdadar in)))
                               entries)))
       ((and (pair? (cdr in)) (pair? (cadr in))
             (eq? (caar in) 'anchor) (memq (caadr in) indices))
        (loop (cddr in) (acons (sxml->string (cadr in))
                               (cadr (assq 'name (cdadar in)))
                               entries)))
       (else
        (loop (cdr in) (loop (car in) entries)))))
     (else
      (loop (cdr in) entries)))))

;;; arch-tag: 216d29d3-1ed9-433f-9c19-0dc4d6b439b6
