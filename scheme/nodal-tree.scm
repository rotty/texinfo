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
;;This module exports a procedure to chunk a stexi doument into pieces,
;;delimited by sectioning commands (@code{@@chapter},
;;@code{@@appendixsec}, etc.). Note that the sectioning commands must be
;;preceded by a @code{@@node}, a condition that the output of
;;@code{(sxml texinfo)} respects.
;;
;;The output is a nodal tree (see (container nodal-tree)), with the
;;following fields defined for each node:
;;
;;; Code:

(define-module (texinfo nodal-tree)
  #:use-module (container nodal-tree)
  #:use-module (sxml simple)
  #:use-module (texinfo) ;; texi-command-depth
  #:export (stexi->nodal-tree))

(define (node? elt)
  (and (pair? elt) (eq? (car elt) 'node)))
(define (chunking-section? elt max-depth)
  (and (pair? elt) (texi-command-depth (car elt) max-depth)))

(define (append-child! parent kid)
  (if parent
      (node-set! parent 'children
                 (append! (node-ref parent 'children) (list kid)))))

(define (find-parent node)
  (or (and=> (node-ref node 'parent) find-parent) node))

;; There has to be a more functional way to do this! Probably involves
;; building up from the leaves, instead of building down from the root.
;; Thankfully, the ugliness of this code isn't exported.
(define (stexi->nodal-tree stexi max-depth)
  "Break @var{stexi} into a nodal tree. Only break until sectioning
identifiers of depth @var{max-depth}. The following fields are defined
for each node:

@table @code
@item name
The name of the section.
@item value
The content of the section, as @code{stexi}. The containing element is
@code{texinfo}.
@item parent
A reference to the parent node.
@item children
A list of subnodes, corresponding to the subsections of the current
section.
@end table"
  (define (make-node* parent tree-title)
    (let ((node (make-node
                 'name (sxml->string tree-title)
                 'value #f
                 'parent parent
                 'children '())))
      (append-child! parent node)
      node))

  (or (eq? (car stexi) 'texinfo) (error "Invalid stexi"))

  (let lp ((in stexi)
           (val '())
           (node (make-node* #f (cadr stexi)))
           (parent #f)
           (depth 0))
    (cond
     ((null? in)
      (node-set! node 'value (reverse! val))
      (find-parent node))
     ((and (node? (car in)) (pair? in) (pair? (cdr in))
           (chunking-section? (cadr in) max-depth))
      (node-set! node 'value (reverse! val))
      (let ((new-depth (texi-command-depth (caadr in) max-depth)))
        (let new-parent ((parent node) (diff (- new-depth depth)))
          (cond
             ((not parent) (error "invalid stexi"))
             ((positive? diff)
              (or (eq? diff 1)
                  (error "can only descend by one depth level at a time"
                         (car in) (cadr in)))
              (lp (cddr in)
                  `(,(cadr in) ,(car in)
                    (% (title ,(sxml->string (cadr in)))) texinfo)
                  (make-node* parent (cadr in)) parent new-depth))
             (else
              (new-parent (node-ref parent 'parent) (1+ diff)))))))
     (else
      (lp (cdr in) (cons (car in) val) node parent depth)))))

;;; arch-tag: aff19153-493d-4755-ba6f-22cc7fb43c60
