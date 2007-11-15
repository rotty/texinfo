;; SPE structure texinfo.serialize
;; Copyright (C) 2005 Andy Wingo <wingo at pobox dot com>
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
;;Serialization of @code{stexi} to plain texinfo.
;;
;;; Code:

(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
      (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
        (if (null? l) (reverse dest)
            (loop (cdr l) (cons (car l) (cons elem dest)))))))

;; converts improper lists to proper lists.
(define (filter* pred l)
  (let lp ((in l) (out '()))
    (cond ((null? in)
           (reverse! out))
          ((pair? in)
           (lp (cdr in) (if (pred (car in)) (cons (car in) out) out)))
          (else
           (lp '() (if (pred in) (cons in out) out))))))

;; (list* 'a '(b c) 'd '(e f g)) => '(a b c d e f g)
(define (list* . args)
  (let* ((args (reverse args))
         (tail (car args)))
    (let lp ((in (cdr args)) (out tail))
      (cond ((null? in) out)
            ((pair? (car in)) (lp (cdr in) (append (car in) out)))
            ((null? (car in)) (lp (cdr in) out))
            (else (lp (cdr in) (cons (car in) out)))))))

(define (empty-command exp lp command type formals args accum)
  (list* " " command "@" accum))

(define (inline-text exp lp command type formals args accum)
  (list* "}"
         (append-map (lambda (x) (lp x '())) (reverse (cdr exp)))
         "{" command "@" accum))

(define (inline-args exp lp command type formals args accum)
  (list* "}"
         (if (not args) ""
             (list-intersperse
              (map
               (lambda (x)
                 (cond ((not x) "")
                       ((pair? x)
                        (if (pair? (cdr x))
                            (texinfo-warn
                             (format #f "Strange inline-args: ~s!" args)))
                        (car x))
                       (else (error "Invalid inline-args" args))))
               (drop-while not
                           (map (lambda (x) (assq-ref args x))
                                (reverse formals))))
              ","))
         "{" command "@" accum))

(define nl (string #\newline))

(define (eol-text exp lp command type formals args accum)
  (list* nl
         (append-map (lambda (x) (lp x '()))
                     (reverse (if args (cddr exp) (cdr exp))))
         (list-intersperse
          (append-map (lambda (x) (lp (assq-ref args x) '()))
                      (reverse formals))
          " ")
         " " command "@" accum))

(define (eol-args exp lp command type formals args accum)
  (list* nl
         (list-intersperse
          (apply append
                 (drop-while not
                             (map (lambda (x) (assq-ref args x))
                                  (reverse formals))))
          " ")
         " " command "@" accum))

(define (environ exp lp command type formals args accum)
  (case (car exp)
    ((texinfo)
     (list* "@bye" nl
            (append-map (lambda (x) (lp x '())) (reverse (cddr exp)))
            nl "@c %**end of header" nl nl
            (assq-ref args 'title) "@settitle "
            "\\input texinfo   @c -*-texinfo-*-" nl "@c %**start of header" nl
            accum))
    (else
     (list* nl nl command nl "@end "
            (append-map (lambda (x) (lp x '()))
                        (reverse (if args (cddr exp) (cdr exp))))
            nl
            (list-intersperse
             (append-map (lambda (x) (lp x '()))
                         (apply append
                                (map
                                 reverse
                                 (drop-while
                                  not (map (lambda (x) (assq-ref args x))
                                           (reverse formals))))))
             " ")
            " " command "@" accum))))

(define (table-environ exp lp command type formals args accum)
  (list* nl nl command "@end "
         (append-map (lambda (x) (lp x '()))
                     (reverse (if args (cddr exp) (cdr exp))))
         nl
         (let* ((arg (if args (cadar args) ""))) ;; zero or one args
           (if (pair? arg)
               (list (symbol->string (car arg)) "@")
               arg))
         " " command "@" accum))

(define (wrap strings)
  (fill-string (string-concatenate strings) '((width . 72))))

(define (paragraph exp lp command type formals args accum)
  (list* nl nl
         (wrap
          (reverse
           (append-map (lambda (x) (lp x '())) (reverse (cdr exp)))))
         accum))

(define (item exp lp command type formals args accum)
  (list* (append-map (lambda (x) (lp x '())) (reverse (cdr exp)))
         "@item" nl
         accum))

(define (entry exp lp command type formals args accum)
  (list* (append-map (lambda (x) (lp x '())) (reverse (cddr exp)))
         nl
         (append-map (lambda (x) (lp x '())) (reverse (cdar args)))
         "@item "
         accum))

(define serializers
  `((EMPTY-COMMAND . ,empty-command)
    (INLINE-TEXT . ,inline-text)
    (INLINE-ARGS . ,inline-args)
    (EOL-TEXT . ,eol-text)
    (INDEX . ,eol-text)
    (EOL-ARGS . ,eol-args)
    (ENVIRON . ,environ)
    (TABLE-ENVIRON . ,table-environ)
    (ENTRY . ,entry)
    (ITEM . ,item)
    (PARAGRAPH . ,paragraph)))

(define (serialize exp lp command type formals args accum)
  ((or (assq-ref serializers type)
       (error "Unknown command type" exp type))
   exp lp command type formals args accum))

(define escaped-chars '(#\} #\{ #\@))
;; Escapes any illegal texinfo characters (currently @{, @}, and @@)."
(define (escape str)
  (let loop ((in (string->list str)) (out '()))
    (if (null? in)
        (apply string (reverse out))
        (if (memq (car in) escaped-chars)
            (loop (cdr in) (cons* (car in) #\@ out))
            (loop (cdr in) (cons (car in) out))))))

;;@ Serialize the stexi @var{tree} into plain texinfo."
(define (stexi->texi tree)
  (string-concatenate-reverse
   (let lp ((in tree) (out '()))
     (cond
      ((null? in) out)
      ((string? in) (cons (escape in) out))
      ((pair? in)
       (let ((command-spec (assq (car in) texi-command-specs)))
         (if (not command-spec)
             (begin
               (texinfo-warn (format #f "Unknown stexi command ~s, not rendering" in))
               out)
             (serialize in
                        lp
                        (symbol->string (car in))
                        (cadr command-spec)
                        (filter* symbol? (cddr command-spec))
                        (and (pair? (cdr in)) (pair? (cadr in))
                             (eq? (caadr in) '%) (cdadr in))
                        out))))
      (else
       (error "Invalid stexi" in))))))

;;; arch-tag: d3fa16ea-0bf7-4ec5-ab9f-3f08490f77f5
