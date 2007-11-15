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
;;Routines to generare @code{stexi} documentation for objects and
;;modules.
;;
;;Note that in this context, an @dfn{object} is just a value associated
;;with a location. It has nothing to do with GOOPS.
;;
;;; Code:

(define-module (texinfo reflection)
  #:use-module ((srfi srfi-1)
                #:select (append-map))
  #:use-module (oop goops)
  #:use-module (texinfo)
  #:use-module (texinfo plain-text)
  #:use-module (srfi srfi-13)
  #:use-module (scheme session)
  #:use-module (ice-9 documentation)
  #:use-module (ice-9 optargs)
  #:use-module ((sxml transform)
                #:select (pre-post-order))
  #:export (module-stexi-documentation
            object-stexi-documentation
            package-stexi-standard-copying
            package-stexi-standard-titlepage
            package-stexi-standard-menu
            package-stexi-standard-prologue
            package-stexi-documentation))

;; List for sorting the definitions in a module
(define defs
  '(deftp defcv defivar deftypeivar defop deftypeop defmethod
    deftypemethod defopt defvr defvar deftypevr deftypevar deffn
    deftypefn defmac defspec defun deftypefun))

(define (sort-defs . args)
  (define (priority def)
    (list-index defs (car def)))
  (define (name def)
    (cadr (assq 'name (cdadr def))))
  (let ((priorities (map priority (map cadr args))))
    (cond
     ((not (apply eq? priorities)) (apply < priorities))
     (else (apply string<=? (map name (map cadr args)))))))

(define (list*-join l infix restfix)
  (let lp ((in l) (out '()))
    (cond ((null? in) (reverse! out))
          ((symbol? in) (reverse! (cons* in restfix out)))
          (else (lp (cdr in) (if (null? out)
                                 (list (car in))
                                 (cons* (car in) infix out)))))))

(define (process-args args)
  (map (lambda (x) (if (symbol? x) (symbol->string x) x))
       (list*-join (or args '())
                   " " " . ")))

(define (get-proc-args proc)
  (cond
   ((procedure-property proc 'arglist)
    => (lambda (arglist)
         (let ((required-args (car arglist))
	       (optional-args (cadr arglist))
	       (keyword-args  (caddr arglist))
	       (rest-arg (car (cddddr arglist))))
           (process-args 
            (append 
                    ;; start with the required args...
                    (map symbol->string required-args)

                    ;; add any optional args if needed...
                    (map (lambda (a)
                           (if (list? a)
                               (format #f "[~a = ~s]" (car a) (cadr a))
                               (format #f "[~a]" a)))
                         optional-args)
                    
                    ;; now the keyword args..
                    (map (lambda (a)
                           (if (list? a)
                               (format #f "[#:~a = ~s]" (car a) (cadr a))
                               (format #f "[#:~a]" a)))
                         keyword-args)
                    
                    ;; now the rest arg...
                    (if rest-arg
                        (list "." (symbol->string rest-arg))
                        '()))))))
   (else
    (process-args (and=> (procedure-source proc) cadr)))))

;; like the normal false-if-exception, but doesn't affect the-last-stack
(define-macro (false-if-exception exp)
  `(catch #t
          (lambda ()
            (with-fluids ((the-last-stack (fluid-ref the-last-stack)))
              ,exp))
          (lambda args #f)))

;; This is really nasty, I wish guile gave a better way to get this...
(define (get-macro-args macro)
  (process-args (false-if-exception
                 (local-eval
                  ;; f is the local name given to the procedure we want
                  '(cadr (procedure-source f))
                  (procedure-environment (macro-transformer macro))))))

(define <class>
  (module-ref (resolve-interface '(oop goops)) '<class>)) ;; HACK!

(define many-space? (make-regexp "[[:space:]][[:space:]][[:space:]]"))
(define initial-space? (make-regexp "^[[:space:]]"))
(define (string->stexi str)
  (or (and (or (not str) (string-null? str))
           '(*fragment* (para "[undocumented]")))
      (and (or (string-index str #\@)
               (and (not (regexp-exec many-space? str))
                    (not (regexp-exec initial-space? str))))
           (false-if-exception
            (texi-fragment->stexi str)))
      `(*fragment* (verbatim ,str))))

(define (object-stexi-documentation object . name)
  (if (pair? name)
      (set! name (symbol->string (car name)))
      (set! name "[unknown]"))
  (let ((stexi ((lambda (x)
                  (cond ((string? x) (string->stexi x))
                        ((and (pair? x) (eq? (car x) '*fragment*)) x)
                        (else #f)))
                (object-documentation object))))
    (define (make-def type args)
      `(,type (% ,@args) ,@(cdr stexi)))
    ;; An interesting dilemma here: How to document generic
    ;; functions? Really we are just documenting methods on
    ;; classes, not operations in general. Let's just document
    ;; methods with classes. (TODO)
    (cond
     ((not stexi) #f)
     ;; stexi is now a list, headed by *fragment*.
     ((and (pair? (cdr stexi)) (pair? (cadr stexi))
           (memq (caadr stexi) defs))
      ;; it's already a deffoo.
      stexi)
     ((is-a? object <class>)
      (make-def 'deftp `((name ,name)
                         (category "Class"))))
     ((is-a? object <macro>)
      (make-def 'defspec `((name ,name)
                           (arguments ,@(get-macro-args object)))))
     ((is-a? object <procedure>)
      (make-def 'defun `((name ,name)
                         (arguments ,@(get-proc-args object)))))
     ((is-a? object <generic>)
      (make-def 'defop `((name ,name)
                         (class "object") ;; we need more info here
                         (category "Generic"))))
     (else
      (make-def 'defvar `((name ,name)))))))

(define (module-name->node-name sym-name)
  (string-join (map symbol->string sym-name) " "))

(define (module-stexi-documentation sym-name)
  "Return documentation for the module named @var{sym-name}. The
documentation will be formatted as @code{stexi}
 (@pxref{texinfo,texinfo})."
  (let* ((commentary (and=> (module-commentary sym-name)
                            (lambda (x) (string-trim-both x #\newline))))
         (stexi (string->stexi commentary))
         (node-name (module-name->node-name sym-name))
         (name-str (with-output-to-string
                     (lambda () (display sym-name))))
         (module (resolve-interface sym-name)))
    (define (anchor-name sym)
      (string-append node-name " " (symbol->string sym)))
    (define (make-defs)
      (sort!
       (module-map
        (lambda (sym var)
          `((anchor (% (name ,(anchor-name sym))))
            ,(if (variable-bound? var)
                 (or (object-stexi-documentation (variable-ref var) sym)
                     `(defvar (% (name ,(symbol->string sym)))
                        (para "[undocumented]")))
                 (begin
                   (warn "variable unbound!" sym)
                   `(defvar (% (name ,(symbol->string sym)))
                      "[unbound!]")))))
        module)
       sort-defs))

    `(texinfo (% (title ,name-str))
              (node (% (name ,node-name)))
              (section "Overview")
              ,@(cdr stexi)
              (section "Usage")
              ,@(apply append! (make-defs)))))

(define (stexi-help-handler name value)
  (and=> (object-stexi-documentation value name)
         stexi->plain-text))

(add-value-help-handler! stexi-help-handler)

(define (module-help-handler name)
  (and (list? name)
       (and-map symbol? name)
       (stexi->plain-text (module-stexi-documentation name))))

(add-name-help-handler! module-help-handler)

(define (package-stexi-standard-copying name version updated years
                                        copyright-holder permissions)
  "Create a standard texinfo @code{copying} section.

@var{years} is a list of years (as integers) in which the modules
being documented were released. All other arguments are strings."
  `(copying
    (para "This manual is for " ,name
          " (version " ,version ", updated " ,updated ")")
    (para "Copyright " ,(string-join (map number->string years) ",")
          " " ,copyright-holder)
    (quotation
     (para ,permissions))))

(define (package-stexi-standard-titlepage name version updated authors)
  "Create a standard GNU title page.

@var{authors} is a list of @code{(@var{name} . @var{email})}
pairs. All other arguments are strings.

Here is an example of the usage of this procedure:

@smallexample
(package-stexi-standard-titlepage
 \"Foolib\"
 \"3.2\"
 \"26 September 2006\"
 '((\"Alyssa P Hacker\" . \"alyssa@@example.com\"))
 '(2004 2005 2006)
 \"Free Software Foundation, Inc.\"
 \"Standard GPL permissions blurb goes here\")
@end smallexample
"
  `(;(setchapternewpage (% (all "odd"))) makes manuals too long
    (titlepage
     (title ,name)
     (subtitle "version " ,version ", updated " ,updated)
     ,@(map (lambda (pair)
              `(author ,(car pair)
                       " (" (email ,(cdr pair)) ")"))
            authors)
     (page)
     (vskip (% (all "0pt plus 1filll")))
     (insertcopying))))

(define (package-stexi-standard-menu name modules module-descriptions
                                     extra-entries)
  "Create a standard top node and menu, suitable for processing
by makeinfo."
  (define (make-entry node description)
    `("* " ,node "::"
      ,(make-string (max (- 21 (string-length node)) 2) #\space)
      ,@description "\n"))
  `((ifnottex
     (node (% (name "Top")))
     (top (% (title ,name)))
     (insertcopying)
     (menu
      ,@(apply
         append
         (map
          (lambda (module description)
            (make-entry (module-name->node-name module) description))
          modules module-descriptions))
      ,@(if extra-entries
            (cons "\n" 
                  (apply append (map make-entry
                                     (map car extra-entries)
                                     (map cdr extra-entries))))
            '())))
    (iftex
     (shortcontents))))

(define (package-stexi-standard-prologue name filename category
                                         description copying titlepage
                                         menu)
  "Create a standard prologue, suitable for later serialization
to texinfo and .info creation with makeinfo.

Returns a list of stexinfo forms suitable for passing to
@code{package-stexi-documentation} as the prologue. @xref{texinfo
reflection package-stexi-documentation}, @ref{texinfo reflection
package-stexi-standard-titlepage,package-stexi-standard-titlepage},
@ref{texinfo reflection
package-stexi-standard-copying,package-stexi-standard-copying},
and @ref{texinfo reflection
package-stexi-standard-menu,package-stexi-standard-menu}."
  `(,copying
    (dircategory (% (category ,category)))
    (direntry
     "* " ,name ": (" ,filename ").  " ,description ".")
    ,@titlepage
    ,@menu))

(define (package-stexi-documentation-helper sym-name)
  ;; returns a list of forms
  (pre-post-order
   (module-stexi-documentation sym-name)
   ;; here taking advantage of the fact that we know what
   ;; module-stexi-documentation outputs
   `((texinfo . ,(lambda (tag attrs node . body)
                   `(,node
                     (chapter ,@(assq-ref (cdr attrs) 'title))
                     ,@body)))
     (*text* . ,(lambda (tag text) text))
     (*default* . ,(lambda args args)))))

(define (package-stexi-documentation modules name filename prologue
                                     epilogue)
  "Create stexi documentation for a @dfn{package}, where a
package is a set of modules that is released together.

@var{modules} is expected to be a list of module names, where a
module name is a list of symbols. The stexi that is returned will
be titled @var{name} and a texinfo filename of @var{filename}.

@var{prologue} and @var{epilogue} are lists of stexi forms that
will be spliced into the output document before and after the
generated modules documentation, respectively.
@xref{texinfo reflection package-stexi-standard-prologue}, to
create a conventional GNU texinfo prologue."
  (define (verify-modules-list l)
    (define (all pred l)
      (and (pred (car l))
           (or (null? (cdr l)) (all pred (cdr l)))))
    (false-if-exception
     (all (lambda (x) (all symbol? x)) modules)))
  (if (not (verify-modules-list modules))
      (error "expected modules to be a list of a list of symbols"
             modules))

  `(texinfo
    (% (title ,name)
       (filename ,filename))
    ,@prologue
    ,@(append-map package-stexi-documentation-helper modules)
    ,@epilogue))

;;; arch-tag: bbe2bc03-e16d-4a9e-87b9-55225dc9836c
