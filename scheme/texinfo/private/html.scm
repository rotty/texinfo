;; (texinfo html) -- translating stexinfo into shtml
;; Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;This module implements transformation from @code{stexi} to HTML. Note
;;that the output of @code{stexi->shtml} is actually SXML with the HTML
;;vocabulary. This means that the output can be further processed, and
;;that it must eventually be serialized by
;;@ref{sxml simple sxml->xml,sxml->xml}.
;;        
;;References (i.e., the @code{@@ref} family of commands) are resolved by
;;a @dfn{ref-resolver}.
;;@xref{texinfo html add-ref-resolver!,add-ref-resolver!}, for more
;;information.
;;
;;; Code:

;; TODO: nice ref resolving API, default CSS stylesheet (esp. to remove
;; margin-top on dd > p)

;; The caller is responsible for carring the returned list.
(define (arg-ref key %-args)
  (cond ((assq key (cdr %-args)) => (lambda (x) (stexi->shtml (cdr x))))
        (else #f)))
(define (arg-req key %-args)
  (or (arg-ref key %-args)
      (error "Missing argument:" key %-args)))
(define (car* x) (and x (car x)))

(define (urlify str)
  (string-downcase
   (string-map
    (lambda (c)
      (case c
        ((#\space #\/ #\:) #\-)
        (else c)))
    str)))

;;@ The list of ref-resolvers. Each element is expected to take the name
;; of a node and the name of a manual and return the URL of the referent,
;; or @code{#f} to pass control to the next ref-resolver in the list.
;;
;; The default ref-resolver will return the concatenation of the manual
;; name, @code{#}, and the node name.
(define stexi-ref-resolvers
  (make-parameter
   (list
    (lambda (node-name manual-name) ;; the default
      (urlify (string-append (or manual-name "") "#" node-name))))))

(define (resolve-ref node manual)
  (or (or-map (lambda (x) (x node manual)) (stexi-ref-resolvers))
      (error "Could not resolve reference" node manual)))

(define (ref tag args)
  (let* ((node (car (arg-req 'node args)))
         (section (or (car* (arg-ref 'section args)) node))
         (manual (car* (arg-ref 'manual args)))
         (target (resolve-ref node manual)))
    `(span ,(and=> (assq tag '((xref "See ") (pxref "see "))) cdr)
           (a (^ (href ,target)) ,section))))

(define (uref tag args)
  (let ((url (car (arg-req 'url args))))
    `(a (^ (href ,url)) ,(or (car* (arg-ref 'title args)) url))))

;; @!*&%( Mozilla gets confused at an empty ("<a .. />") a tag. Put an
;; empty string here to placate the reptile.
(define (node tag args)
  `(a (^ (name ,(urlify (car (arg-req 'name args))))) ""))

(define (def-category tag args)
  (case tag
    ((defun defunx) "Function")
    ((defspec defspecx) "Special Form")
    ((defvar defvarx) "Variable")
    (else (car (arg-req 'category args)))))

(define (def-tr tag args)
  (define (code x) (and x (cons 'code x)))
  (define (var x) (and x (cons 'var x)))
  (define (b x) (and x (cons 'b x)))
  (define (list/spaces . elts)
    (let lp ((in elts) (out '()))
      (cond ((null? in) (reverse! out))
            ((null? (car in)) (lp (cdr in) out))
            (else (lp (cdr in)
                      (cons (car in)
                            (if (null? out) out (cons " " out))))))))
  (define (arguments)
    (let ((arguments (arg-ref 'arguments args)))
      (and arguments (apply list/spaces arguments))))
  (define (left-td-contents)
    (list/spaces (code (arg-ref 'data-type args))
                 (b (list (code (arg-ref 'class args)))) ;; is this right?
                 (b (list (code (arg-ref 'name args))))
                 (if (memq tag '(deftypeop deftypefn deftypefun))
                     (code (arguments))
                     (var (list (code (arguments)))))))
  `(tr (td ,@(left-td-contents))
       (td (div (^ (class "right")) "[" ,(def-category tag args) "]"))))

(define (def tag args . body)
  `(div
    (table
     (^ (cellpadding "0") (cellspacing "0") (width "100%") (class "def"))
     ,(def-tr tag args)
     ,@(filter-map (lambda (sub)
                     (and (procedure? sub) (sub)))
                   body))
    (div (^ (class "description"))
         ,@(filter (lambda (sub)
                     (not (procedure? sub)))
                   body))))

(define (defx tag args)
  (lambda ()
    (def-tr tag args)))

(define (bullet->attrs bullet)
  (if (and (pair? bullet)
           (null? (cdr bullet)))
      (case (car bullet)
        ((bullet) '((type "disk")))
        ((minus)  '((type "square"))) ;; well, at least it's different
        (else     '()))))

(define (enumerate tag . elts)
  (define (tonumber start)
    (let ((c (ascii-lowercase (char->ascii (string-ref start 0)))))
      (+ 1 (- c (char->ascii (if (ascii-upper? c) #\A #\a))))))
  `(ol ,@(if (and (pair? elts) (pair? (car elts)) (eq? (caar elts) '%))
             (cons `(^ (start ,@(tonumber (arg-req 'start (car elts)))))
                       ;; (type ,(type (arg-ref 'start (car elts)))))
                   elts))))

(define (itemize tag . elts)
  `(ul ,@(if (and (pair? elts) (pair? (car elts)) (eq? (caar elts) '%))
             (cons `(^ ,@(bullet->attrs (assq-ref (cdar elts) 'bullet)))
                   (cdr elts))
             elts)))

(define (table tag args . body)
  (let ((formatter (caar (arg-req 'formatter args))))
    (cons 'dl
          (map (lambda (x)
                 (cond ((and (pair? x) (eq? (car x) 'dt))
                        (list (car x) (cons formatter (cdr x))))
                       (else x)))
               (apply append body)))))

(define (entry tag args . body)
  `((dt ,@(arg-req 'heading args))
    (dd ,@body)))

(define tag-replacements
  '((titlepage    div (^ (class "titlepage")))
    (title        h2  (^ (class "title")))
    (subtitle     h3  (^ (class "subtitle")))
    (author       h3  (^ (class "author")))
    (example      pre)
    (lisp         pre)
    (smallexample pre (^ (class "smaller")))
    (smalllisp    pre (^ (class "smaller")))
    (cartouche    div (^ (class "cartouche")))
    (verbatim     pre (^ (class "verbatim")))
    (chapter      h2)
    (section      h3)
    (subsection   h4)
    (subsubsection       h5)
    (appendix     h2)
    (appendixsec  h3)
    (appendixsubsec      h4)
    (appendixsubsubsec   h5)
    (unnumbered   h2)
    (unnumberedsec       h3)
    (unnumberedsubsec    h4)
    (unnumberedsubsubsec h5)
    (majorheading h2)
    (chapheading  h2)
    (heading      h3)
    (subheading   h4)
    (subsubheading       h5)
    (quotation    blockquote)
    (itemize      ul)
    (item         li) ;; itemx ?
    (para         p)
    (*fragment*   div) ;; should be ok

    (asis         span)
    (bold         b)
    (sample       samp)
    (samp         samp)
    (code         code)
    (kbd          kbd)
    (key          code (^ (class "key")))
    (var          var)
    (env          code (^ (class "env")))
    (file         code (^ (class "file")))
    (command      code (^ (class "command")))
    (option       code (^ (class "option")))
    (url          code (^ (class "url")))
    (dfn          dfn)
    (cite         cite)
    (acro         acronym)
    (email        code (^ (class "email")))
    (emph         em)
    (strong       strong)
    (sc           span (^ (class "small-caps")))))

(define ignore-list
  '(page setfilename setchapternewpage
    iftex ifnottex ifinfo ifplaintext ifxml sp vskip
    menu ignore syncodeindex comment c dircategory direntry top shortcontents
    cindex printindex))
(define (ignored? tag)
  (memq tag ignore-list))

(define rules
  `((% *PREORDER* . ,(lambda args args)) ;; Keep these around...
    (texinfo   . ,(lambda (tag args . body)
                    (pre-post-order
                     `(html
                       (^ (xmlns "http://www.w3.org/1999/xhtml"))
                       (head (title ,(car (arg-req 'title args))))
                       (body ,@body))
                     `((% *PREORDER* . ,(lambda args #f)) ;; ... filter out.
                       (*TEXT*       . ,(lambda (tag x) x))
                       (*DEFAULT*    . ,(lambda (tag . body)
                                          (cons tag body)))))))
    (copyright . ,(lambda args '(*ENTITY* "copy")))
    (result    . ,(lambda args '(*ENTITY* "rArr")))
    (xref . ,ref) (ref . ,ref) (pxref . ,ref)
    (uref . ,uref)
    (node . ,node) (anchor . ,node)
    (table . ,table)
    (itemize . ,itemize)
    (enumerate . ,enumerate)
    (entry . ,entry)

    (deftp . ,def) (defcv . ,def) (defivar . ,def) (deftypeivar . ,def)
    (defop . ,def) (deftypeop . ,def) (defmethod . ,def)
    (deftypemethod . ,def) (defopt . ,def) (defvr . ,def) (defvar . ,def)
    (deftypevr . ,def) (deftypevar . ,def) (deffn . ,def) 
    (deftypefn . ,def) (defmac . ,def) (defspec . ,def) (defun . ,def)
    (deftypefun . ,def)
    (defunx . ,defx) (deffnx . ,defx) (defvarx . ,defx) (defspecx . ,defx)
    (*TEXT*    . ,(lambda (tag x) x))
    (*DEFAULT* . ,(lambda (tag . body)
                    (let ((subst (assq tag tag-replacements)))
                      (cond
                       (subst (append (cdr subst) body))
                       ((memq tag ignore-list) #f)
                       (else 
                        (texinfo-warn
                         (format #f "Don't know how to convert ~s to HTML"
                                 tag))
                        body)))))))

;;@ Transform the stexi @var{tree} into shtml, resolving references via
;; ref-resolvers. See the module commentary for more details.
(define (stexi->shtml tree)
  (pre-post-order tree rules))

;;; arch-tag: ab05f3fe-9981-4a78-b64c-48efcd9983a6