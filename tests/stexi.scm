;; guile-lib
;; Copyright (C) 2004 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2001,2002 Oleg Kiselyov <oleg at pobox dot com>

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
;; Unit tests for (sxml texinfo). Adapted from xml.ssax.scm.
;;
;;; Code:

(use-modules (oop goops)
             (unit-test)
             (texinfo))

(define-class <test-texinfo> (<test-case>))

(define nl (string #\newline))

(define-macro (@@ mod var)
  `(module-ref (resolve-module ',mod) ',var))

(define texinfo:read-verbatim-body
  (@@ (texinfo) read-verbatim-body))
(define-method (test-read-verbatim-body (self <test-texinfo>))
  (define (read-verbatim-body-from-string str)
    (define (consumer fragment foll-fragment seed)
      (cons* (if (equal? foll-fragment (string #\newline))
                 (string-append " NL" nl)
                 foll-fragment)
             fragment seed))
    (reverse 
     (call-with-input-string
      str
      (lambda (port) (texinfo:read-verbatim-body port consumer '())))))

  (assert-equal '()
                (read-verbatim-body-from-string "@end verbatim\n"))

  ;; after @verbatim, the current position will always directly after
  ;; the newline.
  (assert-exception (read-verbatim-body-from-string "@end verbatim"))
                     
  (assert-equal '("@@end verbatim" " NL\n")
                (read-verbatim-body-from-string "@@end verbatim\n@end verbatim\n"))

  (assert-equal '("@@@@faosfasf adsfas " " NL\n" " asf @foo{asdf}" " NL\n")
                (read-verbatim-body-from-string
                 "@@@@faosfasf adsfas \n asf @foo{asdf}\n@end verbatim\n"))

  (assert-equal '("@end verbatim " " NL\n")
                (read-verbatim-body-from-string "@end verbatim \n@end verbatim\n")))

(define texinfo:read-arguments
  (@@ (texinfo) read-arguments))
(define-method (test-read-arguments (self <test-texinfo>))
  (define (read-arguments-from-string str)
    (call-with-input-string
     str
     (lambda (port) (texinfo:read-arguments port #\}))))

  (define (test str expected-res)
    (assert-equal expected-res
                  (read-arguments-from-string str)))

  (test "}" '())
  (test "foo}" '("foo"))
  (test "foo,bar}" '("foo" "bar"))
  (test "    foo     ,    bar  }" '("foo" "bar"))
  (test " foo ,   , bar }" '("foo" #f "bar"))
  (test "foo,,bar}" '("foo" #f "bar"))
  (assert-exception (test "foo,,bar" 'foo)))

(define texinfo:complete-start-command
  (@@ (texinfo) complete-start-command))
(define-method (test-complete-start-command (self <test-texinfo>))
  (define (test command str)
    (call-with-input-string
     str
     (lambda (port)
       (call-with-values
           (lambda ()
             (texinfo:complete-start-command command port))
         list))))

  (assert-equal '(section () EOL-TEXT)
                (test 'section "foo bar baz bonzerts"))
  (assert-equal '(deffnx ((category "Function") (name "foo") (arguments)) EOL-TEXT-ARGS)
                (test 'deffnx "Function foo"))
  (assert-exception (test 'emph "no brace here"))
  (assert-equal '(emph () INLINE-TEXT)
                (test 'emph "{foo bar baz bonzerts"))
  (assert-equal '(ref ((node "foo bar") (section "baz") (info-file "bonzerts"))
                      INLINE-ARGS)
                (test 'ref "{ foo bar ,,  baz, bonzerts}"))
  (assert-equal '(node ((name "referenced node")) EOL-ARGS)
                (test 'node " referenced node\n")))

(define texinfo:read-char-data
  (@@ (texinfo) read-char-data))
(define make-texinfo-token cons)
(define-method (test-read-char-data (self <test-texinfo>))
  (let* ((code (make-texinfo-token 'START 'code))
         (ref (make-texinfo-token 'EMPTY 'ref))
         (title (make-texinfo-token 'LINE 'title))
         (node (make-texinfo-token 'EMPTY 'node))
         (eof-object (with-input-from-string "" read))
         (str-handler (lambda (fragment foll-fragment seed)
                        (if (string-null? foll-fragment)
                            (cons fragment seed)
                            (cons* foll-fragment fragment seed)))))
    (define (test str expect-eof? preserve-ws? expected-data expected-token)
      (call-with-values
          (lambda ()
            (call-with-input-string
             str
             (lambda (port)
               (texinfo:read-char-data
                port expect-eof? preserve-ws? str-handler '()))))
        (lambda (seed token)
          (let ((result (reverse seed)))
            (assert-equal expected-data result)
            (assert-equal expected-token token)))))

    ;; add some newline-related tests here
    (test "" #t #f '() eof-object)
    (test "foo bar baz" #t #f '("foo bar baz") eof-object)
    (assert-exception (test "" #f #f '() eof-object))
    (test "  " #t #f '("  ") eof-object)
    (test " @code{foo} " #f #f '(" ") code)
    (test " @code" #f #f '(" ") code)
    (test " {text here} asda" #f #f '(" ") (make-texinfo-token 'START '*braces*))
    (test " blah blah} asda" #f #f '(" blah blah") (make-texinfo-token 'END #f))))
     

(define-method (test-texinfo->stexinfo (self <test-texinfo>))
  (define (test str expected-res)
    (assert-equal expected-res
                  (call-with-input-string str texi->stexi)))
  (define (try-with-title title str)
    (call-with-input-string
     (string-append "foo bar baz\n@settitle " title "\n" str)
     texi->stexi))
  (define (test-with-title title str expected-res)
    (test (string-append "foo bar baz\n@settitle " title "\n" str)
          expected-res))
  (define (test-body str expected-res)
    (assert-equal expected-res
                  (cddr (try-with-title "zog" str))))

  (define (list-intersperse src-l elem)
    (if (null? src-l) src-l
        (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
          (if (null? l) (reverse dest)
              (loop (cdr l) (cons (car l) (cons elem dest)))))))
  (define (join-lines . lines)
    (apply string-append (list-intersperse lines "\n")))

  (assert-exception (test "@dots{}\n" '(texinfo (dots))))

  (test "\\input texinfo\n@settitle my title\n@dots{}\n"
        '(texinfo (% (title "my title")) (para (dots))))
  (test-with-title "my title" "@dots{}\n"
        '(texinfo (% (title "my title")) (para (dots))))
  (test-with-title "my title" "@dots{}"
        '(texinfo (% (title "my title")) (para (dots))))

  (assert-exception (try-with-title "my title" "@dots{arg}"))

  (test-body "@code{arg}"
             '((para (code "arg"))))
  (test-body "@code{     }"
             '((para (code))))
  (test-body "@code{ @code{}    }"
             '((para (code (code)))))
  (test-body "@code{  abc    @code{}    }"
             '((para (code "abc " (code)))))
  (test-body "@code{ arg               }"
             '((para (code "arg"))))
  (test-body "@example\n foo asdf  asd  sadf asd  \n@end example\n"
             '((example " foo asdf  asd  sadf asd  ")))
  (test-body (join-lines
              "@quotation"
              "@example"
              " foo asdf  asd  sadf asd  "
              "@end example"
              "@end quotation"
              "")
             '((quotation (example " foo asdf  asd  sadf asd  "))))
  (test-body (join-lines
              "@quotation"
              "@example"
              " foo asdf  @var{asd}  sadf asd  "
              "@end example"
              "@end quotation"
              "")
             '((quotation (example " foo asdf  " (var "asd") "  sadf asd  "))))
  (test-body (join-lines
              "@quotation"
              "@example"
              " foo asdf  @var{asd}  sadf asd  "
              ""
              "not in new para, this is an example"
              "@end example"
              "@end quotation"
              "")
             '((quotation
                (example
                 " foo asdf  " (var "asd")
                 "  sadf asd  \n\nnot in new para, this is an example"))))
  (test-body (join-lines
              "@titlepage"
              "@quotation"
              " foo asdf  @var{asd}  sadf asd  "
              ""
              "should be in new para"
              "@end quotation"
              "@end titlepage"
              "")
             '((titlepage
                (quotation (para "foo asdf " (var "asd") " sadf asd")
                           (para "should be in new para")))))
  (test-body (join-lines
              ""
              "@titlepage"
              ""
              "@quotation"
              " foo asdf  @var{asd}  sadf asd  "
              ""
              "should be in new para"
              ""
              ""
              "@end quotation"
              "@end titlepage"
              ""
              "@bye"
              ""
              "@foo random crap at the end"
              "")
             '((titlepage
                (quotation (para "foo asdf " (var "asd") " sadf asd")
                           (para "should be in new para")))))
  (test-body (join-lines
              ""
              "random notes"
              "@quotation"
              " foo asdf  @var{asd}  sadf asd  "
              ""
              "should be in new para"
              ""
              ""
              "@end quotation"
              ""
              " hi mom"
              "")
             '((para "random notes")
               (quotation (para "foo asdf " (var "asd") " sadf asd")
                          (para "should be in new para"))
               (para "hi mom")))
  (test-body (join-lines
              "@enumerate"
              "@item one"
              "@item two"
              "@item three"
              "@end enumerate"
              )
             '((enumerate (item (para "one"))
                          (item (para "two"))
                          (item (para "three")))))
  (test-body (join-lines
              "@enumerate 44"
              "@item one"
              "@item two"
              "@item three"
              "@end enumerate"
              )
             '((enumerate (% (start "44"))
                          (item (para "one"))
                          (item (para "two"))
                          (item (para "three")))))
  (assert-exception
   (try-with-title "foo" (join-lines
                          "@enumerate string"
                          "@item one"
                          "@item two"
                          "@item three"
                          "@end enumerate"
                          )))
  (assert-exception
   (try-with-title "foo" (join-lines
                          "@itemize string"
                          "@item one"
                          "@item two"
                          "@item three"
                          "@end itemize"
                          )))
  (test-body (join-lines
              "@itemize" ;; no formatter, should default to bullet
              "@item one"
              "@item two"
              "@item three"
              "@end itemize"
              )
             '((itemize (% (bullet (bullet)))
                        (item (para "one"))
                        (item (para "two"))
                        (item (para "three")))))
  (test-body (join-lines
              "@itemize @bullet"
              "@item one"
              "@item two"
              "@item three"
              "@end itemize"
              )
             '((itemize (% (bullet (bullet)))
                        (item (para "one"))
                        (item (para "two"))
                        (item (para "three")))))
  (test-body (join-lines
              "@itemize -"
              "@item one"
              "@item two"
              "@item three"
              "@end itemize"
              )
             '((itemize (% (bullet "-"))
                        (item (para "one"))
                        (item (para "two"))
                        (item (para "three")))))
  (test-body (join-lines
              "@table @code"
              "preliminary text -- should go in a pre-item para"
              "@item one"
              "item one text"
              "@item two"
              "item two text"
              ""
              "includes a paragraph"
              "@item three"
              "@end itemize"
              )
             '((table (% (formatter (code)))
                      (para "preliminary text -- should go in a pre-item para")
                      (entry (% (heading "one"))
                             (para "item one text"))
                      (entry (% (heading "two"))
                             (para "item two text")
                             (para "includes a paragraph"))
                      (entry (% (heading "three"))))))
  (test-body (join-lines
              "@chapter @code{foo} bar"
              "text that should be in a para"
              )
             '((chapter (code "foo") " bar")
               (para "text that should be in a para")))
  (test-body (join-lines
              "@deffnx Method foo bar @code{baz}"
              "text that should be in a para"
              )
             '((deffnx (% (category "Method")
                          (name "foo")
                          (arguments "bar " (code "baz"))))
               (para "text that should be in a para")))
  )

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: f98bded1-c044-457c-b358-7df6a397d648
;;; xml.texinfo.scm ends here
