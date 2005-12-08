;;; ----------------------------------------------------------------------
;;;    string-transform
;;;    Copyright (C) 2005 Andreas Rottmann
;;;    Copyright (C) 2003 Richard Todd
;;;
;;;    This program is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation; either version 2 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program; if not, write to the Free Software
;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;; ----------------------------------------------------------------------

;;; Commentary:
;; Module @samp{(string transform)} provides functions for modifying
;; blah blah blah strings beyond that which is provided in the guile core and
;; @samp{(srfi srfi-13)}.  
;;; Code:

;; Uses @var{match?} against each character in @var{str}, and performs a
;; replacement on each character for which matches are found.
 
;; @var{match?} may either be a function, a character, a string, or
;; @code{#t}.  If @var{match?}  is a function, then it takes a single
;; character as input, and should return @samp{#t} for matches.
;; @var{match?} is a character, it is compared to each string character
;; using @code{char=?}.  If @var{match?} is a string, then any character
;; in that string will be considered a match.  @code{#t} will cause 
;; every character to be a match.
 
;; If @var{replace} is a function, it is called with the matched
;; character as an argument, and the returned value is sent to the output
;; string via @samp{display}.  If @var{replace} is anything else, it is
;; sent through the output string via @samp{display}.
 
;; Note that te replacement for the matched characters does not need to
;; be a single character.  That is what differentiates this function from
;; @samp{string-map}, and what makes it useful for applications such as
;; converting @samp{#\\&} to @samp{\"&amp;\"} in web page text.  Some other
;; functions in this module are just wrappers around common uses of
;; @samp{transform-string}.  Transformations not possible with this
;; function should probably be done with regular expressions.
 
;; If @var{start} and @var{end} are given, they control which portion
;; of the string undergoes transformation.  The entire input string
;; is still output, though.  So, if @var{start} is @samp{5}, then the
;; first five characters of @var{str} will still appear in the returned
;; string.
 
;; @lisp
;; ; these two are equivalent...
;;  (transform-string str #\\space #\\-) ; change all spaces to -'s
;;  (transform-string str (lambda (c) (char=? #\\space c)) #\\-)
;; @end lisp
(define/optional-args (transform-string str match? replace
                                        (optional (start #f) (end #f)))
  ;;  I had implemented this with string-fold, but it was
  ;; slower...
  (let* ((os (open-output-string))
         (matcher (cond ((char? match?)
                         (lambda (c) (char=? match? c)))
                        ((procedure? match?)
                         match?)
                        ((string? match?)
                         (lambda (c) (string-index match? c)))
                        ((boolean? match?)
                         (lambda (c) match?))
                        (else (error "expected #t, char, string, or procedure"))))
         (replacer (if (procedure? replace)
                       (lambda (c) (display (replace c) os))
                       (lambda (c) (display replace os)))))

    ;; put the first part in, un-transformed if they asked for it...
    (if (and start (<= start (string-length str)))
        (display (substring str 0 start) os))

    ;; process the portion they want processed....
    (string-for-each
     (lambda (c)
       (if (matcher c)
           ;; we have a match! replace the char as directed...
           (replacer c)

           ;; not a match, just insert the character itself...
           (write-char c os)))
     str
     (or start 0)
     (or end (string-length str)))

    ;; if there was any at the end, tack it on...
    (if (and end (< end (string-length str)))
        (display (substring str end (string-length str)) os))

    (get-output-string os)))

;;@ Returns a copy of @var{str} with all tabs expanded to spaces.
;; @var{tab-size} defaults to 8.
;;
;; Assuming tab size of 8, this is equivalent to: @lisp
;;  (transform-string str #\\tab \"        \")
;; @end lisp
(define/optional-args (expand-tabs str (optional (tab-size 8)))
  (transform-string str 
                    #\tab
                    (make-string tab-size #\space)))

;; @ Returns a copy of @var{str} with all given special characters preceded
;; by the given @var{escape-char}.
  
;; @var{special-chars} can either be a single character, or a string consisting
;; of all the special characters.
  
;; @lisp
;; ;; make a string regexp-safe...
;;  (escape-special-chars \"***(Example String)***\"  
;;                       \"[]()/*.\" 
;;                       #\\\\)
;; => \"\\\\*\\\\*\\\\*\\\\(Example String\\\\)\\\\*\\\\*\\\\*\"
  
;; ;; also can escape a singe char...
;;  (escape-special-chars \"richardt@@vzavenue.net\"
;;                       #\\@@
;;                       #\\@@)
;; => \"richardt@@@@vzavenue.net\"
;; @end lisp
(define (escape-special-chars str special-chars escape-char)
  (transform-string str
                    (if (char? special-chars)
                        ;; if they gave us a char, use char=?
                        (lambda (c) (char=? c special-chars))

                        ;; if they gave us a string, see if our character is in it
                        (lambda (c) (string-index special-chars c)))

                    ;; replace matches with the character preceded by
                    ;; the escape character
                    (lambda (c) (string escape-char c))))

;; Returns a copy of @var{str} centered in a field of @var{width}
;; characters.  Any needed padding is done by character @var{chr}, which
;; defaults to @samp{#\\space}.  If @var{rchr} is provided, then the
;; padding to the right will use it instead.  See the examples below.
;; left and @var{rchr} on the right.  The default @var{width} is 80.  The
;; default @var{lchr} and @var{rchr} is @samp{#\\space}.  The string is
;; never truncated.
;; @lisp
;;  (center-string \"Richard Todd\" 24)
;; => \"      Richard Todd      \"
;;  
;;  (center-string \" Richard Todd \" 24 #\\=)
;; => \"===== Richard Todd =====\"
;;  
;;  (center-string \" Richard Todd \" 24 #\\< #\\>)
;; => \"<<<<< Richard Todd >>>>>\"
;; @end lisp
(define/optional-args (center-string str (optional (width 80)
                                                   (chr #\space)
                                                   (rchr #f)))
  (let* ((len (string-length str))
         (lpad (make-string (max (quotient (- width len) 2) 0) chr))
         ;; right-char == char unless it has been provided by the user
         (right-chr (or rchr chr))
         (rpad (if (char=? right-chr chr)
                   lpad
                   (make-string (max (quotient (- width len) 2) 0) right-chr))))
    (if (>= len width)
        str
        (string-append lpad str rpad
                       (if (odd? (- width len)) (string right-chr) "")))))

;; @code{left-justify-string str [width chr]}.  
;; Returns a copy of @var{str} padded with @var{chr} such that it is left
;; justified in a field of @var{width} characters.  The default
;; @var{width} is 80.  Unlike @samp{string-pad} from srfi-13, the string
;; is never truncated.
(define/optional-args (left-justify-string str (optional (width 80) (chr #\space)))
  (let* ((len (string-length str))
         (pad (make-string (max (- width len) 0) chr)))
    (if (>= len width)
        str
        (string-append str pad))))

;; Returns a copy of @var{str} padded with @var{chr} such that it is
;; right justified in a field of @var{width} characters.  The default
;; @var{width} is 80.  The default @var{chr} is @samp{#\\space}.  Unlike
;; @samp{string-pad} from srfi-13, the string is never truncated.
(define/optional-args (right-justify-string str (optional (width 80) (chr #\space)))
  (let* ((len (string-length str))
         (pad (make-string (max (- width len) 0) chr)))
    (if (>= len width)
        str
        (string-append pad str))))

;; Returns a copy of @var{str} with all repeated instances of 
;; @var{chr} collapsed down to at most @var{num} instances.
;; The default value for @var{chr} is @samp{#\\space}, and 
;; the default value for @var{num} is 1.
;;  
;; @lisp
;;  (collapse-repeated-chars \"H  e  l  l  o\")
;; => \"H e l l o\"
;;  (collapse-repeated-chars \"H--e--l--l--o\" #\\-)
;; => \"H-e-l-l-o\"
;;  (collapse-repeated-chars \"H-e--l---l----o\" #\\- 2)
;; => \"H-e--l--l--o\"
;; @end lisp"
(define/optional-args (collapse-repeated-chars str (optional (chr #\space) (num 1)))
  ;; define repeat-locator as a stateful match? function which remembers
  ;; the last character it had seen.
  (let ((repeat-locator
         ;; initialize prev-chr to something other than what we're seeking...
         (let ((prev-chr (if (char=? chr #\space) #\A #\space))
               (match-count 0))
           (lambda (c)
             (if (and (char=? c prev-chr)
                      (char=? prev-chr chr))
                 ;; found enough duplicates if the match-count is high enough
                 (begin
                   (set! match-count (+ 1 match-count))
                   (>= match-count num))

                 ;; did not find a duplicate
                 (begin (set! match-count 0) 
                        (set! prev-chr c) 
                        #f))))))

    ;; transform the string with our stateful matcher...
    ;; deleting matches...
    (transform-string str repeat-locator "")))


;;; arch-tag: 71550291-cf61-4ddd-bb50-2386b4d38756
