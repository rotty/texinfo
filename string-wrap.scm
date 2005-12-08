;;; ----------------------------------------------------------------------
;;;    wrap -- text filling and wrapping 
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

;; Module @samp{(string wrap)} provides functions for formatting text
;; strings such that they fill a given width field.  A class,
;; @code{<text-wrapper>}, does the work, but two convenience methods
;; create instances of it for one-shot use, and in the process make for
;; a more ``schemey'' interface.  If many strings will be
;; formatted with the same parameters, it might be better
;; performance-wise to create and use a single @code{<text-wrapper>}.

;;; Code:

;; This class encapsulates the parameters needing to be fed to the text wrapping
;; algorithm.  The following are the recognized keywords on the call to @code{make}:
  
;; @table @code
;; @item #:line-width
;; This is the target length used when deciding where to wrap lines.  Default is 80.
  
;; @item #:expand-tabs?
;; Boolean describing whether tabs in the input should be expanded.  Default is #t.
  
;; @item #:tab-width
;; If tabs are expanded, this will be the number of spaces to which they expand.
;; Default is 8.
  
;; @item #:collapse-whitespace?
;; Boolean describing whether the whitespace inside the existing text
;; should be removed or not.  Default is #t.
  
;; If text is already well-formatted, and is just being wrapped to fit in a different width,
;; then setting this to @samp{#f}.  This way, many common text conventions (such as
;; two spaces between sentences) can be preserved if in the original text.  If the input
;; text spacing cannot be trusted, then leave this setting at the default, and all repeated
;; whitespace will be collapsed down to a single space.
  
;; @item #:initial-indent
;; Defines a string that will be put in front of the first line of wrapped text.  
;; Default is the empty string, ``''.
  
;; @item #:subsequent-indent
;; Defines a string that will be put in front of all lines of wrapped
;; text, except the first one.  Default is the empty string, ``''.
  
;; @item #:break-long-words?
;; If a single word is too big to fit on a line, this setting tells the
;; wrapper what to do.  Defaults to #t, which will break up long words.
;; When set to #f, the line will be allowed, even though it is longer
;; than the defined @code{#:line-width}.
;; @end table
  
;; Here's an example of creating a @code{<text-wrapper>}:
;; @lisp
;;  (make <text-wrapper> #:line-width 48 #:break-long-words? #f)
;; @end lisp
(define (get-option key options)
  (cond ((or (assq key options)
             (assq key '((line-width . 80)
                         (expand-tabs? . #t)
                         (tab-width . 8)
                         (collapse-whitespace? . #t)
                         (subsequent-indent . "")
                         (initial-indent . "")
                         (break-long-words? . #t))))
         => cdr)
        (else (error "invalid string wrap option" key))))

;;@ @code{fill-string str keywds ...}. 
;; Wraps the text given in string @var{str} according to the parameters
;; provided in @var{keywds}, or the default setting if they are not
;; given.  Returns a single string with the wrapped text.  Valid keyword
;; arguments are discussed with the @code{<text-wrapper>} class.
  
;; @code{fill-string tw str}.  fills @var{str} using the instance
;; of @code{<text-wrapper>} given as @var{tw}.")
(define/optional-args (fill-string str (optional (options '())))
  (string-join (string->wrapped-lines str options)
               "\n"
               'infix))

;; expand tabs, collapse whitespace and trim the string
(define (transform-string/options str options)
  (fold (lambda (transform str)
          (if (or (eq? (car transform) #t)
                  (get-option (car transform) options))
              ((cdr transform) str)
              str))
        (transform-string str (lambda (c) (char=? c #\newline)) #\space)
        `((expand-tabs? . ,(lambda (str)
                             (expand-tabs str (get-option 'tab-width options))))
          (collapse-whitespace? . ,collapse-repeated-chars)
          (#t string-trim))))

;;@ @code{string->wrapped-lines str keywds ...}.  
;; Wraps the text given in string @var{str} according to the parameters
;; provided in @var{keywds}, or the default setting if they are not
;; given.  Returns a list of strings representing the formatted lines.
;; Valid keyword arguments are discussed with the @code{<text-wrapper>}
;; class.
  
;; @code{string->wrapped-lines tw str}. 
;; Wraps the text given in string @var{str} according to the given
;; @code{<text-wrapper>} @var{tw}.  Returns a list of strings representing
;; the formatted lines.  Valid keyword arguments are discussed with the
;; @code{<text-wrapper>} class.")
(define/optional-args (string->wrapped-lines str (optional (options '())))
  (let ((subsequent-indent (get-option 'subsequent-indent options))
        (line-width (get-option 'line-width options))
        (break-long-words? (get-option 'break-long-words? options)))
  ;; now start breaking the text into lines...
  (let loop ((ans '())
             (words (split-by-single-words (transform-string/options str options)))
             (line (get-option 'initial-indent options))
             (count 0))
    (if (null? words)
        ;; out of words? ...done!
        (reverse (if (> count 0)
                     (cons line ans)
                     ans))
        
        ;; not out of words...keep going...
        (let ((length-left (- line-width
                              (string-length line)))
              (next-word (if (= count 0)
                             (string-trim (car words))
                             (car words))))
          (cond 
           ;; does the next entry fit?
           ((<= (string-length next-word)
                length-left)
            (loop ans
                  (cdr words)
                  (string-append line next-word)
                  (+ count 1)))

           ;; ok, it didn't fit...is there already at least one word on the line?
           ((> count 0)
            ;; try to use it for the next line, then...
            (loop (cons line ans)
                  words
                  subsequent-indent
                  0))
           
           ;; ok, it didn't fit...and it's the first word. 
           ;; were we told to break up long words?
           (break-long-words?
            ;; break the like at the limit, since the user wants us to...
            (loop (cons (string-append line (substring next-word 0 length-left))
                        ans)
                  (cons (substring next-word length-left (string-length next-word))
                        (cdr words))
                  subsequent-indent
                  0))

           ;; well, then is it the first word and we *shouldn't* break
           ;; long words, then...
           (else
            (loop (cons line ans)
                  (cdr words)
                  subsequent-indent
                  0))))))))

;; split a text string into segments that have the form...
;;  <ws non-ws>  <ws non-ws> etc..
(define (split-by-single-words str)
  (let ((non-wschars (char-set-complement char-set:whitespace)))
    (let loop ((ans '())
               (index 0))
      (let ((next-non-ws (string-index str non-wschars index)))
        (if next-non-ws
          ;; found non-ws...look for ws following...
          (let ((next-ws (string-index str char-set:whitespace next-non-ws)))
            (if next-ws
                ;; found the ws following...
                (loop (cons (substring str index next-ws) ans)
                      next-ws)
                ;; did not find ws...must be the end...
                (reverse (cons (substring str index (string-length str)) ans))))
          ;; did not find non-ws... only ws at end of the string...
          (reverse ans))))))

;;; arch-tag: 61c4a6f9-0c13-4575-95dc-a7e647812216
