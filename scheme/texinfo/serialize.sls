#!r6rs
;;; stexi.sls --- Scheme Texinfo library

;; Copyright (C) 2011 Andreas Rottmann <a.rottmann@gmx.at>

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

;;; Code:

(library (wak texinfo serialize)
  (export stexi->texi)
  
  (import (rnrs base)
          (rnrs lists)
          (spells alist)
          (spells format)
          (spells tracing) ;debug
          (only (spells error) make-error-signaller)
          (only (srfi :1) append-map concatenate drop-while reverse!)
          (only (srfi :13)
                string-concatenate
                string-concatenate-reverse
                string-index)
          (wak private include)
          (wak texinfo string-utils)
          (wak texinfo)
          (wak texinfo warn))

  (define error (make-error-signaller "texinfo library (serialize)"))
  
  (include-file ((wak texinfo private) serialize)))
