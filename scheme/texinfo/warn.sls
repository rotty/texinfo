;;; warn.sls --- warning library for texinfo

;; Copyright (C) 2008, 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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


#!r6rs

(library (wak texinfo warn)
  (export texinfo-warn-handler texinfo-warn)
  (import (rnrs base)
          (rnrs io simple)
          (spells format)
          (srfi :39 parameters)
          (spells include))
  
  (include-file ((wak texinfo private) warn))

  )
