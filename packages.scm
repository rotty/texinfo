(define-structure texinfo.stexi (export texi-command-specs
                                        texi-command-depth
                                        texi-fragment->stexi
                                        texi->stexi
                                        stexi->sxml)
  (open scheme srfi-1 srfi-6 srfi-11 srfi-13 srfi-14 srfi-23
        spells.condition spells.file spells.alist spells.misc spells.ascii
        sxml.simple sxml.transform sxml.ssax.input-parse sxml.ssax.util)
  (files stexi))

(define-structure texinfo.warn (export texinfo-warn-handler texinfo-warn)
  (open scheme
        spells.port spells.format spells.parameter)
  (files warn))

(define-structure texinfo.html (export stexi->shtml add-ref-resolver!)
  (open scheme srfi-1 srfi-13 srfi-23
        spells.alist spells.ascii spells.misc spells.format
        sxml.transform
        texinfo.stexi texinfo.warn)
  (files html))

(define-structure texinfo.serialize (export stexi->texi)
  (open scheme srfi-1 srfi-13 srfi-23
        spells.alist spells.format
        sxml.transform
        texinfo.stexi texinfo.warn texinfo.string-wrap)
  (files serialize))

;; This should be moved to spells, once the licencing is cleared
(define-structure texinfo.string-wrap (export fill-string
                                              string->wrapped-lines)
  (open scheme srfi-1 srfi-13 srfi-14 srfi-23
        spells.opt-args
        texinfo.string-transform)
  (files string-wrap))

(define-structure texinfo.string-transform (export escape-special-chars
                                                   transform-string
                                                   expand-tabs
                                                   center-string
                                                   left-justify-string
                                                   right-justify-string
                                                   collapse-repeated-chars)
  (open scheme srfi-6 srfi-13 srfi-23
        spells.opt-args)
  (files string-transform))

;; arch-tag: 38070041-8210-48d6-9314-85a6d2c39d0a
