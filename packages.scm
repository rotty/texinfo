(define-structure texinfo.stexi (export texi-command-specs
                                        texi-command-depth
                                        texi-fragment->stexi
                                        texi->stexi
                                        stexi->sxml)
  (open scheme srfi-1 srfi-6 srfi-11 srfi-13 srfi-14 srfi-23
        spells.condition spells.filesys spells.alist spells.misc spells.ascii
        sxml.simple sxml.transform sxml.ssax.input-parse sxml.ssax.util)
  (files stexi))

(define-structure texinfo.warn (export texinfo-warn-handler texinfo-warn)
  (open scheme
        spells.port spells.format spells.parameter)
  (files warn))

(define-structure texinfo.html (export stexi->shtml stexi-ref-resolvers)
  (open scheme srfi-1 srfi-13 srfi-23
        spells.alist spells.ascii spells.misc spells.format spells.parameter
        sxml.transform
        texinfo.stexi texinfo.warn)
  (files html))

(define-structure texinfo.serialize (export stexi->texi)
  (open scheme srfi-1 srfi-13 srfi-23
        spells.alist spells.format
        sxml.transform
        texinfo.stexi texinfo.warn lib42.wrap-string)
  (files serialize))

