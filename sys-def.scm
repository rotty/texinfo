;;@ Parse documents in texinfo and process them S-expressions (stexi),
;; similiar to SXML.
(define-system texinfo
  (dependencies spells sxml)
  (config-files packages))
