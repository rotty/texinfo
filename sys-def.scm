;;@ Parse documents in texinfo and process them S-expressions (stexi),
;; similiar to SXML.
(define-system texinfo
  (dependencies spells)
  (r6rs-libraries (("scheme" "texinfo.sls") . "texinfo.sls")
                  (("scheme" "texinfo") . "texinfo")))
