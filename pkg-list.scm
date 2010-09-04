(package (texinfo (0))
  (depends (srfi)
           (spells)
           (wak-ssax)
           (ocelotl))
  
  (synopsis "Texinfo parser")
  (description
   "This package can be used to parse Texinfo documents"
   "and process them as S-expressions (stexi), similiarly to SXML.")
  
  (libraries (("scheme" "texinfo.sls") -> "texinfo.sls")
             (("scheme" "texinfo") -> "texinfo")))

;; Local Variables:
;; scheme-indent-styles: ((package 1))
;; End:
