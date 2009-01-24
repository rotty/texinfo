#!r6rs

(library (texinfo html)
  (export stexi->shtml stexi-ref-resolvers)
  (import (except (rnrs base) error string-copy string->list string-for-each)
          (rnrs io simple)
          (spells misc)
          (xitomatl srfi lists)
          (xitomatl srfi receive)
          (xitomatl srfi strings)
          (spells alist)
          (spells ascii)
          (spells misc)
          (spells format)
          (spells parameter)
          (only (spells error) make-error-signaller)
          (spells include)
          (sxml transform)
          (texinfo)
          (texinfo warn))

  (define error (make-error-signaller "texinfo html"))
  
  (include-file ((texinfo scheme) html)))
