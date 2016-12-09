
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (24 / 6 - 1994) (12 : 37 : 32 87))
(defq *package* EDITMACH)

(defun edit-mach
 (x)
 (pop x)
 (let
  ((seg -32768) (adr 0))
  (poke seg adr (length x))
  (while x (poke seg (setq adr (1+ adr)) (pop x)))
  (dos-eval '(debug))
  (setq adr 0)
  (repeat-times
   (peek seg adr)
   (setq adr (1+ adr))
   (push (peek seg adr) x))
  (cons 'mach-code (reverse (cons 144 x)))))

(defq EDITMACH (edit-mach EDITMACH))
