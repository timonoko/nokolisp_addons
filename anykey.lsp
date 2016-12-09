
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (12 / 6 - 1994) (19 : 44 : 56 98))
(defq *package* GPS)

(defun any-key () (msdos 11) (= (low-byte (AX-reg)) 255))
