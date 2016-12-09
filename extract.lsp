
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (14 / 1 - 1995) (8 : 14 : 49 31))
(defq *package* EXTRACT)

(defun extract
 (file start len)
 (in (open file))
 (repeat
  (repeat (or (= (readc) 13) (= (nxtch) 26)))
  (print (setq start (1- start)))
  (sp)
  (or (not (< 0 start)) (= (nxtch) 26)))
 (readc)
 (repeat
  (repeat
   (printc (readc))
   (or (= (nxtch) 13) (= (nxtch) 26)))
  (setq len (1- len))
  (or (not (< 0 len)) (= (nxtch) 26)))
 (close (in))
 (in 0))

(defq EXTRACT (extract EXTRACT))
