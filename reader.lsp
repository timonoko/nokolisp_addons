
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (14 / 8 - 1994) (22 : 21 : 31 82))
(defq *package* READER)

(defun reader
 (f)
 (in (open f))
 (setq THIS-LINE 0)
 (unless WANT-LINE (setq WANT-LINE 0))
 (repeat
  (let
   ((r 1) (c) (com))
   (repeat
    (setq c (readc))
    (when
     (= c 13)
     (setq r (1+ r))
     (setq THIS-LINE (1+ THIS-LINE)))
    (if
     (< WANT-LINE THIS-LINE)
     (progn
      (printc c)
      (if (= c 10) (printc c))
      (> r 1))
     (progn (when (= c 13) (print THIS-LINE) (sp)) nil)))
   (setq com (readcc))
   (= com 27)))
 (close (in))
 (in 0))

(defq READER (reader READER))
