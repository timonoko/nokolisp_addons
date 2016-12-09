
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (28 / 4 - 2004) (18 : 51 : 44 77))
(defq *package* BMP)

(defun LSEEK
 (file method pos)
 (BX-reg file)
 (CX-reg (if pos (car pos) 0))
 (DX-reg (if pos (cdr pos) 0))
 (AX-reg (plus 16896 method))
 (if (INT- 33) (cons (DX-reg) (AX-reg))))

(defun koe
 (f s)
 (in (open f))
 (LSEEK (in) 0 (cons s 0))
 (setq quu (readc-bin (in)))
 (close (in))
 (in 0))

(defun bmp-datat
 (f)
 (in (open f))
 (if
  (= (readc-bin) (char B))
  (progn
   (repeat-times 9 (readc-bin))
   (setq biOffset (+ (readc-bin) (* 256 (readc-bin))))
   (repeat-times 4 (readc-bin))
   (setq biWidth (+ (readc-bin) (* 256 (readc-bin))))
   (setq biHeight (+ (readc-bin) (* 256 (readc-bin))))
   (repeat-times 2 (readc-bin))
   (setq biBitCount (+ (readc-bin) (* 256 (readc-bin))))
   (repeat-times (- 47 31) (readc-bin))
   (setq biClrUsed (+ (readc-bin) (* 256 (readc-bin)))))
  (error-reset '(not BMP - file)))
 (close (in))
 (in 0)
 (list biOffset biWidth biHeight biBitCount biClrUsed))

(defq BMP (LSEEK koe bmp-datat BMP))
