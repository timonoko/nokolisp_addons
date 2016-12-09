
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (19 / 6 - 1994) (14 : 35 : 51 72))
(defq *package* SOUND)

(defmacro envelope
 (m x)
 (` progn
  (setq al (logand 254 (input-byte 97)))
  (setq up (logor 2 al))
  (setq down (logand 253 al))
  (repeat-times 500 ,@
   (map x
    (function
     (lambda
      (x)
      (` progn
       (repeat-times , (- m x) (output-byte 97 up))
       (repeat-times , x (output-byte 97 down)))))))))

(defun koe ()
 (uncompile 'argh)
 (setq sins nil)
 (for
  (x 0 330 30)
  (push (/ (+ 100 (sin x)) 20) sins))
 (setq argh (list 'envelope 10 sins))
 (compile 'argh))

(defun sound
 (x)
 (setq al (logand 254 (input-byte 97)))
 (setq up (logor 2 al))
 (setq down (logand 253 al))
 (repeat-times
  (- 200 x)
  (repeat-times x (output-byte 97 up))
  (repeat-times x (output-byte 97 down))))

(defq SOUND (envelope koe sound SOUND))
