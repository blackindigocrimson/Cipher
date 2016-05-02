#|
 | Shin KAWAHARA
 | "Caesar Cipher"
 |#
(defun encipher-character (ch key)
  (let*
    ((c  (char-code ch))
     (la (char-code #\a)) (lz (char-code #\z))
     (ua (char-code #\A)) (uz (char-code #\Z))
     (base (cond
             ((and (>= c la) (<= c lz)) la)
             ((and (>= c ua) (<= c uz)) ua)
             (t nil))))
    ;; Cipher = Plain + key
    (if base (code-char (+ (mod (+ (- c base) key) 26) base)) ch)))

(defun caesar-cipher (str key)
  ;; Ciphering
  (map 'string #'(lambda (c) (encipher-character c key)) str))

(defun caesar-decipher (str key)
  ;; Recovering
  ;; Plain = Cipher - key
  (caesar-cipher str (- key)))

(defun brute-force-attack (str)
  (dotimes (i 25) (progn (format t "key:~2d " (1+ i))
                         (format t "~a~%" (caesar-decipher str (1+ i))))))

(let* ((plain-text "Example")
       (key 13)
       (cipher-text    (caesar-cipher   plain-text  key))
       (recovered-text (caesar-decipher cipher-text key)))
  (format t "  Plain: ~a ~%" plain-text)
  (format t " Cipher: ~a ~%" cipher-text)
  (format t "Recover: ~a ~%" recovered-text))
