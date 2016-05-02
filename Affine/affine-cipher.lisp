#|
 | Shin KAWAHARA
 | "Affine Cipher"
 |#

(defun encipher (ch key-a key-b)
  (let*
    ((c  (char-code ch))
     (low-a (char-code #\a)) (low-z (char-code #\z))
     (up-a  (char-code #\A)) (up-z  (char-code #\Z))
     (basis (cond
             ((and (>= c low-a) (<= c low-z)) low-a)
             ((and (>= c up-a)  (<= c up-z))  up-a)
             (t nil))))
    ;; Cipher = Plain * key-a + key-b  mod 26
    (if basis (code-char (+ basis (mod (+ key-b (* (- c basis) key-a)) 26))) ch)))

(defun decipher (ch key-a key-b)
  (let*
    ((c  (char-code ch))
     (low-a (char-code #\a)) (low-z (char-code #\z))
     (up-a  (char-code #\A)) (up-z  (char-code #\Z))
     (basis (cond
             ((and (>= c low-a) (<= c low-z)) low-a)
             ((and (>= c up-a)  (<= c up-z))  up-a)
             (t nil)))
     (rekey-a (cond ((equal key-a 1)  1) ((equal key-a  3)  5) ((equal key-a  5) 21)
                    ((equal key-a 7) 15) ((equal key-a  9)  3) ((equal key-a 11) 19)
                    ((equal key-a 15) 7) ((equal key-a 17) 23) ((equal key-a 19) 11)
                    ((equal key-a 21) 5) ((equal key-a 23) 17) ((equal key-a 25) 25)))
     (rekey-b (- key-b)))
    ;; Plain = (Cipher - rekey-b) / rekey-a  mod 26
    (if basis (code-char (+ basis (mod (* rekey-a (+ (- c basis) rekey-b)) 26))) ch)))

(defun affine-cipher (str key-a key-b)
  ;; Ciphering
  (map 'string #'(lambda (c) (encipher c key-a key-b)) str))

(defun affine-decipher (str key-a key-b)
  ;; Recovering
  (map 'string #'(lambda (c) (decipher c key-a key-b)) str))

(defun analysis (str)
  (let*
    ((alphabet (coerce "abcdefghijklmnopqrstuvwxyz" 'list))
     (c (coerce str 'list))
     (counter 0))
    (dolist (i alphabet)
      (dolist (j c)
        (if (char-equal i j)
          (setq counter (1+ counter)) nil))
      (if (not (= 0 counter))
        (progn
          (format t "~a:~d " i counter)
          (dotimes (n counter) (format t "="))
          (format t "|~%")
          (setq counter 0)) nil))))

(let* ((plain-text "Example.")
       (key-a 11) ; == 1,3,5,7,9,11,15,17,19,21,23,25
       (key-b 3)
       (cipher-text  (affine-cipher   plain-text  key-a key-b))
       (recover-text (affine-decipher cipher-text key-a key-b)))
  (format t "  Plain: ~a ~%" plain-text)
  (format t " Cipher: ~a ~%" cipher-text)
  (format t "Recover: ~a ~%" recover-text))
