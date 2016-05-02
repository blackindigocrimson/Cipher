;;
;; RSA
;; @klkl_sn

(defun mydiv (a b)
  "div"
  (floor (/ a b)))
(defun speed-power (a b)
  "a^b"
  (cond
    ((= b 0) 1)
    ((evenp b) (speed-power (* a a) (mydiv b 2)))
    (t (* a (speed-power a (1- b))))))
(defun power-mod (a b n)
  "a^b mod n"
  (mod (speed-power a b) n))

(defun char2code (c)
  "char -> code"
  (cond ((eq c #\space) 0)
        (t (1+ (- (char-code c) (char-code #\a))))))
(defun string2list (text)
  "string -> list (char2code)"
  (map 'list (lambda (x)
               (char2code x))
       text))
(defun multiply100 (x) (* x 100))
(defun list2number (input)
  (reduce (lambda (a b)
            (+ (multiply100 a) b))
          input
          :initial-value 0))
(defun encode-rsa (lst)
  "(plain text)^keyE mod keyN (keyE keyN plain)"
  (format t "~a~%" (power-mod (list2number (string2list (third lst))) (first lst) (second lst))))

(defun list2string (x)
  (map 'string (lambda (n)
                 (cond ((eq n 0) #\space)
                       (t (code-char (+ n (char-code #\a) -1))))) x))
(defun divide100 (x a)
  (if (> 100 x) (push x a)
    (progn
      (push (cadr (multiple-value-list (truncate x 100))) a)
      (divide100 (car (multiple-value-list (truncate x 100))) a))))
(defun decode-rsa (lst)
  "(cipher text)^keyD mod keyN (keyD keyN code)"
  (let ((a nil))
    (list2string (divide100 (power-mod (third lst) (first lst) (second lst)) a))))


(defun defq (a b) (mydiv (caddr a) (caddr b)))
(defun euclid (f g)
  (let ((ff (list (- (car   f) (* (defq f g) (car   g)))
                  (- (cadr  f) (* (defq f g) (cadr  g)))
                  (- (caddr f) (* (defq f g) (caddr g))))))
    (if (caddr g)
         (cadr f)
         (euclid g ff))))
(defun find-d (lst)
  (format t "~a~%" (euclid (list 1 0 (lcm (1- (first lst)) (1- (second lst)))) (list 0 1 (third lst)))))

(defun read-prompt (prompt)
  (format *query-io* " ~a : " prompt)
  (force-output)
  (read-line *query-io*))
(defun encode-prompt ()
  (list
   (or (parse-integer (read-prompt "keyE") :junk-allowed t) 0)
   (or (parse-integer (read-prompt "keyN") :junk-allowed t) 0)
   (read-prompt "Text")))
(defun decode-prompt ()
  (list
   (or (parse-integer (read-prompt "keyD") :junk-allowed t) 0)
   (or (parse-integer (read-prompt "keyN") :junk-allowed t) 0)
   (or (parse-integer (read-prompt "Code") :junk-allowed t) 0)))
(defun find-d-prompt ()
  (list
   (or (parse-integer (read-prompt "p")    :junk-allowed t) 0)
   (or (parse-integer (read-prompt "q")    :junk-allowed t) 0)
   (or (parse-integer (read-prompt "keyE") :junk-allowed t) 0)))

(defun helper ()
  (format t "~% encode~% decode~% find-keyd~% quit~2%"))

(defun init-prompt ()
  (format t "~% RSA ~% (help : [//// > help)~2%"))

(defun main-iter ()
  (let ((order (read-prompt "[//// >")))
    (cond ((string-equal order "encode")     (encode-rsa (encode-prompt)))
          ((string-equal order "decode")     (decode-rsa (decode-prompt)))
          ((string-equal order "find-keyd")  (find-d (find-d-prompt)))
          ((string-equal order "help")       (helper)))
    (if (string-equal order "quit")
        (format t "~%bye bye~%")
        (main-iter))))

(defun main ()
  (init-prompt)
  (main-iter))

(main)
