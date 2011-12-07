;;;; The code below can be used freely under the following conditions:
;;;; * Due credit is given to the original author.
;;;; * You use it at your own risk. The code is experimental and meant
;;;;   as a case study to show that "lazy reals" can be implemented.
;;;;   In particular, no guarantee is given that the code conforms to
;;;;   the specification given below.
;;;;
;;;; If you use it in publications (including software), please let me
;;;; know.
;;;;
;;;; Note: The main focus here is on correct results (which are, however,
;;;; not guaranteed, see above). It is certainly possible to improve the
;;;; efficiency of the code quite a bit.
;;;;
;;;; Michael Stoll, December 11, 2009
;;;; firstname.lastname@uni-bayreuth.de

;;;; ======================================================================

;;;; Computable real numbers
;;;; Michael Stoll
;;;; 1989-06-11, 1989-06-12, 1989-06-13, 1989-06-14, 1989-06-17, 1989-06-30


;;;;                        I N T R O D U C T I O N
;;;;                        =======================

;;;; Computable real numbers x are interpreted as (potentially) infinite
;;;; fractions in base 2 that are specified through a rule for computation
;;;; of an integer a with |(2^k)*x - a| <= 1 for any k>=0.

;;;; The internal data structure should not be accessed.
;;;; The interface for the outside world is as follows:

;;;; The type CREAL is a supertype of the type RATIONAL. (CREAL-P x), for
;;;; an object x, returns T if x is of type CREAL, otherwise NIL.
;;;;
;;;; (APPROX-R x k), for a CREAL x and an integer k>=0, returns an integer
;;;; a with |(2^k)*x - a| < 1.
;;;;
;;;; (MAKE-REAL fun) returns the real number given by fun. Here fun is a
;;;; function taking an argument k, that computes a as above.

;;;; CREALs are output by print etc. as a decimal fraction. The error hereby
;;;; is at most one unit in the last digit that was output. The number of
;;;; decimal digits after the decimal point is defined through the dynamic
;;;; variable *PRINT-PREC*.
;;;;
;;;; For comparison operations etc. a precision threshold is used. It is
;;;; defined through the dynamic variable *CREAL-TOLERANCE*. Its value should
;;;; be a nonnegative integer n, meaning that numbers are considered equal
;;;; if they differ by at most 2^(-n).


;;;;      E X P O R T E D   F U N C T I O N S   A N D   C O N S T A N T S
;;;;      ===============================================================

;;;; The following functions, constants and variables are exported. (The
;;;; package is named "REALS".)
;;;; CREAL                  type        type of the computable real numbers
;;;; CREAL-P object         function    tests for type CREAL
;;;; *PRINT-PREC*           variable    specifies precision of output
;;;; *CREAL-TOLERANCE*      variable    precision threshold for comparison
;;;; APPROX-R x:creal k:int>=0
;;;;                        function    returns approximation of x to k digits
;;;; MAKE-REAL function     function    creates object of type CREAL
;;;; RAW-APPROX-R x:creal   function    returns 3 values a,n,s with:
;;;;                                    if a = 0: |x| <= 2^(-n), s = 0
;;;;                                        and n >= *CREAL-TOLERANCE*
;;;;                                    else: a0 integer > 4, n0 integer >=0,
;;;;                                        s = +1 or -1, and sign(x) = s,
;;;;                                        (a-1)*2^(-n) <= |x| <= (a+1)*2^(-n)
;;;; PRINT-R x:creal k:int>=0 &optional (flag t)
;;;;                        function    outputs x with k decimal digits.
;;;;                                    If flag is true, first a newline.
;;;; +R {creal}*            function    computes the sum of the arguments
;;;; -R creal {creal}*      function    computes negative or difference
;;;; *R {creal}*            function    computes the product of the arguments
;;;; /R creal {creal}*      function    computes reciprocal or quotient
;;;; SQRT-R creal           function    computes the square root
;;;; LOG2-R                 constant    log(2)
;;;; PI-R                   constant    pi
;;;; 2PI-R                  constant    2*pi
;;;; PI/2-R                 constant    pi/2
;;;; PI/4-R                 constant    pi/4
;;;; LOG-R x:creal &optional b:creal
;;;;                        function    computes the logarithm of n in base b;
;;;;                                    default is the natural logarithm
;;;; EXP-R creal            function    computes the exponential function
;;;; EXPT-R x:creal y:creal function    computes x^y
;;;; SIN-R creal            function    computes the sine
;;;; COS-R creal            function    computes the cosine
;;;; TAN-R creal            function    computes the tangent
;;;; ATAN-R x:creal &optional y:creal
;;;;                        function    computes the arctangent of x or
;;;;                                    the phase angle of (x,y)
;;;; ASH-R x:creal n:int    function    computes x * 2^n
;;;; ROUND-R x:creal &optional y:creal
;;;;                        function    computes two values q (integer) and r
;;;;                                    (creal) with x = q*y + r and |r|<=|y|/2
;;;;                                    according to the precision specified by
;;;;                                    *CREAL-TOLERANCE*
;;;; FLOOR-R x:creal &optional y:creal
;;;;                        function    like ROUND-R, corresponding to floor
;;;; CEILING-R x:creal &optional y:creal
;;;;                        function    like ROUND-R, corresponding to ceiling
;;;; TRUNCATE-R x:creal &optional y:creal
;;;;                        function    like ROUND-R, corresponding to truncate


;;;; In the code, exported symbols at their location of definition are
;;;; emphasized in UPPERCASE.

;;;; ==========================================================================

;;;;   I N T E R N A L   S T R U C T U R E S   A N D   I N T E R F A C E
;;;;   -----------------------------------------------------------------

;; Package for computable real numbers
(defpackage "REALS" (:use "COMMON-LISP"))
(in-package "REALS")

(export '(creal approx-r make-real creal-p print-r +r -r *r /r sqrt-r
          log2-r log-r exp-r pi-r 2pi-r sin-r cos-r *print-prec* round-r
          *creal-tolerance* ash-r raw-approx-r floor-r ceiling-r truncate-r
          pi/2-r pi/4-r atan-r expt-r tan-r
)        )

;;; Computable reel numbers are rational numbers or structures:

(defstruct (c-real (:copier nil)
                   (:print-function print-c-real)
           )
           (value     0      :type integer)
           (precision -1     :type (integer -1 *))
           (compute   nil    :type (function ((integer 0 *)) integer)
                             :read-only t
)          )

(deftype CREAL () "type of the computable real numbers"
  '(or rational c-real)
)

(defun CREAL-P (x) (or (rationalp x) (c-real-p x)))

;; If r is a c-real with (c-real-value r) = a and (c-real-precision r) = k,
;; then a*2^(-k) is an approximation of the value of the number represented
;; by r that deviates from the actual value by at most 2^(-k).
;; (c-real-compute r) is a function taking an argument k, that returns an
;; approximation of precision 2^(-k) and returns the corresponding value a.

;;; make-real creates a c-real from a computation function.

(defun MAKE-REAL (comp) "This function creates CREALs"
  (declare (type (function ((integer 0 *)) integer) comp))
  (make-c-real :compute comp)
)

;;; The following function takes an object of type creal and a number k,
;;; and returns an integer a with |a*2^(-k) - x| <= 2^(-k), where x denotes
;;; the corresponding real number.

(defun APPROX-R (x k) "This function computes approximations for CREALs"
  (unless (creal-p x) (cr-error 'approx-r x))
  (unless (and (integerp k) (>= k 0)) (nat-error 'approx-r k))
  (get-approx x k)
)

(defun get-approx (x k)
  (declare (type creal x) (type (integer 0 *) k))
  (cond ((integerp x) (ash x k))
        ((rationalp x) (round (ash (numerator x) k) (denominator x)))
        ((c-real-p x)
         (if (>= (c-real-precision x) k)
           (ash (c-real-value x) (- k (c-real-precision x)))
           (let ((a (funcall (c-real-compute x) k)))
             (setf (c-real-value x) a (c-real-precision x) k)
             a
        )) )
        (t (cr-error 'get-approx x))
) )

;;; A few shortcuts for signalling type errors.

(defun cr-error (fun x)
  (error "~S: ~S is not a computable real number" fun x)
)
(defun int-error (fun x)
  (error "~S: ~S is not an integer" fun x)
)
(defun nat-error (fun x)
  (error "~S: ~S is not a nonnegative integer" fun x)
)

;;;; ==========================================================================

;;;;                           V A R I A B L E S
;;;;                           -----------------

;;; *print-prec* specifies how many digits after the decimal point are output
;;; (by print etc.)

(defparameter *PRINT-PREC* 20
  "number of decimal digits after the decimal point during output of CREALs"
)

;;; *creal-tolerance* specifies the precision of comparison operations

(defparameter *CREAL-TOLERANCE* 100
  "precision threshold for the comparison of CREALs,
denoting the number of binary digits after the decimal point"
)

;;;;                  A U X I L I A R Y   F U N C T I O N S
;;;;                  -------------------------------------

;;; The following functions perform rounding, when less precision is needed.

(defun round-cr (a k)
  (declare (type integer a) (type (integer 0 *) k))
  (if (eql k 0)
    a
    (if (logbitp (1- k) a) (1+ (ash a (- k))) (ash a (- k)))
) )

;;; Auxiliary function for approximating.

(defun raw-approx-cr (x)
  (declare (type creal x))
  (do* ((k 0 (+ k 4))
        (a (get-approx x 0) (get-approx x k))
        (crt (+ 2 *creal-tolerance*))
       )
       ((> (abs a) 4) (values (abs a) k (signum a)))
    (when (> k crt) (return (values 0 (- k 3) 0)))
) )

(defun RAW-APPROX-R (x) "This function returns an approximation for CREALs"
  (unless (creal-p x) (cr-error 'raw-approx-r x))
  (raw-approx-cr x)
)

;;;;                        P R I N T   F U N C T I O N
;;;;                        ---------------------------

;;; Small auxiliary function for avoiding repeated computation:

(let* ((pp *print-prec*) (tenpowerpp (expt 10 pp)))
  (declare (type (integer 0 *) pp tenpowerpp))
  (defun tenpower (k)
    (declare (type (integer 0 *) k))
    (if (eql k pp)
      tenpowerpp
      (let ((zhk (expt 10 k)))
        (when (eql k *print-prec*) (setq pp k tenpowerpp zhk))
        zhk
) ) ) )

;;; The next function performs output to k digits after the decimal point,
;;; ensuring an error of at most one unit on the last digit.

(defun PRINT-R (x k &optional (flag t) (stream *standard-output*))
  "output function for CREALs"
  ;; flag /= NIL: the value is printed in a new line
  ;; flag = NIL: no linefeed
  (unless (creal-p x) (cr-error 'print-r x))
  (unless (and (integerp k) (>= k 0)) (nat-error 'print-r k))
  (unless (streamp stream) (error "~S: ~S is not a stream" 'print-r stream))
  (creal-print x k flag stream)
)

(defun creal-print (x k flag stream)
  (declare (type creal x) (type (integer 0 *) k) (type stream stream))
  (let* ((k1 (tenpower k))
         (n (1+ (integer-length k1)))
         (x1 (get-approx x n))
         (sign (signum x1))
         (x2 (round-cr (* (abs x1) k1) n))
         (*print-base* 10.)
        )
    (multiple-value-bind (vor nach) (floor x2 k1)
      (when flag (terpri stream))
      (write-char (if (minusp sign) #\- #\+) stream)
      (prin1 vor stream)
      (write-char #\. stream)
      (let ((s (prin1-to-string nach)))
        (write-string (make-string (- k (length s)) :initial-element #\0)
                      stream
        )
        (write-string s stream)
        (write-string "..." stream)
        (values)
) ) ) )

(defun print-c-real (x stream d)
  (declare (ignore d))
  (creal-print x *print-prec* nil stream))

;;;;                           A R I T H M E T I C
;;;;                           -------------------

;;; Now comes the addition.

(defun +R (&rest args &aux (sn 0) (rl nil)) "addition of CREALs"
  (declare (type rational sn) (type list #|(list creal)|# rl))
  (dolist (x args)
    (cond ((rationalp x) (setq sn (+ x sn)))
          ((c-real-p x) (setq rl (cons x rl)))
          (t (cr-error '+ x))
  ) )
  ;; sn = exact partial sum
  ;; rl = list of the "real" real arguments
  (let* ((n (length rl)) ; n = how many of them
         (k1 (integer-length (if (integerp sn) n (1+ n))))
           ; k1 = number of additional binary digits for the summands
        )
    (if (eql n 0)
      sn        ; sum is exact
      (make-real
        #'(lambda (k &aux (k2 (+ k k1)))
            (do ((sum (get-approx sn k2) (+ sum (get-approx (first l) k2)))
                 (l rl (rest l))
                )
                ((null l) (round-cr sum k1))
) ) ) )   ) )

;;; Negation:

(defun minus-r (x)
  (cond ((rationalp x) (- x))
        ((c-real-p x) (make-real #'(lambda (k) (- (get-approx x k)))))
        (t (cr-error '- x))
) )

;;; Subtraction:

(defun -R (x1 &rest args) "subtraction and negation of CREALs"
  (if (null args)
    (minus-r x1)
    (+r x1 (minus-r (apply #'+r args)))
) )

;;; Now comes the multiplication.

(defun *R (&rest args &aux (pn 1) (rl nil)) "Multiplication for CREALs"
  (declare (type rational pn) (type list #|(list creal)|# rl))
  (dolist (x args)
    (cond ((rationalp x) (setq pn (* x pn)))
          ((c-real-p x) (setq rl (cons x rl)))
          (t (cr-error '* x))
  ) )
  ;; pn = product of the rational factors
  ;; rl = list of the c-real factors
  (when (or (eql pn 0) (null rl)) (return-from *r pn))
  ;; If pn is a true fraction, handle it like a c-real.
  (unless (integerp pn) (setq rl (cons pn rl) pn 1))
  (let ((y (* (length rl) (abs pn))) (al nil) (nl nil) (ns 1) ll)
    (dolist (x rl)
      (multiple-value-bind (a0 n0) (raw-approx-cr x)
        (setq al (cons (1+ a0) al)
              nl (cons n0 nl)
              y (* y (1+ a0))
              ns (- ns n0)
    ) ) )
    (setq ll (mapcar #'(lambda (z m)
                         (+ m ns (integer-length (1- (ceiling y z))))
                       )
                     al nl
             )
          rl (nreverse rl)
    )
    ;; rl = list of the factors (not including the integer pn)
    ;; ll = list of the corresponding precision differences
    ;; nl = list of the correspodning minimum precisions
    (make-real
      #'(lambda (k)
          (let ((erg pn) (s (- k)) (rl rl) (ll ll) (nl nl) k1)
            (loop (setq k1 (max (first nl) (+ k (first ll)))
                        s (+ s k1)
                        erg (* erg (get-approx (first rl) k1))
                        rl (rest rl)
                        ll (rest ll)
                        nl (rest nl)
                  )
                  (when (null rl) 
                    (return (if (minusp s)
                              0
                              (round-cr erg s)
) ) )   ) ) )     ) )       )

;;; Reciprocal:

(defun invert-r (x)
  (cond ((rationalp x) (/ x))
        ((c-real-p x)
         (multiple-value-bind (a0 n0) (raw-approx-cr x)
           (when (eql a0 0) (error "division by 0"))
           (let ((k1 (+ 4 (* 2 (- n0 (integer-length (1- a0))))))
                 (k2 (1+ n0))
                )
             (make-real #'(lambda (k &aux (k0 (max k2 (+ k k1))))
                            (round (ash 1 (+ k k0)) (get-approx x k0))
        )) ) )            )
        (t (cr-error '/ x))
) )

;;; Division:

(defun /R (x1 &rest args) "division for CREALs"
  (if (null args)
    (invert-r x1)
    (*r x1 (invert-r (apply #'*r args)))
) )

;;; Square root:

(defun SQRT-R (x &aux s) "square root for CREALs"
  (unless (creal-p x) (cr-error 'sqrt x))
  (if (and (rationalp x) (>= x 0) (rationalp (setq s (sqrt x))))
    s
    (multiple-value-bind (a0 n0 s) (raw-approx-cr x)
      (unless (plusp s)
        (error "~S: attempting to compute the square root of a negative number"
               'sqrt-r
      ) )
      (let ((k1 (1+ (ceiling (- n0 (integer-length (1- a0))) 2)))
            (n1 (ceiling n0 2))
           )
        (make-real
          #'(lambda (k &aux (k2 (max n1 (ceiling (+ k k1) 2)))
                            (k3 (max 0 (- k -2 k1)))
                    )
              (round-cr (isqrt (ash (get-approx x (* 2 k2)) (* 2 k3)))
                        (+ k3 k2 (- k))
) ) ) ) )   ) )

;;; Now comes a round function.
;;; (round-r x y l) (x, y creal, l int>=0) returns two values q and r,
;;; where q is an integer and r a creal, so that x = q*y + r and
;;; |r| <= (1/2+2^(-l))*|y|. The default value of l is such that |r| exceeds
;;; |y|/2 by at most 2^(- *CREAL-TOLERANCE*).
;;; The third argument is specified only for internal purposes.

(defun ROUND-R (x &optional (y 1) (l nil)) "round for CREALs"
  (divide-r 'round #'round x y l)
)

(defun FLOOR-R (x &optional (y 1) (l nil)) "floor for CREALs"
  (divide-r 'floor #'floor x y l)
)

(defun CEILING-R (x &optional (y 1) (l nil)) "ceiling for CREALs"
  (divide-r 'ceiling #'ceiling x y l)
)

(defun TRUNCATE-R (x &optional (y 1) (l nil)) "truncate for CREALs"
  (divide-r 'truncate #'truncate x y l)
)

(defun divide-r (name what x y l)
  ; name = name of the calling function
  ; what = #'round, #'floor, #'ceiling or #'truncate
  (unless (creal-p x) (cr-error name x))
  (unless (creal-p y) (cr-error name y))
  (if (and (rationalp x) (rationalp y))
    (funcall what x y)    ; for rational numbers use the common function
    (multiple-value-bind (a0 n0) (raw-approx-cr y)
      (when (eql a0 0) (error "~S: division by 0" name))
      (when (null l)
        (setq l (+ (integer-length a0) *creal-tolerance* (- n0)))
      )
      (let* ((x1 (abs (get-approx x n0)))
             (m (max n0 (+ l 2 n0 (integer-length (+ x1 a0 -1))
                           (* -2 (integer-length (1- a0)))
             )  )       )
             (q (funcall what (get-approx x m) (get-approx y m)))
            )
        (values q (rest-help-r x y (- q)))
) ) ) )

;; (rest-help-r x y q), with x,y creal, q integer, computes x + q*y.

(defun rest-help-r (x y q)
  (declare (type creal x y) (type integer q))
  (if (eql q 0)
    x
    (let ((k1 (1+ (integer-length (1- (abs q))))))
      (make-real
        #'(lambda (k)
            (round-cr (+ (ash (get-approx x (+ k 2)) (- k1 2))
                         (* q (get-approx y (+ k k1)))
                      )
                      k1
) ) ) )   ) )

;;; Now comes the arithmetic shift function for infinite binary fractions:

(defun ASH-R (x n) "shift function for CREALs"
  (unless (creal-p x) (cr-error 'ash-r x))
  (unless (integerp n) (int-error 'ash-r n))
  (cond ((eql n 0) x)
        ((integerp x)
         (if (plusp n) (ash x n) (/ x (ash 1 (- n))))
        )
        ((rationalp x)
         (if (plusp n)
           (/ (ash (numerator x) n) (denominator x))
           (/ (numerator x) (ash (denominator x) (- n)))
        ))
        ((plusp n) (make-real #'(lambda (k) (get-approx x (+ k n)))))
        (t (make-real #'(lambda (k)
                          (if (minusp (+ k n))
                            (round-cr (get-approx x 0) (- (+ k n)))
                            (get-approx x (+ k n))
) )     )  )            ) )

;;; Now we look at the most important transcendental functions.

;;; (log-r2 x) takes a creal x |x|<=1/2 and returns log((1+x)/(1-x)) as creal.
;;;   log((1+x)/(1-x)) = 2*(x + x^3/3 + x^5/5 + ... )

(defun log-r2 (x)
  (declare (type creal x))
  (if (eql x 0)
    0
    (make-real
      #'(lambda (k)
          (let* ((k0 (integer-length (1- (integer-length k))))
                     ; k0 = extra precision needed for partial sums
                 (k1 (+ k k0 1)) ; k1 = total precision needed
                                 ; (+1 because of factor 2)
                 (ax (get-approx x (1+ k1)))
                 (fx (round ax 2)) ; fx = k1-approximation of x
                 (fx2 (round-cr (* ax ax) (+ k1 2))) ; fx2 = dito of x^2
                )
            (do ((n 1 (+ n 2))
                 (y fx (round-cr (* y fx2) k1))
                 (erg 0 (+ erg (round y n)))
                )
                ((< (abs y) n) (round-cr erg k0))
) ) )   ) ) )

;;; Now log2:

(defconstant LOG2-R (+r (ash-r (log-r2 1/7) 1) (log-r2 1/17))
  "log(2) as CREAL"
)

(get-approx log2-r 200)  ; precompute to ca. 60 decimal digits

;;; (log-r1 x) takes a creal x from [1,2] and returns log(x) as creal

(defun log-r1 (x)
  (declare (type creal x))
  (log-r2 (transf x))
)

;;; (transf x) takes a creal x from [1,2] and returns (x-1)/(x+1) as creal

(defun transf (x)
  (declare (type creal x))
  (if (rationalp x)
    (/ (1- x) (1+ x))
    (make-real #'(lambda (k)
                   (let ((a (get-approx x k)) (e (ash 1 k)))
                     (round (ash (- a e) k) (+ a e))
) ) )            ) )

;;; Now the logarithm.

(defun LOG-R (x &optional (b nil)) "logarithm for CREALs"
  (unless (creal-p x) (cr-error 'log x))
  (unless (or (null b) (creal-p b)) (cr-error 'log b))
  (if b
    (/r (log-r x) (log-r b))
    ;; remember log(2^n * a) = n*log(2) + log(a)
    (multiple-value-bind (a0 n0 s) (raw-approx-cr x)
      (unless (plusp s)
        (error "~S: attempt to compute the logarithm of a nonpositive number"
               'log-r
      ) )
      (let ((shift (- (integer-length a0) 1 n0)))
        (rest-help-r (log-r1 (ash-r x (- shift))) log2-r shift)
) ) ) )

;;; Now the exponential function.

;;; (exp-r1 x) takes a creal x with |x| <= 1/2*log(2)
;;; and returns exp(x) as creal

(defun exp-r1 (x)
  (declare (type creal x))
  (make-real
    #'(lambda (k)
        (let ((m 3) (k2 (+ k 3)))
          (loop (when (<= k2 (ash (- m 2) m)) (return))
                (incf m)
          )
          (setq m (+ m 3) k2 (+ k m))
          (do ((x1 (get-approx x k2))
               (n 1 (1+ n))
               (y (ash 1 k2) (round-cr (round (* y x1) n) k2))
               (erg 0 (+ erg y))
              )
              ((eql y 0) (round-cr erg m))
) )   ) ) )

(defun EXP-R (x) "exponential function for CREALs"
  (unless (creal-p x) (cr-error 'exp x))
  ;; remember exp(a*log2 + b) = exp(b) * 2^a
  (if (eql x 0)
    1
    (multiple-value-bind (q r) (round-r x log2-r 10)
      (ash-r (exp-r1 r) q)
) ) )

;;; (expt-r x y) takes creals x,y and computes x^y

(defun EXPT-R (x y &aux s) "exponentiation function for CREALs"
  (unless (creal-p x) (cr-error 'expt x))
  (unless (creal-p y) (cr-error 'expt y))
  (cond ((eql y 0) 1)
        ((integerp y)
         (if (rationalp x) (expt x y) (expt-r1 x y))
        )
        ((and (rationalp y)
              (eql 2 (denominator y))
              (rationalp x)
              (rationalp (setq s (sqrt x)))
         )
         (expt s (* 2 y))
        )
        (t (exp-r (*r y (log-r x))))
) )

(defun expt-r1 (x y)
  (declare (type creal x) (integer y))
  (cond ((minusp y) (expt-r1 (invert-r x) (- y)))
        ((eql y 1) x)
        ((evenp y) (expt-r1 (*r x x) (floor y 2)))
        (t (*r x (expt-r1 (*r x x) (floor y 2))))
) )

;;; Now the trigonometric functions.

;;; (atan-r1 x) takes a creal x with |x| <= 1/2 and returns atan(x) as creal

(defun atan-r1 (x)
  (declare (type creal x))
  (if (eql x 0)
    0
    (make-real
      #'(lambda (k)
          (let* ((k0 (integer-length (1- (integer-length k))))
                     ; k0 = extra precision needed for partial sums
                 (k1 (+ k k0)) ; k1 = total precision needed
                 (ax (get-approx x (1+ k1)))
                 (fx (round ax 2)) ; fx = k1-approximation of x
                 (fx2 (- (round-cr (* ax ax) (+ k1 2)))) ; fx2 = dito of -x^2
                )
            (do ((n 1 (+ n 2))
                 (y fx (round-cr (* y fx2) k1))
                 (erg 0 (+ erg (round y n)))
                )
                ((< (abs y) n) (round-cr erg k0))
) ) )   ) ) )

;;; Now pi:

(defconstant PI-R (-r (ash-r (atan-r1 1/10) 5)
                      (ash-r (atan-r1 1/515) 4)
                      (ash-r (atan-r1 1/239) 2)
                  )
  "pi as CREAL"
)

(defconstant 2PI-R (ash-r pi-r 1) "2*pi as CREAL")
(defconstant PI/2-R (ash-r pi-r -1) "pi/2 as CREAL")
(defconstant PI/4-R (ash-r pi-r -2) "pi/4 as CREAL")

(get-approx 2pi-r 200) ; compute to ca. 60 decimal digits
(get-approx pi-r 200)
(get-approx pi/2-r 200)
(get-approx pi/4-r 200)

;;; (atan-r0 x) takes a creal x and returns atan(x) as creal.

(defun atan-r0 (x)
  (declare (type creal x))
  (let ((a (get-approx x 3)))
    (cond ((<= -3 a 3) (atan-r1 x))
          ((< a -3) (minus-r (atan-r0 (minus-r x)))) ; atan(x) = -atan(-x)
          ((< 3 a 17) (+r pi/4-r (atan-r1 (transf x))))
                    ; atan(x) = pi/4 + atan((x-1)/(x+1))
          (t (-r pi/2-r (atan-r1 (invert-r x)))) ; atan(x) = pi/2 - atan(1/x)
) ) )

;;; (atan-r x [y]) computes the arctangent of the creals x (and y if given)

(defun ATAN-R (x &optional (y nil)) "arctangent for CREALs"
  (unless (creal-p x) (cr-error 'atan x))
  (unless (or (null y) (creal-p y)) (cr-error 'atan y))
  (if (null y)
    (atan-r0 x)
    (multiple-value-bind (ax nx sx) (raw-approx-cr x)
      (multiple-value-bind (ay ny sy) (raw-approx-cr y)
        (when (and (eql 0 sx) (eql 0 sy))
          (error "~S: both arguments should not be zero"
                 'atan
        ) )
        (let ((mx-my (+ (integer-length ax) ny 
                        (- (integer-length ay)) (- nx)
             ))      )
          (cond ((and (plusp sx) (>= mx-my 0)) (atan-r0 (/r y x)))
                ((and (plusp sy) (<= mx-my 0))
                 (-r pi/2-r (atan-r0 (/r x y)))
                )
                ((and (minusp sy) (<= mx-my 0))
                 (minus-r (+r (atan-r0 (/r x y)) pi/2-r))
                )
                ((and (minusp sx) (minusp sy) (>= mx-my 0))
                 (-r (atan-r0 (/r y x)) pi-r)
                )
                (t (+r (atan-r0 (/r y x)) pi-r))
) ) ) ) ) )

;;; (sin-r1 x) takes a creal x with |x|<4 and returns sin(x) as creal.

(defun sin-r1 (x)
  (declare (type creal x))
  (make-real
    #'(lambda (k)
        (let ((m 3) (k2 (+ k 3)))
          (loop (when (<= k2 (ash (- m 2) m)) (return))
                (incf m)
          )
          (setq m (+ m 4) k2 (+ k m))
          (let ((x0 (get-approx x k2)))
            (do ((x1 (- (round-cr (* x0 x0) k2)))
                 (n 2 (+ n 2))
                 (y x0 (round-cr (round (* y x1) (* n (1+ n))) k2))
                 (erg 0 (+ erg y))
                )
                ((eql y 0) (round-cr erg m))
) )   ) ) ) )

(defun SIN-R (x) "sine for CREALs"
  (unless (creal-p x) (cr-error 'sin x))
  ;; remember sin(k*2pi + y) = sin(y)
  (if (eql x 0)
    0
    (multiple-value-bind (q r) (round-r x 2pi-r 10)
      (declare (ignore q))
      (sin-r1 r)
) ) )

;;; (cos-r1 x) takes a creal x with |x|<4 and returns cos(x) as creal.

(defun cos-r1 (x)
  (declare (type creal x))
  (make-real
    #'(lambda (k)
        (let ((m 3) (k2 (+ k 3)))
          (loop (when (<= k2 (ash (- m 2) m)) (return))
                (incf m)
          )
          (setq m (+ m 4) k2 (+ k m))
          (let ((x0 (get-approx x k2)))
            (do ((x1 (- (round-cr (* x0 x0) k2)))
                 (n 1 (+ n 2))
                 (y (ash 1 k2) (round-cr (round (* y x1) (* n (1+ n))) k2))
                 (erg 0 (+ erg y))
                )
                ((eql y 0) (round-cr erg m))
) )   ) ) ) )

(defun COS-R (x) "cosine for CREALs"
  (unless (creal-p x) (cr-error 'cos x))
  ;; remember cos(k*2pi + y) = cos(y)
  (if (eql x 0)
    1
    (multiple-value-bind (q r) (round-r x 2pi-r 10)
      (declare (ignore q))
      (cos-r1 r)
) ) )

(defun TAN-R (x) "tangent for CREALs"
  (unless (creal-p x) (cr-error 'tan x))
  (/r (sin-r x) (cos-r x))
)
