(polymorphic-functions.defpackage:defpackage kalman-td
  (:shadowing-import-exported-symbols :dense-numericals)
  (:use
   :excl
   :defclass-std
   :split-sequence
   :metabang.bind)
  (:export #:kalman-td
           #:predict-reward
           #:update))

(in-package :kalman-td)

(setf (assoc-value *array-element-type-alist* (find-package :kalman-td)) 'single-float)

(defun @ (&rest args) (apply #'dense-arrays:magicl-funcall #'magicl:@ args))

(defclass/std kalman-td ()
  ((num-weights
    weights-mean
    weights-covariance
    weights-tau
    weights-covariance-noise
    discount-rate
    sigma-w
    sigma-r
    old-kalman-gain)))

(defun make-kalman-td-instance (num-stimulus-features
                                &key weights-tau discount-rate sigma-w sigma-r)
  (let ((*array-element-type* 'single-float))
    (make-instance 'kalman-td
                   :weights-tau weights-tau
                   :discount-rate discount-rate
                   :sigma-w (coerce sigma-w 'single-float)
                   :sigma-r (coerce sigma-r 'single-float)

                   :num-weights num-stimulus-features
                   :weights-mean (zeros num-stimulus-features 1)
                   :weights-covariance (* sigma-w sigma-w
                                          (eye num-stimulus-features))
                   :weights-covariance-noise (* weights-tau weights-tau
                                                (eye num-stimulus-features)))))

(defun default-kalman-td (num-stimulus-features)
  (make-kalman-td-instance num-stimulus-features
                           :discount-rate 0.98
                           :weights-tau 0.1
                           :sigma-r 1
                           :sigma-w 1))

(defun compute-discounted-time-derivative (kalman-td stimulus-features-new stimulus-features-old)
  (declare (type (array single-float) stimulus-features-new stimulus-features-old))
  (with-slots (discount-rate) kalman-td
    (declare (type single-float discount-rate))
    (- stimulus-features-old (* discount-rate stimulus-features-new))))

(defun predict-reward (ktd stimulus-features-new stimulus-features-old)
  (optima.extra:let-match*
      ((discounted-time-derivative (compute-discounted-time-derivative
                                    ktd stimulus-features-new stimulus-features-old))
       ((list ns1 _) (shape (slot-value ktd 'weights-mean)))
       ((list ns2 _) (shape discounted-time-derivative)))
    (assert (= ns1 ns2)
            ()
            "Weights and stimulus are of incompatible shape: ~S ~S"
            (shape (slot-value ktd 'weights-mean))
            (shape discounted-time-derivative))
    (with-slots ((wc weights-covariance)
                 (wm weights-mean)
                 (wcn weights-covariance-noise))
        ktd
      (+ wc wcn :out wc)
      (row-major-aref (@ (transpose wm) discounted-time-derivative)
                      0))))

(defun update (ktd expected-reward actual-reward stimulus-features-new stimulus-features-old)
  "
    Update the weights, given the actual reward. We are taking
    expected_reward as an argument, because the weights_cov change in
    the mere process of calculating it.
  "
  (with-slots ((wm weights-mean)
               (wc weights-covariance)
               (okg old-kalman-gain))
      ktd
    ;; (break)
    (let* ((discounted-time-derivative (compute-discounted-time-derivative
                                        ktd stimulus-features-new stimulus-features-old))
           (delta (- actual-reward expected-reward))

           (numer (@ wc discounted-time-derivative))
           (denom (+ (row-major-aref (@ (transpose discounted-time-derivative)
                                        wc
                                        discounted-time-derivative)
                                     0)
                     (expt (slot-value ktd 'sigma-r) 2)))
           (kalman-gain (/ numer denom)))

      (setf wm (+ wm (* kalman-gain delta)))
      (setf wc (- wc (@ kalman-gain
                        (transpose discounted-time-derivative)
                        wc)))
      (setf okg kalman-gain)
      t)))
