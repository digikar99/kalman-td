(in-package :kalman-td)

(defun simulate (name) (funcall (symbolicate 'simulate '- name)))


;; A basic demonstration, without using simulations-helper.lisp
(defun simulate-latent-inhibition ()

  (let ((*array-element-type* 'single-float))
    (let ((control-ktd      (default-kalman-td 1))
          (experimental-ktd (default-kalman-td 1)))

      (loop :for i :from 1 :below 20
            :with preex-stimulus
              := (asarray (loop :repeat 10
                                :appending (list (asarray '((1))) (asarray '((0))))))
            :with preex-reward
              := (asarray (loop :repeat 10
                                :appending (list (asarray '((0))) (asarray '((0))))))
            :for expected-reward
              := (predict-reward experimental-ktd
                                 (aref preex-stimulus i)
                                 (aref preex-stimulus (1- i)))
            :do (update experimental-ktd
                        expected-reward
                        (aref preex-reward i)
                        (aref preex-stimulus i)
                        (aref preex-stimulus (1- i))))

      (iter (for i from 1 below 20)

        (with exp-stimulus = (asarray (loop :repeat 10
                                            :appending (list (asarray '((1))) (asarray '((0)))))))
        (with exp-reward = (asarray (loop :repeat 10
                                          :appending (list (asarray '((0))) (asarray '((1)))))))

        (for exp-expected-reward = (predict-reward experimental-ktd
                                                   (aref exp-stimulus i)
                                                   (aref exp-stimulus (1- i))))
        (update experimental-ktd
                exp-expected-reward
                (aref exp-reward i)
                (aref exp-stimulus i)
                (aref exp-stimulus (1- i)))
        (collect exp-expected-reward into exp-expected-rewards)

        (for ctrl-expected-reward = (predict-reward control-ktd
                                                    (aref exp-stimulus i)
                                                    (aref exp-stimulus (1- i))))
        (update control-ktd
                ctrl-expected-reward
                (aref exp-reward i)
                (aref exp-stimulus i)
                (aref exp-stimulus (1- i)))
        (collect ctrl-expected-reward into ctrl-expected-rewards)

        (finally
         (setq exp-expected-rewards  (asarray exp-expected-rewards))
         (setq ctrl-expected-rewards (asarray ctrl-expected-rewards))
         (plt:plot (alexandria:iota 10 :start 1)
                   (as-cl-array (aref exp-expected-rewards '(0 :step 2)))
                   :label "Pre Group")
         (plt:plot (alexandria:iota 10 :start 1)
                   (as-cl-array (aref ctrl-expected-rewards '(0 :step 2)))
                   :label "No-pre Group")
         (plt:title :label "Latent Inhibition aka Preexposure Effect")
         (plt:xlabel :xlabel "Stimulus")
         (plt:ylabel :ylabel "Expected Reward")
         (plt:legend)
         (plt:show)

         (print (list exp-expected-rewards
                      ctrl-expected-rewards)))))))


;;; All the functions this point forward rely on simulations-helper.lisp
(defun simulate-latent-inhibition ()
  "
    Phase 1: X -> - for experimental, none for control
    Phase 2: X -> + for both
  "
  (declare (optimize debug))

  (let* ((control-ktd      (default-kalman-td 1))
         (experimental-ktd (default-kalman-td 1)))

    (multiple-value-bind (preex-stimuli preex-rewards)
        (prepare-stimuli-rewards '(a) 10 '(a > -))
      (process-stimuli-rewards experimental-ktd preex-stimuli preex-rewards))

    (multiple-value-bind (exp-stimuli exp-rewards)
        (prepare-stimuli-rewards '(a) 10 '(a > +))
      (let ((ctrl-expected-rewards
              (process-stimuli-rewards experimental-ktd exp-stimuli exp-rewards
                                       :callback (lambda (ktd exp-reward)
                                                   (declare (ignore ktd))
                                                   exp-reward)))
            (exp-expected-rewards
              (process-stimuli-rewards control-ktd      exp-stimuli exp-rewards
                                       :callback (lambda (ktd exp-reward)
                                                   (declare (ignore ktd))
                                                   exp-reward))))

        (setq exp-expected-rewards  (asarray exp-expected-rewards))
        (setq ctrl-expected-rewards (asarray ctrl-expected-rewards))
        (plt:plot (alexandria:iota 10 :start 1)
                  (as-cl-array (aref exp-expected-rewards '(0 :step 2)))
                  :label "Pre Group")
        (plt:plot (alexandria:iota 10 :start 1)
                  (as-cl-array (aref ctrl-expected-rewards '(0 :step 2)))
                  :label "No-pre Group")
        (plt:title :label "Latent Inhibition aka Preexposure Effect")
        (plt:xlabel :xlabel "Stimulus")
        (plt:ylabel :ylabel "Expected Reward")
        (plt:legend)
        (plt:show)

        (print (list exp-expected-rewards
                     ctrl-expected-rewards))))))



(defun simulate-overshadowing-and-unovershadowing ()

  (bind ((*array-element-type* 'single-float)
         (control-ktd      (default-kalman-td 2))
         (experimental-ktd (default-kalman-td 2))
         ((:values pre-stimuli pre-rewards)   (prepare-stimuli-rewards '(a b) 10 '(a > -)))
         ((:values p1-stimuli p1-rewards)     (prepare-stimuli-rewards '(a b) 10 '(a b > +)))
         ((:values test-stimuli test-rewards) (prepare-stimuli-rewards '(a b) 01 '(b > +))))

    (process-stimuli-rewards experimental-ktd pre-stimuli pre-rewards)

    (process-stimuli-rewards experimental-ktd p1-stimuli p1-rewards)
    (process-stimuli-rewards control-ktd      p1-stimuli p1-rewards)

    (bind ((ctrl-test-reward (nth 0 (process-stimuli-rewards control-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward))))
           (exp-test-reward  (nth 0 (process-stimuli-rewards experimental-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward)))))

      (plt:clf)
      (plt:bar :x '(1 2)
               :height (list ctrl-test-reward exp-test-reward))
      (plt:xticks :ticks '(1 2)
                  :labels '("overshadowed" "unovershadowed"))
      (plt:ylabel :ylabel "Reward Expectation")
      (plt:title :label "Reversal of overshadowing")
      (plt:show))))



(defun simulate-forward-blocking ()
  "
    Phase 1: A -> +
    Phase 2: AB -> +
    Phase 3: A -> - (only for unblocking  group)
    Test : B -> ?
  "

  (bind ((experimental-ktd (default-kalman-td 2))
         (control-ktd      (default-kalman-td 2))
         ((:values p1-stimuli p1-rewards)     (prepare-stimuli-rewards '(a b) 10 '(a > +)))
         ((:values p2-stimuli p2-rewards)     (prepare-stimuli-rewards '(a b) 10 '(a b > +)))
         ((:values p3-stimuli p3-rewards)     (prepare-stimuli-rewards '(a b) 10 '(a > -)))
         ((:values test-stimuli test-rewards) (prepare-stimuli-rewards '(a b) 01 '(b > -))))

    (process-stimuli-rewards experimental-ktd p1-stimuli p1-rewards)
    (process-stimuli-rewards control-ktd      p1-stimuli p1-rewards)

    (process-stimuli-rewards experimental-ktd p2-stimuli p2-rewards)
    (process-stimuli-rewards control-ktd      p2-stimuli p2-rewards)

    (process-stimuli-rewards control-ktd      p3-stimuli p3-rewards)

    (bind ((exp-test-reward  (nth 0 (process-stimuli-rewards experimental-ktd test-stimuli test-rewards
                                                              :callback (lambda (ktd exp-reward)
                                                                          (declare (ignore ktd))
                                                                          exp-reward))))
           (ctrl-test-reward (nth 0 (process-stimuli-rewards control-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward)))))
      (plt:clf)
      (plt:bar :x '(1 2) :height (list exp-test-reward ctrl-test-reward))
      (plt:xticks :ticks '(1 2) :labels '("Block" "No-Block"))
      (plt:ylabel :ylabel "Value")
      (plt:title :label "Forward Blocking")
      (plt:show))))



(defun simulate-overexpectation ()
  "
    Phase 1: A -> + / B -> +
    Phase 2: AB -> +
    Phase 3: A -> - (only experimental group)
    Test : B -> ?
  "

  (bind ((experimental-ktd (default-kalman-td 2))
         (control-ktd      (default-kalman-td 2))
         ((:values p1-stimuli p1-rewards)     (prepare-stimuli-rewards '(a b) 10 '(a > + / b > +)))
         ((:values p2-stimuli p2-rewards)     (prepare-stimuli-rewards '(a b) 10 '(a b > +)))
         ((:values p3-stimuli p3-rewards)     (prepare-stimuli-rewards '(a b) 10 '(a > -)))
         ((:values test-stimuli test-rewards) (prepare-stimuli-rewards '(a b) 01 '(b > -))))

    (process-stimuli-rewards experimental-ktd p1-stimuli p1-rewards)
    (process-stimuli-rewards control-ktd      p1-stimuli p1-rewards)

    (process-stimuli-rewards experimental-ktd p2-stimuli p2-rewards)
    (process-stimuli-rewards control-ktd      p2-stimuli p2-rewards)

    (process-stimuli-rewards experimental-ktd p3-stimuli p3-rewards)

    (bind ((exp-test-reward  (nth 0 (process-stimuli-rewards experimental-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward))))
           (ctrl-test-reward (nth 0 (process-stimuli-rewards control-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward)))))
      (plt:clf)
      (plt:bar :x '(1 2) :height (list ctrl-test-reward exp-test-reward))
      (plt:xticks :ticks '(1 2) :labels '("overexpectation" "unoverexpectation"))
      (plt:ylabel :ylabel "Reward Expectation")
      (plt:title :label "Reversal of overexpectation")
      (plt:show))))



(defun simulate-conditioned-inhibition ()
  "
    Phase 1: A -> +
    Phase 2: AB -> -
    Phase 3: A -> - (only uninhibition group)
    Test : B -> ?
  "

  (bind ((experimental-ktd (default-kalman-td 2))
         (control-ktd      (default-kalman-td 2))
         ((:values p1-stimuli p1-rewards)     (prepare-stimuli-rewards '(a b) 10 '(a > +)))
         ((:values p2-stimuli p2-rewards)     (prepare-stimuli-rewards '(a b) 10 '(a b > -)))
         ((:values p3-stimuli p3-rewards)     (prepare-stimuli-rewards '(a b) 10 '(a > -)))
         ((:values test-stimuli test-rewards) (prepare-stimuli-rewards '(a b) 01 '(b > -))))

    (process-stimuli-rewards control-ktd p1-stimuli p1-rewards)
    (multiple-value-bind (posterior-variances kalman-gains)

        (iter (for (variance gain) in
                   (process-stimuli-rewards experimental-ktd p1-stimuli p1-rewards
                                            :callback (lambda (ktd expected-reward)
                                                        (declare (ignore expected-reward))
                                                        (list (aref (slot-value ktd
                                                                                'weights-covariance)
                                                                    0 0)
                                                              (aref (slot-value ktd 'old-kalman-gain)
                                                                    0 0)))))
          (collect variance into variances)
          (collect gain into gains)
          (finally (return (values variances gains))))

      (plt:clf)
      (plt:plot (iota 9 :start 1) (as-cl-array (aref (asarray posterior-variances) '(1 :step 2))))
      (plt:xlabel :xlabel "Conditioning Trial")
      (plt:ylabel :ylabel "Posterior Variance (A)")
      (plt:title :label "Conditioned Inhibition")
      (plt:show)

      (plt:clf)
      (plt:plot (iota 9 :start 1) (as-cl-array (aref (asarray kalman-gains) '(1 :step 2))))
      (plt:xlabel :xlabel "Conditioning Trial")
      (plt:ylabel :ylabel "Kalman Gain (A)")
      (plt:title :label "Conditioned Inhibition")
      (plt:show))

    (process-stimuli-rewards experimental-ktd p2-stimuli p2-rewards)
    (process-stimuli-rewards control-ktd      p2-stimuli p2-rewards)

    (process-stimuli-rewards control-ktd      p3-stimuli p3-rewards)

    (bind ((exp-test-reward  (nth 0 (process-stimuli-rewards experimental-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward))))
           (ctrl-test-reward (nth 0 (process-stimuli-rewards control-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward)))))
      (plt:clf)
      (plt:bar :x '(1 2) :height (list exp-test-reward ctrl-test-reward))
      (plt:xticks :ticks '(1 2) :labels '("CI" "No-CI"))
      (plt:ylabel :ylabel "Value")
      (plt:title :label "Conditioned Inhibition")
      (plt:show))))



(defun simulate-serial-compound-extinction ()
  "
    Phase 1: Z -> X -> - / X -> +
    Phase 2: X -> - (only for experimental group)
    Test   : Z -> ?
  "

  (bind ((experimental-ktd (default-kalman-td 2))
         (control-ktd      (default-kalman-td 2))
         ((:values p1-stimuli p1-rewards)     (prepare-stimuli-rewards '(z x) 10
                                                                       '(z > x > - / x > +)))
         ((:values p2-stimuli p2-rewards)     (prepare-stimuli-rewards '(z x) 10 '(x > -)))
         ((:values test-stimuli test-rewards) (prepare-stimuli-rewards '(z x) 01 '(z > -))))

    (process-stimuli-rewards control-ktd      p1-stimuli p1-rewards)
    (process-stimuli-rewards experimental-ktd p1-stimuli p1-rewards)

    (process-stimuli-rewards experimental-ktd p2-stimuli p2-rewards)
    (bind ((exp-test-reward  (nth 0 (process-stimuli-rewards experimental-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward))))
           (ctrl-test-reward (nth 0 (process-stimuli-rewards control-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward)))))
      (plt:clf)
      (plt:bar :x '(1 2) :height (list exp-test-reward ctrl-test-reward))
      (plt:xticks :ticks '(1 2) :labels '("Ext" "No-Ext"))
      (plt:ylabel :ylabel "Value")
      (plt:title :label "Second Order Extinction")
      (plt:show))))



(defun simulate-overshadowing-and-second-order-conditioning ()
  "
    AX->+ /BY->+ ; A->- ; Z->X ; Z->?
    AX->+ /BY->+ ; B->- ; Z->X ; Z->?
  "
  (declare (optimize debug))
  (bind ((experimental-ktd (default-kalman-td 5))
         (control-ktd      (default-kalman-td 5))
         ((:values p1-stimuli p1-rewards) (prepare-stimuli-rewards '(a x b y z) 10
                                                                   '(a x > +   /   b y > +)))
         ((:values p2a-stimuli p2a-rewards)   (prepare-stimuli-rewards '(a x b y z) 10 '(a > -)))
         ((:values p2b-stimuli p2b-rewards)   (prepare-stimuli-rewards '(a x b y z) 10 '(b > -)))
         ((:values p3-stimuli p3-rewards)     (prepare-stimuli-rewards '(a x b y z) 10 '(z > x)))
         ((:values test-stimuli test-rewards) (prepare-stimuli-rewards '(a x b y z) 01 '(z > -))))

    (process-stimuli-rewards control-ktd      p1-stimuli p1-rewards)

    (multiple-value-bind (covariances gains)

        (iter (for (covariance gain)
                   in (process-stimuli-rewards experimental-ktd p1-stimuli p1-rewards
                                               :callback
                                               (lambda (ktd exp-reward)
                                                 (declare (ignore exp-reward))
                                                 (list (aref (slot-value ktd 'weights-covariance) 0 1)
                                                       (aref (slot-value ktd 'old-kalman-gain) 1 0)))))
          (collect covariance into covariances)
          (collect gain into gains)
          (finally (return (values (asarray covariances) (asarray gains)))))

      (plt:clf)
      (plt:plot (iota 10 :start 1) (as-cl-array (aref covariances '(0 :step 4))))
      (plt:ylabel :ylabel "Posterior Covariance (AX)")
      (plt:xlabel :xlabel "Phase 1 Trial")
      (plt:title :label "Overshadowing and Second Order Conditioning")
      (plt:show)

      (plt:clf)
      (plt:plot (iota 10 :start 1) (as-cl-array (aref gains '(0 :step 4))))
      (plt:ylabel :ylabel "Kalman Gain (X)")
      (plt:xlabel :xlabel "Phase 1 Trial")
      (plt:title :label "Overshadowing and Second Order Conditioning")
      (plt:show))

    (bind (((:values cov-ax kalman-gain-x-ctrl)
            (iter (for (covariance gain)
                       in (process-stimuli-rewards control-ktd p2a-stimuli p2a-rewards
                                                   :callback
                                                   (lambda (ktd exp-reward)
                                                     (declare (ignore exp-reward))
                                                     (list (aref (slot-value ktd
                                                                             'weights-covariance)
                                                                 0 1)
                                                           (aref (slot-value ktd
                                                                             'old-kalman-gain)
                                                                 1 0)))))
              (collect covariance into covariances)
              (collect gain into gains)
              (finally (return (values (asarray covariances) (asarray gains))))))
           (kalman-gain-x-expt
            (asarray
             (process-stimuli-rewards experimental-ktd p2b-stimuli p2b-rewards
                                      :callback (lambda (ktd exp-reward)
                                                  (declare (ignore exp-reward))
                                                  (aref (slot-value ktd 'old-kalman-gain) 1 0))))))

      (plt:clf)
      (plt:plot (iota 10 :start 1) (as-cl-array (aref cov-ax '(0 :step 2))))
      (plt:ylabel :ylabel "Posterior Covariance (AX)")
      (plt:xlabel :xlabel "Phase 2 Trial")
      (plt:title :label "Overshadowing and Second Order Conditioning")
      (plt:show)

      (plt:clf)
      (plt:plot (iota 10 :start 1)
                (as-cl-array (aref kalman-gain-x-ctrl '(0 :step 2)))
                :label "OV-A")
      (plt:plot (iota 10 :start 1)
                (as-cl-array (aref kalman-gain-x-expt '(0 :step 2)))
                :label "OV-B")
      (plt:ylabel :ylabel "Kalman Gain (X)")
      (plt:xlabel :xlabel "Phase 2 Trial")
      (plt:title :label "Overshadowing and Second Order Conditioning")
      (plt:show))

    (process-stimuli-rewards control-ktd      p3-stimuli p3-rewards)
    (process-stimuli-rewards experimental-ktd p3-stimuli p3-rewards)

    (bind ((exp-test-reward  (nth 0 (process-stimuli-rewards experimental-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward))))
           (ctrl-test-reward (nth 0 (process-stimuli-rewards control-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward)))))
      (plt:clf)
      (plt:bar :x '(1 2) :height (list ctrl-test-reward exp-test-reward))
      (plt:xticks :ticks '(1 2) :labels '("OV-A" "OV-B"))
      (plt:ylabel :ylabel "Value")
      (plt:title :label "Overshadowing and Second Order Conditioning")
      (plt:show))))



(defun simulate-serial-compound-extinction ()
  "
    Phase 1: Z -> X -> +
    Phase 2: X -> - (only for extinction group)
    Test   : Z -> ?
  "

  (bind ((experimental-ktd (default-kalman-td 2))
         (control-ktd      (default-kalman-td 2))
         ((:values p1-stimuli p1-rewards)     (prepare-stimuli-rewards '(z x) 10 '(z > x > +)))
         ((:values p2-stimuli p2-rewards)     (prepare-stimuli-rewards '(z x) 10 '(x > -)))
         ((:values test-stimuli test-rewards) (prepare-stimuli-rewards '(z x) 01 '(z > -))))

    (process-stimuli-rewards control-ktd p1-stimuli p1-rewards)
    (bind ((posterior-covariances
            (cons 0 (process-stimuli-rewards experimental-ktd p1-stimuli p1-rewards
                                             :callback (lambda (ktd expected-reward)
                                                         (declare (ignore expected-reward))
                                                         (aref (slot-value ktd
                                                                           'weights-covariance)
                                                               0 1))))))
      (plt:clf)
      (plt:plot (iota 10 :start 1) (as-cl-array (aref (asarray posterior-covariances) '(0 :step 3))))
      (plt:xlabel :xlabel "Conditioning Trial")
      (plt:ylabel :ylabel "Posterior Covariance (ZX)")
      (plt:title :label "Serial Compound Extinction")
      (plt:show))

    (process-stimuli-rewards experimental-ktd p2-stimuli p2-rewards)
    (bind ((exp-test-reward  (nth 0 (process-stimuli-rewards experimental-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward))))
           (ctrl-test-reward (nth 0 (process-stimuli-rewards control-ktd test-stimuli test-rewards
                                                             :callback (lambda (ktd exp-reward)
                                                                         (declare (ignore ktd))
                                                                         exp-reward)))))
      (plt:clf)
      (plt:bar :x '(1 2) :height (list exp-test-reward ctrl-test-reward))
      (plt:xticks :ticks '(1 2) :labels '("Ext" "No-Ext"))
      (plt:ylabel :ylabel "Value")
      (plt:title :label "Serial Compound Extinction")
      (plt:show))))


(defun simulate-recovery-from-overshadowing ()
  "
    Phase 1: Z -> X -> + for experimental ; B -> Y -> + for control
    Phase 2: Z -> - for experimental
    Test: X -> ? for experimental; Y -> ? for control
  "
  (declare (optimize debug))
  (bind ((control-ktd      (default-kalman-td 4))
         (experimental-ktd (default-kalman-td 4))
         ((:values p1e-stimuli p1e-rewards) (prepare-stimuli-rewards '(z x b y) 10 '(z > x > +)))
         ((:values p1c-stimuli p1c-rewards) (prepare-stimuli-rewards '(z x b y) 10 '(b > y > +)))
         ((:values p2-stimuli p2-rewards)   (prepare-stimuli-rewards '(z x b y) 10 '(z > -)))
         ((:values test-exp-stimuli)        (prepare-stimuli-rewards '(z x b y) 01 '(x > -)))
         ((:values test-ctrl-stimuli)       (prepare-stimuli-rewards '(z x b y) 01 '(y > -))))

    (process-stimuli-rewards control-ktd      p1c-stimuli p1c-rewards)
    (process-stimuli-rewards experimental-ktd p1e-stimuli p1e-rewards)

    (process-stimuli-rewards experimental-ktd p2-stimuli p2-rewards)

    (bind ((exp-test-reward (predict-reward experimental-ktd
                                            (aref test-exp-stimuli 1)
                                            (aref test-exp-stimuli 0)))
           (ctrl-test-reward (predict-reward control-ktd
                                             (aref test-ctrl-stimuli 1)
                                             (aref test-ctrl-stimuli 0))))
      (plt:clf)
      (plt:bar :x '(1 2)
               :height (list exp-test-reward ctrl-test-reward))
      (plt:xticks :ticks '(1 2) :labels '("X" "Y"))
      (plt:ylabel :ylabel "Value")
      (plt:title :label "Recovery from overshadowing")
      (plt:show))))
