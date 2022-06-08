
(in-package :kalman-td)

(defun prepare-stimuli-rewards (all-stimuli-types num-trials paradigm)
  "
    ALL-STIMULI-TYPES should be a list and is used to map the stimuli to their
      one-hot-encoding; order in which stimuli is listed in ALL-STIMULI-TYPES matters.

    Returns two values: STIMULI REWARDS

    PARADIGM should be a list of symbols comprising of the symbols in
      ALL-STIMULI-TYPES combined using the following special symbols:
        > for right-arrow, for separating
        / for forward-slash (same as paper)
        % for semicolon
        + for reward present (same as paper)
        - for reward absent (same as paper)
  "
  (declare (optimize debug))
  (let* ((num-stimuli-types (size all-stimuli-types))
         (stimuli-types (set-difference (remove-duplicates paradigm :test #'string=)
                                        '(> / % + -)
                                        :test #'string=))
         (num-actual-stimuli-types (size stimuli-types)))
    (assert (>= num-stimuli-types num-actual-stimuli-types)
            ()
            "Inferred number of stimuli types exceeds provided number of stimuli types")

    (let ((trial-types (split-sequence '% paradigm :test #'string=)))
      (iter outer
        (for trial-type in trial-types)
        (iter sub-outer
          (for subtrial-type in (split-sequence '/ trial-type :test #'string=))
          (for subtrial-components = (split-sequence '> subtrial-type :test #'string=))
          (for expectation = (lastcar subtrial-components))
          (if (member (first expectation) '(+ -) :test #'string=)
              (setq subtrial-components (butlast subtrial-components)
                    expectation (first expectation))
              (setq expectation '-))
          (iter (for subtrial-component in subtrial-components)
            (for one-hot-encoding = (let ((ohe (zeros num-stimuli-types 1)))
                                      (iter (for letter in subtrial-component)
                                        (setf (aref ohe
                                                    (position letter all-stimuli-types
                                                              :test #'string=)
                                                    0)
                                              (coerce 1 dense-arrays::default-element-type)))
                                      ohe))
            (in sub-outer
                (collect one-hot-encoding into single-trial-stimuli)
                (collect '(0) into single-trial-rewards)))
          (collect (zeros num-stimuli-types 1) into single-trial-stimuli)
          (collect (eswitch (expectation :test #'string=)
                     ('+ '(1))
                     ('- '(0)))
            into single-trial-rewards)
          (finally
           (in outer
               (appending (make-list num-trials :initial-element single-trial-stimuli)
                 into all-stimuli)
               (appending (make-list num-trials :initial-element (mapcar #'list single-trial-rewards))
                 into all-rewards))))
        (finally (return-from prepare-stimuli-rewards
                   (values (apply #'concat all-stimuli)
                           (apply #'concat all-rewards))))))))



(defun process-stimuli-rewards (ktd stimuli rewards &key callback)
  "
    CALLBACK : function, called after each trial, and its results are accumulated
      into a list which forms the return value of this function.
      The function should take two arguments:
      - first is the ktd supplied,
      - and, second is the expected_reward
  "
  (declare (type kalman-td ktd))
  (iter (for i from 1 below (shape rewards 0))
    (for expected-reward
         = (predict-reward ktd
                           (aref stimuli i)
                           (aref stimuli (1- i))))
    (update ktd
            expected-reward
            (aref rewards i)
            (aref stimuli i)
            (aref stimuli (1- i)))
    (when callback
      (collect (funcall callback ktd expected-reward)
        into accumulated-values))
    (finally (return-from process-stimuli-rewards
               accumulated-values))))


