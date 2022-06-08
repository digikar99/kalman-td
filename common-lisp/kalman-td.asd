
(defsystem "kalman-td"
  :depends-on ("polymorphic-functions"
               "dense-numericals"
               "alexandria"
               "defclass-std"
               "dense-arrays+magicl"
               "metabang-bind")
  :serial t
  :components ((:file "py4cl")
               (:file "kalman-td")
               (:file "simulations-helper")
               (:file "simulations")))
