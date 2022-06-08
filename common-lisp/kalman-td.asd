
(defsystem "kalman-td"
  :depends-on ("polymorphic-functions"
               "dense-numericals"
               "alexandria"
               "defclass-std"
               "dense-arrays+magicl"
               "metabang-bind")
  :components ((:file "py4cl")
               (:file "kalman-td")))
