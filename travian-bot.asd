(defsystem "travian-bot"
  :description "A bot primarily made for infinite build queue."
  :depends-on ("py4cl2-cffi"
               "alexandria"
               "pathname-utils"
               "iterate"
               "reader"
               "shasht"
               "str"
               "uiop")
  :components ((:file "package")
               (:file "py4cl2")
               (:file "config")
               (:file "travian-bot")))
