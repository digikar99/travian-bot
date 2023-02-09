(in-package :travian-bot)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *bot-home-dir*
    (asdf:component-pathname (asdf:find-system "travian-bot")))
  (py4cl2:py-cd (namestring *bot-home-dir*)))

(py4cl2:defpymodule "undetected_chromedriver" nil :lisp-package "UC")
(py4cl2:defpymodule "selenium" nil :lisp-package "SELENIUM")
(py4cl2:defpymodule "selenium.webdriver.remote.webdriver" nil :lisp-package "BY")
