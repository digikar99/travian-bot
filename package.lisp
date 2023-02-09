(defpackage :travian-bot
  (:use :py4cl2 :cl :alexandria :reader :iterate)
  (:local-nicknames (:json :shasht))
  (:export #:start-bot
           #:login
           #:get-page
           #:ensure-page
           #:upgrade-building
           #:remaining-build-duration
           #:notify-when-build-complete
           #:upgrade-resource-field
           #:upgrade-resource-fields
           #:adventure-send))

(in-package :travian-bot)
