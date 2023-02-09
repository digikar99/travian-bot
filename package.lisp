(defpackage :travian-bot
  (:use :py4cl2 :cl :alexandria :reader :iterate)
  (:local-nicknames (:json :shasht))
  (:export #:start-bot
           #:login

           #:get-page
           #:ensure-page

           #:remaining-build-duration
           #:notify-when-build-complete
           #:upgrade-building
           #:upgrade-resource-field
           #:upgrade
           #:adventure-send

           #:enqueue-upgrades
           #:start-build-thread))

(in-package :travian-bot)
