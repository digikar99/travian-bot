(in-package :travian-bot)

(defvar *driver*)

(defun start-bot ()
  (setq *driver* (uc:chrome/class))
  (pymethod *driver* 'get (get-val *config* "server")))

(defun ensure-start-bot ()
  (unless (boundp '*driver*) (start-bot)))

(defun delay () (sleep (+ 2 (* 2 (random 1.0)))))

(defun find-unique-element (by value &optional (driver *driver*))
  (ensure-start-bot)
  (let ((fields (pymethod driver "find_elements" by value)))
    (unless (= 1 (length fields))
      (error "Non-unique fields~%  ~S~%encountered with by~%  ~S~%and value~%  ~S"
             fields by value))
    (get-val fields 0)))

(defun login (&optional (username (get-val *config* "username"))
                (password (get-val *config* "password")))
  (ensure-start-bot)
  (let ((username-field (find-unique-element "xpath" "//input[@name=\"name\"]"))
        (password-field (find-unique-element "xpath" "//input[@name=\"password\"]"))
        (login-button   (find-unique-element "xpath" "//button[@value=\"Login\"]")))
    (delay)
    (pymethod username-field "send_keys" username)
    (pymethod password-field "send_keys" password)
    (delay)
    (pymethod login-button "click")))

(defparameter *page-table*
  (alist-hash-table (list (cons :buildings "dorf2.php")
                          (cons :resources "dorf1.php")
                          (cons :adventures "hero/adventures")
                          (cons :tasks "tasks"))
                    :test #'equal))

(defun get-page (page-key-or-name)
  (let ((page-name (etypecase page-key-or-name
                     (string page-key-or-name)
                     (symbol (get-val *page-table* page-key-or-name)))))
    (pymethod *driver* "get" (uiop:strcat (get-val *config* "server")
                                          "/"
                                          page-name))))

(defun pagep (page-key-or-name)
  (let ((page-name (etypecase page-key-or-name
                     (string page-key-or-name)
                     (symbol (get-val *page-table* page-key-or-name)))))
    (str:ends-with-p page-name
                     (get-val (str:split #\?
                                         (pyslot-value *driver* "current_url"))
                              0))))

(defun ensure-page (page-key-or-name)
  (let ((page-name (etypecase page-key-or-name
                     (string page-key-or-name)
                     (symbol (get-val *page-table* page-key-or-name)))))
    (unless (pagep page-key-or-name)
      (get-page page-name))))

(defun all-building-elements ()
  (ensure-page :buildings)
  (pymethod *driver* "find_elements"
            "xpath" "//div[contains(@class, \"buildingSlot\")]"))

(defun find-building-element (name)
  (let ((buildings (all-building-elements)))
    (iter (for b in-sequence buildings)
      (for data-name = (pymethod b "get_attribute" "data-name"))
      (when (string= data-name name)
        (return b)))))

(defun click-upgrade-button ()
  (delay)
  (pymethod (find-unique-element
             "xpath"
             "//button[contains(@value, \"Upgrade to level\")]")
            "click"))

(defun remaining-build-duration ()
  (let ((build-duration-element (pymethod *driver*
                                          "find_elements"
                                          "xpath"
                                          "//div[@class=\"buildDuration\"]")))
    (cond ((< 0 (length build-duration-element))
           (pyslot-value (get-val (pymethod (get-val build-duration-element 0)
                                            "children")
                                  0)
                         "text"))
          ((or (pagep :resources) (pagep :buildings))
           nil)
          (t
           :unknown))))

(defun notify-when-build-complete (&optional name (poll-interval 1))
  (loop :while (remaining-build-duration)
        :do (sleep poll-interval)
        :finally (let ((message (format nil "Travian: ~A Building Complete!"
                                        (or name ""))))
                   (format t message)
                   (terpri)
                   (uiop:run-program (format nil "notify-send '~A'" message)))))

(defun notify-build-start (&optional name)
  (let ((message (format nil "Travian: ~A Building Started"
                         (or name ""))))
    (format t message)
    (terpri)
    (uiop:run-program (format nil "notify-send '~A'" message))))

(defun upgrade-building (name &optional wait)
  (declare (optimize debug))
  (let ((building (find-building-element name)))
    (unless building
      (error "No building with name ~A" name))
    (delay)
    (let* ((elements (pymethod building "children" :recursive t))
           (level-button
             (iter (for elt in-sequence elements)
               (when (string= "a" (pyslot-value elt "tag_name"))
                 (return elt)))))
      (pymethod level-button "click"))
    (click-upgrade-button)
    (notify-build-start name)
    (when wait
      (notify-when-build-complete name)))
  t)

(defun upgrade-buildings (&rest names)
  (loop :for name :in names
        :do (upgrade-building name t)))

(defun upgrade-resource-field (id &optional wait)
  (ensure-page :resources)
  (let* ((resource-field-elements
           (pymethod *driver* "find_elements" "xpath" "//a[contains(@class, \"level\")]"))
         (resource-field-element
           (get-val resource-field-elements (1- id))))
    (pymethod resource-field-element "click")
    (click-upgrade-button)
    (notify-build-start id)
    (when wait
      (notify-when-build-complete id))))

(defun upgrade-resource-fields (&rest ids)
  (loop :for id :in ids
        :do (upgrade-resource-field id t)))

(defun upgrade (name-or-id &optional wait)
  (etypecase name-or-id
    (string (upgrade-building name-or-id wait))
    ((integer 1 18) (upgrade-resource-field name-or-id wait))))

(defun adventure-send ()
  (ensure-page :adventures)
  (let ((explore-buttons
          (iter (for elt in-sequence
                     (pymethod *driver* "find_elements" "xpath" "//button"))
            (when (string= "Explore" (pyslot-value elt "text"))
              (collect elt)))))
    (delay)
    (pymethod (get-val explore-buttons 0) "click")
    (delay)
    (ensure-page :resources)))

;; (adventure-send)

(defun collect-rewards ()
  (ensure-page :tasks)
  (let ((collect-buttons
          (remove-if-not (lambda (elt)
                           (string= "Collect" (pyslot-value elt "text")))
                         (pymethod *driver* "find_elements" "xpath"
                                   "//div[@class=\"progress\"]//button"))))
    (iter (for button in-sequence collect-buttons)
      (delay)
      (pymethod button "click"))
    (delay)
    (ensure-page :resources))
  t)
