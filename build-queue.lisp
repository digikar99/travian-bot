(in-package :travian-bot)

(defvar *build-queue-lock* (bt:make-lock "travian-build-queue-lock"))

(defvar *build-queue* ())

;; FIXME: Ideally, needs to sync with the actual status on the servers.
;; That will also later require keeping a track of the building levels.

;; TODO: Add option to cancel.
;; TODO: Add a proper specification.

(defun push-to-build-queue (name-or-id)
  (bt:with-lock-held (*build-queue-lock*)
    (nconcf *build-queue* (list name-or-id))))

(defun pop-from-build-queue ()
  (bt:with-lock-held (*build-queue-lock*)
    (let ((name-or-id (first *build-queue*)))
      (setq *build-queue* (rest *build-queue*))
      name-or-id)))

(defun enqueue-upgrades (&rest names-or-ids)
  (loop :for name-or-id :in names-or-ids
        :do (push-to-build-queue name-or-id)))

(defvar *build-thread*)

(defun start-build-thread ()
  (when (and (boundp '*build-thread*)
             (bt:thread-alive-p *build-thread*))
    (return-from start-build-thread nil))
  (setq *build-thread*
        (bt:make-thread
         (lambda ()
           (notify-when-build-complete "Previous")
           (loop :for name-or-id := (pop-from-build-queue)
                 :while name-or-id
                 :do (upgrade name-or-id t)))
         :name "travian-build-thread")))
