(in-package :travian-bot)

(defvar *config-file*
  (pathname-utils:file-in *bot-home-dir* #P"config.json"))

(defvar *config* (json:read-json (read-file-into-string *config-file*)))
