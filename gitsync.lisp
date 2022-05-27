#!/usr/bin/env -S sbcl --core ${HOME}/lib/sbcl-cores/scripting.core --script
(unless (member :script *features*)
  (ql:quickload (list :cl-getopt
                      :sbcl-script
                      :split-sequence
                      :usocket
                      :uiop
                      :cl-fad
                      :cl-ppcre
                      :trivial-clipboard)))
(in-package :sbcl-script)

(defparameter *name*
  "gitsync.lisp")
(defparameter *options*
  (list
   (list :long "help"
         :short "h"
         :argspec :none
         :description "show this help message")
   (list :long "verbose"
         :short "v"
         :argspec :none
         :description "increase verbosity")
   (list :long "quiet"
         :short "q"
         :argspec :none
         :description "suppress output")))

(defparameter *quiet-p* nil)
(defparameter *verbose* 0)
(defparameter *width* 72)

(defun help ()
  (format *error-output* "Usage: ~a <path1> [path2]...~%~%"

          *name*)
  (format *error-output*
          "~a~%~%"
          (wrap-string "gitsync synchronizes multiple sets of git repositories which should track the same source code.  An example use would be keeping multiple projects on a thumb drive or on multiple computers.  To use gisync, place a file .gitsync just inside each supplied path which contains lines of the form" *width*))
  (format *error-output*
          "\"/path/to/repo/1\" \"/path/to/repo/2\" [branch-list]~%~%")
  (format *error-output*
          "~a~%~%"
          (wrap-string "where branch-list is an optional Lisp list of branch strings, e.g." *width*))
  (format *error-output*
          "(\"master\" \"future\" \"testing\")~%~%")
  (format *error-output* "Options:~%~a~%"
          (option-descriptions *options*)))

(defun git (&rest args)
  "Runs git with convenience"
  (run "git" args
       :show-output (and (not *quiet-p*)
                         (not (zerop *verbose*)))))

(defun get-git-branch ()
  (let* ((scans
           (second
            (multiple-value-list
             (cl-ppcre:scan-to-strings
              "\\* *(.*)"
              (run "git" (list "branch"))))))
         (len (length scans)))
    (when (plusp len)
      (aref scans 0))))

(define-condition git-no-branches (error)
  ((path :initarg :path :initform "")))

(defmethod print-object ((x git-no-branches) stream)
  (format stream "No git branches for path ~s"
          (slot-value x 'path)))

(define-condition git-no-repo (error)
  ((paths :initarg :path :initform "")))

(defmethod print-object ((x git-no-repo) stream)
  (format stream "No git repos at any paths ~s"
          (slot-value x 'paths)))

(defun gitclone (path1 path2 &optional (branches (list "master")))
  "Ensure that Git repos exist at both path1 and path2 via cloning if
needed.  Does not check that they are actually clones of each other."
  (let* ((olddir (getcwd))
         (d1 (namestring (make-directory-pathname path1)))
         (d2 (namestring (make-directory-pathname path2))))
    ;; clone if necessary
    (unless (and (probe-file (merge-pathnames ".git" d1))
                 (probe-file (merge-pathnames ".git" d2)))
      (let* ((p1 nil) ; source
             (p2 nil)) ; dest
        (when (probe-file (merge-pathnames ".git" d1))
          (setf p1 d1)
          (setf p2 d2))
        (when (probe-file (merge-pathnames ".git" d2))
          (setf p1 d2)
          (setf p2 d1))
        (when (not (or p1 p1))
          (error 'git-no-repo :paths (list d1 d2)))
        ;; cd into parentdir
        (restart-case
            (let* ((firstbranch NIL)
                   (parentdir
                     (make-pathname :directory
                                    (butlast (pathname-directory p2)))))
              (ensure-directories-exist parentdir)
              (chdir parentdir)
              (git "clone" p1)
              (chdir p2)
              (setf firstbranch (get-git-branch))
              (setf branches (remove firstbranch branches :test #'equal))
              (dolist (b branches)
                (let* ((branch (get-git-branch)))
                  (unless (equal b branch)
                    (git "checkout"
                         (concatenate 'string
                                      "remotes/origin/"
                                      b)
                         "-b"
                         b))))
              (git "checkout" firstbranch)
              (chdir olddir))
          (abort ()
            (chdir olddir)
            (return-from gitclone)))))))

(defun gitpull (path1 path2 &optional (branches (list "master")))
  (let* ((olddir (getcwd))
         (d1 (namestring (make-directory-pathname path1)))
         (d2 (namestring (make-directory-pathname path2)))
         ;; original branch
         (oldbranch nil))
    ;; cd into path2
    (restart-case
        (chdir d2)
      (abort () (return-from gitpull))
      (use-value (value) (setf d2 value)))
    (setf oldbranch (get-git-branch))
    (unless oldbranch
      (chdir olddir)
      (error 'git-no-branches :path d2))
    (unless *quiet-p*
      (format t "Pulling ~a into ~a~%" d1 d2))
    ;; loop over branches
    (dolist (b branches)
      (unless *quiet-p*
        (format t "Branch: ~a~%" b))
      (restart-case
          (progn
            (git "checkout" b)
            (git "pull" d1 b))
        (skip () :report "Skip this branch")
        (abort () :report "Abort gitpull for all branches"
          (git "checkout" oldbranch)
          (chdir olddir)
          (return-from gitpull nil))))
    (chdir olddir)
    t))

(defun gitsync (path)
  "Parses gitsync spec lines in file located at path and runs gitpull.
  Each spec line is of the form

\"/path/to/repo/1\" \"/path/to/repo/2\" [branches]

where [branches] is an optional Lisp list of branch name strings.  By
default it's assumed to be (\"master\").  NOTE: I have switched from
the Bash-friendly path syntax to a Lisp-friendly one where paths must
be treated as Lisp strings."
  (with-open-file (f path
                     :direction :input)
    (loop
      for line = (read-line f nil nil)
      while line
      do (with-input-from-string (s (safe-string line))
           (let* ((repo1 (read s nil nil))
                  (repo2 (read s nil nil))
                  (branches (read s nil nil)))
             (cond
               ((or (null repo1)
                    (null repo2))
                (unless *quiet-p*
                  (format *error-output*
                          "Warning: Skipping malformed line:~%~a~%"
                          line)))
               (t
                (unless branches (setf branches (list "master")))
                (gitclone repo1 repo2 branches)
                (gitpull repo1 repo2 branches)
                (gitpull repo2 repo1 branches)
                (unless *quiet-p*
                  (terpri)))))))))

(defun main ()
  (let* ((args sb-ext:*posix-argv*))
    (multiple-value-bind (options remaining)
        (getopt args *options*)
      (cond
        ((or (null remaining)
             (gethash "h" options))
         (help)
         (return-from main))
        ((gethash "v" options)
         (loop for i in (gethash "v" options)
               do (incf *verbose*)))
        ((gethash "q" options)
         (setf *quiet-p* t)))
      (dolist (path remaining)
        (let* ((p (namestring
                   (merge-pathnames ".gitsync"
                                    (make-directory-pathname path)))))
          (unless *quiet-p*
            (format t "gitsync: ~a~%" p))
          (gitsync p))))))

(when (member :script *features*)
  (flet ((custom-abort (&optional condition)
           (format *error-output* "ABORTING: ~a~%" condition)
           (abort condition)))
    (handler-bind ((git-no-branches #'custom-abort)
                   (run-error #'custom-abort))
      (main)
      (sb-ext:quit :unix-status 0))))
