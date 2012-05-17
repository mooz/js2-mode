(eval-when-compile
  (require 'cl))

(defconst js2-imenu-extension-styles
  `((:framework jquery
     :call-re   "\\_<\\(?:jQuery\\|\\$\\|_\\)\\.extend\\s-*("
     :recorder  js2-imenu-record-jquery-extend)

    (:framework jquery-ui
     :call-re   "^\\s-*\\(?:jQuery\\|\\$\\)\\.widget\\s-*("
     :recorder  js2-imenu-record-string-declare)

    (:framework dojo
     :call-re   "^\\s-*dojo.declare\\s-*("
     :recorder  js2-imenu-record-string-declare)

    (:framework backbone
     :call-re   ,(concat "\\_<" js2-mode-identifier-re "\\.extend\\s-*(")
     :recorder  js2-imenu-record-backbone-extend)))

(defconst js2-imenu-available-frameworks
  (mapcar (lambda (style) (plist-get style :framework)) js2-imenu-extension-styles)
  "List of available JavaScript framework symbols.")

(defcustom js2-imenu-enabled-frameworks js2-imenu-available-frameworks
  "Frameworks to be recognized by `js2-mode'."
  :type (cons 'set (mapcar (lambda (x) (list 'const x))
                           js2-imenu-available-frameworks))
  :group 'js2-mode)

(defcustom js2-imenu-show-other-functions t
  "Non-nil to show functions not recognized by other mechanisms,
in a shared namespace."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-imenu-other-functions-ns "?"
  "Namespace name to use for other functions."
  :type 'string
  :group 'js2-mode)

(defun js2-imenu-extras-setup ()
  (when js2-imenu-enabled-frameworks
    (add-to-list 'js2-post-parse-callbacks 'js2-imenu-record-declarations t))
  (when js2-imenu-show-other-functions
    (add-to-list 'js2-post-parse-callbacks 'js2-imenu-record-hashes t)))

(declare (special root))

(defun js2-imenu-record-declarations ()
  (let* ((styles (loop for style in js2-imenu-extension-styles
                       when (memq (plist-get style :framework)
                                  js2-imenu-enabled-frameworks)
                       collect style))
         (re (mapconcat (lambda (style)
                          (concat "\\(" (plist-get style :call-re) "\\)"))
                        styles "\\|"))
         ;; Dynamic scoping. Ew.
         (js2-mode-ast root)
         chains)
    (goto-char (point-min))
    (while (js-re-search-forward re nil t)
      (push (loop for i from 0 to (1- (length styles))
                  when (match-beginning (1+ i))
                  return (funcall (plist-get (nth i styles) :recorder)))
            chains))
    chains))

(defun js2-imenu-record-jquery-extend ()
  (let ((pred (lambda (subject)
                (and
                 (js2-prop-get-node-p subject)
                 (string= (js2-name-node-name (js2-prop-get-node-right subject))
                          "prototype")))))
    (js2-imenu-record-extend-first-arg (1- (point)) pred
                                       'js2-compute-nested-prop-get)))

(defun js2-imenu-record-string-declare ()
  (js2-imenu-record-extend-first-arg
   (1- (point)) 'js2-string-node-p
   (lambda (node) (split-string (js2-string-node-value node) "\\." t))))

(defun js2-imenu-record-extend-first-arg (point pred qname-fn)
  (let* ((node (js2-node-at-point point))
         (args (js2-call-node-args node))
         (subject (first args)))
    (when (funcall pred subject)
      (loop for arg in (cdr args)
            when (js2-object-node-p arg)
            do (js2-record-object-literal
                arg (funcall qname-fn subject) (js2-node-abs-pos arg))))))

(defun js2-imenu-record-backbone-extend ()
  (let* ((node (js2-node-at-point (1- (point))))
         (args (js2-call-node-args node))
         (methods (first args))
         (parent (js2-node-parent node)))
    (when (js2-object-node-p methods)
      (let ((subject (cond ((js2-var-init-node-p parent)
                            (js2-var-init-node-target parent))
                           ((js2-assign-node-p parent)
                            (js2-assign-node-left parent)))))
        (when subject
          (js2-record-object-literal methods
                                     (js2-compute-nested-prop-get subject)
                                     (js2-node-abs-pos methods)))))))

(defun js2-imenu-record-hashes ()
  (js2-visit-ast
   root
   (lambda (node end-p)
     (unless end-p
       (if (and (js2-object-prop-node-p node)
                (js2-function-node-p (js2-object-prop-node-right node)))
           (let ((fn-node (js2-object-prop-node-right node)))
             (unless (and js2-imenu-function-map
                          (gethash fn-node js2-imenu-function-map))
               (let ((key-node (js2-object-prop-node-left node)))
                 (js2-record-imenu-entry fn-node
                                         (list js2-imenu-other-functions-ns
                                               (js2-prop-node-name key-node))
                                         (js2-node-abs-pos key-node))))
             nil)
         t)))))

(provide 'js2-imenu-extras)
