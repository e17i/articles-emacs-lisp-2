(provide 'macro-namespace)

(defmacro with-macro-namespace (syms body)
  (declare (indent defun))
  (let ((pquote (gensym)))
    (let ((qbody
           (macroexpand-all `(cl-macrolet ((quote (arg)
                                                  (cond
                                                   ((atom arg) (if (member arg (quote ,syms))
                                                                   arg
                                                                 `(,',pquote ,arg)))
                                                   ((eq (car arg) 'outer) `(,',pquote ,(cadr arg)))
                                                   (`(list ,@(mapcar (lambda (elem) `(quote ,elem))
                                                                     arg))))))
                               ,body))))
      `(let ,(mapcar (lambda (sym)
                       `(,sym (gensym ,(symbol-name sym))))
                     syms)
         (cl-macrolet ((,pquote (arg) `(quote ,arg)))
           ,qbody)))))

(defmacro with-macro-namespace-defines (syms body)
  (declare (indent defun))
  (let ((symnames (mapcar (lambda (sym)
                            (pcase sym
                              (`(,var . ,_) var)
                              (var var)))
                          syms))
        (pquote (gensym)))
    (let ((qbody
           (macroexpand-all `(cl-macrolet ((quote (arg)
                                                  (cond
                                                   ((atom arg) (if (member arg (quote ,symnames))
                                                                   arg
                                                                 `(,',pquote ,arg)))
                                                   ((eq (car arg) 'outer) `(,',pquote ,(cadr arg)))
                                                   (`(list ,@(mapcar (lambda (elem) `(quote ,elem))
                                                                     arg))))))
                               (list 'let
                                     (list
                                      ,@(mapcar (lambda (symdef)
                                                  (cond
                                                   ((atom symdef) symdef)
                                                   (`(list ,@symdef))))
                                                syms))
                                     ,body)))))
      `(let ,(mapcar (lambda (sym)
                       `(,sym (gensym ,(symbol-name sym))))
                     symnames)
         (cl-macrolet ((,pquote (arg) `(quote ,arg)))
           ,qbody)))))
