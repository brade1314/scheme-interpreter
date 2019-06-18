


(load "core.scm")
(load "environment.scm")
(load "syntax/assignment.scm")

;;;;; 安装set语句的处理方法
(define (install-assignment-eval)
  
  (let ((assign-dispatch (make-assignment)))

    ; 调用赋值方法:environment.scm -> set-variable-value!
    (define set-variable-value! ((make-environment) 'set))
    ; 调用获取变量名方法
    (define assign-variable (assign-dispatch 'variable))
    ; 调用获取变量值方法
    (define assign-value (assign-dispatch 'value))
    
    (define (assign-eval exp env)
      (set-variable-value! (assign-variable exp)
                           (eval (assign-value exp) env)
                           env)
      'ok)
   
    
    (put eval-proc-key assign-eval 'set!)
    
    '(assignment eval installed)))
