

(load "core.scm")
(load "environment.scm")
(load "syntax/if.scm")

;;;;; 安装if语句的处理方法
(define (install-if-eval)

  (let ((if-dispatch (make-if)))
    
    (define true? (if-dispatch 'true?))
    
    (define if-predicate (if-dispatch 'predicate))
    
    (define if-consequent (if-dispatch 'consequent))
    
    (define if-alternative (if-dispatch 'alternative))
    
    (define (if-eval exp env)
      (if (true? (eval (if-predicate exp) env))
          (eval (if-consequent exp) env)
          (eval (if-alternative exp) env)))

    (put eval-proc-key if-eval 'if)
    
    '(if eval installed)))