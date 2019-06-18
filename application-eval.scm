


(load "core.scm")
(load "procedure.scm")
(load "environment.scm")
(load "syntax/application.scm")

;;;;;  组合式
;;;;;  过程应用,eval递归求值组合式的运算符部分和运算对象部分
;;;;;  然后将得到的过程和参数传给apply,由它去处理实际的过程应用
(define (install-application-eval)
  
  (let ((application-dispatch (make-application))
        (procedure-dispatch (make-procedure))
        (environment-dispatch (make-environment)))
    
    ; 表达式操作部分
    (define operator (application-dispatch 'operator))
    
    ; 操作数
    (define operands (application-dispatch 'operands))
    
    ; 没有操作数?
    (define (no-operands? ops) (null? ops))
    
    ; 第一个操作数
    (define (first-operand ops) (car ops))
    
    ; 剩余的操作数
    (define (rest-operands ops) (cdr ops))
    
    (define primitive-procedure? (procedure-dispatch 'primitive?))
    
    (define apply-primitive-procedure (procedure-dispatch 'apply-primitive))
    
    (define compound-procedure? (procedure-dispatch 'compound?))
    
    (define procedure-parameters (procedure-dispatch 'parameters))
    
    (define procedure-body (procedure-dispatch 'body))
    
    (define procedure-environment (procedure-dispatch 'environment))
    
    (define extend-environment (environment-dispatch 'extend))
    
    ; 求参数列表的值
    ; 若参数列表总某个参数的值为null,则解释器会判断参数列表结束,
    ; 因此，此处需要特殊处理，每个参数都用一个cons存放。
    (define (list-of-values exps env)
      (if (no-operands? exps)
          exps
          (cons (let ((value (eval (first-operand exps) env)))
                  value)
                (list-of-values (rest-operands exps) env))))
    
    
    (define (apply procedure arguments)
      (cond ((primitive-procedure? procedure)
              ; 基本过程?
             (apply-primitive-procedure procedure arguments))
            ; 复合过程,扩充基本环境,加入一个框架,将过程各个形参约束于过程调用的实际参数
            ((compound-procedure? procedure) 
             (eval-sequence (procedure-body procedure)
                            (extend-environment
                             (procedure-parameters procedure)
                             arguments
                             (procedure-environment procedure))))
            (else
             (error "Unknown procedure type -- APPLY" procedure arguments))))
    
    
    (define (application-eval exp env)
      (apply
       (eval (operator exp) env)
       (list-of-values (operands exp) env)))
    
    (define (execute procedure arguments)
      (cond ((primitive-procedure? procedure)
             (apply-primitive-procedure procedure arguments))
            ((compound-procedure? procedure)
             ((procedure-body procedure)
              (extend-environment (procedure-parameters procedure)
                                  arguments
                                  (procedure-environment procedure))))
            (else (error "Unknown procedure -- EXECUTE"
                         procedure
                         arguments))))
    
    
    (put eval-proc-key application-eval application-keyword)

    '(application eval installed)))
