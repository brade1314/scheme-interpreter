


(load "core.scm")
(load "procedure.scm")
(load "proc-transform.scm")
(load "syntax/lambda.scm")

(define (install-lambda-eval)
  
  (let ((lambda-dispatch (make-lambda))
        (procedure-dispatch (make-procedure))
        (trans-dispatch (make-proc-transform)))
    
    
    (define lambda-parameters (lambda-dispatch 'parameters))
    
    (define (lambda-body exp) ((trans-dispatch 'trans-body) ((lambda-dispatch 'body) exp)))
    
    (define new-procedure (procedure-dispatch 'construct))
    ; 构造新的过程
    (define (lambda-eval exp env) (new-procedure (lambda-parameters exp) (lambda-body exp) env))
    
    (put eval-proc-key lambda-eval 'lambda)

    '(lambda eval installed)))