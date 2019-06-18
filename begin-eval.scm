


(load "core.scm")
(load "syntax/begin.scm")

;;;;; 对begin表达式的处理
(define (install-begin-eval)
  
  (let ((begin-dispatch (make-begin)))
    
    (define begin-actions (begin-dispatch 'actions))
    
    (define (begin-eval exp env) (eval-sequence (begin-actions exp) env))
    
    (put eval-proc-key begin-eval 'begin)

    '(begin eval installed)))


