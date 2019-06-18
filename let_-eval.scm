


(load "core.scm")
(load "syntax/let.scm")

;;;;; 对let*处理
(define (install-let*-eval)
  
  
  (define (let*-binds exp) (cadr exp))
  
  (define (let*-body exp) (cddr exp))
  
  (define (let*-new-binds k-v) (list k-v))
  
  (define (let*-new-body seq) (list seq))
  
  (define new-let ((make-let) 'construct))
  
  ;转成嵌套的let
  (define (let*->lets binds body)
    (define (iter binds)
      (if (null? binds)
          body
          (new-body (new-let (new-binds (car binds)) (iter (cdr binds))))))
    (car (iter binds)))
  
  (define (let*-eval exp env)
    (let ((new-exp (let*->lets
                    (let*-binds exp)
                    (let*-body exp))))
      (eval new-exp env)))
  
  (put eval-proc-key let*-eval 'let*)

  '(let* eval installed))
