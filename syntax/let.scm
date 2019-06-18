


(load "syntax/definition.scm")
(load "syntax/application.scm")
(load "syntax/lambda.scm")

(define (make-let)
  
  (define new-define ((make-define) 'construct))
  
  (define new-application ((make-application) 'construct))
  
  (define new-lambda ((make-lambda) 'construct))
  
  ; let构造器
  ; body是一个列表
  ; binds是一个列表
  (define (let-construct binds body)
    (cons 'let (cons binds body)))
  
  (define (let-binds exp)
    (if (named-let? exp)
        (caddr exp)
        (cadr exp)))
  
  (define (let-bind var value) (list var value))
  
  (define (let-body exp)
    (if (named-let? exp)
        (new-body exp)
        (cddr exp)))
  
  ;命名let
  (define (named-let? exp)
    (not (pair? (cadr exp))))
  
  (define (let-parameters binds)
    (if (null? binds)
        binds
        (cons (caar binds) (let-parameters (cdr binds)))))
  
  (define (let-values! binds)
    (if (null? binds)
        binds
        (cons (cadar binds) (let-values! (cdr binds)))))
  
  ;对body进行改变,转成内部定义,以及一次初始化调用
  (define (new-body exp)
    (let ((name (cadr exp))
          (params (let-parameters (binds exp))))
      (list (new-define name (new-lambda params (cdddr exp)))
            (new-application name params))))
  
  
  (define (dispatch m)
    (cond ((eq? m 'construct) let-construct)
          ((eq? m 'binds) let-binds)
          ((eq? m 'body) let-body)
          ((eq? m 'parameters) let-parameters)
          ((eq? m 'values) let-values!)
          ((eq? m 'bind) let-bind)
          (else (error "Unknown operator" m))))
  
  dispatch)

