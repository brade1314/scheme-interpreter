

;;;;; lambda 表达式处理方法：lambda表达式是由符号lambda开始的表
(define (make-lambda)

  ; lambda 参数列表
  (define (lambda-parameters exp) (cadr exp))
  
  ; lambda过程体
  (define (lambda-body exp) (cddr exp))
  
  ; 构造lambda
  ; parameters是一个列表
  ; body是一个列表
  (define (lambda-construct parameters body) (cons 'lambda (cons parameters body)))

  ;是否为lambda语句
  (define (lambda? exp) (tagged-list? exp 'lambda))
  
  
  (define (dispatch m)
    (cond ((eq? m 'parameters) lambda-parameters)
          ((eq? m 'body) lambda-body)
          ((eq? m 'construct) lambda-construct)
          ((eq? m 'lambda?) lambda?)
          (else (error "Unknown operator" m))))
  
  dispatch)
