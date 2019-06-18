


;;;;; 需要注意,operator与operands需要用cons连接,而不是list
(define (make-application)
  
  ; 表达式运算符
  (define (application-operator exp) (car exp))
  
  ; 表达式运算对象
  (define (application-operands exp) (cdr exp))

  ; 构建函数调用
  ; params是一个列表
  (define (application-construct proc params) (cons proc params))
  
  (define (dispatch m)
    (cond ((eq? 'operator m) application-operator)
          ((eq? 'operands m) application-operands)
          ((eq? 'construct m) application-construct)
          (else (error "Unknown operator" m))))
  dispatch)

