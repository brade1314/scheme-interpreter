


;;;;; begin 包装起一个表达式序列,在这里提供了对begin表达式的一组语法操作,
;;;;; 以便从begin表达式中实际表达式序列,还有选择函数返回序列中的第一个表达式
;;;;; 和其余表达式
(define (make-begin)
  
  ;构造begin
  ;seq是一个列表
  (define (begin-construct seq) (cons 'begin seq))
    
  
  ;操作序列
  (define (begin-actions exp) (cdr exp))
  
  (define (dispatch m)
    (cond ((eq? 'construct m) begin-construct)
          ((eq? 'actions m) begin-actions)
          (else (error "Unknown operator" m))))
  dispatch)

