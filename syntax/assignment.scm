

;;;;; 赋值
; 赋值的形式是(set! <var> <value>)
(define (make-assignment)
  
  ; 取出变量名
  (define (assignment-variable exp) (cadr exp))
  ; 取出值的表达式
  (define (assignment-value exp) (caddr exp))
  ; 构造新的赋值语句
  (define (assignment-construct var value) (list 'set! var value))
  ; 是否set!语句
  (define (assignment? exp) (tagged-list? exp 'set!))
  
  (define (dispatch m)
    (cond ((eq? m 'variable) assignment-variable)
          ((eq? m 'value) assignment-value)
          ((eq? m 'construct) assignment-construct)
          ((eq? m 'set!?) assignment?)
          (else (error "Unknown operator" m))))
  
  dispatch)