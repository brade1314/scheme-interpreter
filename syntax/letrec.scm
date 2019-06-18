

;;;;;  letrec处理: 将内部定义的过程或变量间进行相互引用
(define (make-letrec)
  
  ; let构造器
  ; body是一个列表
  ; binds是一个列表
  (define (letrec-construct binds body) (cons 'letrec (cons binds body)))
  
  ; letrec 绑定表
  (define (letrec-binds exp) (cadr exp))
  
  ; letrec 体
  (define (letrec-body exp) (cddr exp))
  
  ; 创建一条绑定
  (define (letrec-bind key value) (list key value))
  
  ; 绑定的key
  (define (letrec-variable kv) (car kv))
  
  ; 绑定的value
  (define (letrec-value kv) (cadr kv))
  
  (define (letrec-parameters binds)
    (if (null? binds)
        binds
        (cons (caar binds) (letrec-parameters (cdr binds)))))
  
  (define (letrec-values! binds)
    (if (null? binds)
        binds
        (cons (cadar binds) (letrec-values! (cdr binds)))))
  
  (define (dispatch m)
    (cond ((eq? m 'construct) letrec-construct)
          ((eq? m 'binds) letrec-binds)
          ((eq? m 'body) letrec-body)
          ((eq? m 'parameters) letrec-parameters)
          ((eq? m 'values) letrec-values!)
          ((eq? m 'bind) letrec-bind)
          ((eq? m 'variable) letrec-variable)
          ((eq? m 'value) letrec-value)
          (else (error "Unknown operator" m))))
  
  dispatch)
