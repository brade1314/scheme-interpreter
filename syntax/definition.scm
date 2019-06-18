(load "syntax/lambda.scm")


;;;;; 定义的形式是 (define <var> <value>)
;;;;; 或者 (define (<var> <parameter_1> ... <parameter_n>) <body>)
;;;;; 后一形式只是下面形式的一种语法包装
;;;;; (define <var> (lambda (<parameter_1> ... <parameter_n>) <body>))
(define (make-define)
  
  (define new-lambda ((make-lambda) 'construct))
  
  ; 变量名
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  ; 值
  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (new-lambda (cdadr exp)
                    (cddr exp))))
  ; 构造器
  (define (definition-construct var param) (list 'define var param))

  ; 是否为定义语句
  (define (definition? exp) (tagged-list? exp 'define))
  
  (define (dispatch m)
    (cond ((eq? m 'variable) definition-variable)
          ((eq? m 'value) definition-value)
          ((eq? m 'construct) definition-construct)
          ((eq? m 'define?) definition?)
          (else (error "Unknown operator" m))))
  
  dispatch)



