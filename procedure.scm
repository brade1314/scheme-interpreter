

;;;;; 过程的表示
(define (make-procedure)
  
  ; 确定一个表的开始是不是一个操作符(tag)
  (define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

  
  ; 是否为复合过程
  (define (compound-procedure? p) (tagged-list? p 'procedure))
  ; ;过程参数列表
  (define (compound-procedure-parameters p) (cadr p))
  ; 过程体
  (define (compound-procedure-body p) (caddr p))
  ; 过程环境
  (define (compound-procedure-environment p) (cadddr p))
  ; 构建过程
  ; parameters:参数表
  ; body: 过程体列表
  ; env: 环境
  (define (construct-compound-procedure parameters body env) (list 'procedure parameters body env))

  ; 是否为基本过程
  (define (primitive-procedure? proc) (tagged-list? proc 'primitive))
  ; 基本过程的实现
  (define (primitive-implementation proc) (cadr proc))
  ; 基本过程
  (define primitive-procedures
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'list list)
          (list 'eq? eq?)
          (list 'cons cons)
          (list 'null? null?)
          (list '+ +)
          (list '- -)
          (list '* *)
          (list '/ /)
          (list '< <)
          (list '> >)
          (list '= =)
          (list 'not not)
          (list 'abs abs)
          (list 'cadr cadr)
          (list 'caddr caddr)
          (list 'display display)
          (list 'newline newline)
          (list 'map map)))
  ; 基本过程名列表
  (define (primitive-procedure-names) (map car primitive-procedures))
  ; 基本过程值(对象)列表
  (define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

  ; 采用不同的而名字同为apply的东西,将会在元循环求值器的运行中产生一个技术问题
  ; 因为元循环求值器的apply定义会掩盖相应基本过程的定义,所以重命名
  (define apply-in-underlying-scheme apply)
  (define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme (primitive-implementation proc) args))
  
  (define (dispatch m)
    (cond ((eq? m 'construct) construct-compound-procedure)
          ((eq? m 'compound?) compound-procedure? )
          ((eq? m 'parameters) compound-procedure-parameters)
          ((eq? m 'body) compound-procedure-body)
          ((eq? m 'environment) compound-procedure-environment)
          ((eq? m 'primitive?) primitive-procedure?)
          ((eq? m 'apply-primitive) apply-primitive-procedure)
          ((eq? m 'primitive-names) primitive-procedure-names)
          ((eq? m 'primitive-objects) primitive-procedure-objects)
          (else (error "Unknown operator" m))))
  dispatch)

