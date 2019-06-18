
(load "constants.scm")

;;;;; 对环境的操作

;;;;; 空环境
(define the-empty-environment '())

;;;;; 将环境表示为一个框架的表,一个环境的外围环境就是这个表的cdr,空环境则空表示
(define (make-environment)
  
  ; 已装入的框架
  (define (enclosing-environment env) (cdr env))
  
  ; 第一个框架,表示当前环境
  (define (first-frame env) (car env))
  
  ; 构造一个框架
  ; 缺点:求值器为了找到一个给定变量的约束,可能需要搜索许多框架
  ; @@@可以优化,不把框架表示为表的序对,而是表示为约束的表,其中的每个约束是一个名字-值序对
  (define (make-frame variables values) (cons variables values))
  
  ; 在环境里的每个框架都是一对表形成的序对:
  ; 一个是这一框架中的所有变量的表,
  (define (frame-variables frame) (car frame))
  
  ; 另一个是框架中约束值的表
  (define (frame-values frame) (cdr frame))
  
  ; 添加一个键值对到框架中
  (define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))
  
  ;扩展环境,为了能够用一个新框架的环境,让框架由一个变量的表和一个值的表组成
  (define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "Too many arguments supplied"
                   vars vals)
            (error "Too few arguments suuuplied"
                   vars vals))))
  
  ;查找变量的值
  ;递归扫描第一个框架里的变量表,直到找到变量为止
  ; @@@可优化,以下3个方法抽象出env-loop
  (define (lookup-variable-value var env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars) 
               (env-loop (enclosing-environment env)))
              ((eq? var (car vars))
               (car vals))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))
  
  ;需要为某个变量在给定的环境里设置一个新值时,我们也要扫描这个变量,全环境搜索
  (define (set-variable-value! var val env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-environment env)))
              ((eq? var (car vars))
               (set-car! vals val))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable --SET" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))
  
  ; 定义一个变量,我们需要在第一个框架里查找该变量的约束,
  ; 如果找就修改其约束,为空就加入约束
  (define (define-variable! var val env)
    (let ((frame (first-frame env)))
      (define (scan vars vals)
        (cond ((null? vars)
               (add-binding-to-frame! var val frame))
              ((eq? var (car vars))
               (set-car! vals val))
              (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame))))
  
  (define (dispatch m)
    (cond ((eq? m 'define) define-variable!)
          ((eq? m 'set) set-variable-value!)
          ((eq? m 'lookup) lookup-variable-value)
          ((eq? m 'extend) extend-environment)
          (else (error "Unknown operator" m))))
  dispatch)