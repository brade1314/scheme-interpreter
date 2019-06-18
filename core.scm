

(load "types.scm")
(load "constants.scm")

;;;;; 确定一个表达式是否以某个标签开始
(define (tagged-list? exp tag)
  (eq? (type-tag exp) tag))

;;;;;; 封装语法类型
(define (wrap exp)
  (if (pair? exp)
      (attach-tag (car exp) exp)
      (attach-tag exp exp)))


;;;;; 自求值(字面量)只有数和字符串
(define (self-evaluation? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;;;;; 是否变量?
(define (variable? exp) (symbol? exp))

;;;;; 是否过程？
(define (application? exp) (pair? exp))

;;;;; 动态转发解释过程
(define (eval-rules tagged-exp env)
  (let ((type (type-tag tagged-exp)) ;; 获取符号
        (exp (content tagged-exp))) ;; 获取表达式
    (let ((proc (get eval-proc-key type))) ;;  查询变量表
      (if proc
          (proc exp env)
          (if (or (eq? type variable-keyword) ;; 变量
                  (eq? type application-keyword)) ;; 过程
              (error "Unknown expression type -- INTERP" exp)
              (cond ((variable? exp)
                     (eval-rules (attach-tag variable-keyword exp) env))
                    ((application? exp)
                     (eval-rules (attach-tag application-keyword exp) env))
                    (else (error "Unknown expression type -- INTERP" exp))))))))

;;;;; 解释器
;;;;; exp: 表达式
;;;;; env: 执行环境
(define (eval exp env)
  (if (self-evaluation? exp)
      exp
      (eval-rules (wrap exp) env)))

;;;;; 是否序列最后一个

(define (last-exp? seq) (null? (cdr seq)))

;;;;; 获取序列第一个
(define (first-exp seq) (car seq))

;;;;; 剩余的序列
(define (rest-exps seq) (cdr seq))


;;;;; 处理表达式序列
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))




