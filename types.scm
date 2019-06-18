

(load "lib/table.scm")

;;;;; 表操作

;;;;; 查找表的值
(define (get-operation table) (table 'lookup-proc))

;;;;; 将值保存到表
(define (put-operation table) (table 'insert-proc))

;;;;; 创建默认表名的表
(define operation-talbe (make-table))


(define get (get-operation operation-talbe))

(define put (put-operation operation-talbe))


;;;;; 获取表达式类型符号
(define (type-tag exp)
  (if (pair? exp)
      (car exp)
      (error 'error "Bad Type Tag [ ~a ]" exp)))


;;;;; 获取表达式内容
(define (content exp)
  (if (pair? exp)
      (cdr exp)
      (error 'error "Bad Type Tag [ ~a ]" exp)))


;;;;; 为数据实体增加类型符号
;;;;; type-tag 类型符号
;;;;; contents 表达式内容
(define (attach-tag type-tag contents) (cons type-tag contents))