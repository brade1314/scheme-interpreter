
;;;;; 创建一个初始表
;;;;; table-name:一个包含tableName的list
(define (make-table . table-name)
  (define tableName '*table*)
  (if (null? table-name) tableName (set! tableName (car table-name)))
  
  (let ((local-table (cons tableName '())))
    ;; 查找
    ;; 初始列表的是一个序对,作为table的入口
    (define (lookup table key . tag)
      (let ((record (assoc key (cdr table))))
        (if record
            (if (null? tag)
                (cdr record)
                (if (null? (cdr record))
                    #f
                    ;如果记录体中的car不是序对,表示record不是一个子表也不是一条记录,而是单纯的数据。
                    (if (pair? (cadr record))
                        (lookup record tag)
                        #f)))
            #f)))
    ;; 插入
    (define (insert table key value . tag)
      (let ((record (assoc key (cdr table))))
        ;; 判断是否已经存在key
        (cond (record
               (cond ((null? tag) (set-cdr! record value) table)
                     (else
                      ;;;如果有记录,且不是列表,表示值需要覆盖为列表
                      (if (not (list? record))
                          (set-cdr! record '())
                          #f)
                      (insert record (car tag) value)
                      table)))
              (else ;;不存在key
               (cond ((null? tag) (join table (cons key value)) table)
                     (else
                      (join table (insert (make-subtable key) (car tag) value))
                      table))))))

    ;; 查找
    (define (lookup-proc key . tag)
      (apply lookup local-table key tag))

    ;; 插入
    (define (insert-proc key value  . tag)
      (insert local-table key value tag))

    ;合并记录
    (define (join table record)
      (set-cdr! table (cons record (cdr table))))

    ;创建子表
    (define (make-subtable name)
      (list name))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup-proc)
            ((eq? m 'insert-proc) insert-proc)
            (else (error "Unknown opertion -- TABLE" m))))

    dispatch))
