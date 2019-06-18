


(load "core.scm")
(load "syntax/if.scm")


;;;;; 对or表达式的处理
(define (install-or-eval)
  
  (define new-if ((make-if) 'construct))
  
  ; or语句序列
  (define (or-clauses exp)  (cdr exp))
  
  ; or转成if
  (define (or->if exp) (expand-clauses (or-clauses exp)))
  
  ; 展开or语句序列
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest  (cdr clauses)))
          (new-if first 'true (expand-clauses rest)))))
  
  (define (or-eval exp env) (eval (or->if exp) env))
  
  (put eval-proc-key or-eval 'or)
  (put eval-proc-key or-eval '||)

  '(or eval installed))
