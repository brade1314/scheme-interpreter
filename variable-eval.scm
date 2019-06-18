

(load "environment.scm")

;;;;; 安装查找变量
(define (install-variable-eval)
  
  (define lookup-variable-value ((make-environment) 'lookup))
  
  (define (variable-eval exp env) (lookup-variable-value exp env))
  
  (put eval-proc-key variable-eval variable-keyword)

  '(variable eval installed))
