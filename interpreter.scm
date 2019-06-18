#lang scheme/load
(require (planet neil/sicp))


(load "core.scm")
(load "environment.scm")
(load "quote-eval.scm")
(load "variable-eval.scm")
(load "definition-eval.scm")
(load "assignment-eval.scm")
(load "if-eval.scm")
(load "lambda-eval.scm")
(load "begin-eval.scm")
(load "let-eval.scm")
(load "letrec-eval.scm")
(load "let_-eval.scm")
(load "and-eval.scm")
(load "or-eval.scm")
(load "application-eval.scm")



(install-quote-eval)
(install-variable-eval)
(install-definition-eval)
(install-assignment-eval)
(install-if-eval)
(install-letrec-eval)
(install-let*-eval)
(install-let-eval)
(install-lambda-eval)
(install-and-eval)
(install-or-eval)
(install-application-eval)



;;;;; 环境调度
(define env-dispatch (make-environment))

;;;;; 过程调度
(define proc-dispatch (make-procedure))

;; 创建初始环境,主要创建基本过程和基本值.
(define (setup-environment)
  (define def-var (env-dispatch 'define))
  ;;环境扩展
  (define extend-environment (env-dispatch 'extend))
  ;;基本过程变量表
  (define primitive-procedure-names (proc-dispatch 'primitive-names))
  ;;基本过程值(对象)表
  (define primitive-procedure-objects (proc-dispatch 'primitive-objects))
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)));;环境
    ;; 为一些基本值进行定义 
    (def-var 'true true initial-env)
    (def-var 'false false initial-env)
    initial-env))

;;;;; 全局环境
(define the-global-environment (setup-environment))

;;;;;; 基本过程read将一直等待用户的输入,并返回键入的下一个完整表达式
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (prompt-for-input input)
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define input-prompt ";;;M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))


;;;;; 使用一个特殊的打印过程user-print,以避免打印出复合过程的环境部分
;;;;; 因为可能是一个非常长的表(而且还可能包含循环)
(define (user-print object)
  (if ((proc-dispatch 'compound?) object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)))
      (display object)))

