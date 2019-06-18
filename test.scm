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
(load "let_-eval.scm")
(load "letrec-eval.scm")
(load "and-eval.scm")
(load "or-eval.scm")
(load "application-eval.scm")
;(load "procedure.scm")



;;测试 type-tag
(define exp '(define (add x y) (+ x y)))
(define exp-tag (type-tag exp))
(displayln exp-tag)

;;测试 content
(define exp-content (content exp))
(displayln exp-content)

;;测试 tagged-list?
(displayln (tagged-list? exp 'define ))

;;测试 attach-tag
(displayln (attach-tag exp-tag exp-content))

;;测试 self-evaluation?
(displayln (self-evaluation? 'test))

;;测试 wrap
(displayln (wrap exp))

;;测试 variable?
(displayln (variable? 'test))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(displayln "============ environment ==============")
;; 测试创建环境
(define env-dispatch (make-environment))
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

(define the-global-environment (setup-environment))
(displayln the-global-environment)

;;;;;;;;;;;;;; table ;;;;;;;;;;;;;;;;;
(displayln "============ table ==============")
(define table (make-table 'test))
;(define table (make-table))

(define get! (get-operation table))
(define put! (put-operation table))

(displayln (put! 'key 'data 'tag))
(displayln (get! 'key 'tag))
(displayln (put! 'key 'data1 'tag))
(displayln (get! 'key 'tag))
(displayln (put! 'key 'data 'tag1))
(displayln (get! 'key 'tag1))

;;;;;;;;;;;;;; variable ;;;;;;;;;;;;;;
(displayln "============ eval ==============")

(displayln (install-quote-eval))
(define quote-eval (get eval-proc-key 'quote))
(displayln quote-eval)

(displayln (install-variable-eval))
(define lookup-env (get eval-proc-key variable-keyword))
(displayln lookup-env)

;;;;;;;;;;;;;; define ;;;;;;;;;;;;;;;;;;;;
(displayln (install-definition-eval))
(define def-eval (get eval-proc-key 'define))
(displayln (def-eval '(define num 3)  the-global-environment))
(displayln (lookup-env 'num the-global-environment))

;;;;;;;;;;;;;; set! ;;;;;;;;;;;;;;;;;;;;
(displayln (install-assignment-eval))
(define set-eval (get eval-proc-key 'set!))
(displayln (set-eval '(set! num 5) the-global-environment))
(displayln (lookup-env 'num the-global-environment))


;;;;;;;;;;;;;; if ;;;;;;;;;;;;;;;;;;;;
(displayln (install-if-eval))
(define if-eval (get eval-proc-key 'if))
(displayln  if-eval)

;;;;;;;;;;;;;; lanbda ;;;;;;;;;;;;;;;;;;;;
(displayln (install-lambda-eval))
(define lambda-eval (get eval-proc-key 'lambda))
(displayln lambda-eval)

;;;;;;;;;;;;;; begin ;;;;;;;;;;;;;;;;;;;;
(displayln (install-begin-eval))
(define begin-eval (get eval-proc-key 'begin))
(displayln begin-eval)

;;;;;;;;;;;;;; let ;;;;;;;;;;;;;;;;;;;;
(displayln (install-let-eval))
(define let-eval (get eval-proc-key 'let))
(displayln let-eval)

;;;;;;;;;;;;;; let* ;;;;;;;;;;;;;;;;;;;;
(displayln (install-let*-eval))
(define let*-eval (get eval-proc-key 'let*))
(displayln let*-eval)

;;;;;;;;;;;;;; letrec ;;;;;;;;;;;;;;;;;;;;
(displayln (install-letrec-eval))
(define letrec-eval (get eval-proc-key 'letrec))
(displayln letrec-eval)

;;;;;;;;;;;;;; and ;;;;;;;;;;;;;;;;;;;;
(displayln (install-and-eval))
(define and-eval (get eval-proc-key 'and))
(displayln (and-eval '(and 1 2) the-global-environment))

;;;;;;;;;;;;;; or ;;;;;;;;;;;;;;;;;;;;
(displayln (install-or-eval))
(define or-eval (get eval-proc-key 'or))
(displayln (or-eval '(or 1 2) the-global-environment))

;;;;;;;;;;;;;; application ;;;;;;;;;;;;;;;;;;;;
(displayln (install-application-eval))
(define application-eval (get eval-proc-key '**application**))
(displayln application-eval)

















