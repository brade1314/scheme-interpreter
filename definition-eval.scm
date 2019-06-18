


(load "core.scm")
(load "environment.scm")
(load "syntax/definition.scm")


;对define语句的处理
(define (install-definition-eval)

  (let ((define-dispatch (make-define))
        (env-dispatch (make-environment)))

    (define definition-variable (define-dispatch 'variable))

    (define definition-value (define-dispatch 'value))

    ; 调用environment.scm -> define-variable!
    (define define-variable! (env-dispatch 'define))

    (define (definition-eval exp env)
      (define-variable! (definition-variable exp)
                        (eval (definition-value exp) env)
                        env)
      'ok)

    (put eval-proc-key definition-eval 'define)

    '(define eval installed)))


