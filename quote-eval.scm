



(load "core.scm")

;;;;; 对引号的处理
(define (install-quote-eval)
  ; 符号内容
  (define (text-of-quotation exp) (cadr exp))
  
  (define (quote-eval exp env) (text-of-quotation exp))
  
  (put eval-proc-key quote-eval 'quote)

  '(quote eval installed))
