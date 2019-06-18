

;;;;; if 表达式
(define (make-if)
  
  ;if 谓词
  (define (if-predicate exp) (cadr exp))
  ;if 推论
  (define (if-consequent exp) (caddr exp))
  ;一个(可缺)替代部分
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))
  
  ;  predicate:谓词
  ;  consequent:推论
  ;  alternative:一个(可缺)替代部分
  (define (if-construct predicate consequent alternative)
    (list 'if predicate consequent alternative))

  (define (true? x) (not (false? x)))

  ; 除非是字面量类型,否则解释器得到的结果应该是解释器语言中的值。
  ; 这需要解释器提供一些基本变量或过程。
  (define (false? x) (eq? x false))

  ; 是否if语句
  (define (if? exp) (tagged-list? exp 'if))
  
  (define (dispatch m)
    (cond ((eq? m 'predicate) if-predicate)
          ((eq? m 'consequent) if-consequent)
          ((eq? m 'alternative) if-alternative)
          ((eq? m 'true?) true?)
          ((eq? m 'construct) if-construct)
          ((eq? m 'if?) if?)
          (else (error "Unknown operator" m))))
  dispatch)