#lang racket
(require 2htdp/batch-io)

(define file (read-file "test.txt"))

(define file-lt (string-replace file "<" "&lt"))

(define file-gt (string-replace file-lt ">" "&gt"))

(define file-br (string-replace file-gt "\n" " <br> "))

(define is-id? ;;(string-append (string-append (string-append "<span class=id>" (car lst)) "</span>") aux-file)
  (lambda (atom)
    (cond
      [(null? atom) #f]
      [(regexp-match-exact? #rx"[a-zA-Z_]+[a-zA-Z_0-9]*;?" atom) #t]
      [else #f])))

(define is-int? ;;(string-append (string-append (string-append "<span class=int>" (car lst)) "</span>") aux-file)
  (lambda (atom)
    (cond
      [(null? atom) #f]
      [(regexp-match-exact? #rx"[0-9]+;?" atom) #t]
      [else #f])))

(define is-real? ;;(string-append (string-append (string-append "<span class=real>" (car lst)) "</span>") aux-file)
  (lambda (atom)
    (cond
      [(null? atom) #f]
      [(regexp-match-exact? #rx"[+-]?[0-9]+[.][0-9]+([eE][+-]?[0-9]+)?;?" atom) #t]
      [else #f])))

(define is-op? ;;(string-append (string-append (string-append "<span class=op>" (car lst)) "</span>") aux-file)
  (lambda (atom)
    (cond
      [(null? atom) #f]
      [(regexp-match-exact? #rx"[a-zA-Z]*([+][+]|[+]|[-][-]|[-]|[*]|[%]|[/]|[\\^]|[!][=]|&lt&lt|&gt&gt|&gt=|&lt=|[=][=]|&lt|&gt|=)" atom) #t]
      [else #f])))

(define is-comment? ;;(string-append (string-append (string-append "<span class=comment>" (car lst)) "</span>") aux-file)
  (lambda (atom)
    (cond
      [(null? atom) #f]
      [(regexp-match-exact? #rx"[/][/][ a-zA-Z_0-9]*;?" atom) #t]
      [else #f])))

(define is-string? ;;(string-append (string-append (string-append "<span class=string>" (car lst)) "</span>") aux-file)
  (lambda (atom)
    (cond
      [(null? atom) #f]
      [(regexp-match-exact? #rx"\\?*[\"][ a-zA-Z_0-9]*\\?*[\"];?" atom) #t]
      [else #f])))

(define is-reserved? ;;(string-append (string-append (string-append "<span class=reserved>" (car lst)) "</span>") aux-file)
  (lambda (atom)
    (cond
      [(null? atom) #f]
      [(regexp-match-exact? #rx"if|else|while|for|do|const|int|float|string|char|void|return|continue|using|namespace|break|bool|static|new|null|false|switch|this|throw|case|true|catch|try|class|public|virtual|double|cout|cin|long;?" atom) #t]
      [else #f])))

(define is-punctuation? ;;(string-append (string-append (string-append "<span class=punctuation>" (car lst)) "</span>") aux-file)
  (lambda (atom)
    (cond
      [(null? atom) #f]
      [(regexp-match-exact? #rx"\\[|\\]|\\{|\\}|\\(|\\)|\\[\\]|\\{\\}|\\(\\);*" atom) #t]
      [else #f])))

(define is-include? ;;(string-append (string-append (string-append "<span class=include>" atom) "</span>") aux-file)
  (lambda (atom)
    (cond
      [(null? atom) #f]
      [(regexp-match-exact? #rx"#include[ ]*&lt[a-zA-Z_0-9]*&gt|#include|&lt[a-zA-Z_0-9]*&gt" atom) #t]
      [else #f])))

(define word-lexer
  (lambda (lst)
    (cond
      [(null? lst) ""]
      [(is-include? (car lst)) (string-append (string-append (string-append "<span class=include> " (car lst)) " </span>") (word-lexer (cdr lst)))]
      [(is-reserved? (car lst)) (string-append (string-append (string-append "<span class=reserved> " (car lst)) " </span>") (word-lexer (cdr lst)))]
      [(is-id? (car lst)) (string-append (string-append (string-append "<span class=id> " (car lst)) " </span>") (word-lexer (cdr lst)))]
      [(is-int? (car lst)) (string-append (string-append (string-append "<span class=int> " (car lst)) " </span>") (word-lexer (cdr lst)))]
      [(is-real? (car lst)) (string-append (string-append (string-append "<span class=real> " (car lst)) " </span>") (word-lexer (cdr lst)))]
      [(is-op? (car lst)) (string-append (string-append (string-append "<span class=op> " (car lst)) " </span>") (word-lexer (cdr lst)))]
      [(is-string? (car lst)) (string-append (string-append (string-append "<span class=string> " (car lst)) " </span>") (word-lexer (cdr lst)))]
      [(is-comment? (car lst)) (string-append (string-append (string-append "<span class=comment> " (car lst)) " </span>") (word-lexer (cdr lst)))]
      [(is-punctuation? (car lst)) (string-append (string-append (string-append "<span class=punctuation> " (car lst)) " </span>") (word-lexer (cdr lst)))]
      [else (string-append (car lst) (word-lexer (cdr lst)))])))

(define html-head (string-append "<!DOCTYPE html><html lang=\"es\"><head><meta charset=\"UTF-8\"><title>Analizador Lexico</title><link href=\"styles.css\" rel=\"stylesheet\"></head><body>
" (word-lexer (string-split file-br))))

(define html (string-append html-head "</body></html>"))

(define test (write-file "prueba.html" html))


