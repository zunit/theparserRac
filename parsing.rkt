#| Assignment 1 - Parsing (due Oct 11, noon)

***Write the names and CDF accounts for each of your group members below.***
<Name>, <CDF>
<Name>, <CDF>
|#
#lang racket
(provide parse-html-tag make-text-parser
         parse-non-special-char parse-plain-char
         either both star
         parse-html
         parser-lst-output
         count-lst
         parse-start-tag
         )

#|
(parse-html-tag str)
  If str starts with "<html>", returns a pair (list "<html>" rest), where
  rest is the part of str after "<html>".
  Otherwise, returns (list 'error "hi"), signifying an error.

> (parse-html-tag "<html></html>")
'("<html>" "</html>")
> (parse-html-tag "<hey><html>")
'(error "<hey><html>")
|#
(define (parse-html-tag str) 
  ; check if first is <html>
  ; check if last is <html>
  ; return both
  ; otherwise return '(error string)
  (cond [(equal? (string-length str) 0) (list 'error str)]
        [(< (string-length str) 6) (list 'error str)]
        [(string=? (substring str 0 6) "<html>") (list (substring str 0 6) (substring str 6 (string-length str)))]
        [else (list 'error str)])
  )


#|
(make-text-parser t)
  Return a parser that tries to read *one* occurrence of t at the
  start of its input.

> (define parse-hi (make-text-parser "hi"))
> (parse-hi "hiya!")
'("hi" "ya!")
> (parse-hi "goodbye hi")
'(error "goodbye hi")
|#
(define (make-text-parser t)
  ; find the len of the t
  (lambda (x)
    (if (string=? t (substring x 0 (string-length t))) 
        (list (substring x 0 (string-length t)) (substring x (string-length t) (string-length x))) 
        (list 'error x))
    ))
; TODO substring length 0 doesn't work! 


#|
(parse-non-special-char str)
  Try to parse *one* non-special character at the start of str.

> (parse-non-special-char "hi")
'(#\h "i")
> (parse-non-special-char "<html>")
'(error "<html>")
|#
(define (parse-non-special-char str) 
  (cond [(equal? (string-length str) 0) (list 'error str)]
        [(string=? "<" (substring str 0 1)) (list 'error str)]
        [(string=? ">" (substring str 0 1)) (list 'error str)]
        [(string=? "=" (substring str 0 1)) (list 'error str)]
        [(string=? "\"" (substring str 0 1)) (list 'error str)]
        [(string=? "/" (substring str 0 1)) (list 'error str)]
        [else (list (string-ref str 0) (substring str 1 (string-length str)))]  
        ))


#|
(parse-plain-char str)
  Try to parse *one* non-special, non-white character at the start of str.

> (parse-plain-char "hi")
'(#\h "i")
> (parse-plain-char " hello!")
'(error " hello!")
|#
(define (parse-plain-char str) 
  (cond [(string=? "" str) (list 'error str)]
        [(string=? " " (substring str 0 1))(list 'error str)]
        [(equal? (first (parse-non-special-char str)) 'error) (parse-non-special-char str)]
        [else (list (string-ref str 0) (substring str 1 (string-length str)))]))
#|
  (if (string=? " " (substring str 0 1)) 
      (list 'error str)
      (list (string-ref str 0) (substring str 1 (string-length str)))
      ))
|#


#| Parsing Combinators |#

#|
(either parser1 parser2)

  Return a new parser that does the following:
    - Try to apply parser 1; if success, return that result
    - Otherwise, return the result of applying parser 2

> ((either parse-plain-char parse-html-tag) "hello")
'(#\h "ello")
> ((either parse-plain-char parse-html-tag) "<html>hello")
'("<html>" "hello")
> ((either parse-plain-char parse-html-tag) "<xml>hello")
'(error "<xml>hello")
|#
(define (either parser1 parser2) 
  (lambda (x) 
    (if (equal? (first (parser1 x)) 'error ) 
        (parser2 x) 
        (parser1 x))
    )
  )


#|
(both parser1 parser2)

  Return a new parser that does the following:
    - Apply parser1; if failure, return failure
    - Otherwise, apply parser2 to the rest of the string
      not parsed by parser1
    - If failure, emit failure, together with *original* string
    - If success, return (list data rest), where data is a *LIST*
      containing the data parsed by parser1 and parser2, in that order,
      and rest is the part of the string not parsed by either
      parser1 or parser2.

> ((both parse-html-tag parse-plain-char) "<html>hello")
'(("<html>" #\h) "ello")
> ((both parse-html-tag parse-plain-char) "<xml>hello")
'(error "<xml>hello")
> ((both parse-html-tag parse-plain-char) "<html> hello")
'(error "<html> hello")
|#

; AKA it has to work for both!!

(define (both parser1 parser2)
  (lambda (x) 
    (if (equal? (first (parser1 x)) 'error) 
        (parser1 x)
        (if (equal? (first (parser2 (second (parser1 x)))) 'error) 
            (parser2 x)
            (append 
             (list (list (first(parser1 x)) (first (parser2 (second (parser1 x))))))
             (rest (parser2 (second (parser1 x)))))
            ) 
        )))


#|
(star parser)

  Return a new parser that tries to parse using parser
  0 or more times, returning as its data a list of *all*
  parsed values. This new parser should be *greedy*: it
  always uses the input parser as many times as it can,
  until it reaches the end of the string or gets an error.

  Note that the new parser never returns an error; even if
  the first attempt at parsing fails, the data returned
  is simply '().

> ((star parse-plain-char) "hi")
'((#\h #\i) "")
> ((star parse-plain-char) "hi there")
'((#\h #\i) " there")
> ((star parse-plain-char) "<html>hi")
'(() "<html>hi")
|#

#| 
case0 -  IF + for 1 char 
(list (star-helper (list) parser x) 
              (substring x 
                         (length (star-helper (list) parser x)) 
                         (string-length x)))

case1 - <tags> (PLURAL)
(if (char? (first (star-helper (list) parser x)))  

|#

(define (star parser) 
  (lambda (x)
    (if (equal? (first (parser x)) 'error) 
        (list (list) x)     
        (if (char? (first (parser-lst-output (list) parser x)))
            (list (parser-lst-output (list) parser x) 
                  (substring x 
                             (length (parser-lst-output (list) parser x)) 
                             (string-length x)))
            (list (parser-lst-output (list) parser x)
                  (substring x 
                             (count-lst (parser-lst-output (list) parser x)) 
                             (string-length x))))
        )))
#|
(parser-lst-output lst parser str)

  Return a list of parser's result. parser
  0 or more times, returning as its data a list of *all*
  parsed values. it always uses the input parser as many 
  times as it can, until it reaches the end of the string 
  or gets an error.


 >(parser-lst-output (list) parse-html-tag "")
 '()
 >(parser-lst-output (list) parse-non-special-char "Hi there")
 '(#\H #\i #\space #\t #\h #\e #\r #\e)
 >(parser-lst-output (list) parse-non-special-char ">BlaBla")
 '()
 >(parser-lst-output (list) parse-non-special-char "Hi = Hello")
 '(#\H #\i #\space)

|#
(define (parser-lst-output lst parser x)
  (cond [(empty? x) x]
        [(equal? (first (parser x)) 'error) (list)] 
        ;(append lst (list (first (parser x))) (list (first (parser (second (parser x))))))
        [else (append lst (list (first (parser x))) (parser-lst-output lst parser (second(parser x))))]
        ))

#|
(count-lst lst)

  Return a number that is the sum of each elements string-length

 >(count-lst '("" ""))
 0
 >(count-lst '("abc" ""))
 3
 >(count-lst '("abc" "cdf"))
 6

|#
; counts the string-length of each element in the lst and return the total 
(define (count-lst lst)
  (count-lst-helper 0 lst))

(define (count-lst-helper acc lst)
  (if (empty? lst)
      acc
      (count-lst-helper (+ acc (string-length (first lst))) (rest lst)))) 


#| HTML Parsing |#

#|
(parse-html str)

  Parse HTML content at the beginning of str, returning (list data rest),
  where data is the tree representation of the parsed HTML specified in the
  assignment handout, and rest is the rest of str that has not been parsed.

  If the string does not start with a valid html string, return
  (list 'error str) instead.

> (parse-html "<html><body class=\"hello\" >Hello, world!</body></html> Other")
'(("html"
   ()
   ("body"
    (("class" "hello"))
    "Hello, world!"))
  " Other")
> (parse-html "<blAh></blAh>")
'(("blAh"
   ()
   "")
  "")
> (parse-html "<body><p>Not good</body></p>")
'(error "<body><p>Not good</body></p>")
|#

#|
> (parse-op "<html")
'("<" "html")
 
> (parse-cl ">html")
'(">" "html")
|#
(define parse-op (make-text-parser "<"))
(define parse-cl (make-text-parser ">"))
(define parse-sl (make-text-parser "/"))

#|

 > (parse-start-tag "<html>")
 '("html" "")

 > (parse-start-tag "<html>dfsafas")
 '("html" "dfsafas")

 > (parse-sl "html")
 '(error "html")
|#
(define (parse-start-tag str)
  (if (equal? (first (parse-op str)) 'error) 
      (list 'error str)
      (if (string=? (first (parse-cl (second ((star parse-non-special-char) (second (parse-op str)))))) ">")
          (list (list->string (first ((star parse-non-special-char) (second (parse-op str))))) 
                (second (parse-cl (second ((star parse-non-special-char) (second (parse-op str)))))))
          (list 'error str))        
      ))
#|

 > (parse-end-tag "</html>")
 '("html" "")

 > (parse-end-tag "</html>dfsafas")
 '("html" "dfsafas")

|#

(define (parse-end-tag str)
  (if (equal? (first (parse-op str)) 'error) ;check if first char is "<"
      (list 'error str)
      (if (equal? (first (parse-sl (second (parse-op str)))) 'error) ;check second char is "/"
          (list 'error str)
          (if (string=? (first (parse-cl (second ((star parse-non-special-char) 
                                                  (second (parse-sl (second (parse-op str)))) ;valid "</"
                                                  )))) 
                               ">")
              (list (list->string (first ((star parse-non-special-char) (second (parse-sl (second (parse-op str)))))))
                    (second (parse-cl (second ((star parse-non-special-char) (second (parse-sl (second (parse-op str)))))))))
              (list 'error str))
          )))

#|
 > (parse-start-end-tag "<html></html>")
 '("html" "")

 > (parse-start-end-tag "<html>asdf</html>")
 '("html" "asdf")
|#

(define (parse-html str) (void)
  )
