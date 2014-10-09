#| Assignment 1 - Parsing TESTS (due Oct 11, noon) 

***Write the names and CDF accounts for each of your group members below.***
<Name>, <CDF>
<Name>, <CDF>
|#
#lang racket
(require "parsing.rkt")
(require test-engine/racket-tests)

; TODO: WRITE TESTS!!

;parse-html-tag function
(check-expect (parse-html-tag "<html></html>") '("<html>" "</html>"))
(check-expect (parse-html-tag "<hey><html>") '(error "<hey><html>"))
(check-expect (parse-html-tag "<html>") '("<html>" ""))
(check-expect (parse-html-tag "<hey>") '(error "<hey>"))
(check-expect (parse-html-tag "<html") '(error "<html"))

; make-text-parser function
(define parse-hi (make-text-parser "hi"))
(check-expect (parse-hi "hiya!")'("hi" "ya!"))
(check-expect (parse-hi "goodbye hi") '(error "goodbye hi"))

(define parse-tags (make-text-parser "<html>"))
(check-expect (parse-tags "goodbye hi")'(error "goodbye hi"))
(check-expect (parse-tags "<html>goodbye hi")'("<html>" "goodbye hi"))

; parse-non-special-char function 
(check-expect (parse-non-special-char "hi")'(#\h "i"))
(check-expect (parse-non-special-char "<html>")'(error "<html>"))
(check-expect (parse-non-special-char "hi")'(#\h "i"))

; parse-plain-char function
(check-expect (parse-plain-char "hi")'(#\h "i"))
(check-expect (parse-plain-char " hello!")'(error " hello!"))
(check-expect (parse-plain-char "i")'(#\i ""))

; either - function
(check-expect ((either parse-plain-char parse-html-tag) "hello") '(#\h "ello"))
(check-expect ((either parse-plain-char parse-html-tag) "<html>hello") '("<html>" "hello"))
; ALEX CODE BELOW FOR EITHER
(check-expect ((either parse-plain-char parse-html-tag) "<xml>hello") '(error "<xml>hello"))
(check-expect ((either parse-html-tag parse-non-special-char) "<html>byebye") '("<html>" "byebye"))
(check-expect ((either parse-html-tag parse-non-special-char) "regText <tag>") '(#\r "egText <tag>"))
(check-expect ((either parse-html-tag parse-non-special-char) " spaceCharFirst") '(#\space "spaceCharFirst"))
(check-expect ((either parse-html-tag parse-non-special-char) "===") '(error "==="))
(check-expect ((either parse-non-special-char parse-plain-char) "something") '(#\s "omething"))
(check-expect ((either parse-plain-char parse-non-special-char) " spaceChar") '(#\space "spaceChar"))
(check-expect ((either parse-non-special-char parse-plain-char) "/oops") '(error "/oops"))

;both - function 
(check-expect ((both parse-html-tag parse-plain-char) "<html>hello") '(("<html>" #\h) "ello"))
(check-expect ((both parse-html-tag parse-plain-char) "<xml>hello") '(error "<xml>hello"))
(check-expect ((both parse-html-tag parse-plain-char) "<html> hello") '(error "<html> hello"))
(check-expect ((both parse-non-special-char parse-html-tag) " <html>end") '((#\space "<html>") "end"))
(check-expect ((both parse-non-special-char parse-html-tag) ">string") '(error ">string"))
(check-expect ((both parse-non-special-char parse-html-tag) "h<someTag>") '(error "h<someTag>"))

; star function
(check-expect ((star parse-html-tag) "") '(() ""))
(check-expect ((star parse-non-special-char) "") '(() ""))
(check-expect ((star parse-non-special-char) "Hi there") '((#\H #\i #\space #\t #\h #\e #\r #\e) ""))
(check-expect ((star parse-non-special-char) "Hi = Hello") '((#\H #\i #\space) "= Hello"))
(check-expect ((star parse-non-special-char) ">BlaBla") '(() ">BlaBla"))
(check-expect ((star parse-html-tag) "<html>") '(("<html>") ""))
(check-expect ((star parse-html-tag) "<html><html>") '(("<html>" "<html>") ""))
(check-expect ((star parse-html-tag) "<html><html> <html>") '(("<html>" "<html>") " <html>"))
;--- ALEX TEST BOT FOR STAR
(check-expect ((star parse-plain-char) "hi") '((#\h #\i) ""))
(check-expect ((star parse-plain-char) "hi there") '((#\h #\i) " there"))
(check-expect ((star parse-plain-char) "<html>hi") '(() "<html>hi"))
(check-expect ((star parse-html-tag) "") '(() ""))
(check-expect ((star parse-non-special-char) "") '(() ""))
(check-expect ((star parse-non-special-char) "Hi there") '((#\H #\i #\space #\t #\h #\e #\r #\e) ""))
(check-expect ((star parse-non-special-char) "Hi = Hello") '((#\H #\i #\space) "= Hello"))
(check-expect ((star parse-non-special-char) ">BlaBla") '(() ">BlaBla"))
(check-expect ((star parse-html-tag) "<xml></xml>") '(() "<xml></xml>"))
(check-expect ((star parse-html-tag) "a<html>") '(() "a<html>"))
(check-expect ((star parse-html-tag) "<html>") '(("<html>") ""))
(check-expect ((star parse-html-tag) "") '(() ""))
(check-expect ((star parse-html-tag) "<html><html><html><html> <html>hi") '(("<html>" "<html>" "<html>" "<html>") " <html>hi"))


; star Helper function -> parser-lst-output
; output a list of parsed values that is valid from input
(check-expect (parser-lst-output (list) parse-html-tag "") '())
(check-expect (parser-lst-output (list) parse-non-special-char "Hi there") '(#\H #\i #\space #\t #\h #\e #\r #\e))
(check-expect (parser-lst-output (list) parse-non-special-char ">BlaBla") '())
(check-expect (parser-lst-output (list) parse-non-special-char "Hi = Hello") '(#\H #\i #\space))

;star helper function -> count-lst
;count-lst, counts the number of chars in a lst of strings thats the star-helper output
(check-expect (count-lst '("abc" "cdf")) 6)
(check-expect (count-lst '("abc" "")) 3)
(check-expect (count-lst '("" "")) 0)

; parse-html function

; parse-html helper -> parse-start-tags
(check-expect (parse-start-tag "<html>")'("html" ""))
(check-expect (parse-start-tag "<html>dfsafas") '("html" "dfsafas"))

(test)