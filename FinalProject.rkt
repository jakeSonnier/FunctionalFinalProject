#lang racket
(require net/url)

;;List<String>, List<String> -> List<String>
;;Filters a list of strings that have certain tags
(define (filter-by-tag taglst strlst flst)
  (cond
    [(empty? taglst) flst]
    [else
     (define new-flst (append flst (filter (lambda (s) (string-contains? s (first taglst))) strlst)))
     (filter-by-tag (rest taglst) strlst new-flst)]))

;;String, String, List -> List
;;Splits a string into substrings contained by <>
(define (splitbyarrows raws s l)
  (cond
    [(= (string-length raws) 0) l]
    [(equal? (string-ref raws 0) #\>)
     (define new-l (cons (string-append s ">") l))
     (define new-ls (substring raws 1))
     (define new-s "")
     (splitbyarrows new-ls new-s new-l)]
    [else
     (define new-s (string-append s (substring raws 0 1)))
     (define new-ls (substring raws 1))
     (splitbyarrows new-ls new-s l)]))

;;String -> String
;;Returns a website as a raw string
(define (read-website  website) (port->string
 (get-pure-port (string->url website))))

;;String, List<String> -> List
;;Returns a list of elements with tags corresponding to taglst
;;for a given website
(define (return-strings-with-tags website taglst)
  (define webblob (read-website website))
  (define weblst (splitbyarrows webblob "" '()))
  (filter-by-tag taglst weblst '()))

 
;;(define strings-with-tags (return-strings-with-tags "http://www.arrl.org/making-your-first-contact" '("href" "script")))
;;(define ignore (map writeln strings-with-tags))

;;(define strings-with-tags (return-strings-with-tags "http://www.google.com" '("div" "span")))
;;(define ignore (map writeln strings-with-tags))

;;(define strings-with-tags (return-strings-with-tags "https://www.nasa.gov/" '("img" "src")))
;;(define ignore (map writeln strings-with-tags))