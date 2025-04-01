 #lang racket

;Define a a node structure  and display it in transparent way for debugging and printing
(struct node (type children) #:transparent)

;custom tokenize
(define (tokenize filename)
  ;open the input file and pass the port
  (define (custom-read port)
    ;look at the next character in the input streem
    (define next-char (peek-char port))
    (cond
      ;if next charact is end of file return  end of file
      [(eof-object? next-char) eof]

      ;check for new line
      [(char=? next-char #\newline)
       ;read the new line character
       (read-char port)
       ;increment line
       (current-line (add1 (current-line)))
        ; read the next character
       (custom-read port)]
        ;check if the next character is a white space
      [(char-whitespace? next-char)
       ;discard it with reach-char port
       (read-char port)
       ;read the next character
       (custom-read port)]
      ;check for semi colon
      [(char=? next-char #\;)
       (read-char port)
       'SEMICOLON] ;returns the semi colon
      ;if the next character is an open parenthesis
      [(char=? next-char #\()
       (read-char port)
       'OPEN-PAREN] ;return open parenthesis
      ;check for close parenthesis
      [(char=? next-char #\))
       (read-char port)
       'CLOSE-PAREN] ;return close parenthesis
      [else
       ;read the next token
       (read port)]))
 
  
  ;function that takes is file name and builds a list of tokens and adds an end of file marker $$
  (call-with-input-file filename
    (lambda (port)
      (define (read-tokens)
        (define token (custom-read port))
        (if (eof-object? token)
            '($$); add end of file marker
            (cons token (read-tokens))))
      (read-tokens))))

;;Two help functions  to help in tracking the current state during token matching and parsing operations
(define current-tokens (make-parameter '())) ; current-token initialized to empty list
(define current-line (make-parameter 0)) ;current line initialized to 0

;;Next-Token function designed to retrieve and advance through a list of tokens
(define (next-token)
  ;;current-token returns the list of tokens
  (when (not (null? (current-tokens))) ;check if the current list of token is not empty
   ; return the rest of the tokens
    (current-tokens (cdr (current-tokens)))))

;view the current token in the list without modifying the list
(define (peek-token)
  (if (null? (current-tokens)) ;check if list is empty
      #f
      ;return the first token in the list of tokens
      (car (current-tokens))))

;checking if a function matches a specificied value in the list
;@param token current token to compare
;@param value to compare it with token
(define (match-token? token value)
  ;if token and value are equal return true
  (equal? token value))

   ;process a sequence of tokens
(define (parse-program)
 ( define tokens (current-tokens)) ;initialize tokens with current list of token
  ; a recursive helper function  that takes in statements as parameter initial with empty list
(let loop ([statements '()])
  (cond
    ;check current tokene if it is an end of file marker
    [(match-token? (peek-token) '$$)
     ;reverse the program if end of file marker is found
     (node 'PROGRAM (reverse statements))]
    [else
     ;if no end of file marker execute the statemen
     (define stmt (parse-statement))
     (loop (cons stmt statements))]))) ; add the statement in front of statements

;;processing individual statements that returns an abstract syntax tree
(define (parse-statement)
  (define current (peek-token)) ;store the current tokens
  (cond ; checking for valid statements
    [(and (symbol? current); check if the current token is a symbol
          ;check if the next token is an assignment
          (match-token? (cadr (current-tokens)) '=))
     ;assigne the current token to a variable
     (define var current)
     (next-token); consumer the current token and move to next in the list
     (next-token)
     ;parse the right hand side of the assignment
    (define expr (parse-expression))
    ;check for semi colons
    (when (not (match-token? (peek-token) 'SEMICOLON))
      ;if not semicolon at the end of statemt raise an error
      (error (format "Syntax error on line ~a: Expected ';' after assignment" (current-line))))
    ;consume the next token and move to the next token
    (next-token)
    (node 'ASSIGN-STMT (list var expr))]

    ;check if the current token is an if
    [(match-token? current 'if)
     ;move to next token
     (next-token)
     ;check for open parenthesis
     (when (not (match-token? (peek-token) 'OPEN-PAREN))
       ;raise an error
       (error (format "Syntax error on line ~a: Expected '(' after if " (current-line))))
     ;if open parenthesis is found move next
     (next-token)
     (define condition (parse-expression))
     ;checking for closing parenthesis
     (when (not (match-token? (peek-token) 'CLOSE-PAREN))
       (error (format "Syntax error on line ~a: expected ')' after condition" (current-line))))
     ;move to next token
     (next-token)
     ;parse list of statements that make up the body of the if statements 
     (define body (parse-statement-list))
     ;checking of end of if statement that ends the if statement
     (when (not (match-token? (peek-token) 'endif))
       (error (format "Syntax error on line ~a: expected 'endif'"(current-line))))
     ;move to next token
     (next-token)
     ;;checking for semicolons
     (when (not (match-token? (peek-token) 'SEMICOLON))
       ;not semi colon raise an error
       (error (format "Syntax error on line ~a: Expected ';' after endif" (current-line))))
     ;move to the next token
     (next-token)
     (node 'IF-STMT (list condition body))]
    
    ;checking is the current token if read keyword
    [(match-token? current 'read)
     (next-token)
     ;getting the variable to read from input
     (define var (peek-token))
     ;checking if the token is symbol
     (when (not (symbol? var))
       (error (format "Syntax error on line ~a: Expected variable after read" (current-line))))
     (next-token)
     (when (not (match-token? (peek-token) 'SEMICOLON))
       (error (format "Syntax error on line ~a: Expected ';' after read" (current-line))))
     (next-token)
     (node 'READ-STMT (list var))]
    ;;check for write key word
    [(match-token? current 'write)
     (next-token)
     (define expr (parse-expression))
     (when (not (match-token? (peek-token) 'SEMICOLON))
       (error (format "Syntax error on line ~a: Expected ';' after write" (current-line))))
     (next-token)
     (node 'WRITE-STMT (list expr))]
    [else
     (error (format "Syntax error on line ~a: unexpected token ~a" (current-line) current))]))

;Function to parse a list of statements
(define (parse-statement-list)
  ;initialize an empty list of statements
  (define statements '())
  ;recursive helper function  to parse individual statements
  (let loop()
    ;look at the current token
    (define current (peek-token))
    (cond
      ;check if the current token is empty
      [ (or (not current)
            ;check if the current token matches end of if statemen
            (match-token? current 'endif)
            ;check if the current token is $$
            (match-token? current '$$))
        ;return statement list in  reverse order to maintain correct order
        (node 'STATEMENT-LIST (reverse statements))]
      [else
       (define stmt (parse-statement))
       (set! statements (cons stmt statements))
       (loop)]))
  (node 'STATEMENT-LIST (reverse statements)))
    

;parse
(define (parse-expression)
  (define primary 
  (cond
    ;check for symbol
    [(symbol? (peek-token))
     ;store symbol in id variable
     (define id (peek-token))
     ;move to the next token
     (next-token)
     (node 'VARIABLE (list id))]
    ;check for a number
    [(number? (peek-token))
     ;store it in num variable
     (define num (peek-token))
     ;move to the next token
     (next-token)
     ;create number variables
     (node 'NUMBER (list num))]
    [else
     (error (format "Syntax error on line ~a: Expected variable or number" (current-line)))]))
  ;building abstract syntax tree
  (let loop ([current-expr primary])
    ;operators stores the current token
    (define op (peek-token))
    (cond
      ;check for addition operator
      [(match-token? op '+)
       (next-token)
       (define right (parse-expression))
       (node 'ADD-EXPR (list current-expr right))]
      ;; Subtraction operator
    [(match-token? op '-)
     (next-token)
     (define right (parse-expression))
     (node 'SUB-EXPR (list current-expr right))]
    ;comparison operators
    [(member op '(< <= > >= == !=))
     (next-token)
     (define right (parse-expression))

     ;; convert an operator into string
     (define opt-string (symbol->string op))
     
     (node 'COMPARE-EXPR 
           (list (node 'COMPARE-OP (list opt-string)) 
                 current-expr 
                 right))]
    ;no more operators
    [else current-expr])))

 (define (type-of obj)
     (cond
    [(list? obj) 'list] ; return a list is the object is  a list
    [(vector? obj) 'vector] ; vector 
    [(hash? obj) 'hash] ; return a hash
    [(string? obj) 'string] ; returns a string
    [(number? obj) 'number]; return a number
    [(symbol? obj) 'symbol] ; return a symbol
    [(boolean? obj) 'boolean] ; returns a boolean
    [(procedure? obj) 'procedure] ; returns a procedures
    [(void? obj) 'void] ; returns a void
    [(null? obj) 'null] ; return null
    [(char? obj) 'character]; returns a character
    [(pair? obj) 'pair] ; returns a pair
    [else 'unknown]))

;debug function
(define (debug-function node [level 0])
  ;create indentation by multiplying the current level by 2 to determine level of spacing
  (define indent (make-string (* level 2) #\space))
  ;display node type
  (printf "~a[~a] Type: ~a\n" indent level (node-type node))
  ;display children's type
  (printf "~a[~a] children type: ~a\n" indent level (
                                                     if (list? (node-children node))
                                                        "list"
                                                        (format "~a" (type-of (node-children node)))))
          ;; display the count of children by display the length
  (printf "~a[~a] children count: ~a\n" indent level ( if (list? (node-children
                                                                          node))
                                                                  (length (node-children
                                                                           node))
                                                                  1))
          ;displaying the children's type
          (when (list? (node-children node));check if children returns a list
            (for ([child (in-list (node-children node))]
                  [ i ( in-naturals)])
              (printf "~a[~a] child ~a type: ~a\n" indent level i
                      (cond
                        [(node? child) "node"]; display node
                        [(list? child) "list"] ; display list
                        [(symbol? child) "symbol"] ; display symbol
                        [(string? child) "string"]; display string
                        [(number? child) "number"]; display number
                        [else (format "Unknown: ~a" (type-of child))]))))
          ;check if the node has children
          (when (list? (node-children node))
            (for ([child (in-list (node-children node))]
                  [i (in-naturals)]) ;index assigned to natural numbers
              (when (node? child)
                (debug-function child (add1 level ))))))
                        
  
     
          
  

;; function pretty-print-tree designed to  tree that is generated by a parser
(define (pretty-print-tree tree [indent 0])
  ;helper function to diplay spaces
  (define (print-indent n)
    ;using for loop to print spaces n times
    (for ([i (in-range n)])
      (display "  ")))
  (cond
    ;check is the node is a tree
    [(node? tree)
  (print-indent indent)
  (display "(") ; display opening parenthesis
  (display (node-type tree)) ; display the type of the tree

  ;retrieving the node's children
  (define children (node-children tree))
  (cond
    ;check if there's more children
    [(list? children)
  (for ([child children])
    (display "\n")
       (pretty-print-tree child (add1 indent)))]
    ;if a child  is not a list, display it
    [else
     (display "\n")
     (print-indent (add1 indent))
       (display children)])

  (display "\n")
  (print-indent indent)
  (display ")")]

    ;if list process each item
    [(list? tree)
     (for ([item tree])
       (pretty-print-tree item indent ))]

    ;; If it's a symbol, number, or string, print it directly
    [(or (symbol? tree) (number? tree))
     (print-indent indent)
     (display tree)]
    
    [(string? tree)
     (print-indent indent)
     (display "\"") ;display string
     (display tree)
     (display "\"")]
    
    ;; For any other type, convert to string and print
    [else
     (print-indent indent)
     (display (format "~a" tree))]))
 
;main function to call the file with the source code to generate  an parse tree
(define (parse filename)
  ;handling exceptions
  (with-handlers 
    ([exn:fail? 
      (lambda (exn)
        ;display the exception messafe
        (format "Syntax error: ~a" (exn-message exn)))])
    
    ;; Tokenize the input file
    (current-tokens (tokenize filename))
    
    ;; Parse the program
    (define parse-tree (parse-program))
    
    ;; Display the formatted parse tree
    (display "Parse successful! \n\n")
    (pretty-print-tree parse-tree)
    (newline)
    
    ;; Return success message
    (format "Parse completed successfully.")))

;call the parse
(parse "file1.txt")
  
       
    
      