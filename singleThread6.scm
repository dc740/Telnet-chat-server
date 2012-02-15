;;    Copyright 2012 Emilio Moretti <emilio.morettiATgmailDOTcom>
;;    This program is distributed under the terms of the GNU Lesser General Public License.
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU Lesser General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU  Lesser General Public License for more details.
;;
;;    You should have received a copy of the GNU Lesser General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.



;;#lang mzscheme
 ;;(require mzscheme/bool)
;; (define (stop) (serve 8081))

(define (serve port-no)
    (define main-cust (make-custodian))

    (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
      ;; create two empty lists
    (define-values (counter inList outList nickNames) (values 0 null null  null))
    (define messages null)
    (define mainList null)
      
      
    (define (loop)

      ;; accept new connections
      ;; this should be something like:
      ;; define values newIn newOut (accept-and-handle listener)   
      ;; because we accept one conection per loop
      (define-values (newIn newOut) (accept-and-handle listener))
      (cond [(tcp-port? newIn)
           (display "Hello New User.\n" newOut)
           (newline newOut)
           (flush-output newOut)
           ;; add them to the in and out lists
           ;; should be something like:
           (set! inList (cons newIn inList))
           (set! outList (cons newOut outList))
           ;;add nickname
           (set! counter (+ counter 1))
           (set! nickNames (cons counter nickNames))
           ;;test
           ;;(display (read-char newIn) newOut )
           ;;(close-input-port newIn)
           ;;(close-output-port newOut) 
          ]
            );;ENDCOND

      
      ;;call the function to handle all the incoming messages
      (set! mainList (handle-messages nickNames inList outList null))
      
      (cond [(null? mainList) (set! nickNames null)
                              (set! inList null)
                              (set! outList null)
                              (set! messages null)
                              ]
            [(not (list? (car mainList)));;if it's the only element of the list, then
                   (set! nickNames (list (car mainList)))
                   (set! inList (list (car (cdr mainList))))
                   (set! outList (list (car (cdr (cdr mainList)))))
                   ;;if we add an empty list as a message then it will send a lot of empty brackets!
                   (if (not(null? (car (cdr (cdr (cdr mainList))))))                  
                   (set! messages (list (car (cdr (cdr (cdr mainList))))))
                   (set! messages null))
                   ]
            [else  (set! nickNames (car mainList))
                   (set! inList (car (cdr mainList)))
                   (set! outList (car (cdr (cdr mainList))))
                   ;;if we add an empty list as a message then it will send a lot of empty brackets!
                   (if (not(null? (car (cdr (cdr (cdr mainList))))))                  
                   (set! messages (list (car (cdr (cdr (cdr mainList))))))
                   (set! messages null))
                   ])
      
      
      
      ;;send messages to everyone
      (sendAll messages outList)
      

      (loop))

    (thread loop)
      ;;comment Thread loop and uncomment loop for debuging purposes
    ;;(loop)

  (lambda ()
    ;; this closes the server AND the connections
      (custodian-shutdown-all main-cust))))

;;########################################
;;This funtion accepts connections
;;########################################
  (define (accept-and-handle listener)

    (define-values (in out) (values -1 -1))
    (cond [(tcp-accept-ready? listener) 
             (set!-values (in out) (tcp-accept listener))]
     )
    (values in out))
  
  
  
  ;;########################################
  ;;This function is n charge of processing special parameters
  ;;also this function returns four lists (for example if someone has been disconnected)
  ;;new nickname list
  ;;new inPort list
  ;;new outPort list
  ;;new message list
  ;;########################################
  (define (handle-messages nickNames inClients outClients messages)
    
    (define-values (nick inPort outPort) (values null null null))
    (define message null)
    (define-values (specialMessage parameter) (values null null))
    (define-values (tmpNick tmpIn tmpOut tmpMessages) (values null null null null))
    (define tmpMainList null)
    (define tmp null)
    
    (cond [(not (null? nickNames)) 
             (set!-values (nick inPort outPort) (values (car nickNames) (car inClients) (car outClients)))
             ;;read message
             (cond [(char-ready? inPort)
                                     (set! message (read-line inPort))
                                     (set! message (removeChar message))
                                     (set! tmp  (checkMessage message inPort outPort))
                                     
                                     (cond [(not(null? tmp))
                                            (set! specialMessage (car tmp))
                                            (set! parameter (car (cdr tmp)))
                                            ])
                                     ])
                                    
             ;;return the remaining list
             (cond [(and (not(null? specialMessage)) (string=? specialMessage "exit")) 
                                                     (close-input-port inPort)
                                                     (close-output-port outPort) 
                                                     ;;delete this port
                                                     (set! tmpMainList (handle-messages (cdr nickNames) (cdr inClients) (cdr outClients) messages))
                                                     (cond [ (null? tmpMainList) null]
                                                        [else
                                                         ;;if it's not the last element
                                                         (set! tmpNick (car tmpMainList))
                                                         (set! tmpIn (car (cdr tmpMainList)))
                                                         (set! tmpOut (car (cdr (cdr tmpMainList))))
                                                         (set! tmpMessages(car (cdr (cdr (cdr tmpMainList)))))
                                                         ;;always return a list instead of four elements
                                                         (list tmpNick tmpIn tmpOut tmpMessages)
                                                         ])
                                                      ]
             [(and (not(null? specialMessage)) (string=? specialMessage "nick")) 
                                                     (set! tmpMainList (handle-messages (cdr nickNames) (cdr inClients) (cdr outClients) messages))
                                                     
                                                    (cond [(null? tmpMainList) (list parameter inPort outPort messages)]
                                                        [else
                                                         ;;if it's not the last element
                                                         (set! tmpNick (correctListGenerator parameter (car tmpMainList)))
                                                         (set! tmpIn (correctListGenerator inPort (car (cdr tmpMainList))))
                                                         (set! tmpOut (correctListGenerator outPort (car (cdr (cdr tmpMainList)))))
                                                         (set! tmpMessages(car (cdr (cdr (cdr tmpMainList)))))
                                                         ;;return list
                                                         (list tmpNick tmpIn tmpOut tmpMessages)
                                                     ])
                                                     ]
             [(not(null? message))
                                                    
                                                     (set! tmpMainList (handle-messages (cdr nickNames) (cdr inClients) (cdr outClients) messages))
                                                     (set! message (string-append ": " message))
                                                     (if (not (string? nick)) 
                                                         (set! message (string-append (number->string nick) message))
                                                         (set! message (string-append nick message))
                                                         )
                                                     (cond [(null? tmpMainList) 
                                                            (list nick inPort outPort message)
                                                            ]
                                                           [else
                                                            ;;if it's not the last element
                                                            (set! tmpNick (correctListGenerator nick (car tmpMainList)))
                                                            (set! tmpIn (correctListGenerator inPort (car (cdr tmpMainList))))
                                                            (set! tmpOut (correctListGenerator outPort (car (cdr (cdr tmpMainList)))))
                                                            (set! tmpMessages (correctListGenerator message (car (cdr (cdr (cdr tmpMainList))))))
                                                            
                                                            
                                                            ;;return values
                                                            (list tmpNick tmpIn tmpOut tmpMessages)                                          
                                                            ])
                                                     ]
             [else (set! tmpMainList (handle-messages (cdr nickNames) (cdr inClients) (cdr outClients) messages))
                   (cond[ (null? tmpMainList) (list nick inPort outPort messages)]
                        [else
                         ;;if it's not the last element
                         
                         (set! tmpNick (correctListGenerator nick (car tmpMainList)))
                         (set! tmpIn (correctListGenerator inPort (car (cdr tmpMainList))))
                         (set! tmpOut (correctListGenerator outPort (car (cdr (cdr tmpMainList)))))
                         (set! tmpMessages(car (cdr (cdr (cdr tmpMainList)))))
                         ;;return values ERRORRRR, no se arman asi las listas porque quedan:
                         ;;(2.1 inport.inport
                         
                         (list tmpNick tmpIn tmpOut tmpMessages)
                         ])
                    ]
             )
             
                                    
                                 ]
          [else null]
          )
    
    
    )
  
  
  (define (sendAllMessages messages outPort)
    (define tmpString null)
    (cond [(not(null? messages))
           
           (set! tmpString (car messages))
           (set! tmpString (cleanOutput tmpString))
           (display tmpString outPort)
           (newline outPort)
           (flush-output outPort)
           (sendAllMessages (cdr messages) outPort)
           ]
          )
           
    
    )
    ;;########################################
  ;;
  ;;########################################
  
  (define (sendAll messages outClients)
    (cond [(not (null? outClients))
           (sendAllMessages messages (car outClients))
          (sendAll messages (cdr outClients))
          ]
    );;ENDCOND
   );;ENDDEF
  

  ;;########################################
  ;;Checks for commands on the messages
  ;;########################################
(define (checkMessage message currentInPort currentOutPort) 
                                (cond [(string=? message "exit") 
                                      ;;notify the main thread that we are going out
                                      (display "Goodbye" currentOutPort)
                                      (newline currentOutPort)
                                      (flush-output currentOutPort)
                                      ;;terminate de connection
                                      (close-input-port currentInPort)
                                      (close-output-port currentOutPort)
                                      (list "exit" 0)
                                      ]
                                      [(and (< 4 (string-length message)) (string=? (substring message 0 4) "nick") )
                                      (display "Nick changed" currentOutPort)
                                      (newline currentOutPort)
                                      (flush-output currentOutPort)
                                       
                                      (list "nick" (substring message 5))
                                      ]
                                      [else null]
                                      )
                                
  )
  ;;Useful funtions
  ;;
  
;;Removes the last char of a received message (always \r)
  (define (removeChar aString) (substring aString 0 (- (string-length aString) 1)) )
  
  (define (cleanOutput aString) (cond [(list? aString) (car aString)]
                                      [else aString]))
  (define (correctListGenerator a b) (cond [(list? b) 
                                            (cons a b)]
                                           [else
                                            (list a b)]
                                           )
    )
  (define (start) (serve 8081))
