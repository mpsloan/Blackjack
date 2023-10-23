#lang racket

; global definitions of faces and suits of cards
(define faces '(2 3 4 5 6 7 8 9 10 J Q K A))
(define suits '("\u2663" "\u2666" "\u2665" "\u2660"))

; combine lists to make deck
; expects no parameters
; returns list of 52 pairs (deck of cards)
(define make-deck
  ; deck starts at empty
  (let ([deck '()])
  ; nested for loop to iterate through faces and suits
  (for ([i faces])
    (for ([j suits])
      ; adding a new pair to the deck each iteration
      (set! deck (cons (cons i j) deck))))
    ; return deck
    deck))

; determine best value of hand
; hand is a list of cards being evaluated
; returns integer of best value of hand
(define (eval-hand hand)
  ; boolean to see if ace is in hand
  (let ([ace #f])
    ; sum starts at 0
    (let ([sum 0])
      ; loop to cycle through hand
      (for ([i hand])
        ; check if face is a number
        (if (number? (car i))
            ; add the number to the sum
            (set! sum (+ (car i) sum))
            ; check if it's an ace
            (if (equal? (car i) 'A)
                ; set to true when ace is in deck
                (set! ace #t)
                ; if it's a face card that isn't an ace add 10
                (set! sum (+ 10 sum)))))
      ; if ace is in deck
      ; see if 11 would bust the hand
      ; if it would bust, add 1, else add 11
      (if (equal? #t ace)
          (if (> (+ sum 11) 21)
              (set! sum (+ 1 sum))
              (set! sum (+ 11 sum)))
          ; ace isn't in hand so do nothing
          (set! sum (+ 0 sum)))
      ; return sum
      sum)))

; create two card hand
; deck is the entire deck of cards
; returns top two cards from the hand
(define (deal! deck)
  ; hand starts out as empty list
  (let ([hand '()])
    ; since a hand starts with two cards, loop twice
    (for ([i 2])
      ; adding the first card from the deck to the hand
      ; box and unbox allow for me to pass the deck by reference
      ; meaning I can change the memory of the deck (e.g. remove an element)
      ; without returning the deck
      (set! hand (cons (car (unbox deck)) hand))
      ; setting the deck to the cdr of the deck since first card is removed
      (set-box! deck (cdr (unbox deck))))
  ; return hand
  hand))

; takes top card from deck and places it in hand
; deck is the entire deck of cards
; hand is the players hand that is getting hit
; returns nothing, just alters the deck and hand
(define (hit! deck hand)
  ; adds the first card from the deck to the hand
  (set-box! hand (cons (car (unbox deck)) (unbox hand)))
  ; takes the first card of the deck off
  (set-box! deck (cdr (unbox deck))))

; displays hand (or if dealer some of hand)
; hand is the hand that is being displayed
; how is what determines if the full hand or partial hand is shown (give 'Part or 'Full)
; description is the string that distinguishes the dealer's hand from the player's
; returns nothing
(define (show-hand hand how description)
  ; display whether hand is dealer of full
  (display description)
  ; if how = Full display full hand
  ; else block first card and display rest of hand for dealer
  (if (equal? how 'Full)
      (display (unbox hand))
      ; had to use printf to put a string and argument together
      (printf "***** ~a" (cdr (unbox hand)))))

; main method to supply user interface and run the functions to simulate blackjack
(define (main)
  ; declaring deck and shuffling
  (let ([deck (box make-deck)])
    (set-box! deck (shuffle (unbox deck)))
    ; start user with $500
    (let ([bank 500])
      ; declare bet variable
      (let ([bet 0])
        ; string to decide if user want to continue playing or not
        (let ([continue ""])
          ; user this notation for infinite loops, cycles through all natural numbers
          ; breaks out of loop if user enters n (meaning they don't want to continue playing
          (for ([i (in-naturals)])
            #:break (or (= bank 0) (or (equal? "n" continue) (equal? "N" continue)))
            ; read in bet ammount
            (printf "You have ~a dollars, how much money would you like to bet? " bank)
            (set! bet (read-line (current-input-port)))
            (set! bet (string->number bet))
            ; make sure user didn't bet more money than they have
            (if (> bet bank)
                ; loop until they fix it
                (for ([i (in-naturals)])
                  #:break (<= bet bank)
                  (display "You bet more money than you have, please try again")
                  (set! bet (read-line (current-input-port)))
                  (set! bet (string->number bet))
                  ) ; do nothing because input is valid
                (set! bank (+ bank 0)))
            ; take valid bet from bank
            (set! bank (- bank bet))
            ; define user and dealer hand and output them to the screen
            (let ([user-hand (box (deal! deck))])
              (let ([dealer-hand (box (deal! deck))])
                (newline)
                (show-hand user-hand 'Full "You have: ")
                (newline)
                (show-hand dealer-hand 'Part "The dealer has: ")
                (newline)
                ; user choice to hit or stay
                (let ([choice ""])
                  ; infinite loop again that breaks when user selects stay (s)
                  (for ([k (in-naturals)])
                    #:break (or (equal? choice "s") (equal? choice "S"))
                    (display "Would you like to hit or stay? (h/s) ")
                    (set! choice (read-line (current-input-port)))
                    ; conditional to handle hit or stay decision
                    (cond
                      [(or (equal? choice "h") (equal? choice "H"))
                       ; hit and show hand after every hit
                       (hit! deck user-hand)
                       (show-hand user-hand 'Full "You have: ")]
                      [(or (equal? choice "s") (equal? choice "S"))
                       ; show final hand if user stays
                       (show-hand user-hand 'Full "You finished with: ")]
                      [else (display "invalid choice")])
                    (newline))
                  )
                (newline)
                
                ; dummy variables to handle else conditions, I'll set to true and false
                (let ([user-bust #f])
                  (let ([dealer-bust #f])
                    
                    ; determine if the user busted
                    (if (> (eval-hand (unbox user-hand)) 21)
                        ; if user busted, dealer doesn't have to play bc the dealer already won
                        (set! user-bust #t)
                        ; if user didn't bust, allow dealer to hit until hand is >= 17
                        (for ([l (in-naturals)])
                          #:break (>= (eval-hand (unbox dealer-hand)) 17)
                          (hit! deck dealer-hand)
                          (show-hand dealer-hand 'Part "The dealer has: ")
                          (newline)))
                    
                    
                    (show-hand dealer-hand 'Full "The dealer finished with: ")
                    (newline)
                    
                    ; seeing if the dealer busted
                    (if (> (eval-hand (unbox dealer-hand)) 21)
                        (set! dealer-bust #t)
                        (set! dealer-bust #f))
                    
                    ; conditional to handle end of game clean up
                    (cond
                      ; check if user-busted
                      [(equal? #t user-bust)
                       (printf "You have busted! The dealer won with: ~a" (eval-hand (unbox dealer-hand)))]
                      ; check if dealer busted
                      [(equal? #t dealer-bust)
                       (printf "The dealer busted! You won with: ~a" (eval-hand (unbox user-hand)))]
                      ; if dealer hand > user hand, dealer wins
                      [(> (eval-hand (unbox dealer-hand)) (eval-hand (unbox user-hand)))
                       (printf "You have lost the hand. The dealer had: ~a and you had ~a" (eval-hand (unbox dealer-hand)) (eval-hand (unbox user-hand)))]
                      ; if dealer hand < user hand, user wins
                      [(< (eval-hand (unbox dealer-hand)) (eval-hand (unbox user-hand)))
                       (printf "You have won the hand. You had ~a and the dealer had ~a" (eval-hand (unbox user-hand)) (eval-hand (unbox dealer-hand)))]
                      ; if hands are equal, tie happened and dealer still wins
                      [(= (eval-hand (unbox dealer-hand)) (eval-hand (unbox user-hand)))
                       (printf "You have tied with the dealer. You have: ~a and the dealer has ~a" (eval-hand (unbox user-hand)) (eval-hand (unbox dealer-hand)))]
                      [else (display "Something went wrong")]
                      )
                    
                    ; setting bank wasn't working inside conditional so I had to do it out here
                    ; checking if user busted and if user-hand is greater than dealer hand or if the dealer busted
                    (if (and (equal? #f user-bust) (or (> (eval-hand (unbox user-hand)) (eval-hand (unbox dealer-hand))) (equal? #t dealer-bust)))
                        ; if it is, double the bet and add it to bank
                        (set! bank (+ bank (* 2 bet)))
                        ; if not, don't add anything
                        (set! bank (+ bank 0)))
                    
                    (newline)
                    
                    (cond ; making sure user stil has money, if not, then the game is over
                      [(<= bank 0)
                       (display "You have no money left to bet! Game over :(")]
                      ; see if user wants to continue to play
                      ; checks this at the top of the loop as a break condition
                      ; if yes, just continue as normal
                      [else (display "Would you like to continue playing? (y/n) ")
                            (set! continue (read-line (current-input-port)))]
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

(main)
