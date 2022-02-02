#lang racket

(define (main)
  (display "-------------------------\n=== consult the FATES ===\n-------------------------\n\nask your question:\n")
  (oracle))

(define (oracle)
  (display "> ")
  (let [[input (read-line)]]
    (if (equal? input "")
        (display "be not afraid to speak; make your query!\n")
        (let [[results (respond input)]]
          (display (apply format (cons "CLOTHO:   \"~a\"\nLACHESIS: \"~a\"\nATROPOS:  \"~a\"\n" results))))))
  (oracle))

(define (get-chunks str)
  (define input (string-split str))
  (define chunks
    (foldl
     (λ (word acc)
       (cond
         [(connective? word)
          (if (empty? (car acc))
              acc
              (cons '() acc))]
         [(char-punctuation? (last (string->list word)))
          (cons '() (cons (append (car acc) (list word)) (cdr acc)))]
         [else (cons (append (car acc) (list word)) (cdr acc))]))
     '(())
     input))
  (if (empty? (first chunks))
      (rest chunks)
      chunks))

(define (connective? word)
  (member word
          '("a" "an" "the"
            "in" "if" "to"
            "and" "or" "of"
            "about" "before"
            "with" "after"
            "is" "are" "it"
            "do" "so" "then"
            "when" "who" "how" "what" "why" "where")))

(define (process-phrase phrase)
  (pronounise (append (drop-right phrase 1) (list (drop-end-punctuation (last phrase))))))

(define (pronounise phrase)
  (map (λ (word)
         (case (string-downcase word)
           [("i") "you"]
           [("we") "you"]
           [("me") "you"]
           [("am") "are"]
           [("my") "your"]
           [("myself") "yourself"]
           [("you") "we"]
           [("your") "our"]
           [("yourself") "ourselves"]
           [else word]))
       phrase))

(define (drop-end-punctuation str)
  (if (char-alphabetic? (last (string->list str)))
      str
      (substring str 0 (- (string-length str) 1))))

(define (respond input)
  (define chunks (get-chunks input))
  (define possible-clotho-models (append* (take clotho-responses (min 3 (length chunks)))))
  (define possible-lachesis-models (append* (take lachesis-responses (min 3 (length chunks)))))
  (define possible-atropos-models (append* (take atropos-responses (min 3 (length chunks)))))
  (list
   (construct-response (list-ref possible-clotho-models (random (length possible-clotho-models))) (shuffle chunks))
   (construct-response (list-ref possible-lachesis-models (random (length possible-lachesis-models))) (shuffle chunks))
   (construct-response (list-ref possible-atropos-models (random (length possible-atropos-models))) (shuffle chunks))))

(define (construct-response model chunks)
  (foldl (λ (phrase-no model)
           (regexp-replace* (format "\\[~a\\]" phrase-no)
                            model
                            (string-join (process-phrase (list-ref chunks phrase-no)))))
         model
         (build-list (length chunks) (λ (x) x))))

(define clotho-responses
  '[["[0] and only [0]"
     "be not afraid of [0]"
     "i have seen [0]"
     "[0] is not your concern"]
    ["[0] leads to [1]"
     "see to [0], leave [1] to the gods"
     "care more for [0] than for [1]"]
    ["once [0], twice [1], three times [2]"
     "i know of [0], only my sisters can tell you of [1] and [2]"
     "[0] for the autumn, [1] for the winter, [2] for the spring"]
    [] [] [] [] [] []])

(define lachesis-responses
  '[["are you so certain of [0]?"
     "my sisters lie to you regarding [0]"
     "let [0] be as a guiding star"]
    ["without [0], how [1]?"
     "[0] or [1], it's all the same"
     "you will see that [0] is like to [1]"]
    ["[0] and [1] together bring [2]"
     "let [0] decide between [1] and [2]"
     "[0] for knowledge, [1] for myth, [2] for a secret"]
    [] [] [] [] [] []])

(define atropos-responses
  '[["alas, [0]"
     "cunning lachesis knows not [0]"
     "foolish clotho cannot see [0]"]
    ["[0] precludes [1]"
     "[0] brings only [1]"
     "[0] cannot bear [1]"]
    ["if [0] then [1], but otherwise [2]"
     "an end to [0], and end to [1], and end to [2]"
     "[0] for birth, [1] for life, [2] for the final touch of death"]
    [] [] [] [] [] []])

(main)