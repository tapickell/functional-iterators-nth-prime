(ns nth-prime)

;; Working from this paper to implement this functional iterator solution
;; https://www.cambridge.org/core/services/aop-cambridge-core/content/view/FD3E90871269020CA6C64C25AB8A4FBD/S0956796808007004a.pdf/the-genuine-sieve-of-eratosthenes.pdf

(defn is-prime?
  "Naive, just check for small primes < 10"
  [n]
  (contains? #{2 3 5 7} n))

(defn prime-iterator [prime]
  (fn [index] [prime (+ index prime)]))

(defn update-iterators
  "If number is prime :: Adds new iterator to number squared index {sqr (prime-iterator number)}
   Then :: Moves all iterators at current index to next index for iterator"
  [iterators number prime?]
  (let [iters-found (map #(% number) (get iterators number)) ;; [[prime next-index]...]
        next-multiple (* number number)
        iter-tuples (if prime? (conj iters-found [number next-multiple]) iters-found)
        cleared-iterators (dissoc iterators number)]
    (loop [tuples iter-tuples iters cleared-iterators]
      (if (empty? tuples)
        iters
        (let [tuple (first tuples)]
          (recur (rest tuples) (update iters (second tuple) #(conj % (prime-iterator (first tuple))))))))))

(defn could-this-be-prime?
  "Check to see if this number is prime or could possibly be prime.
  Don't let them fool ya, this approximation is good enough."
  [number composite-iterators]
  (let [composite-found? (contains? composite-iterators number)
      prime? (is-prime? number)
      could-be-prime? (and (boolean (seq composite-iterators)) (not composite-found?))]
    (or prime? could-be-prime?)))

(defn test-from-state
  "Loop over all numbers-to-test to check for prime then test to see if it is the nth prime"
  [nth-position primes-found-in numbers-to-test-in composite-iterators-in]
  (loop [primes-found primes-found-in
         numbers-to-test numbers-to-test-in
         composite-iterators composite-iterators-in]
    (let [number (first numbers-to-test)
          assume-prime? (could-this-be-prime? number composite-iterators)
          match? (= nth-position (+ primes-found 1))]
      (if (and assume-prime? match?)
        number
        (let [up-its (update-iterators composite-iterators number assume-prime?)
              up-count (if assume-prime? (+ primes-found 1) primes-found)]
          (recur up-count (rest numbers-to-test) up-its))))))

(defn nth-prime
  " Returns the prime number in the nth position."
  [nth-position]
  (when (< nth-position 1)
    (throw (IllegalArgumentException. "there is not zeroth prime")))
  (test-from-state nth-position 0 (rest (range)) (sorted-map)))

(comment
  "
N :: prime? :: composite-found? composites on recur
1    false     false            {}
2    true->1   false            {4 [2]}
3    true->2   false            {4 [2] 9 [3]}
4    false     true             {6 [2] 9 [3]}
5    true->3   false            {6 [2] 9 [3] 25 [5]}
6    false     true             {8 [2] 9 [3]}
7    true->4   false            {8 [2] 9 [3] 25 [5] 49 [7]}
8    false     true             {9 [3] 10 [2] 25 [5] 49 [7]}
9    false     true             {10 [2] 12 [3] 25 [5] 49 [7]}
10   false     true             {12 [2 3] 25 [5] 49 [7]}
11   true->5   false            {12 [2 3] 25 [5] 49 [7] 121 [11]}
12   false     true             {14 [2] 15 [3] 25 [5] 49 [7] 121 [11]}
13   true->6   false            {14 [2] 15 [3] 25 [5] 49 [7] 121 [11] 169 [13]}
14   false     true             {15 [3] 16 [2] 25 [5] 49 [7] 121 [11] 169 [13]}
15   false     true             {16 [2] 18 [3] 25 [5] 49 [7] 121 [11] 169 [13]}
16   false     true             {18 [2 3] 25 [5] 49 [7] 121 [11] 169 [13]}
17   true->7   false            {18 [2 3] 25 [5] 49 [7] 121 [11] 169 [13] 289 [17]}
18   false     true             {20 [2] 21 [3] 25 [5] 49 [7] 121 [11] 169 [13] 289 [17]}
19   true->8   false            {20 [2] 21 [3] 25 [5] 49 [7] 121 [11] 169 [13] 289 [17] 361 [19]}
20   false     true             {21 [3] 22 [2] 25 [5] 49 [7] 121 [11] 169 [13] 289 [17] 361 [19]}
21   false     true             {22 [2] 24 [3] 25 [5] 49 [7] 121 [11] 169 [13] 289 [17] 361 [19]}
22   false     true             {24 [2 3] 25 [5] 49 [7] 121 [11] 169 [13] 289 [17] 361 [19]}
23   false     false   ->9
24   false     true
25   false     true
26   false     false   ->10
"
(let [iters {22 [(prime-iterator 2)]
             24 [(prime-iterator 3)]
             25 [(prime-iterator 5)]
             49 [(prime-iterator 7)]
             121 [(prime-iterator 11)]
             169 [(prime-iterator 13)]
             289 [(prime-iterator 17)]
             361 [(prime-iterator 19)]}]
  (test-from-state 10 8 (range 22 42) iters))

(update-iterators {3 [(prime-iterator 2)]} 3 true)
;; => {9 (#function[nth-prime/prime-iterator/fn--8732]), 5 (#function[nth-prime/prime-iterator/fn--8732])}
(update-iterators {3 [(prime-iterator 2)]} 3 false)
;; => {5 (#function[nth-prime/prime-iterator/fn--8732])}

(nth-prime 1) ;; => 2
(nth-prime 2) ;; => 3
(nth-prime 3) ;; => 5
(nth-prime 4) ;; => 7
(nth-prime 5) ;; => 11
(nth-prime 6) ;; => 13
(nth-prime 7) ;; => 17
(nth-prime 8) ;; => 19
(nth-prime 9) ;; => 23 This worked past the < 20 prime check
(nth-prime 10)  ;; this should be 29 so 6 more iterations the the 9th
"
  (update-iterators {3 [(prime-iterator 2)]} 3)
;; => {9 (#function[nth-prime/prime-iterator/fn--8979]), 5 (#function[nth-prime/prime-iterator/fn--8979])}

  (map #(% 4) (get {4 [(prime-iterator 2) (prime-iterator 3)]} 4)) ;; => ([2 6] [3 7])
  (assoc {4 [(prime-iterator 2) (prime-iterator 3)]} 4 [])


(nth-prime 1) ;; => 2
(nth-prime 2) ;; => 3
(nth-prime 3) ;; => 5
(nth-prime 4) ;; => 7
(nth-prime 5) ;; => 11
(nth-prime 6) ;; => 13
(nth-prime 7) ;; => 17
(nth-prime 8) ;; => 19
(nth-prime 9) ;; => 23 This worked past the < 20 prime check
(nth-prime 10)  ;; this should be 29 so 6 more iterations the the 9th
;; =>
(nth-prime 25)
(nth-prime 30)
(nth-prime 50)
(nth-prime 100)
(nth-prime 101)
(nth-prime 10001)

;; iterator should start at index prime^2
;; if n is prime get second iteration of n and place the third iterator there
;; 2 is prime
;; 2 * 2 = 4
{4 [(prime-iterator 2)]}
;; when we get to four we apply the function to the index 4
((prime-iterator 2) 4)
;; => {2 6}
;; we add the next iterator to the index returned
{6 [(prime-iterator 2)]}
(update {4 [(prime-iterator 2)]} 4 #(conj % (prime-iterator 4)))
;; => {4 [#function[nth-prime/prime-iterator/fn--9110] #function[nth-prime/prime-iterator/fn--9110]]}
(update {4 [(prime-iterator 2)]} 6 #(conj % (prime-iterator 6)))
;; => {4 [#function[nth-prime/prime-iterator/fn--9110]], 6 (#function[nth-prime/prime-iterator/fn--9110])}

  ")
