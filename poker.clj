(ns euler)

;; In the card game poker, a hand consists of five cards and are ranked, from
;; lowest to highest, in the following way:
;;
;; - High Card :: Highest value card.
;; - One Pair :: Two cards of the same value.
;; - Two Pairs :: Two different pairs.
;; - Three of a Kind :: Three cards of the same value.
;; - Straight :: All cards are consecutive values.
;; - Flush :: All cards of the same suit.
;; - Full House :: Three of a kind and a pair.
;; - Four of a Kind :: Four cards of the same value.
;; - Straight Flush :: All cards are consecutive values of same suit.
;; - Royal Flush :: Ten, Jack, Queen, King, Ace, in same suit.
;;
;; The cards are valued in the order:
;; 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
;;
;; If two players have the same ranked hands then the rank made up of the
;; highest value wins; for example, a pair of eights beats a pair of fives (see
;; example 1 below). But if two ranks tie, for example, both players have a
;; pair of queens, then highest cards in each hand are compared (see example 4
;; below); if the highest cards tie then the next highest cards are compared,
;; and so on.
;;
;; Consider the following five hands dealt to two players:
;;
;; | Hand | Player 1                                           | Player 2                                            | Winner   |
;; |------+----------------------------------------------------+-----------------------------------------------------+----------|
;; |    1 | 5H 5C 6S 7S KD (Pair of Fives)                     | 2C 3S 8S 8D TD (Pair of Eights)                     | Player 2 |
;; |    2 | 5D 8C 9S JS AC (Highest card Ace)                  | 2C 5C 7D 8S QH (Highest card Queen)                 | Player 1 |
;; |    3 | 2D 9C AS AH AC (Three Aces)                        | 3D 6D 7D TD QD (Flush with Diamonds)                | Player 2 |
;; |    4 | 4D 6S 9H QH QC (Pair of Queens, Highest card Nine) | 3D 6D 7H QD QS (Pair of Queens, Highest card Seven) | Player 1 |
;; |    5 | 2H 2D 4C 4D 4S (Full House With Three Fours)       | 3C 3D 3S 9S 9D (Full House with Three Threes)       | Player 1 |
;;
;; The file poker.txt contains one-thousand random hands dealt to two
;; players. Each line of the file contains ten cards (separated by a single
;; space): the first five are Player 1's cards and the last five are Player 2's
;; cards. You can assume that all hands are valid (no invalid characters or
;; repeated cards), each player's hand is in no specific order, and in each
;; hand there is a clear winner.
;;
;; How many hands does Player 1 win?

;(def debug println)
(def debug list)

(defstruct card :value :suit)

(def *card-vals* (vec "..23456789TJQKA"))

(defn make-card
  "Given a single card string like \"8H\", returns a card structure."
  [s]
  (struct card (.indexOf *card-vals* (first s)) (second s)))

(defn make-hand
  "Pass this function a string like \"8H 2S AC JS JD\" and it returns a
sequence of card structures. Used only during testing/development. "
  [s]
  (map make-card (.split s " ")))

(defn hand-str
  "Used only during testing/development."
  [hand]
  (.trim (apply str (interleave (sort (map #(str (nth *card-vals* (:value %)) (:suit %)) hand)) (repeat " ")))))

;; Hand-rank stores information about a hand's value: its rank number from 0
;; (simple highest value card) through 9 (royal flush), its hand-val (the
;; value of the highest pair, three of a kind, highest in straight or flush,
;; full house, etc.), and the highest card in the hand.
(defstruct hand-rank :name :hand-rank :hand-val :highest-card)

(defn all-same-suit?
  [hand]
  (= 1 (count (set (map #(get % :suit) hand)))))

(defn straight-hand?
  [vals]
  (let [min-val (apply min vals)]
    (= (range min-val (+ min-val 5)) (sort vals))))

(defn hand-vals
  [hand]
  (map #(get % :value) hand))

(defn hand-freqs
  [hand]
  (frequencies (hand-vals hand)))

(defn hand-freq-vals
  [hand]
  (vals (hand-freqs hand)))

(defn highest-val
  "Return the highest value in the hand."
  [hand]
  (apply max (hand-vals hand)))

(defn poker-royal-flush?
  [hand]
  (when (and (all-same-suit? hand)
             (= #{10 11 12 13 14} (set (hand-vals hand))))
    (struct hand-rank "royal flush" 9 nil 14)))

(defn poker-straight-flush?
  [hand]
  (let [vals (hand-vals hand)]
    (when (and (all-same-suit? hand)
               (straight-hand? vals))
      (let [max-val (apply max vals)]
        (struct hand-rank "straight flush" 8 max-val max-val)))))

(defn poker-four-of-a-kind?
  [hand]
  (let [freqs (hand-freqs hand)
        found-four (first (filter #(= 4 (val %)) freqs))]
    (when found-four
      (struct hand-rank "four of a kind" 7 (key found-four) (highest-val hand)))))

(defn poker-full-house?
  [hand]
  (let [freqs (hand-freqs hand)
        freq-vals (map val freqs)
        found-three (first (filter #(= 3 (val %)) freqs))]
    (when (and (some #{3} freq-vals)
               (some #{2} freq-vals))
      (struct hand-rank "full house" 6 (key found-three) (highest-val hand)))))

(defn poker-flush?
  [hand]
  (when (all-same-suit? hand)
    (let [hv (highest-val hand)]
      (struct hand-rank "flush" 5 hv hv))))

(defn poker-straight?
  [hand]
  (when (straight-hand? (hand-vals hand))
    (let [hv (highest-val hand)]
      (struct hand-rank "straight" 4 hv hv))))

(defn poker-three-of-a-kind?
  [hand]
  (let [freqs (hand-freqs hand)
        grouped (group-by #(= 3 (val %)) freqs)] ; {true [three-freq] false [not-three-freqs]}
    (when (get grouped true)
      (struct hand-rank "three of a kind" 3
              (ffirst (get grouped true))
              (apply max (map first (get grouped false)))))))

(defn poker-two-pairs?
  [hand]
  (let [freqs (hand-freqs hand)
        grouped (group-by #(= 2 (val %)) freqs)] ; {true [pair-freqs] false [not-pair-freqs]}
    (when (= 2 (count (get grouped true)))
      (struct hand-rank "two pairs" 2
              (apply max (map first (get grouped true)))
              (apply max (map first (get grouped false)))))))

(defn poker-one-pair?
  [hand]
  (let [freqs (hand-freqs hand)
        grouped (group-by #(= 2 (val %)) freqs)] ; {true [pair-freq] false [not-pair-freqs]}
    (when (get grouped true)
      (struct hand-rank "one pair" 1 (ffirst (get grouped true))
              (apply max (map first (get grouped false)))))))

(defn rank-poker-hand
  [hand]
  (or (poker-royal-flush? hand)
      (poker-straight-flush? hand)
      (poker-four-of-a-kind? hand)
      (poker-full-house? hand)
      (poker-flush? hand)
      (poker-straight? hand)
      (poker-three-of-a-kind? hand)
      (poker-two-pairs? hand)
      (poker-one-pair? hand)
      (struct hand-rank "simple high card" 0 nil (highest-val hand))))

(defn player-1-wins-tie-breaker?
  "Breaks ties in the case of equal hand rank values. Looks first at hand
  value then if those are equal looks at highest card."
  [rank1 rank2]
  (debug "player-1-wins-tie-breaker?")
  (if (and (:hand-val rank1) (:hand-val rank2))
    (if (= (:hand-val rank1) (:hand-val rank2))
      (> (:highest-card rank1) (:highest-card rank2))
      (> (:hand-val rank1) (:hand-val rank2)))
    (> (:highest-card rank1) (:highest-card rank2))))

(defn player-1-wins?
  [hand1 hand2]
  (debug "player-1-wins?")
  (let [rank1 (rank-poker-hand hand1)
        rank2 (rank-poker-hand hand2)]
    (debug "hand1 =" (hand-str hand1) "rank1 =" rank1) ; DEBUG
    (debug "hand2 =" (hand-str hand2) "rank2 =" rank2) ; DEBUG
    (cond (> (:hand-rank rank1) (:hand-rank rank2)) true
          (< (:hand-rank rank1) (:hand-rank rank2)) false
          true (player-1-wins-tie-breaker? rank1 rank2))))

(defn p54
  []
  (reduce +
          (for [hand (.split (slurp "poker_p54.txt") "[\\r\\n]+")
                :let [cards (.split hand " ")
                      p1 (map make-card (take 5 cards))
                      p2 (map make-card (take 5 (drop 5 cards)))]]
            (if (player-1-wins? p1 p2)
              (do
                (debug "winner: player 1" (hand-str p1) "-" (hand-str p2))
                1)
              (do
                (debug "winner: player 2" (hand-str p1) "-" (hand-str p2))
                0)))))

;; ================ testing ================

(defstruct test-hand :hand1 :hand2 :winner)

(defn test-hands
  []
  (let [f #(struct test-hand (make-hand %1) (make-hand %2) %3)
        test-hands
        (list
         (f "5H 5C 6S 7S KD" "2C 3S 8S 8D TD" 2)
         (f "5D 8C 9S JS AC" "2C 5C 7D 8S QH" 1)
         (f "2D 9C AS AH AC" "3D 6D 7D TD QD" 2)
         (f "4D 6S 9H QH QC" "3D 6D 7H QD QS" 1)
         (f "2H 2D 4C 4D 4S" "3C 3D 3S 9S 9D" 1)
         (f "2H 3H 4H 5C 6H" "3S 5S 6S AS QH" 1))]
    (doseq [hand test-hands]
      (let [winner (if (player-1-wins? (:hand1 hand) (:hand2 hand)) 1 2)]
        (if (= winner (:winner hand)) (print ".")
            (print "E"))))
    (println)))
