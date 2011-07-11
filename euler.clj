(ns euler
  (:use [clojure.contrib.lazy-seqs :only (primes fibs)]
        [clojure.set :only (difference)]
        [clojure.contrib.combinatorics :only (lex-permutations)]))

(defn- prime-test
  "Helper for prime?"
  [n i j h]
  (let [n (int n) i (int i) j (int j) h (int h)]
    (cond (== i j) (== i h)
          (zero? (unchecked-remainder n i)) (recur n i i h)
          true (recur n (inc 1) j h))))

(defn prime?
  "Returns true if n is prime."
  [n]
  (let [n (int n)
        i (Math/floor (Math/sqrt n))]
    (if (= n (* i i)) false
        (prime-test n 2 i i))))

(def prime? (memoize prime?))

(defn easy-prime?
  "Returns non-nil if n is a prime. Looks for n in \"primes\"."
  [n]
  (= (first (drop-while #(< % n) primes))))

(def easy-prime? (memoize easy-prime?))

;;; See also easy2-prime? below

(defn next-prime
  "Returns the lowest prime that is greater than n."
  [n]
  (loop [n (int n) n-plus-1 (inc n)]
    (if (prime? n-plus-1)
      n-plus-1
      (recur n-plus-1 (inc n-plus-1)))))

(def next-prime (memoize next-prime))

(defn primes-upto
  "A seq of all primes up to and including n."
  [n]
  (take-while #(>= n %) primes))

(defn prime-factors-of
  "A seq containing the prime factors of n. The prime factor of a prime number
  is itself."
  [n]
  (filter #(zero? (rem n %))            ; #(zero? (unchecked-remainder n %))
          (primes-upto n)))

;; ================ utils ================

(defn digit-to-int
  "Converts a Character that is a digit to an int."
  [c]
  (- (int c) (int \0)))

(defn alpha-to-int
  "Converts a Character that is an alpha ([a-zA-Z]) to an int."
  [c]
  (inc (- (int (Character/toLowerCase c)) (int \a))))

(defn factorial
  [n]
  (loop [cnt n acc 1]
    (if (zero? cnt)
      acc
      (recur (dec cnt) (* acc cnt)))))

(defn divisors
  [n]
  (let [max-divisor-check (int (Math/sqrt n))
        low-divisors (filter #(zero? (unchecked-remainder n %)) (take max-divisor-check (iterate inc 1)))]
  (set (concat low-divisors (map #(unchecked-divide n %) low-divisors)))))

;;; See also easy-prime? and prime? above
(defn easy2-prime?
  "Returns true if n is prime. Does this by finding divisors and counting them."
  [n]
  (= 2 (count (divisors n))))

(def easy2-prime? (memoize easy2-prime?))

(defn max-val-index
  "Return the index of the maximum value found after applying f to coll."
  [f coll]
  (loop [coll coll
         i 0
         max-index nil
         max-val nil]
    (if (nil? coll) max-index
        (let [val (f (first coll))]
          (if (or (nil? max-val) (> val max-val))
            (recur (next coll) (inc i) i val)
            (recur (next coll) (inc i) max-index max-val))))))

;; ================ problems ================

(defn mults-of-3or5-below-1000 []
  (filter
   #(or (zero? (unchecked-remainder % 5)) (zero? (unchecked-remainder % 3)))
   (take 999 (iterate inc 1))))

(defn p1
  "Add all the natural numbers below one thousand that are multiples of 3 or
5."
  []
  (reduce + (mults-of-3or5-below-1000)))

;; ================

(defn fib-upto-acc [n vec]
  (let [rev (reverse vec)
        next-val (+ (first rev) (fnext rev))]
    (if (> next-val n)
      vec
      (recur n (conj vec next-val)))))

(defn fib-upto [n]
  (cond (= n 0) 0
        (= n 1) 1
        true (fib-upto-acc n [0 1])))

(defn p2
  "Find the sum of all the even-valued terms in the Fibonacci sequence which
  do not exceed four million."
  []
  (reduce + (filter even? (fib-upto 3999999))))

;; ================

(defn p3
  "Find the largest prime factor of a composite number."
  []
  (last (prime-factors-of 317584931803)))

;; ================

(defn palindrome?
  "Return true if n is a palindrome number."
  [n]
  (let [s (str n)]
    (= (seq s) (reverse s))))
(defn p4
  "Find the largest palindrome made from the product of two 3-digit numbers."
  []
  (apply max (for [x (range 100 1000) y (range 100 1000)
                   :when (palindrome? (* x y))]
               (* x y))))

;; ================

(defn div-by-all
  [n]
  (not (filter #(zero? (unchecked-remainder n %) ) (range 1 21))))

(defn p5
  "What is the smallest number divisible by each of the numbers 1 to 20?
First, manually reduce list by removing factors of higher numbers. Next, find
product. That should be it, right?"
  []
  (reduce * [11 12 13 14 15 16 17 18 19 20]))

;; ================

(defn p6
  "Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of the sum."
  []
  (let [r (range 1 101)
        sum (reduce + r)
        squares (for [x r] (* x x))]
    (- (* sum sum) (reduce + squares))))

;; ================

(defn p7
  "What is the 10001st prime number?"
  []
  (take 1 (drop 10000 primes)))

;; ================

(defn chars-to-product
  "Takes a seq of digit characters and returns the product of the digits."
  [chars]
  (reduce * (map digit-to-int chars)))

(defn p8
  "Find the greatest product of five consecutive digits in the 1000-digit
number N."
  []
  (let [s "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"]
    (loop [chars (seq s)
           max-product 0]
      (let [five (take 5 chars)]
        (if (= 5 (count five))
          (recur (rest chars)
                 (max max-product (chars-to-product five)))
          max-product)))))

;; ================

;; Triangle number N is (n(n+1))/2, or (n^2 + n)/2. This is an infinite
;; sequence of triangle numbers.
(def triangle-numbers (map #(/ (* % (inc %)) 2)
                           (iterate inc 1)))

(defn num-divisors
  "We don't just call divisors and count the result because by doing that we
  do extra work: calculating all the factors that are above the square root
  of n. We don't care what those divisors are, we can take the number of
  factors <= square root and double that (possible decreasing by one if max
  value == square root)." [n]
  (let [max-divisor-check (int (Math/sqrt n))
        low-divisors (filter #(zero? (unchecked-remainder n %)) (take max-divisor-check (iterate inc 1)))
        naive-num-divisors (* 2 (count low-divisors))
        last-divisor (last low-divisors)]
    (if (= (* last-divisor last-divisor) n)
      (dec naive-num-divisors)
      naive-num-divisors)))

(defn p12
  "Find the value of the first triangle number (1+2+...+N) with over five
hundred divisors."
  []
  (first (filter #(> (num-divisors %) 500) triangle-numbers)))

;; ================

(defn p15
  "How many routes are there through a 20x20 grid? We can solve this by
using the combinitorial function, combining 40 things 20 at a time."
  []
  (let [f20 (factorial 20)]
    (/ (factorial 40) (* f20 f20))))

;; ================

(defn p16
  "What is the sum of the digits of the number 2^1000?"
  []
  (reduce + (map digit-to-int (str (BigDecimal. (Math/pow 2 1000))))))

;; ================

(def *digit-words* ["" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def *tens-words* ["" "ten" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])
(def *teens-words* ["ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])

(defn number-to-words
  "Assumes number is max 9,999."
  [n]
  (let [thousands (int (/ n 1000))
        hundreds (mod (int (/ n 100)) 10)
        tens (mod (int (/ n 10)) 10)
        ones (mod n 10)]
    (str
     (when (> thousands 0) (str (*digit-words* thousands) " thousand"))
     (when (> hundreds 0) (str
                           (when (> thousands 0) ", ")
                           (*digit-words* hundreds) " hundred"))
     (when (and (pos? (+ thousands hundreds)) (pos? (+ tens ones)))
       " and ")
     (cond (> tens 1) (str
                       (*tens-words* tens)
                       (when (pos? ones) "-")
                       (*digit-words* ones))
           (= tens 1) (*teens-words* ones)
           true       (*digit-words* ones)))))

(defn only-letters
  [s]
  (.replaceAll s "[^a-zA-Z]" ""))

(defn p17
  "How many letters are in the word versions of 1-1000? Exclude punctuation
  and whitespace but include \"and\", as in \"three hundred and
  forty-two\"."
  []
  (reduce + (map count
                 (map only-letters
                      (map number-to-words (take 1000 (iterate inc 1)))))))

;; ================

;; By starting at the top of the triangle below and moving to adjacent
;; numbers on the row below, the maximum total from top to bottom is 23.
;;
;;    3
;;   7 4
;;  2 4 6
;; 8 5 9 3
;;
;; That is, 3 + 7 + 4 + 9 = 23.
;;
;; Find the maximum total from top to bottom of the triangle defined below.
;;
;; NOTE: As there are only 16384 routes, it is possible to solve this
;; problem by trying every route. However, Problem 67, is the same challenge
;; with a triangle containing one-hundred rows; it cannot be solved by brute
;; force, and requires a clever method! ;o)

(def triangle [
[                             75]
[                           95 64]
[                         17 47 82]
[                       18 35 87 10]
[                     20  4 82 47 65]
[                   19  1 23 75  3 34]
[                 88  2 77 73  7 63 67]
[               99 65  4 28  6 16 70 92]
[             41 41 26 56 83 40 80 70 33]
[           41 48 72 33 47 32 37 16 94 29]
[         53 71 44 65 25 43 91 52 97 51 14]
[       70 11 33 28 77 73 17 78 39 68 17 57]
[     91 71 52 38 17 14 91 43 58 50 27 29 48]
[   63 66  4 68 89 53 67 30 73 16 69 87 40 31]
[  4 62 98 27 23  9 70 98 73 93 38 53 60  4 23]
               ])

(defstruct triangle-path :value :nodes)

(defn add-to-path
  [p node]
  (struct triangle-path
          (+ (:value p) node)
          (conj (:nodes p) node)))

;; Each entry in each row has at most two parents. Keep the path through the
;; parent that has the larger sum.

(defn max-paths-to
  [triangle row-idx prev-row-paths]
  (cond
   ; First row: create single-entry list of prev-row-paths
   (zero? row-idx)
   (recur triangle 1 (list (struct triangle-path (first (first triangle)) (list (first (first triangle))))))

   ; Last row: return list of prev-row-paths
   (= (count triangle) row-idx)
   prev-row-paths

   ; Middle rows: calculate new list of prev-row-paths
   true
   (let [row (nth triangle row-idx)
         prev-row (nth triangle (dec row-idx))
         prev-row-length (count prev-row)]
     (recur triangle
            (inc row-idx)
            (map #(let [entry (nth row %)
                        left-parent-row-path (when (pos? %) (nth prev-row-paths (dec %)))
                        right-parent-row-path (when (< % prev-row-length) (nth prev-row-paths %))]
                    (cond
                     (nil? left-parent-row-path)
                     (add-to-path right-parent-row-path entry)

                     (nil? right-parent-row-path)
                     (add-to-path left-parent-row-path entry)

                     (> (:value left-parent-row-path) (:value right-parent-row-path))
                     (add-to-path left-parent-row-path entry)

                     true
                     (add-to-path right-parent-row-path entry)))
                 (range 0 (count row)))))))

(defn p18
  "Find max path values of triangle."
  []
  (let [last-row-paths (max-paths-to triangle 0 nil)]
    (apply max (map :value last-row-paths))))

;; ================

(defn p19
  "How many Sundays fell on the first of the month during the twentieth
century (1 Jan 1901 to 31 Dec 2000)?"
  []
  (let [cal (java.util.Calendar/getInstance)
        month-first-days (for [year (range 1901 2001)
                               month (range java.util.Calendar/JANUARY (inc java.util.Calendar/DECEMBER))]
                           (do (.set cal year month 1)
                               (.get cal java.util.Calendar/DAY_OF_WEEK)))]
    (count (filter #(= java.util.Calendar/SUNDAY %)
                   month-first-days))))

;; ================

(defn p20
  "Find the sum of the digits in 100!"
  []
  (reduce + (map digit-to-int (str (factorial 100)))))

;; ================

;; Let d(n) be defined as the sum of proper divisors of n (numbers less than
;; n which divide evenly into n).
;;
;; If d(a) = b and d(b) = a, where a b, then a and b are an amicable pair
;; and each of a and b are called amicable numbers.
;;
;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22,
;; 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1,
;; 2, 4, 71 and 142; so d(284) = 220.
;;
;; Evaluate the sum of all the amicable numbers under 10000.

(defn proper-divisors
  "Proper divisors of n are those divisors less than n."
  [n]
  (filter #(not= % n) (divisors n)))

(defn sum-of-proper-divisors
  [n]
  (reduce + (proper-divisors n)))

(defn p21
  "Evaluate the sum of all amicable numbers under 10000."
  []
  (let [n-and-dn-list (for [n (range 10000)]
                        (list n (sum-of-proper-divisors n)))]
    ; n-and-dn-list is a list of numbers and their proper divisor sums
    ; (PDS). We now take pairs of those lists and find pairs whose PDSs
    ; equals the other number. These are amicable pairs. We return those two
    ; numbers in the list comprehension, then sum the unique set of those
    ; numbers using reduce.
    (println (take 20 n-and-dn-list)) ; DEBUG
    (reduce +
            (set
             (flatten
              (for [x n-and-dn-list
                    y n-and-dn-list
                    :when (and (not= (first x) (first y))
                               (= (first x) (second y))
                               (= (second x) (first y)))]
                (list (first x) (first y))))))))

;; ================

;; Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
;; containing over five-thousand first names, begin by sorting it into
;; alphabetical order. Then working out the alphabetical value for each name,
;; multiply this value by its alphabetical position in the list to obtain a
;; name score.
;;
;; For example, when the list is sorted into alphabetical order, COLIN, which
;; is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
;; would obtain a score of 938 * 53 = 49714.
;;
;; What is the total of all the name scores in the file?

(defn alpha-value
  [s]
  (reduce + (map alpha-to-int s)))

(defn p22
  []
  (let [txt (slurp "names_p22.txt")
        names (sort (.split (.substring txt 1 (dec (count txt))) "\",\""))
        alpha-vals (map alpha-value names)]
    (loop [sum 0
           idx 1
           alpha-vals alpha-vals]
      (if alpha-vals
        (recur (+ sum (* (first alpha-vals) idx))
               (inc idx)
               (next alpha-vals))
        sum))))

;; ================

;; A perfect number is a number for which the sum of its proper divisors is
;; exactly equal to the number. For example, the sum of the proper divisors of
;; 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect
;; number.

;; A number n is called deficient if the sum of its proper divisors is less
;; than n and it is called abundant if this sum exceeds n.

;; As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
;; number that can be written as the sum of two abundant numbers is 24. By
;; mathematical analysis, it can be shown that all integers greater than 28123
;; can be written as the sum of two abundant numbers. However, this upper limit
;; cannot be reduced any further by analysis even though it is known that the
;; greatest number that cannot be expressed as the sum of two abundant numbers
;; is less than this limit.

;; Find the sum of all the positive integers which cannot be written as the sum
;; of two abundant numbers.

(def *abundant-sum-floor* 28123)

(defn abundant?
  "Returns true if n is abundant."
  [n]
  (> (sum-of-proper-divisors n) n))

;; An infinite lazy sequence of abundant numbers.
(def abundant-numbers (filter abundant? (iterate inc 1)))

(defn numbers-upto-not-in
  "Returns all non-negative integers up to but not including N that are not
in IS, which must be a sorted sequence of integers." [n is]
  (loop [i 0
         is is
         acc []]
    (cond
     (>= i n) acc
     (empty? is)      (recur (inc i) is (conj acc i))
     (= i (first is)) (recur (inc i) (rest is) acc)
     (< i (first is)) (recur (inc i) is (conj acc i))
     true             (recur (inc i) (rest is) (conj acc i)))))

(defn p23
  "Find the sum of all the positive integers which cannot be written as the
sum of two abundant numbers.

We do this by finding all the ones that /can/ be written that way, up to the
max value of *abundant-sum-floor*, then returning all the integers that are
below *abundant-sum-floor* and are not in that list." []
  (let [abundant-up-to-max (for [x abundant-numbers :while (< x *abundant-sum-floor*)] x)
        abundant-sums (for [x abundant-up-to-max
                            y abundant-up-to-max
                            :when (< (+ x y) *abundant-sum-floor*)]
                        (+ x y))]
    (reduce +
            (numbers-upto-not-in *abundant-sum-floor* (sort (set abundant-sums))))))

;; ================

;; A permutation is an ordered arrangement of objects. For example, 3124 is one
;; possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
;; are listed numerically or alphabetically, we call it lexicographic order.
;; The lexicographic permutations of 0, 1 and 2 are:
;;
;; 012   021   102   120   201   210
;;
;; What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
;; 5, 6, 7, 8 and 9?

(defn p24
  "Return the millionth lexicographic permutation of the digits 0-9."
  []
  (take 1 (drop 999999 (lex-permutations (range 0 10)))))

;; ================

(defn p25
  "Return the first term (index) in the Fibonacci sequence to contain 1000
digits."
  []
  (count (take-while #(< (count (str %)) 1000) (fibs))))

;; ================

(defn recurring-digits
  "Return a sequence containing the recurring digits in the ratio 1/n. May
return an empty sequence. We do this by performing long division, looking for a previously seen divisor/remainder pair that indicates a cycle."
  [n]
  (loop [gozinta 10                     ; how many times n goes into this number
         digits-and-remainders []]
    (let [digit (int (/ gozinta n))
          remainder (- gozinta (* digit n))
          pair (list digit remainder)]
      (cond
       (zero? remainder)
       []

       (.contains digits-and-remainders pair)
       (map first (drop (.indexOf digits-and-remainders pair) digits-and-remainders))

       true
       (recur (* remainder 10) (conj digits-and-remainders pair))))))

(defn p26
  "Find the value of d < 1000 for which 1/d contains the longest recurring
cycle in its decimal fraction part."
  []
  (let [recurring-cycles (map recurring-digits (range 1 1000))]
    (inc (max-val-index count recurring-cycles))))

;; ================

;; Euler published the remarkable quadratic formula:

;; n^2 + n + 41

;; It turns out that the formula will produce 40 primes for the consecutive
;; values n = 0 to 39. However, when n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41 is
;; divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly
;; divisible by 41.

;; Using computers, the incredible formula n^2 79n + 1601 was discovered,
;; which produces 80 primes for the consecutive values n = 0 to 79. The
;; product of the coefficients, -79 and 1601, is -126479.

;; Considering quadratics of the form:

;; n^2 + an + b, where |a|  1000 and |b|  1000

;; where |n| is the modulus/absolute value of n
;; e.g. |11| = 11 and |-4| = 4

;; Find the product of the coefficients, a and b, for the quadratic expression
;; that produces the maximum number of primes for consecutive values of n,
;; starting with n = 0.

(defn num-primes-generated
  [[a b]]
  (count (take-while easy2-prime? (map #(+ (* % %) (* a %) b) (iterate inc 0)))))

(defn p27
  "Find the product of the coefficients, a and b, for the quadratic
expression that produces the maximum number of primes for consecutive values
of n, starting with n = 0."
  []
  (let [ab-seq (for [a (range -999 1000) b (range -999 1000)] (list a b))]
    (loop [ab-seq ab-seq
           max-num-primes 0
           max-primes-coeffs ()]
      (if (nil? ab-seq)
        (apply * max-primes-coeffs)
        (let [npg (num-primes-generated (first ab-seq))]
          (if (> npg max-num-primes)
            (recur (next ab-seq) npg (first ab-seq))
            (recur (next ab-seq) max-num-primes max-primes-coeffs)))))))

;; ================

;; Starting with the number 1 and moving to the right in a clockwise
;; direction a 5 by 5 spiral is formed as follows:
;;
;; 21 22 23 24 25
;; 20  7  8  9 10
;; 19  6  1  2 11
;; 18  5  4  3 12
;; 17 16 15 14 13
;;
;; It can be verified that the sum of the numbers on the diagonals is 101.
;;
;; What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
;; formed in the same way?
