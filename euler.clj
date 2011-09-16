(ns euler
  (:use [clojure.contrib.lazy-seqs :only (primes fibs)]
        [clojure.set :only (difference)]
        [clojure.string :only (split-lines)]
        [clojure.contrib.combinatorics :only (lex-permutations combinations permutations)]
        [clojure.contrib.math :only (expt abs exact-integer-sqrt gcd)]
        [clojure.contrib.greatest-least :only (greatest-by)]))

(defn- prime-test
  "Helper for prime?"
  [n i j h]
  (let [n (int n) i (int i) j (int j) h (int h)]
    (cond (== i j) (== i h)
          (zero? (unchecked-remainder n i)) (recur n i i h)
          :else (recur n (inc 1) j h))))

(defn prime?
  "Returns true if n is prime. See also lookup-prime? and divs-prime?."
  [n]
  (cond (== n 2) true
        (even? n) false
        :else (let [n (int n)
                   i (Math/floor (Math/sqrt n))]
               (cond (== n (* i i)) false
                     :else (prime-test n 2 i i)))))

(def m-prime? (memoize prime?))

(defn lookup-prime?
  "Returns non-nil if n is a prime. Looks for n in \"primes\"."
  [n]
  (cond (== n 2) true
        (even? n) false
        :else (== n (first (drop-while #(< % n) primes)))))

(def m-lookup-prime? (memoize lookup-prime?))

(defn divs-prime?
  "Returns true if n is prime. Does this by seeing if there is any divisor
other than 1 in the numbers up to (sqrt n)."
  [n]
  (cond (< n 2) false
        (== n 2) true
        (even? n) false
        :else (let [max-divisor-check (/ (- (int (Math/sqrt n)) 2) 2)] ; subtract 2 because we start iterating at 3
                (not-any? #(zero? (unchecked-remainder n %)) (take max-divisor-check (iterate #(+ % 2) 3))))))

(def m-divs-prime? (memoize divs-prime?))

(defn next-prime
  "Returns the lowest prime that is greater than n."
  [n]
  (loop [n (int n) n-plus-1 (inc n)]
    (if (prime? n-plus-1)
      n-plus-1
      (recur n-plus-1 (inc n-plus-1)))))

(def m-next-prime (memoize next-prime))

(defn primes-upto
  "A seq of all primes up to and including n."
  [n]
  (take-while #(>= n %) primes))

(defn prime-factors-of
  "A seq containing the prime factors of n. The prime factor of a prime number
  is itself."
  ([n] (prime-factors-of n false))
  ([n unchecked]
     (let [f (if unchecked unchecked-remainder rem)]
       (filter #(zero? (f n %))
               (primes-upto n)))))

;; ================ utils ================

(defn evenly-divisible-by?
  "Return true if n is evenly divisible by d."
  [n d]
  (zero? (unchecked-remainder n d)))

(defn digit-to-int
  "Converts a Character that is a digit to an int."
  [c]
  (- (int c) (int \0)))

(defn alpha-to-int
  "Converts a Character that is an alpha ([a-zA-Z]) to an int where a=1,
b=2..."
  [c]
  (inc (- (int (Character/toLowerCase c)) (int \a))))

(defmulti sum-of-digits class)
(defmethod sum-of-digits String [s] (reduce + (map digit-to-int s)))
(defmethod sum-of-digits Number [n] (sum-of-digits (str n)))

;; Provides quick lookup of factorials up to 100!.
(def factorials-to-100
  (loop [fs [1 1]
         i 2]
    (if (< i 101) (recur (conj fs (* i (last fs))) (inc i))
      fs)))

;; Would (reduce * (range 2 (inc n))) be faster than using recur? I should
;; time them and find out.
(defn factorial
  [n]
  (if (<= n 100)
    (nth factorials-to-100 n)
    (loop [cnt n acc 1]
      (if (or (zero? cnt) (== 1 cnt)) acc
          (recur (dec cnt) (* acc cnt))))))

(defn divisors
  [n]
  (let [max-divisor-check (int (Math/sqrt n))
        low-divisors (filter #(zero? (unchecked-remainder n %)) (take max-divisor-check (iterate inc 1)))]
  (set (concat low-divisors (map #(unchecked-divide n %) low-divisors)))))

(defn max-val-key
  "Returns from map m the key whose value, a number, is greatest."
  [m]
  (loop [max-key 0, max-val 0, m m]
    (cond (nil? m) max-key
          (> (val (first m)) max-val) (recur (key (first m)) (val (first m)) (next m))
          :else (recur max-key max-val (next m)))))

(defn max-val-index
  "Return the index of the maximum value found after applying f to coll.

See also core/max-key, but this is faster for what I need."
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

(defn assoc-conj
  "Calls assoc, conj-ing val onto (get map key). If (get map key) is nil,
uses empty-val as a default value."
  [m key val empty-val]
  (assoc m key (conj (or (get m key) empty-val) val)))

(defn all-different?
  "Returns true if all of the items are unique."
  [& items]
  (== (count (vec items)) (count (set items))))

(defn numeric-permutation?
  "Returns true if i and j are numbers whose digits are permutations of each other."
  [i j]
  (= (sort (str i)) (sort (str j))))

(defn coprime? [m n] (== 1 (gcd m n)))

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
  (cond (zero? n) 0
        (== n 1) 1
        :else (fib-upto-acc n [0 1])))

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
        (if (== 5 (count five))
          (recur (rest chars)
                 (max max-product (chars-to-product five)))
          max-product)))))

;; ================

;; Triangle number N is (n(n+1))/2, or (n^2 + n)/2. This is an infinite
;; sequence of triangle numbers.
(def triangle-numbers (map #(/ (* % (inc %)) 2)
                           (iterate inc 1)))

(defn nth-triangle
  [n]
  (/ (* n (inc n)) 2))

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
    (if (== (* last-divisor last-divisor) n)
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
  (sum-of-digits (expt 2 1000)))

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
           (== tens 1) (*teens-words* ones)
           :else       (*digit-words* ones)))))

(defn only-letters
  [s]
  (.replaceAll s "[^a-zA-Z]" ""))

(defn p17
  "How many letters are in the word versions of 1-1000? Exclude punctuation
  and whitespace but include \"and\", as in \"three hundred and
  forty-two\"."
  []
  (reduce + (map (comp count only-letters number-to-words) (take 1000 (iterate inc 1)))))

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

(def p18-triangle [
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
   (== (count triangle) row-idx)
   prev-row-paths

   ; Middle rows: calculate new list of prev-row-paths
   :else
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

                     :else
                     (add-to-path right-parent-row-path entry)))
                 (range 0 (count row)))))))

(defn p18
  "Find max path values of triangle."
  []
  (let [last-row-paths (max-paths-to p18-triangle 0 nil)]
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
  (sum-of-digits (factorial 100)))

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
    (reduce +
            (set
             (flatten
              (for [x n-and-dn-list
                    y n-and-dn-list
                    :when (and (not= (first x) (first y))
                               (== (first x) (second y))
                               (== (second x) (first y)))]
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
  (let [txt (slurp "data/names_p22.txt")
        names (sort (.split (subs txt 1 (dec (count txt))) "\",\""))
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
     (empty? is)       (recur (inc i) is (conj acc i))
     (== i (first is)) (recur (inc i) (rest is) acc)
     (< i (first is))  (recur (inc i) is (conj acc i))
     :else             (recur (inc i) (rest is) (conj acc i)))))

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
return an empty sequence. We do this by performing long division, looking
for a previously seen divisor/remainder pair that indicates a cycle."
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

       :else
       (recur (* remainder 10) (conj digits-and-remainders pair))))))

(defn p26
  "Find the value of d < 1000 for which 1/d contains the longest recurring
cycle in its decimal fraction part."
  []
  ;; Could use core/max-key but using max-val-index is faster.
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
  (count (take-while divs-prime? (map #(+ (* % %) (* a %) b) (iterate inc 0)))))

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
;; 73 74 75 76 77 78 79 80 81
;; 72 43 44 45 46 47 48 49 50
;; 71 42 21 22 23 24 25 26 51
;; 70 41 20  7  8  9 10 27 52
;; 69 40 19  6  1  2 11 28 53
;; 68 39 18  5  4  3 12 29 54
;; 67 38 17 16 15 14 13 30 55
;; 66 37 36 35 34 33 32 31 56
;; 65 64 63 62 61 60 59 58 57
;;
;; It can be verified that the sum of the numbers on the diagonals is 101.
;;
;; What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
;; formed in the same way?

;; From the middle, each diagonal goes outward in a series
;; - going northeast the numbers are (2+n)^2
;; - going northwest each number +6, +14, +22, +30 (3*2, 7*2, 11*2)
;; - going southwest each number +4, +12, +20, +28 (2x2x1, 2x2x3, 2x2x5, 2x2x7)
;; - going southeast each number +2, +10, +18, +28 ????

(defn northeast-diagonal
  "Returns a lazy sequence of all northeast diagonal numbers, excluding the
center."
  []
  (map #(* (inc %) (inc %)) (iterate #(+ 2 %) 2)))

(defn northwest-diagonal
  "Returns a lazy sequence of all northwest diagonal numbers, excluding the
center."
  []
  (map #(- (* (inc %) (inc %)) %) (iterate #(+ 2 %) 2)))

(defn southeast-diagonal
  "Returns a lazy sequence of all southeast diagonal numbers, excluding the
center."
  []
  (map #(- (* (inc %) (inc %)) %) (iterate #(+ 2 %) 1)))
443839
(defn southwest-diagonal
  "Returns a lazy sequence of all southwest diagonal numbers, excluding the
center."
  []
  (map #(- (* (inc %) (inc %)) (* 2 %)) (iterate #(+ 2 %) 2)))

(defn p28
  []
  (let [max-val (* 1001 1001)]
  (reduce +
   (concat '(1)                         ; the yummy, chewy center
           (take-while #(<= % max-val) (northeast-diagonal))
           (take-while #(<= % max-val) (northwest-diagonal))
           (take-while #(<= % max-val) (southeast-diagonal))
           (take-while #(<= % max-val) (southwest-diagonal))))))

;; ================

;; Consider all integer combinations of a^b for 2 <= a <= 5 and 2 <= b <= 5:
;;
;; 2^2=4, 2^3=8, 2^4=16, 2^5=32
;; 3^2=9, 3^3=27, 3^4=81, 3^5=243
;; 4^2=16, 4^3=64, 4^4=256, 4^5=1024
;; 5^2=25, 5^3=125, 5^4=625, 5^5=3125
;;
;; If they are then placed in numerical order, with any repeats removed, we
;; get the following sequence of 15 distinct terms:
;;
;; 4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
;;
;; How many distinct terms are in the sequence generated by a^b for 2 <= a
;; <= 100 and 2 <= b <= 100?

(defn p29
  []
  (let [nums (for [a (range 2 101)
                   b (range 2 101)]
                   (Math/pow a b))]
    (count (set nums))))

;; ================

;; Surprisingly there are only three numbers that can be written as the sum of
;; fourth powers of their digits:
;;
;; 1634 = 1^4 + 6^4 + 3^4 + 4^4
;; 8208 = 8^4 + 2^4 + 0^4 + 8^4
;; 9474 = 9^4 + 4^4 + 7^4 + 4^4
;;
;; As 1 = 1^4 is not a sum it is not included.
;;
;; The sum of these numbers is 1634 + 8208 + 9474 = 19316.
;;
;; Find the sum of all the numbers that can be written as the sum of fifth
;; powers of their digits.

(defn p30-max-num
  "Returns an upper bound on the integers that can be expressed as the sum
of their digits to the power p."
  [p]
  (let [max-digit-val (int (Math/pow 9 p))]
    (int (Math/pow 10 (first (drop-while #(< (int (Math/pow  10 %)) (* % max-digit-val)) (iterate inc 1)))))))

;; Hard-coded 5th power
(def p30-digit-powers (vec (map #(int (Math/pow % 5)) (range 0 10))))

;; Uses p30-digit-powers, which uses hard-coded power
(defn sum-of-pow-of-digits
  "The sum of the fifth powers of the digits of n."
  [n]
  (reduce + (map #(nth p30-digit-powers (digit-to-int %)) (str n))))

(defn p30
  "Return sum of all numbers > 1 that can be written as sum of fifth power
of their digits."
  []
  (reduce +
          (filter #(== % (sum-of-pow-of-digits %))
                  (range 2 (p30-max-num 5)))))

;; ================

;; In England the currency is made up of pound, £, and pence, p, and there are
;; eight coins in general circulation:
;;
;; 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
;;
;; It is possible to make £2 in the following way:
;;
;; 1x£1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p
;;
;; How many different ways can £2 be made using any number of coins?

(def gbp-coins [200 100 50 20 10 5 2 1])

(defn p31
  "A naive way to count coin combinations that total 200p using list
comprehension."
  []
  (let [f #(<= % 200)]
    (inc                  ; shortcut: ignore p200 and add one to final total
     (count (for [p100                                         (map #(* % 100) (range 3))
                  p50 (filter #(f (+ p100 %))                  (map #(* % 50)  (range 5)))
                  p20 (filter #(f (+ p100 p50 %))              (map #(* % 20)  (range 11)))
                  p10 (filter #(f (+ p100 p50 p20 %))          (map #(* % 10)  (range 21)))
                  p5 (filter #(f (+ p100 p50 p20 p10 %))       (map #(* % 5)   (range 41)))
                  p2 (filter #(f (+ p100 p50 p20 p10 p5 %))    (map #(* % 2)   (range 101)))
                  p1 (filter #(f (+ p100 p50 p20 p10 p5 p2 %)) (map #(* % 1)   (range 201)))
                  :when (== 200 (+ p100 p50 p20 p10 p5 p2 p1))]
              1)))))

;; ================

;; We shall say that an n-digit number is pandigital if it makes use of all
;; the digits 1 to n exactly once; for example, the 5-digit number, 15234,
;; is 1 through 5 pandigital.
;;
;; The product 7254 is unusual, as the identity, 39 * 186 = 7254, containing
;; multiplicand, multiplier, and product is 1 through 9 pandigital.
;;
;; Find the sum of all products whose multiplicand/multiplier/product
;; identity can be written as a 1 through 9 pandigital.
;;
;; HINT: Some products can be obtained in more than one way so be sure to
;; only include it once in your sum.

;; Returns true if the string s consists of one of each of the digits 1-9.
(defmulti pandigital? class)

(defmethod pandigital? String [s]
  (= (sort s) '(\1 \2 \3 \4 \5 \6 \7 \8 \9)))

(defmethod pandigital? Number [n]
  (pandigital? (str n)))

(defmethod pandigital? clojure.lang.Sequential [coll]
  (pandigital? (apply str coll)))

;; Returns true if the string s consists of one of each of the digits 1-9.
(defmulti pandigital-0? class)

(defmethod pandigital-0? String [s]
  (= (sort s) '(\0 \1 \2 \3 \4 \5 \6 \7 \8 \9)))

(defmethod pandigital-0? Number [n]
  (pandigital? (str n)))

(defmethod pandigital-0? clojure.lang.Sequential [coll]
  (pandigital? (apply str coll)))

(defn p32
  []
  (reduce + (set (for [x (range 1 10000)
                       y (range 1 (int (/ 10000 x)))
                       :when (pandigital? (str x y (* x y)))]
                   (* x y)))))

;; ================

;; The fraction 49/98 is a curious fraction, as an inexperienced mathematician
;; in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which
;; is correct, is obtained by cancelling the 9s.

;; We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

;; There are exactly four non-trivial examples of this type of fraction, less
;; than one in value, and containing two digits in the numerator and
;; denominator.

;; If the product of these four fractions is given in its lowest common terms,
;; find the value of the denominator.

(defn curious-fraction
  [numer denom]
  (let [dn (seq (str numer))
        dd (seq (str denom))]
    (cond (and (= (last dn) \0)
               (= (last dd) \0))
          false

          (= (first dn) (last dd))
          (and (pos? (digit-to-int (first dd)))
               (= (/ numer denom) (/ (digit-to-int (last dn)) (digit-to-int (first dd)))))

          (= (last dn) (first dd))
          (and (pos? (digit-to-int (last dd)))
               (= (/ numer denom) (/ (digit-to-int (first dn)) (digit-to-int (last dd)))))

          :else
          false)))

(defn p33
  []
  (denominator (reduce *
                       (for [numer (range 10 100)
                             denom (range (inc numer) 100)
                             :when (curious-fraction numer denom)]
                         (/ numer denom)))))

;; ================

;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
;;
;; Find the sum of all numbers which are equal to the sum of the factorial of
;; their digits.
;;
;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.

(def digit-factorials (map factorial (range 0 10)))

(defn sum-of-factorial-of-digits?
  "Is n the sum of the factorial of its digits?"
  [n]
  (== n (reduce + (map #(nth digit-factorials (digit-to-int %)) (seq (str n))))))

;; This is correct, but I wish I knew why 10000000 is an upper bound.
(defn p34
  "Find the sum of all numbers which are equal to the sum of the factorial
of their digits."
  []
  (reduce + (filter sum-of-factorial-of-digits? (range 3 10000000))))

;; ================

;; 197 is a circular prime because all rotations of the digits: 197, 971,
;; 719, are themselves prime.
;;
;; There are 13 such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
;; 73, 79, and 97.
;;
;; How many circular primes are there below one million?

(defn circular-rotations
  "Return all circular rotations of n's digits."
  [n]
  (let [s (str n)
        len (count s)]
    (map #(Integer/parseInt (apply str (take len (drop % (cycle s)))))
         (range 0 len))))

(defn circular-prime?
  "Is p a circular prime?"
  [p]
  (== (count (str p))
      (count (take-while divs-prime? (circular-rotations p))))) ; start at one because we know 0'th entry is prime

(defn p35
  []
  (count (filter circular-prime? (primes-upto 1000000))))

;; ================

;; The decimal number, 585 = 1001001001 base 2 (binary), is palindromic in both
;; bases.
;;
;; Find the sum of all numbers, less than one million, which are palindromic in
;; base 10 and base 2.
;;
;; (Please note that the palindromic number, in either base, may not include
;; leading zeros.)

(defn as-binary-bits
  [n]
  (loop [n n
         bits ()]
    (if (zero? n)
      bits
      (recur (bit-shift-right n 1) (conj bits (bit-and n 1))))))

(defn palindromic-in-two-bases
  [n]
  (and (palindrome? n)
       (let [bits (as-binary-bits n)]
         (= bits (reverse bits)))))

(defn p36
  []
  "Sum of all numbers less than 1 million which are palindromic in both base 10 and base 2."
  (reduce + (filter palindromic-in-two-bases (range 0 1000000))))

;; ================

;; The number 3797 has an interesting property. Being prime itself, it is
;; possible to continuously remove digits from left to right, and remain prime
;; at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
;; left: 3797, 379, 37, and 3.
;;
;; Find the sum of the only eleven primes that are both truncatable from left
;; to right and right to left.
;;
;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

(defn truncatables
  "Given a number, return a sequence containing all of the truncatable forms
including that number."
  [n]
  (let [s (seq (str n))
        rs (reverse s)
        f (fn [s] (Long/parseLong (apply str s)))]
    (concat
     (map f (take-while identity (iterate next s)))
     (map #(f (reverse %)) (rest (take-while identity (iterate next rs)))))))

(defn truncatable-prime?
  "Return non-nil if all truncatables of n are prime."
  [n]
  (empty? (drop-while lookup-prime? (truncatables n))))
       
(defn p37
  []
  (reduce + (take 11 (filter truncatable-prime? (drop 4 primes)))))

;; ================

;; Take the number 192 and multiply it by each of 1, 2, and 3:
;;
;; 192 * 1 = 192
;; 192 * 2 = 384
;; 192 * 3 = 576
;;
;; By concatenating each product we get the 1 to 9 pandigital, 192384576. We
;; will call 192384576 the concatenated product of 192 and (1,2,3)
;;
;; The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
;; and 5, giving the pandigital, 918273645, which is the concatenated product
;; of 9 and (1,2,3,4,5).
;;
;; What is the largest 1 to 9 pandigital 9-digit number that can be formed as
;; the concatenated product of an integer with (1,2, ... , n) where n > 1?

(defn concat-prods
  [i n]
  (map #(* i %) (range 1 (inc n))))

(defn p38
  []
  (apply max
        (for [n (range 2 10)
              ; max num digits determined by num of products we are using
              i (range 1 (Math/pow 10 (inc (int (/ 10 n)))))
              :let [cp (concat-prods i n)]
              :when (pandigital? cp)]
          (Integer/parseInt (apply str cp)))))

;; ================

;; If p is the perimeter of a right angle triangle with integral length sides,
;; {a,b,c}, there are exactly three solutions for p = 120.
;;
;; {20,48,52}, {24,45,51}, {30,40,50}
;;
;; For which value of p <= 1000, is the number of solutions maximised?

(defn p39
  []
  (let [perims (for [p (range 3 1001)
                     a (range 1 p)
                     b (range a p)
                     :let [c (- p (+ a b))]
                     :when (== (* c c) (+ (* a a) (* b b)))]
                 p)
        freqs (frequencies perims)]
    (max-val-key freqs)))

;; ================

;; An irrational decimal fraction is created by concatenating the positive integers:
;;
;;   0.123456789101112131415161718192021...
;;                ^ (12th digit)
;;
;; It can be seen that the 12th digit of the fractional part is 1.
;;
;; If d(sub n) represents the nth digit of the fractional part, find the
;; value of the following expression:
;;
;; d(sub 1) x d(sub 10) x d(sub 100) x d(sub 1,000) x d(sub 10,000) x
;;   d(sub 100,000) x d(sub 1,000,000)

;; Let's brute-force this one
(defn p40
  []
  (let [v (vec (apply str (range 0 1000000)))
        len (count v)]
    (reduce * (map #(digit-to-int (nth v %)) [1 10 100 1000 10000 100000 1000000]))))

;; ================

;; What is the largest n-digit pandigital prime that exists?

;; This code assumes there is a 9-digit pandigital prime. If not, this will
;; return nil.

;; NOTE: This ran way too slowly with all 9 digits 1-9, so I backed off to 7
;; (range 1 8), got the answer REALLY quickly, checked the answer, and it
;; was right.
;;
;; After solving this accidentally, I read on the forums that 9- and 8-digit
;; answers are impossible because they would both be divisible by 3 (the
;; sums of the digits is divisible by 3). I could have saved myself a lot of
;; time.
(defn p41
  []
  (let [digits (reverse (range 1 8))
        pandigitals (for [d7 digits
                          :let [i7 (* d7 1000000)
                                d6-digits (remove #(== d7 %) digits)]
                          d6 d6-digits
                          :let [i6 (* d6 100000)
                                d5-digits (remove #(== d6 %) d6-digits)]
                          d5 d5-digits
                          :let [i5 (* d5 10000)
                                d4-digits (remove #(== d5 %) d5-digits)]
                          d4 d4-digits
                          :let [i4 (* d4 1000)
                                d3-digits (remove #(== d4 %) d4-digits)]
                          d3 d3-digits
                          :let [i3 (* d3 100)
                                d2-digits (remove #(== d3 %) d3-digits)]
                          d2 d2-digits
                          :let [i2 (* d2 10)
                                d1-digits (remove #(== d2 %) d2-digits)]
                          d1 d1-digits
                          :when (odd? d1)
                          :let [num (+ i7 i6 i5 i4 i3 i2 d1)]
                          :when (divs-prime? num)]
                      num)]
    (first pandigitals)))

;; ================

;; The nth term of the sequence of triangle numbers is given by, tn =
;; 1/2n(n+1); so the first ten triangle numbers are:
;;
;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;;
;; By converting each letter in a word to a number corresponding to its
;; alphabetical position and adding these values we form a word value. For
;; example, the word value for SKY is 19 + 11 + 25 = 55 = t(sub 10). If the
;; word value is a triangle number then we shall call the word a triangle word.
;;
;; Using words_p42.txt, a 16K text file containing nearly two-thousand common
;; English words, how many are triangle words?

(defn triangle?
  [n]
  (== n (first (drop-while #(< % n) triangle-numbers))))

(def m-triangle? (memoize triangle?))

(defn p42
  []
  (let [txt (slurp "data/words_p42.txt")
        words (sort (.split (subs txt 1 (dec (count txt))) "\",\""))
        alpha-vals (map alpha-value words)]
    (count (filter #(m-triangle? %) alpha-vals))))

;; ================

;; The number 1406357289 is a 0 to 9 pandigital number because it is made up of
;; each of the digits 0 to 9 in some order, but it also has a rather
;; interesting sub-string divisibility property.
;;
;; Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we
;; note the following (where d's are concatenated):
;;
;; d2 d3 d4  = 406 is divisible by 2
;; d3 d4 d5  = 063 is divisible by 3
;; d4 d5 d6  = 635 is divisible by 5
;; d5 d6 d7  = 357 is divisible by 7
;; d6 d7 d8  = 572 is divisible by 11
;; d7 d8 d9  = 728 is divisible by 13
;; d8 d9 d10 = 289 is divisible by 17
;;
;; Find the sum of all 0 to 9 pandigital numbers with this property.

;; divisors of all numbers to 999.
(def divisors-to-999 (vec (map divisors (range 0 1000))))

;; Can skip check for 2, because there's always a 3-digit # ending in an
;; even number. Can check for divisible by 5 by ensuring that either "05" or
;; "50" are not first two digits (because if they aren't then some
;; three-digit number ends with 5 or 0).

(defn p43
  []
    (let [digits (reverse (range 0 10)) ; 0 - 9 pandigital
          mults-of-17 (filter #(evenly-divisible-by? % 17) (range 1 1000))
          pandigitals (for [d2-d0 mults-of-17
                            :let [d2-d0-str (format "%03d" d2-d0)]
                            :when (or (< d2-d0 100)
                                      (== 3 (count (set d2-d0-str))))

                            :let [d3-digits (remove #(some (set d2-d0-str) (str %)) (range 0 10))]
                            d3 d3-digits
                            :when (evenly-divisible-by? (+ (* 100 d3) (int (/ d2-d0 10))) 13)

                            :let [d4-digits (remove #(= d3 %) d3-digits)]
                            d4 d4-digits
                            :when (evenly-divisible-by? (+ (* 100 d4) (* 10 d3) (int (/ d2-d0 100))) 11)

                            :let [d5-digits (remove #(= d4 %) d4-digits)]
                            d5 d5-digits
                            :when (evenly-divisible-by? (+ (* 100 d5) (* 10 d4) d3) 7)

                            :let [d6-digits (remove #(= d5 %) d5-digits)]
                            d6 d6-digits
                            :when (evenly-divisible-by? (+ (* 100 d6) (* 10 d5) d4) 5)

                            :let [d7-digits (remove #(= d6 %) d6-digits)]
                            d7 d7-digits
                            :when (evenly-divisible-by? (+ (* 100 d7) (* 10 d6) d5) 3)

                            :let [d8-digits (remove #(= d7 %) d7-digits)]
                            d8 d8-digits
                            :when (even? (+ (* 100 d8) (* 10 d7) d6))

                            :let [d9-digits (remove #(= d8 %) d8-digits)]
                            d9 d9-digits]
                        (+ (* d9 1000000000)
                           (* d8 100000000)
                           (* d7 10000000)
                           (* d6 1000000)
                           (* d5 100000)
                           (* d4 10000)
                           (* d3 1000)
                           d2-d0))]
      (reduce + pandigitals)))

;; ================

;; Pentagonal numbers are generated by the formula Pn = n(3n-1)/2. The first
;; ten pentagonal numbers are:
;;
;; 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
;;
;; It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their
;; difference, 70 - 22 = 48, is not pentagonal.
;;
;; Find the pair of pentagonal numbers, Pj and Pk, for which their sum and
;; difference is pentagonal and D = |Pk - Pj| is minimised; what is the value
;; of D?

;; Why 3000? Because somebody said so on the 'net. I'm ashamed of this
;; answer because I didn't figure out how to crawl the pairs of numbers in
;; the order that would produce increasing differences.

(def pentagonal-numbers (map #(/ (* % (dec (* 3 %))) 2) (iterate inc 1)))

(defn nth-pentagonal
  [n]
  (/ (* n (dec (* 3 n))) 2))

(def pentagonals (into [] (map nth-pentagonal (range 1 3001))))

(defn pentagonal?
  [n]
  (let [i (/ (inc (Math/sqrt (inc (* 24 n)))) 6)]
    (== i (int i))))

(def pentagonal? (memoize pentagonal?))

(defn p44
  []
  (min (for [j (range 1 3000)
             k (range (inc j) 3000)
             :let [pj (nth pentagonals j)
                   pk (nth pentagonals k)]
             :when (and (pentagonal? (+ pk pj))
                        (pentagonal? (- pk pj)))]
         (- pk pj))))

;; ================

;; Triangle, pentagonal, and hexagonal numbers are generated by the
;; following formulae:
;;
;; Triangle     Tn=n(n+1)/2     1, 3, 6, 10, 15, ...
;; Pentagonal   Pn=n(3n-1)/2    1, 5, 12, 22, 35, ...
;; Hexagonal    Hn=n(2n-1)      1, 6, 15, 28, 45, ...
;;
;; It can be verified that T285 = P165 = H143 = 40755.
;;
;; Find the next triangle number that is also pentagonal and hexagonal.

(def hexagonal-numbers (map #(* % (dec (* 2 %))) (iterate inc 1)))

(defn nth-hexagonal
  [n]
  (* n (dec (* 2 n))))

;; Since hexagonals jump fastest, we use those in the inner loop
(defn p45
  []
  (loop [ih 144, h (nth-hexagonal ih)
         ip 166, p (nth-pentagonal ip)
         it 286, t (nth-triangle it)]
    (cond
     (== h p t) h
     (== h p) (cond
               (< t p) (recur ih h ip p (inc it) (nth-triangle (inc it)))
               (> t p) (recur (inc ih) (nth-hexagonal (inc ih))
                              (inc ip) (nth-pentagonal (inc ip))
                              it t))
     (< p h) (recur ih h (inc ip) (nth-pentagonal ip) it t)
     (> p h) (recur (inc ih) (nth-hexagonal ih) ip p it t))))

;; ================

;; It was proposed by Christian Goldbach that every odd composite number can
;; be written as the sum of a prime and twice a square.
;;
;; 9 = 7 + 2(1^2)
;; 15 = 7 + 2(2^2)
;; 21 = 3 + 2(3^2)
;; 25 = 7 + 2(3^2)
;; 27 = 19 + 2(2^2)
;; 33 = 31 + 2(1^2)
;;
;; It turns out that the conjecture was false.
;;
;; What is the smallest odd composite that cannot be written as the sum of a
;; prime and twice a square?

(defn goldbachian?
  "If n satisfies the Goldbach Conjecture then return n, else return nil. We
return n so that the caller can use it as the return value of a call to
'some'."
  [n]
  (first (for [prime (drop 1 (primes-upto n)) ; we can skip 2 since 2 + even = even
               i (range 0 (inc (inc (Math/sqrt (/ n 2)))))
               :when (== n (+ prime (* 2 i i)))]
           n)))

(defn p46
  []
  (first (drop-while goldbachian? (iterate #(+ 2 %) 35))))

;; ================

;; The first two consecutive numbers to have two distinct prime factors are:
;;
;; 14 = 2 x 7
;; 15 = 3 x 5
;;
;; The first three consecutive numbers to have three distinct prime factors
;; are:
;;
;; 644 = 2^2 x 7 x 23
;; 645 = 3 x 5 x 43
;; 646 = 2 x 17 x 19.
;;
;; Find the first four consecutive integers to have four distinct primes
;; factors. What is the first of these numbers?

(defn p47
  []
  (loop [group-start 647
         checking-nth 3]
    (cond (not= 4 (count (prime-factors-of (+ group-start checking-nth)))) (recur (+ group-start (inc checking-nth)) 3)
          (zero? checking-nth) group-start
          :else (recur group-start (dec checking-nth)))))

;; ================

;; The series 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
;;
;; Find the last ten digits of the series
;; 1^1 + 2^2 + 3^3 + ... + 1000^1000.

(defn p48
  []
  (let [i (reduce + (map #(expt % %) (range 1 1001)))
        s (str i)]
    (subs s (- (count s) 10))))

;; ================

;; The arithmetic sequence 1487, 4817, 8147, in which each of the terms
;; increases by 3330, is unusual in two ways: (i) each of the three terms
;; are prime, and (ii) each of the 4-digit numbers are permutations of one
;; another.
;;
;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
;; primes exhibiting this property, but there is one other 4-digit
;; increasing sequence.
;;
;; What 12-digit number do you form by concatenating the three terms in this
;; sequence?

(defn p49-sequences
  "Given a vector of primes, return the sequences that have three or more
elements."
  [primes]
  (let [; Create a map whose keys are differences between primes and values
        ; are all the pairs of primes that have that difference.
        diffs-to-pairs (loop [d2p {}
                              ps (for [p1 primes
                                       p2 primes
                                       :when (> p2 p1)]
                                   (list p1 p2))]
                              ;; ps (combinations primes 2)]
                         (if ps (let [key (abs (- (first (first ps)) (second (first ps))))]
                                  (recur (assoc-conj d2p key (first ps) [])
                                         (next ps)))
                             d2p))
        ; Find only those values where there is more than one pair with the
        ; same diffs.
        d2p (filter #(== (count (val %)) 2) diffs-to-pairs)]
    ; find all those where the pattern is [(X Y) (Y Z)]
    (filter #(== (second (first %)) (first (second %))) (vals d2p))))

(defn p49
  []
  (let [fd-primes (drop-while #(< % 1000) (primes-upto 9999))
        ; Create a map whose keys are sorted digit characters and values are
        ; all primes with those digits.
        digits-to-primes (loop [d2p {}
                                ps fd-primes]
                           (if ps (let [key (sort (seq (str (first ps))))]
                                    (recur (assoc-conj d2p key (first ps) []) (next ps)))
                               d2p))]
    (first
     (remove #(= "148748178147" %)
             (map #(apply str (set (flatten %)))
                  (remove empty? (map p49-sequences (remove #(< (count %) 3) (vals digits-to-primes)))))))))

;; ================

;; The prime 41 can be written as the sum of six consecutive primes:
;;
;; 41 = 2 + 3 + 5 + 7 + 11 + 13
;;
;; This is the longest sum of consecutive primes that adds to a prime below
;; one hundred.
;;
;; Which prime below one million can be written as the sum of the most
;; consecutive primes?

(defn make-p50-sums
  [psl]
  (loop [ps (next psl)
         sums [(first psl)]]
    (if (nil? ps) sums
        (recur (next ps) (conj sums (+ (last sums) (first ps)))))))

;; This is inefficent, but correct.
(defn p50
  []
  (let [max-prime 1000000
        ps (vec (primes-upto max-prime))
        sums (make-p50-sums ps)
        answer (ref {:i 0 :j 5 :len 6 :prime 41 :nums []})]
    (loop [ijs (for [i (range 0 (count sums))
                     j (range (inc i) (count sums))]
                 {:i i :j j})]
      (if (nil? ijs) @answer
          (let [i (:i (first ijs))
                j (:j (first ijs))
                len (inc (- j i))]
            (if (> len (:len @answer))
              (let [sum (if (zero? i) (nth sums j) (- (nth sums j) (nth sums i)))]
                (if (and (<= sum max-prime) (some #{sum} ps))
                  (dosync (ref-set answer {:i i :j j :len len :prime sum :nums (take len (drop i sums))})))))
            (recur (next ijs)))))
    @answer))

;; ================

;; By replacing the 1st digit of *3, it turns out that six of the nine possible
;; values: 13, 23, 43, 53, 73, and 83, are all prime.
;;
;; By replacing the 3rd and 4th digits of 56**3 with the same digit, this
;; 5-digit number is the first example having seven primes among the ten
;; generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663,
;; 56773, and 56993. Consequently 56003, being the first member of this family,
;; is the smallest prime with this property.
;;
;; Find the smallest prime which, by replacing part of the number (not
;; necessarily adjacent digits) with the same digit, is part of an eight prime
;; value family.

(defn primes-with-n-digits
  "Return all primes with the same number of digits base 10, along with the
rest of the prime sequence (so we don't have to start at the beginning again
the next time this method is called)."
  [ps n-digits]
  (let [logval (dec n-digits)]
    (take-while #(== (int (Math/log10 %)) logval) (drop-while #(< (int (Math/log10 %)) logval) ps))))

;; TODO generalize p51-regexes-2 and p51-regexes-2 into one function.

(defn p51-regexes-2
  "Generates list of pairs of (A) all regex strings (not regexes) for
n-digit numbers where 2 digits are the same and (B) a \"mask\" that helps
return other digits in the number."
  [n-digits]
  (for [i (range 0 n-digits)
        j (range (inc i) n-digits)
        :let [num-leading-digits i
              num-middle-digits (- j i 1)
              num-trailing-digits (- n-digits j 1)
              match-n #(cond (== 1 %) "\\d"
                             (pos? %) (str "\\d{" % "}+"))]]
    (list
     (str (match-n num-leading-digits)   ; leading digits
          "(\\d)"                        ; first match
          (match-n num-middle-digits)    ; middle digits
          "\\1"                          ; second match
          (match-n num-trailing-digits)) ; trailing digits
     (apply str (flatten (conj []
                               (take num-leading-digits (repeat \.))
                               \X
                               (take num-middle-digits (repeat \.))
                               \X
                               (take num-trailing-digits (repeat \.))))))))

(defn p51-regexes-3
  "Generates list of pairs of (A) all regex strings (not regexes) for
n-digit numbers where 3 digits are the same and (B) a \"mask\" that helps
return other digits in the number."
  [n-digits]
  (for [i (range 0 n-digits)
        j (range (inc i) n-digits)
        k (range (inc j) n-digits)
        :let [num-leading-digits i
              num-middle-1-digits (- j i 1)
              num-middle-2-digits (- k j 1)
              num-trailing-digits (- n-digits k 1)
              match-n #(cond (== 1 %) "\\d"
                             (pos? %) (str "\\d{" % "}+"))]]
    (list
     (str (match-n num-leading-digits)   ; leading digits
          "(\\d)"                        ; first match
          (match-n num-middle-1-digits)  ; first middle digits
          "\\1"                          ; second match
          (match-n num-middle-2-digits)  ; digits middle digits
          "\\1"                          ; third match
         (match-n num-trailing-digits))  ; trailing digits
     (apply str (flatten (conj []
                               (take num-leading-digits (repeat \.))
                               \X
                               (take num-middle-1-digits (repeat \.))
                               \X
                               (take num-middle-2-digits (repeat \.))
                               \X
                               (take num-trailing-digits (repeat \.))))))))

(defn p51-masked
  "Given a mask like ...XX.. and a string, return a character sequence
with the characters at the 'X' places replaced by 'X'."
  [mask s]
  (map #(if (= \X %1) \X %2)
       mask s))

(defn p51-find
  "Given a number of digits and a regex-generating func (actually, it
generates regex/mask pairs), return the longest sequence of primes that
satisfies this problem's criteria."
  [n-digits regex-func]
  (let [ps (primes-with-n-digits primes n-digits)
        sps (map str ps)
        regex-mask-pairs (regex-func n-digits)
        pm-map (ref {})]
    ; Group numbers into those that match each regex. Key of pm-map is a
    ; single pair (regex, mask) and value is the list of number strings that
    ; matches the regex.
    (doseq [rm regex-mask-pairs
            s sps
            :when (.matches s (first rm))]
      (dosync (alter pm-map assoc-conj rm s ())))
    ; Now for each (regex, mask) pair further split up the matching values
    ; into those that match the same mask (for example, 1224 and 1334 both
    ; match the mask ".XX.").
    (let [mask-map (ref {})]
      (doseq [key (keys @pm-map)
              s (get @pm-map key)
              :let [mask (second key)
                    mask-key (p51-masked mask s)]]
        (dosync (alter mask-map assoc-conj mask-key s ())))
      (map #(Integer/parseInt %) (apply greatest-by #(count %) (vals @mask-map))))))

(defn p51
  []
  (apply min (flatten (for [n-digits (range 4 7)
                            regex-func (list p51-regexes-2 p51-regexes-3)
                            :let [longest (p51-find n-digits regex-func)]
                            :when (== (count longest) 8)]
                        longest))))

;; ================

;; It can be seen that the number, 125874, and its double, 251748, contain
;; exactly the same digits, but in a different order.
;;
;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
;; contain the same digits.

(defn p52-num
  [n]
  (let [digits (set (str n))]
    (and (= digits (set (str (* 2 n))))
         (= digits (set (str (* 3 n))))
         (= digits (set (str (* 4 n))))
         (= digits (set (str (* 5 n))))
         (= digits (set (str (* 6 n)))))))

(defn p52
 []
 (first (take 1 (drop-while (complement p52-num) (iterate inc 1)))))

;; ================

;; There are exactly ten ways of selecting three from five, 12345:
;;
;; 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
;;
;; In combinatorics, we use the notation, 5C3 = 10. (<sup>5</sup>C<sub>3</sub>)
;;
;; In general,
;;
;;   nCr = n! / (r!(n-r)!)
;;
;; where r <= n, n! = n*(n1)*...*3*2*1, and 0! = 1.
;;
;; It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
;;
;; How many, not necessarily distinct, values of nCr, for 1 <= n <= 100, are
;; greater than one-million?

;; We can rewrite this formula as
;;  (n(n-1)(n-2)..(n-k+1)) / k!

(defn num-combinations
  "Returns the number of conbinations of n things picked r at a time."
  [n r]
  (/ (nth factorials-to-100 n)
     (* (nth factorials-to-100 r) (nth factorials-to-100 (- n r)))))

(defn p53
  "Naive solution. Need to simplify the num-combinations forumula."
  []
  (reduce + (for [n (range 1 101)
                  r (range 1 n)         ; wasteful
                  ;; ; Since nCr == nC(n-r) we can look at only half the r
                  ;; ; values and return "2" if we need to double the count.
                  ;; :let [num-matches (if (== r (/ n 2)) 1 2)]
                  :when (> (num-combinations n r) 1000000)]
              1)))

;; ================

;; For p54, see poker.clj

;; ================

;; If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
;;
;; Not all numbers produce palindromes so quickly. For example,
;;
;; 349 + 943 = 1292,
;; 1292 + 2921 = 4213
;; 4213 + 3124 = 7337
;;
;; That is, 349 took three iterations to arrive at a palindrome.
;;
;; Although no one has proved it yet, it is thought that some numbers, like
;; 196, never produce a palindrome. A number that never forms a palindrome
;; through the reverse and add process is called a Lychrel number. Due to the
;; theoretical nature of these numbers, and for the purpose of this problem, we
;; shall assume that a number is Lychrel until proven otherwise. In addition
;; you are given that for every number below ten-thousand, it will either (i)
;; become a palindrome in less than fifty iterations, or, (ii) no one, with all
;; the computing power that exists, has managed so far to map it to a
;; palindrome. In fact, 10677 is the first number to be shown to require over
;; fifty iterations before producing a palindrome:
;; 4668731596684224866951378664 (53 iterations, 28-digits).
;;
;; Surprisingly, there are palindromic numbers that are themselves Lychrel
;; numbers; the first example is 4994.
;;
;; How many Lychrel numbers are there below ten-thousand?

(defn reverse-and-add
  [n]
  (+ n (read-string (apply str (drop-while #(= \0 %) (reverse (str n)))))))

(defn lychrel?
  [n]
  (loop [i 40
         n (reverse-and-add n)]
    (cond (zero? i) true
          (palindrome? n) false
          :else (recur (dec i) (reverse-and-add n)))))

(defn p55
  []
  (count (for [n (range 1 10000)
               :when (lychrel? n)]
           1)))

;; ================

;; A googol (10^100) is a massive number: one followed by one-hundred zeros;
;; 100^100 is almost unimaginably large: one followed by two-hundred zeros.
;; Despite their size, the sum of the digits in each number is only 1.
;;
;; Considering natural numbers of the form, a^b, where a, b < 100, what is the
;; maximum digital sum?

(defn p56
  []
  (apply max
         (for [a (range 1 100)
               b (range 1 100)]
           (sum-of-digits (expt a b)))))

;; ================

;; It is possible to show that the square root of two can be expressed as an
;; infinite continued fraction.
;;
;;  2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
;;
;; By expanding this for the first four iterations, we get:
;;
;; 1 + 1/2 = 3/2 = 1.5
;; 1 + 1/(2 + 1/2) = 7/5 = 1.4
;; 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
;; 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
;;
;; The next three expansions are 99/70, 239/169, and 577/408, but the eighth
;; expansion, 1393/985, is the first example where the number of digits in
;; the numerator exceeds the number of digits in the denominator.
;;
;; In the first one-thousand expansions, how many fractions contain a
;; numerator with more digits than denominator?

(defn p57
  []
  (loop [expansion 0
         frac-part 0
         num-big-numerator 0]
    (if (>= expansion 1000)
      num-big-numerator
      (let [new-frac-part (/ 1 (+ 2 frac-part))
            new-num (inc new-frac-part)]
        (recur (inc expansion)
               new-frac-part
               (if (> (count (str (numerator new-num))) (count (str (denominator new-num))))
                 (inc num-big-numerator)
                 num-big-numerator))))))

;; ================

;; Starting with 1 and spiralling anticlockwise in the following way, a square
;; spiral with side length 7 is formed.
;;
;;   37 36 35 34 33 32 31
;;   38 17 16 15 14 13 30
;;   39 18  5  4  3 12 29
;;   40 19  6  1  2 11 28
;;   41 20  7  8  9 10 27
;;   42 21 22 23 24 25 26
;;   43 44 45 46 47 48 49
;;
;; It is interesting to note that the odd squares lie along the bottom right
;; diagonal, but what is more interesting is that 8 out of the 13 numbers
;; lying along both diagonals are prime; that is, a ratio of 8/13 62%.
;;
;; If one complete new layer is wrapped around the spiral above, a square
;; spiral with side length 9 will be formed. If this process is continued, what
;; is the side length of the square spiral for which the ratio of primes along
;; both diagonals first falls below 10%?
;;
;; Note: Make sure we eat the list of primes in the right order
(defn p58
  []
    (loop [side-length 1
           num-diagonals 1
           num-primes 0
           ;; NOTE: the diagonal initialization looks wrong because this
           ;; spiral is counter-clockwise, while the diagonal frpm p28 is
           ;; clockwise.
           ne-diagonal (southeast-diagonal)
           nw-diagonal (southwest-diagonal)
           ;; can skip SE diagonal since they're all squares
           sw-diagonal (northwest-diagonal)]
      (let [new-side-length (+ 2 side-length)
            new-num-diagonals (+ num-diagonals 4)
            new-num-primes (+ num-primes
                              (if (divs-prime? (first ne-diagonal)) 1 0)
                              (if (divs-prime? (first nw-diagonal)) 1 0)
                              (if (divs-prime? (first sw-diagonal)) 1 0))]
        (cond (< (* 10 new-num-primes) new-num-diagonals) (+ 2 side-length)
              :else (recur (+ 2 side-length)
                           new-num-diagonals
                           new-num-primes
                           (rest ne-diagonal)
                           (rest nw-diagonal)
                           (rest sw-diagonal))))))

;; ================

;; Each character on a computer is assigned a unique code and the preferred
;; standard is ASCII (American Standard Code for Information Interchange).
;; For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
;;
;; A modern encryption method is to take a text file, convert the bytes to
;; ASCII, then XOR each byte with a given value, taken from a secret key.
;; The advantage with the XOR function is that using the same encryption key
;; on the cipher text, restores the plain text; for example, 65 XOR 42 =
;; 107, then 107 XOR 42 = 65.
;;
;; For unbreakable encryption, the key is the same length as the plain text
;; message, and the key is made up of random bytes. The user would keep the
;; encrypted message and the encryption key in different locations, and without
;; both "halves", it is impossible to decrypt the message.
;;
;; Unfortunately, this method is impractical for most users, so the modified
;; method is to use a password as a key. If the password is shorter than the
;; message, which is likely, the key is repeated cyclically throughout the
;; message. The balance for this method is using a sufficiently long password
;; key for security, but short enough to be memorable.
;;
;; Your task has been made easy, as the encryption key consists of three lower
;; case characters. Using cipher1.txt (right click and 'Save Link/Target
;; As...'), a file containing the encrypted ASCII codes, and the knowledge that
;; the plain text must contain common English words, decrypt the message and
;; find the sum of the ASCII values in the original text.

(defn xor-decrypt
  [nums c1 c2 c3]
  (apply str (map (comp char bit-xor) nums (cycle (list c1 c2 c3)))))

(defn p59
  []
  (let [text (first
              (let [nums (read-string (str "[" (slurp "data/cipher1_p59.txt") "]"))]
                (for [c1 (range (int \a) (inc (int \z)))
                      c2 (range (int \a) (inc (int \z)))
                      c3 (range (int \a) (inc (int \z)))
                      :let [decrypted (xor-decrypt nums c1 c2 c3)]
                      :when (and (pos? (.indexOf decrypted "the"))
                                 (pos? (.indexOf decrypted "and"))
                                 (== -1 (.indexOf decrypted (str (char 127)))))]
                  decrypted)))]
    (println text)
    (reduce + (map int text))))

;; ================

;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two
;; primes and concatenating them in any order the result will always be prime.
;; For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of
;; these four primes, 792, represents the lowest sum for a set of four primes
;; with this property.
;;
;; Find the lowest sum for a set of five primes for which any two primes
;; concatenate to produce another prime.

;; If remarkable then returns (p1 p2), else returns nil.
(defn p60-pair-remarkable?
  ([pair]
     (let [[p1 p2] (vec pair)]
       (p60-pair-remarkable? p1 p2)))
  ([p1 p2]
     (let [sp1p2 (str p1 p2)
           p1p2 (read-string sp1p2)
           p2p1 (read-string (str p2 p1))]
       (when (and (m-divs-prime? p1p2)
                  (m-divs-prime? p2p1))
         (list p1 p2)))))

(defn p60-remarkable?
  "If the primes are remarkable as described above, return them."
  [plist]
  (loop [pairs (combinations plist 2)]
    (cond (nil? pairs) plist            ; all pairs are OK, return primes
          (p60-pair-remarkable? (first pairs)) (recur (next pairs)) ; this pair OK, check next pair
          :else false)))         ; this pair is not remarkable, return false

;; This naive implementation is too slow. It's correct, because it finds the
;; group of four remarkable primes described above when I change
;; (combinations ps 5) to (combinations ps 4).

;; (defn p60
;;   [max-prime]
;;   (let [ps (take-while #(<= % max-prime) (filter #(not= 5 %) (drop 1 primes)))] ; remove 2 and 5
;;     (reduce + (some p60-remarkable? (combinations ps 5)))))

;; Also tested this version by running with 4 primes (by removing "e"), and
;; it works, too. It runs slowly, but it works.

(defn p60
  [max-prime]
  (let [ps (take-while #(<= % max-prime) (filter #(not= 5 %) (drop 1 primes))) ; remove 2 and 5
        nps (count ps)]
    (first (for [ai (range 0 nps)
                 :let [a (nth ps ai)]

                 bi (range (inc ai) nps)
                 :let [b (nth ps bi)]
                 :when (p60-pair-remarkable? a b)

                 ci (range (inc bi) nps)
                 :let [c (nth ps ci)]
                 :when (and (p60-pair-remarkable? a c)
                            (p60-pair-remarkable? b c))

                 di (range (inc ci) nps)
                 :let [d (nth ps di)]
                 :when (and (p60-pair-remarkable? a d)
                            (p60-pair-remarkable? b d)
                            (p60-pair-remarkable? c d))

                 ei (range (inc di) nps)
                 :let [e (nth ps ei)]
                 :when (and (p60-pair-remarkable? a e)
                            (p60-pair-remarkable? b e)
                            (p60-pair-remarkable? c e)
                            (p60-pair-remarkable? d e))]
               (+ a b c d e)))))

;; ================

;; Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal
;; numbers are all figurate (polygonal) numbers and are generated by the
;; following formulae:
;;
;; Triangle     P3,n=n(n+1)/2     1, 3, 6, 10, 15, ...
;; Square       P4,n=n^2          1, 4, 9, 16, 25, ...
;; Pentagonal   P5,n=n(3n-1)/2    1, 5, 12, 22, 35, ...
;; Hexagonal    P6,n=n(2n-1)      1, 6, 15, 28, 45, ...
;; Heptagonal   P7,n=n(5n-3)/2    1, 7, 18, 34, 55, ...
;; Octagonal    P8,n=n(3n-2)      1, 8, 21, 40, 65, ...
;;
;; The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three
;; interesting properties.
;;
;; 1. The set is cyclic, in that the last two digits of each number is the
;; first two digits of the next number (including the last number with the
;; first).
;;
;; 2. Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and
;; pentagonal (P5,44=2882), is represented by a different number in the set.
;;
;; 3. This is the only set of 4-digit numbers with this property.

;; Find the sum of the only ordered set of six cyclic 4-digit numbers for
;; which each polygonal type: triangle, square, pentagonal, hexagonal,
;; heptagonal, and octagonal, is represented by a different number in the
;; set.


(def square-numbers (map #(* % %) (iterate inc 1)))

(def heptagonal-numbers (map #(/ (* % (- (* 5 %) 3)) 2) (iterate inc 1)))

(def octagonal-numbers (map #(* % (- (* 3 %) 2)) (iterate inc 1)))

(defn p61-diff-in-each?
  "Find out if each of the polygonal types is represented by a different number in nset."
  [nset octs hepts hexes pents squares tris]
  (let [members {:octs    (filter identity (map #(some #{%} octs) nset))
                 :hepts   (filter identity (map #(some #{%} hepts) nset))
                 :hexes   (filter identity (map #(some #{%} hexes) nset))
                 :pents   (filter identity (map #(some #{%} pents) nset))
                 :squares (filter identity (map #(some #{%} squares) nset))
                 :tris    (filter identity (map #(some #{%} tris) nset))}]
    (and (every? (complement empty?) (vals members))
         (apply all-different? (vals members)))))

(defn p61
  []
  (let [in-range (fn [nums] (take-while #(<= % 9999) (drop-while #(< % 1010) nums)))
        octs    (in-range octagonal-numbers)
        hepts   (in-range heptagonal-numbers)
        hexes   (in-range hexagonal-numbers)
        pents   (in-range pentagonal-numbers)
        squares (in-range square-numbers)
        tris    (in-range triangle-numbers)
        interesting (set (concat octs hepts hexes pents squares tris))
        != (complement ==)]
    (apply + (first (set (for [half1 (range 10 100) ; need set because I'm generating dupes :-(

                               half2 (range 10 100)
                               :let [n1 (+ (* 100 half1) half2)]
                               :when (some #{n1} interesting)

                               half3 (range 10 100)
                               :let [n2 (+ (* 100 half2) half3)]
                               :when (and (!= n1 n2)
                                          (some #{n2} interesting))

                               half4 (range 10 100)
                               :let [n3 (+ (* 100 half3) half4)]
                               :when (and (!= n1 n3)
                                          (!= n2 n3)
                                          (some #{n3} interesting))

                               half5 (range 10 100)
                               :let [n4 (+ (* 100 half4) half5)]
                               :when (and (!= n1 n4)
                                          (!= n2 n4)
                                          (!= n3 n4)
                                          (some #{n4} interesting))

                               half6 (range 10 100)
                               :let [n5 (+ (* 100 half5) half6)]
                               :when (and (!= n1 n5)
                                          (!= n2 n5)
                                          (!= n3 n5)
                                          (!= n4 n5)
                                          (some #{n5} interesting))

                               :let [n6 (+ (* 100 half6) half1)]
                               :when (and (!= n1 n6)
                                          (!= n2 n6)
                                          (!= n3 n6)
                                          (!= n4 n6)
                                          (!= n5 n6)
                                          (some #{n6} interesting))
                               :let [nset (hash-set n1 n2 n3 n4 n5 n6)]
                               :when (p61-diff-in-each? nset octs hepts hexes pents squares tris)]
                           nset))))))

(comment
  (defn p61-in-range [nums] (take-while #(<= % 9999) (drop-while #(< % 1010) nums)))
  (def octs    (p61-in-range octagonal-numbers))
  (def hepts   (p61-in-range heptagonal-numbers))
  (def hexes   (p61-in-range hexagonal-numbers))
  (def pents   (p61-in-range pentagonal-numbers))
  (def squares (p61-in-range square-numbers))
  (def tris    (p61-in-range triangle-numbers))
  (def interesting (set (concat octs hepts hexes pents squares tris)))
)

;; ================

;; The cube 41063625 (345^3), can be permuted to produce two other cubes:
;; 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest
;; cube which has exactly three permutations of its digits which are also
;; cubes.
;;
;; Find the smallest cube for which exactly five permutations of its digits
;; are cubes.

(def cubes (for [i (iterate inc 1)] (* i i i)))

(defn find-min-cube-with-five-perms
  "Given a list of cubes, all with the same number of digits, find the first
  group of five cubes that are permutations of each other."
  [n-digit-cubes]
  ;; DEBUG finds first set of digits and returns that
  (let [found (first (drop-while #(< (val %) 5) (frequencies (map (comp sort str) n-digit-cubes))))]
    (when found                         ; we found five cubes that have the same digits
      (first (filter #(= (key found) (sort (str %))) n-digit-cubes)))))

(defn p62
  []
  (first (for [num-digits (iterate inc 8)
               :let [n-digit-cubes (take-while #(== (count (str %)) num-digits) (drop-while #(< (count (str %)) num-digits) cubes))
                     min-cube-with-five-perms (find-min-cube-with-five-perms n-digit-cubes)]
               :when min-cube-with-five-perms]
           min-cube-with-five-perms)))

;; ================

;; The 5-digit number 16807=7^5, is also a fifth power. Similarly, the 9-digit
;; number 134217728=8^9, is a ninth power.
;;
;; How many n-digit positive integers exist which are also an nth power?

(defn p63
  []
  (loop [power 1
         nums ()]
    (let [more-nums (for [base (range 1 10) ; base 1-9; 10^x has x+1 digits
                          :let [n (expt base power)]
                          :when (== power (count (str n)))]
                      n)]
      (if (empty? more-nums)
        (count (set nums))              ; set removes repeated 1s etc.
        (recur (inc power) (concat nums more-nums))))))

;; ================

;; See http://projecteuler.net/index.php?section=problems&id=64

;; I approached this problem a bit differently than most: the fractions are
;; manipulated symbolically.

;; A p64-frac struct holds normalized (sqrt in numerator) and non-normalized
;; (sqrt in denominator) fractions that are part of a continued fraction.
;; See p64-frac-str for a function that returns a human readable
;; non-parseable string representation.
;;
;; int-sqrt is not needed by the representation of the fraction, but it is
;; useful to carry this value around instead of having to recompute it or
;; pass it in to functions separately all the time.
(defstruct p64-frac :n :int-sqrt :numer-mult :numer-const :denom-const :normalized)

(defn p64-frac-str
  "Print a p64-frac. Not pretty. For debugging."
  [frac]
  (let [posneg #(if (pos? %) (str "+" %) (str "-" (abs %)))]
    (if (:normalized frac)
      (let [nm (:numer-mult frac)]
        (str (if (not= nm 1) (str nm "*")) "(sqrt(" (:n frac) ")" (posneg (:numer-const frac)) ") / "
             (:denom-const frac)))
      (str (:numer-const frac) " / (sqrt(" (:n frac) ")" (posneg (:denom-const frac)) ")"))))

(defn invert-p64-frac
  "Return a p64-frac that is 1 / frac."
  [frac]
  (struct p64-frac (:n frac) (:int-sqrt frac) 1 (:denom-const frac) (:numer-const frac) (not (:normalized frac))))

(defn symbolically-normalize
  "Given a non-normalized p64-frac

    numer-const / (sqrt(N) + denom-const)

with the sqrt in the denominator, return a new p64-frac where sqrt is in the
numerator, which will be of the form

  (numer-mult * (sqrt(N) + numer-const) / denom-const

If frac is already normalized, return it unchanged."
  [frac]
  (if (:normalized frac) frac
      (let [denom-const (- (:n frac) (* (:denom-const frac) (:denom-const frac)))
            numer-mult (:numer-const frac)
            numer-const (- (:denom-const frac))
            nd-ratio (/ numer-mult denom-const)]
        (struct p64-frac (:n frac) (:int-sqrt frac)
                (if (ratio? nd-ratio) (numerator nd-ratio) nd-ratio)
                numer-const
                (if (ratio? nd-ratio) (denominator nd-ratio) 1)
                true))))

(defn p64-next-step
  "Takes a non-normalized p64-frac and returns a pair of values consisting
of the integer portion and a normalized p64-frac representing the
remainder."
  [frac]
  (let [frac2 (symbolically-normalize (invert-p64-frac frac))
        int-portion (int (/ (* (:numer-mult frac2) (+ (:int-sqrt frac2) (:numer-const frac2)))
                            (:denom-const frac2)))
        new-numer-const (- (:numer-const frac2) (* int-portion (:denom-const frac2)))]
    (list int-portion (assoc frac2 :numer-const new-numer-const))))

(defn sqrt-continued-fraction-representation
  "Return a vector whose first element is the integer part and second
element is a vector containing the repeating sequence portion of the
continued fraction representation of the square root of n."
  [n]
  (let [[int-portion rem] (exact-integer-sqrt n)]
    (if (zero? rem) [int-portion []]
        (loop [frac (struct p64-frac n int-portion 1 (- int-portion) 1 true)
               seen-fracs []
               nums []]
          (let [[term new-frac] (p64-next-step frac)]
            (cond (zero? term) [int-portion []] ; exact sqare root
                  (some #{new-frac} seen-fracs) [int-portion nums] ; found repeat
                  :else (recur new-frac (conj seen-fracs new-frac) (conj nums term))))))))

(defn p64
  "Return the number of square roots of 1 <= N <= 10000 whose repeating
sequences' lengths are odd."
  []
  (let [f (comp odd? count second sqrt-continued-fraction-representation)]
    (count
     (for [n (range 2 10001)
           :when (f n)]
         n))))

;; ================

;; See http://projecteuler.net/index.php?section=problems&id=65
;;
;; The first ten terms in the sequence of convergents for e are:
;;
;; 2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...

;; Includes the initial integer 2 and the fraction constants a(k)
;; where the nth term of e-terms is a(k-1).
(def e-frac-consts (concat '(2) (mapcat #(list 1 (* 2 %) 1) (iterate inc 1))))

(defn nth-convergent-term
  "Takes a list of continued fraction constants, including the first integer
term, and returns the ratio that is the nth convergent term."
  [frac-consts n]
  (let [fcs (reverse (take n frac-consts))]
    (loop [ak (next fcs)
           val (first fcs)]
      (if (or (nil? ak) (empty? ak)) val
          (recur (next ak) (+ (first ak) (/ 1 val)))))))

(defn p65
  []
  (sum-of-digits (numerator (nth-convergent-term e-frac-consts 100))))

;; ================

;; Consider quadratic Diophantine equations of the form:
;;
;; x^2 – Dy^2 = 1
;;
;; For example, when D=13, the minimal solution in x is 649^2 - 13*180^2 = 1.
;;
;; It can be assumed that there are no solutions in positive integers when D
;; is square.
;;
;; By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the
;; following:
;;
;; 3^2 – 2*2^2 = 1
;; 2^2 – 3*1^2 = 1
;; 9^2 – 5*4^2 = 1
;; 5^2 – 6*2^2 = 1
;; 8^2 – 7*3^2 = 1
;;
;; Hence, by considering minimal solutions in x for D <= 7, the largest x is
;; obtained when D=5.
;;
;; Find the value of D <= 1000 in minimal solutions of x for which the
;; largest value of x is obtained.
;;
;; These solutions yield rational approximations of the form x/y to the
;; square root of D.

;; A naive solution where we keep trying x's until there is a y such that
;;
;;   (x^2 - 1)/D == y^2
;;
;; takes too long. Research lead to the conclusion that we need to use
;; sqrt(D)'s continued fraction representation and find the first term where
;; x/y is a solution for the Diophantine equation.

(defn minimal-diophantine-x
  [d]
  (let [[int-portion repeating-terms] (sqrt-continued-fraction-representation d)
        frac-consts (concat [int-portion] (cycle repeating-terms))
        diophantine-solution? (fn [d x y] (== 1 (- (* x x) (* d y y))))]
    (if (zero? (count repeating-terms)) -1 ; d is an exact square
        (loop [n 1]
          (let [r (nth-convergent-term frac-consts n)
                x (if (ratio? r) (numerator r) r)
                y (if (ratio? r) (denominator r) 1)]
            (if (diophantine-solution? d x y) x
                (recur (inc n))))))))

(defn p66
  []
  (apply max-key minimal-diophantine-x (range 8 1001)))
    

;; ================

;; Find the maximum total from top to bottom in data/triangle_p67.txt, a 15K
;; text file containing a triangle with one-hundred rows.
;;
;; See problem 18.

(defn p67
  []
  (let [lines (split-lines (slurp "data/triangle_p67.txt"))
        t-text (str "[[" (apply str (interpose "][" lines)) "]]")
        triangle (read-string (.replaceAll (.replaceAll t-text " 0" " ") "\\[0" "[")) ; need to remove leading 0s
        last-row-paths (max-paths-to triangle 0 nil)]
    (apply max (map :value last-row-paths))))

;; ================

;; http://projecteuler.net/index.php?section=problems&id=68
;; Find the maximum 16-digit string for a "magic" 5-gon.

(def fivegon-order [[0 1 2] [3 2 4] [5 4 6] [7 6 8] [9 8 1]])

(defn make-fgon
  [coll]
  (let [v (vec coll)]
    (map #(vector (nth v (nth % 0)) (nth v (nth % 1)) (nth v (nth % 2))) fivegon-order)))

(defn fivegon-description
  "Take fivegon and turn it into a single integer per the directions in the
problem description."
  [fgon]
  (let [min-group (apply min-key first fgon)
        in-order (take 5 (drop-while #(not= min-group %) (cycle fgon)))]
    (read-string (apply str (flatten in-order)))))

(defn magic-5gon?
  "If we have a magic 5-gon, return the 16-digit string representing it."
 [coll]
 (let [fgon (make-fgon coll)
       sum1 (reduce + (first fgon))
       matching-sums (take-while #(== sum1 (reduce + %)) (rest fgon))]
   (when (== (dec (count fivegon-order)) (count matching-sums))
     (fivegon-description fgon))))

(defn p68
  []
  (apply max (filter #(when (and % (== (count (str %)) 16)) %)
                     (map #(magic-5gon? %) (permutations (range 1 11))))))

;; ================

;; http://projecteuler.net/index.php?section=problems&id=69
;; Find the number up to 1,000,000 with a max value of N/φ(N).

;; My original solution that calculated φ was correct, but WAY too slow. A
;; max value is found when there are a maximal number of primes as divisors.
;; When does that happen? When the number is made up of as many primes as
;; possible.

;; Simplified: product of primes that are less than 1,000,000.
(defn p69
  []
  (loop [prod 1
         ps primes]
    (let [p (first ps)]
    (if (> (* prod p) 100000) (* prod p)
        (recur (* prod p) (rest ps))))))

;; ================

;; Euler's Totient function, φ(n) [sometimes called the phi function], is
;; used to determine the number of positive numbers less than or equal to n
;; which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8,
;; are all less than nine and relatively prime to nine, φ(9)=6.

;; The number 1 is considered to be relatively prime to every positive
;; number, so φ(1)=1.

;; Interestingly, φ(87109)=79180, and it can be seen that 87109 is a
;; permutation of 79180.

;; Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n
;; and the ratio n/φ(n) produces a minimum.

;; Notes: φ(n) must have same number of digits.
;; Minimal value if n is prime, but that won't work. What if n is the
;; product of two primes?
;;
;; (- 1 (/ 1 p)) == (/ (dec p) p)
;; φ(p) = p-1, if p is prime
;; φ(pq) = φ(p)φ(q)
;;       = (p-1)(q-1), if p and q are both prime

(defn p70-ns-and-nphis
  []
  (let [sqrt (inc (int (Math/sqrt 10000000)))
        ps (primes-upto 10000000)
        under (reverse (take-while #(<= % sqrt) ps))
        over (drop (count under) ps)]
    (for [p (take 300 under)
          q (take 300 over)
          :let [n (* p q)]
          :when (< n 10000000)
          :let [phi (* (dec p) (dec q))]
          :when (numeric-permutation? n phi)]
      (list n (/ n phi) p q))))

(defn p70
  []
  (apply min-key second (p70-ns-and-nphis)))

;; ================

;; Consider the fraction n/d, where n and d are positive integers. If n<d
;; and HCF(n,d)=1, it is called a reduced proper fraction.
;; 
;; If we list the set of reduced proper fractions for d <= 8 in ascending
;; order of size, we get:
;;
;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8,
;; 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;;
;; It can be seen that 2/5 is the fraction immediately to the left of 3/7.
;;
;; By listing the set of reduced proper fractions for d <= 1,000,000 in
;; ascending order of size, find the numerator of the fraction immediately
;; to the left of 3/7.


;; See http://en.wikipedia.org/wiki/Farey_Sequence
;;
;; If a/b and c/d are neighbors in a Farey seq, with a/b < c/d, then
;;
;;   bc - ad = 1
;;
;; Remember that for this problem c = 3 and d = 7, so
;;
;;   3b - 7a = 1
;;
;; Conversely, if the previous is true then the two are neighbors in the
;; Farey sequence of order max(b, d). Now all we need to do is find the
;; highest a where b is an integer.
;;
;; (bc - 1) / d = a
;; (3b - 1) / 7 = a

(defn p71
  []
  (loop [b 1000000]
    (let [a (/ (dec (* 3 b)) 7)]
      (cond (integer? a) a
            :else (recur (dec b))))))

;; ================

;; See p71. How many elements are in the set of reduced proper fractions for
;; d <= 1000000?

;; See http://en.wikipedia.org/wiki/Farey_Sequence
;;
;;   length Fn = 1 + sum(for m=1..n, phi(m))

;; Notes:
;;
;; We need to subtract 2 from Fn because this problem doesn't count 0/1 or
;; 1/1.
;;
;; Calculating the prime factors takes a lot of time, so we pre-calculate
;; them using a prime sieve.

(defn fill-mults-of
  "Given a vector v of n+1 vectors (0..n), return a new vector where each
index that is a multiple of p contains p."
  [n p v]
  (loop [v v
         mult p]
    ;; (println "fmo n =" n "mult =" mult) ; DEBUG
    (if (<= mult n) (recur (assoc v mult (conj (nth v mult) p)) (+ mult p))
        v)))

(defn prime-factors-upto
  "Return a vector whose i'th entry is a sequence of the prime factors of i.
For example, the entry for 36 will be [2, 3]. Entries 0 and 1 will be empty
vectors."
  [n]
  (loop [v (vec (repeat (inc n) []))
         i 2
         p 2
         ps (next primes)]
    (if (<= p n) (recur (fill-mults-of n p v) (inc i) (first ps) (next ps))
        v)))

(defn phi
  "Return the value of Euler's totient function for n.

Because calculating the prime factors of a number takes time and there is a
lot of work if you are doing this for a range of numbers, you should first
call prime-factors-upto for the max value of the range then pass that in.
See prime-factors-upto and p72."
  ([n] (phi n (prime-factors-upto n)))
  ([n prime-factor-vec]
     (let [pf (nth prime-factor-vec n)]
           (/ (* n (reduce * (map dec pf)))
              (reduce * pf)))))

(defn p72
  []
  (let [prime-factors (prime-factors-upto 1000000)]
    (dec (reduce + (map #(phi % prime-factors) (range 1 1000001))))))

;; ================

;; See p71. How many fractions lie between 1/3 and 1/2 in the sorted set of
;; reduced proper fractions for d 12,000?

(defn p73
  []
  (loop [a 0, b 1, c 1, d 12000
         num-fracs 0]
    (let [frac (/ a b)
          k (int (/ (+ 12000 b) d))]
      (cond (<= frac 1/3) (recur c d (- (* k c) a) (- (* k d) b) num-fracs)
            (>= frac 1/2) num-fracs
            :else         (recur c d (- (* k c) a) (- (* k d) b) (inc num-fracs))))))

;; ================

;; The number 145 is well known for the property that the sum of the factorial
;; of its digits is equal to 145:
;;
;;   1! + 4! + 5! = 1 + 24 + 120 = 145
;;
;; Perhaps less well known is 169, in that it produces the longest chain of
;; numbers that link back to 169; it turns out that there are only three
;; such loops that exist:

;; 169 -> 363601 -> 1454 -> 169
;; 871 -> 45361 -> 871
;; 872 -> 45362 -> 872
;;
;; It is not difficult to prove that EVERY starting number will eventually
;; get stuck in a loop. For example,
;;
;; 69 -> 363600 -> 1454 -> 169 -> 363601 (-> 1454)
;; 78 -> 45360 -> 871 -> 45361 (-> 871)
;; 540 -> 145 (-> 145)
;;
;; Starting with 69 produces a chain of five non-repeating terms, but the
;; longest non-repeating chain with a starting number below one million is
;; sixty terms.
;;
;; How many chains, with a starting number below one million, contain
;; exactly sixty non-repeating terms?

(def digit-factorials (vec (map factorial (range 0 10))))
(def char-digit-factorials
  (apply hash-map (interleave [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9] digit-factorials)))

(defn sum-of-factorials-of-digits
  "Returns the sum of the factorials of each of the digits of n."
  [n]
  (apply + (map #(get char-digit-factorials %) (str n))))

(defn num-non-repeating-sofod-terms
  "Returns the number of non-repeating terms in the sequence of integers
found by repeatedly applying sum-of-factorials-of-digits to n."
  [n]
  (loop [n n
         terms [n]]
    (let [next-term (sum-of-factorials-of-digits n)]
      (if (some #{next-term} terms) (count terms)
          (recur next-term (conj terms next-term))))))

(defn p74
  []
  (count (for [n (range 2 1000000)
               :when (== 60 (num-non-repeating-sofod-terms n))]
           n)))

;; ================

;; It turns out that 12 cm is the smallest length of wire that can be bent
;; to form an integer sided right angle triangle in exactly one way, but
;; there are many more examples.
;;
;; 12 cm: (3,4,5)
;; 24 cm: (6,8,10)
;; 30 cm: (5,12,13)
;; 36 cm: (9,12,15)
;; 40 cm: (8,15,17)
;; 48 cm: (12,16,20)
;;
;; In contrast, some lengths of wire, like 20 cm, cannot be bent to form an
;; integer sided right angle triangle, and other lengths allow more than one
;; solution to be found; for example, using 120 cm it is possible to form
;; exactly three different integer sided right angle triangles.
;;
;; 120 cm: (30,40,50), (20,48,52), (24,45,51)
;;
;; Given that L is the length of the wire, for how many values of L <=
;; 1,500,000 can exactly one integer sided right angle triangle be formed?

;; The naive implementation (find all legit perims for each L) takes too
;; long. See http://en.wikipedia.org/wiki/Pythagorean_triple.

(defn p75-euclid-perims
  [l]
  (let [sq (int (Math/sqrt l))]
    (for [m (range 1 sq 2)
          n (range 2 sq 2)
          :when (coprime? m n)
          :let [perim (+ (abs (- (* m m) (* n n)))
                         (* 2 m n)
                         (+ (* m m) (* n n)))]
          k (range 1 (/ (inc l) perim))]
      (* k perim))))

(defn p75
  []
  (count (filter #(== 1 (val %)) (frequencies (p75-euclid-perims 1500000)))))

;; ================

;; It is possible to write five as a sum in exactly six different ways:
;;
;; 4 + 1
;; 3 + 2
;; 3 + 1 + 1
;; 2 + 2 + 1
;; 2 + 1 + 1 + 1
;; 1 + 1 + 1 + 1 + 1
;;
;; How many different ways can one hundred be written as a sum of at least
;; two positive integers?

;; http://en.wikipedia.org/wiki/Partition_(number_theory)

(defn partial-partition
  [k n]
  (cond (> k n) 0
        (== k n) 1
        :else (+ (partial-partition (inc k) n)
                 (partial-partition k (- n k)))))

;; Memoization helps a lot here.
(def partial-partition (memoize partial-partition))

(defn p76
  []
  ;; We subtract 1 because we don't want to count the case where the
  ;; partition is 100 itself.
 (dec (partial-partition 1 100)))

;; ================

;; It is possible to write ten as the sum of primes in exactly five
;; different ways:
;;
;; 7 + 3
;; 5 + 5
;; 5 + 3 + 2
;; 3 + 3 + 2 + 2
;; 2 + 2 + 2 + 2 + 2
;;
;; What is the first value which can be written as the sum of primes in over
;; five thousand different ways?

;; Notes

;; http://oeis.org/A000607
;;
;; Like p76, we subtract 1 because we don't want to count the case where the
;; partition is the number itself.

;; From OEIS, MAPLE code:
;;
;; t1:=mul(1/(1-q^ithprime(n)), n=1..51);
;; t2:=series(t1, q, 50);
;; t3:=seriestolist(t2);

;; wrong
(defn a000607-maple
  [n]
  (reduce * (for [q (range 2 (inc n))
                  :let [_ (println q)]]
              (/ 1 (- 1 (expt q (nth primes n)))))))

(def m-primes-upto (memoize primes-upto))

(defn num-ways-sum-of-primes
  [n]
  (cond (< n 2) 0
        (== n 2) 1
        (== n 3) 1
        (== n 5) 2
        :else (reduce + (for [p (reverse (m-primes-upto n))]
                          (num-ways-sum-of-primes (- n p))))))

(def num-ways-sum-of-primes (memoize num-ways-sum-of-primes))

(defn p77
  []
  "not yet implemented")

;; ================

;; Let p(n) represent the number of different ways in which n coins can be
;; separated into piles. For example, five coins can separated into piles in
;; exactly seven different ways, so p(5)=7.
;;
;; OOOOO
;; OOOO   O
;; OOO   OO
;; OOO   O   O
;; OO   OO   O
;; OO   O   O   O
;; O   O   O   O   O
;;
;; Find the least value of n for which p(n) is divisible by one million.

;; Notes

;; This is the same as the p76 partitioning problem, except we can now
;; include the case where the partition equals the number (a single pile of
;; coins).

;; This naive implementation is too slow.
;; (defn p78
;;   []
;;   (first (for [n (iterate inc 1)
;;                :when (zero? (mod (partial-partition 1 n) 1000000))]
;;            n)))

(defn p78
  []
  (loop [n 1]
    (when (zero? (mod n 1000)) (println n)) ; DEBUG
    (if (zero? (mod (partial-partition 1 n) 1000000)) n
        (recur (inc n)))))
