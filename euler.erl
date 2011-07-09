-module(euler).
-author("Jim Menard, jim@jimmenard.com").
-compile(export_all).
-import(lists, [sum/1, max/1, filter/2, seq/2, reverse/1, foldl/3, split/2,
                map/2]).

%% Project Euler http://projecteuler.net
%% code:add_path("../puzzles").

timed_funcall(F) ->
    statistics(runtime),
    statistics(wall_clock),
    Answer = F(),
    {_, T1} = statistics(runtime),
    {_, T2} = statistics(wall_clock),
    U1 = T1,
    U2 = T2,
    io:format("Run time = ~p (~p) seconds~n", [U1 / 1000.0, U2 / 1000.0]),
    Answer.

% ****************************************************************

%% Sum of all multiples of 3 or 5 below 1000.
p1() ->
    sum(mults_of_3or5_below_1000()).
mults_of_3or5_below_1000() ->
    mults_of_3or5_below_1000([], 1).
mults_of_3or5_below_1000(L, 1000) ->
    L;
mults_of_3or5_below_1000(L, I) when I rem 3 =:= 0; I rem 5 =:= 0 ->
    mults_of_3or5_below_1000([I | L], I + 1);
mults_of_3or5_below_1000(L, I) ->
    mults_of_3or5_below_1000(L, I + 1).

%% Sum of all even-valued terms in Fibonacci which do not exceed four million.
p2() ->
    sum([X || X <- fibonacci_upto(3999999),
                    X rem 2 =:= 0]).
fibonacci_upto(N) ->
    fibonacci_upto(N, [], 1, 1).
fibonacci_upto(N, L, I1, I2) when I1 + I2 > N ->
    L;
fibonacci_upto(N, L, I1, I2) ->
    Next = I1 + I2,
    fibonacci_upto(N, [Next | L], I2, Next).

%% Largest prime factor of 600851475143. Since 600851475143 is not prime (it's
%% divisible by 3), we can start with the quare root.
p3() ->
    N = 600851475143,
    max_prime_factor_of(N, puzzle:prev_prime(trunc(math:sqrt(N)) + 1)).

max_prime_factor_of(_N, 1) ->
    1;
max_prime_factor_of(N, P) when N rem P =:= 0 ->
    P;
max_prime_factor_of(N, P) ->
    max_prime_factor_of(N, puzzle:prev_prime(P)).

%% Largest palindrome made from the product of two 3-digit numbers.
p4() ->
    max([Y * Z || Y <- seq(100, 999),
                        Z <- seq(100, 999),
                        is_palindrome(Y * Z)]).
is_palindrome(N) when is_integer(N) ->
    S = integer_to_list(N),
    S == reverse(S).

%% Smallest number evenly divisible by all of the numbers from 1 to 20. We
%% start with a list that has already removed factors of higher numbers.
stupid_p5() ->
    stupid_p5(2050).
stupid_p5(I) when is_integer(I),
%%                   I rem 2 =:= 0,
%%                   I rem 3 =:= 0,
%%                   I rem 4 =:= 0,
%%                   I rem 5 =:= 0,
%%                   I rem 6 =:= 0,
%%                   I rem 7 =:= 0,
%%                   I rem 8 =:= 0,
%%                   I rem 9 =:= 0,
%%                   I rem 10 =:= 0,
                  I rem 11 =:= 0,
                  I rem 12 =:= 0,
                  I rem 13 =:= 0,
                  I rem 14 =:= 0,
                  I rem 15 =:= 0,
                  I rem 16 =:= 0,
                  I rem 17 =:= 0,
                  I rem 18 =:= 0,
                  I rem 19 =:= 0,
                  I rem 20 =:= 0 ->
    I;
stupid_p5(I) when is_integer(I) ->
    stupid_p5(I+1).
                  
p5() ->
    L = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20],
    I = foldl(fun(I, Acc) -> Acc * I end, 1, L),
    p5_reduce(L, I).

p5_reduce([], I) when is_integer(I) ->
    I;
p5_reduce([H|T], I) when is_integer(H), is_integer(I) ->
    io:format("H = ~p, I = ~p, (I/H) = ~p, I div trunc(I/H) = ~p~n", [H, I, (I/H), I div trunc(I/H)]),
    p5_reduce(T, I div trunc(I / H)).

%% Find the difference between the sum of the squares of the first one hundred
%% natural numbers and the square of the sum.
p6() ->
    L = seq(1, 100),
    Sum = sum(L),
    SquareOfSum = Sum * Sum,
    SumOfSquares = foldl(fun(I, Acc) -> Acc + (I * I) end, 0, L),
    SquareOfSum - SumOfSquares.

%% What is the 10001st prime number?
p7() ->
    nth_prime(10001).
nth_prime(N) ->
    nth_prime(N, 2).
nth_prime(1, Prime) ->
    Prime;
nth_prime(N, Prime) ->
    nth_prime(N-1, puzzle:next_prime(Prime)).

%% Find the greatest product of five consecutive digits in the 1000-digit
%% number N.
p8() ->
    Nstr = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450",
    biggest_prod(0, Nstr).
biggest_prod(BiggestProd, Nstr) when length(Nstr) < 5 ->
    BiggestProd;
biggest_prod(BiggestProd, Nstr) ->
    {FirstFive, _Rest} = split(5, Nstr),
    Prod = foldl(fun(I, Acc) -> Acc * I end, 1, map(fun(C) -> C - $0 end, FirstFive)),
    {_FirstChar, Rest2} = split(1, Nstr),
    biggest_prod(max([BiggestProd, Prod]), Rest2).

%% There exists exactly one Pythagorean triplet for which a + b + c = 1000.
%% Find the product abc.
%%
%% See also p9b below which is faster because it stops after finding the first
%% answer (the other answers are duplicates with A, B, and C in different
%% orders).
%%
%% Note: this is correct but slow, and it returns more than one copy of the
%% answer because of permutations of A, B, and C. Using "andalso" instead of
%% "," helped speed things up, because "," does not short-circuit.
%%
%% Answer: 31875000
p9() ->
    [A * B * C ||
        A <- seq(1, 998),
        B <- seq(1, 998),
        C <- seq(1, 998),
        A + B + C =:= 1000 andalso A*A + B*B =:= C*C].

%% A faster version of p9().
p9b() ->
    A = seq(1, 998),
    B = seq(1, 998),
    C = seq(1, 998),
    first_pythag_1000(A, A, B, B, C, C).

first_pythag_1000([], _AllA, [], _AllB, [], _AllC) ->
    throw(not_found);
first_pythag_1000([], AllA, [_|BT], AllB, C, AllC) ->
    first_pythag_1000(AllA, AllA, BT, AllB, C, AllC);
first_pythag_1000(A, AllA, [], AllB, [_|CT], AllC) ->
    first_pythag_1000(A, AllA, AllB, AllB, CT, AllC);
first_pythag_1000([AH|_], _, [BH|_], _, [CH|_], _)
  when AH + BH + CH =:= 1000 andalso AH*AH + BH*BH =:= CH*CH ->
    AH * BH * CH;
first_pythag_1000([_|AT], AllA, B, AllB, C, AllC) ->
    first_pythag_1000(AT, AllA, B, AllB, C, AllC).
    
%% Find the sum of all the primes below two million.
%% Answer: 
p10() ->
    sum_primes_below(2000000).
sum_primes_below(N) ->
    sum_primes_below(N, 0, 2).
sum_primes_below(N, Sum, Prime) when Prime >= N -> 
    Sum;
sum_primes_below(N, Sum, Prime) ->
    sum_primes_below(N, Sum + Prime, puzzle:next_prime(Prime)).

%% What is the greatest product of four numbers in any direction (up, down,
%% left, right, or diagonally) in the 20×20 grid?
%% Answer: 70600674
p11() ->
    Grid = [[08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08],
            [49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00],
            [81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65],
            [52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91],
            [22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80],
            [24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50],
            [32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70],
            [67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21],
            [24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72],
            [21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95],
            [78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92],
            [16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57],
            [86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58],
            [19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40],
            [04,52,08,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66],
            [88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69],
            [04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36],
            [20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16],
            [20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54],
            [01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48]],
    MaxRows = max_four(Grid, fun(Row, Col) -> [Row, Col+1] end),
    MaxCols = max_four(Grid, fun(Row, Col) -> [Row+1, Col] end),
    MaxDiag1 = max_four(Grid, fun(Row, Col) -> [Row+1, Col+1] end),
    MaxDiag2 = max_four(Grid, fun(Row, Col) -> [Row-1, Col+1] end),
    lists:max([MaxRows, MaxCols, MaxDiag1, MaxDiag2]).

max_four(Grid, NextCoordFun) ->
    Rows = length(Grid),
    Cols = length(hd(Grid)),
    Prods = for(1, Rows, fun(Row) ->
                                for(1, Cols, fun(Col) ->
                                                     prod_four(Grid, Rows, Row,
                                                              Cols, Col,
                                                              NextCoordFun)
                                             end)
                        end),
    lists:max(lists:flatten(Prods)).

prod_four(Grid, Rows, Row, Cols, Col, NextCoordFun) ->
    V0 = grid_val(Grid, Rows, Row, Cols, Col),
    [R1, C1] = NextCoordFun(Row, Col),
    V1 = grid_val(Grid, Rows, R1, Cols, C1),
    [R2, C2] = NextCoordFun(R1, C1),
    V2 = grid_val(Grid, Rows, R2, Cols, C2),
    [R3, C3] = NextCoordFun(R2, C2),
    V3 = grid_val(Grid, Rows, R3, Cols, C3),
    V0 * V1 * V2 * V3.

grid_val(_, Rows, Row, Cols, Col) 
  when Row < 1; Row > Rows; Col < 1; Col > Cols ->
    0;
grid_val(Grid, _, Row, _, Col) -> lists:nth(Col, lists:nth(Row, Grid)).

for(Max, Max, F) -> [F(Max)]; 
for(I, Max, F) -> [F(I)|for(I+1, Max, F)]. 

%% %% What is the value of the first triangle number to have over five hundred
%% %% divisors?
%% %% Triangle number N = N + (N-1) + (N-2) + ... + 0 = (n^2 + n) / 2
%% p12() ->
%%     Min = factorial(500),
%%     _T = triangle_ge(Min),
%%     throw(not_implemented).

%% %% Return triangle number N, where triangle number N
%% %% = N + (N-1) + (N-2) + ... + 1
%% %% = (n^2 + n) / 2
triangle(N) when is_integer(N) ->
    (N * N + N) div 2.

%% prime_factors_of(N) when is_integer(N) ->
%%     [X || X <- primes_upto(trunc(math:sqrt(N)) + 1), N rem X =:= 0].

%% primes_upto(N) ->
%%     primes_upto(N, [], 2).
%% primes_upto(N, L, P) when P >= N ->
%%     L;
%% primes_upto(N, L, P) ->
%%     primes_upto(N, [P|L], puzzle:next_prime(P)).

%% %% Return the lowest triangle number greater than Min.
%% triangle_ge(Min) ->
%%     N = trunc(math:sqrt(Min * 2) / 2),
%%     triangle_ge(Min, triangle(N), N).
%% triangle_ge(Min, Tri, _N) when is_integer(Min), is_integer(Tri), Tri >= Min ->
%%     Tri;
%% triangle_ge(Min, Tri, N) when is_integer(Min), is_integer(Tri), is_integer(N) ->
%%     triangle_ge(Min, Tri + N + 1, N + 1).

%% %% Return number of divisors of N.
%% num_divisors(0) -> 0;
%% num_divisors(1) -> 1;
%% num_divisors(N) when is_integer(N) ->
%%     ndsqrt(N, trunc(math:sqrt(N) + 1), 0) * 2.

%% ndsqrt(_, 0, Acc) -> Acc;
%% ndsqrt(_, 1, Acc) -> Acc + 1;
%% ndsqrt(N, Val, Acc) when is_integer(N) ->
%%     Div = N rem Val,
%%     case Div of
%%         0 -> ndsqrt(N, Val-1, Acc + 1);
%%         _ -> ndsqrt(N, Val-1, Acc)
%%     end.

%% %% Return N factorial.
factorial(0) -> 1;
factorial(1) -> 1;
factorial(N) when is_integer(N) -> N * factorial(N-1).

p13_data() ->
    [37107287533902102798797998220837590246510135740250,
     46376937677490009712648124896970078050417018260538,
     74324986199524741059474233309513058123726617309629,
     91942213363574161572522430563301811072406154908250,
     23067588207539346171171980310421047513778063246676,
     89261670696623633820136378418383684178734361726757,
     28112879812849979408065481931592621691275889832738,
     44274228917432520321923589422876796487670272189318,
     47451445736001306439091167216856844588711603153276,
     70386486105843025439939619828917593665686757934951,
     62176457141856560629502157223196586755079324193331,
     64906352462741904929101432445813822663347944758178,
     92575867718337217661963751590579239728245598838407,
     58203565325359399008402633568948830189458628227828,
     80181199384826282014278194139940567587151170094390,
     35398664372827112653829987240784473053190104293586,
     86515506006295864861532075273371959191420517255829,
     71693888707715466499115593487603532921714970056938,
     54370070576826684624621495650076471787294438377604,
     53282654108756828443191190634694037855217779295145,
     36123272525000296071075082563815656710885258350721,
     45876576172410976447339110607218265236877223636045,
     17423706905851860660448207621209813287860733969412,
     81142660418086830619328460811191061556940512689692,
     51934325451728388641918047049293215058642563049483,
     62467221648435076201727918039944693004732956340691,
     15732444386908125794514089057706229429197107928209,
     55037687525678773091862540744969844508330393682126,
     18336384825330154686196124348767681297534375946515,
     80386287592878490201521685554828717201219257766954,
     78182833757993103614740356856449095527097864797581,
     16726320100436897842553539920931837441497806860984,
     48403098129077791799088218795327364475675590848030,
     87086987551392711854517078544161852424320693150332,
     59959406895756536782107074926966537676326235447210,
     69793950679652694742597709739166693763042633987085,
     41052684708299085211399427365734116182760315001271,
     65378607361501080857009149939512557028198746004375,
     35829035317434717326932123578154982629742552737307,
     94953759765105305946966067683156574377167401875275,
     88902802571733229619176668713819931811048770190271,
     25267680276078003013678680992525463401061632866526,
     36270218540497705585629946580636237993140746255962,
     24074486908231174977792365466257246923322810917141,
     91430288197103288597806669760892938638285025333403,
     34413065578016127815921815005561868836468420090470,
     23053081172816430487623791969842487255036638784583,
     11487696932154902810424020138335124462181441773470,
     63783299490636259666498587618221225225512486764533,
     67720186971698544312419572409913959008952310058822,
     95548255300263520781532296796249481641953868218774,
     76085327132285723110424803456124867697064507995236,
     37774242535411291684276865538926205024910326572967,
     23701913275725675285653248258265463092207058596522,
     29798860272258331913126375147341994889534765745501,
     18495701454879288984856827726077713721403798879715,
     38298203783031473527721580348144513491373226651381,
     34829543829199918180278916522431027392251122869539,
     40957953066405232632538044100059654939159879593635,
     29746152185502371307642255121183693803580388584903,
     41698116222072977186158236678424689157993532961922,
     62467957194401269043877107275048102390895523597457,
     23189706772547915061505504953922979530901129967519,
     86188088225875314529584099251203829009407770775672,
     11306739708304724483816533873502340845647058077308,
     82959174767140363198008187129011875491310547126581,
     97623331044818386269515456334926366572897563400500,
     42846280183517070527831839425882145521227251250327,
     55121603546981200581762165212827652751691296897789,
     32238195734329339946437501907836945765883352399886,
     75506164965184775180738168837861091527357929701337,
     62177842752192623401942399639168044983993173312731,
     32924185707147349566916674687634660915035914677504,
     99518671430235219628894890102423325116913619626622,
     73267460800591547471830798392868535206946944540724,
     76841822524674417161514036427982273348055556214818,
     97142617910342598647204516893989422179826088076852,
     87783646182799346313767754307809363333018982642090,
     10848802521674670883215120185883543223812876952786,
     71329612474782464538636993009049310363619763878039,
     62184073572399794223406235393808339651327408011116,
     66627891981488087797941876876144230030984490851411,
     60661826293682836764744779239180335110989069790714,
     85786944089552990653640447425576083659976645795096,
     66024396409905389607120198219976047599490197230297,
     64913982680032973156037120041377903785566085089252,
     16730939319872750275468906903707539413042652315011,
     94809377245048795150954100921645863754710598436791,
     78639167021187492431995700641917969777599028300699,
     15368713711936614952811305876380278410754449733078,
     40789923115535562561142322423255033685442488917353,
     44889911501440648020369068063960672322193204149535,
     41503128880339536053299340368006977710650566631954,
     81234880673210146739058568557934581403627822703280,
     82616570773948327592232845941706525094512325230608,
     22918802058777319719839450180888072429661980811197,
     77158542502016545090413245809786882778948721859617,
     72107838435069186155435662884062257473692284509516,
     20849603980134001723930671666823555245252804609722,
     53503534226472524250874054075591789781264330331690].

%% The first 10 digits of the sum of all values in p13_data().
p13() ->
    Sum = lists:sum(p13_data()),
    string:substr(integer_to_list(Sum), 1, 10).

%% Number under one million that produces longest Collatz sequence.
p14() ->
    longest_collatz_seq(999999, 0, 0).

longest_collatz_seq(1, LongStart, _) ->
    LongStart;
longest_collatz_seq(N, LongStart, LongLen) ->
    Seq = collatz_seq(N),
    Len = length(Seq),
    Longer = Len > LongLen,
    case Longer of
        true ->
            longest_collatz_seq(N-1, N, Len);
        false ->
            longest_collatz_seq(N-1, LongStart, LongLen)
    end.

collatz_seq(N) ->
    collatz_seq(N, []).

collatz_seq(1, L) ->
    lists:reverse([1|L]);
collatz_seq(N, L) ->
    collatz_seq(next_collatz(N), [N|L]).

next_collatz(N) when (N band 1) =:= 1 ->
    3 * N + 1;
next_collatz(N) ->
    N bsr 1.

%% wrong: it's (40)
%%             (20)
%% (combinatorial, 40 things taken 20 at a time).
p15() ->
    factorial(21).
