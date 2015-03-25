NAME: Akshay Kumar
NETID: aktwo@
PRECEPT: P02

=======================================================================
Optional: Explain any difficulties you had with this assignment or any
parts that are incomplete.  Suggestions on how this assignment could
be improved to help students learn more are welcome:

I thought this assignment was really useful! I felt like I learned
a ton of OCAML in the process :)

=======================================================================
For each of the following questions, 

(A) give the answer 

(B) give the shell command (or commands) you used to answer the
question and in a sentence, explain why your shell command gives you
helpful data.  (FYI: using fewer commands and piping is more elegant
than using more commands and will be given more credit).

Use the data given in the files G.txt, PG.txt, PG-13.txt, R.txt for
the set of top-grossing films in each rating category (G, PG, PG-13
and R).  Use the data in the file alltime.txt for the alltime
inflation-adjusted returns.

1.  Adjusting for inflation, what is the top-grossing film of all time
(use the alltime.txt data)?

(A) Gone with the Wind

(B) ./boxoffice -sort-gross < alltime.txt | ./boxoffice -take 1

2. What is the 50th ranked R film by gross on the list?

(A) The Silence of the Lambs

(B) ./boxoffice -sort-gross < R.txt | ./boxoffice -drop 49 | ./boxoffice -take 1

3. Suppose you had a chance to make 1 film with a top director and the
director was so good you were guaranteed that whatever film you made
would be in the top 5 grossing films in its ratings category (G, PG,
PG-13, R) --- and equally likely to be ranked 1, 2, 3, 4, or 5.  What
rating (G, PG, PG-13, R) would you choose to give your film if you
wanted to make the most money?

(A) PG-13

(B) ./boxoffice -sort-gross < G.txt | ./boxoffice -take 5 
    | ./boxoffice -average
    ./boxoffice	-sort-gross < PG.txt | ./boxoffice -take 5 
    | ./boxoffice -average
    ./boxoffice	-sort-gross < PG-13.txt | ./boxoffice -take 5 
    | ./boxoffice -average
    ./boxoffice	-sort-gross < R.txt | ./boxoffice -take 5 
    | ./boxoffice -average

4. Taking inflation in to account, would you have preferred to make
money off of blockbusters in the 70s or in the 80s?

(A) 70s

(B) ./boxoffice -decade 70 < alltime.txt | ./boxoffice -average
    ./boxoffice -decade 80 < alltime.txt | ./boxoffice -average

5. Taking inflation in to account, which studio made the most
money off of blockbusters in the 60s?

(A) Disney (represented as Dis.)

(B) ./boxoffice -decade 60 < alltime.txt | ./boxoffice -by-studio 
    | ./boxoffice -sort-studio | ./boxoffice -take 1
