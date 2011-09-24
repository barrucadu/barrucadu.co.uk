As I had my computing exam today I decided to prepare and practice my Pascal by writing something several orders of magnitude more complex than what I would have to do in the exam --- a Towers of Hanoi solver. I'm sure you're familiar with Towers of Hanoi, it's a simple puzzle game in which you have three pegs, one of which has an array of various-sized disks on. You must move the disks from the first to the third peg, only moving one at a time, and you can't put a big one on a small one.

When you first see it, the game looks a lot more complex than it is, in fact there is a simple algorithm to be followed to solve any given puzzle in the minimum number of moves possible, which is **2^n - 1**, where **n** is the number of disks.

Simply put, the algorithm is this:

 * Move the smallest disk one peg left if there is an even number of disks or one peg right of there is an odd number, wrapping around to the other side if you're at the end.
 * Make the only available move that does not involve the smallest disk.
 * Repeat until the puzzle is solved.

This can be implemented fairly easily — the thing I don't like is the second step. As this algorithm is derived from my playing of the game, and so not read anywhere by me, there is probably a much clearer version that doesn't involve finding moves by inspection floating around online somewhere. However, I digress.

So, I set about implementing this in Pascal. It probably helped that I have already done this in Python (though I can't find the Python version on my computer…) . My implementation saves the solution to a text file and uses a randomly-sized tower, as I was focusing more on testing my Pascal than writing a solver.

Anyway, firstly we need somewhere to store information about the current state of our puzzle. Using a type, a constant, and a few variables, we have all the storage space we need:

    Program Hanoi;
    
    { A Towers of Hanoi solver:
      * Generates a tower of random height between 3 and 10 blocks
      * Saves solution to "soln.txt"
      * Prints info to STDOUT }
    
    Const
       SOLNFILE  = 'soln.txt';
    
    Type
       Tower = Array [0..10] of Integer;
    
    Var 
       disks : Integer;
       moves : Integer;
       ltr : Boolean;

The **Tower** type is used to store information about the state of one particular tower, and will be used later on in the solve procedure. The **disks** variable will contain the number of disks in the puzzle, the **moves** variable the number of moves, and the **ltr** variable whether we are moving from the left to the right, or the right to the left. Admittedly, that variable is rather useless, as I later on use an integer variable for the same purpose.

Now that we can store information about our randomly-generated puzzle, it's probably best to tell the user what it is we're solving.

    Function pow(base, exponent : Integer) : Integer; { Raise base to power exponent }
    begin
       pow := Round(exp(exponent * ln(base)));
    end; { pow }
    
    Procedure printinfo(); { Print information about the puzzle generated }
    begin
       writeln('Towers of Hanoi Solver.');
       writeln('    ', disks, ' disks.');
       writeln('    ', moves, ' moves required.');
    
       if ltr then
          writeln('    L->R move direction.')
       else
          writeln('    R->L move direction.')
    end; { printinfo }

I couldn't remember how to raise a number to a power in Pascal, so I wrote my own function using exponentials and logs. With that function, we can calculate the number of moves (which is actually done in the main bit) and print information to the screen. I promise that all of these code snippets will make sense if you look at them together :P

Anyway, we have information-printing and variables, what else could we possibly need? Oh, yes, the solver itself. This consists of a few parts - a solve procedure, which calls a findmove procedure, which calls a makemove procedure. There's a bit more hiding away in there, too.

Such as, the initttower procedure.

    Procedure inittower(Var tow : Tower; ndisks : Integer); { Initialises the tower variables }
    var
       count : Integer;
    begin
       for count := 0 to 10 do
       begin
          if ndisks - count >= 0 then
           tow[count] := ndisks - count
          else
           tow[count] := 0
       end;
    end; { inittower }

A very simple procedure, I'm sure you can see instantly what it does. It adds disks to a tower, starting from 10 and going down, filling the rest of the space with zeros. This is called for the three tower objects which keep trac of the disks.

    Procedure makemove(Var towa, towb : Tower); { Move the top disk from one tower to another }
    var
    i, j: Integer;
    begin
       { Find the indexes of the last nonzero value in the towers }
       i := 0;
       j := 0;
       
       while towa[i + 1] <> 0 do
          i := i + 1;
    
       while towb[j + 1] <> 0 do
          j := j + 1;
    
       towb[j + 1] := towa[i];
       towa[i] := 0;
    end; { makemove }

Another fairly simple procedure, this finds the index of the first disks on two towers, and moves the last disk from **towa** (towa the tower, I like that) to the top of **towb**. The most complex functions, perhaps, in the program are for finding moves.

    Procedure getmoveorder(towa, towb, diska, diskb : Integer; Var towerbegin, towerend : Integer); { Get the move between two towers }
    begin
       if (diska < diskb) or (diskb = 0) then
       begin
          towerbegin := towa;
          towerend   := towb;
       end
       else
       begin
          towerbegin := towb;
          towerend   := towa;
       end;
    end; { getmoveorder }
    
    Procedure findmove(Var towa, towb, towc: Tower; Var towerbegin, towerend : Integer; smallest : Boolean); { Find the next move that either does or does not involve moving the smallest disk }
    var
       i, j, k : Integer;
       dir   : Integer;
    begin
       { Find the indexes of the last nonzero value in the towers }
       i := 0;
       j := 0;
       k := 0;
       
       while towa[i + 1] <> 0 do
          i := i + 1;
       
       while towb[j + 1] <> 0 do
          j := j + 1;
       
       while towc[k + 1] <> 0 do
          k := k + 1;
       
       if smallest then
       begin
          if ltr then
             dir := 1
          else
             dir := -1;
          
          if towa[i] = 1 then
             towerbegin := 0
          else
             if towb[j] = 1 then
                towerbegin := 1
             else
                towerbegin := 2;
          
          towerend := towerbegin + dir;
          
          if towerend = 3 then
             towerend := 0
          else
             if towerend = -1 then
                towerend := 2;
       end
       else
       begin
          if towa[i] = 1 then
             getmoveorder(1, 2, towb[j], towc[k], towerbegin, towerend)
          else
             if towb[j] = 1 then
                getmoveorder(0, 2, towa[i], towc[k], towerbegin, towerend)
             else
                getmoveorder(0, 1, towa[i], towb[j], towerbegin, towerend);
       end;
    end; { findmove }

The **findmove** procedure is called, with references to the three tower objects, begin and end integers, and a boolean indicating whether to move the smallest or not.

It starts off by finding the top disk on every tower, and then comparing their sizes. If the smallest is to be moved, it identifies the next tower to move the disk to, which is fairly simple, and returns those tower numbers. However, if the smallest is not to be moved, the process is more complex. FIrstly, it identifies the two towers which do not contain the smallest disk, and then calls the **getmoveorder** function to return the start and end towers.

The **getmoveorder** procedure is very simple, it checks the sizes of the top disks on the two towers passed to it and returns the tower containing the smaller disk as the start tower, and the other as the end tower. Looking relatively simple compared to all that, is the **solve** procedure.

    Procedure solve(); { Solve the puzzle }
    var
       fh         : TextFile;
       move       : Integer;
       towa       : Tower;
       towb       : Tower;
       towc       : Tower;
       towerbegin : Integer;
       towerend   : Integer;
    begin
       { Open solution file for writing }
       Assign(fh, SOLNFILE);
       Rewrite(fh);
       
       move := 0;
       writeln(fh, move:3, ': Build a tower of ', disks, ' disks.');
       
       { Keep track of which tower has what }
       inittower(towa, disks);
       inittower(towb, 0);
       inittower(towc, 0);
       
       for move := 1 to moves do
       begin
          if (move - 1) mod 2 = 0 then
             { Move smallest disk }
             findmove(towa, towb, towc, towerbegin, towerend, True)
          else
             { Move other disk }
             findmove(towa, towb, towc, towerbegin, towerend, False);
          
          if towerbegin = 0 then
          begin
             if towerend = 1 then
                makemove(towa, towb)
             else
                makemove(towa, towc)
          end
          else
             if towerbegin = 1 then
             begin
                if towerend = 0 then
                   makemove(towb, towa)
                else
                   makemove(towb, towc)
             end
             else
             begin
                if towerend = 1 then
                   makemove(towc, towb)
                else
                   makemove(towc, towa)
             end;
          
          writeln(fh, move:3, ': Move disk from tower ', towerbegin, ' to tower ', towerend, '.');
       end;
       
       close(fh);
    end; { solve }

Whilst not particularly nice looking when you first see it, you'll soon notice that almost all of the functionality is handled by the other procedures, making the **solve** procedure little more than a loop and some if statements. Firstly, it writes what I call the *initial move* to the output text file, which is the instruction to build tow tower. It then initialises the towers with all the disks on the first tower, and nothing on the other two, using the **inittower** procedure.

From there, solving the puzzle happens. It loops until the maximum number of moves has been reached and, on every 'even' move moves the smallest disk, and on every 'odd' move moves another disk. The if statements handing the **makemove** calls aren't particularly nice, however, and there is probably a better way of doing that. Also, the solution is entered to the text file as it goes.

This leaves only the main section.

    begin
       { Do some set up }
       Randomize;
       disks := 3 + Random(8); { Generate random number between 3 and 10 }
       
       moves := pow(2, disks) - 1; { Figure out the number of moves required, and the move direction. }
       ltr := (disks mod 2 = 0);
    
       { Information about the puzzle generated }
       printinfo;
       
       { Now solve }
       solve;
    end.

Simple — picks a random number of disks between 3 and 10, calculates the number of moves, figures out the direction, shows some information, and solves it.

Put all those bits of code together and you have a Towers of Hanoi solver though, admittedly, for it to be useful you'd have to make some changes — such as allowing the user to enter the number of disks, or even using a command line parameter. Hey, you could even have it print an ASCII representation of the towers at every step, shouldn't be too hard.
