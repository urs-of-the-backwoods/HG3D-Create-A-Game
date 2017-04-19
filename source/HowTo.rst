HowTo's
#######

The Command Line
----------------

The phrase "command line" is referring to a program used to input commands directly by typing them on a single line and getting a response back in 
textual format. This also is known as a "shell". In Windows the command to start the command line is "cmd.exe". In linux it is "bash". The shell
typically has commands to list the contents of the current directory, to copy or rename files and to navigate the folder structure. 

**Different Commands in Linux bash and Windows cmd.exe**

======================================= ================ ================
Command Purpose                         Cmd in Linux     Cmd in Windows
======================================= ================ ================
List contents of directory              ls               dir
Copy file or files                      cp               copy
Rename file                             mv               rename
change directory, navigate hierarchy    cd               cd
======================================= ================ ================


Learning Haskell
----------------

The road to Haskell can be long and windy. To help, there is very good introductory material and excellent "top-notch" material from research available in books and on the internet. Somehow the middle ground is missing, though and it is a question how to best cross the chasm from beginner to master level.

I'm also struggling with that and I worked out the following plan, to advance my own skills. Let me share this, although it migth be not the best plan on earth it is at least a starting point.

**Start**

I started with a textbook and learned and practiced the basic syntax. The one I took is generally seen as a good reference introductory textbook on Haskell, it is *Programming in Haskell* from Graham Hutton [Hutton]_. It should get you pretty far for all your needs on the basics and a little bit further.

I had some hard times getting a first understanding on the basic typeclasses, like Functor, Monad and so on. There is a great resource for that, which helped me a lot and still does. I'm referring to *Learn You A Haskell for Great Good* from Miran Lipovaca [LYAHGG]_. It is also available free on the Web and very popular and it introduces nicely the functional way of thinking.

**Practice**

Then I practiced, mostly in the context of game programming and FRP and similar topics. I detected that the next step is a practical knowledge on *do notation* and *Monads*. After mastering this step everyhting was in place to write sequential programs, with functional areas interspersed for computation of values. I'm still mostly programming on this level and it is already a fruitful stage where programs are nicely structured and useful things are happening.

.. note:: Since |HGamer3D| is written by a novice, the API is not highly complex but instead quite simple. With a "Practice" knowledge level you should be able to use it.

**Advance**

Now comes the issue and I stress it is probably a real one. Haskell has tons of great libraries available to use. Some of them exhibit a more procedural and easy to use interface. Most of them actually exhibit a good usable interface, but they are only digestable if you are at the expert level. But advancing now to the final stages of mastering Haskell is tricky. I'm still fighting here a fierce battle, good tutorials are not so easy to find and understanding the material seems to be difficult. 

In this situation I found an interesting blog from Gabriel Gonzales [GGAHT]_ who has given me some hope. He put together a number of links to good tutorials of advanced topics. Based on this my personal plan now looks like that:

.. _`post`: http://www.haskellforall.com/2014/03/introductions-to-advanced-haskell-topics.html

- master typeclasses (just the basic functor contains a lot of abstraction) and understand it.
- go step by step and learn the content of the tutorials in the post mentioned above.

This should bring me pretty far, I hope. I will tell you here on the progress, if there is some.


do Notation
-----------

This short page should get everybody started on actions and do notation, since those are the most basic topics you need to be able to use on a practical level. 

**Haskell is non-strict**

The order of evaluation of an expression in Haskell is non-strict. This means every expression is just evaluated to the depth to which results are really needed. All other evaluations are skipped and stored for later evaluation. You can make this apparent by starting the interpreter and trying a couple of expressions with the ``undefined`` value.

**undefined**

The undefined value can mimic every type and it is just the so-called *bottom* of that type. That means it is the non-valid value for this type. If it is being evaluated it generates a runtime error. This makes it a suitable candidate to test if evaluation happens or not at a certain place in an expression.

**non-strict evaluation examples**

Kickstart the ghci interpreter by typing ``./repl`` and evaluate the following examples:

.. code-block :: haskell

	let l = [0,1,2,3,undefined,5,6]     -- no output, no error, l is being assigned
	l                                   -- prints [0,1,2,3 and then an error
	l !! 1                              -- output: 1
	l !! 4                              -- gives an error
	l !! 5                              -- output: 5 (although !! needs to traverse the undefined element)
	let x = l !! 4                      -- no output (undefined is NOT evaluated, though assigned to x)
	x                                   -- gives an error

The results are remarkable. Evaluation only happens, if the output is needed. Even giving the element 4 of the list another name is not generating an error, since this still does not mean this element is evaluated. Only printing it evaluates it.

In general pure code in Haskell is only evaluated at the time the value is needed, for example to print it. This is a very useful property in functional code because it enables the definition of recursive data structures.

You might ask, is this a problem for the unexperienced programmer? It is mostly not. Pure code in Haskell is referentially transparent, meaning each invocation of a pure expression always yields the same value. So it is not important, when the code is evaluated, at least as longs as you get a result, when you need it, which is of course guaranteed. Sometimes there are efficiency considerations, but in case your program is slow or consumes too much memory you still can tackle the problem then. Don't be worried too much about it.

.. note:: In pure code you do not know when sub-expressions are evaluated and you do not care!


**IO and Actions**

Sometimes of course it is important to know when things are happening, one example is input and output. If you get a key-press in an interactive game which triggers a bullet, you would like to have the bullet on the screen, the same moment, the user presses the key and not waiting until, let's say at the end of the game the final result is computed as a scoring between different players.

So there are two worlds, which need to be separated, the world of pure functions and values which are computed at the time the result is needed and the world of impure code, reacting to IO (input/output) and changing things in the real world. Only pure functions are referentially transparent, impure functions are not, they can give different results at different times when called with the same parameters.

IO and impure code in general is integrated in Haskell by means of the ``IO`` type. Anything of type ``IO a`` - with "a" being a type parameter - is considered an action resulting in a value of type "a". Actions are executable entities and they modify the real world or getting input from the real world. Actions can be chained and in this case they are executed in sequence. This is neccessary, since being not referentially transparent the execution order matters and therefore the sequence needs to be well defined.

Furthermore, each Haskell program is just a sequence of those actions, starting at the ``main`` function, which has the type ``IO a``. A Haskell program can be seen as a pure function, generating a sequence of actions, which is then executed in ``main``. Or, since creating the sequence still is a pure operation which is non-strict, a Haskell programm evaluates the executable actions step by step as given by the computed sequence and executes them.

.. note:: the IO type constructor is a Monad. But don't be afraid, you do not need to know monads and all the details around it to get going.

**Action Chaining**

How are actions chained then? Pretty easy, there are two operators available, which are helping with that. The famous ``bind`` operator ``>>=`` and the ``>>`` operator. ``>>=`` takes two actions and feeds the result of the first action as a parameter to the second one. The ``>>`` operator discards the result of the first action and simply executes both actions in sequence.

Using the operators above is resulting in a lot of nested lambda expressions, and specially inside the interpreter it is complicated to use, since intermediate values are not being bound. Do notation is giving us a more convenient way. 

**do Notation**

Do notation is provided by the Haskell compiler and interpreter and it is just syntactic sugar for using the bind and sequence operators directly. It is immensly useful but there are some pitfalls. Let's explain the main elements in an example.


.. literalinclude:: ../../HGamer3D/examples/SpaceInvaders.hs
    :start-after: -- HGamer3D website, actions and do notation, first example
    :end-before: -- end of website text
    :language: Haskell

You need to memorize the following rules around do notation:

- Each line is either an action and of type "IO a" or a let binding.
- Actions are executed, one by one in sequence.
- Lines with a left arrow ``<-`` de-sugar into a bind operator so that the name on the left side of the arrow is bound with the result of executing the action on the right hand side. The bound value is valid for the remaining part of the do block.
- Lines without a left arrow are actions which are executed and whose value is discarded.
- The result of the last action in the do block is the result of the overall do block.
- ``return`` lifts a pure value into a value of type "IO a". It is needed at the end since the last statement of a do block needs to be an action.
- Finally, let statements are not actions but they just bind a new name to a value. The name again is valid for the remaining part of the do block. 

Following those rules, you should not get into trouble with do notation but on the contrary you will master sequential code and are able to write useful programs. Be careful to grasp the difference between a line with an action and a let binding. 

.. note:: The example uses the IO monad as base monad of the do block since this page is about actions, which are of type "IO a". Do notation can be used on any monad but that is not of interest here. (The compiler infers the type of the monad automatically, based on the expressions within the do block).


**If Statements**

As a last point on this page, there is one more tip to follow with regards to do notation. It is simple to memorize and follow but if not done, often drives people crazy, especially beginners.

If statements and do notation interact in strange ways. Therefore another tip, if you need to write an if statement within a do loop, follow the indentation layout as is shown below:

.. code-block:: Haskell

	main = do
	  s <- readline
	  if (s == "go")
	    then do 
	      something 
	      here
	    else return ()

Specifically, do NOT put the "then" on the same line as the "if" and indent "then" and "else" to the same level. This looks good and works. See https://prime.haskell.org/wiki/DoAndIfThenElse for details.




**References**

.. [Hutton] Graham Hutton, "Programming in Haskell", January 2007, Cambridge University Press, ISBN: 0521692695.

.. [LYAHGG] Miran Lipovaĉa, "Learn You a Haskell for Great Good, A Beginners Guide", April 2011, No Starch Press, ISBN: 978-1-59327-283-8. Also on the web: http://learnyouahaskell.com.

.. [GGAHT] Gabriel Gonzales, "Introductions to advanced Haskell topics", March 2014, http://www.haskellforall.com/2014/03/introductions-to-advanced-haskell-topics.html.


.. include:: GeneralInclusions
