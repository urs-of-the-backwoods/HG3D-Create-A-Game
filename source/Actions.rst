Actions and do Notation
#######################

This short page should get everybody started on actions and do notation, since those are the most basic topics you need to be able to use on a practical level. 

Haskell is non-strict
---------------------

The order of evaluation of an expression in Haskell is non-strict. This means every expression is just evaluated to the depth to which results are really needed. All other evaluations are skipped and stored for later evaluation. You can make this visibl by starting the interpreter and trying a couple of expressions with the ``undefined`` value.

**undefined**

The undefined value can mimic every type and it is just the so-called *bottom* of that type. That means it is the non-valid value for this type. If it is being evaluated it generates a runtime error. This makes it a suitable candidate to test if evaluation happens or not at a certain place in an expression.

**non-strict evaluation examples**

Kickstart the ghci interpreter by typing ``./repl`` and evaluate the following examples:

.. code :: haskell

	let l = [0,1,2,3,undefined,5,6]     -- no output, no error, l is being assigned
	l                                   -- prints [0,1,2,3 and then an error
	l !! 1                              -- output: 1
	l !! 4                              -- gives an error
	l !! 5                              -- output: 5 (although !! needs to traverse the undefined element)
	let x = l !! 5                      -- no output (undefined is NOT evaluated, though assigned to x)
	x                                   -- gives an error

The results are remarkable. Evaluation only happens, if the output is needed. Even giving the element 4 of the list another name is not generating an error, since this still does not mean this element is evaluated. Only printing it evaluates it.

In general pure code in Haskell is only evaluated at the time the value is needed, for example to print it. This is a very useful property in functional code because it enables the definition of recursive data structures.

You might ask, is this a problem for the unexperienced programmer? It is mostly not. Pure code in Haskell is referentially transparent, meaning each invocation of a pure expression always yields the same value. So it is not important, when the code is evaluated, at least as longs as you get a result, when you need it, which is of course guaranteed. Sometimes there are efficiency considerations, but in case your program is slow or consumes too much memory you still can tackle the problem then. Don't be worried too much about it.

.. note:: In pure code you do not know when sub-expressions are evaluated and you do not care!


IO and Actions
--------------

Sometimes of course it is important to know when things are happening, one example is input and output. If you get a key-press in an interactive game which triggers a bullet, you would like to have the bullet on the screen, the same moment, the user presses the key and not waiting until, let's say at the end of the game the final result is computed as a scoring between different players.

So there are two worlds, which need to be separated, the world of pure functions and values which are computed at the time the result is needed and the world of impure code, reacting to IO (input/output) and changing things in the real world. Only pure functions are referentially transparent, impure functions are not, they can give different results at different times when called with the same parameters.

IO and impure code in general is integrated in Haskell by means of the ``IO`` type. Anything of type ``IO a`` - with "a" being a type parameter - is considered an action resulting in a value of type "a". Actions are executable entities and they modify the real world or getting input from the real world. Actions can be chained and in this case they are executed in sequence. This is neccessary, since being not referentially transparent the execution order matters and therefore the sequence needs to be well defined.

Furthermore, each Haskell program is just a sequence of those actions, starting at the ``main`` function, which has the type ``IO a``. A Haskell program can be seen as a pure function, generating a sequence of actions, which is then executed in ``main``. Or, since creating the sequence still is a pure operation which is non-strict, a Haskell programm evaluates the executable actions step by step as given by the computed sequence and executes them.

.. note:: the IO type constructor is a Monad. But don't be afraid, you do not need to know monads and all the details around it to get going.

Action Chaining
---------------

How are actions chained then? Pretty easy, there are two operators available, which are helping with that. The famous ``bind`` operator ``>>=`` and the ``>>`` operator. ``>>=`` takes two actions and feeds the result of the first action as a parameter to the second one. The ``>>`` operator discards the result of the first action and simply executes both actions in sequence.

Using the operators above is resulting in a lot of nested lambda expressions, and specially inside the interpreter it is complicated to use, since intermediate values are not being bound. Do notation is giving us a more convenient way. 

do Notation
-----------

Do notation is provided by the Haskell compiler and interpreter and it is just syntactic sugar for using the bind and sequence operators directly. It is immensly useful but there are some pitfalls. Let's explain the main elements in an example.

.. code:: haskell

	main = do 
	    let loopTime = msecT 20
	    (eGeo, hg3d) <- start
	    forkIO $ rotateCube eGeo
	    loopHG3D hg3d loopTime (return True) -- allow close on windows click
	    return ()

You need to memorize the following rules around do notation:

- Each line is either an action and of type "IO a" or a let binding.
- Actions are executed, one by one in sequence.
- Lines with a left arrow ``<-`` de-sugar into a bind operator where the name on the left side of the operator is bound with the result of executing the action on the right hand side. The bound value is valid for the remaining part of the do block.
- Lines without a left arrow are actions which are executed and whose value is discarded.
- The result of the last action in the do block is the result of the overall do block.
- ``return`` lifts a pure value into a value of type "IO a". It is needed at the end since the last statement of a do block needs to be an action.
- Finally, let statements are not actions but they just bind a new name to a value. The name again is valid for the remaining part of the do block. 

Following those rules, you should not get into trouble with do notation but on the contrary you will master sequential code and are able to write useful programs. Be careful to grasp the difference between a line with an action and a let binding. 

.. note:: The example uses the IO monad as base monad of the do block since this page is about actions, which are of type "IO a". Do notation can be used on any monad but that is not of interest here. (The compiler infers the type of the monad automatically, based on the expressions within the do block).


If Statements
-------------

As a last point on this page, there is one more tip to follow with regards to do notation. It is simple to memorize and follow but if not done, often drives people crazy, especially beginners.

If statements and do notation interact in strange ways. Therefore another tip, if you need to write an if statement within a do loop, follow the indentation layout as is shown below:

.. code:: haskell

	main = do
	  s <- readline
	  if (s == "go")
	    then do 
	      something 
	      here
	    else return ()

Specifically, do NOT put the "then" on the same line as the "if" and indent "then" and "else" to the same level. This looks good and works. See https://prime.haskell.org/wiki/DoAndIfThenElse for details.


.. include:: GeneralInclusions
