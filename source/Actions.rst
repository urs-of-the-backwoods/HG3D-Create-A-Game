Actions and do Notation
#######################

Actions and the IO t type
-------------------------

Actions are the ingredients of every game. Due to the very functional nature of Haskell actions are handled differently and it is good to learn how to program with actions, to be able to get the first baby steps done without too much frustration. So we are up for a short introduction into actions and do notation.

You probably heard about Haskell being a "lazy" language, right. What does this mean, actually? It does mean that each expression is evaluated only to the degree to which the result is needed or being asked for. Imagine a function with two parameters and you are calling this function with two expressions which should give you back the current values of the parameters. Different as in most "normal" programming languages
those expressions are not evaluated before the function is called. Instead the expression is shipped to the inside of the function and only evaluated once it is really needed to compute the resulting value. 

Thinking about that, how should it work? Also the expressions inside the functions are build from other functions and if we are then only shipping variable into them unevaluated, when will the evaluation start, finally? What causes actually an expression to be computed instead just stored for future computation?

The answer is kind of astonishing. There is a special type, the "IO t" type which cares for that. Expressions of type "IO t" are actually executed in the main program flow.

Have a look at the type of the main function in a Haskell program, it is of type "IO t". The main function carries one expression of type "IO t" and this statement is executed! Execution means, that results from elements of this expression are evaluated as long as needed for the resulting type of t of IO t. The "IO t" type effectively transforms the expression into a real statement.

The next question directly follows, if there is one expression evaluated, how can I evaluate a series of expressions in sequence? The answer is that the "IO t" type has special characteristics. Due to a mechanism in Haskell called typeclasses types can own operations on the values they describe. The "IO t" type is equipped with two famous operators in Haskell, the bind and return operators.

In case of "IO t" if you combine two actions with those operators what happens is:

*The bind operator (>>=)*

This operator builds a new action from two existing ones. In case the new action gets executed in main, the working of the operator is that first the left expression is evaluated, then the result is fed into the right expression. The right expression is then also evaluated and the final result is the result of the overall expression. This only happens during execution. Beforehand, simply both actions are sequenced. So you see how evaluation bubbles down from the main program into each subroutine of type "IO t" by the mechanism of the bind operator for IO. Basically what happens is that in Haskell you compose actions with functional means and finally hand over one action to the main function, which executes them. This is somewhat hidden behind the do notation I explain below, but it is exactly what is happening.

You will also see the bind operator for other types. The mechanism explained here is how the bind operator works for the "IO t" type. For other types, bind have other implementations. Usually it is always some kind of sequence involved but not neccessarily it is a sequence of actions being executed in main.

*The return operator*

This operator takes a value and frames it in a "IO t" type. So the type of "return 2" is "IO Int". This is important, there is no
other way to put values into this type and also you should know, that there is no way to get values out of this "IO t" type but
by executing an action within the chain of actions build up from the main program into the last subroutine.


*IO means Input / Output*

How is input output carried out. It is pretty clear that exchange of data with the outer world over the keyboard, the network stack or other interfaces can only be done during an execution. So it happens during an action in Haskell. All IO actions carry the "IO t" type. Initially this has given the name to this kind of type.

This all makes a lot of sense as you will discover and it has tons of benefits for the structure of programs. For now just remember that in Haskell you need to differentiate between the pure functions, which can be combined in an almost mathematical sense and the world of actions which finally drive execution and also handle IO. Usually a Haskell programmer tries to code the most logic and data processing in pure code and uses the IO mechanism for structuring program execution flow.


do notation
-----------

Writing bigger chunks of code with only the bind operator does not look nice and needs a lot of memorizing the different layers of indenting. Fortunately there is do notation, which helps with that. do Notation transforms successive bind operators to a block of sequenced statements. It is important nevertheless to keep the meaning in your head and not confuse statements which gets executed from definitions of computations, which are not executed immediately.

In the explanations below we assume a do block for "IO t" although do blocks can be used also for other types with a bind operator.

To work successfully with Haskell, you need to memorize a number of simple rules and concepts for do notation. This will enable you to write programs more than anything else.

*Three different types of lines in do blocks*

There are three different types of lines in do blocks, you need to differentiate carefully, since they have different meaning. If we take a look at the code snippet below you will recognize them. There are lines with a "let" binding (2, 8), lines with a left arrow in it "x <- y" (9, 10, 11) and lines with neither a let nor a left arrow (3, 4).

.. literalinclude:: ../../HGamer3D/examples/Cuboid2.hs
    :start-after: -- draw the static frame around the cubes
    :end-before: setPos er fp = setC er ctPosition (f2pos fp)
    :language: Haskell
    :linenos:

The lines with a let introduce a new binding to a variable. On the left of the equal sign after the let you have a variable name and on the right hand side you have a pure function, which gives back the value of this variable. This simply introduces a naming for something and the right hand side expression is not of the "IO t" type, but a pure functional expression. The line is also not executed directly but only when the value is needed inside one of the following IO expressions. (There might be cases where you have a "IO t" type on the right hand side of the let expression, what is that? It is a new naming for an action, without executing it, but keeping it for further use, kind of action computing and renaming.)

A line with a "<-" left arrow resembles an IO action. The type on the right hand side of the arrow is of type "IO t", and the type of the left hand side is of type t. So you can read such a line as: execute the action on the right hand side and give me back the value for t, which then is fed into the next actions within the do block. Behind the scenes this is implemented by a bind operator. The left side name can be used in further actions and this actually gives the power to do notation. 

Think about it, the fact that you can combine results from actions inside an action do block is the single element which gives the whole power to do notation! You can combine actions and re-use the IO values within the bigger action which is given by the surrouding do block. This is one of the most used basic forms of composition used withing Haskell. And all usabel in do notation.

A line with neither a let nor a left arrow is a single IO action, which output is not used or discarded. It's type also needs to be of type "IO t", a pure expression does not work solely within a do block (only on the right side of a let statement, see case one).

*Final line of a do block*

The final line of a do block needs to be a blank IO action, it cannot be a line with a left arrow or a let binding.

*return*

You can get a blank IO action from a pure value by using "return value", so you can use this in the last statement of a do block. But be aware this is not a return it is a function which transforms a value into an "IO value" type. Otherwise the last IO action on a do block is given the value to the overall do block.

*if then else*

If then else statements interact in strange ways with do notation. Be aware of that, therefore you should put the "if then else" either on one line, or use the indenting, as shown below:

.. literalinclude:: ../../HGamer3D/examples/Cuboid2.hs
    :start-after: keys <- updateVar' varKeysUp (\ks -> ([], ks))
    :end-before: processMove pos m = case steps pos m level of
    :language: Haskell

Put "else" and "then" on separate lines, both indented from the "if" by the same level. This works flawlessly in all situations and it gives you also a good view into the meaning of the code.

With those basic elements you are able to code working do notation in all situations and this should give you a good background for further Haskell programs. Finally a short note on other types of do blocks.

Other monads and do notation
----------------------------

You know it already, do you, the bind and return operators together with some rules on the properties of the data they are used on form something which is called a monad. Important here, do notation can be used on any monad and the compiler infers the type of the do block the same way it infers other types. An eye opener is the following code, can you guess, what value it will give back? Try it!

.. code-block :: Haskell

	do
		n <- [1, 2]
		ch <- ['a', 'b']
		return (n, ch)

In this case the list monad concept will help you to understand this. Have a look at "Learn You A Haskell for Great Good!"  from Miran Lipovaĉa [LYAHGG]_ for more information on this.

