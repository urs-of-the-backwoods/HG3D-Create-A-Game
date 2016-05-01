Your First Haskell Game
#######################

You are sitting in front of your computer, a new Haskell book at your side and wondering, what to do next? Sure you expect some setup to be done, to get tools installed, libraries installed, editors installed and more of that. Fortunately this can be done with a single tool, called arriccio.

Arriccio
--------

Arriccio is a specific layer of clay, which is used during the creation of a fresco. To ease up game programming with Haskell, I created a small utility layer, called `fresco`_ and one of the tools out of this toolbox is the arriccio tool, with the command to start it being named "aio". So please download a copy and extract it in a folder in your path. (Being concerned about security, you can also download the `source`_, check it carefully and compile it yourself.)

.. _`fresco`: http://github.com/urs-of-the-backwoods/fresco
.. _`source`: http://github.com/urs-of-the-backwoods/fresco/blob/master/arriccio/main.go

* `Arriccio - Linux Version Download`_
* `Arriccio - Windows Version Download`_
* `Arriccio - OS X Version Download`_

So what is it, we use this tool for? Programming a game in any language involves a lot of different components to play together, for example game libraries, bindings, media and so on. All of those pieces need to go somewhere and normally it is quite some work to just figure out where to place items and to setup everything. Arriccio is doing that for you. It is a bookkeeper managing components and assembling ready to run packages by reading metadata and installing the neccessary files. 

To use it you need to know that components are identified by their url or alternatively by an alias you can freely choose. |HGamer3D| already provides you with a number of those components and arriccio manages them for you. 

Fire up arriccio without parameter and you will get a short helping introduction. If you look closer at the arriccio commands you see that most of them take the url or a name as parameter. Since those url's tend to be long and difficult to remember, you can give them an alias for easier use. Please directly do that with a number of component we will use during the next steps. Enter the following commands to assign the alias names:

.. code-block:: console

	aio alias Stack http://www.hgamer3d.org/component/Stack
	aio alias CreateProject http://www.hgamer3d.org/component/CreateProject
	aio alias Edit http://www.hgamer3d.org/component/Edit
	aio alias Run http://www.hgamer3d.org/component/Run
	aio alias Lua http://www.hgamer3d.org/component/Lua

Let's try now one of those components in isolation for purpose of getting more used to arriccio. Simply issue the following command in a shell:

.. code-block:: console

	aio Lua

What happens? The first time you try to run a component with arriccio it needs to download the component to your machine. So the tool comes back with a question and asks, if you are ok with that. Have a closer look, it gives you a tar.gz filename it is going to download and a signing key. This signing key identifies the person who created the downloaded data, which in this case is me, I signed the components from |HGamer3D| with my key. Arriccio will check if the data has not being messed up somehow by verifying the signature.

If you answer with "yes" a download will appear, and after that the command line of the lua interpreter is displayed. Cool! You can try some lua commands and exit with Ctrl-D. 

Try some other commands of arriccio, for example the license command which gives you licensing information on the component you name. This was a long introduction to one single tool but the following will be much faster.

Setup Haskell
-------------

So the next step is to setup the Haskell environment. We will use the Stack component, which is a bundled version of `The Haskell Tool Stack`_. It might be a good idea to get some background knowledge on the stack tool itself from `The Haskell Tool Stack`_ website. For |HGamer3D| a specific version of the Haskell compiler is useful so you can initialize that with the resolver parameter:

.. code-block:: console

	aio Stack setup --resolver lts-5.8

This short command will download the complete Haskell environemnt and install it on your machine, it will take some while.


Compile Your First Program
--------------------------

Enter a fresh directory and initialize a project there, which is being done with the CreateProject component. This component creates a new project structure for you, by creating some stack boiler plate and some utility commands.

.. code-block:: console

	aio CreateProject

Ok, no let's go to and compile the created program. Simply use the created "build" and "run" commands to build and run the executable.

.. code-block:: console

	./build
	./run

A blue rotating cube in a |HGamer3D| window appears. Congratulations, you just build your first program on your own platform. In case you wonder how it works, check out the "build" and "run" scripts. You see they are working by invoking the stack toolset by leveraging the arriccio mechanisms.


.. _`The Haskell Tool Stack`: http://www.haskellstack.org
.. _`http://www.hgamer3d.org/component/Edit`: http://www.hgamer3d.org/component/Edit
.. _`http://www.hgamer3d.org/component/Stack`: http://www.hgamer3d.org/component/Stack
.. _`http://www.hgamer3d.org/component/Lua`: http://www.hgamer3d.org/component/Lua
.. _`Arriccio - Linux Version Download`: http://www.hgamer3d.org/downloads/aio-amd64-linux-0.1.1.tar.gz
.. _`Arriccio - Windows Version Download`: http://www.hgamer3d.org/downloads/aio-amd64-windows-0.1.1.zip
.. _`Arriccio - OS X Version Download`: http://www.hgamer3d.org/downloads/aio-amd64-darwin-0.1.1.tar.gz


Create Your First Game
----------------------

Now it is up to you, to create a first game. What about an exercise, putting together a short number guessing game? The game should choose a random number between 1 and 100 and the player needs to guess it. The single pieces you would need to make this work are:

Random number creation:

.. code-block:: Haskell

	import System.Random -- makes library available
	i  <- randomRIO (0, 100) :: IO Int -- creates an integer random number between 0 and 100

Input and Output:

.. code-block:: Haskell

	print -- output function
	readLn -- input function

The looping function:

.. code-block:: Haskell

	doWhile :: [IO Bool] -> IO ()
	doWhile (x : xs) = x >>= \r -> if r then doWhile xs else return ()


It is not as easy as it looks like, try yourself before skipping to the next section.


The Game Code
-------------

And here you are, presenting the final number guessing game code in 22 lines of Haskell:

.. code-block :: Haskell
	:linenos:

	module Main where

	import System.Random 

	getNumber :: IO Int -- gets an integer (not error save)
	getNumber = print "enter new guess: " >> readLn

	evaluateNumber :: Int -> Int -> IO Bool  -- returns condition to continue guessing
	evaluateNumber n guess = 
	  if n == guess 
	    then print "congratulations, you won" >> return False
	    else if n > guess 
	            then print "the number is bigger" >> return True
	            else print "the number is smaller" >> return True

	doWhile :: [IO Bool] -> IO ()  -- takes list of action and evaluate them until first false
	doWhile (x : xs) = x >>= \r -> if r then doWhile xs else return ()

	main :: IO ()
	main = 
	  randomRIO (0, 100) >>=                                   -- create a number between 1 and 100
	  \n -> doWhile (repeat (getNumber >>= evaluateNumber n))  -- guessing game loop


This code is written without do notation instead using the bind ">>=" operator. Some more on this operator will be explained in the next section. Did you see the "\\x ->" notation after the bind operator? This is needed since at the right side of the bind we consume a value and we need to name this value. This is being done by introducing an anonymous function and the "\\x -> " is part of the syntax for anonymous functions.

Have a look also at the type of doWhile taking a list of actions and performing them step by step. Where in the code
is the list composed? It is actually the "repeat" function taking something and building a list of it by repeating it indefinetly. The repeat function in this case takes an action itself, so it composes a list of actions and then later in the doWhile loop those actions are performed.
This only works without blowing up the stack, since Haskell is a lazy language, which builds the list itself only to the degree where it is needed.

Now as a last one for this page, start your editor copy the code into the game.hs file, compile again and run it!

.. code-block:: console

	aio start Edit game.hs
	- paste the above program in the file and save -
	./build
	./run


.. include:: GeneralInclusions
