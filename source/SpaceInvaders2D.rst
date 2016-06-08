Space Invaders 2D
#################

Now finally, let's head towards a bigger game with key input, sound and a challenging game play. I would like you to create a space invaders game in 2D with text graphics. Below you will find all the ingredients, you will need to program it. But first we also need to adapt the cabal file a little to adjust for the added dependencies. 

Preparation
-----------


To start, please create a new directory, enter it and create a project there and start the editor with the commands:

.. code-block:: console

	aio CreateProject
	aio start Edit game.hs 


And paste the following minimal runtime information into the haskell file:


.. code-block:: Haskell

	{-# LANGUAGE OverloadedStrings #-}
	module Main where
	import HGamer3D

	import Control.Concurrent
	import Control.Monad
	import qualified Data.Map as M
	import qualified Data.Text as T
	import Data.Maybe

	main = do
	  hg3d <- configureHG3D	
	  loopHG3D hg3d (msecT 30) (return True)

Save the file, compile it and start it as usual with:

.. code-block:: console
	
	./build
	./run

Adding a Dependency
~~~~~~~~~~~~~~~~~~~

Here we are getting an error, since some dependencies in cabal are missing. So we need to adapt the file ``game.cabal`` to include the dependencies for an additional package.

.. code-block:: console

	executable game
	  hs-source-dirs:      .
	  main-is:             game.hs
	  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
	  build-depends:       base, text, containers, HGamer3D >= 0.7.1
	  default-language:    Haskell2010

The added part is the ``containers`` package in the ``build-depends:`` line. Now you know how to add libraries, in case you need another one.  

Building now will work and if you run the program it just displays an empty window, since we replaced all game logic with an empty loop. But now you are ready to fill in your own game logic!


The Challenge
-------------

For a 2D space invaders game you will need some code snippets for various game pieces. Graphics, sound and key input are all neccessary functionalities. It will be useful in addition to use multiple threads in parallel with ``forkIO``. In the context of more than one thread accessing values, variables are of some help. To get you started I compiled some code snippets for usage below.

Graphics
~~~~~~~~

For simplicity we start with simple text graphics, which moves on the screen. Below you fill find the code to define text and move it. The idea behind this is to build some "graphics" by putting together multiple lines of text with characters like ``#`` or ``*``. It is always a very creative task in game programming to design the multi-media components.


.. code-block:: Haskell

	do
		t <- newE [ ctText #: "Nice Text Picture", ctScreenRect #: Rectangle 10 10 150 50 ] -- creates text
		setC t ctScreenRect (Rectangle 100 10 150 50) -- moves text

Sound
~~~~~

It is possible to mix background music and effects, here is a nice background music together with two sound effects. They are included in the standard media package which is also used in the samples.

.. code-block:: Haskell

	do
		m <- newE [ ctSoundSource #: Music "Sounds/RMN-Music-Pack/OGG/CD 3 - Clash of Wills/3-04 Joyful Ocean.ogg" 1.0 True "Music"
           			, ctPlayCmd #: Stop ]  -- creates music

		s1 <- newE [ ctSoundSource #: Sound "Sounds/inventory_sound_effects/ring_inventory.wav" 1.0 False "Sounds"
					, ctPlayCmd #: Stop ] -- creates a sound

		s2 <- newE [ ctSoundSource #: Sound "Sounds/inventory_sound_effects/metal-clash.wav" 1.0 False "Sounds"
					, ctPlayCmd #: Stop ] -- creates another sound

		-- play a sound and play music
		setC s1 ctPlayCmd Play
		setC m ctPlayCmd Play


Key Input
~~~~~~~~~

For key input, two steps are needed. First an event handler needs to be created and then a callback needs to be registered for key events. The function handleKeys needs to be defined in the game logic.

.. code-block:: Haskell

	do 
		ieh <- newE [ctInputEventHandler #: DefaultEventHandler, ctKeyEvent #: NoKeyEvent]
		registerCallback hg3d ieh ctKeyEvent (\k -> handleKeys k)


Multithreading and Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To use multiple threads and access stored state from them, variables are useful. They are defined in |HGamer3D| and they are thread safe.

.. code-block:: Haskell

	do
		var <- createVar []						-- a variable with an empty list
		updateVar var (\l -> (newEl: l, ()))	-- adds a new element to the list, this happens atomically

		forkIO someLoop                         -- runs the loop in another thread


Think!
------

Now it is your turn. Of course the next page displays a solution, but it is much more fun doing it yourself!


.. include:: GeneralInclusions
