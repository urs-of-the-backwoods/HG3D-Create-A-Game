Your First Haskell Game
#######################

You are sitting in front of your computer, a new Haskell book at your side and wondering, what to do next? Sure you expect some setup to be done, to get tools installed, libraries installed, editors installed and more of that. So I have good and bad news. The bad news is, we only will create a template based program with a spinning cube in this section and nothing else. The good news is, it will take less then 15 min.

Quick Start
-----------

The following commands will get you very quickly to a running program. 

First, download the ``aio`` tool and install it in your execution path. (Being concerned about security, you can also download the `source`_, check it carefully and compile it yourself.)

* `Arriccio - Linux Version Download`_
* `Arriccio - Windows Version Download`_
* `Arriccio - OS X Version Download`_

.. _`Arriccio - Linux Version Download`: http://www.hgamer3d.org/downloads/aio-amd64-linux-0.1.2.tar.gz
.. _`Arriccio - Windows Version Download`: http://www.hgamer3d.org/downloads/aio-amd64-windows-0.1.2.zip
.. _`Arriccio - OS X Version Download`: http://www.hgamer3d.org/downloads/aio-amd64-darwin-0.1.2.tar.gz

.. note:: 64 bit only at the moment (due to time constraints in setting up another 3 environments, sorry)

.. _`fresco`: http://github.com/urs-of-the-backwoods/fresco
.. _`source`: http://github.com/urs-of-the-backwoods/fresco/blob/master/arriccio/main.go

Second, install Haskell.

.. code-block:: console

	aio http://www.hgamer3d.org/component/Stack setup --resolver lts-5.8

Third, create a test directory, cd into it and install a program skeleton to start with.

.. code-block:: console

	aio http://www.hgamer3d.org/component/CreateProject

.. note:: CreateProject works in the current directory and overwrites existing files, so please only use it in a new, freshly created and empty directory!

Fourth, build and run the program.

.. code-block:: console

	./build
	./run

You should see a spinning cube! Wow that was fast. To be fair there is some time needed during execution of the commands to install things and you need to answer ``yes`` to all install questions. Details on the arriccio tool can be found in the next section, now we shortly look at the pieces, which you just used.


Haskell
~~~~~~~

To setup Haskell we used ``aio`` to get us a bundled version of `The Haskell Tool Stack`_. It might be a good idea to get some background knowledge on the stack tool itself from `The Haskell Tool Stack`_ website. For |HGamer3D| a specific version of the Haskell compiler was setup by using a specific resolver version in the commands given above. Stack has some profound advantages over alternative build methods for Haskell code. It was invented specifically to fight the Haskell version of dll hell, the so-called cabal hell. Therefore it is used here. 

.. _`The Haskell Tool Stack`: http://www.haskellstack.org


Spinning Cube Code
------------------

Now let's have a look at the code of the spinning cube to get some ideas, how game programming with |HGamer3D| looks like. For example you can fire up the editor and inspect the code directly.

.. code-block:: console

	aio Edit game.hs

Or you have a look at the code as it is copied below. In the beginning there is one extension used, ``OverloadedStrings`` which is pretty harmless, it is needed to convert code strings to ``Text`` which is used througout the library. The imports are done, the main one being the one importing the library. 

All the game logic is inside the function ``gameLogic`` which takes one parameter ``hg3d``. This parameter contains hidden data on an initialized system and is handed in to the function from the ``runGame``` function in ``main``. First comes the setup of entities like camera, text and the cube geometry 
with ``newE`` calls handing in parameters as a list of special key value pairs. Those pairs are typed behind the scenes to have the correct mapping between key and value type. Rotation is done in a separately started thread, accessing the cube entity - called ``eGeo``. Finally the game loop is run in the main function by calling the ``gameLogic`` function from ``runGame`` and that's it.  

.. code-block:: haskell

	{-# LANGUAGE OverloadedStrings #-}
	module Main where

	import HGamer3D

	import qualified Data.Text as T
	import Control.Concurrent
	import Control.Monad
	import System.Exit

	gameLogic hg3d = do

	    -- create minimum elements, like a camera
	    eCam <- newE hg3d [
	        ctCamera #: FullViewCamera,
	        ctPosition #: Vec3 1 1 (-30.0),
	        ctLight #: Light PointLight 1.0 1000.0 1.0 
	        ]

	    -- do something interesting here, in this example case, it is a text and
	    -- a rotating cube

	    eText <- newE hg3d [
	        ctText #: "Rotating Cube Example",
	        ctScreenRect #: Rectangle 10 10 100 25
	        ]

	    eGeo <- newE hg3d [
	        ctGeometry #: ShapeGeometry Cube,
	        ctMaterial #: matBlue,
	        ctScale #: Vec3 10.0 10.0 10.0,
	        ctPosition #: Vec3 0.0 0.0 0.0,
	        ctOrientation #: unitU
	        ]

	    let rotateCube = do
	            forever $ do 
	                updateC eGeo ctOrientation (\u -> (rotU vec3Z 0.02) .*. u)
	                updateC eGeo ctOrientation (\u -> (rotU vec3X 0.015) .*. u)
	                sleepFor (msecT 12)

	    forkIO rotateCube
	    return ()

	main = do 
	    runGame standardGraphics3DConfig gameLogic (msecT 20)
	    return ()

.. include:: GeneralInclusions
