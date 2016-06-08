Your First Haskell Game
#######################

You are sitting in front of your computer, a new Haskell book at your side and wondering, what to do next? Sure you expect some setup to be done, to get tools installed, libraries installed, editors installed and more of that. So I have good and bad news. The bad news is, we only will create a template based program with a spinning cube in this section and nothing else. The good news is, it will take less then 15 min and then we are ready to go for more fun.

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

.. note:: Arriccio is a specific layer of clay, which is used during the creation of a fresco. To ease up game programming with Haskell, I created a small utility layer, called `fresco`_ and one of the tools out of this toolbox is the arriccio tool, with the short name ``aio``.

.. _`fresco`: http://github.com/urs-of-the-backwoods/fresco
.. _`source`: http://github.com/urs-of-the-backwoods/fresco/blob/master/arriccio/main.go

Second, install Haskell.

.. code-block:: console

	aio http://www.hgamer3d.org/component/Stack setup --resolver lts-5.8

Third, create a test directory, cd into it and install a start program skeleton.

.. code-block:: console

	aio http://www.hgamer3d.org/component/CreateProject

Fourth, build and run the program.

.. code-block:: console

	./build
	./run

You should see a spinning cube! Wow that was fast, did you stop the time? (To be fair some of the comments take some time to install things and you need to answer ``yes`` to all install questions). How did it work?

----

Arriccio
~~~~~~~~

Programming a game in any language involves a lot of different components to play together, for example game libraries, bindings, media and so on. All of those pieces need to go somewhere and usually it is quite some work to just figure out where to place items and to setup everything. Arriccio is doing that for you. It is a bookkeeper managing components and assembling ready to run packages by reading metadata and installing the neccessary files. 

To use it you need to know that components are identified by their url or alternatively by an alias you can freely choose. |HGamer3D| already provides you with a number of those components and arriccio manages them for you. You probably saw the component names, like ``http://www.hgamer3d.org/component/Stack`` being used in commands to ``aio`` all the time. 

Fire up arriccio without parameter and you will get a short helping introduction. If you look closer at the arriccio commands you see that most of them take the url or a name as parameter. Since those url's tend to be long and difficult to remember, you can give them an alias for easier use. Please directly do that with a number of components we will use during the next pages. Enter the following commands to assign the alias names:

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

Try some other commands of arriccio, for example the license command which gives you licensing information on the component you name.

Haskell
~~~~~~~

To setup Haskell we used ``aio`` to get us a bundled version of `The Haskell Tool Stack`_. It might be a good idea to get some background knowledge on the stack tool itself from `The Haskell Tool Stack`_ website. For |HGamer3D| a specific version of the Haskell compiler was setup by using a specific resolver version in the commands given above. Stack has some profound advantages over alternative build methods for Haskell code. It was invented specifically to fight the Haskell version of dll hell, the so-called cabal hell. Therefore it is used here. Nevertheless since using it within the context of game programming where libraries needs to be found, the command to start the program is somehow cryptic and hidden in a batch file. If interested have a look at the ``run`` batch file. 

.. _`The Haskell Tool Stack`: http://www.haskellstack.org


Spinning Cube Code
------------------

Great, now let's check out the code of the spinning cube to get some ideas, how game programming with |HGamer3D| looks like. For example you can fire up the editor and inspect the code.

.. code-block:: console

	aio Edit game.hs

Or you have a look at the code as it is copied below. In the beginning there is one extension used, ``OverloadedStrings`` which is pretty harmless but needed to convert code strings to ``Text`` which is used througout the library. The imports are done, the main one being the one importing the library. 

System initialization follows with the ``configureHG3D`` call and the setup of the entities like camera, text, cube and so on with ``newE`` calls handing in parameters as a list of special key value pairs. Those pairs are typed behind the scenes to have the correct mapping between key and value type. Rotation is done in a separately started thread, accessing the cube entity - called ``eGeo``. Finally the game loop is run in the main function and that's it. No magic, no complex API, simple and clean. 

.. code-block:: haskell

	{-# LANGUAGE OverloadedStrings #-}
	module Main where

	import HGamer3D

	import qualified Data.Text as T
	import Control.Concurrent
	import Control.Monad
	import System.Exit

	start = do

	    -- initialize system
	    hg3d <- configureHG3D      -- initialize

	    -- create minimum elements, like a camera
	    eCam <- newE [
	        ctCamera #: FullViewCamera,
	        ctPosition #: Vec3 1 1 (-30.0),
	        ctLight #: Light PointLight 1.0 1000.0 1.0 
	        ]

	    -- do something interesting here, in this example case, it is a text and
	    -- a rotating cube

	    eText <- newE [
	        ctText #: "Rotating Cube Example",
	        ctScreenRect #: Rectangle 10 10 100 25
	        ]

	    eGeo <- newE [
	        ctGeometry #: ShapeGeometry Cube,
	        ctMaterial #: matBlue,
	        ctScale #: Vec3 10.0 10.0 10.0,
	        ctPosition #: Vec3 0.0 0.0 0.0,
	        ctOrientation #: unitU
	        ]

	    return (eGeo, hg3d)


	rotateCube eGeo = do
	    forever $ do 
	        updateC eGeo ctOrientation (\u -> (rotU vec3Z 0.02) .*. u)
	        updateC eGeo ctOrientation (\u -> (rotU vec3X 0.015) .*. u)
	        sleepFor (msecT 12)
	    return ()

	main = do 
	    (eGeo, hg3d) <- start
	    forkIO $ rotateCube eGeo
	    loopHG3D hg3d (msecT 20) (return True) -- allow close on windows click
	    return ()





.. include:: GeneralInclusions
