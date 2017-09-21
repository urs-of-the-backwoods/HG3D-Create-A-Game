Start Programming
#################

|HGamer3D| comes pre-build with a set of tools, to use it easily. So before you try to compile everything yourself - this is explained in a later section (somewhere in the future) - at least give the following short starting guide a try.

The single steps to create a running game from a source template for all three platforms (Windows, Mac, Linux) are as follows: first you need to download a small toolset and install it in a directory of your choice. Then you need to start a command line interpreter (cmd.exe, bash) with access to git and source the path of the toolset by calling a specific batch file. After that you can initialize the tooling and finally you are ready to scaffold a template project, compile and run it.

The time to do all that is about 10 minutes.

**Create Tool Directory**

Please create a new directory either in the explorer of your OS or with the command line. Then download the toolset for your specific platform from one the following links:

* `Windows Tools Download`_
* `Mac Tools Download`_
* `Linux Tools Download`_

.. _`Windows Tools Download`: https://github.com/urs-of-the-backwoods/HGamer3D-Client/releases/download/v0.9.0/hgamer3d-client-windows-v0.9.zip
.. _`Mac Tools Download`: https://github.com/urs-of-the-backwoods/HGamer3D-Client/releases/download/v0.9.0/hgamer3d-client-darwin-v0.9.tar.gz
.. _`Linux Tools Download`: https://github.com/urs-of-the-backwoods/HGamer3D-Client/releases/download/v0.9.0/hgamer3d-client-linux-v0.9.tar.gz

Finally extract the content of the downloaded archive there. The content of your directory should be a single ``set-path`` batch file and a ``tools`` directory. Note the path to the the ``set-path`` batch file, you will need to call this one in the next step.

**Run command line interpreter and modify PATH**

During creation of the |HGamer3D| template project and the compilation of its source you will work in a command line interpreter. Usually this is ``cmd.exe`` on Windows and ``bash`` on Linux or Mac. You will work in a different directory than the one where you extracted the tools. All you need to do to make the tools available is to source the ``set-path`` script like so:

.. code-block:: console

	source <PATH TO TOOLS>/set-path

In Windows you can ommit the source command. With that you added the tools directory to your path and you can start to initialize the toolset.

**Initialize Tools**

There is one command, which you only need to call once per user, to setup the toolset. Specifically what this command does is it links some generic commands to more specific ones by setting alias commands. In addition it installs the Haskell toolchain for the used version of GHC. Call the following command from your command line interpreter.

.. code-block:: console

	initial-setup

This might take a while and will involve downloading a set of tools and initializing Stack and GHC. If you want to remove the downloaded things later, everything downloaded is located in ``~/.aio``, so no trouble in removing it later.

**Scaffold, Build and Run Project**

Now everything is ready to go, you can create a new project directory with ``create``, build the created program with ``build`` and run it with ``run game``.

.. code-block:: console

	create GameProject
	build
	run ./game


Done.

|

.. image:: images/RotatingCube2.jpg
   :width: 40%

|


**Examining the Code**

If you want to have a first preview into a functioning code snippet fire up the editor and have a look at the code with ``aio Edit game.hs``.

Do you recognize the typical Haskell program structure? The ``Main`` module declaration and imports at the beginning, a function called ``gameLogic`` and one called ``main``? Try to modify this code in the editor, for example, change the color to green, re-compile the code with ``build`` and run it with ``run``. 


.. literalinclude:: ../../HGamer3D/samples/RotatingCube2.hs
    :language: Haskell


.. include:: GeneralInclusions
