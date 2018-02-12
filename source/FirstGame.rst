Start Programming
#################

The purpose of this chapter is showing you how to setup |HGamer3D| on your platform and to start compiling and running a first example game.


Setup of HGamer3D
-----------------

**Prerequisites**

On Linux you need devtools and libgmp, for example on Ubuntu, please install: ``libgmp3-dev`` and ``build-essentials`` and similar packages on other distros. On Windows, you will need the `Visual C++ 2015 Runtime`_. Mac users need to setup developer tools also.

.. _`Visual C++ 2015 Runtime`: https://www.microsoft.com/de-de/download/details.aspx?id=52685

**Install aio**

Get the binaries of `aio` by either downloading the `tar.gz` file, or by cloning the installer repo from GitHub:

*  `aio tools download`_
*  `aio github site`_

As a second step cd into the folder of your operating system (Linux, Windows, Mac supported) and run the batch file. Please check, if ``aio`` is in your path afterwards, this is needed. If the batch file did not succeed, placing *aio* into your path, you need to do this manually by yourself.

.. _`aio tools download`: http://www.hgamer3d.org/downloads/aio-installer-1217.tar.gz
.. _`aio github site`: https://github.com/urs-of-the-backwoods/aio-installer


**Install HGamer3D Client Tools**

As described above, *aio* is just a command, to download dependent libraries, media and tools. I provided an additional installer to create commands for the different tasks of |HGamer3D|. Please issue the following command, which will create some aio alias commands for the next steps and which installs Stack and GHC, the Haskell tools:

.. code-block:: console

  aio http://www.hgamer3d.org/tools/Install.1217 install

This should end up with a message that GHC is now available, locally. In addition some alias are setup, which you can check with the ``aio list alias`` command. 


Create a Project from a Template
--------------------------------

**Create and Compile a Sample Project**

Now everything is ready to go, follow the final steps to create a sample project, compile and run it:

.. code-block:: console

  aio Create GameProject
  cd GameProject
  aio Stack install --local-bin-path .	
  aio Run ./game (Linux/Mac)
  aio Run game.exe (Windows)

|

.. image:: images/RotatingCube2.jpg
   :width: 40%

|


**Examining the Code**

If you want to have a first preview into a functioning code snippet fire up the editor and have a look at the code with ``aio Edit game.hs``.

Do you recognize the typical Haskell program structure? The ``Main`` module declaration and imports at the beginning, a function called ``gameLogic`` and one called ``main``? Try to modify this code in the editor, for example, change the color to green, re-compile the code with ``aio Stack install --local-bin-path .`` and run it with ``aio Run ./game``. 


.. literalinclude:: ../../HGamer3D/samples/RotatingCube2.hs
    :language: Haskell


.. include:: GeneralInclusions
