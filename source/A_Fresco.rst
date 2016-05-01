Appendix A - Fresco (Setup, Compile, Run)
#########################################

This is the first part of the appendix. It gathers the commands you need to setup, compile and run
|HGamer3D| programs with help of the Fresco utilities.

Getting Arriccio (aio)
----------------------

The download links are here:

* `Arriccio - Linux Version Download`_
* `Arriccio - Windows Version Download`_
* `Arriccio - OS X Version Download`_

Extract the "aio" executable in a folder which is on your path.

Component Aliasing
------------------

Setting alias names for the main arriccio components:

.. code-block:: console

	aio alias Edit http://www.hgamer3d.org/component/Edit
	aio alias Stack http://www.hgamer3d.org/component/Stack
	aio alias Run http://www.hgamer3d.org/component/Run
	aio alias CreateProject http://www.hgamer3d.org/component/CreateProject

Setting up a Haskell environment
--------------------------------

This is done using the Stack component and initializing an evnironment with a specific resolver. The command is needed
only once per system user and it will download a Haskell compiler and setup a stack environment with LTS 5.8.

.. code-block:: console

	aio Stack setup --resolver lts-5.8


Preparing a folder for development
----------------------------------

Create a folder, cd into it and issue the following command:

.. code-block:: console

	aio CreateProject

This command creates all boiler-plate code and files needed to get started with a project. 


Compiling a Haskell file
------------------------

.. code-block:: console

	./build


Running a compiled program
--------------------------

.. code-block:: console

	./run


.. _`Arriccio - Linux Version Download`: http://www.hgamer3d.org/downloads/aio-amd64-linux-0.1.1.tar.gz
.. _`Arriccio - Windows Version Download`: http://www.hgamer3d.org/downloads/aio-amd64-windows-0.1.1.zip
.. _`Arriccio - OS X Version Download`: http://www.hgamer3d.org/downloads/aio-amd64-darwin-0.1.1.tar.gz


.. include:: GeneralInclusions
