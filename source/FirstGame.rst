Start Programming
#################

Three steps in 10 minutes are enough to get you going!

Install Arriccio
----------------

- Use git to download the aio (arriccio) client
- cd into the folder of your operating system (Linux, Windows, Darwin) and start the installer script

.. code::

  git clone http://github.com/urs-of-the-backwoods/aio-installer
  cd linux
  ./install.sh
 
This will copy ``aio`` into your path and update your environment. Finally you can run ``aio`` from the command line.

.. note:: It is neccessary, that ``aio`` is in your path, the created scripts do assume that. 

|

Install HGamer3D
----------------

**Prerequisites**

On Linux you need devtools and libgmp


**HGamer3D toolset**

Run the following command:

.. code::

  aio http://www.hgamer3d.org/tools/HGamer3D.0218 install

This will give you some aliases, for example from now on, you only need to type ``aio HGamer3D <command>`` but also additional tooling and it installs the Haskell compiler.

.. note:: 

	To update |HGamer3D| from previous installations, you can use the ``aio HGamer3D update`` command. This will always give you the latest version. If you encounter strange errors, try to remove everything by ``rm -rf ~/.aio`` and start from the beginning with the install command above.

|

Create and Compile a Project
----------------------------

Run the following commands:

.. code::

  aio HGamer3D create MyExampleProject
  cd MyExampleProject
  ./build
  ./run


|

.. image:: images/RotatingCube2.jpg
   :width: 40%


.. include:: GeneralInclusions
