.. _MainIndex:

HGamer3D
########

|

.. image:: images/RotatingCube2.jpg
   :width: 30%

.. image:: images/SpaceInvaders.jpg
   :width: 30%

.. image:: images/SoundEffects.jpg
   :width: 30%

|

Welcome to the home of |HGamer3D|! 

|HGamer3D| is a tool to program 3D games with the Haskell programming language. Have a look at these `slides`_ from 
`HAL 2016`_, which explain the motivation behind the library and the structure of the code. 

The table below gives a current status of the main features. 

|

Try It!
-------

A short guide to compiling your first program is on the `next page`_. You can see |HGamer3D| in action, by 
starting the pre-compiled example programs (sample browser) or a small space invaders clone.

.. _`next page`: http://www.hgamer3d.org/FirstGame.html

**Installer**

|HGamer3D| comes with an installer, the tool is named arriccio, or for short ``aio``. There is a `detailed explanation`_ of it
below in a separate page. The installer is used for starting programs and tools. It works by copying binaries to a separate 
path in your home directory (``~/.aio``). In case you want to get rid of |HGamer3D| you can delete everything by just removing this path.

To install ``aio``, there are two ways, if you have git installed, simply clone the installer and run the provided
install scripts, there is one for each operating system, Linux, Windows or Mac.

.. code:: console

  git clone http://github.com/urs-of-the-backwoods/aio-installer
  cd Linux
  ./install.sh

Or install ``aio`` manually, by copying the executable to a directory, which is in your ``PATH``.

- `Windows binary`_ 
- `Linux binary`_ 
- `Mac binary`_ 

.. _`Windows binary`: https://github.com/urs-of-the-backwoods/aio-installer/raw/master/windows/aio.exe
.. _`Linux binary`: https://github.com/urs-of-the-backwoods/aio-installer/raw/master/linux/aio
.. _`Mac binary`: https://github.com/urs-of-the-backwoods/aio-installer/raw/master/darwin/aio

.. _`detailed explanation`: http://www.hgamer3d.org/Arriccio.html 

**Sample Browser**

The sample browser can be started with the ``aio http://www.hgamer3d.org/tools/HGamer3D.0218 sample-browser`` command.


**Example Game**

Run the Minecraft-style space invaders game, by simply issuing this command: ``aio http://www.hgamer3d.org/game/SpaceIn3d.0218``.

|

Features
--------

+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Feature                                       | Status                                                | `Samples`_             |
+===============================================+=======================================================+========================+
| Scene Tree                                    | available, 3D hierarchy is possible                   | `Sample 03`_           |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| 3D Objects                                    | you can create 3D objects from meshes and geometries  | `Sample 01`_           |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Materials                                     | materials can be used on 3D objects, yes              | `Sample 02`_           |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Scaling, Moving, Rotating Objects             | available                                             |                        |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Camera                                        | available, also split screen with 2 cameras           |                        |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Light                                         | different types of light available                    |                        |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Sound                                         | available                                             | `Sample 04`_           |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Input Devices                                 | Keyboard yes, Mouse yes, Joystick yes                 | `Sample 12`_, `05`_,   |
|                                               |                                                       | `07`_                  |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| GUI                                           | small set of most important widgets available         | `Sample 06`_           |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Event Handling                                | available                                             | `Sample 12`_           |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Multi-Threading (Haskell can do it)           | available, calls form threads will be serialized      |                        |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Skybox                                        | available                                             | `Sample 09`_           |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Scene Loading                                 | available                                             | `Sample 08`_           |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Particle Effects                              | available                                             | `Sample 11`_           |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| 3D Text                                       | available                                             | `Sample 10`_           |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Networking                                    | not available                                         |                        |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Physics                                       | not available                                         |                        |
+-----------------------------------------------+-------------------------------------------------------+------------------------+
| Animated Model                                | not available                                         |                        |
+-----------------------------------------------+-------------------------------------------------------+------------------------+

.. _`Samples`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples
.. _`aio installer`: http://github.com/urs-of-the-backwoods/aio-installer

.. _`Sample 01`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_01_RotatingCube.hs
.. _`Sample 02`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_02_Materials.hs
.. _`Sample 03`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_03_Hierarchy.hs
.. _`Sample 04`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_04_Sound.hs
.. _`05`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_05_MouseModes.hs
.. _`Sample 06`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_06_GUI.hs
.. _`07`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_07_Joystick.hs
.. _`Sample 08`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_08_SceneLoad.hs
.. _`Sample 09`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_09_Skybox.hs
.. _`Sample 10`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_10_3DText.hs
.. _`Sample 11`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_11_Particles.hs
.. _`Sample 12`: http://github.com/urs-of-the-backwoods/HGamer3D/tree/master/samples/Sample_12_EventHandling.hs

I will update this table when new features are available. Also my goal is to get tooling into a state where others can add features themselves (the underlying game engine - Urho3D - is pretty feature complete). 

If you encounter any difficulties in using |HGamer3D| according to the guides below, please e-mail me a short description. The e-mail address for this is: uotbw@hgamer3d.org. This will help me to detect errors and fix them and future readers can benefit from it. Thank you!


.. note:: A big "thank you!" to the creators and maintainers of Haskell, GHC, Urho3D, stack, vect and other open source libraries and tools. |HGamer3D| is based on their work. 


|

API
---

The following pages give an introduction to the API:

.. 	toctree::
  :maxdepth: 1

  Start Programming <FirstGame>
  Geometries <Geometries>
  Entities <Entities>
  Coordinates <Coordinates>
  Scene Graph <SceneGraph>
  Events <Events>
  Arriccio <Arriccio>
  Impressum

|

News
----
- Feb, 2018 - version 0.95 with new features (Joystick, Skybox, Scene Load, Particles, 3D Text)
- Dec, 2017 - updated instructions, to use it, fixed errors in Linux usage
- Sep, 2017 - Version 0.9 - updated Website
- April, 10th, 2017 - updated Website
- Sep, 14th, 2016 - HGamer3D presented at `HAL 2016`_, see `abstract`_ and `slides`_.

.. _`HAL 2016`: http://hal2016.haskell.org
.. _`abstract`: http://hal2016.haskell.org/program.html#althainz-talk
.. _`slides`: http://hal2016.haskell.org/slides/HAL2016-althainz.pdf


.. include:: GeneralInclusions



