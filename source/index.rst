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

Welcome to the home of |HGamer3D|. I created |HGamer3D| for the purpose of experimenting with the Haskell language in combination with 3D world creation. Unfortunatly, |HGamer3D| is not feature complete for the purpose of creating more advanced games. To give transparency on it's status, I have a small table, showing available features:

+-----------------------------------------------+-------------------------------------------------------+
| Feature                                       | Status                                                |
+===============================================+=======================================================+
| Scene Tree                                    | available, 3D hierarchy is possible                   |
+-----------------------------------------------+-------------------------------------------------------+
| 3D Objects                                    | you can create 3D objects from meshes and geometries  |
+-----------------------------------------------+-------------------------------------------------------+
| Materials                                     | materials can be used on 3D objects, yes              |
+-----------------------------------------------+-------------------------------------------------------+
| Scaling, Moving, Rotating Objects             | available                                             |
+-----------------------------------------------+-------------------------------------------------------+
| Camera                                        | available, also split screen with 2 cameras           |
+-----------------------------------------------+-------------------------------------------------------+
| Light                                         | different types of light available                    |
+-----------------------------------------------+-------------------------------------------------------+
| Sound                                         | available                                             |
+-----------------------------------------------+-------------------------------------------------------+
| Input Devices                                 | Keyboard yes, Mouse yes, Joystick no                  | 
+-----------------------------------------------+-------------------------------------------------------+
| GUI                                           | small set of most important widgets available         |
+-----------------------------------------------+-------------------------------------------------------+
| Event Handling                                | available                                             |
+-----------------------------------------------+-------------------------------------------------------+
| Multi-Threading (Haskell can do it)           | available, calls form threads will be serialized      |
+-----------------------------------------------+-------------------------------------------------------+
| Animated Characters                           | not available                                         |
+-----------------------------------------------+-------------------------------------------------------+
| Scene Loading                                 | not available                                         |
+-----------------------------------------------+-------------------------------------------------------+
| Particle Effects                              | not available                                         |
+-----------------------------------------------+-------------------------------------------------------+
| Path Finding                                  | not available                                         |
+-----------------------------------------------+-------------------------------------------------------+
| Networking                                    | not available                                         |
+-----------------------------------------------+-------------------------------------------------------+
| Physics                                       | not available                                         |
+-----------------------------------------------+-------------------------------------------------------+
| ...                                           | not available                                         |
+-----------------------------------------------+-------------------------------------------------------+


I will update this table when new features are going to be available. Also my goal is to get tooling into a state where others can add features themselves (the underlying game engine - Urho3D - is pretty feature complete). 

To get started, you need to use a small helper tool, called `aio`. Due to the nature of C++ / Haskell bindings there was no other way to structure an approach, which is easy to use for everybody and which works without hassle on Windows, Linux and Mac. Since I know, that many of you like transparency and don't like binaries, I explain the motivation behind it and the tool itself in more detail on the `first page`_.

You can skip that if you feel safe enought to use `aio` right away and jump into installing. The 10 minute guide to get a first program build on your computure is in the `First Game`_ chapter.

You might wonder, what it takes to distribute a created program. This is shortly explained in the `Distribution Guide`_.

If you encounter any difficulties in using |HGamer3D| according to the guides below, please e-mail me a short description. The e-mail address for this is: uotbw@hgamer3d.org. This will help me to detect errors and fix them and future readers can benefit from it. Thank you!


.. note:: A big "thank you!" to the creators and maintainers of Haskell, GHC, Urho3D, stack, vect and other open source libraries and tools. |HGamer3D| is based on their work. 

.. _`first page`: Arriccio.html
.. _`First Game`: FirstGame.html
.. _`Distribution Guide`: Distribution.html


**Table of Contents**

.. 	toctree::
  :maxdepth: 1

  Arriccio <Arriccio>
  First Game <FirstGame>
  More Examples <MoreExamples>
  Geometries <Geometries>
  Entities <Entities>
  Coordinates <Coordinates>
  Scene Graph <SceneGraph>
  Events <Events>
  Cuboid2 <Cuboid2>
  Space Invaders <SpaceInvaders>
  Distribution Guide <Distribution>
  How To <HowTo>
  Impressum

..
	Tools: Distributing your Game <Distribution>
..

|



News
----
- Dec, 2017 - updated instructions, to use it, fixed errors in Linux usage
- Sep, 2017 - Version 0.9 - updated Website
- April, 10th, 2017 - updated Website
- Sep, 14th, 2016 - HGamer3D presented at `HAL 2016`_, see `abstract`_ and `slides`_.

.. _`HAL 2016`: http://hal2016.haskell.org
.. _`abstract`: http://hal2016.haskell.org/program.html#althainz-talk
.. _`slides`: http://hal2016.haskell.org/slides/HAL2016-althainz.pdf

.. include:: GeneralInclusions



