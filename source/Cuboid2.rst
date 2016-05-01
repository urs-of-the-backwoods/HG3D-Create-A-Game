Cuboid2 Game
############

Cuboid2 is a re-implementation of the `Cuboid`_ game from Pedro Martins, who originally implemented it with Yampa and GLUT. A typical game level exhibits a green and a red shpere and some cubes arranged in a 3D world. You can move the green sphere along the 3 axes, the movement only stops at other cubes, so it is important not to "loose" the green sphere in space. Goal is to reach the red sphere, the target. After finishing with one level, the next level show up until the third, final level is reached. The level data are from the original game. The original implementation is a showcase for FRP with usage of the Yampa library. The implemenation below is in "imperative" style meaning that for drawing and other IO actions are sequenced like in a standard language. The gamelogic is not complex and written in purely functional style.

.. _`Cuboid`: https://hackage.haskell.org/package/cuboid

.. _`Cuboid2`: https://github.com/urs-of-the-backwoods/HGamer3D/raw/master/examples/Cuboid2.hs

|

.. image:: images/cuboid2.jpg
   :width: 600px

|

Let's now look through some code snippets from this program. The purely functional game logic of this prgogram is quite short, basically there are three levels implemented and the game data just contain those levels. Each level has the cube position and the start end goal position for the green and red sphere. Then there is a short function, which evaluates a given direction.

.. literalinclude:: ../../HGamer3D/examples/Cuboid2.hs
    :start-after: -- CH3-7s
    :end-before: -- CH3-7e
    :language: Haskell
    
Light is a central element to each 3D game and to have the visuals more nicer to this scenes I added three light sources of differnt type. 

.. literalinclude:: ../../HGamer3D/examples/Cuboid2.hs
    :start-after: -- CH3-8s
    :end-before: -- CH3-8e
    :language: Haskell
    
And as a final code snippet, lets look at the actual loop, which runs one level of the game which is shown below. 

.. literalinclude:: ../../HGamer3D/examples/Cuboid2.hs
    :start-after: -- CH3-11s
    :end-before: -- CH3-11e
    :language: Haskell


.. include:: GeneralInclusions

