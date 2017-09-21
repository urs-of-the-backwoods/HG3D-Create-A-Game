Scene Graph
###########

|

.. image:: images/RotatingCube2.jpg
   :width: 60%

|

To structure all the 3d elements in a scene and to enable efficient manipulation of those elements most game engines use a concept called *scene graph*. The scene graph orders all elements in a tree structure and manipulation of a node of this tree actually acts on all children of this node without additional work. The example which demonstrates this is the *RotatingCube2* example. In this example the scene graph is build out of three elements: a blue cube, a green sphere and a red sphere. The red sphere is a child of the green sphere and the green sphere is a child of the blue cube. When the blue cube is rotated, the green sphere and the red sphere are following. In addition the red sphere rotates around the green one, this is done, by just rotating the green sphere around an axis which goes through the origin. Since the red sphere is attached to the green sphere as a child, it rotates with it.

The code to make all this possible is shown in the example below:

.. literalinclude:: ../../HGamer3D/samples/RotatingCube2.hs
    :start-after: -- HGamer3D website, scene graph, complete example
    :end-before: -- end of website text
    :language: Haskell

As you can see, this example introduces a small syntax for building trees from entities. The rules to use this syntax are:

- use ``newET`` instead of ``newE`` and as parameter a list of combinations with the following operators:
- ``() -: [<parameters>]`` creates an anonymous entity without child entities.
- ``<name> <: [<parameters>]`` creates a named entity without child entities.
- ``<name> <| ([<parameters>], [<children>])`` creates a named entity with children.

There is not much more to say towards the scene graph, maybe just those two remarks:

- There is additional syntax to assign a parent, you can get the unique id of an entity with ``idE <entity>`` and you can add a property called parent, like so ``ctParent #: <id>``.
- GUI elements can also be structured this way.


.. include:: GeneralInclusions
