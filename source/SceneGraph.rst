Scene Graph
###########

|

.. image:: images/RotatingCube2.jpg
   :width: 40%

|

To structure all the 3d elements in a scene and to enable efficient manipulation of those elements most game engines use a concept called *scene graph*. The scene graph orders all elements in a tree structure and manipulation of a node of this tree actually acts on all children of this node without additional work. The example which demonstrates this is the *RotatingCube2* example. In this example the scene graph is build out of three elements: a blue cube, a green sphere and a red sphere. The red sphere is a child of the green sphere and the green sphere is a child of the blue cube. When the blue cube is rotated, the green sphere and the red sphere are following. In addition the red sphere rotates around the green one, this is done, by just rotating the green sphere around an axis which goes through the origin. Since the red sphere is attached to the green sphere as a child, it rotates with it.

The code to make all this possible is shown in the example below:

.. code-block:: haskell

      es <- newET hg3d [
            -- create camera
            () -: [
                  ctCamera #: FullViewCamera,
                  ctPosition #: Vec3 1 1 (-30.0),
                  ctLight #: Light PointLight 1.0 1000.0 1.0 
                  ],
            -- create text
            () -: [
                  ctStaticText #: "Rotating Cube Example",
                  ctScreenRect #: ScreenRect 10 10 100 25
                  ],
            -- create geometry, with child geometry items
            "eGeo" <| ([
                  ctGeometry #: ShapeGeometry Cube,
                  ctMaterial #: matBlue,
                  ctScale #: Vec3 5.0 5.0 5.0,
                  ctPosition #: Vec3 0.0 0.0 0.0,
                  ctOrientation #: unitU
                  ], [
                        "eSmall" <| ([
                              ctGeometry #: ShapeGeometry Sphere,
                              ctMaterial #: matGreen,
                              ctPosition #: Vec3 (-0.5) 0.5 (-0.5),
                              ctScale #: Vec3 0.8 0.8 0.8,
                              ctOrientation #: unitU
                              ],
                              [
                                () -: [
                                    ctGeometry #: ShapeGeometry Sphere,
                                    ctMaterial #: matRed,
                                    ctScale #: Vec3 0.5 0.5 0.5,
                                    ctPosition #: Vec3 (0.0) (-0.5) (-0.5),
                                    ctOrientation #: unitU
                                    ]
                              ])
                  ]),

            -- create button
            "eButton" <: [
                  ctButton #: Button False "Exit",
                  ctScreenRect #: ScreenRect 200 10 50 25
                  ]

			]

As you can see, this example introduces a small syntax for building trees from entities. The rules to use this syntax are:

- use ``newET`` instead of ``newE`` and as parameter a list of combinations with the following operators:
- ``() -: [<parameters>]`` creates an anonymous entity without child entities.
- ``<name> <: [<parameters>]`` creates a named entity without child entities.
- ``<name> <| ([<parameters>], [<children>])`` creates a named entity with children.

There is not much more to say towards the scene graph, maybe just those two remarks:

- There is additional syntax to assign a parent, you can get the unique id of an entity with ``idE <entity>`` and you can add a property called parent, like so ``ctParent #: <id>``.
- GUI elements can also be structured this way.


.. include:: GeneralInclusions
