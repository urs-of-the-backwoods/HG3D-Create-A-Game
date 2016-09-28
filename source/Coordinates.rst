API III: 3D Coordinates
#######################

There is a big problem you will face when starting with 3D game programming and this is vector math and locations. If you start with 2D game programming you draw a sprite at (0, 0) move it and snip-snap you see somehting moving. In 3D game programming, you do the same and - you see nothing. Ah you forgot the camera, add it, still a black screen. After thinking a while you find it, you need light, so you add light. Still a black screen. Finally you discover the camera points into positive z direction and is located at z-axis 10.0 and your object is at z-axis 0.0. So figuring out the correct camera direction costs you a day of recapture of math and you got it, your first object appears on the screen.

Therefore no 3D game API can exist without the neccessary vector math library and an explanation of the coordinate system and the functions, working to scale, move and rotate objects. This section handles this.

Vector Math
-----------

|HGamer3D| uses the `vector math library`_ from Balázs Kőműves for 3D vector math. This excellent library provides an easy-to-use API and all the neccessary tools for computer graphics in 3D. The basic operators can be found in the ``Data.Vect.Float.Base`` module. The main operators to use are:

**Vector creation**

- ``mkVec3 (Float, Float, Float)`` - creation function from tuple
- ``Vec3 Float Float Float`` - constructor

**Vector operators and constants**

- ``+&``, ``-&`` - vector addition and substraction
- ``neg`` - negation operator
- ``zero`` - zero vector
- ``vec3X``, ``vec3Y``, ``vec3Z`` - the x, y and z coordinate system vector

**Scalar multiplication**

- ``*&``, ``&*`` - multiply with scalar value on the left, right side of the vector

**Dot product operations**

- ``&.`` - dot product
- ``norm``, ``normsqr``, ``len``, ``lensqr`` - different norms on vectors
- ``normalize`` - normalize a vector
- ``distance`` - between points
- ``angle`` - between vectors

**Cross product operations**

- ``&^`` - cross product

**Projections**

- ``project`` - projects a vector to the second vector
- ``project'`` - project a vector to the hyperplane orthogonal to the second unit vector


**Rotation**

- ``rotU`` - create a unit quaternion from a vector and an angle. The quaternion describes a rotation around the vector by the angle (radians).
- ``*.`` - rotate a vector (quaternion on the left side, vector on the right)
- ``.*.`` - combine quaternions or rotate orientations with a quaternion (quaternion on the left)

.. _`vector math library`: https://hackage.haskell.org/package/vect

Coordinates
-----------

|HGamer3d| uses a left handed `cartesian coordinate system`_ with the x-axis being the horizontal axis, the y-axis being the vertical axis and the z-axis pointing into the screen (positive values are growing, going into the screen). The default camera position is at the origin ``Vec3 0.0 0.0 0.0`` and is pointed towards positive z values.

.. _`cartesian coordinate system`: https://en.wikipedia.org/wiki/Cartesian_coordinate_system


Rotation and Quaternions
------------------------

`Quaternions`_ are special mathematical consctructs with 4 components, which are used to describe rotations and orientations in computer games. They have certain properties, which make them advantegous over other representataions of rotations. Although the theory is quite complicated, there is a good message. You do not need to understand it, to use it. Simply follow the usage patterns below and you are ready to go with them.

**Defining a Quaternion as rotation around an axis**

If you want to define a quaternion initially, you can create one by specifying a rotation around an axis with the following formula:

.. code-block:: haskell

    -- create a unit quaternion which rotates 30.0 radians around x-axis
    let u = rotU vec3X 30.0


**Use a Quaternion to rotate a vector, an object**

To use this quaternion to rotate an object, you need to apply the quartenion on the left:

.. code-block:: haskell

    -- rotate a vector/point with the quaternion
    let v' = u *. v

    -- rotate an object by changing its orientation with the quaternion
    let orientation' = u .*. orientation

.. _`Quaternions`: https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation


Rotations with regards to the x, y and z-axis are commonly known as ``pitch``, ``yaw`` and ``roll`` and those functions are also defined. They operate on orientations and angles and give back the new orientation.

This should suffice for a first introduction. The vect library contains a lot more interesting functionality in case you need specific computations.


.. include:: GeneralInclusions
