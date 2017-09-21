Entities
########

Before starting with more Haskell game programming it might be a good idea to have a look at some basic concepts of the |HGamer3D| API and its structure. The most basic building blocks we will need are entities and events. Entities are explained in this section, events in the next one. 

Overview
--------

The Entity API is constructed in a way, wich intends to give you an easy to use interface for object creation and property manipulation. Let's have a look at some interesting topics on entities:

- they are composable from components
- they are references to at least one object, combined with properties
- the CRUD operations are in the IO Monad (most basic interface)
- their components are build from regular algebraic data types
- reading and writing to them is fully thread-safe
- creation is done with ``newE`` and a component list
- one or more of the components of an entity are objects the others are properties
- attributes modify all objects in one entity

Example
-------

Let's have a closer view on them by looking at an example. Below you will find the code to create a cube in |HGamer3D|. It is composed of an IO call to ``newE`` and an argument list of key value pairs. Each key denotes a specific type and each value instantiates this type with a corresponding setting. 

.. literalinclude:: ../../HGamer3D/samples/RotatingCube.hs
    :start-after: -- HGamer3D website, entities and events, example entity
    :end-before: -- end of website text
    :language: Haskell

In this specific case, the object created is the ``eGeo`` one, a geometry object. The other components are actually properties, which are set on the one object. This is automatically done, combining different properties with objects in an entity works the way that each property which is available in the object is set, once the property is modified or initialized. Properties which are not present in the list are set to a default value. Properties which are not available in the object do no harm and are ignored. In the case above the geometry gets a material, a position, a scale and an orientation property. 

Functions on Entities
---------------------

Reading and writing to components can be done with the following methods:

- ``readC`` - reads a component
- ``setC`` - sets a component to a specific value
- ``updateC`` - updates a component with a function from value to value

Those functions are living in the IO Monad and they are fully thread safe, they can be called from any thread concurrently even on one entity, the last set operation wins. 

Now let's see this in action: 

.. literalinclude:: ../../HGamer3D/samples/RotatingCube.hs
    :start-after: -- HGamer3D website, entities and events, rotation explanation
    :end-before: -- end of website text
    :language: Haskell
    
Here a rotation is programmed by updating one component of the entity - namely the rotation - with two calls to ``updateC``. This function takes as a first argument the entity (remember behind the scenes it is a reference) to be modified, as a second argument the component which value is going to be updated and as a third argument a function which takes a value and transforms it into a new value. In this case a rotation around the z-axis by 0.02 and around the x-axis by 0.015 radians. This is done cyclic with a pause of 12 msec. 

Components
----------

The "C" in ``updateC`` stand for "component" and correspondingly the "ct" in ``ctOrientation`` means "component type". 

The data values behind the components are ordinary Haskell algebraic data types. And effects to the game world are caused by setting or updating those data values in the entity conglomerate. This means to use the |HGamer3D| API you only need to know two things: the first is how entities work, the second are the details of the types which make up the components. Below you will find two examples, which show how easy this can.

.. literalinclude:: ../../HGamer3D/src/HGamer3D/Graphics3D/Material.hs
    :start-after: -- HGamer3D website, entities, example 1 for data types
    :end-before: -- end of website text
    :language: Haskell

So a ``Material`` can be a ``ResourceMaterial`` and that's it. You need to supply a Text value. Of course this is the name of the material resource in the resources. For convenience there are a couple of predefined resource materials available. 

.. _`package HGamer3D`: http://hackage.haskell.org/package/HGamer3D

.. literalinclude:: ../../HGamer3D/src/HGamer3D/Graphics3D/Light.hs
    :start-after: -- HGamer3D website, entities, example 2 for data types
    :end-before: -- end of website text
    :language: Haskell

A ``Light`` is more complex, it can be of type ``PointLight``, ``SpotLight`` or ``DirectionalLight`` and also has brightness, range and specular intensity. 

You can lookup the haddock documentation of all interesting types on hackage, `package HGamer3D`_.
    
Two additional remarks: the instance ``ComponentClass`` is a serializer kind of type, which transforms the ADT into a messagepack object, which is sent to the binding layer. The ``ctMaterial`` and ``ctLight`` types are used to tag each component with a 64 bit id, which are also used in the binding and additionally to make the components type safe


.. include:: GeneralInclusions
