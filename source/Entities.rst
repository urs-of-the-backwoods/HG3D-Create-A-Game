Entities and Events
###################

The Entity
----------

You might have noticed the specific API used for creating 3D world objects. Let's have a closer view on them. Below you will find the code to create a cube in |HGamer3D|. It is composed of an IO call to ``newE`` and an argument list of key value pairs. Each key denotes a specific type and each value instantiates this type with a corresponding setting. 

.. literalinclude:: ../../HGamer3D/examples/RotatingCube.hs
    :start-after: -- CH4-1s
    :end-before: -- CH4-1e
    :language: Haskell

The ``eGeo`` value created here is an entity in the game. And the type of it is ``Entity`` which is a reference to an entity data value, with a type named ``EntityData``. So since it is a reference it does not only denote the value it also refers specifically to the distinct object being created and shown on the screen at the position and with the orientation, size and material properties, which were given in the creation call. Now let's have a look how this entity is used in another code snippet. 

.. literalinclude:: ../../HGamer3D/examples/RotatingCube.hs
    :start-after: -- CH4-2s
    :end-before: -- CH4-2e
    :language: Haskell
    
Here a rotation is programmed by updating one component of the entity - namely the rotation - with a call to ``updateC``. This call takes as a first argument the entity (remember behind the scenes it is a reference) to be modified, as a second argument the component which value is going to be updated and as a third argument a function which takes a value and transforms it into a new value. In this case a rotation around the y-axis by 0.02 radians. This is done all 20 msec. The "C" in ``updateC`` stand for "component" and correspondingly the "ct" in ``ctOrientation`` means "component type". 

If you look up the specific orientation component type in the source code, it is just a type constructor for a 64 bit number, denoting the unique type of the value. In addition the ``ComponentType`` is parameterized by an additional phantom type - this is the type of the actual value - to which the  connection by the 64 bit number is done. (The 64 bit number is used in the C-API binding). 

.. literalinclude:: ../../HGamer3D/hgamer3d/HGamer3D/Data/TypeSynonyms.hs
    :start-after: -- CH4-3s
    :end-before: -- CH4-3e
    :language: Haskell

So the entity combines different components in one conglomerate which can be referenced through one single pointer. The data values behind the components are ordinary Haskell algebraic data types. And effects to the game world are caused by setting or updating those data values in the entity conglomerate. 

You might want to look up the types of the basic components which make up a 3D object. As an example, below you will find the information for the ``Material`` data type and component. Since components carry ordinary algebraic data types as values you only need to know two things to use |HGamer3D|, the first is how entities work, the second is the details of the types which make up the components. Have a look into the source you will find the datatypes for components in the ``HGamer3D/Data`` and ``HGamer3D/Grahpics3D`? folders.

.. literalinclude:: ../../HGamer3D/hgamer3d/HGamer3D/Graphics3D/Geometry.hs
    :start-after: -- CH4-4s
    :end-before: -- CH4-4e
    :language: Haskell

You already have seen the functions ``newE`` and ``updateC`` for creating entities and updating components in it. You also can use ``setC`` for directly setting a new value of a component. 

Entities have two more very important properties in the API. First, you can establish callbacks for the case a component value changes, which can be used to implement events. Second, entities are thread safe. Reading and writing to such an entity is possible from any thread. That means you are able to modify the objects, let's say from the interpreter thread in an editor while the engine is running. 

Events
------

To program games, you need to capture events from the input devices, for example mouse, keyboard or joystick. Also when you have GUI elements like buttons or similar you would like to get events from those sources. And more general events might occur at different places in the program, for example if some value of an entity changes. 

The most basic API to react to events is by registering a listener or a callback to the event. As a first example consider the following code snippet, which programs an exit button:

.. literalinclude:: ../../HGamer3D/examples/RotatingCube.hs
    :start-after: -- CH5-1s
    :end-before: -- CH5-1e
    :language: Haskell

The main section is where the ``registerCallback`` function is called. This functions registers an anonymous callback for the case the value of the button changes. More specifically the ``ctButton`` component of the ``eButton`` entity. The handler function takes as argument the new value of the component. Since the value of the button is its pressed state, each time the button changes its state the handler is called. Inside the handler there is a test if the new value is False, since we want to exit in the moment the button is released. The function itself calls a routine which marks the exit of the program.

And basically, that's it, not much more to say about it. Maybe a short introduction how to register for key-presses and mouse events and how to specifically filter on specific event types. 

We take the Cuboid2 example and look at the key event handling. The first, we need to do is to add an entity to the world, representing the input event handler, listening to keyboard events. This is done as follows: in the ``setupWorld`` function is one line, which adds an event listener entity. The entity actually contains two components, one is the input event handler, which is just an object receiving all kind of events and distributing it. The second component is actually a key event component and this one is used, to listen for key events as explained in the next section.

.. literalinclude:: ../../HGamer3D/examples/Cuboid2.hs
    :start-after: -- CH5-2s
    :end-before: -- CH5-2e
    :language: Haskell
    
In addition, we need to listen from this entity and have a handler programmed which processes the received events. In the following code section, this handler is shown. This handler reacts to ``KeyUp`` and ``KeyDown`` events, looks into the 3rd element of this event - which actually holds a textual representation of the Key - and does two things:

- It stores KeyUp events in a variable
- It keeps a list of currently pressed keys, by checking corresponding KeyUp and KeyDown events

The list of currently pressed keys is used for camera movement (W, A, S, D) whereas the KeyUp events are used in the game logic.

.. literalinclude:: ../../HGamer3D/examples/Cuboid2.hs
    :start-after: -- CH5-3s
    :end-before: -- CH5-3e
    :language: Haskell


.. include:: GeneralInclusions
