Appendix C - HGamer3D API
#########################

The Entity
----------

You might have noticed the specific API used for creating 3D world objects. Let's have a closer view on this. Below you will find the code to create a cube in |HGamer3D|. It is composed of an IO call to *newE* and an argument list of key value pairs. Each key denotes a specific type and each value instantiates this type with a corresponding setting. The "new" indicates that something is created here which by the way is also in the IO monad.

.. literalinclude:: ../../HGamer3D/examples/RotatingCube.hs
    :start-after: -- CH4-1s
    :end-before: -- CH4-1e
    :language: Haskell

The *eGeo* value created here is an entity in the game. And the type of it is also named *Entity* which is a reference to an entity data value, with a type named *EntityData*. So since it is a reference it does not only denote the value it also refers specifically to the distinct object being created and shown on the screen at the position, orientation, size, material and so on, which was given in the creation call. Now let's have a look how this entity is used in another code snippet. 

.. literalinclude:: ../../HGamer3D/examples/RotatingCube.hs
    :start-after: -- CH4-2s
    :end-before: -- CH4-2e
    :language: Haskell
    
Here a rotation is programmed by updating one component of the entity, the rotation with a call to *updateC*. This call takes as a first argument the reference to the entity to be modified, as a second argument the component in the entity which value is updated and as a third argument a function which takes a value and transforms it into a new value. In this case a rotation around the y-axis by 0.02 radians. This is done all 20 msec. The "c" in *updateC* clearly means "component" and also the "ct" in *ctOrientation* means "component type". If we look up the specific orientation component type in the source code, it is just a type synonym for orientation. You also notice the *ComponentType* is parameterized by an additional phantom type - this is the type of the actual value - and it is connected to a 64 bit number, which is a unique identifier of this type used in the C-API binding. 

.. literalinclude:: ../../HGamer3D/library/HGamer3D/Data/TypeSynonyms.hs
    :start-after: -- CH4-3s
    :end-before: -- CH4-3e
    :language: Haskell

So the entity combines different components in one conglomerate which can be referenced through one single pointer. The data values behind the components are ordinary Haskell data types. And effects to the game world are caused by setting or updating those data values in the entity conglomerate. 

As a first exercise I would like to suggest looking up the types of the basic components which make up a 3D object. Below you will find the information for the *Material* data type and component. Since components carry ordinary algebraic data types as values you only need to know two things to use |HGamer3D|, the first is how entities work, the second is the details of the types which make up the components. Have a look into the source you will find the datatypes for components in the :file:`HGamer3D/Data` and :file:`HGamer3D/Grahpics3D` folders.

.. literalinclude:: ../../HGamer3D/library/HGamer3D/Graphics3D/Geometry.hs
    :start-after: -- CH4-4s
    :end-before: -- CH4-4e
    :language: Haskell

You already have seen the functions *newE* and *updateC* for creating entities and updating components in it. You also can use *setC* for directly setting a new value of a component. 

Behind the scenes Entities are references to a map data structure which maps component type ids to an encoding of the data as ByteString. You can get this entity data with *readE* but probably only need that seldomly. Once you have an entity data value you can get component values out of it with the operators "#" and "#!", the second one throws an exception in case the component is not present in the entity, the first one delivers a Maybe.

Entities have two more very important properties in the API. First, you can establish callbacks for the case a component value changes, which can be used to implement events, more on that in the next chapter. Second, entities are thread safe. Reading and writing to such an entity is possible from any thread. This is actually used in the engine binding since the graphics itself runs in another thread while you are still able to modify the objects, let's say from the interpreter thread. 

Audio
-----

Playing sound and music with |HGamer3D| is quite easy to accomplish. The example `SoundEffects`_ shows all what needs to be known. The main two datastructures and components to be known are the *SoundSource* and *PlayCmd* data types. A *SoundSource* simply gives as a string the name where the sound data can be found, the current volume from 0.0 to 1.0, a bool which denotes, if the sound repeats playing and a volume group, which indicates which master volume also has an effect on the sound volume.

.. literalinclude:: ../../HGamer3D/examples/SoundEffects.hs
    :start-after: -- CH7-1s
    :end-before: -- CH7-1e
    :language: Haskell

Sound volume is regulated by two volume numbers. The one, which is given in the SoundSource data and the one which is given in the corresponding volume group. The picture shows a typical game setup with two sliders which control volume for sounds and volume for music. Those are the volume groups with a main volume setting. They are used by the component *Volume*.

.. literalinclude:: ../../HGamer3D/examples/SoundEffects.hs
    :start-after: -- CH7-2s
    :end-before: -- CH7-2e
    :language: Haskell

One volume component can be used to set volume for all groups, as shown in the code below, which connects the both sliders for sound and music volume with the same volume entity. The parameter given in the volume data type distinguishes for which group the volume is going to be set.    
    
.. literalinclude:: ../../HGamer3D/examples/SoundEffects.hs
    :start-after: -- CH7-3s
    :end-before: -- CH7-3e
    :language: Haskell


.. _`SoundEffects`: https://github.com/urs-of-the-backwoods/HGamer3D/raw/master/examples/SoundEffects.hs

GUI
---

|HGamer3D| also comes with a small number of GUI elements, as shown in the `Gui1`_ example. Those GUI elements are also programmed by using specific entities. The source code of the sample shows how to do it. Currently as properties only the position on the screen can be set as a component called *ScreenRect*. For example to create the Button and the text for it, the following code is used. 

.. literalinclude:: ../../HGamer3D/examples/Gui1.hs
    :start-after: -- CH6-1s
    :end-before: -- CH6-1e
    :language: Haskell

The picture below shows the elements which are currently implemented:

- Text
- Button
- EditText
- Slider
- CheckBox
- DropDownList

   
.. _`Gui1`: https://github.com/urs-of-the-backwoods/HGamer3D/raw/master/examples/Gui1.hs

To get the values out of the GUI elements, the value of the corresponding component can be read. This is shown in the code which regularly updates the text on the top right side with the current value of the GUI elements. This works in a polling mode with now events, but it is also possible to setup a handler which reacts on the change of the value as shown in the last Audio section.
   
.. literalinclude:: ../../HGamer3D/examples/Gui1.hs
    :start-after: -- CH6-2s
    :end-before: -- CH6-2e
    :language: Haskell


Events
------

To program games, you need to capture events from the input devices, for example mouse, keyboard or joystick. Also when you have GUI elements like buttons or similar you would like to get events from those sources. And more general events might occur at different places in the program, for example if some value of an entity changes. 

The most basic API to react to events is by registering a listener or a callback to the event. As a first example consider the following code snippet, which programs an exit button:

.. literalinclude:: ../../HGamer3D/examples/RotatingCube.hs
    :start-after: -- CH5-1s
    :end-before: -- CH5-1e
    :language: Haskell

The main section is where the *registerCallback* function is called. This functions registers an anonymous callback for the case the value of the button changes. More specifically the *ctButton* component of the *eButton* entity. The handler function takes as argument the new value of the component. Since the value of the button is its pressed state, each time the button changes its state the handler is called. Inside the handler there is a test if the new value is False, since we want to exit in the moment the button is released. The function itself calls a routine which marks the exit of the program.

And basically, that's it, not much more to say about it. Maybe a short introduction how to register for key-presses and mouse events and how to specifically filter on specific event types. 

We take the Cuboid2 example and look at the key event handling. The first, we need to do is to add an entity to the world, representing the input event handler, listening to keyboard events. This is done as follows: in the *setupWorld* function is one line, which adds an event listener entity. The entity actually contains two components, one is the input event handler, which is just an object receiving all kind of events and distributing it. The second component is actually a key event component and this one is used, to listen for key events as explained in the next section.

.. literalinclude:: ../../HGamer3D/examples/Cuboid2.hs
    :start-after: -- CH5-2s
    :end-before: -- CH5-2e
    :language: Haskell
    
In addition, we need to listen from this entity and have a handler programmed which processes the received events. In the following code section, this handler is shown. This handler reacts to *KeyUp* and *KeyDown* events, looks into the 3rd element of this event - which actually holds a textual representation of the Key - and does two things:

- It stores KeyUp events in a variable
- It keeps a list of currently pressed keys, by checking corresponding KeyUp and KeyDown events

The list of currently pressed keys is used for camera movement (W, A, S, D) whereas the KeyUp events are used in the game logic.

.. literalinclude:: ../../HGamer3D/examples/Cuboid2.hs
    :start-after: -- CH5-3s
    :end-before: -- CH5-3e
    :language: Haskell
    

.. include:: GeneralInclusions
