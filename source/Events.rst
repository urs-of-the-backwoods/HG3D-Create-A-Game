API II: Events
##############

Interactive games are nothing without events. Key input, joystick input, sounds playing all of that are events. So the next step in API knowledge is about how are events handled. 

Sending Events
--------------

Sending events is actually nothing special in the API. You set a specific component, to send an event. Since the ``setC`` function is in the IO Monad setting a value actually resembles an action. Let's take for example the commands to play sounds as shown below.

The following code creates an entity with a combination of a button and a sound source. This entity will be bound to a variable called ``sound`` in the code following hereafter.

.. literalinclude:: ../../HGamer3D/examples/SoundEffects.hs
    :start-after: -- HGamer3D website, events, create sound source
    :end-before: -- end of website text
    :language: Haskell

On a button click (explanation for registerCallback, see below) an event is send to the ``sound`` source by simply setting the ``ctPlayCmd`` component to ``Play`` with the code part ``setC sound ctPlayCmd Play``. 

.. literalinclude:: ../../HGamer3D/examples/SoundEffects.hs
    :start-after: -- HGamer3D website, events, send play event
    :end-before: -- end of website text
    :language: Haskell


Receiving Events
----------------

In |HGamer3D| the functionality to receive events is based on detecting the change of a component by registering a callback to one component. This can be done actually with any component and as soon as its value changes, the callback is triggered. This works also for plain components set by the program itself.

As a first example consider the following code snippet, which programs an exit button:

.. literalinclude:: ../../HGamer3D/examples/TestDel.hs
    :start-after: -- HGamer3D website, events, register callback on button 1
    :end-before: -- end of website text
    :language: Haskell

This code creates a button with the label *Exit*. To get events from this button, we can create a callback to the ``ctButton`` object/property.

.. literalinclude:: ../../HGamer3D/examples/TestDel.hs
    :start-after: -- HGamer3D website, events, register callback on button 2
    :end-before: -- end of website text
    :language: Haskell

This functions registers a callback for the case the value of the button changes. More specifically the ``ctButton`` component of the ``eButton`` entity. The handler function takes as argument the new value of the component. Since the value of the button is its pressed state, each time the button changes its state the handler is called. Inside the handler there is a test if the new value is False, since we want to exit in the moment the button is released. The function itself calls a routine which marks the exit of the program.


Key Events
----------

In case of GUI elements the callback can be registered on the value component of the GUI element. But what about other input types, for example key input? There is no component for a specific key, is it? For this scenario, you need to create an ``InputEventHandler`` component and a property for the type of events you would like to receive. See the code below.

.. literalinclude:: ../../HGamer3D/examples/Cuboid2.hs
    :start-after: -- HGamer3D website, events, event listener for keys
    :end-before: -- end of website text
    :language: Haskell

An entity is created with an ``ctInputEventHandler`` component and a ``ctKeyEvent`` property. Now events can be registered on the ``ctKeyEvent`` property:
    
.. literalinclude:: ../../HGamer3D/examples/Cuboid2.hs
    :start-after: -- HGamer3D website, events, register callback for keys
    :end-before: -- end of website text
    :language: Haskell

The registered callback handler reacts to ``KeyUp`` and ``KeyDown`` events and stores the received events in a variable for future use. 

.. include:: GeneralInclusions
