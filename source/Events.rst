Events
######

Interactive games are nothing without events. Key input, joystick input, sounds playing all of that are events. So the next step in API knowledge is about how events are being received and sent. 

Sending Events
--------------

Sending events is actually nothing special in the API. You set a specific component, to send an event. Since the ``setC`` function is in the IO Monad setting a value actually resembles an action. Let's take for example the commands to play sounds as shown below.

The following code creates an entity with a combination of a button and a sound source. This entity will be bound to a variable called ``sound`` in the code following hereafter.

.. code-block:: haskell

            [   -- ring-inventory button and sound
                ctButton #: Button False "ring inventory"
                , ctScreenRect #: ScreenRect 180 10 150 50
                , ctSoundSource #: SoundSource Sound "Sounds/inventory_sound_effects/ring_inventory.wav" False 1.0 "Sounds"
                , ctPlayCmd #: Stop
            ]


On a button click (explanation for registerCallback, see below) an event is send to the ``sound`` source by simply setting the ``ctPlayCmd`` component to ``Play`` with the code part ``setC sound ctPlayCmd Play``. 

.. code-block:: haskell

    registerSoundButtons hg3d sound1 sound2 sound3 = do
        mapM (\sound -> addActionButton hg3d sound (setC sound ctPlayCmd Play)) [sound1, sound2, sound3]


Receiving Events
----------------

In |HGamer3D| the functionality to receive events is based on detecting the change of a component by registering a callback to one component. This can be done actually with any component and as soon as its value changes, the callback is triggered. This works also for plain components set by the program itself.

As a first example consider the following code snippet, which programs an exit button:

.. code-block:: haskell

            -- create button
            "eButton" <: [
                  ctButton #: Button False "Exit",
                  ctScreenRect #: ScreenRect 740 550 50 25
                ]

This code creates a button with the label *Exit*. To get events from this button, we can create a callback to the ``ctButton`` object/property.

.. code-block:: haskell

    registerCallback hg3d (es # "eButton") ctButton (\(Button flag _) -> if not flag then exitHG3D hg3d else return ())

This functions registers a callback for the case the value of the button changes. More specifically the ``ctButton`` component of the ``eButton`` entity. The handler function takes as argument the new value of the component. Since the value of the button is its pressed state, each time the button changes its state the handler is called. Inside the handler there is a test if the new value is False, since we want to exit in the moment the button is released. The function itself calls a routine which marks the exit of the program.


Key Events
----------

In case of GUI elements the callback can be registered on the value component of the GUI element. But what about other input types, for example key input? There is no component for a specific key, is it? For this scenario, you need to create an ``KeyEvent`` component.
See the code below.

.. code-block:: haskell

    "eK" <: [
        ctKeyEvent #: NoKeyEvent
        ]

Now events can be registered on the ``ctKeyEvent`` property:
    
.. code-block:: haskell

    registerCallback hg3d (es # "eK") ctKeyEvent (\evt -> do
                                                   setC (es # "txt") ctStaticText ("key event")
                                                   setC (es # "txt2") ctStaticText (T.pack (show evt))
                                                   return ()
                                                 )


The registered callback handler reacts to events and displays the events.

.. include:: GeneralInclusions
