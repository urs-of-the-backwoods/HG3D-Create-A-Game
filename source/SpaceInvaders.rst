Space Invaders
##############

|

.. image:: images/SpaceInvaders.jpg
   :width: 60%

|

Architecture and Features
-------------------------

With the following features the `space invaders clone`_ is a show-case for game programming with |HGamer3D|:

- Sound, GUI, Key-Input
- Game modes: intro screen, playing, camera move mode
- Animated invaders
- Collision detection
- Actors based game architecture
- Reader and State Monad for Actor functions
- Persistent tree data-type for data of all game elements
- Separation of concerns by actor architecture

.. _`space invaders clone`: http://github.com/urs-of-the-backwoods/SpaceIn3d

Howto Play
----------

- install aio (see :ref:`arriccio`)
- run the command ``aio start http://www.hgamer3d.org/game/SpaceIn3d``
- allow the downloads (only first time, next time it is cached)
- you might put the ``aio start ..`` command in a batch file or start link
- try the "fly" feature by hitting ``F1`` and using the keys ``W``, ``A``, ``S``, ``D``, ``Q``, ``up``, ``down``


Howto Compile
-------------

- create a new directory and cd into it
- follow the steps below:

.. code-block:: console

	git clone http://github.com/atzeus/HMap
	git clone http://github.com/urs-of-the-backwoods/SpaceIn3d
	cd SpaceIn3d
	aio http://www.hgamer3d.org/component/Stack install --local-bin-path .
	aio http://www.hgamer3d.org/component/Run ./game (Linux)
	aio http://www.hgamer3d.org/component/Run game.exe (Windows)

Actors
------

A small actor implementation serves the purpose of separating different concerns to different high-level components. The capabilities of an actor are to receive messages, act upon them in a function looping endlessly and sending messages to other actors as outcome of their specific logic piece. Additionally they might spawn new actors to support them with functionality. The ``Actor`` type itself is decoupled from the function running the actor to enable circular dependencies between actors. The main function is a Reader-State-Monad to keep state between loops and to carry read only configuration type of information.

.. note:: the forkIO in the ``runActor`` function creates a new thread for each actor. 

.. literalinclude:: ../../SpaceIn3d/Actor.hs
    :start-after: -- HGamer3D website, space invaders, actors
    :end-before: -- end of website text
    :language: Haskell

The code base of the space invader clone is split into a number of actors, as shown in the following table. Each actor is running its own function in a separate IO thread.

========== ==================== ==================== ==================== =========================
Actor      spawns               sends messages to    rec. messages from   function
========== ==================== ==================== ==================== =========================
Switch     Music, Key, Animate, Music, Key, Animate, Key, GameLoop,       main handler for modes,
           Flying, GameLoop,    Flying, GameLoop,    main game routine    event distribution
           StatusBar            StatusBar            (heartbeat)

GameLoop   Move, Canon,         Switch, Move, Canon, Collision, Switch    handles main movement of
           Collision            Collision                                 invaders, shots, canon
                                                                          and collision detection

Move                            Collision, Music,     GameLoop            moves invaders
                                StatusBar

Canon                           Collision, Music     GameLoop             moves canon, shots

Collision                       GameLoop             GameLoop, Move,      Collision detection
                                                     Canon

Flying                                               Switch               moves camera in "fly" mode

Animate                                              Switch               animates invaders

StatusBar                                            Move, Switch         display game status            

Key                             Switch               Switch (poll pressed pick up keys from Input,
                                                     keys)                keep keys pressed down

Music                                                Switch, Move, Canon  plays sounds and music
========== ==================== ==================== ==================== =========================

**Switch Actor**

The switch actor is the coordination center of the complete appication. It keeps an indicator in which mode the game is currently executing and processes events, depending on this mode. As an example the key-input from the key actor is routed through the switch actor and is interpreted differently if the game is in "playing" mode or in "pause" state. The switch actor initializes new actors also depending on progress in game state. In the beginning it also displays the entry screen and after the user entered her name, the switch actor progresses to build state and creates the moving entities. For the main activities during the "play" mode the switch actor creates a gameloop actor, which handles parallel computations and collision detection. Finally the switch actor interconnects the auxilliary actors like music, animation, status bar and key input.

.. literalinclude:: ../../SpaceIn3d/Switch.hs
    :start-after: -- HGamer3D website, space invaders, switch actor
    :end-before: -- end of website text
    :language: Haskell

The typical code example above shows the first lines of the big *switch* statement the switch actor executes. Specifically if in ``ProgramInitializing`` state the switch actor initializes the init screen and the key and music actor upon receiving the ``StartProgram`` message. After this is done it forwards the state to ``InitScreen``.

**GameLoop Actor**

The gameloop actor is a prototypical actor and short enough to display here completely. It creates the move, canon and collision actor and communicates in addition with the switch actor. In addition it hands over the music and status bar actor to the move and collision actors, created.

The main function of the gameloop actor is coordinating the computation of the next gamestate by running the move and the canon actor in parallel. Those actor hand over the result of their computations to the collision actor which computes the collision status and sends it back to the gameloop actor for progressing with the next step. This makes up an efficient and fast gameloop with some parallel computation for movement of invaders (move actor) and movement of canon (canon actor), whereas collision detection is sequenced after having the results of both previous steps. Also note that the animation is done completely independently from the gameloop actor. This works since the 3D scene graph allows us to move the invader and canon entities independently from the pixels they are composed of. Also interesting to see how the data flow is implemented. The actual date for the current step is simply send in a message to the next computing actor. This is functional programming with immutable data structures at its best.

.. literalinclude:: ../../SpaceIn3d/GameLoop.hs
    :start-after: -- HGamer3D website, space invaders, gameloop actor
    :end-before: -- end of website text
    :language: Haskell

Data Structure
--------------

**Message ADT**

All messages used in the program are shown below. They constitute the high level run-time coordination flow, used in the game.

.. literalinclude:: ../../SpaceIn3d/Actor.hs
    :start-after: -- HGamer3D website, space invaders, messages
    :end-before: -- end of website text
    :language: Haskell

**Tree for game elements**

The main game status is held in a tree data structure. The elements of the tree contain a node type and node data. The node data contains different entries, position information, size information, the |HGamer3D| entities, animation information and a unique key for each node. To be more flexible the node data is stored in an `heterogenous map`_. 

.. _`heterogenous map`: https://hackage.haskell.org/package/HMap

.. literalinclude:: ../../SpaceIn3d/Data.hs
    :start-after: -- HGamer3D website, space invaders, data tree
    :end-before: -- end of website text
    :language: Haskell

**HGamer3D Entities**

The function ``createMoveNode`` in the ``Data`` module creates the |HGamer3D| entities and stores them in the node data as an entry. This seems weird because the approach mixes non-mutual with mutual data. But think for a while. The entities are fully thread safe. Upon each modification step from one gameloop step to the next, data changes and a new immutable tree is created. During this step, simply the data is synchronized with the entity data of the 3D world. There is no reason to be concerned. References are not created or destroyed only values are being set. This makes up a working model, where the benefits of the immutable tree data structure can be kept. Even in parallel the animation takes place, which is explained in the next section.

Animation
---------

The animation is the movement of the "legs" and "arms" of the invaders. If you watch closely, you see that each invader has its own rythm, when to move. This is done by storing a random number and move only each n cycles. Since also the starting point is not uniform, state for the animation is also stored in the node data. On each animation step, pixels are exchanged from a hidden location to the screen and back in the next cycle. The animations are completely independent from the other changes on the data. This is leveraged to parallelize the animation completely. In the beginning, after the creation of the game data tree, the tree is copied to both the animation actor and the gameloop actor. Both actors work independently with their local copies and advance their respective node data states only on their data.

.. literalinclude:: ../../SpaceIn3d/Animate.hs
    :start-after: -- HGamer3D website, space invaders, animate
    :end-before: -- end of website text
    :language: Haskell

The code of the animation function makes use of the tree structure of the invaders data. For each invader there is a central node holding the animation information and sub-nodes with the pixels. The pixels have different node types ``Pixel``, ``PixelA`` and ``PixelB`` indicating to which animation state they belong. The overall loop is an accumL type of function in the accumulator during the fold the last node information of the central node is stored. When the algorithm visits the sub-node pixels the top node info is still available and used to decide the exchange of pixels on the screen.

Collision Detection
-------------------

During collision detection the tree is flattened to a list and on the list different filters are applied. Then collisions are detected between shots and invaders and between invaders and boulders for detecting an overrun of the invaders in the position of the boulders.

.. literalinclude:: ../../SpaceIn3d/Collision.hs
    :start-after: -- HGamer3D website, space invaders, collision
    :end-before: -- end of website text
    :language: Haskell


Multi-Threaded Game Architecture
--------------------------------

As a summary it is fair to say that the standard functional methodologies work well with game programming. Immutable data structures combined with actors make up for a good toolset. Actors are not more then functions itself, encapsulated with their own state and interacting via messages. So compared to object oriented programming where objects encapsulate state, runtime and pure functions in one object in the example shown here, more coarse grained runtime information is structured with actors whereas all other aspects are implemented with mostly pure functions. This leds to a mult-threaded game architecture with a separation of concerns.

.. include:: GeneralInclusions
