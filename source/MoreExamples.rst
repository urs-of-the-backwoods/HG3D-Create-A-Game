More Examples
#############

Would you like to see more examples? Or even better, compile them yourself and use them as starting point for your ideas? Below are some links to more examples and the `HGamer3D github repository`_.

.. _`HGamer3D github repository`: https://github.com/urs-of-the-backwoods/HGamer3D

================== ======================================== ======================================
Example            Description                              Link
================== ======================================== ======================================
Cuboid2            A puzzle game, finding a path            `Cuboid2 on github`_
Gui1               GUI example                              `Gui1 on github`_
Materials          Showcasing all available Materials       `Materials on github`_
RotatingCube2      Attaching elements to each other         `RotatingCube2 on github`_
SoundEffects       How to program sound                     `SoundEffects on github`_
SpaceInvaders      A comprehensive space invaders clone     `SpaceInvaders on github`_ 
================== ======================================== ======================================

.. _`Cuboid2 on github`: https://github.com/urs-of-the-backwoods/HGamer3D/blob/master/samples/Cuboid2.hs
.. _`Gui1 on github`: https://github.com/urs-of-the-backwoods/HGamer3D/blob/master/samples/Gui1.hs
.. _`Materials on github`: https://github.com/urs-of-the-backwoods/HGamer3D/blob/master/samples/Materials.hs
.. _`RotatingCube2 on github`: https://github.com/urs-of-the-backwoods/HGamer3D/blob/master/samples/RotatingCube2.hs
.. _`SoundEffects on github`: https://github.com/urs-of-the-backwoods/HGamer3D/blob/master/samples/SoundEffects.hs
.. _`SpaceInvaders on github`: https://github.com/urs-of-the-backwoods/SpaceIn3d

To compile the examples, you need to:

- Download the ``raw`` version of the file by clicking the appropriate link in the github interface
- Rename the file to ``game.hs`` in the project folder
- Run ``build`` and then ``run``

Alternatively, you can figure out, how the build batch file work and issue the same command on the downloaded file. In the case of the space invader example, you can clone the complete repo into a new folder and use the build and run batches within that cloned folder.

**Adding Haskell dependencies, by modifying the Cabal file**

There is one example, which does not compile out of the box, because a Haskell dependency is missing. If you copy SpaceInvaders.hs to game.hs and run the build command,
the following error will appear:

.. code-block:: console

    Could not find module `Data.Map'                                                                   
    It is a member of the hidden package `containers-0.5.6.2@conta_2C3ZI8RgPO2LBMidXKTvIU'.            
    Perhaps you need to add `containers' to the build-depends in your .cabal file.                     
    Use -v to see a list of the files searched for.                                                    
                                                                                                       
This error means, that a Haskell package is missing, more concrete, the ``Data.Map`` module, which is included in the ``containers`` package. To resolve the
issue, you need to tell the Haskell build mechanism that you want to include this package. You can do this by opening ``game.cabal`` in the editor (type ``edit game.cabal``)
and adding the dependency to the ``build depends:`` section, like so:

.. code-block:: console

	executable game
	  hs-source-dirs:      .
	  main-is:             game.hs
	  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
	  build-depends:       base, text, HGamer3D (>= 0.8.0 && < 2.0.0), containers
	  default-language:    Haskell2010

After you saved this file you are able to build the project.

.. note:: You need to go through the same procedure for each Haskell module, you want to add to your program.


.. include:: GeneralInclusions
