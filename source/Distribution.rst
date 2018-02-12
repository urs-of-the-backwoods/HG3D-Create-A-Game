Distribution Guide
##################

How about distributing your game? Simply copying the executabl is not enough, you know by now that `aio` copies dependencies into a special folder. And this folder does not only contain the runtime dependencies, but also the tools, used to build the game. What to do?

There are actually two ways, to distribute your running binaries. The first way is to use `aio` itself and create a toml file, sign it, create a binary package sign it and upload everything to a website. The second way is to create a self-contained distributable folder structure, which works without any additional external programs.

Using aio
---------

A more in detail explanation will be done, for now, have a look at some example tools, like the Urho3D Editor. A first starting point is given by the `toml file`_. If you want to sign your own products, you can use the `aio` tool, by running a command, similar to ``aio sign <file> ~/.ssh/id_rsa``. This will sign a file with your private ssh key. You need to provide the public ssh key as download and refer in the toml file to it.

.. _`toml file`: http://www.hgamer3d.org/tools/Urho3DEditor-1.6


Create Self-Contained Binary
----------------------------

There is another way! If you set the environment variable ``AIO_COMPONENT_PATH`` aio is running with a different folder as database. So to get the runtime downlaods needed into a specific folder, you do the following steps.

- first run ``aio list alias`` and note the exact URL for the run command, it is ``http://www.hgamer3d.org/tools/Run.0517``
- copy ``aio`` itself to the project folder
- cd into your project folder, create a ``.aio`` folder there
- set AIO_COMPONENT_PATH to this folder
- execute the run command ``aio http://www.hgamer3d.org/tools/Run.0517``, you need the long URL, since alias DB is not existing in the new database
- this will download all executables and media needed into the new folder
- unset AIO_COMPONENT_PATH, otherwise your normal workflow will be disturbed or you download not-needed stuff into your distributable folder
- create a batch script, which sets the AIO_COMPONENT_PATH to the new folder and runs the start script
- this should leave you with a self-sufficient folder to run your program

.. include:: GeneralInclusions
