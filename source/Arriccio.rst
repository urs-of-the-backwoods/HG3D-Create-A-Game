.. _arriccio:

The Arriccio Tool
#################

Programming a game in any language involves a lot of different components to play together, for example game libraries, language bindings, multi media files and so on. All of those pieces need to go somewhere and usually it is quite some work to just figure out where to place them and to setup everything. 

To ease up game programming with Haskell, I created a small utility layer, called `fresco`_ and one of the tools out of this toolbox is the arriccio tool, with the short name ``aio``. The name arriccio is taken from the world of art, it is a specific layer of clay, which is used during the creation of a fresco. The arriccio tool is used to split a larger program into components and manage the bookkeeping on running components. It is based on some of the principles of `dependency injection`_ to separate the components from each other. Arriccio has been heavily inspired by `0install`_. 

.. _`fresco`: http://github.com/urs-of-the-backwoods/fresco
.. _`dependency injection`: https://en.wikipedia.org/wiki/Dependency_injection
.. _`0install`: http://0install.net


The program arriccio can be found in the "tools" folder, it is named "aio".

Fire up arriccio without parameter and you will get a short helping introduction. If you look closer at the arriccio commands you'll see that most of them take the url or a name - an alias - as parameter. The primary key for distinguishing components is the url. Since url's tend to be long and difficult to remember, you can attach an alias for day to day use with arriccio. Please do that now for a number of components which you will need during the next pages. Please enter the following commands to assign the alias names:

.. code-block:: console

	aio alias Stack http://www.hgamer3d.org/component/Stack
	aio alias CreateProject http://www.hgamer3d.org/component/CreateProject
	aio alias Edit http://www.hgamer3d.org/component/Edit
	aio alias Run http://www.hgamer3d.org/component/Run
	aio alias Lua http://www.hgamer3d.org/component/Lua

Let's try now one of those components in isolation for purpose of getting more used to arriccio. Simply issue the following command in a shell:

.. code-block:: console

	aio Lua

What happens? The first time you try to run a component with arriccio it needs to download the component to your machine. So the tool comes back with a question and asks, if you are ok with that. Have a closer look, it gives you a tar.gz filename it is going to download and a signing key. This signing key identifies the person who created the downloaded data, which in this case is me, I signed the components from |HGamer3D| with my key. Arriccio will check if the data has not being messed up somehow by verifying the signature.

If you answer with "yes" a download will appear, and after that the command line of the lua interpreter is displayed. You can try some lua commands and exit with Ctrl-D (Ctrl-C on Windows). 

Try some other commands of arriccio, for example the license command which gives you licensing information on the component you name.

.. note:: There are actually two versions of commands for running components:

.. code-block:: console

	aio <component url> <parameters>
	aio start <component url> <parameters>

The first one starts the component with parameters in the same console with console output. The second version starts a detached program, which does not show console output.


Use in HGamer3D
---------------

In |HGamer3D| different components are implemented for

- The underlying C++ game engine (``Engine``)
- The C++ binding layer towards the runtime (``GameEngineGio``)
- An intermediate runtime, making the binding threadsafe (``Intonaco``)
- The final setup which runs the Haskell program binary with information on where to find dependencies (``Run``)
- Media Packs for media files used by the game engine and the games (``MediaPack1``, ``MediaPlain``)

To separate specifically the Haskell binary from the underlying C++ technologies, this binary loads libraries dynamicall with ``dlopen``. The information, where to find the needed libraries is provided by mechanisms implemented in the arriccio tool. This has the advantage that the Haskell programs itself do not depend on statically linked C, C++ libraries, there is no library mentioned in the cabal file, which increases the maintainability of the overall solution.

The dependency tree looks like this:

.. code-block:: console

	Run
	|
	+--Intonaco
	|
	+--MediaPack1
	|
	+--GameEngineGio
	     |
	     +--Engine
	          |
	          +--MediaPlain



Components
----------

If you are going to stick components together to one working program, you need a kind of resolver, which looks up the current environemnt where those components need to run, gather the fitting pieces together, make them aware of each other and start them. Those steps are performed by arriccio. The entry point to understand how it works is the meta-information, placed at the *component url*, the most important parameter to any arricio command.

**The Component Url**

Components are uniquely identified by their component url. For example the ``Run`` component of |HGamer3D| is identified by ``http://www.hgamer3d.org/component/Run``. This url is used in all places where a reference to this component needs to be made. As a component provides one unique piece of functionality, the name reflects the action or content of the specific component. In the case of the ``Run`` component it is obious that this component enables to run HGamer3D programs. The aio program can be used to start those component, the command:

.. code-block:: console

	aio http://www.hgamer3d.org/component/Run ./game

starts the ``Run`` component and invokes the program ``./game`` to execute it. The reason why a component is started, to start another program is simple, it prepares the environment in a way, that ``./game`` is able to find its dependencies, namely all the media files and the |HGamer3D| runtime libraries.

**The arriccio.toml file**

If you open one of the component url's in your browser you will see, that there is specific information contained in the file, which is opened. This file is called the ``arriccio.toml`` file and the format of it is toml a kind of dialect of yaml. So behind each component url an arriccio.toml file is waiting with meta-information, describing the component. This information is read by the arriccio tool and gives all needed information to execute the component and to resolve any additional dependencies. To give you a deeper understanding of arriccio, we will go through the structure of this file below.

Let's have a look at the first lines of the ``GameEngineGio`` component arriccio.toml file:

.. code-block:: console

	# arriccio.toml file for HGamer3D - game engine gio

	Id = "http://www.hgamer3d.org/component/GameEngineGio"
	Purpose = "HGamer3D Game Engine Gio"
	Description = """
	  The fresco runtime component to bind game engine functionality.
	"""

	License = "Apache 2.0 License"

	FullLicenseText = """
	Copyright 2016 Peter Althainz

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

	    http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.
	"""

	signingkey = "https://www.github.com/urs-of-the-backwoods.keys"


Pretty self-explanatory, is it? The header of the component metadata gives information about the id, the license and the purpose of the component. It also contains a signing-key url. This url contains the public key of a cryptographic key pair and is used by arriccio to verify that the component is originating from the author who owns the corresponding private key. In this case it says that the github user urs-of-the-backwoods signed the arriccio.toml file with his private key. Arriccio checks this before doing anyhting with the component.

Now let's have a look at the further details of the ``arriccio.toml`` file.


.. code-block:: console

	#
	# version 1
	#

	# Linux, 64 Bit, version 1.0.2

	[[Impls]]
	  location = "http://www.hgamer3d.org/downloads/gamegio-amd64-linux-1.0.2.tar.gz"
	  signingkey = "https://www.github.com/urs-of-the-backwoods.keys"
	  version = "1.0.2"
	  architecture = "amd64"
	  os = "linux"

	[[Impls.dependencies]]
	  Id = "http://www.hgamer3d.org/component/Engine"
	  VersionConstraint = "2.0"
	  Environment = ["add-val LD_LIBRARY_PATH /usr/lib/x86_64-linux-gnu", "add-path LD_LIBRARY_PATH ."]


This now is much more interesting. You need to get a little knowledge on toml, before going ahead. The double square brackets indicate arrays with an array entry below it. It is also important to understand that although all blocks have the same indentation the top lines ``Impls`` and ``Impls.dependencies`` indicate that the dependencies are actually part of the Impls, so you need to read this as if the dependencies are one level below the Impls.

So what does it mean? ``Impls`` means *Implementation*. So each ``Impls`` section describes one specific implementation of the component. So in this case, the first lines contain the specification for an implmentation, which runs on machines with Linux as its OS and an *amd64* cpu architecture (essentially 64 bit). The version of this implementation is ``1.0.2``. Finally there is a location indication and a signing-key again which gives the data of the implementation. This data is downloaded by arriccio and cached on the local system so it can be used by other components or directly by arriccio if this component is started.

Below the implementation you will find its dependencies. And this is the most important part in this file. Each specific implementation may depend on other components. Those dependencies are specified by the id of the component, which is needed, its version region and a field called ``Environment``. The dependencies of the ``GameEngineGio`` component is only one component, the ``Engine`` component.

The environment field gives information which is used by arriccio to interconnect the dependencies with the components. Since components have no knowledge about each other, we need to tell the ``GameEngineGio`` component about the existence and location of the ``Engine`` component. This is done at runtime by the arriccio tool. This tool populates the environment with entries giving information about the location of the dependencies. In the case of the ``Engine`` dependency, arriccio modifies an environment variable called ``LD_LIBRARY_PATH`` and adds to it the location where arriccio downloaded the ``Engine`` component. Upon start, the ``GameEngineGio`` component is reading this environment variable and therefore is able to locate the ``Engine`` component. This is a mechanism called *dependency injection* and it decouples components from each other.

And what is this specific component about? The ``GameEngineGio`` component is the C++ binding library, which abstracts functionality from the Urho3D ++ game engine to be used later by the Haskell binding. It is a dynamic library and it uses the Urho3D engine. The latter is actually contained in the ``Engine`` component, so it is clear that the binding library needs the Urho3D engine as a pre-requisite. Arriccio also makes sure, that only the correct versions are interconnected and the fields ``os`` and ``architecture`` make sure, that only components which can be run by the underlying hardware are chosen. As you can see, this also enables one component ``arriccio.toml`` file to specify all implementations for all supported platforms, so there is really just one ``arriccio.toml`` file for one component and it contains all information for all implementations. If you look into the ``http://www.hgamer3d.org/component/GameEngineGio`` component, you will see that there are additional entries for the Windows and OS X platforms.


Running Components
------------------

The arriccio tool runs components. It does so by following the sequence below:

- Read each components ``arriccio.toml`` file in the complete tree. Check signatures.
- Check if there is a combined set of implementations, which are fitting the current platform, architecture and version constraints.
- Take the most up to date versions, fitting the version constraints.
- Download the component implementations of the identified target set to the internal cache, if not already done so. Ask for permission beforehand.
- Check signatures of downloaded components.
- Set all needed environment variables to inject the information on location and parameters of dependencies.
- Execute the given executable in the context of all dependencies, or the command given in the component meta-data itself.


**Version Constraints**

Arriccio uses the given versions to check, which components play well together. According to semantic versioning, the following rules are built-in into the tool and cannot be changed. This means, you need to assign the versions carefully and think beforehand when releasing new versions of components. You also need to bump the major version, when introducing incompatibilities.

- a version needs to have the format MAJOR.MINOR.PATCH .
- a version constraint can limit the version information to less accuracy, specifying only MAJOR.MINOR or even MAJOR only.
- a version constraint fits a version if MAJOR numbers are equal and one of two cases apply:
- either the constrain does contain a PATCH number, then version and version constraint need to be identical 
- or the constraint does not contain a PATCH number, then the version need to be larger or equal to the version constraint
- the highest version, which fulfills the constraints is taken

Some examples migth be appropriate:

======== ============ ==== =======================
Version  Constraint   Ok   Why?
======== ============ ==== =======================
0.2.1    0.2          Yes  version larger then constraint
4.2.1    3.2          No   MAJOR does not match
1.3.2    1.3.0        No   PATCH present in constraint, therefore version need to be identical to constraint
2.3.4    2.2          Yes  MAJOR equal and version larger then constraint
2.1.4    2.2          No   MAJOR equal but version smaller then constraint
======== ============ ==== =======================

*Remark*

There is a tendency in the open source software community to not strictly follow the semantic versioning, but instead only bump the major version number at an outstanding new release each 5-10 years and keep the minor version number as main indicator for new content. This is not working in the case of arriccio at all! Rather you need to bump the major version quite often, since each time you are introducing incompatibilities between versions of components - for the clients of the component - you need to update the MAJOR number. Therefore a good approach is to not be afraid of having higher MAJOR version numbers. You can - and actually should - view the component version as being independent from the software version of the underlying software, which is used in the component.

**Internal Cache**

Arriccio keeps its data in ``~/.aio``. In this folder there is a database of local alias and directory definitions. In addition there are two subfolders, ``cache`` and ``impl``. The ``cache`` folder contains caches of the component arriccio.toml files. Therefore all programs work without internet connection, once a set of component arriccio.toml files have been cached. The ``impl`` folder contains the data of the implementations. Names in those folders are according to the hash values of the component urls for the component meta-data and of the file urls for the implementation data. You can do a ``find`` on the ~/.aio folder and see, how the structure looks like after downloading for example the ``Run`` component.

An update to the cache is not initiated automatically, you need to issue the ``aio update`` command with the respective component url as parameter to force it.

.. note:: Arriccio is not well behaving currently, if there is not enough space in ``~/.aio``. So please make sure you have enough free space available.

**Signatures**

Arriccio uses ssh keys and hashing, to generate a signature for component url files and the implementation files. You can sign files with your private key yourself, by using the following command:

.. code-block:: console

	aio sign <file> ~/.ssh/id_rsa

The resulting signature file needs to be placed at the same location as the file itself. You can check a signature with the ``aio verify`` command. If you place the public key somewhere in the internet everybody can check if the file has been signed by yourself. Arriccio does those checks automatically by comparing if the meta-data files and the implementation content files have a signature file at the same url (with appended ``.sig`` extension) which matches the data and the public key, given in the arriccio.toml file.

**Execution and Commands**

Once arriccio has gathered the information about components and dependencies correctly, downloaded the component data, which will be for a part also some program and also set the right environment variables what does it do, to start the initial component, given in the ``aio`` command? There are two options, which are working. Either there is a ``Command`` field in the implementation section of the component. In this case this command is executed with the directory of the downloaded component cache attached to the front. Remaining parameters are handed over to this started program. Or there is no ``Command`` field in this case, the first parameter is supposed to be a runnable program, which is started. In both cases the context of the program will contain the set environmen variables.

**Environment Parameters**

There are a number of possibilities, to set environment parameters, in the section of the implementation or its dependencies. Let's have a look at one example:

.. code-block:: console

	# first example, Run component

	[[Impls]]
	  version = "1.0.0"
	  architecture = "*"
	  os = "*"

	[[Impls.dependencies]]
	  Id = "http://www.hgamer3d.org/component/MediaPack1"
	  VersionConstraint = "1.0"
	  Environment = ["add-path HG3D_RESOURCE_PATH ."]

	[[Impls.dependencies]]
	  Id = "http://www.hgamer3d.org/component/Intonaco"
	  VersionConstraint = "1.0"
	  Environment = ["add-path INTONACO intonaco.gio"]

	[[Impls.dependencies]]
	  Id = "http://www.hgamer3d.org/component/GameEngineGio"
	  VersionConstraint = "1.0"
	  Environment = ["add-path GIORNATA game_engine.gio"]	

	# second example, GameEngineGio component

	[[Impls]]
	  location = "http://www.hgamer3d.org/downloads/gamegio-amd64-linux-1.0.2.tar.gz"
	  signingkey = "https://www.github.com/urs-of-the-backwoods.keys"
	  version = "1.0.2"
	  architecture = "amd64"
	  os = "linux"

	[[Impls.dependencies]]
	  Id = "http://www.hgamer3d.org/component/Engine"
	  VersionConstraint = "2.0"
	  Environment = ["add-val LD_LIBRARY_PATH /usr/lib/x86_64-linux-gnu", "add-path LD_LIBRARY_PATH ."]	

As you can see in the both examples, the syntax of the ``Environment`` setting is an array of strings and each string has the following possible syntax: ``cmd <environment-variable> <value> [sep]``. Cmd can be ``add-val`` or ``add-path``. The meaning of those commands is simple, add-val adds another value to the given environment variable, with the directory separator or, in case a separator is given, with this separator. Add-path does the same, but it prepends the path to dependency implementation directory in the cache to the given value, so that it can be found within the directory structure of the machine on which the command is running.



Build your own components
-------------------------

You can use arriccio, to build your own components. What you need to do is roughly the following steps:

- Create a compiled executable for each platform and tar it for each platform in an archive.
- Sign the archive with the private key of a ssh keypair and the ``aio sign`` command.
- Put both files, archive and signature on a web-server.
- Create an ``arriccio.toml`` file, place it on a web-server.
- Place your public key on the web-server.
- The web-server need to support the https protocol in addition to http. 

And that is it, you now can use arriccio, to start your own programs.

This works also in case you need additional media files for a program you wrote with |HGamer3D|. You also can distribute the games itself this way. Have a look at the existing components, to see how there ``arriccio.toml`` files are structured.


Command Reference
-----------------

If you start arriccio without any parameter, you will get the following help text:

.. code:: console

	aio (arriccio, all in one) command:

	  aio alias <name> <url>           - gives an alias to a target url
	  aio list alias                   - list given alias
	  aio remove-alias <name> <url>    - removes an alias

	  aio local <url> <local-dir>      - caches a local implementation dir for a target url
	  aio list local                   - list given local dirs
	  aio remove-local <url>           - removes a local implementation dir

	  aio sign <file> <private key>    - create signature for file with private key
	  aio verify <file> <public key>   - verify if signature is correct

	  aio deps <name> | <url>          - prints dependencies
	  aio unsafe <name> | <url> [args] - process a target url without asking for confirmation
	  aio version                      - displays version information

	  aio info <name> | <url>          - prints information about a component
	  aio license <name> | <url>       - prints detailed license information about a component

	  aio update <name> | <url> [args] - updates a target component - re-read url from internet
	  aio start <name> | <url> [args]  - executes a target component - no console
	  aio <name> | <url> [args]        - executes a target component - with console i/o



.. include:: GeneralInclusions
