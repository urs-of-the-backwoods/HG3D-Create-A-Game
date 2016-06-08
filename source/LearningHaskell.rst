Learning Haskell
################

The road to Haskell can be long and windy. To help, there is very good introductory material and excellent "top-notch" material from research available in books and on the internet. Somehow the middle ground is missing, though and it is a question how to best cross the chasm from beginner to master level.

I'm also struggling with that and I worked out the following plan, to advance my own skills. Let me share this, although it migth be not the best plan on earth it is at least a starting point.

Start
-----

I started with a textbook and learned and practiced the basic syntax. The one I took is generally seen as a good reference introductory textbook on Haskell, it is *Programming in Haskell* from Graham Hutton [Hutton]_. It should get you pretty far for all your needs on the basics and a little bit further.

I had some hard times getting a first understanding on the basic typeclasses, like Functor, Monad and so on. There is a great resource for that, which helped me a lot and still does. I'm referring to *Learn You A Haskell for Great Good* from Miran Lipovaca [LYAHGG]_. It is also available free on the Web and very popular and it introduces nicely the functional way of thinking.

Practice
--------

Then I practiced, mostly in the context of game programming and FRP and similar topics. I detected that the next step is a practical knowledge on *do notation* and *Monads*. After mastering this step everyhting was in place to write sequential programs, with functional areas interspersed for computation of values. I'm still mostly programming on this level and it is already a fruitful stage where programs are nicely structured and useful things are happening.

.. note:: Since |HGamer3D| is written by a novice, the API is not highly complex but instead quite simple. With a "Practice" knowledge level you should be able to use it.

Advance
-------

Now comes the issue and I stress it is probably a real one. Haskell has tons of great libraries available to use. Some of them exhibit a more procedural and easy to use interface. Most of them actually exhibit a good usable interface, but they are only digestable if you are at the expert level. But advancing now to the final stages of mastering Haskell is tricky. I'm still fighting here a fierce battle, good tutorials are not so easy to find and understanding the material seems to be difficult. 

In this situation I found an interesting blog from Gabriel Gonzales [GGAHT]_ who has given me some hope. He put together a number of links to good tutorials of advanced topics. Based on this my personal plan now looks like that:

.. _`post`: http://www.haskellforall.com/2014/03/introductions-to-advanced-haskell-topics.html

- master typeclasses (just the basic functor contains a lot of abstraction) and understand it.
- go step by step and learn the content of the tutorials in the post mentioned above.

This should bring me pretty far, I hope. I will tell you here on the progress, if there is some.


**References**

.. [Hutton] Graham Hutton, "Programming in Haskell", January 2007, Cambridge University Press, ISBN: 0521692695.

.. [LYAHGG] Miran Lipovaĉa, "Learn You a Haskell for Great Good, A Beginners Guide", April 2011, No Starch Press, ISBN: 978-1-59327-283-8. Also on the web: http://learnyouahaskell.com.

.. [GGAHT] Gabriel Gonzales, "Introductions to advanced Haskell topics", March 2014, http://www.haskellforall.com/2014/03/introductions-to-advanced-haskell-topics.html.


.. include:: GeneralInclusions
