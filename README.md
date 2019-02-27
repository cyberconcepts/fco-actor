# fco-actor - A Simple Actor Framework for Concurrent Processing

Actors are lightweight processes that communicate via messages.
All messages are sent to and reveived from mailboxes that accept
just one type of message, so these mailboxes may be seen as
typed channels. 

This project is inspired by the programming languages 
[Erlang](http://www.erlang.org/) and 
[Pony](https://www.ponylang.io/), which provide similar 
(though much more elaborate) functionality
built into the language.

There is also some relationship to typed channels as provided by
[Cloud Haskell](http://haskell-distributed.github.io/).

For a simple usage example see the Control.Concurrent.Actor.Console module.

