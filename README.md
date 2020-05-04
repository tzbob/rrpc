# Experimental PolyRPC Runtime

## Getting started

To get started with the PolyRPC runtime you need the [scala build tool](https://www.scala-sbt.org) and [npm](https://www.npmjs.com).
Given an appropriate build file, SBT will pull down all required libraries and the appropriate Scala version.
NPM is required since there are some client-side libraries that are used by Scala.js dependencies of the runtime.

> # **Important:** without `npm` or `sbt` none of the following steps will work.

### Get the source code 

```
git pull https://github.com/tzbob/rrpc.git
cd rrpc
sbt
```

This starts up the SBT shell and the first time this will take a long time. It downloads the proper Scala and Scala.JS versions as well as the proper SBT version itself.

Once in the SBT shell all you need to do is execute ```reStart``` or ```~reStart```.
This (asynchronously) runs the webserver, the client code is compiled as a dependency as well (this too will take a while for the first time).
The ```~``` option watches the sources for any changes and recompiles and restarts the webserver automatically.


### Running a PolyRPC Application

All examples are in ```jvm/src/main/resources/examples```, for now this is a fixed path.
Any new examples have to be written in this directory and have to end with the ```.rl``` extension.

To run an example go to http://localhost:8080/<example-name>, e.g., chat.rl can be found on http://localhost:8080/chat/

We currently have the following examples:

- http://localhost:8080/chat
- http://localhost:8080/arrow
- http://localhost:8080/constructor
- http://localhost:8080/counter
- http://localhost:8080/count
- http://localhost:8080/cross
- http://localhost:8080/getorelse
- http://localhost:8080/head
- http://localhost:8080/hellolet
- http://localhost:8080/helloworld
- http://localhost:8080/map
- http://localhost:8080/missingcase
- http://localhost:8080/operators
- http://localhost:8080/ref
- http://localhost:8080/reversehtml
- http://localhost:8080/servercounter_tupled
- http://localhost:8080/stream
- http://localhost:8080/tup

We especially urge the reader to look at the chat example since this showcases a typical PolyRPC application.


To modify an example make your modifications to the ```<example>.rl``` file and **DELETE** the ```<example>.json``` file.
(If you're not running in ```~reStart``` mode then ```reStart``` manually.)

## Structure of the repository

```
├── js
│   └── src
│       ├── main
│       │   └── scala
│       │       └── rpc <<< sources of the client-side runtime
│       └── test
│           └── scala
│               └── rpc <<< tests of the client-side runtime
├── jvm
│   └── src
│       ├── main
│       │   ├── resources
│       │   │   ├── examples <<< examples to run
│       │   │   ├── linux    <<< linux binary for polyrpc
│       │   │   └── windows  <<< windows binary for polyrpc
│       │   └── scala
│       │       └── rpc      <<< sources of the server-side runtime
│       └── test
│           └── scala
│               └── rpc      <<< tests of the server-side runtime
└── shared
    └── src
        ├── main
        │   └── scala
        │       ├── examples <<< main class which runs all examples
        │       └── rpc      <<< all sources shared by client and server runtime (e.g., interpreter)
        └── test
            └── scala
                └── rpc      <<< all runtime agnostic tests
```
