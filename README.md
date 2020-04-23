# Experimental PolyRPC Runtime

## Getting started

To get started with the PolyRPC runtime you need the [scala build tool](https://www.scala-sbt.org) and [npm](https://www.npmjs.com).
Given an appropriate build file, SBT will pull down all required libraries and the appropriate Scala version.
NPM is required since there are some client-side libraries that are used by Scala.js dependencies of the runtime.

> **Important:** without `npm` or `sbt` none of the following steps will work.

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

To run an example go to http://localhost:8080/<example-name>, e.g., for ```arrow.rl```: http://localhost:8080/arrow (note the absence of ```.rl```).

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
