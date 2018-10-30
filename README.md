Computation of logical bifurcation diagrams
===========================================

The aim of this project is to be able to compute the bifurcation diagram of a given (Boolean or multivalued) logical function. To achieve this it computes the local neighbors of monotone multivalued functions.

This work is a follow-up of [functionhood](https://github.com/ptgm/functionhood), with Claudine Chaouiya and José Cury, where a procedure is proposed to compute the local neighborhood of a given monotone Boolean function.

The current implementation is described in the paper submitted to _Journal of Theoretical Biology_, accounting only for Property 2 of the paper. Property 3 will be addressed in the future.

How to use it?
--------------

To compile it you will need Java6 JDK and [maven](http://maven.apache.org/).

* grab the source from github
* run "mvn package assembly:single" to compile and package it. Please note the **assembly:single** to include all the dependencies ([GINsim](http://ginsim.org) to obtain regulators of a node and [bioLQM](http://github.com/colomoto/biolqm) to obtain the logical function) in the final jar package
* you can use the jar in the "target/" subdirectory.

You can either integrate the .jar file in your tool, or launch the program by command line, providing a GINsim model (.zginml file format) and the name of the component under study, as follows:

    java -jar bifurcation-VERSION-jar-with-dependencies.jar model_file.zgiml ComponentName

Licence
-------

This code is available under GPL-3.0.


Authors
-------

Pedro T. Monteiro - [ptgm](https://github.com/ptgm)

Wassim Abou-Jaoudé - [aboujaoudew](https://github.com/aboujaoudew)
