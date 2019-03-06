# jsbach-mock
A [Serialbox2](https://github.com/eth-cscs/serialbox2) based test mock of the [JSBACH land model](https://www.mpimet.mpg.de/en/science/models/mpi-esm/jsbach/)

It's not actually a mock in the [Fowler-sense](https://martinfowler.com/articles/mocksArentStubs.html). It has a Capture and a Replay mode. In the Capture mode it calls the original JSBACH interface, records its output, and returns it. In the Replay mode it returns a previous recorded output.

This module is used by the [FortranTestGenerator](https://github.com/fortesg/fortrantestgenerator) template [`IconJsbachMock`](https://github.com/fortesg/fortrantestgenerator/tree/master/templates/IconJsbachMock).
