# grDevices2

Test Bed for Changes and Improvements to Rstats grDevices Base Package.

This package is not a fully functional version of grDevices.  It contains the
bare minimum required to run the functions that are being tested.

There are three branches of interest on this repository:

* level-0: mimics the state of `grDevices` in r-devel >= 75340 c.a. R3.6.0.
* level-1: applies the first "minimal changes" patch.
* level-2: applies level-1 and additional optimizations.

You can install all these branches with the following commands:

```
git clone https://github.com/brodieG/grDevices2.git
cd grDevices2
git checkout level-0
R CMD INSTALL .
git checkout level-1
R CMD INSTALL .
git checkout level-2
R CMD INSTALL . --install-tests
```

Then you can run tests with (note these are not intended for running with R CMD
check, I just put the tests in tests):

```
source('tests/convertColor.R', echo=TRUE)
```
