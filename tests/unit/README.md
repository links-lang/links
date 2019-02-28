To run the unit tests, call `./run-tests lens` in the root directory of the repository.

Some tests can contain verbose output, which can be useful for understanding why a test fails. To enable the verbose output, you can run the unit tests using the verbose flag `v` set to `true`, e.g.:

`./run-tests lens -v true`

To run only a single test, first find the test name using:

`./run-tests lens -list-test`

And then run:

`./run-tests lens -only-test <test name> -v true`

