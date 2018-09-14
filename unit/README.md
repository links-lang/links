To run the unit tests, call `./run-unit-tests`.

Some tests can contain verbose output, which can be useful for understanding why a test fails. To enable the verbose output, you can run the unit tests using the verbose flag `v` set to `true`, e.g.:

`./run-unit-tests -v true`

To run only a single test, first find the test name using:

`./run-unit-tests -list-test`

And then run:
 
`./run-unit-tests -only_test <test name> -v true`

