To run the unit tests, call `./run-tests nodb-unit-test` or `./run-tests db-unit-test` in the root directory of the repository.

Some database/lens tests can contain verbose output, which can be useful for understanding why a test fails. To enable the verbose output, you can run the unit tests using the verbose flag `v` set to `true`, e.g.:

`./run-tests db -v true`

To run only a single test, first find the test name using:

`./run-tests db -list-test`

And then run:

`./run-tests db -only-test <test name> -v true`
