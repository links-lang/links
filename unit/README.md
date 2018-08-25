To run the unit tests, simply call `jbuilder runtest`.

Some tests can contain verbose output, which can be useful for understanding why a test fails. To enable the verbose output, you can run the unit tests using the verbose flag `v` set to `true`, e.g.:

`_build/default/unit-tests/unitTests.exe -v true`

To run only a single test, first find the test name using:

`_build/default/unit-tests/unitTests.exe -list-test`

And then run:
 
`_build/default/unit-tests/unitTests.exe -only_test <test name> -v true`

