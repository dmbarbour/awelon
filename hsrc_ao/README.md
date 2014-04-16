
The `ao` executable provides quick, non-interactive access to AO programs. 

Usage:

* `ao help` or any unrecognized command, will display options
* `ao abc command` generate ABC code for AO command (provided as string)
* `ao exec command` execute a specific AO command
* `ao exec.abc command` execute a command provided as ABC
* `ao abc.s` generate ABC output stream from an AO input stream
* `ao exec.s` execute AO code provided on input stream
* `ao exec.abc.s` execute an input stream provided as ABC
* `ao test` process all `test.` words
* `ao type` attempt to detect type errors in dictionary

The `exec` operations run in the same environment as `aoi` uses. Tests run in a confined testing environment. If input is received from the input stream, it is processed incrementally a paragraph at a time. That is, it does not execute until either end-of-input or an LF LF sequence is observed.
