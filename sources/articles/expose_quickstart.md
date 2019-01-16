!=!=! Title: ExpoSE: A Quick Start Guide
!=!=! Created: 1533896630.641723
!=!=! Tags: Tutorials, ExpoSE, JavaScript
!=!=! Intro: Start
In this tutorial I explain how to use the <a href="/articles/expose">ExpoSE</a> dynamic symbolic execution (DSE) tool. First I run through how to use ExpoSE with some simple JavaScript test-cases and then explain how to interpret the results.
!=!=! Intro: End

### Installation

ExpoSE requires a Node.js installation. Many different versions of Node.js will work, but we test with version v8.11 and recommend to swap to it if there are any issues with installation. We also require that python, git and clang be installed as part of the [Z3Javascript](/articles/z3javascript) build process.

To begin, download a copy of ExpoSE using the command `git clone git@github.com:ExpoSEJS/ExpoSE.git` and then enter the created directory. Once there, execute the command `npm install` to begin the installation process. This can take some time as the Z3 SMT Solver needs to be compiled for use with Node.js. 

### Executing the test suite.

To test ExpoSE is installed properly we can execute the ExpoSE test suite. Simply execute the command: `./expoSE test_suite` and wait. If all of the test cases pass then ExpoSE has installed without issue.

### Constructing a test case

To begin using ExpoSE to analyze JavaScript we first need to construct a test case. A test case is simply a JavaScript program which has been modified to mark some variables as symbolic. To begin, lets start with a simple program:

```var key = ... //Some user input

if (key.length < 5 || !key.match(/[0-9]+/)) {
    throw 'Error in key';
}
```

In this program we are interested in analyzing the impact that changes in the variable `key` can have on control flow. To do this we need to mark that variable as symbol. In ExpoSE this marking is done through the use of a library `S$` and the function `S$.symbol(name, initial_value)`. This function takes a name, used to represent the variable in SMT, and an initial value, which is used for the first test case. The initial value also decides the type of the symbol. The following illustrates how we can use these to modify our example:

```var S$ = require('S$');
var key = S$.symbol('Key', '12345');

if (key.length < 5 || !key.match(/[0-9]+/)) {
    throw 'Error in key';
}
```

Now that we have constructed our test case we can execute it with ExpoSE using the command `./expoSE test_case_filename`.</p>

### Parsing the output

Now that we have executed our test case we need to parse the ExpoSE output. After executing the test we should get some output like:

```*-- Test Case {"_bound":0,"Key":"12345"} Path Condition: (<= 5.0 (to_real (str.len Key))), (str.in.re |0 Fill 0| (re.++ (re.range "0" "9") (re.* (re.range "0" "9")))), (str.in.re |0 Fill 0| (re.++ (re.range "0" "9") (re.* (re.range "0" "9")))), (∨ (¬(str.in.re Key (re.++ (re.++ (re.* (re.range "\x00" "\xff")) (re.++ (re.range "0" "9") (re.* (re.range "0" "9")))) (re.* (re.range "\x00" "\xff"))))) (= Key (str.++ |0 Fill 1| (str.++ |0 Fill 0| |0 Fill 2|)))), (str.in.re Key (re.++ (re.++ (re.* (re.range "\x00" "\xff")) (re.++ (re.range "0" "9") (re.* (re.range "0" "9")))) (re.* (re.range "\x00" "\xff")))) start 0 took 0.0031s
*-- Test Case {"Key":"","_bound":1} Path Condition: ¬(<= 5.0 (to_real (str.len Key))) start 0.0031 took 0.0029s
*-- Errors occured in test {"Key":"","_bound":1}
* Error: Error in key
*-- Replay with expoSE replay '/Users/blake/tc1.js' '{"Key":"","_bound":1}'
*-- Test Case {"Key":"\u0000\u0000\u0000\u0000\u0000","_bound":5} Path Condition: (<= 5.0 (to_real (str.len Key))), (str.in.re |0 Fill 0| (re.++ (re.range "0" "9") (re.* (re.range "0" "9")))), (str.in.re |0 Fill 0| (re.++ (re.range "0" "9") (re.* (re.range "0" "9")))), (∨ (¬(str.in.re Key (re.++ (re.++ (re.* (re.range "\x00" "\xff")) (re.++ (re.range "0" "9") (re.* (re.range "0" "9")))) (re.* (re.range "\x00" "\xff"))))) (= Key (str.++ |0 Fill 1| (str.++ |0 Fill 0| |0 Fill 2|)))), ¬(str.in.re Key (re.++ (re.++ (re.* (re.range "\x00" "\xff")) (re.++ (re.range "0" "9") (re.* (re.range "0" "9")))) (re.* (re.range "\x00" "\xff")))) start 0.0031 took 0.003s
*-- Errors occured in test {"Key":"\u0000\u0000\u0000\u0000\u0000","_bound":5}
* Error: Error in key
*-- Replay with expoSE replay '/Users/blake/tc1.js' '{"Key":"\u0000\u0000\u0000\u0000\u0000","_bound":5}'
*-- Coverage Data
*- File /Users/blake/tc1.js. Coverage (Term): 97% Coverage (Decisions): 100%
*- File /Users/blake/ExpoSE/lib/S$/bin/symbols.js. Coverage (Term): 28% Coverage (Decisions): 5%
*- Re-run with EXPOSE_PRINT_COVERAGE=1 to print line by line coverage information
** ExpoSE Finished. 3 paths with 2 errors **
```

There is a lot of noise in this output so to begin lets investigate the test cases. The first thing to notice is the number of test cases and the number of program errors triggered when executing those test cases. In our example we see that we executed three cases and encountered two runtime exceptions. This makes sense as the control flow in the program is dependent on three conditional operations.

The test cases section details how many test cases executed, the input that test case was executed with, and the SMT query which represented its path condition. In our case we see that ExpoSE has generated three path conditions, one which executes the true branch of the if condition and two which executes the false.

The errors in the output comes from the `throw '...'` in our test script. In the sample output given we see that these errors occur in the test cases which exercise the true branch of the if condition.

Finally, we have program coverage information. This details how much of the program we explored during symbolic execution. In this case we see that we covered 100% of the lines of the program, this means that each line in the program was executed at least once during test-case execution. This does not mean that each test-case touched every single line in the program.

Note that, by default ExpoSE does not include the stdout of each test case in its output. A tutorial on how to enable test case logging is available [here](/articles/expose_high_fidelity_logs).
