!=!=! Title: ExpoSE: Practical Symbolic Execution Of Standalone JavaScript
!=!=! Tags: Projects, Research, Papers
!=!=! Created: 1533834600.512846

!=!=! Intro: Start
JavaScript has evolved into a versatile ecosystem for not just the
web, but also a wide range of server-side and client-side applications.
With this increased scope, the potential impact of bugs increases.
Despite this, testing tools for JavaScript have remained relatively primitive,
largely due to the languages complex implementation and confusing specification.

ExpoSE is a dynamic symbolic execution (DSE) tool for JavaScript with support for
asynchronous events, strings, and complex regular expressions (including capture groups).
It also supports concurrent test-case execution and provides detailed coverage statistics.
!=!=! Intro: End

In DSE, some inputs to the program under test are made
symbolic while the rest are fixed. Starting with an initial concrete assignment
to the symbolic inputs, the DSE engine executes the program both
concretely and symbolically and maintains a symbolic state
that maps program variables to expressions over the symbolic inputs.
Whenever the symbolic execution encounters a conditional opera-
tion, the symbolic state’s evaluation of the condition or its negation
are added to the path condition , depending on the concrete result
of the operation. Once the execution finishes, the path condition
uniquely characterizes the executed control flow path. By negating
the last constraint of the path condition or of one of its prefixes, the
DSE engine generates a constraint for a different path. It then calls
a constraint solver to check feasibility of that path and to obtain a
satisfying assignment for the symbolic input variables that drives
the next execution down that path.

When attempting to use DSE to analyze JavaScript we face some unique challenges. Instrumenting JavaScript interpreters is typically a daunting task due to the complexity of modern JavaScript execution engines. Maintenance of such systems is typically also very difficult, as the base language and interpreters are updated often. Adding to this, the JavaScript type system is based upon a complex set of coercion rules. In practice this means that almost all primitive operations are valid, but can result is strange values. To analyze JavaScript programs correctly a full support for these coercion rules is essential, due to their wide-spread intentional usage in driving program control flow. JavaScript also supports several different mechanisms which allow for run-time code evaluation and execution, such as the eval. As such, the total source code of a program often is not known ahead-of-time. Finally, JavaScript programs heavily rely on string and regular expression functions for typical use-cases. Support for these complex data-types in SMT solvers is incomplete, and support for capture groups and backreferences requires extensive rewriting.

ExpoSE avoids the issue of interpreter instrumentation by using the Jalangi2 framework. Jalangi2 is a source-code instrumentation framework that rewrites a program at the source-code level, rather then instrumenting the interpreter, to achieve analysis. It supports symbolic execution of strings and regular expressions (including backreferences and capture groups) through a complicated encoding in [Z3Javascript](/articles/z3javascript). Code which uses asynchronous callbacks in JavaScript is fully supported, although in some cases the non-deterministic execution of callbacks can cause issues. Execution is parellized through the use of our Distributor program, which leads to dramatic performance increases in practice.

### Publications

* [ExpoSE: Practical Symbolic Execution of Standalone JavaScript](/papers/practical_dse.pdf)
* [Sound Regular Expression Semantics for Dynamic Symbolic Execution of JavaScript](/papers/sound_regex_in_js.pdf)

### Resources

* [GitHub Source Code](http://github.com/ExpoSEJS/)

### Tutorials

* [A Quick Start Guide to ExpoSE](/articles/ExpoSE_A_Quick_Start_Guide)
* [Logging in ExpoSE](/articles/A_Short_Tutorial_on_Logging_in_ExpoSE)

