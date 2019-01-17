!=!=! Title: A Short Tutorial on Logging in ExpoSE
!=!=! Created: 1533847431.223146
!=!=! Tags: ExpoSE, Tutorials, JavaScript
!=!=! Intro: Start
The concurrent execution of test-cases and JavaScript compilation process can make logging test-case output confusing in ExpoSE. In this short tutorial I explain how to enable test-case logging and give quick configurations.
!=!=! Intro: End

Before beginning it should be noted that all changes to logging should be executed with the rebuild flag set. Recompilation is required when modifying the log level as we remove all calls to logging during compilation to improve the performance of the test-case execution. We found that this often leads to 3-4x faster executions over an if condition. A rebuild of ExpoSE can be triggered by using the `REBUILD=1` environment variable like so:

```REBUILD=1 expoSE ...```

By default ExpoSE does not output per-path logging. This is disabled because ExpoSE can be executing many test-cases concurrently. The output of these test-cases often gets interleaved and as such can just become noise. By using the `EXPOSE_PRINT_PATHS=1` environment variable per-path logging can be re-enabled. For example the following command can be used:

```EXPOSE_PRINT_PATHS=1 ./expoSE ...```

With test-case logging enabled the concurrent execution of test-cases can add a lot of noise into the data. To avoid this, the `EXPOSE_MAX_CONCURRENT` environment flag can be used to limit the number of test-cases being executed concurrently. For example, the following command limits concurrent execution to a single test case:

```EXPOSE_MAX_CONCURRENT=1 ./expoSE ...```

ExpoSE supports 4 logging modes, set through the `EXPOSE_LOG_LEVEL` environment variable.

* Level `0` is no logging at all, and will just include the original programs stdout.
* Level `1` is standard ExpoSE logging and includes primitive runtime information.
* Levels `2` and `3` are high fidelity logging modes, and include large amounts of debugging information. Executing ExpoSE with these modes has a dramatic impact on performance and is not recommended unless you are debugging the interpreter itself.

The `EXPOSE_LOG_LEVEL` variable can be set using the following command:

```EXPOSE_LOG_LEVEL=2 REBUILD=1 ./expoSE ...```

### Quick Configurations:

By configuring all of these variables ExpoSE can be configured to log in a variety of ways. Below are some quick configurations you may want to execute ExpoSE with.

Return to default behavior:

```REBUILD=1 ./expoSE ...```

Extract test case output with no ExpoSE logging:

```REBUILD=1 EXPOSE_MAX_CONCURRENT=1 EXPOSE_LOG_LEVEL=0 EXPOSE_PRINT_PATHS=1 ./expoSE ...```

View ExpoSE's interpretation of a program control flow:

```REBUILD=1 EXPOSE_MAX_CONCURRENT=1 EXPOSE_LOG_LEVEL=1 EXPOSE_PRINT_PATHS=1 ./expoSE ...```

Prepare a log for issue submission:

```REBUILD=1 EXPOSE_MAX_CONCURRENT=1 EXPOSE_LOG_LEVEL=3 EXPOSE_PRINT_PATHS=1 ./expoSE ...```

Note that in each of these examples we limit the number of concurrent tests executing using the `EXPOSE_MAX_CONCURRENT` environment variable. If this restriction is removed performance will increase at the cost of noise in the output.
