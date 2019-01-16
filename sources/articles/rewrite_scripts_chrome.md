!=!=! Title: How to Instrument JavaScript in Chromium
!=!=! Created: 1533847554.552199
!=!=! Tags: JavaScript, Tutorials, Chromium
!=!=! Intro: Start
When analyzing JavaScript software it is common to rewrite or instrument the program in some way in order to expose specific data during execution. Typically instrumentation of JavaScript in the browser is achieved by a proxy - a tool that rewrites JavaScript in network requests. In practice proxies typically do not perform well as there are often multiple entry points to a program and it can be hard to correctly rewrite all cases. Another alternative is modifying V8 to achieve the desired level of program instrumentation. Unfortunately, modern JavaScript interpreters are complex pieces of software and this often carries heavy technical and maintenance overheads. Instead we propose rewriting the JavaScript entry point within a browser to instrument source code, achieving a reasonable compromise between maintainability and development time. In this tutorial we are going to modify V8, the JavaScript interpreter used by Chromium, so that all JavaScript executed by Chromium can be rewritten by an instrumentation framework.
!=!=! Intro: End

### Initial Setup

To begin, we need a working clone of the Chromium source code. There is a good tutorial on building Chromium for the first time [here](https://www.chromium.org/developers/how-tos/get-the-code). Next we need to find the best point in Chromium to rewrite incoming JavaScript. We are looking for a point in Chromium through which all JavaScript source code, as such the V8 JavaScript interpreter is likely the ideal instrumentation point. As such, the file `v8/src/parsing/parse-info.cc` contains an ideal instrumentation point in the function `CreateScript`.

### Rewriting incoming scripts

To rewrite we are going to write the script to a file. Then we will call a separate program with a file path. We expect that file to be rewritten after this program terminates. To simplify development we will use an environment variable in order to pass the path of our instrumentation tool to Chromium:

```auto instrumentPath = getenv("INSTR_PATH");
```

Next we create a stub rewrite function which takes the path and the input source code:

```Handle<String> rewrite(Isolate* isolate, Handle<String> source, char* rewriteCmd) {}
```

We then call our new function at the start of `CreateScript`:

```if (instrumentPath) { 
    source = rewrite(isolate, source, instrumentPath);
}
```

Moving back to our new `rewrite` function, we first need to extract the program source code as a C-style string and construct a temporary file to place it in:

```/** Prepare a temp file for the rewrite and grab a c_str of the string **/
char newFileName[] = "/tmp/chrometmp.XXXXXXXXXXXX";
char* origin = source->ToCString().get();

int fd = mkstemp(newFileName);

//Test for failure
if (fd == -1) {
    return source;
}
```

Next, we need to write the incoming source code into a file so that it can be instrumented:

```FILE* f_path = fopen(newFileName, "w");
    
if (!f_path) {
    printf("Rewrite fopen error\n");
    return source;
} 

if (fwrite(origin, strlen(origin), 1, f_path) != 1) {
    fclose(f_path);
    unlink(newFileName);
    return source;
}

fclose(f_path);
```

Then we rewrite the code by calling our instrumentation program:

```/** Issue a rewrite command **/

std::string cmd = rewriteCmd;
cmd += " ";
cmd += newFileName;

if (system(cmd.c_str())) {
    print("Error rewrite\n");
    return source;
}
```

Finally, we read in the new source code from the temporary file and return it as a V8 string

```/** Re-open the file for reading **/

f_path = fopen(newFileName, "r");

if (!f_path) {
    printf("Rewrite fopen error\n");
    unlink(newFileName);
    return source;
} 

/** Extract the new filesize **/
fseek(f_path, 0, SEEK_END);
size_t fsize = ftell(f_path);
fseek(f_path, 0, SEEK_SET);

/** Read in the new file **/
char* modified = (char*) malloc(fsize + 1);
    
if (fread(modified, fsize, 1, f_path) != 1) {
    printf("Rewriting Error\n");
    free(modified);
    fclose(f_path);
    unlink(newFileName);
    return source;
}

modified[fsize] = 0;

//Release the temporary file
fclose(f_path);
unlink(newFileName);

/** Re-write the handle **/
Handle<String> result = isolate->factory()->NewStringFromAsciiChecked(modified); 
free(modified);

return result;
```

The result of this is a Chromium browser will now write all incoming JavaScript to a file, and then execute whatever is in that file after a instrumentation program executes.

### When To Instrument

Testing this we find that the browser fails to compile. This is due to the instrumentation of native scripts. To test for this we add the condition `natives == NativesFlag::NOT_NATIVES_CODE` to our rewrite condition so that we get:

```if (instrumentPath && natives == NativesFlag::NOT_NATIVES_CODE) { 
    source = rewrite(isolate, source, iPath);
}
```

By adding this condition we ensure that we only rewrite a script if it not part of the Chromium internals.

### Testing

Now we need a way to test whether our rewriting script works in practice. To achieve this lets build a simple bash script that instruments a file with a suffix of `console.log("Hello World");`.

```#!/bin/bash
echo "console.log('Hello World')" >> $1
```

If we execute our Chromium build with the environment variable `INSTR_PATH=our_bash_script` we should see that Hello World is logged as every JavaScript file in parsed within webpages. Unfortunately, instead we see that each file open fails. This is a result of Chromium's sandboxing policy - which prevents the V8 parser of a webpage from writing to temporary files. To disable this policy we need to execute our Chromium instance with the `--no-sandbox` flag. After enabling this flag we should see 'Hello World' in each web page log:

```[74489:775:0814/104653.194963:INFO:CONSOLE(61)] "Hello World", source: https://cdn.static.zdbb.net/eu/js/z0WVjCBSEeGLoxIxOQVEwQ.min.js (61)
```

### Conclusions

Using this approach we can now modify instrumentation frameworks such as Jalangi2 for usage inside browsers with increased compatibility over alternative approaches. We are currently using this approach to modify our dynamic symbolic execution engine ExpoSE for use in browsers. It is important to note that the implementation in this article is very naive. A better solution for heavier workloads would be to use a rewriting server in order to reduce the runtime overhead.
