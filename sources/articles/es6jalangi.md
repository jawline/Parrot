!=!=! Title: Analyzing modern JavaScript with Jalangi2
!=!=! Created: 1533832867.3697422
!=!=! Tags: Tutorials

!=!=! Intro: StartAs part of a recent piece of work with ExpoSE we found that Jalangi2 often fails to analyze JavaScript programs which use features from recent JavaScript standards. In particular, a common point of failure is the use of let or const keywords. As such, we were unable to analyze a large number of the libraries we downloaded from the NPM package manager. As we want to be able to execute ExpoSE on real-world software we had to find a modification of Jalangi2 that would permit analysis of such code.
!=!=! Intro: End

Our solution is a simple one. By using the JavaScript transpiler Babel we can rewrite all source-code immediately prior to instrumentation in Jalangi2. To do this we created a modified fork of Jalangi2 with each instrumentation point immediately preceded by a call to the Babel rewriter. We modified Jalangi2 directly instead of calling Babel from the instrumentCodePre callback for two reasons. 1) We wanted a generic solution for all our analyses and 2) We encountered issues with portions of Babel being analyzed by Jalangi2 when imported in the analysis.

When we implemented this approach we found that transpilation time was very slow (upwards of 30 seconds per file). After some debugging we found that this slowdown is caused by Jalangi2 attempting to instrument the entirety of Babel while performing program transpilation. This is due to the lazy evaluation of Babel dependencies at runtime. We fixed this issue by finding every entry-point to Jalangi2 and ensuring that transpilation of an empty source-file is done before the initial analysis is loaded.

One limitation of our modification is that our Jalangi2 fork no longer works when attempting to analyze dynamically evaluated code in a web browser. Luckily, we have some upcoming work which should mitigate this and make analyzing web software with Jalangi2 significantly easier.

The source code available <a href="https://github.com/ExpoSEJS/jalangi2">here</a>.
