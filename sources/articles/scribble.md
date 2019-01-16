!=!=! Title: Scribble - Interpreted, Garbage Collected, Programming Language
!=!=! Tags: Projects
!=!=! Created: 1533825900

!=!=! Intro: Start
For my university dissertation I designed and implemented a programming language and virtual machine. The language was designed to make extending existing C++ applications easier and to enable users to extend a programs functionality, increasing the usefulness of a piece of software. The implementation also attempts to reduce the likelihood of security issues by allowing developers to strictly control what functionality the virtual machine exposes to the end user through a simple interface, allowing them to control what access the scripts will have to the host platform.
!=!=! Intro: End

Scribble is strictly typed but supports a simple type inference system which allows for variable types to be inferred from their initial declaration, allowing for the benefits strictly typed languages provide to large projects without too much additional typed overhead. It was originally designed as a procedural language with a simple C like syntax. However as it has grown I have begun to extend it with a simple, experimental, functional syntax which I hope to extend in the future.

The language and accompanying tools are primarily written in C++, the repository also contains a set of examples written in Scribble.
