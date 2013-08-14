JSJS
----

jsjs.js is an (incomplete) JavaScript-to-JavaScript compiler written
in JavaScript.  Unlike regular JavaScript code, the generated code can
be stopped and resumed at any statement boundary.  When the generated
code is stopped, the caller can inspect the code's environment,
including the full call stack, and the values of local variables.

scapegoat.js is a demo that uses jsjs.js to build an interactive
visualization of a scapegoat tree data structure.  The algorithm is
implemented in JavaScript and executed using JSJS while a simple user
interface lets the user see and single-step through the code while
visualizing the current state of the scapegoat tree.  Though not
currently implemented, the idea is to extend the visualization to also
show the local state of the algorithm as it executes.

Note that JSJS is not currently a fully sandboxed environment, so it
cannot be used to securely execute untrusted JavaScript.  While the
global environment is controlled, compiled code can still access and
modify the prototypes of global types like string.
