This project's goal is to support interactive visualization of live
algorithms and data structures.  Think of it as a general-purpose
interactive JavaScript debugger coupled with algorithm-aware
visualization.  There are plenty of data structure visualizations out
there, but to my knowledge, none couple the visualization with a
directly executable description of the algorithm.

Currently, this provides one (rather incomplete) demo of Andersson's
scapegoat tree data structure.  The demo shows the algorithm,
implemented in JavaScript, along with a visualization of the scapegoat
tree's state, and allows the user to single-step through the
algorithm's code while the visualization updates in real time.  Though
not currently implemented, the idea is to extend the visualization to
also show the local state of the algorithm as it executes.

JSJS - A JavaScript-to-JavaScript compiler
------------------------------------------

Did you catch the hard part above?  JavaScript provides no way to
single-step through JavaScript code.  In fact, JavaScript is a
single-stack language and, essentially, the only time the browser's
user interface can refresh is when the JavaScript code has fully
unwound its stack and returned to the browser code.  To make
single-stepping possible, this project includes an (incomplete)
JavaScript-to-JavaScript compiler (written in JavaScript) that
transforms JavaScript code into "interruptable" code that maintains
its own call stack that can be paused and resumed at any point.
Furthermore, the outside environment can inspect a paused call stack
and retrieve the current point in execution and query the values of
local variables.  It can also execute multiple concurrent JavaScript
environments, which could be used to visualize distributed algorithms.

Note that JSJS is not (currently) a fully sandboxed environment, so it
cannot be used to securely execute untrusted JavaScript.  While the
global environment is controlled, compiled code can still access and
modify the prototypes of global types like string.

Limitations
-----------

JSJS is complete enough to run the scapegoat code (and quite a bit
more), but it is still missing several JavaScript features:

* The parser does not implement switch statements, labels, regexp
  literals or semicolon insertion.

* The compiler does not implement compound assignments, for-in
  statements, try, throw, named function expressions, or eval.

* Exceptions that occur while running compiled code do not correctly
  unwind the JSJS execution stack, making exceptions unrecoverable.

* Calls directly to JSJS-compiled functions from outside functions are
  not implemented (though the calling convention does allow for this).

* JSJS only supports strict-mode JavaScript.

* Several smaller things that are documented in the source.
