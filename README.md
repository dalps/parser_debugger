## Breakpoints

There are three kinds of breakpoint can be set at three different levels in a grammar:
- Token level: these also accept an optional string payload. The debugger will stop when it encounters this token. If a string payload is specified, it will stop on this token if the matched string equals the payload.
- Producer level (don't know)
- Production level: the debugger will stop on this production when it gets reduced.
- Rule level: the debugger will stop when any production of this rule gets reduced.