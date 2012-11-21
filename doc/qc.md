

#Module qc#
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#eunit_module-1">eunit_module/1</a></td><td>Wrap module as an EUnit test fixture.</td></tr><tr><td valign="top"><a href="#eunit_module-2">eunit_module/2</a></td><td></td></tr><tr><td valign="top"><a href="#eunit_module-3">eunit_module/3</a></td><td></td></tr><tr><td valign="top"><a href="#eunit_module-4">eunit_module/4</a></td><td></td></tr><tr><td valign="top"><a href="#module-2">module/2</a></td><td>PropER has a different API than EQC.</td></tr><tr><td valign="top"><a href="#module-2">module/2</a></td><td>Same API as EQC.</td></tr><tr><td valign="top"><a href="#silent-1">silent/1</a></td><td>Disable QuickCheck's test output (i.e.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>PropER doesn't have a server.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Starts (and possibly restarts) the QuickCheck server.</td></tr><tr><td valign="top"><a href="#write_counterexample-3">write_counterexample/3</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexample-4">write_counterexample/4</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexamples-1">write_counterexamples/1</a></td><td>Write failing counterexamples for specified Module.</td></tr><tr><td valign="top"><a href="#write_counterexamples-2">write_counterexamples/2</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexamples-3">write_counterexamples/3</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexamples-3">write_counterexamples/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="eunit_module-1"></a>

###eunit_module/1##


`eunit_module(Module) -> any()`

Wrap module as an EUnit test fixture<a name="eunit_module-2"></a>

###eunit_module/2##


`eunit_module(Module, NumTests) -> any()`

<a name="eunit_module-3"></a>

###eunit_module/3##


`eunit_module(Module, NumTests, Timeout) -> any()`

<a name="eunit_module-4"></a>

###eunit_module/4##


`eunit_module(Module, NumTests, Timeout, Teardown) -> any()`

<a name="module-2"></a>

###module/2##


`module(Options, Module) -> any()`

PropER has a different API than EQC<a name="module-2"></a>

###module/2##


`module(Options, Module) -> any()`

Same API as EQC<a name="silent-1"></a>

###silent/1##


`silent(Prop) -> any()`

Disable QuickCheck's test output (i.e. the "dots")<a name="start-0"></a>

###start/0##


`start() -> any()`

PropER doesn't have a server.  Always return true.<a name="start-0"></a>

###start/0##


`start() -> any()`

Starts (and possibly restarts) the QuickCheck server. If
another instance is not running, start the server and return the
server's process id.  If another instance is already running,
return true.  Otherwise, forcefully restart the server.<a name="write_counterexample-3"></a>

###write_counterexample/3##


`write_counterexample(Module, Prop, CounterExample) -> any()`

<a name="write_counterexample-4"></a>

###write_counterexample/4##


`write_counterexample(Module, Prop, CounterExample, X4) -> any()`

<a name="write_counterexamples-1"></a>

###write_counterexamples/1##


`write_counterexamples(Module) -> any()`

Write failing counterexamples for specified Module<a name="write_counterexamples-2"></a>

###write_counterexamples/2##


`write_counterexamples(Module, CounterExamples) -> any()`

<a name="write_counterexamples-3"></a>

###write_counterexamples/3##


`write_counterexamples(Module, CounterExamples, LocalTime) -> any()`

<a name="write_counterexamples-3"></a>

###write_counterexamples/3##


`write_counterexamples(Module, CounterExamples, LocalTime) -> any()`

