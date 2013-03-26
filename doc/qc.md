

# Module qc #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#eunit_module-1">eunit_module/1</a></td><td><p>Wrap module as an EUnit test fixture</p>.</td></tr><tr><td valign="top"><a href="#eunit_module-2">eunit_module/2</a></td><td></td></tr><tr><td valign="top"><a href="#eunit_module-3">eunit_module/3</a></td><td></td></tr><tr><td valign="top"><a href="#eunit_module-4">eunit_module/4</a></td><td></td></tr><tr><td valign="top"><a href="#module-2">module/2</a></td><td><p>PropER has a different API than EQC</p>.</td></tr><tr><td valign="top"><a href="#module-2">module/2</a></td><td><p>Triq has a different API than EQC</p>.</td></tr><tr><td valign="top"><a href="#module-2">module/2</a></td><td><p>Same API as EQC</p>.</td></tr><tr><td valign="top"><a href="#module-2">module/2</a></td><td><p>Same API as EQC</p>.</td></tr><tr><td valign="top"><a href="#silent-1">silent/1</a></td><td><p>Disable QuickCheck's test output (i.e. the "dots")</p>.</td></tr><tr><td valign="top"><a href="#silent-1">silent/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td><p>PropER doesn't have a server.  Always return true.</p>.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td><p>Triq doesn't have a server.  Always return true.</p>.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td><p>Starts (and possibly restarts) the QuickCheck server. If
another instance is not running, start the server and return the
server's process id.  If another instance is already running,
return true.  Otherwise, forcefully restart the server.</p>.</td></tr><tr><td valign="top"><a href="#write_counterexample-3">write_counterexample/3</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexample-4">write_counterexample/4</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexamples-1">write_counterexamples/1</a></td><td><p>Write failing counterexamples for specified Module</p>.</td></tr><tr><td valign="top"><a href="#write_counterexamples-2">write_counterexamples/2</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexamples-3">write_counterexamples/3</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexamples-3">write_counterexamples/3</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexamples-3">write_counterexamples/3</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexamples-3">write_counterexamples/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="eunit_module-1"></a>

### eunit_module/1 ###

`eunit_module(Module) -> any()`

<p>Wrap module as an EUnit test fixture</p>

<a name="eunit_module-2"></a>

### eunit_module/2 ###

`eunit_module(Module, NumTests) -> any()`


<a name="eunit_module-3"></a>

### eunit_module/3 ###

`eunit_module(Module, NumTests, Timeout) -> any()`


<a name="eunit_module-4"></a>

### eunit_module/4 ###

`eunit_module(Module, NumTests, Timeout, Teardown) -> any()`


<a name="module-2"></a>

### module/2 ###

`module(Options, Module) -> any()`

<p>PropER has a different API than EQC</p>

<a name="module-2"></a>

### module/2 ###

`module(Options, Module) -> any()`

<p>Triq has a different API than EQC</p>

<a name="module-2"></a>

### module/2 ###

`module(Options, Module) -> any()`

<p>Same API as EQC</p>

<a name="module-2"></a>

### module/2 ###

`module(Options, Module) -> any()`

<p>Same API as EQC</p>

<a name="silent-1"></a>

### silent/1 ###

`silent(Prop) -> any()`

<p>Disable QuickCheck's test output (i.e. the "dots")</p>

<a name="silent-1"></a>

### silent/1 ###

`silent(Prop) -> any()`


<a name="start-0"></a>

### start/0 ###

`start() -> any()`

<p>PropER doesn't have a server.  Always return true.</p>

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

<p>Triq doesn't have a server.  Always return true.</p>

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

<p>Starts (and possibly restarts) the QuickCheck server. If
another instance is not running, start the server and return the
server's process id.  If another instance is already running,
return true.  Otherwise, forcefully restart the server.</p>

<a name="write_counterexample-3"></a>

### write_counterexample/3 ###

`write_counterexample(Module, Prop, CounterExample) -> any()`


<a name="write_counterexample-4"></a>

### write_counterexample/4 ###

`write_counterexample(Module, Prop, CounterExample, X4) -> any()`


<a name="write_counterexamples-1"></a>

### write_counterexamples/1 ###

`write_counterexamples(Module) -> any()`

<p>Write failing counterexamples for specified Module</p>

<a name="write_counterexamples-2"></a>

### write_counterexamples/2 ###

`write_counterexamples(Module, CounterExamples) -> any()`


<a name="write_counterexamples-3"></a>

### write_counterexamples/3 ###

`write_counterexamples(Module, CounterExamples, LocalTime) -> any()`


<a name="write_counterexamples-3"></a>

### write_counterexamples/3 ###

`write_counterexamples(Module, CounterExamples, LocalTime) -> any()`


<a name="write_counterexamples-3"></a>

### write_counterexamples/3 ###

`write_counterexamples(Module, CounterExamples, LocalTime) -> any()`


<a name="write_counterexamples-3"></a>

### write_counterexamples/3 ###

`write_counterexamples(Module, CounterExamples, LocalTime) -> any()`


