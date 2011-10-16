

#Module qc#
* [Function Index](#index)
* [Function Details](#functions)




<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#eunit_module-1">eunit_module/1</a></td><td><p>Wrap module as an EUnit test fixture</p>.</td></tr><tr><td valign="top"><a href="#eunit_module-2">eunit_module/2</a></td><td></td></tr><tr><td valign="top"><a href="#eunit_module-3">eunit_module/3</a></td><td></td></tr><tr><td valign="top"><a href="#eunit_module-4">eunit_module/4</a></td><td></td></tr><tr><td valign="top"><a href="#module-2">module/2</a></td><td><p>PropER has a different API than EQC</p>.</td></tr><tr><td valign="top"><a href="#module-2">module/2</a></td><td><p>Same API as EQC</p>.</td></tr><tr><td valign="top"><a href="#silent-1">silent/1</a></td><td><p>Disable QuickCheck[8217,115,32,116,101,115,116,32,111,117,116,112,117,116,32,40,105,46,101,46,
 32,116,104,101,32,34,100,111,116,115,34,41]</p>.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td><p>PropER doesn[8217,116,32,104,97,118,101,32,97,32,115,101,114,118,101,114,46,32,32,65,108,
 119,97,121,115,32,114,101,116,117,114,110,32,116,114,117,101,46]</p>.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td><p>Starts (and possibly restarts) the QuickCheck server. If
another instance is not running, start the server and return the
server[8217,115,32,112,114,111,99,101,115,115,32,105,100,46,32,32,73,102,32,97,110,
 111,116,104,101,114,32,105,110,115,116,97,110,99,101,32,105,115,32,97,108,
 114,101,97,100,121,32,114,117,110,110,105,110,103,44,10,32,32,114,101,116,
 117,114,110,32,116,114,117,101,46,32,32,79,116,104,101,114,119,105,115,101,
 44,32,102,111,114,99,101,102,117,108,108,121,32,114,101,115,116,97,114,116,
 32,116,104,101,32,115,101,114,118,101,114,46]</p>.</td></tr><tr><td valign="top"><a href="#write_counterexample-3">write_counterexample/3</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexample-4">write_counterexample/4</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexamples-1">write_counterexamples/1</a></td><td><p>Write failing counterexamples for specified Module</p>.</td></tr><tr><td valign="top"><a href="#write_counterexamples-2">write_counterexamples/2</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexamples-3">write_counterexamples/3</a></td><td></td></tr><tr><td valign="top"><a href="#write_counterexamples-3">write_counterexamples/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="eunit_module-1"></a>

###eunit_module/1##




`eunit_module(Module) -> any()`



<p>Wrap module as an EUnit test fixture</p>
<a name="eunit_module-2"></a>

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



<p>PropER has a different API than EQC</p>
<a name="module-2"></a>

###module/2##




`module(Options, Module) -> any()`



<p>Same API as EQC</p>
<a name="silent-1"></a>

###silent/1##




`silent(Prop) -> any()`



<p>Disable QuickCheck[8217,115,32,116,101,115,116,32,111,117,116,112,117,116,32,40,105,46,101,46,
 32,116,104,101,32,34,100,111,116,115,34,41]</p>
<a name="start-0"></a>

###start/0##




`start() -> any()`



<p>PropER doesn[8217,116,32,104,97,118,101,32,97,32,115,101,114,118,101,114,46,32,32,65,108,
 119,97,121,115,32,114,101,116,117,114,110,32,116,114,117,101,46]</p>
<a name="start-0"></a>

###start/0##




`start() -> any()`



<p>Starts (and possibly restarts) the QuickCheck server. If
another instance is not running, start the server and return the
server[8217,115,32,112,114,111,99,101,115,115,32,105,100,46,32,32,73,102,32,97,110,
 111,116,104,101,114,32,105,110,115,116,97,110,99,101,32,105,115,32,97,108,
 114,101,97,100,121,32,114,117,110,110,105,110,103,44,10,32,32,114,101,116,
 117,114,110,32,116,114,117,101,46,32,32,79,116,104,101,114,119,105,115,101,
 44,32,102,111,114,99,101,102,117,108,108,121,32,114,101,115,116,97,114,116,
 32,116,104,101,32,115,101,114,118,101,114,46]</p>
<a name="write_counterexample-3"></a>

###write_counterexample/3##




`write_counterexample(Module, Prop, CounterExample) -> any()`

<a name="write_counterexample-4"></a>

###write_counterexample/4##




`write_counterexample(Module, Prop, CounterExample, X4) -> any()`

<a name="write_counterexamples-1"></a>

###write_counterexamples/1##




`write_counterexamples(Module) -> any()`



<p>Write failing counterexamples for specified Module</p>
<a name="write_counterexamples-2"></a>

###write_counterexamples/2##




`write_counterexamples(Module, CounterExamples) -> any()`

<a name="write_counterexamples-3"></a>

###write_counterexamples/3##




`write_counterexamples(Module, CounterExamples, LocalTime) -> any()`

<a name="write_counterexamples-3"></a>

###write_counterexamples/3##




`write_counterexamples(Module, CounterExamples, LocalTime) -> any()`

