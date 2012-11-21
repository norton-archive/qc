

#Abstract module qc_statem_impl [MOD]#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

##Data Types##




###<a name="type-proplist">proplist()</a>##



<pre>proplist() = [atom() | {atom(), term()}]</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#command-1">command/1</a></td><td></td></tr><tr><td valign="top"><a href="#next_state-3">next_state/3</a></td><td></td></tr><tr><td valign="top"><a href="#postcondition-3">postcondition/3</a></td><td></td></tr><tr><td valign="top"><a href="#precondition-2">precondition/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_prop-1">qc_prop/1</a></td><td></td></tr><tr><td valign="top"><a href="#qc_run-2">qc_run/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_sample-1">qc_sample/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="command-1"></a>

###command/1##


`command(S) -> any()`

<a name="next_state-3"></a>

###next_state/3##


`next_state(S, R, C) -> any()`

<a name="postcondition-3"></a>

###postcondition/3##


`postcondition(S, C, R) -> any()`

<a name="precondition-2"></a>

###precondition/2##


`precondition(S, C) -> any()`

<a name="qc_prop-1"></a>

###qc_prop/1##


<pre>qc_prop(Options::<a href="#type-proplist">proplist()</a>) -> any()</pre>
<br></br>


<a name="qc_run-2"></a>

###qc_run/2##


<pre>qc_run(NumTests::non_neg_integer(), Options::[{name, string()} | cover | {cover, [module()]} | parallel | noshrink | {sometimes, pos_integer()} | {timeout, timeout()} | any()]) -&gt; boolean()</pre>
<br></br>


<a name="qc_sample-1"></a>

###qc_sample/1##


<pre>qc_sample(Options::<a href="#type-proplist">proplist()</a>) -> any()</pre>
<br></br>


