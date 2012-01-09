

#Module qc_statem#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)





<a name="types"></a>

##Data Types##




###<a name="type-modstate">modstate()</a>##



__abstract datatype__: `modstate()`



###<a name="type-proplist">proplist()</a>##



<pre>proplist() = [atom() | {atom(), term()}]</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#command-1">command/1</a></td><td></td></tr><tr><td valign="top"><a href="#initial_state-0">initial_state/0</a></td><td></td></tr><tr><td valign="top"><a href="#initial_state-1">initial_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#next_state-3">next_state/3</a></td><td></td></tr><tr><td valign="top"><a href="#postcondition-3">postcondition/3</a></td><td></td></tr><tr><td valign="top"><a href="#precondition-2">precondition/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_gen_command-2">qc_gen_command/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_prop-1">qc_prop/1</a></td><td></td></tr><tr><td valign="top"><a href="#qc_prop-2">qc_prop/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_sample-1">qc_sample/1</a></td><td></td></tr><tr><td valign="top"><a href="#qc_sample-2">qc_sample/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="behaviour_info-1"></a>

###behaviour_info/1##




`behaviour_info(X1) -> any()`

<a name="command-1"></a>

###command/1##




`command(S) -> any()`

<a name="initial_state-0"></a>

###initial_state/0##




`initial_state() -> any()`

<a name="initial_state-1"></a>

###initial_state/1##




`initial_state(Mod) -> any()`

<a name="next_state-3"></a>

###next_state/3##




`next_state(S, R, C) -> any()`

<a name="postcondition-3"></a>

###postcondition/3##




`postcondition(S, C, R) -> any()`

<a name="precondition-2"></a>

###precondition/2##




`precondition(S, C) -> any()`

<a name="qc_gen_command-2"></a>

###qc_gen_command/2##




<pre>qc_gen_command(Mod::module(), ModState::<a href="#type-modstate">modstate()</a>) -> any()</pre>
<br></br>


<a name="qc_prop-1"></a>

###qc_prop/1##




<pre>qc_prop(Mod::module()) -&gt; any()</pre>
<br></br>


<a name="qc_prop-2"></a>

###qc_prop/2##




<pre>qc_prop(Mod::module(), Options::<a href="#type-proplist">proplist()</a>) -> any()</pre>
<br></br>


<a name="qc_sample-1"></a>

###qc_sample/1##




<pre>qc_sample(Mod::module()) -&gt; any()</pre>
<br></br>


<a name="qc_sample-2"></a>

###qc_sample/2##




<pre>qc_sample(Mod::module(), Options::<a href="#type-proplist">proplist()</a>) -> any()</pre>
<br></br>


