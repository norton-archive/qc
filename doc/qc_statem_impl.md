

# Module qc_statem_impl #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-impl">impl()</a> ###



<pre><code>
impl() = #'?MODULE'{}
</code></pre>





### <a name="type-proplist">proplist()</a> ###



<pre><code>
proplist() = [atom() | {atom(), term()}]
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#command-2">command/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#next_state-4">next_state/4</a></td><td></td></tr><tr><td valign="top"><a href="#postcondition-4">postcondition/4</a></td><td></td></tr><tr><td valign="top"><a href="#precondition-3">precondition/3</a></td><td></td></tr><tr><td valign="top"><a href="#qc_prop-2">qc_prop/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_run-3">qc_run/3</a></td><td></td></tr><tr><td valign="top"><a href="#qc_sample-2">qc_sample/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="command-2"></a>

### command/2 ###

`command(S, ?MODULE) -> any()`


<a name="new-1"></a>

### new/1 ###


<pre><code>
new(Mod::module()) -&gt; <a href="#type-impl">impl()</a>
</code></pre>

<br></br>



<a name="next_state-4"></a>

### next_state/4 ###

`next_state(S, R, C, ?MODULE) -> any()`


<a name="postcondition-4"></a>

### postcondition/4 ###

`postcondition(S, C, R, ?MODULE) -> any()`


<a name="precondition-3"></a>

### precondition/3 ###

`precondition(S, C, ?MODULE) -> any()`


<a name="qc_prop-2"></a>

### qc_prop/2 ###


<pre><code>
qc_prop(Options::<a href="#type-proplist">proplist()</a>, ?MODULE::<a href="#type-impl">impl()</a>) -&gt; any()
</code></pre>

<br></br>



<a name="qc_run-3"></a>

### qc_run/3 ###


<pre><code>
qc_run(NumTests::non_neg_integer(), Options::[{name, string()} | cover | {cover, [module()]} | parallel | noshrink | {sometimes, pos_integer()} | {timeout, timeout()} | any()], ?MODULE::<a href="#type-impl">impl()</a>) -&gt; boolean()
</code></pre>

<br></br>



<a name="qc_sample-2"></a>

### qc_sample/2 ###


<pre><code>
qc_sample(Options::<a href="#type-proplist">proplist()</a>, ?MODULE::<a href="#type-impl">impl()</a>) -&gt; any()
</code></pre>

<br></br>



