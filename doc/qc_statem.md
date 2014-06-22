

# Module qc_statem #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `qc_statem` behaviour.__<br /> Required callback functions: `command/1`, `initial_state/0`, `initial_state/1`, `next_state/3`, `invariant/1`, `precondition/2`, `postcondition/3`, `init/0`, `init/1`, `stop/2`, `aggregate/1`.

<a name="types"></a>

## Data Types ##




### <a name="type-filename">filename()</a> ###



<pre><code>
filename() = <a href="file.md#type-name">file:name()</a>
</code></pre>





### <a name="type-options">options()</a> ###



<pre><code>
options() = [{name, string()} | cover | {cover, [module()]} | parallel | noshrink | {sometimes, pos_integer()} | {timeout, timeout()} | any()]
</code></pre>





### <a name="type-proplist">proplist()</a> ###



<pre><code>
proplist() = <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#qc_check-3">qc_check/3</a></td><td></td></tr><tr><td valign="top"><a href="#qc_check_file-3">qc_check_file/3</a></td><td></td></tr><tr><td valign="top"><a href="#qc_prop-2">qc_prop/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_run-3">qc_run/3</a></td><td></td></tr><tr><td valign="top"><a href="#qc_sample-2">qc_sample/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="behaviour_info-1"></a>

### behaviour_info/1 ###

`behaviour_info(Other) -> any()`


<a name="qc_check-3"></a>

### qc_check/3 ###


<pre><code>
qc_check(Mod::module(), Opts::<a href="#type-proplist">proplist()</a>, CounterExample::term()) -&gt; any()
</code></pre>
<br />


<a name="qc_check_file-3"></a>

### qc_check_file/3 ###


<pre><code>
qc_check_file(Mod::module(), Opts::<a href="#type-proplist">proplist()</a>, FileName::<a href="#type-filename">filename()</a>) -&gt; any()
</code></pre>
<br />


<a name="qc_prop-2"></a>

### qc_prop/2 ###


<pre><code>
qc_prop(Mod::module(), Opts::<a href="#type-proplist">proplist()</a>) -&gt; any()
</code></pre>
<br />


<a name="qc_run-3"></a>

### qc_run/3 ###


<pre><code>
qc_run(Mod::module(), NumTests::non_neg_integer(), Opts::<a href="#type-options">options()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="qc_sample-2"></a>

### qc_sample/2 ###


<pre><code>
qc_sample(Mod::module(), Opts::<a href="#type-proplist">proplist()</a>) -&gt; any()
</code></pre>
<br />


