

# QC - Wrappers for QuickCheck and Proper testing tools #

Copyright (c) 2013-2014 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<p>QC is an Erlang application that provides a set of wrappers for
QuickCheck and Proper testing tools.</p>
<p><em>This repository is a "work-in-progress" - please contribute if you
find QC useful.</em></p>

<h2 id="_quick_start_recipe">Quick Start Recipe</h2>

<p>To download and build the qc application in one shot, please follow
this recipe:</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/norton/qc.git qc
$ cd qc
$ make deps clean compile</code></pre>




<h2 id="_documentation">Documentation</h2>


<h3 id="_where_should_i_start">Where should I start?</h3>
<p>This README is the only bit of documentation right now.</p>
<p>Please see <a href="https://github.com/norton/lets">https://github.com/norton/lets</a> for an Erlang application
that has been developed and tested by using <strong>only</strong> QC.</p>


<h3 id="_what_is_quickcheck">What is QuickCheck?</h3>
<p>QuickCheck is a commercial property-based testing tool for Erlang.</p>
<ul>
<li>
<p>
A <em>language</em> for stating properties of programs (implemented as a
  library of functions and macros).
</p>
</li>
<li>
<p>
A <em>tool</em> for testing properties in randomly generated cases.
</p>
</li>
</ul>
<p>See <a href="http://www.quviq.com/">http://www.quviq.com/</a> for further details.</p>


<h3 id="_what_is_proper">What is Proper?</h3>
<p>PropEr (PROPerty-based testing tool for ERlang) is a
QuickCheck-inspired open-source property-based testing tool for
Erlang.</p>
<p>See <a href="http://proper.softlab.ntua.gr/">http://proper.softlab.ntua.gr/</a> for further details.</p>




<h2 id="_roadmap">Roadmap</h2>

<ul>
<li>
<p>
Documentation and Examples
</p>
</li>
<li>
<p>
PropEr resize issue (<a href="https://github.com/manopapad/proper/issues/10">https://github.com/manopapad/proper/issues/10</a>)
</p>
</li>
</ul>




## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/norton/qc/blob/master/doc/qc.md" class="module">qc</a></td></tr>
<tr><td><a href="https://github.com/norton/qc/blob/master/doc/qc_gen.md" class="module">qc_gen</a></td></tr>
<tr><td><a href="https://github.com/norton/qc/blob/master/doc/qc_gen_http.md" class="module">qc_gen_http</a></td></tr>
<tr><td><a href="https://github.com/norton/qc/blob/master/doc/qc_slave.md" class="module">qc_slave</a></td></tr>
<tr><td><a href="https://github.com/norton/qc/blob/master/doc/qc_statem.md" class="module">qc_statem</a></td></tr></table>

