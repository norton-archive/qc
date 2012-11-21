

#QC - Wrappers for QuickCheck and Proper testing tools#


Copyright (c) 2012 by Gemini Mobile Technologies, Inc.

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).


QC is an Erlang application that provides a set of wrappers for
QuickCheck and Proper testing tools.

_This repository is a "work-in-progress" - please contribute if you
find QC useful._

== Quick Start Recipe

To download and build the qc application in one shot, please follow
this recipe:

------
$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/norton/qc.git qc
$ cd qc
$ make deps clean compile
------

== Documentation

=== Where should I start?

This README is the only bit of documentation right now.

Please see https://github.com/norton/lets for an Erlang application
that has been developed and tested by using *only* QC.

=== What is QuickCheck?

QuickCheck is a commercial property-based testing tool for Erlang.

- A _language_ for stating properties of programs (implemented as a
library of functions and macros).
- A _tool_ for testing properties in randomly generated cases.

See http://www.quviq.com/ for further details.

=== What is Proper?

PropEr (PROPerty-based testing tool for ERlang) is a
QuickCheck-inspired open-source property-based testing tool for
Erlang.

See http://proper.softlab.ntua.gr/ for further details.

== Roadmap

- Documentation and Examples

- PropEr resize issue (https://github.com/manopapad/proper/issues/10)- Support for Triq (http://krestenkrab.github.com/triq/)


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/norton/qc/blob/master/doc/qc.md" class="module">qc</a></td></tr>
<tr><td><a href="https://github.com/norton/qc/blob/master/doc/qc_gen.md" class="module">qc_gen</a></td></tr>
<tr><td><a href="https://github.com/norton/qc/blob/master/doc/qc_gen_http.md" class="module">qc_gen_http</a></td></tr>
<tr><td><a href="https://github.com/norton/qc/blob/master/doc/qc_slave.md" class="module">qc_slave</a></td></tr>
<tr><td><a href="https://github.com/norton/qc/blob/master/doc/qc_statem.md" class="module">qc_statem</a></td></tr>
<tr><td><a href="https://github.com/norton/qc/blob/master/doc/qc_statem_impl.md" class="module">qc_statem_impl</a></td></tr></table>

