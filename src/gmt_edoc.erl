%%%----------------------------------------------------------------------
%%% Copyright (c) 2007-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% File    : gmt_edoc.erl
%%% Purpose : edoc helpers
%%%----------------------------------------------------------------------

%%%
%%% NOTE: A minimial set of "edoc" helpers.
%%%

-module(gmt_edoc).
-include_lib("xmerl/include/xmerl.hrl").

-compile(export_all).

%% @spec () -> ok | whatever
%% @doc Helper function to make it easier to generate HTML Edoc
%% documentation
%%
%% We assume that the current working directory is the app's "src"
%% dir, so if we go up 1 level to find the name of the app's dir,
%% e.g. "foo-app__HEAD", then up one more, <tt>edoc:application()</tt>
%% will work correctly.

make_html() ->
    make_html([undefined]).

%% @spec ([atom()]) -> ok | whatever
%% @doc Helper function to make it easier to generate HTML Edoc
%% documentation
%%
%% We assume that the current working directory is the app's "src"
%% dir, so if we go up 1 level to find the name of the app's dir,
%% e.g. "foo-app__HEAD", then up one more, <tt>edoc:application()</tt>
%% will work correctly.

make_html([ApplicationName]) ->
    {ok, CurrentDir} = file:get_cwd(),

    ok = file:set_cwd(".."),
    {ok, Up1Path} = file:get_cwd(),
    LastDir = filename:basename(Up1Path),
    ok = file:set_cwd(".."),
    Options = [{new, true}, {hidden, true}, {private, true}, {todo, true}],
    Res =
        if ApplicationName =/= undefined ->
                edoc:application(atom_ify(ApplicationName), LastDir, Options);
           true ->
                edoc:application(atom_ify(LastDir), LastDir, Options)
        end,

    ok = file:set_cwd(CurrentDir),
    Res.

%% @spec () -> ok | whatever
%% @doc Helper function to make it easier to generate XML Edoc
%% documentation
%%

make_xml() ->
    make_xml([undefined]).

%% @spec ([atom()]) -> ok | whatever
%% @doc Helper function to make it easier to generate XML Edoc
%% documentation
%%

make_xml([ApplicationName]) ->
    {ok, CurrentDir} = file:get_cwd(),

    ok = file:set_cwd(".."),
    {ok, Up1Path} = file:get_cwd(),
    LastDir = filename:basename(Up1Path),
    ok = file:set_cwd(".."),
    Options = [{new, true}, {hidden, true}, {private, true}, {todo, true}]
        ++ [{layout, ?MODULE}, {dir, LastDir ++ "/doc/xml"}, {file_suffix, ".xml"}],
    Res =
        if ApplicationName =/= undefined ->
                edoc:application(atom_ify(ApplicationName), LastDir, Options);
           true ->
                edoc:application(atom_ify(LastDir), LastDir, Options)
        end,

    ok = file:set_cwd(CurrentDir),
    Res.

%% ---------------------------------------------------------------------

%% @spec atom_ify(X::term()) -> atom()
%% @doc Convert a term to an atom (if convertable).

atom_ify(X) when is_atom(X) ->
    X;
atom_ify(X) when is_binary(X) ->
    list_to_atom(binary_to_list(X));
atom_ify(X) when is_list(X) ->
    list_to_atom(X).

%% ---------------------------------------------------------------------
module(#xmlElement{name = module, content = Es}, _Options) ->
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"],
    Content = [#xmlElement{
      name=package
      , attributes=[#xmlAttribute{name=name,value=[]}]
      , content=[#xmlElement{
          name=modules
          , attributes=[]
          , content=[#xmlElement{
              name=module
              , attributes=[#xmlAttribute{name=name,value=[]}]
              , content=Es
             }]}]}],
    xmerl:export_simple(Content,xmerl_xml,[{prolog,Prolog}]).

package(#xmlElement{name = package, content = Es}, _Options) ->
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"],
    xmerl:export_simple(Es,xmerl_xml, [{prolog,Prolog}]).

overview(#xmlElement{name = overview, content = Es}, _Options) ->
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"],
    xmerl:export_simple(Es,xmerl_xml, [{prolog,Prolog}]).
