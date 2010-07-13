###----------------------------------------------------------------------
### Copyright (c) 2007-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
###
### Licensed under the Apache License, Version 2.0 (the "License");
### you may not use this file except in compliance with the License.
### You may obtain a copy of the License at
###
###     http://www.apache.org/licenses/LICENSE-2.0
###
### Unless required by applicable law or agreed to in writing, software
### distributed under the License is distributed on an "AS IS" BASIS,
### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
### See the License for the specific language governing permissions and
### limitations under the License.
###
### File    : Makefile
### Purpose : 
###----------------------------------------------------------------------

APPNAME1 = gmt
MODULES1 = \
	gmt_app \
	gmt_charset \
	gmt_cinfo_basic \
	gmt_cli \
	gmt_config \
	gmt_config_svr \
	gmt_elog \
	gmt_elog_policy \
	gmt_event_h \
	gmt_loop \
	gmt_parallel_limit \
	gmt_pmap \
	gmt_sup \
	gmt_sysmon_server \
	gmt_time \
	gmt_tlog_svr \
	gmt_util

PRIVS = \

TESTS = \
	gmt_config_svr_SUITE \
	gmt_time_SUITE

DISABLEDQUICKTESTS = \

# include erl make
include $(shell cat ../.depends-gmt-bom)/make/erl_make.mk

# standard targets
check: all
	-rm -f ./*.log
	$(RUNAPP1) -noinput -noshell -pz $(TEST_DIR) \
		-s gmt_config_svr_SUITE start_test \
		-s gmt_time_SUITE start_test \
		-s erlang halt \
		>> ./check.log

quickcheck: all
	-rm -f ./*.log
	$(RUNAPP1) -noinput -noshell -pz $(QUICKTEST_DIR) \
		-s erlang halt \
		>> ./check.log

clean::
	-rm -f ./*.log

# special targets
