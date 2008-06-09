#
# Makefile to run all tests for Vim
#

VIMPROG = ../vim-both/src/vim

# Uncomment this line for using valgrind.
# The output goes into a file "valgrind.$PID" (sorry, no test number).
# VALGRIND = valgrind --tool=memcheck --leak-check=yes --num-callers=15 --logfile=valgrind

SCRIPTS = 	regexptest.out test24.out test36.out test43.out \
			test46.out test64.out \
			test44.out

SCRIPTS_GUI = test16.out

.SUFFIXES: .in .out

all: new nongui

new: nolog regexptest.out
	@echo
	@cat test.log
	@echo Regexp test done

64: nolog test64.out
	@echo
	@cat test.log
	@echo TEST 64 DONE

nongui:	nolog $(SCRIPTS)
	@echo
	@cat test.log
	@echo ALL DONE

gui:	nolog $(SCRIPTS) $(SCRIPTS_GUI)
	@echo
	@cat test.log
	@echo ALL DONE

$(SCRIPTS) $(SCRIPTS_GUI): $(VIMPROG)

clean:
	-rm -rf *.out *.failed *.rej *.orig test.log tiny.vim small.vim mbyte.vim test.ok X* valgrind.pid* viminfo

test1.out: test1.in
	-rm -f $*.failed tiny.vim small.vim mbyte.vim test.ok X* viminfo
	$(VALGRIND) $(VIMPROG) -u unix.vim -U NONE --noplugin -s dotest.in $*.in
	@/bin/sh -c "if diff test.out $*.ok; \
		then mv -f test.out $*.out; \
		else echo; \
		echo test1 FAILED - Something basic is wrong; \
		echo; exit 1; fi"
	-rm -rf X* viminfo

.in.out:
	-rm -rf $*.failed test.ok test.out X* viminfo
	cp $*.ok test.ok
	# Sleep a moment to avoid that the xterm title is messed up
	@-sleep .2
	-$(VALGRIND) $(VIMPROG) -u unix.vim -U NONE --noplugin -s dotest.in $*.in
	@/bin/sh -c "if test -f test.out; then\
		  if diff test.out $*.ok; \
		  then mv -f test.out $*.out; \
		  else echo $* FAILED >>test.log; mv -f test.out $*.failed; \
		  fi \
		else echo $* NO OUTPUT >>test.log; \
		fi"
	-rm -rf X* test.ok viminfo

test49.out: test49.vim

test60.out: test60.vim

nolog:
	-echo Test results: >test.log
