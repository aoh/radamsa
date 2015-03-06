#!/bin/bash

THREADS=1
SETSIZE=100
TIMEOUT=10
ARENA=$(pwd)/tournament
FLAGS=""

USAGE="Usage: tournament --arena <test-dir> --timeout <timeout($TIMEOUT)> --setsize <setsize($SETSIZE)> -j <njobs($THREADS)> -m_foo_-p_bar ..."

test $# = 0 && echo "$USAGE" && exit 0

fail() {
   echo "FAIL: $@"
   exit 1;
}

waitnjobs() {
   while true
   do
      test $(jobs | grep Running | wc -l) -lt $1 && break
	      sleep 0.1
   done
}

while [ $# != 0 ]
do
   case "$1" in
      (--help|-h)
         echo "$USAGE"
         exit 0;;
      (--arena|-a)
         test $# == 1 && fail "$1 needs an argument"
         ARENA=$(readlink -f $2)
         test -d $ARENA || fail "$ARENA is not there"
         shift 2;;
      (--timeout|-t)
         test $# == 1 && fail "$1 needs an argument"
         TIMEOUT=$2
         shift 2;;
      (--jobs|-j)
         test $# == 1 && fail "$1 needs an argument"
         THREADS=$2
         shift 2;;
      (--setsize|-n)
         test $# == 1 && fail "$1 needs an argument"
         SETSIZE=$2
         shift 2;;
      (*)
         FLAGS="$@"
         break
         # what this flag?
   esac
done

test -n "$FLAGS" || fail "No flags to compare"

echo "$THREADS threads"
echo "set size $SETSIZE"
echo "timeout $TIMEOUT"
echo "arena is $ARENA"
echo "flags:" 
for foo in $FLAGS
do 
   echo " - $foo" | sed -e 's/_/ /g'; done

ls $ARENA | grep -q "target-" || fail "no $ARENA/target-* found"

for TARGET in $ARENA/target-*
do
   echo "*********************************************************************"
   echo Competing at $TARGET
   cd $TARGET || exit 1
   rm -rf rad-* set-* &>/dev/null
   for ARGS in $FLAGS
   do
      CMD=$(echo "radamsa $ARGS" | sed -e 's/_/ /g')
      START=$(ol -e '(time-ms)')
      $CMD -o rad-%n.%s -n $SETSIZE samples/* &>/dev/null
      ELAPSED=$(ol -e "(- (time-ms) $START)")
      echo "  $CMD took ${ELAPSED}ms"

      for foo in rad-*
      do
        (ulimit -t $TIMEOUT;
         mkdir $foo.dir;
         cp $foo $foo.dir;
         cd $foo.dir;
         ASAN_OPTIONS=detect_leaks=0:coverage=1 ../run "$foo" 2>"../$foo.asan" > /dev/null &
         PID=$!;
         wait;
         $HOME/src/llvm/projects/compiler-rt/lib/sanitizer_common/scripts/sancov.py unpack *.packed &>/dev/null 
         $HOME/src/llvm/projects/compiler-rt/lib/sanitizer_common/scripts/sancov.py print *.$PID.sancov > ../rad-$foo.coverage 2>/dev/null
         #cat "../$foo.asan"
         cd ..;
         ) &
         waitnjobs $THREADS
      done
      cat rad-*.coverage > set-$ARGS.coverage-all
      lines rad-*.coverage > set-$ARGS.coverage
      echo "   $(wc -l < set-$ARGS.coverage) unique, total $(wc -l < set-$ARGS.coverage-all)"
      rm -rf rad-*
   done
   echo "Best average coverages:"
   wc -l set-*.coverage-all | sort -rn | grep -v total | head -n 8
   echo
   echo "Best reached coverages:"
   wc -l set-*.coverage | sort -rn | grep -v total | head -n 8
   echo
   echo "Most unique coverage found:"
   for foo in set-*.coverage
   do
      lines $(ls *.coverage | grep -v "$foo") > others
      lines --difference $foo others > $foo.unique
      rm others
   done
   wc -l set-*.unique | sort -rn | grep -v total | head -n 8 
   echo
   echo
done

# -g_file_-p_od_-m_bd,bf,bi,br,bp,bei,bed,ber -g_file_-p_od_-m_sr,sd -g_file_-p_od_-m_ld,lds,lr,li,lr,ls,lp,lis,lrs -g_file_-p_od_-m_td,tr2,ts1,ts2,tr -g_file_-p_od_-m_num -g_file_-p_od_-m_xp -g_file_-p_od_-m_ft,fn,fo -g_jump_-m_ftk -v


