#!/bin/sh

# Detect supported features and put the result in setup.data

set -e

if uname | grep -q -i linux; then
    linux_possible=true
else
    linux_possible=false
fi

ptimer=`getconf _POSIX_TIMERS || echo undefined`
case $ptimer in
    undefined)
        posix_timers_possible=false
        ;;
    *)
        if [ $ptimer -ge 200111 ]; then
            posix_timers_possible=true
        else
            posix_timers_possible=false
        fi
        ;;
esac

if [ -e setup.data ]; then
    sed '/^\(linux\|posix_timers\)_possible=/d' setup.data > setup.data.new
    mv setup.data.new setup.data
fi

cat >> setup.data <<EOF
linux_possible="$linux_possible"
posix_timers_possible="$posix_timers_possible"
EOF
