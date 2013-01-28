#!/usr/bin/env bash
#
#	/etc/rc.d/init.d/foo
#
# Starts the foo daemon
#
# chkconfig: 345 95 5
# description: the foo service makes all your dreams come true and \
#    is really quite awesome.
# processname: foo

. /etc/init.d/functions

# this definition belongs in /etc/init.d/functions
function ocaml-init-script {
  cmd="$1"
  subcmd="$2"
  case "$subcmd" in
    start|stop|restart|status)
      $cmd $subcmd; exit $?
      ;;
    *)
      echo $"Usage: $0 {start|stop|restart|status}"
      exit 1
  esac
}

ocaml-init-script '/path/to/exe/for/foo subcommand ... service-subcommand' "$1"

