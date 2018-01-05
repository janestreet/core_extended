include Extended_common
include Textutils

module Alternating_primary_backup_assignment = Alternating_primary_backup_assignment
module Array = struct
  include Core.Array
  include Extended_array
end
module Atomic_edit = Atomic_edit
module Bin_io_utils = Bin_io_utils
module Bitarray = Bitarray
module Cache = Cache
module Color_print = Color_print
module Service_command = Service_command
module Csv_writer = Csv_writer
module Date = struct
  include Core.Date
  include Extended_time.Extended_date
end
module Deprecated_fcommand = Deprecated_fcommand
module Deprecated_command = Deprecated_command
module Deprecated_service_command = Deprecated_service_command
module English = English
module Environment = Environment
module Documented_match_statement = Documented_match_statement
module Exception_check = Exception_check
module Exn = struct
  include Core.Exn
  include Extended_exn
end
module Extra_fields = Extra_fields
module Fd_leak_check = Fd_leak_check
module Filename = struct
  include Core.Filename
  include Extended_filename
end
module Find = Find
module Fold_map = Fold_map
module Float = struct
  include Core.Float
  include Extended_float
end
module Float_ref = Float_ref
module Gc = struct
  include Core.Gc
  include Extended_gc
end
module Hashtbl = struct
  include Core.Hashtbl
  include Extended_hashtbl
end
module Hashtbl2 = Hashtbl2
module Hashtbl2_pair = Hashtbl2_pair
module Int = struct
  include Core.Int
  include Extended_int
end
module Int32 = struct
  include Core.Int32
  include Extended_int32
end
module Int63 = struct
  include Core.Int63
  include Extended_int63
end
module Int64 = struct
  include Core.Int64
  include Extended_int64
end
module Interval_map = Interval_map
module Invocation = Invocation
module Lazy_sequence = Lazy_sequence
module Low_level_debug = Low_level_debug
module Nativeint = struct
  include Core.Nativeint
  include Extended_nativeint
end
module Number = Number
module Thread = struct
  include Core.Thread
  include Extended_thread
end
module Timed_function = Timed_function
module Lazy_list = Lazy_list
module Lazy_m = Lazy_m
module Linebuf = Linebuf

module Linux_ext = struct
  include Core.Linux_ext
  include Extended_linux
end

module List = struct
  include Core.List
  include Extended_list
end
module List_zipper = List_zipper
module Logger = Logger
module Memo = struct
  include Core.Memo
  include Extended_memo
end
module Multi_map = Multi_map
module Net_utils = Net_utils
module Packed_array = Packed_array
module Packed_map = Packed_map
module Flang = Flang
module Olang = Olang
module Set_lang = Set_lang
module Pp = Pp
module Printc = Printc
module Process = Process
module Procfs = Procfs
module Prod_or_test = Prod_or_test
module Random_selection = Random_selection
module Readline = Readline
module Result = struct
  include Core.Result
  include Extended_result
end
module Runtime_blockout_detector = Runtime_blockout_detector
module Rw_mutex = Rw_mutex
module Sampler = Sampler
module Search = Search_foo
module Semaphore = Semaphore
module Sendmail = Sendmail
module Sexp = struct
  include Core.Sexp
  include Extended_sexp
end
module Shell = Shell
module Sntp = Sntp
module String = struct
  include Core.String
  include Extended_string
end
module String_zipper = String_zipper
module Sys = struct
  include Core.Sys
  include Extended_sys
end
module Sys_utils = Sys_utils
module Tcp = Tcp
module Time = struct
  include Core.Time
  include Extended_time
end
module Unix = struct
  include Core.Unix
  include Extended_unix
  module Pty = struct
    include Unix_pseudo_terminal
  end
end
module Unix_utils = Unix_utils
module Update_queue = Update_queue
