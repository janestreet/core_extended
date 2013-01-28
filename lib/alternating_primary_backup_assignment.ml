open Core.Std

type 'a t = { primary : 'a; backup : 'a }

let baseline = Time.to_date Time.epoch Time.Zone.utc

let select date ~slot1:a ~slot2:b =
  let n = Date.diff baseline date in
  if n % 2 = 0 then
    {primary = a; backup = b}
  else
    {primary = b; backup = a}

