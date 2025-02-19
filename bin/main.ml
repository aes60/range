(* Q, DO WE REALLY USE VECTORS*)
(* Q, CAN WE IMPLEMETN IHAT AND JHAT? WHY? *)
(* open Vector *)
(* UPDATE -> INTEGRATE fn_check_candle INTO fn_make_candle *)

type candle = {
prc_opn : float;
prc_cls : float;
prc_hi : float;
prc_lo : float;
range : string;
date : string;
}

let fn_make_candle prc_opn prc_cls prc_hi prc_lo = 
  (* Ensure that high and low prcs wrap the open and close prcs. *)
  let actual_hi = max prc_hi(max prc_opn prc_cls) in
  let actual_lo = min prc_lo(min prc_opn prc_cls) in
  { prc_opn; prc_cls; prc_hi = actual_hi; prc_lo = actual_lo };;
let fn_check_candle candle =
  if candle.prc_hi < candle.prc_lo then
    false (* Invalid candle *)
  else if candle.prc_opn < 0.0 || candle.prc_cls < 0.0 || candle.prc_hi < 0.0 || candle.prc_lo < 0.0 then
    false (* prcs cannot be negative*)
  else
    true (* Valid candle *)

let fn_make_candle prc_opn prc_cls prc_hi prc_lo range date =
  let for_wrap_hi = max prc_hi(max prc_opn prc_cls) in
  let for_wrap_lo = max prc_lo(min prc_opn prc_cls) in
  {prc_opn; prc_cls; prc_hi = actual_hi; prc_lo = for_wrap_lo};;


(* Print values of the candlestick *)
let fn_print_candle candle =
  match candle with
  | { prc_opn; prc_cls; prc_hi; prc_lo } ->
    Printf.printf "Open: %.2f\n" prc_opn;
    Printf.printf "Close: %.2f\n" prc_cls;
    Printf.printf "High: %.2f\n" prc_hi;
    Printf.printf "Low: %.2f\n" prc_lo;;

let test_candle_1 = fn_make_candle 90.0 110.0 120.0 85.0
let test_candle_2 = fn_make_candle 90.0 10.0 20.0 85.0

let () =
  if fn_check_candle test_candle_1 then fn_print_candle test_candle_1
  else print_endline "err: candle";;

  if fn_check_candle test_candle_2 then fn_print_candle test_candle_2
  else print_endline "err: candle";;
