(* open Vector *)

type candle = {
price_opn : float;
price_cls : float;
price_hi : float;
price_lo : float;
}

let fun_make_candle price_opn price_cls price_hi price_lo = 
  (* Ensure that high and low prices wrap the open and close prices. *)
  let actual_hi = max price_hi(max price_opn price_cls) in
  let actual_lo = min price_lo(min price_opn price_cls) in
  { price_opn; price_cls; price_hi = actual_hi; price_lo = actual_lo };;

let fun_check_candle candle =
  if candle.price_hi < candle.price_lo then
    false (* Invalid candle *)
  else if candle.price_opn < 0.0 || candle.price_cls < 0.0 || candle.price_hi < 0.0 || candle.price_lo < 0.0 then
    false (* Prices cannot be negative*)
  else
    true (* Valid candle *)

(* Print values of the candlestick *)
let fun_print_candle candle =
  match candle with
  | { price_opn; price_cls; price_hi; price_lo } ->
    Printf.printf "Open: %.2f\n" price_opn;
    Printf.printf "Close: %.2f\n" price_cls;
    Printf.printf "High: %.2f\n" price_hi;
    Printf.printf "Low: %.2f\n" price_lo;;

let test_candle_1 = fun_make_candle 90. 110. 120. 85.
let test_candle_2 = fun_make_candle 90. 110. 20. 85.

let () =
  if fun_check_candle test_candle_1 then fun_print_candle test_candle_1
  else print_endline "err: candle";;

  if fun_check_candle test_candle_2 then fun_print_candle test_candle_1
  else print_endline "err: candle";;
