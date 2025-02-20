(*bin/candle.ml*)

type candle_time_range =
  | Minutes of int  (* e.g., Minutes 15 for 15m *)
  | Hours of int    (* e.g., Hours 1 for 1h *)
  | Days of int     (* e.g., Days 1 for 1d *)
  | Months of int   (* e.g., Months 1 for 1M *)

type candle = {
  open_price : float;
  close_price : float;
  high_price : float; 
  low_price : float;
  timestamp : string; (* 1. No need for id since we have timestamp for each candle 2. Calendar.t ? *)
  range : string; (* f.e. 15m, 1h, 1M -> needs own type*)
}

type candle_error =
  |HighLessThanLow
  |NegativePrice
  |OutOfPriceWrap
  

(* Constructor function with validation *)
let make_candle open_price close_price high_price low_price timestamp range =
  if high_price < low_price then Error HighLessThanLow
  else if open_price < 0.0 || close_price < 0.0 || high_price < 0.0 || low_price < 0.0 then
    Error NegativePrice
  else if open_price > high_price || open_price < low_price || close_price > high_price || close_price < low_price then
    Error OutOfPriceWrap
  else Ok {
    open_price;
    close_price;
    high_price;
    low_price;
    timestamp;
    range;
  }
