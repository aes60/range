type candle = {
  open_p : float;
  close_p : float;
  high_p : float; 
  low_p : float;
  timestamp : string; (* 1. No need for id since we have timestamp for each candle 2. Calendar.t ? *)
  range : string; (* f.e. 15m, 1h, 1M -> needs own type*)
}

type candle_error =
  |HighLessThanLow
  |NegativePrice
  |OutOfPriceWrap
  

(* Constructor function with validation *)
let make_candle open_p close_p high_p low_p timestamp range =
  if high_p < low_p then Error HighLessThanLow
  else if open_p < 0.0 || close_p < 0.0 || high_p < 0.0 || low_p < 0.0 then
    Error NegativePrice
  else if open_p > high_p || open_p < low_p || close_p > high_p || close_p < low_p then
    Error OutOfPriceWrap
  else Ok {
    open_p;
    close_p;
    high_p;
    low_p;
    timestamp;
    range;
  }
