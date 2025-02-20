(*bin/main.ml*)

open Candle

let () =
  match Candle.make_candle 150.0 100.0 152.0 90.0 "7-2-2003" "15m" with
  | Ok candle ->
      Printf.printf "Candle (%s): Open=%.2f Close=%.2f High=%.2f Low=%.2f at %s\n"
        candle.range candle.open_price candle.close_price candle.high_price candle.low_price
        candle.timestamp;
      flush stdout
  | Error Candle.HighLessThanLow -> Printf.printf "Error: High < Low\n"; flush stdout
  | Error Candle.NegativePrice -> Printf.printf "Error: Price is negative\n"; flush stdout
  | Error Candle.OutOfPriceWrap -> Printf.printf "Error: Price out of range\n"; flush stdout
