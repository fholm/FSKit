namespace FSKit

open System
open System.IO
open System.Text

module Stream =

  let utf8 (stream:Stream) = new StreamReader(stream, Encoding.UTF8)
  let ascii (stream:Stream) = new StreamReader(stream, Encoding.ASCII)
  
  let chars (reader:TextReader) = seq{while true do yield reader.Read()}
  let lines (reader:TextReader) = seq{while true do yield reader.ReadLine()}
  let all (reader:TextReader) = reader.ReadToEnd()

module Console =
  
  let chars = seq{while true do yield Console.Read()}
  let keys = seq{while true do yield Console.ReadKey()}
  let lines = seq{while true do yield Console.ReadLine()}
