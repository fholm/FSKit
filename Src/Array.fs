namespace FSKit

module Array =

  let appendOne item (array:_ array) =
    let array' = Array.zeroCreate (array.Length+1)
    System.Array.Copy(array, array', array.Length)
    array'.[array.Length] <- item
    array'

