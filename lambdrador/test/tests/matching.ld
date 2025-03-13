match [[1;2;3]; 2; 3] with
  case (x :: [2;3]) :: xs then
    println x;
    println xs;
    "matched"
