 let a = new_float [| 2;1;3 |] 1.0
 let () = set a [| 0;0;0 |]  1.0
 let () = set a [| 0;0;1 |]  2.0
 let () = set a [| 0;0;2 |]  3.0
 let () = set a [| 1;0;0 |]  4.0
 let () = set a [| 1;0;1 |]  5.0
 let () = set a [| 1;0;2 |]  6.0
 let b = new_float [| 1;4;1 |] 2.0
 let () = set b [| 0;0;0 |]  7.0
 let () = set b [| 0;1;0 |]  8.0
 let () = set b [| 0;2;0 |]  9.0
 let () = set b [| 0;3;0 |]  10.0

