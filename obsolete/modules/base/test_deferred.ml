#use "deferred.ml";;

let dq = init "dq array of int" (fun () -> Array.init 10000 (fun i -> i));;

assert ((get dq).(3) = 3);;
assert ((get dq).(100) = 100);;
assert ((get dq).(34) = 34);;
assert ((get_computation_count dq) == 1);;
reset dq;;
assert ((get dq).(256) = 256);;
assert ((get dq).(435) = 435);;
assert ((get dq).(43) = 43);;
assert ((get_computation_count dq) == 2);;
delete dq;;
try get dq with DeferredDeleted _ -> exit 0;;
assert false
