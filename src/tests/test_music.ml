open OUnit
open Music.Diatonic

let suite name e1 e2 =
  let error = Printf.sprintf "%s == %s" (to_string e1) (to_string e2) in
  let test () = error @? (e1 = e2) in
  name >:: test

let test = "Test_music" >:::
  List.map (fun (name, e1, e2) ->
    suite name e1 e2)
  [
    ("init", {note=`ASharp;
              octave=0},
     shift_n_semitone {note=`A;
                       octave=0} 1);
    ("one_octave", {note=`A;
                    octave=1},
     shift_n_semitone {note=`A;
                       octave=0} 12);
    ("next_do", {note=`C;
                 octave=2},
     shift_n_semitone {note=`A;
                       octave=0} 15);
    ("next_mi", {note=`E;
                 octave=1},
     shift_n_semitone {note=`B;
                       octave=0} 5)]

let _ =
  run_test_tt ~verbose:true test
