let drum_element_to_mxml_param x =
  (* this is from guitar pro 6 music xml
     that is buggy, as the same notation is given
     for splash and crash_01 for example (the produced
     sound is however different, their code might mix
     notation and midi elements)
   *)
  match x with
    | `Tom_01 -> "A", 4, "normal"
    | `Splash -> "A", 5, "x"
    | `Crash_01 -> "A", 5, "x" (* bug in gpro 6 *)
    | `Tom_02 -> "B", 4, "normal"
    | `China -> "B", 5, "x"
    | `Snare -> "C", 5, "normal"
    | `Snare_muted -> "C", 5, "x"
    | `PedalHiHat -> "D", 4, "x"
    | `Tom_03 -> "D", 5, "normal"
    | `Tom_04 -> "E", 5, "normal"
    | `Kick -> "F", 4, "normal"
    | `Ride -> "F", 5, "x"
    | `Ride_01 -> "F", 5, "x" (* bug in gpro 6 *)
    | `Ride_bell -> "F", 5, "diamond"
    | `Tom -> "G", 4, "normal"
    | `Hihat -> "G", 5, "x"
    | `Hihat_02 -> "G", 5, "circle-x"
    | `Crash -> "G", 5, "x" (* bug in gpro 6 *)
    | `Cowbell  -> failwith("unsupported") (* did not manage to make gpro generate it *)

let drum_element_to_unpitched x =
  let step, octave, _ = drum_element_to_mxml_param x in
  <:xml<
   <unpitched>
   <display-step>$str:step$</display-step>
   <display-octave>$int:octave$</display-octave>
   </unpitched>
   >>

let drum_element_to_notehead x =
  let _, _, head = drum_element_to_mxml_param x in
  <:xml<
   <notehead>$str:head$</notehead>
   >>

type drum_part = {
    part_id: string;
    name: string;
    channel: int;
    unpitched: int;
    volume: int;
    pan: int;
  }

let kick = {
    part_id = "E0-V0";
    name = "Kick_00";
    channel = 10;
    unpitched = 35;
    volume = 80;
    pan = 0;
  }

let snare_00 = {
    part_id = "E1-V0";
    name = "Snare_00";
    channel = 10;
    unpitched = 38;
    volume = 80;
    pan = 0;
  }

let snare_02 = {
    part_id = "E1-V2";
    name = "Snare_02";
    channel = 10;
    unpitched = 37;
    volume = 80;
    pan = 0;
  }

let cowbell_02 = {
    part_id = "E2-V0";
    name = "Cowbell_02";
    channel = 10;
    unpitched = 56;
    volume = 80;
    pan = 0;
  }

let cowbell_01 = {
    part_id = "E3-V0";
    name = "Cowbell_01";
    channel = 10;
    unpitched = 56;
    volume = 80;
    pan = 0;
  }

let cowbell_00 = {
    part_id = "E4-V0";
    name = "Cowbell_00";
    channel = 10;
    unpitched = 56;
    volume = 80;
    pan = 0;
  }

let tom_00 = {
    part_id = "E5-V0";
    name = "Tom_00";
    channel = 10;
    unpitched = 41;
    volume = 80;
    pan = 0;
  }

let tom_01 = {
    part_id = "E6-V0";
    name = "Tom_01";
    channel = 10;
    unpitched = 43;
    volume = 80;
    pan = 0;
  }

let tom_02 = {
    part_id = "E7-V0";
    name = "Tom_02";
    channel = 10;
    unpitched = 45;
    volume = 80;
    pan = 0;
  }

let tom_03 = {
    part_id = "E8-V0";
    name = "Tom_03";
    channel = 10;
    unpitched = 47;
    volume = 80;
    pan = 0;
  }

let tom_04 = {
    part_id = "E9-V0";
    name = "Tom_04";
    channel = 10;
    unpitched = 48;
    volume = 80;
    pan = 0;
  }

let hihat_00 = {
    part_id = "E10-V0";
    name = "Hihat_00";
    channel = 10;
    unpitched = 42;
    volume = 80;
    pan = 0;
  }

let hihat_01 = {
    part_id = "E10-V1";
    name = "Hihat_01";
    channel = 10;
    unpitched = 46;
    volume = 80;
    pan = 0;
  }

let hihat_02 = {
    part_id = "E10-V2";
    name = "Hihat_02";
    channel = 10;
    unpitched = 46;
    volume = 80;
    pan = 0;
  }

let pedalhihat_00 = {
    part_id = "E11-V0";
    name = "PedalHihat_00";
    channel = 10;
    unpitched = 44;
    volume = 80;
    pan = 0;
  }

let crash_00 = {
    part_id = "E12-V0";
    name = "Crash_00";
    channel = 10;
    unpitched = 49;
    volume = 80;
    pan = 0;
  }

let crash_01 = {
    part_id = "E13-V0";
    name = "Crash_01";
    channel = 10;
    unpitched = 57;
    volume = 80;
    pan = 0;
  }

let splash_00 = {
    part_id = "E14-V0";
    name = "Splash_00";
    channel = 10;
    unpitched = 55;
    volume = 80;
    pan = 0;
  }

let ride_00 = {
    part_id = "E15-V0";
    name = "Ride_00";
    channel = 10;
    unpitched = 51;
    volume = 80;
    pan = 0;
  }

let ride_01 = {
    part_id = "E15-V1";
    name = "Ride_01";
    channel = 10;
    unpitched = 59;
    volume = 80;
    pan = 0;
  }

let ride_02 = {
    part_id = "E15-V2";
    name = "Ride_02";
    channel = 10;
    unpitched = 53;
    volume = 80;
    pan = 0;
  }

let china_00 = {
    part_id = "E16-V0";
    name = "China_00";
    channel = 10;
    unpitched = 52;
    volume = 80;
    pan = 0;
  }

let from_drum_element_to_music_xml x =
  match x with
  | `Kick -> kick
  | `Snare -> snare_00
  | `Snare_muted -> snare_02
  | `Cowbell -> cowbell_00
  | `Tom -> tom_00
  | `Tom_01 -> tom_01
  | `Tom_02 -> tom_02
  | `Tom_03 -> tom_03
  | `Tom_04 -> tom_04
  | `Hihat -> hihat_00
  | `Hihat_02 -> hihat_02
  | `PedalHiHat -> pedalhihat_00
  | `Crash -> crash_00
  | `Crash_01 -> crash_01
  | `Splash -> splash_00
  | `Ride -> ride_00
  | `Ride_01 -> ride_01
  | `Ride_bell -> ride_02
  | `China -> china_00

let prefix x res =
  (Printf.sprintf "%d" x) ^ "-" ^ res

let drum_element_to_instrument_line id x =
  let elt = from_drum_element_to_music_xml x in
  let id_without_prefix = elt.part_id in
  <:xml<
    <instrument $alist:["id", prefix id id_without_prefix]$/>
   >>

let drum_parts = [kick;
                  snare_00;
                  snare_02;
                  cowbell_02;
                  cowbell_01;
                  cowbell_00;
                  tom_00;
                  tom_01;
                  tom_02;
                  tom_03;
                  tom_04;
                  hihat_00;
                  hihat_01;
                  hihat_02;
                  pedalhihat_00;
                  crash_00;
                  crash_01;
                  splash_00;
                  ride_00;
                  ride_01;
                  ride_02;
                  china_00]

let create_score_instrument id t =
  let attr = ["id", prefix id t.part_id] in
  <:xml<
   <score-instrument $alist:attr$>
   <instrument-name>$str:t.name$</instrument-name>
   </score-instrument>
   >>

let create_midi_instrument id t =
  let attr = ["id", prefix id t.part_id] in
  <:xml<
   <midi-instrument $alist:attr$>
   <midi-channel>$int:t.channel$</midi-channel>
   <midi-unpitched>$int:t.unpitched$</midi-unpitched>
   <volume>$int:t.volume$</volume>
   <pan>$int:t.pan$</pan>
   </midi-instrument>
   >>

type t = {
    id: int;
    name: string;
    abbrev: string;
    parts: drum_part list;
  }

let drum_part drum =
  let score_instruments = List.map (create_score_instrument drum.id) drum.parts in
  let midi_instruments = List.map (create_midi_instrument drum.id) drum.parts in
  <:xml<
   <score-part $alist:["id", Printf.sprintf "%d" drum.id]$>
   <part-name>$str:drum.name$</part-name>
   <part-abbreviation>$str:drum.abbrev$</part-abbreviation>
   $list:score_instruments$
   $list:midi_instruments$
   </score-part>
   >>

let create_std_drum id = {
    id = id;
    name = "Drumkit";
    abbrev = "Drums";
    parts = drum_parts;
  }

let create_midi_instrument id =
  let drum = create_std_drum id in
  drum_part drum
