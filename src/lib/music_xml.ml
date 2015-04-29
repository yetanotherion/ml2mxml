open Cow

let join join_str l =
  List.fold_left (fun accum elt ->
    match accum with
      | "" -> elt
      | _ -> accum ^ join_str ^ elt) "" l

let decl = join "\n" ["<?xml version=\"1.0\" encoding=\"UTF-8\" ?>";
                      "<!DOCTYPE score-partwise PUBLIC '-//Recordare//DTD MusicXML 2.0 Partwise//EN' 'http://www.musicxml.org/dtds/2.0/partwise.dtd'>";
                      ""]

let create_title title =
  <:xml<
  <movement-title>$str:title$</movement-title>
  >>

let create_date ?(name="find it") date =
  <:xml<
  <identification>
    <encoding>
      <encoding-date>$str:date$</encoding-date>
      <software>$str:name$</software>
    </encoding>
  </identification>
  >>

(* XXX cannot use "with xml" as:
   - the node have - in their name
   - ocaml does not allow - in the field of struct-s *)

type midi_instrument = {
  channel: int;
  bank: int;
  program: int;
  volume: int;
  pan: int;
}

let create_midi_instrument_body t =
  <:xml<
  <midi-channel>$int:t.channel$</midi-channel>
  <midi-bank>$int:t.bank$</midi-bank>
  <midi-program>$int:t.program$</midi-program>
  <volume>$int:t.volume$</volume>
  <pan>$int:t.pan$</pan>
  >>

let create_midi_instrument id name abbrev t =
  let attr = ["id", Printf.sprintf "%d" id] in
  <:xml<
  <score-part $alist:attr$>
    <part-name>$str:name$</part-name>
    <part-abbreviation>$str:abbrev$</part-abbreviation>
    <midi-instrument $alist:attr$>
      $create_midi_instrument_body t$
    </midi-instrument>
  </score-part>
  >>


module Mode = struct
  let to_string x =
    match x with
      | `Major -> "major"
end

module Sign = struct
  let to_string x =
    match x with
    | `Tab -> "TAB"
    | `F -> "F"
    | `G -> "G"
    | `Drum -> "percussion"

  let to_line x =
    (* http://www.musicxml.com/UserManuals/MusicXML/MusicXML.htm#EL-MusicXML-clef.htm *)
    match x with
    | `Tab -> 5
    | `F -> 4
    | `G -> 2
    | `Drum -> assert(false)

end

let create_key fifhts mode =
  <:xml<
  <key>
  <fifths>$int:fifhts$</fifths>
  <mode>$str:Mode.to_string mode$</mode>
  </key>
  >>

let create_time nb_beats beat_type =
  <:xml<
  <time>
  <beats>$int:nb_beats$</beats>
  <beat-type>$int:beat_type$</beat-type>
  </time>
  >>

let create_clef sign number =
  let attrlist = ["number", Printf.sprintf "%d" number] in
  <:xml<
   <clef $alist:attrlist$>
   <sign>$str:Sign.to_string sign$</sign>
   <line>$int:Sign.to_line sign$</line>
   </clef>
  >>

let instrument_clef_to_clef x =
  match x with
  | `F -> `F
  | `G -> `G
  | `Drum -> `Drum

module DiatonicScaleStepToTuningStep = struct
  let sharp_flat_to_int x =
    match x with
      | `Sharp -> 1
      | `Flat -> 1

  let note_to_string_alter x =
      match x with
        | `A -> ("A", None)
        | `ASharp -> ("A", Some `Sharp)
        | `B -> ("B", None)
        | `C -> ("C", None)
        | `CSharp -> ("C", Some `Sharp)
        | `D -> ("D", None)
        | `DSharp -> ("D", Some `Sharp)
        | `E -> ("E", None)
        | `F -> ("F", None)
        | `FSharp -> ("F", Some `Sharp)
        | `G -> ("G", None)
        | `GSharp -> ("G", Some `Sharp)

  let to_music_xml x =
    let note_str, sharp_flato = note_to_string_alter x in
    let step = <:xml<
      <tuning-step>$str:note_str$</tuning-step>
      >> in
    let l = [step] in
    match sharp_flato with
      | None -> l
      | Some sharp_flat ->
        let new_elt = <:xml<
          <tuning-alter>$int:sharp_flat_to_int sharp_flat></tuning-alter>
          >> in
        List.rev (new_elt :: l)
end


let create_instrument_staff_lines instrument =
  let open Music in
  let l = Array.to_list instrument.strings in
  let num_lines = List.length l in
  let make_staff_tuning idx step octave =
    let attrs = ["line", Printf.sprintf "%d" (idx + 1)] in
    <:xml<
    <staff-tuning $alist:attrs$>
      $list:(DiatonicScaleStepToTuningStep.to_music_xml step)$
      <tuning-octave>$int:octave$</tuning-octave>
    </staff-tuning>
    >>
  in
  let staff_details =   List.map
    (fun (idx, elt) ->
      make_staff_tuning idx elt.Diatonic.note elt.Diatonic.octave)
    (enumerate l)
  in
  <:xml<
  <staff-details>
    <staff-lines>$int:num_lines$</staff-lines>
    $list:staff_details$
  </staff-details>
  >>

let create_transpose diatonic chromatic octave_change =
  <:xml<
    <transpose>
    <diatonic>$int:diatonic$</diatonic>
    <chromatic>$int:chromatic$</chromatic>
    <octave-change>$int:octave_change$</octave-change>
  </transpose>
   >>

let create_divisions number =
  <:xml<
    <divisions>$int:number$</divisions>
   >>

module BeatUnit = struct
  let to_string x =
    match x with
      | `Quarter -> "quarter"
end

let create_metronome ?(default_y=40) ?(beat_unit=`Quarter) bpm =
  <:xml<
   <direction $alist:[("directive", "yes"); ("placement", "above")]$>
    <direction-type>
     <metronome $alist:[("default-y", Printf.sprintf "%d" default_y); ("parentheses","yes")]$>
      <beat-unit>$str:BeatUnit.to_string beat_unit$</beat-unit>
      <per-minute>$int:bpm$</per-minute>
     </metronome>
    </direction-type>
    <sound $alist:["tempo", Printf.sprintf "%d" bpm]$/>
   </direction>
  >>

let create_pitch instrument played_note =
  let open Music in
  let string = played_note.string in
  let string_empty_note = instrument.strings.(string) in
  let diatonic = Diatonic.shift_n_semitone string_empty_note played_note.fret in
  let step, altero = DiatonicScaleStepToTuningStep.note_to_string_alter diatonic.Diatonic.note in
  let r = <:xml<
    <step>$str:step$</step>
    >>
  in
  let r = [r] in
  let r = match altero with
    | None -> r
    | Some alter ->
      let s = DiatonicScaleStepToTuningStep.sharp_flat_to_int alter in
      let elt = <:xml<
        <alter>$int:s$</alter>
        >> in
      List.rev (elt :: r)
  in
  let pitch = <:xml<
  <pitch>
    $list:r$
    <octave>$int:diatonic.Diatonic.octave$</octave>
  </pitch>
    >> in
  (* XXX when guitar pro opens a music_xml it seems
     the string numbers are upside down *)
  let number_strings = Array.length instrument.strings in
  let curr_string = number_strings - played_note.string in
  let notations = <:xml<
   <notations>
    <technical>
     <string>$int:curr_string$</string>
     <fret>$int:played_note.fret$</fret>
    </technical>
   </notations>
    >>
  in
  pitch, [notations]

let duration_to_string dur =
  match dur with
    | `Eighth -> "eighth"

(* we hardcode that value to 24
   we create all the duration to
   http://www.musicxml.com/tutorial/the-midi-compatible-part/attributes/ *)
let number_of_divions_per_quarter_note = 24

let duration_to_duration dur =
  match dur with
    | `Eighth -> 12

let create_note instrument note =
  let open Music in
  let pitch_or_rest, notations = match note.note with
    | `Played x -> create_pitch instrument x
    | `Rest -> let rest = <:xml<
                 <rest/>
                 >> in
               rest, []
  in
  <:xml<
   <note>
    $pitch_or_rest$
    <duration>$int:duration_to_duration note.duration$</duration>
    <voice>1</voice>
    <type>$str:duration_to_string note.duration$</type>
    <stem>up</stem>
    $list:notations$
   </note>
  >>

let create_measure measure_number instrument notes =
  let notes = List.map (fun note -> create_note instrument note) notes in
  let is_first_measure = measure_number = 0 in
  let attribute = create_clef `Tab 1 in
  let other_attributes =
    if is_first_measure then
      [create_divisions number_of_divions_per_quarter_note;
       create_time 4 4;
       create_key 0 `Major;
       create_instrument_staff_lines instrument;
       create_transpose 0 0 0]
    else []
  in
  let all_attr = attribute :: other_attributes in
  let last_attributes = notes in
  let after_attributes =
    if is_first_measure then
      [create_metronome 184]
    else []
  in
  let before_end = after_attributes @ last_attributes in
  (* XXX Guitar pro does not display five strings
     when adding the F clef, we put these lines in commentary for now
     let instrument_clef = instrument_clef_to_clef instrument.Music.clef in
     $create_clef instrument_clef 1$*)
  let measure_attr = ["number", Printf.sprintf "%d" measure_number] in
  <:xml<
   <measure $alist:measure_attr$>
     <attributes>
      $list:all_attr$
     </attributes>
     $list:before_end$
   </measure>
  >>


type instrument = {
    instrument_id: int;
    midi_instrument: Xml.t;
    music_instrument: Music.string_instrument;
    measures: Music.measure_elt list list;
}

let create_instrument id midi_instrument music_instrument measures =
  {instrument_id = id;
   midi_instrument = midi_instrument id;
   music_instrument = music_instrument;
   measures = measures }

module MidiInstruments = struct
    let bass_midi_instrument = {channel = 3;
                                bank = 1;
                                program = 34;
                                volume = 80;
                                pan = 0}

    let std5_bass id =
      create_midi_instrument id "Electric Bass" "E-Bass5" bass_midi_instrument
    let guitar_midi_instrument = {channel = 1;
                                  bank = 1;
                                  program = 30;
                                  volume = 40;
                                  pan = 0}
    let std_guitar id =
      create_midi_instrument id "Guitar" "E-Guitar" guitar_midi_instrument
  end

let create title date instruments =
  let attr = ["version", "2.0"] in
  let title = create_title title in
  let date = create_date date in
  let partLists = List.map (fun x -> x.midi_instrument) instruments in
  let parts x = List.map (fun (i, measure_notes) ->
                          create_measure i x.music_instrument measure_notes)
                         (Music.enumerate x.measures) in

  let make_part x =
    let attr = ["id", Printf.sprintf "%d" x.instrument_id] in
    let inside_part = parts x in
    <:xml<
    <part $alist:attr$>
      $list:inside_part$
    </part>
  >>
  in
  let all_parts = List.map make_part instruments in
  let xml = <:xml<
    <score-partwise $alist:attr$>
      $title$
      $date$
      <part-list>
       $list:partLists$
      </part-list>
      $list:all_parts$
    </score-partwise>
    >> in
  xml

let to_string xml_elt =
 decl ^ (Cow.Xml.to_string ~decl:false xml_elt)
