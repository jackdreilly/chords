use chords::{Chord, Letter, Note, Scale};
use itertools::join;
fn main() -> Result<(), String> {
    println!(
        "Complex -> chord: {chord}, note: {note}, scale: {scale} letter {letter}",
        letter = "C".parse::<Letter>().map_err(|x| x.to_string())?,
        note = "C".parse::<Note>()?,
        chord = "C".parse::<Chord>()?,
        scale = "C".parse::<Scale>()?,
    );
    println!(
        "Complex -> chord: {chord}, note: {note}, scale: {scale}",
        note = "C#".parse::<Note>()?,
        chord = "C#m7 b9#9\\D".parse::<Chord>()?,
        scale = "C# dorian".parse::<Scale>()?,
    );
    println!(
        "Chord notes: {notes}",
        notes = join("Am7 / F#".parse::<Chord>()?.notes(), ", "),
    );
    println!(
        "Scale notes: {notes}",
        notes = join("C# dorian".parse::<Scale>()?.notes(), ", "),
    );
    println!(
        "Transposition -> chord: {chord}, note: {note}, scale: {scale}, letter: {letter}",
        letter = "C".parse::<Letter>().map_err(|x| x.to_string())? + 1,
        note = "C#".parse::<Note>()? + 1,
        chord = "C#m7 b9#9\\D".parse::<Chord>()? - 3,
        scale = "C# dorian".parse::<Scale>()? + 23,
    );

    Ok(())
}
