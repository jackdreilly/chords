use std::{
    fmt::{Debug, Display},
    ops::{Add, Neg, Sub},
    str::FromStr,
    vec,
};

use derive_builder::Builder;
use itertools::Itertools;
use num::{FromPrimitive, ToPrimitive};
use num_derive::{FromPrimitive, ToPrimitive};
use serde::{Deserialize, Serialize};
use strum::{Display, EnumCount, EnumString};
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    EnumString,
    Display,
    EnumCount,
    FromPrimitive,
    num_derive::ToPrimitive,
    Hash,
    Serialize,
    Deserialize,
)]
#[strum(ascii_case_insensitive)]
#[repr(u8)]
pub enum Letter {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
}
#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumString, Display, Hash, Serialize, Deserialize)]
#[strum(ascii_case_insensitive)]
pub enum ScaleInterval {
    #[strum(serialize = "2")]
    Two,
    #[strum(serialize = "4")]
    Four,
    #[strum(serialize = "5")]
    Five,
    #[strum(serialize = "6")]
    Six,
    #[strum(serialize = "7")]
    Seven,
    #[strum(serialize = "9")]
    Nine,
    #[strum(serialize = "11")]
    Eleven,
    #[strum(serialize = "13")]
    Thirteen,
}

impl ScaleInterval {
    fn as_scale_interval(&self) -> u8 {
        match self {
            ScaleInterval::Two => 2,
            ScaleInterval::Four => 4,
            ScaleInterval::Five => 5,
            ScaleInterval::Six => 6,
            ScaleInterval::Seven => 7,
            ScaleInterval::Nine => 2,
            ScaleInterval::Eleven => 4,
            ScaleInterval::Thirteen => 6,
        }
    }

    fn as_literal(&self) -> u8 {
        match self {
            ScaleInterval::Two => 2,
            ScaleInterval::Four => 4,
            ScaleInterval::Five => 5,
            ScaleInterval::Six => 6,
            ScaleInterval::Seven => 7,
            ScaleInterval::Nine => 9,
            ScaleInterval::Eleven => 11,
            ScaleInterval::Thirteen => 13,
        }
    }

    fn is_dominant(&self) -> bool {
        self.as_literal() > 7
    }
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    EnumString,
    Display,
    Hash,
    EnumCount,
    ToPrimitive,
    Serialize,
    Deserialize,
)]
#[strum(ascii_case_insensitive)]
pub enum Mode {
    #[strum(
        serialize = "",
        serialize = "major",
        serialize = "maj",
        serialize = "ionian",
        to_string = ""
    )]
    Ionian,
    Dorian,
    Phrygian,
    Lydian,
    Mixolydian,
    #[strum(
        serialize = "m",
        serialize = "min",
        serialize = "minor",
        serialize = "aeolian",
        to_string = "m"
    )]
    Aeolian,
    #[strum(
        serialize = "dim",
        serialize = "diminished",
        serialize = "locrian",
        to_string = "dim"
    )]
    Locrian,
    Augmented,
}

impl Default for Mode {
    fn default() -> Self {
        Mode::Ionian
    }
}

const SCALE_INCREMENTS: [u8; 7] = [2, 2, 1, 2, 2, 2, 1];
impl Mode {
    fn increments(&self) -> Vec<u8> {
        match self {
            Mode::Augmented => vec![2, 2, 2, 2, 1, 1, 2],
            _ => SCALE_INCREMENTS
                .iter()
                .copied()
                .cycle()
                .skip(self.to_usize().unwrap())
                .take(6)
                .collect_vec(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Builder, Serialize, Deserialize)]
pub struct Scale {
    pub root: Note,
    #[builder(default)]
    pub mode: Mode,
}

impl Display for Scale {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{root}{mode}", root = self.root, mode = self.mode)
    }
}

impl FromStr for Scale {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (root, s) = s.scan()?;
        let (mode, s) = s.trim().scan()?;
        if !s.is_empty() {
            return Err(format!("Leftover {s}"));
        }
        Ok(Self { root, mode })
    }
}

impl Scale {
    pub fn notes(&self) -> Vec<Note> {
        self.mode
            .increments()
            .into_iter()
            .fold(vec![self.root], |mut notes, inc| {
                notes.push(*notes.last().unwrap() + inc);
                notes
            })
    }
}

fn cyclical_add<E: EnumCount + Debug + FromPrimitive + num_traits::ToPrimitive, N: ToPrimitive>(
    e: E,
    n: N,
) -> E {
    let my_int32 = e.to_i32().unwrap();
    let their_int32: i32 = n.to_i32().unwrap();
    let sum = my_int32 + their_int32;
    let remainder = sum.rem_euclid(E::COUNT as i32);
    E::from_u8(remainder as u8).unwrap()
}

impl<N: ToPrimitive> Add<N> for Letter {
    type Output = Self;

    fn add(self, rhs: N) -> Self::Output {
        cyclical_add(self, rhs)
    }
}

#[derive(
    Debug, PartialEq, Eq, Clone, Copy, EnumString, Display, Hash, EnumCount, Serialize, Deserialize,
)]
#[strum(ascii_case_insensitive)]
pub enum Accidental {
    #[strum(serialize = "#", serialize = "♯", to_string = "#")]
    Sharp,
    #[strum(serialize = "b", serialize = "♭", to_string = "b")]
    Flat,
    #[strum(to_string = "", serialize = "", serialize = "♮")]
    Natural,
}

impl ToPrimitive for Accidental {
    fn to_i64(&self) -> Option<i64> {
        Some(match self {
            Accidental::Sharp => 1,
            Accidental::Flat => -1,
            Accidental::Natural => 0,
        })
    }

    fn to_u64(&self) -> Option<u64> {
        Some(match self {
            Accidental::Sharp => 1,
            Accidental::Flat => 11,
            Accidental::Natural => 0,
        })
    }
}

impl Default for Accidental {
    fn default() -> Self {
        Accidental::Natural
    }
}

#[derive(Debug, Clone, Copy, Builder, Serialize, Deserialize)]
pub struct Note {
    pub letter: Letter,
    #[builder(default)]
    pub accidental: Accidental,
}

impl std::hash::Hash for Note {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let a = self.standardize_flat();
        a.letter.hash(state);
        a.accidental.hash(state);
    }
}

impl PartialEq for Note {
    fn eq(&self, other: &Self) -> bool {
        let (a, b) = (self.standardize_flat(), other.standardize_flat());
        a.letter == b.letter && a.accidental == b.accidental
    }
}
impl Eq for Note {}

impl<N: ToPrimitive> Add<N> for Note {
    type Output = Self;

    fn add(self, rhs: N) -> Self::Output {
        cyclical_add(self, rhs)
    }
}

impl EnumCount for Note {
    const COUNT: usize = 12;
}

impl FromPrimitive for Note {
    fn from_i64(n: i64) -> Option<Self> {
        n.to_u64().and_then(Self::from_u64)
    }

    fn from_u64(n: u64) -> Option<Self> {
        n.to_u8().map(Self::from)
    }
}

impl ToPrimitive for Note {
    fn to_i64(&self) -> Option<i64> {
        self.to_u64().map(|f| f as i64)
    }

    fn to_u64(&self) -> Option<u64> {
        Some(u8::from(self) as u64)
    }
}

impl Display for Note {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { letter, accidental } = self;
        write!(f, "{letter}{accidental}")
    }
}

impl Note {
    fn standardize_flat(&self) -> Self {
        u8::from(self).into()
    }
}

static FLAT_NOTES: [Note; 12] = [
    Note {
        letter: Letter::C,
        accidental: Accidental::Natural,
    },
    Note {
        letter: Letter::D,
        accidental: Accidental::Flat,
    },
    Note {
        letter: Letter::D,
        accidental: Accidental::Natural,
    },
    Note {
        letter: Letter::E,
        accidental: Accidental::Flat,
    },
    Note {
        letter: Letter::E,
        accidental: Accidental::Natural,
    },
    Note {
        letter: Letter::F,
        accidental: Accidental::Natural,
    },
    Note {
        letter: Letter::G,
        accidental: Accidental::Flat,
    },
    Note {
        letter: Letter::G,
        accidental: Accidental::Natural,
    },
    Note {
        letter: Letter::A,
        accidental: Accidental::Flat,
    },
    Note {
        letter: Letter::A,
        accidental: Accidental::Natural,
    },
    Note {
        letter: Letter::B,
        accidental: Accidental::Flat,
    },
    Note {
        letter: Letter::B,
        accidental: Accidental::Natural,
    },
];

impl From<&Note> for u8 {
    fn from(note: &Note) -> Self {
        let Note { letter, accidental } = note;
        let letter = match letter {
            Letter::A => 0,
            Letter::B => 2,
            Letter::C => 3,
            Letter::D => 5,
            Letter::E => 7,
            Letter::F => 8,
            Letter::G => 10,
        };
        let accidental = match accidental {
            Accidental::Sharp => 1,
            Accidental::Flat => 11,
            Accidental::Natural => 0,
        };
        (letter + accidental + 9) % 12
    }
}
impl From<u8> for Note {
    fn from(interval: u8) -> Self {
        FLAT_NOTES[interval as usize % Note::COUNT]
    }
}

impl FromStr for Note {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (letter, s) = s.scan()?;
        let (accidental, s) = s.scan()?;
        if !s.is_empty() {
            return Err(format!("Leftover characters: {s}"));
        }
        Ok(Self { letter, accidental })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Builder, Serialize, Deserialize, Hash)]
pub struct Augmentation {
    #[builder(default)]
    pub accidental: Accidental,
    pub interval: ScaleInterval,
}

impl Display for Augmentation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            accidental,
            interval,
        } = self;
        write!(f, "{accidental}{interval}")
    }
}

impl Augmentation {
    fn as_offset(&self, chord_type: &ChordType) -> u8 {
        chord_type
            .mode()
            .increments()
            .iter()
            .take(self.interval.as_scale_interval() as usize - 1)
            .sum::<u8>()
            + self.accidental.to_u8().unwrap()
    }
}

impl FromStr for Augmentation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (accidental, s) = s.scan()?;
        let (interval, s) = s.scan()?;
        if !s.is_empty() {
            return Err(format!("Leftover characters: {s}"));
        }
        Ok(Self {
            accidental,
            interval,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumString, Display, Serialize, Deserialize, Hash)]
#[strum(ascii_case_insensitive)]
pub enum ChordType {
    #[strum(serialize = "dim", serialize = "o")]
    Diminished,
    #[strum(serialize = "min", serialize = "m", serialize = "minor")]
    Minor,
    #[strum(serialize = "", serialize = "major", serialize = "maj", to_string = "")]
    Major,
    #[strum(serialize = "aug", serialize = "+")]
    Augmented,
    #[strum(serialize = "5", serialize = "sus")]
    Fifth,
    // sus 2
    #[strum(serialize = "sus2")]
    SuspendedSecond,
    // sus 4
    #[strum(serialize = "sus4", serialize = "4")]
    SuspendedFourth,
    // dominant
    #[strum(serialize = "7")]
    Seventh,
    // also show the triangle representation
    #[strum(serialize = "maj7", serialize = "Δ7")]
    MajorSeventh,
}
impl ChordType {
    fn mode(&self) -> Mode {
        match self {
            ChordType::Diminished => Mode::Locrian,
            ChordType::Minor => Mode::Aeolian,
            ChordType::Augmented => Mode::Augmented,
            ChordType::Major
            | ChordType::Fifth
            | ChordType::SuspendedFourth
            | ChordType::SuspendedSecond
            | ChordType::MajorSeventh => Mode::Ionian,
            ChordType::Seventh => Mode::Mixolydian,
        }
    }
}

impl Default for ChordType {
    fn default() -> Self {
        ChordType::Major
    }
}

#[derive(Builder, Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Hash)]
pub struct Chord {
    pub root: Note,
    #[builder(default)]
    pub chord_type: ChordType,
    #[builder(setter(each(name = "augmentation")), default)]
    pub augmentations: Vec<Augmentation>,
    #[builder(default)]
    pub bass: Option<Note>,
}

impl<N: ToPrimitive + Copy> Add<N> for Chord {
    type Output = Self;
    fn add(self, rhs: N) -> Self::Output {
        let Self {
            root,
            chord_type,
            augmentations,
            bass,
        } = self;
        let root = root + rhs;
        let bass = bass.map(|bass| bass + rhs);
        Self {
            root,
            chord_type,
            augmentations,
            bass,
        }
    }
}

impl<N: ToPrimitive + Copy> Sub<N> for Chord {
    type Output = Self;
    #[allow(clippy::suspicious_arithmetic_impl)]
    fn sub(self, rhs: N) -> Self::Output {
        self + rhs.to_i32().unwrap().neg()
    }
}

impl<N: ToPrimitive + Copy> Add<N> for Scale {
    type Output = Self;

    fn add(self, rhs: N) -> Self::Output {
        Self::Output {
            root: self.root + rhs,
            ..self
        }
    }
}
impl<N: ToPrimitive + Copy> Sub<N> for Scale {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn sub(self, rhs: N) -> Self::Output {
        self + rhs.to_i32().unwrap().neg()
    }
}

impl Display for Chord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            root,
            chord_type,
            augmentations,
            bass,
        } = self;
        write!(f, "{root}{chord_type}")?;
        for augmentation in augmentations {
            write!(f, "{augmentation}")?;
        }
        if let Some(bass) = bass {
            write!(f, "\\{bass}")?;
        }
        Ok(())
    }
}

trait ScanStr<T> {
    type Err;
    fn scan(&self) -> Result<(T, &str), Self::Err>;
}

impl<T: FromStr + Debug> ScanStr<T> for str {
    type Err = String;
    fn scan(&self) -> Result<(T, &str), Self::Err> {
        let s = self.trim();
        (0..=s.len())
            .rev()
            .find_map(|i| {
                if !s.is_char_boundary(i) {
                    return None;
                }
                let (left, right) = s.split_at(i);
                if let Ok(r) = left.parse::<T>() {
                    Some((r, right))
                } else {
                    None
                }
            })
            .ok_or_else(|| format!("Could not parse {self}"))
    }
}

impl Chord {
    pub fn notes(&self) -> Vec<Note> {
        let mut notes = vec![self.root];
        let intervals = match self.chord_type {
            ChordType::Diminished => vec![3, 6, 9],
            ChordType::Minor => vec![3, 7],
            ChordType::Major => vec![4, 7],
            ChordType::Augmented => vec![4, 8],
            ChordType::Fifth => vec![7],
            ChordType::SuspendedSecond => vec![2, 7],
            ChordType::SuspendedFourth => vec![5, 7],
            ChordType::Seventh => vec![4, 7, 10],
            ChordType::MajorSeventh => vec![4, 7, 11],
        };
        let excepted = self
            .augmentations
            .iter()
            .map(|a| {
                (a.as_offset(&self.chord_type)
                    + (Note::COUNT as u8 - a.accidental.to_u8().unwrap()))
                .rem_euclid(Note::COUNT as u8)
            })
            .collect_vec();
        notes.extend(
            intervals
                .into_iter()
                .filter(|x| !excepted.contains(x))
                .map(|x| self.root + x),
        );
        notes.extend(
            self.augmentations
                .iter()
                .map(|a| self.root + a.as_offset(&self.chord_type)),
        );
        if let Some(bass) = self.bass {
            if !notes.contains(&bass) {
                notes.push(bass);
            }
        }
        if self.augmentations.iter().any(|x| x.interval.is_dominant()) {
            let seventh = self.root + 10;
            if !notes.contains(&seventh) {
                notes.push(seventh);
            }
        }
        notes
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParsingAttempt {
    pub mode: Option<Mode>,
    pub letter: Option<Letter>,
    pub accidental: Option<Accidental>,
    pub note: Option<Note>,
    pub chord_type: Option<ChordType>,
    pub augmentation: Option<Augmentation>,
    pub chord: Option<Chord>,
    pub chord_notes: Option<Vec<String>>,
    pub scale: Option<Scale>,
    pub scale_notes: Option<Vec<String>>,
}

impl ParsingAttempt {
    pub fn chord(&self) -> Option<Chord> {
        self.chord.clone()
    }
}

#[wasm_bindgen]
pub fn parse(string: &str, transpose: i32) -> JsValue {
    serde_wasm_bindgen::to_value(&ParsingAttempt {
        mode: string.parse::<Mode>().ok(),
        letter: string.parse::<Letter>().ok().map(|f| f + transpose),
        accidental: string.parse().ok(),
        note: string.parse::<Note>().ok().map(|f| f + transpose),
        chord_type: string.parse().ok(),
        augmentation: string.parse().ok(),
        chord: string.parse::<Chord>().ok().map(|f| f + transpose),
        scale: string.parse::<Scale>().ok().map(|f| f + transpose),
        chord_notes: string
            .parse::<Chord>()
            .ok()
            .map(|f| f + transpose)
            .map(|x| x.notes().into_iter().map(|x| x.to_string()).collect()),
        scale_notes: string
            .parse::<Scale>()
            .ok()
            .map(|f| f + transpose)
            .map(|x| x.notes().into_iter().map(|x| x.to_string()).collect()),
    })
    .unwrap()
}

impl FromStr for Chord {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (root, s) = s.scan()?;
        let (chord_type, s) = s.scan()?;
        let mut augmentations = vec![];
        let mut rest = s;
        while let Ok((augmentation, s)) = rest.scan() {
            augmentations.push(augmentation);
            rest = s;
        }
        let rest = rest.trim();
        let (bass, rest) = if rest.is_empty() {
            (None, rest)
        } else if rest.starts_with('/') || rest.starts_with('\\') {
            rest.split_at(1).1.scan().map(|(a, b)| (Some(a), b))?
        } else {
            (None, rest)
        };
        if !rest.trim().is_empty() {
            return Err(format!("Could not parse {s} -> leftover {rest}"));
        }
        Ok(Self {
            root,
            chord_type,
            augmentations,
            bass,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::*;
    use rstest::rstest;

    #[rstest]
    #[case("C", Letter::C)]
    #[case("c", Letter::C)]
    fn letter_parse(#[case] input: &str, #[case] expected: Letter) {
        assert_eq!(expected, input.parse().unwrap());
    }

    #[rstest]
    #[case("", Accidental::Natural)]
    #[case("♮", Accidental::Natural)]
    #[case("#", Accidental::Sharp)]
    #[case("♯", Accidental::Sharp)]
    #[case("b", Accidental::Flat)]
    #[case("♭", Accidental::Flat)]
    fn accidental_parse(#[case] input: &str, #[case] expected: Accidental) {
        assert_eq!(expected, input.parse().unwrap());
    }

    #[rstest]
    #[case("C", NoteBuilder::default().letter(Letter::C).build().unwrap())]
    #[case("C#", NoteBuilder::default().letter(Letter::C).accidental(Accidental::Sharp).build().unwrap())]
    #[case("A b", NoteBuilder::default().letter(Letter::A).accidental(Accidental::Flat).build().unwrap())]
    #[case("ab", NoteBuilder::default().letter(Letter::A).accidental(Accidental::Flat).build().unwrap())]
    fn note_parse(#[case] input: &str, #[case] expected: Note) {
        assert_eq!(expected, input.parse().unwrap());
    }

    // Make tests for ChordType using rstest
    #[rstest]
    #[case("dim", ChordType::Diminished)]
    #[case("o", ChordType::Diminished)]
    #[case("min", ChordType::Minor)]
    #[case("m", ChordType::Minor)]
    #[case("maj", ChordType::Major)]
    #[case("aug", ChordType::Augmented)]
    #[case("+", ChordType::Augmented)]
    #[case("5", ChordType::Fifth)]
    #[case("sus", ChordType::Fifth)]
    #[case("sus2", ChordType::SuspendedSecond)]
    #[case("sus4", ChordType::SuspendedFourth)]
    #[case("4", ChordType::SuspendedFourth)]
    #[case("7", ChordType::Seventh)]
    #[case("maj7", ChordType::MajorSeventh)]
    // triangle case
    #[case("Δ7", ChordType::MajorSeventh)]
    #[case("", ChordType::Major)]
    fn chord_type_parse(#[case] input: &str, #[case] expected: ChordType) {
        assert_eq!(expected, input.parse().unwrap());
    }

    // Make tests for Chord using rstest
    #[rstest]
    #[case("C", ChordBuilder::default().root("C".parse().unwrap()).build().unwrap())]
    #[case("Cm", ChordBuilder::default().chord_type(ChordType::Minor).root("C".parse().unwrap()).build().unwrap())]
    #[case("C min", ChordBuilder::default().chord_type(ChordType::Minor).root("C".parse().unwrap()).build().unwrap())]
    #[case("C 13", ChordBuilder::default().augmentation(AugmentationBuilder::default().interval(ScaleInterval::Thirteen).build().unwrap()).root("C".parse().unwrap()).build().unwrap())]
    #[case("Cb7", ChordBuilder::default().root("Cb".parse().unwrap()).chord_type(ChordType::Seventh).build().unwrap())]
    #[case("Cb7 # 5", ChordBuilder::default().root("Cb".parse().unwrap()).chord_type(ChordType::Seventh).augmentation( "#5".parse::<Augmentation>().unwrap()).build().unwrap())]
    #[case("Cb7#5/f#", ChordBuilder::default().root("Cb".parse().unwrap()).chord_type(ChordType::Seventh).augmentation("#5".parse::<Augmentation>().unwrap()).bass("F#".parse().ok()).build().unwrap())]
    #[case("c/a", ChordBuilder::default().root("c".parse().unwrap()).bass("A".parse().ok()).build().unwrap())]
    #[case("Caug7", ChordBuilder::default().root("c".parse().unwrap()).chord_type(ChordType::Augmented).augmentation(Augmentation{interval: ScaleInterval::Seven, accidental: Accidental::Natural}).build().unwrap())]
    fn chord_parse(#[case] input: &str, #[case] expected: Chord) {
        assert_eq!(expected, input.parse().unwrap(), "{input} -> {expected:?}");
    }

    // Make tests for Augmentation using rstest
    #[rstest]
    #[case("b9", AugmentationBuilder::default().accidental("b".parse().unwrap()).interval(ScaleInterval::Nine).build().unwrap())]
    #[case("♯9", AugmentationBuilder::default().accidental("#".parse().unwrap()).interval(ScaleInterval::Nine).build().unwrap())]
    #[case("#9", AugmentationBuilder::default().accidental("#".parse().unwrap()).interval(ScaleInterval::Nine).build().unwrap())]
    #[case("b5", AugmentationBuilder::default().accidental("b".parse().unwrap()).interval(ScaleInterval::Five).build().unwrap())]
    #[case("♭5", AugmentationBuilder::default().accidental("b".parse().unwrap()).interval(ScaleInterval::Five).build().unwrap())]
    #[case("5", AugmentationBuilder::default().interval(ScaleInterval::Five).build().unwrap())]

    fn augmentation_parse(#[case] input: &str, #[case] expected: Augmentation) {
        assert_eq!(expected, input.parse().unwrap());
    }

    // Make tests for standardizing flat
    #[rstest]
    #[case("Cb", "B")]
    #[case("Bb", "Bb")]
    #[case("C", "C")]
    #[case("G#", "Ab")]
    fn standardize_flat(#[case] input: &str, #[case] expected: &str) {
        assert_eq!(
            expected,
            input
                .parse::<Note>()
                .unwrap()
                .standardize_flat()
                .to_string()
        );
    }

    #[rstest]
    #[case("C", 1, "Db")]
    #[case("C", 2, "D")]
    #[case("C", 3, "Eb")]
    #[case("C", 4, "E")]
    #[case("C", 5, "F")]
    #[case("C", 6, "Gb")]
    #[case("C", 7, "G")]
    #[case("C", 8, "Ab")]
    #[case("C", 9, "A")]
    #[case("C", 10, "Bb")]
    #[case("C", 11, "B")]
    #[case("C", 12, "C")]
    #[case("C", 13, "Db")]
    fn note_arithmetic(#[case] input: &str, #[case] offset: i32, #[case] expected: &str) {
        assert_eq!(
            expected,
            (input.parse::<Note>().unwrap() + offset).to_string()
        );
    }

    #[rstest]
    #[case("C", 0, "C")]
    #[case("C", 1, "D")]
    #[case("C", -1, "B")]
    #[case("C", -2, "A")]
    #[case("C", -3, "G")]
    #[case("C", -7, "C")]
    #[case("C", 7, "C")]
    #[case("C", 8, "D")]
    fn letter_arithmetic(#[case] input: &str, #[case] offset: i32, #[case] expected: &str) {
        assert_eq!(
            expected,
            (input.parse::<Letter>().unwrap() + offset).to_string()
        );
    }

    #[rstest]
    #[case("C", &["C", "E", "G"])]
    #[case("D", &["D", "F#", "A"])]
    #[case("Dm", &["D", "F", "A"])]
    #[case("Ddim", &["D", "F", "Ab", "B"])]
    #[case("Ddim7", &["D", "F", "Ab", "B", "C"])]
    #[case("C7", &["C", "E", "G", "Bb"])]
    #[case("Cmaj7", &["C", "E", "G", "B"])]
    #[case("Cmin7", &["C", "Eb", "G", "Bb"])]
    #[case("C6", &["C", "E", "G", "A"])]
    #[case("Caug", &["C", "E", "G#"])]
    #[case("Caug7", &["C", "E", "G#", "Bb"])]
    #[case("Cmajb5", &["C", "E", "F#"])]
    #[case("C7#9", &["C", "E", "G","Bb", "Eb"])]
    #[case("C7#9b9", &["C", "E", "G","Bb", "Eb", "Db"])]
    #[case("C5", &["C", "G"])]
    #[case("Csus", &["C", "G"])]
    #[case("Csus4", &["C", "G", "F"])]
    #[case("Csus2", &["C", "G", "D"])]
    #[case("C9", &["C", "G", "D", "Bb", "E"])]
    #[case("C13b9", &["C", "G", "A", "Bb", "E", "Db"])]
    #[case("C / C", &["C", "E", "G"])]
    fn chord_notes(#[case] input: &str, #[case] expected: &[&str]) {
        let chord = input.parse::<Chord>().unwrap();
        let notes = chord.notes();
        assert_eq!(
            expected
                .iter()
                .map(|x| x.parse::<Note>().unwrap())
                .collect::<HashSet<_>>(),
            notes.iter().copied().collect(),
            "{input} -> {expected:?}"
        );
        assert_eq!(expected.len(), notes.len(), "{input} -> {expected:?}");
    }

    #[rstest]
    #[case("F#", "Gb")]
    #[case("Bb", "A#")]
    #[case("C", "C")]
    #[case("C#", "C#")]
    #[case("Cb", "B")]
    fn note_standardize(#[case] input: &str, #[case] expected: &str) {
        assert_eq!(expected.parse::<Note>(), input.parse::<Note>());
    }

    #[rstest]
    #[case("C", ["C", "D", "E", "F", "G", "A", "B"])]
    #[case("Cmajor", ["C", "D", "E", "F", "G", "A", "B"])]
    #[case("C#", ["C#", "D#", "E#", "F#", "G#", "A#", "B#"])]
    #[case("Cb", ["Cb", "Db", "Eb", "Fb", "Gb", "Ab", "Bb"])]
    #[case("Aminor", ["A", "B", "C", "D", "E", "F", "G"])]
    #[case("Cminor", ["C", "D", "Eb", "F", "G", "Ab", "Bb"])]
    #[case("C dorian", ["C", "D", "Eb", "F", "G", "A", "Bb"])]
    fn scale_notes(#[case] input: &str, #[case] expected: [&str; 7]) {
        let scale = input.parse::<Scale>().unwrap();
        let notes = scale.notes();
        assert_eq!(
            expected
                .iter()
                .map(|x| x.parse::<Note>().unwrap())
                .collect_vec(),
            notes.to_vec(),
            "{input} -> {expected:?}"
        );
    }
}
