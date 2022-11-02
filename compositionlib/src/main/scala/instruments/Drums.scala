package instruments

import enums.DrumNames._
import enums.{DrumNames, MidiControlNumbers}
import midi.DevicesNames.LOOP_MIDI_PORT
import midi.FLStudioMIDIMap
import models.Primitives.{MidiPitch, PitchName}

//FL Studio FPC instrument
object CC_FPC extends MidiControlNumbers {}

object FPC extends DrumKit {
  val channel = 1
  val port = LOOP_MIDI_PORT
  val voices = 100
  val midiPitchMap: Map[DrumNames.Value, MidiPitch] = pitchNameMap.map { case (name, pitchname) => (name, FLStudioMIDIMap.midiMap.get(pitchname).get)}
  val pitchNameMap: Map[DrumNames.Value, PitchName] = Map(
    KICK -> "C3",
    SNARE -> "D3",
    HHC -> "F#3",
    HHO -> "A#3",
    HHP -> "G#3"
  )
}

object LabsDrums extends DrumKit {
  val channel = 1
  val port = LOOP_MIDI_PORT
  val voices = 100
  lazy val midiPitchMap: Map[DrumNames.Value, MidiPitch] = pitchNameMap.map { case (name, pitchname) => (name, FLStudioMIDIMap.midiMap.get(pitchname).get)}
  lazy val pitchNameMap: Map[DrumNames.Value, PitchName] = Map(
    KICK -> "C3",
    SNARE -> "D3",
    HHC -> "F#3",
    HHO -> "A#3",
    HHP -> "G#3"
  )
}

object SSD5 extends DrumKit {
  val channel = 1
  val port = LOOP_MIDI_PORT
  val voices = 100
  lazy val midiPitchMap: Map[DrumNames.Value, MidiPitch] = pitchNameMap.map { case (name, pitchname) => (name, FLStudioMIDIMap.midiMap.get(pitchname).get)}
  lazy val pitchNameMap: Map[DrumNames.Value, PitchName] = Map(
    CHINA_TIP -> "A7",
    CRASH_RIGHT_TIP -> "G#7",
    CRASH_LEFT_TIP -> "G7",
    SPLASH_TIP -> "F#7",
    HH_FOOTSPLASH -> "C#6",
    HH_PEDAL -> "C6",
    HHP -> "C6",
    HH_TIP_OPEN3 -> "B5",
    HH_TIP_OPEN2 -> "A#5",
    HH_TIP_OPEN1 -> "A5",
    HH_TIP_LOOSEN -> "G#5",
    HH_TIP_CLOSED -> "G5",
    HH_TIP_CLOSED_TIGHT -> "F#5",
    HH_SHANK_OPEN3 -> "F5",
    HH_SHANK_OPEN2 -> "E5",
    HH_SHANK_OPEN1 -> "D#5",
    HH_SHANK_LOOSEN -> "D5",
    HH_SHANK_CLOSED -> "C#5",
    HH_SHANK_CLOSED_TIGHT -> "C5",
    RIDE_EDGE -> "B4",
    RIDE_CHOKE -> "A#4",
    CRASH_RIGHT_EDGE -> "A4",
    CRASH_RIGHT_CHOKE -> "G#4",
    CRASH_LEFT_EDGE -> "G4",
    CRASH_LEFT_CHOKE -> "F#4",
    RIDE_BELL -> "F4",
    RIDE_BOW_SHANK -> "E4",
    RIDE_BOW_TIP -> "D#4",
    SPLASH_EDGE -> "D4",
    SPLASH_CHOKE -> "C#4",
    RACK_TOM1 -> "C4",
    RACK_TOM2 -> "B3",
    HH_TIP_CC_CONTROLLABLE -> "A#3",
    HH_PEDAL2 -> "G#3",
    FLOOR_TOM1 -> "G3",
    HH_TIP_CLOSED2 -> "F#3",
    FLOOR_TOM2 -> "F3",
    SNARE_RIMSHOT -> "E3",
    SNARE_RIMCLICK -> "D#3",
    SNARE -> "D3",
    SNARE_SIDESTICK -> "C#3",
    KICK -> "C3",
    KICK_DOUBLE -> "B2",
    SNARE_SIDE -> "A#2",
    SNARE_RIMSHOT_EDGE -> "A2",
    CHINA_CHOKE -> "G#2",
    CHINA_EDGE -> "G2",
    HH_SHANK_CC_CONTROLLABLE -> "D2",
    HH_SHANK_CLOSED -> "A#1",
    SNARE_ROLL -> "A1",
    RACK_TOM1_RIMCLICK -> "G1",
    RACK_TOM2_RIMCLICK -> "F1",
    FLOOR_TOM1_RIMCLICK -> "D1",
    FLOOR_TOM2_RIMCLICK -> "C1"
  )
}