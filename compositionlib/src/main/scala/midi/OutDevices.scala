package midi

object OutDevices extends Enumeration  {
  type Device = Value
  val MS_MIDI_MAPPER = Value("Microsoft MIDI Mapper")
  val COOLSOFT_MIDI_MAPPER = Value("CoolSoft MIDIMapper")
  val MS_GS_WAVETABLE_SYNTH = Value("Microsoft GS Wavetable Synth")
  val LOOP_MIDI_PORT = Value("loopMIDI Port")
}
