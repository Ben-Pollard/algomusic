package models.midibuilders

import instruments.Instrument
import models.barconstructors.CCBarConstructor
import models.midibuilders
import models.midibuilders.ArrangementConstruction.BarConstructionAndSequencingData

case class Track(bars: Seq[Bar], instrument: Instrument, joinNeighbours: Boolean = false)
case class VoiceJoining(expand: Boolean, join: Boolean)

case class Arrangement(tracks: List[Track]) {

  def ++ (arrangement: Arrangement) = {
    Arrangement(tracks ++ arrangement.tracks)
  }

  def repeat(n: Int): Arrangement = {

    Arrangement(tracks.map(track => {
      track.copy(bars = List.fill(n)(track.bars).flatten)
    }))
  }

}

object Arrangement {
  def apply(barConstructionAndSequencingData: BarConstructionAndSequencingData, instrument: Instrument, voiceLeadingOptions: Option[VoiceJoining] = None): Arrangement = {

    val (expand, join) = voiceLeadingOptions match {
      case Some(VoiceJoining(expand: Boolean, join: Boolean)) => {
        (expand, join)
      }
      case None => (false, false)
    }

    val bars = barConstructionAndSequencingData.map( c => {
      val polyBarConstructor = c.constructor

      val bar = if (expand) {
          midibuilders.Bar(polyBarConstructor.copy(rhythm = polyBarConstructor.rhythm.expandDurations()), instrument)
      } else Bar(polyBarConstructor, instrument)

      polyBarConstructor.controlBarConstructor match {
        case Some(cCBarConstructor: CCBarConstructor) => bar + ControlBar(cCBarConstructor, instrument)
        case None => bar
      }
    })

    val barSequence = List(midibuilders.Track(bars, instrument, join))

    Arrangement(barSequence)

  }
}
