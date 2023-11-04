import numpy as np

def getWeightMatrix(noteMatrix, n_bars, n_beats, n_subdivisions, decay_constant):
    length = noteMatrix.shape[0]
    w = np.exp(np.linspace(0, (n_bars*n_beats) - 1, n_bars*n_beats*n_subdivisions) * -decay_constant)
    w[w < 1/128.] = 0
    indices = -1 * (np.tile(np.arange(length), length).reshape(length,length) - np.arange(length).reshape(-1,1) % length)
    w = np.take_along_axis(w.reshape(-1,1), indices, axis=0)
    w = np.tril(w)
    return w #dot can yield weights > 1


# This expresses the harmonicity matrix given point notes.
# Next we need to represent note duration, time, notes, bars
n_bars = 2
n_beats = 4
n_subdivisions = 1
n_notes = 12
n_octaves = 1
decay_constant = 1.
notes = np.zeros([n_bars*n_beats*n_subdivisions, n_notes*n_octaves])
notes[0,0] = 1
notes[4,0] = 1
notes[5,0] = 1
notes

w = getWeightMatrix(notes, n_bars, n_beats, n_subdivisions, decay_constant)
harm_hist = np.dot(w, notes)
np.round(harm_hist, 1)
# 1. Apply norm then sigmoid to keep weights bounded
# 2. Calculate harmonicity at each time step
# 3. Boringness
# 4. Network with time at output
# 5. Boringness loss function

##The harmonicity raw data should quantise the input weights to byte-space (to cutoff low weights)
