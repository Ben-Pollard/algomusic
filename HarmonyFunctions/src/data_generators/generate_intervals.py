import numpy as np
import src.data_generators.Harmonicity as Harmonicity
from src.data_generators.BatchHarmonicity import BatchHarmonicity
import math
from tqdm import tqdm

if __name__ == "__main__":

    num_indices = 128
    
    num_set_random_weights = 250
    num_chords_per_batch = 50

    #Define chords
    notes = np.arange(0,num_indices, dtype=np.int32)
    chords = np.array(np.meshgrid(notes, notes)).T.reshape(-1,2)

    #Define weights
    if num_set_random_weights == 1:
        weights = np.ones(chords.size).reshape(chords.shape).astype(np.float32)
    else:
        weights = []
        for w in range(num_set_random_weights):
            weights.append(np.random.random(chords.size).reshape(chords.shape).astype(np.float32))
        weights = np.vstack(weights)
        chords = np.vstack([chords for w in range(num_set_random_weights)]) #repeat chords with different weights


    #Reshape to n-hot
    def reindex_flat(index):
        note = (index+1) % 12
        octave = math.floor((index+1) / 12)
        return 11*note + octave - 1

    def reindex_2d(index):
        note = (index) % 12
        octave = math.floor(index / 12)
        return [note, octave]

    num_octaves = 11

    num_chords = chords.shape[0]
    #chords_onehot = np.zeros([num_chords, num_indices], np.int32)
    #weights_onehot = np.zeros([num_chords, num_indices], np.float32)
    #weights_onehot_notename_first = np.zeros([num_chords, 132], np.float32)
    chords_2d = np.zeros([num_chords, 12, num_octaves])
    for i in range(0, num_chords):
        if chords[i].size == np.unique(chords[i]).size:
            #chords_onehot[i][chords[i]] = 1
            for j in range(len(chords[i])):
                note = chords[i,j]
                weight = weights[i,j]
                #weights_onehot[i][note] = weight
                #weights_onehot_notename_first[i][note] = weight
                note_name, octave = reindex_2d(note)
                chords_2d[i, note_name, octave] = weight

    filter = np.logical_not(chords_2d.sum(axis=1).sum(axis=1)==0)
    chords = chords[filter]
    weights = weights[filter]
    #chords_onehot = chords_onehot[filter]
    #weights_onehot = weights_onehot[filter]
    #weights_onehot_notename_first = weights_onehot_notename_first[filter]
    chords_2d = chords_2d[filter]


    
    bh = BatchHarmonicity(num_chords_per_batch)
    num_chords = chords.shape[0]
    if num_chords % num_chords_per_batch != 0:
       raise ValueError(f"Chord matrix must be multiple of {self.num_chords}")
    
    i = 0 
    harmonicity = []
    num_chords = chords_2d.shape[0]
    with tqdm(total=num_chords, position=0, leave=True) as pbar:
        while i < num_chords:
            hi = bh.calcHarmonicity(chords[i:i+num_chords_per_batch], np.asarray(weights[i:i+num_chords_per_batch], dtype=np.float16))
            harmonicity += hi.numpy().tolist()
            i += num_chords_per_batch
            pbar.update(num_chords_per_batch)

    if len(harmonicity) != chords.shape[0]:
        raise ValueError(f"Harmonicity is len {len(harmonicity)} but there are {chords.shape[0]} chords")

    #np.save('data/chords.npy', chords)
    #np.save('data/weights.npy', weights)
    #np.save('data/chords_onehot.npy', chords_onehot)
    #np.save('data/weights_onehot.npy', weights_onehot)
    #np.save('data/weights_onehot_notename_first.npy', weights_onehot_notename_first)
    np.save('data/chords_2d.npy', chords_2d)
    np.save('data/harmonicity.npy', harmonicity)

