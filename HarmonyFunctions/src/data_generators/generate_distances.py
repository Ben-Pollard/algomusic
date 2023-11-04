import numpy as np
from src.data_generators.BatchHarmonicity import BatchHarmonicity
from tqdm import tqdm
import os
import shutil

#Data for the siamese network
"""
Two chords that are the same will have the same harmonicity.

* Loss will be function of Hc1c2
* It needs to describe the relative harmonicity of the two chords i.e. how they relate to each other harmonically
* Hc1c2 needs to be defined such that when c1==c2 then Hc1==Hc1c2
* So Hc1c2 = H[(c1+c2)/2]
* Define c1 and c2
* Calc harmonicity of c1
* Calc harmonicity of c1c2
* y = Hc1 - Hc1c2
"""

num_chords_per_batch = 25
bh = BatchHarmonicity(num_chords_per_batch)
notes = 12
octaves = 11

#Calculate harmonicity
num_batches = 40000
num_chords = num_chords_per_batch * num_batches
print(f'Comparing {num_chords} chords')

num_chords_per_file = 100000
i = 0
file_iterator = 0
file_cutoffs = sorted(list(set(range(num_chords_per_file, num_chords, num_chords_per_file)).union({num_chords})))
c1_all = np.zeros([0,12,11])
c2_all = np.zeros([0,12,11])
hc1_all = np.zeros([0])
hc1c2_all = np.zeros([0])
relative_h_all = np.zeros([0])


shutil.rmtree('data/distance/')

with tqdm(total=num_chords, position=0, leave=True) as pbar:
    while i <= num_chords:
        if i >= file_cutoffs[file_iterator]:
            # Save and re-initialise
            os.makedirs(f'data/distance/{file_iterator}', exist_ok=True)
            np.save(f'data/distance/{file_iterator}/c1_all.npy', c1_all)
            np.save(f'data/distance/{file_iterator}/c2_all.npy', c2_all)
            np.save(f'data/distance/{file_iterator}/hc1_all.npy', hc1_all)
            np.save(f'data/distance/{file_iterator}/hc1c2_all.npy', hc1c2_all)
            np.save(f'data/distance/{file_iterator}/relative_h_all.npy', relative_h_all)
            c1_all = np.zeros([0,12,11])
            c2_all = np.zeros([0,12,11])
            hc1_all = np.zeros([0])
            hc1c2_all = np.zeros([0])
            relative_h_all = np.zeros([0])

            file_iterator += 1

        #Define chords
        c1_size = np.random.randint(1,13)
        c1_indices = np.random.randint(0,notes*octaves, c1_size*num_chords_per_batch).reshape(num_chords_per_batch, c1_size)
        c1_weights = np.random.random([num_chords_per_batch, c1_size])
        c1 = np.zeros([num_chords_per_batch, notes*octaves])
        np.put_along_axis(c1, c1_indices, c1_weights, axis=1)
        c1 = c1.reshape([num_chords_per_batch, notes, octaves])

        c2_size = np.random.randint(1,13)
        c2_indices = np.random.randint(0,notes*octaves, c2_size*num_chords_per_batch).reshape(num_chords_per_batch, c2_size)
        c2_weights = np.random.random([num_chords_per_batch, c2_size])
        c2 = np.zeros([num_chords_per_batch, notes*octaves])
        np.put_along_axis(c2, c2_indices, c2_weights, axis=1)
        c2 = c2.reshape([num_chords_per_batch, notes, octaves])

        c1c2 = ((c1 + c2)/2.0)
        #Calculate harmonicity
        hc1 = bh.calcHarmonicityFromWeightMatrix(c1, notes*octaves)
        hc1c2 = bh.calcHarmonicityFromWeightMatrix(c1c2, notes*octaves)
        relative_h = hc1 - hc1c2
        # Append data
        c1_all = np.vstack([c1_all, c1])
        c2_all = np.vstack([c2_all, c2])
        hc1_all = np.hstack([hc1_all, hc1])
        hc1c2_all = np.hstack([hc1c2_all, hc1c2])
        relative_h_all = np.hstack([relative_h_all, relative_h])
        
        i += num_chords_per_batch
        pbar.update(num_chords_per_batch)


#Todo tests on known chords


