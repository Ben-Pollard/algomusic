import tensorflow as tf
import numpy as np
import src.data_generators.Harmonicity as Harmonicity
from src.data_generators.BatchHarmonicity import BatchHarmonicity

if __name__ == "__main__":

    #chord2d = np.array([[0,7,10], [0,6,10], [0,5,10]] , dtype = np.int32)
    #weights2d = np.array([[1.0, 1.0, 0.0], [1.0,1.0, 0.0], [1.0,1.0, 0.0]], np.float32)

    chord2d = np.array([[0,7], [0,6], [0,5]] , dtype = np.int32)
    weights2d = np.array([[1.0, 1.0], [1.0,1.0], [1.0,1.0]], np.float32)
    bh = BatchHarmonicity(chord2d.shape[0])
    bh.calcHarmonicity(chord2d, tf.cast(weights2d, tf.float16)).numpy().tolist()

    for i in range(chord2d.shape[0]):
        chord = chord2d[i]
        weights = weights2d[i]
        result = Harmonicity.calcHarmonicity(chord, weights)
        print(result)
    
    
