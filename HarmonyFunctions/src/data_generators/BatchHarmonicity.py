import tensorflow as tf
import numpy as np
import math
from scipy.stats import norm

def log2tf(x):
    return tf.math.divide(tf.math.log(x), tf.cast(tf.math.log(2.0), tf.float16))

class BatchHarmonicity:

  array_dim = 1200
  num_harmonics = 12
  array_dim_float = tf.constant(1200, tf.float16)
  num_harmonics_float = tf.constant(12, tf.float16)
  sigma = 6.83
  rho = 0.75

  def __init__(self, num_chords) -> None:
     self.num_chords = num_chords
     self.pc_spectrum_template_1 = self.get_pc_spectrum_template_1(self.array_dim, self.sigma)
     self.harmonicity = tf.Variable(np.zeros(num_chords), dtype=tf.float16)

  def get_pc_spectrum_template_1(self, array_dim, sigma): 
      limit = math.floor(sigma * 12) + 1
      template_values = np.concatenate([
        norm.pdf(np.arange(limit), 0, sigma), 
        np.zeros(array_dim - 2*limit, dtype=np.int32), 
        norm.pdf(array_dim - np.arange(array_dim - limit, array_dim), 0, sigma)
        ])

      return tf.constant(template_values, dtype=tf.float16)

  
  def get_pc_spectrum_template_2(self, mean, mass, chordSize):

      scaled = tf.math.multiply(
          tf.reshape(self.pc_spectrum_template_1, (1,1,1,-1)),
          tf.expand_dims(mass, 3)
      )

      indexRange = tf.cast(tf.range(0, self.array_dim, 1), tf.int16)

      origin = tf.cast(tf.math.round(mean), tf.int16)

      x_minus_origin = tf.math.subtract(
          tf.reshape(indexRange, (1,1,1,-1)),
          tf.expand_dims(origin, 3)
      )
      indices = tf.math.floormod(x_minus_origin, self.array_dim)

      spectralBandIndices = tf.reshape(
          indices,
          np.array([-1]))

      harmonicIndices = tf.tile(
        tf.tile(
          tf.reshape(
          tf.linalg.matrix_transpose(
              tf.reshape(
              tf.tile(
                  tf.cast(tf.range(0, self.num_harmonics, 1), tf.int16),
                  np.array([self.array_dim])
              ), (self.array_dim,-1)
              )
          ),
          np.array([-1])
          ),
          tf.expand_dims(chordSize, 0)),
      tf.expand_dims(mean.shape[0], 0))

      noteIndices = tf.tile(
        tf.reshape(
          tf.linalg.matrix_transpose(
          tf.reshape(
              tf.tile(
              tf.cast(tf.range(0, chordSize, 1), tf.int16),
              np.array([self.array_dim*self.num_harmonics])
              ),
              (self.array_dim*self.num_harmonics, -1)
          )
          ),
          np.array([-1])
      ),
      tf.expand_dims(mean.shape[0], 0))

      chordIndices = tf.reshape(
          tf.linalg.matrix_transpose(
          tf.reshape(
              tf.tile(
              tf.cast(tf.range(0, mean.shape[0], 1), tf.int16),
              tf.expand_dims(tf.constant(self.array_dim)*tf.constant(self.num_harmonics)*chordSize,0)
              ),
              (tf.constant(self.array_dim)*tf.constant(self.num_harmonics)*chordSize, -1)
          )
          ),
          np.array([-1])
      )

      gatherIndices = tf.linalg.matrix_transpose(
          tf.stack([chordIndices, noteIndices, harmonicIndices, spectralBandIndices])
      )

      gathered = tf.reshape(
          tf.gather_nd(scaled, tf.cast(gatherIndices, tf.int32)),
          tf.concat([tf.reshape(mean.shape[0], np.array([1])), tf.reshape(chordSize, np.array([1])), (self.num_harmonics,self.array_dim)], 0)
      )

      return gathered
      

  def getComplexTone(self, tone, weight):
      harmonicIndices = tf.math.add(tf.range(0, self.num_harmonics, 1), 1)
      harmonicIndicesFloat = tf.cast(harmonicIndices, tf.float16)
      dimMulIndex = tf.math.multiply(tf.constant(self.array_dim, tf.float16), log2tf(harmonicIndicesFloat))

      toneAsFloat = tf.cast(tone, tf.float16)
      dimMulTone = tf.math.multiply(toneAsFloat, tf.math.divide(self.array_dim_float, self.num_harmonics_float))

      pcs = tf.math.mod(
        tf.math.add(
          tf.expand_dims(dimMulTone, 2),
          tf.reshape(dimMulIndex, [1,1,-1])
        ), self.array_dim)

      weights = tf.math.divide(
        tf.expand_dims(weight, 2),
        tf.reshape(tf.math.pow(harmonicIndicesFloat, self.rho), [1,1,-1])
      )

      pc_spectrum_template_2 = self.get_pc_spectrum_template_2(pcs, weights, tf.shape(tone)[1])
      return tf.reduce_sum(pc_spectrum_template_2, 2)
    



  def get_milne_pc_spectrum(self, chord, weights):
      spectra = self.getComplexTone(chord, weights)
      milne_pc_spectrum = tf.reduce_sum(spectra, 1)
      return milne_pc_spectrum
    

  def sweep_template(self, milne_pc_spectrum, template):

      num_chords = milne_pc_spectrum.shape[0]

      indices = tf.cast(tf.range(0, self.array_dim, 1), tf.int16)

      colIndices = tf.tile(
        tf.reshape(
        tf.math.mod(
          tf.math.add(
            tf.reshape(
              tf.tile(
                indices,
                np.array([self.array_dim])
              ),
              (self.array_dim,-1)
            ),
            tf.expand_dims(indices, 1)
          ),
          self.array_dim
        ),
        np.array([-1])
      ), tf.expand_dims(num_chords, 0))


      rowIndices = tf.tile(
        tf.reshape(
        tf.linalg.matrix_transpose(
          tf.reshape(
            tf.tile(indices, np.array([self.array_dim])),
            (self.array_dim,-1)
          )
        ),
        np.array([-1])
      ), tf.expand_dims(num_chords, 0))


      chordIndices = tf.repeat(np.arange(num_chords, dtype=np.int16), self.array_dim**2)

      matrixTemplate = tf.linalg.matrix_transpose(
        tf.reshape(
          tf.repeat(milne_pc_spectrum, self.array_dim, axis=1), 
          [num_chords, self.array_dim, self.array_dim]))

      matrix = tf.reshape(
        tf.gather_nd(
          matrixTemplate,
          tf.linalg.matrix_transpose(
            tf.cast(tf.stack([chordIndices, rowIndices, colIndices]), tf.int32)
          )
        ),
        (num_chords, self.array_dim,self.array_dim)
      ) # np.sum(matrix[0]) 9792.713; np.sum(matrix[0], axis=0) [8.160591, 8.160592, 8.160592, ..., 8.160591, 8.16059 , 8.160591]

      matrixNorm = tf.math.pow(
        tf.linalg.einsum("aij,aij->ai", matrix, matrix),
        0.5
      ) #[0.7976032 , 0.7976032 , 0.79760313, ..., 0.7976032 , 0.79760313,   0.79760313]

      vectorNorm = tf.math.pow(
        tf.linalg.einsum("ij,ij->i", template, template),
        0.5
      ) #0.486687

      dot = tf.linalg.einsum("hij,i->hj", matrix, template[0]) #[0.31808537, 0.31467187, 0.3080029 , ..., 0.3082483 , 0.3148217 , 0.31813574]

      cosineSimilarity = tf.math.divide(
        dot,
        tf.math.multiply(
          matrixNorm,
          vectorNorm
        )
      ) #[0.81942093, 0.8106274 , 0.7934475 , ..., 0.79407966, 0.8110134 , 0.81955075]

      return cosineSimilarity
    
  @tf.function
  def calcHarmonicity(self, chords, weights):

      #chord = np.array([[0,7], [0,6], [0,5], [0,1]] , dtype = np.int16)
      #weights = np.array([[1.0, 1.0], [1.0,1.0], [1.0,1.0], [1.0,1.0]], np.float16)
      if chords.shape != weights.shape:
        raise ValueError("Chords and weights must be the same shape")
      
      if chords.shape[0] != self.num_chords:
        raise ValueError(f"Number of chords must be {self.num_chords}")

      milne_pc_spectrum = self.get_milne_pc_spectrum(chords, weights) #[0.12607166, 0.12472759, 0.12078077, ..., 0.11447829, 0.12078077, 0.12472759] 8.160593
      template = self.get_milne_pc_spectrum(tf.expand_dims(tf.constant([0], dtype=tf.float16), 0), tf.expand_dims(tf.constant([1.0], dtype=tf.float16),0)) #4.0802965
      y = self.sweep_template(milne_pc_spectrum, template)
      sum_y = tf.math.reduce_sum(y, 1)
      probs = tf.math.divide(y, tf.expand_dims(sum_y, 1))

      uniform_probs = tf.math.divide(
        tf.constant(1.0, dtype=tf.float16),
        tf.cast(probs.shape[1], tf.float16)
      )
      mask = tf.math.greater(probs, tf.constant(0.0, dtype=tf.float16))

      @tf.function
      def harmonicityPerChord():
        i = tf.constant(0, dtype=tf.int32)
        while tf.less(i, self.num_chords):
          nonzero_probs = tf.boolean_mask(probs[i], mask[i])

          hc = tf.math.reduce_sum(
            tf.math.multiply(
            nonzero_probs,
            log2tf(tf.math.divide(nonzero_probs, uniform_probs))
          ))

          self.harmonicity[i].assign(hc)
          i+=1
      
      harmonicityPerChord()

      return self.harmonicity
  

  def calcHarmonicityFromWeightMatrix(self, chord, clip_to_top_n_weights):
    num_chords = chord.shape[0]
    if num_chords % self.num_chords != 0:
       raise ValueError(f"Chord matrix must be multiple of {self.num_chords}")
    
    chord = chord.astype(np.float16)
    chord_flat = np.transpose(chord, [0,2,1]).reshape(num_chords, -1)
    top_n_chord = chord_flat.argsort(axis=1)[:,-clip_to_top_n_weights:]
    top_n_weights = np.take_along_axis(chord_flat, top_n_chord, axis=1).reshape(num_chords, clip_to_top_n_weights)
    top_n_chord = top_n_chord.reshape(num_chords, clip_to_top_n_weights)
    top_n_weights[top_n_weights<0.0] = 0.0

    harmonicity = np.array([], dtype=np.float16)
    for i in range(0,num_chords, self.num_chords):
       h = self.calcHarmonicity(top_n_chord[i:i+self.num_chords], top_n_weights[i:i+self.num_chords]).numpy().tolist()
       harmonicity = np.hstack([harmonicity, h])

    return harmonicity

