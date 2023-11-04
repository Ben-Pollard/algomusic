import tensorflow as tf
import numpy as np
import math
from scipy.stats import norm

graph = tf.Graph()

array_dim = 1200
num_harmonics = 12
array_dim_float = tf.constant(1200, tf.float32)
num_harmonics_float = tf.constant(12, tf.float32)
sigma = 6.83
rho = 0.75

def log2tf(x):
    return tf.math.divide(tf.math.log(x), tf.math.log(2.0))

def get_pc_spectrum_template_1(array_dim, sigma): 
    limit = math.floor(sigma * 12) + 1
    template_values = np.concatenate([
      norm.pdf(np.arange(limit), 0, sigma), 
      np.zeros(array_dim - 2*limit, dtype=np.int32), 
      norm.pdf(array_dim - np.arange(array_dim - limit, array_dim), 0, sigma)
      ])

    return tf.constant(template_values, dtype=tf.float32)

pc_spectrum_template_1 = get_pc_spectrum_template_1(array_dim, sigma)


def get_pc_spectrum_template_2(mean, mass, chordSize):

    scaled = tf.math.multiply(
        tf.reshape(pc_spectrum_template_1, (1,1,-1)),
        tf.expand_dims(mass, 2)
    )

    indexRange = tf.range(0, array_dim, 1)

    origin = tf.cast(tf.math.round(mean), tf.int32)

    x_minus_origin = tf.math.subtract(
        tf.reshape(indexRange, (1,1,-1)),
        tf.expand_dims(origin, 2)
    )
    indices = tf.math.floormod(x_minus_origin, array_dim)

    spectralBandIndices = tf.reshape(
        indices,
        (-1))

    harmonicIndices = tf.tile(
        tf.reshape(
        tf.linalg.matrix_transpose(
            tf.reshape(
            tf.tile(
                tf.range(0, num_harmonics, 1),
                np.array([array_dim])
            ), (array_dim,-1)
            )
        ),
        -1
        ),
        tf.expand_dims(chordSize, 0)
    )

    chordIndices = tf.reshape(
        tf.linalg.matrix_transpose(
        tf.reshape(
            tf.tile(
            tf.range(0, chordSize, 1),
            np.array([array_dim*num_harmonics])
            ),
            (array_dim*num_harmonics, -1)
        )
        ),
        -1
    )

    gatherIndices = tf.linalg.matrix_transpose(
        tf.stack([chordIndices, harmonicIndices, spectralBandIndices])
    )

    gathered = tf.reshape(
        tf.gather_nd(scaled, gatherIndices),
        tf.concat([tf.reshape(chordSize, 1), (num_harmonics,array_dim)], 0)
    )

    return gathered
    

def getComplexTone(tone, weight):
    harmonicIndices = tf.math.add(tf.range(0, num_harmonics, 1), 1)
    harmonicIndicesDouble = tf.cast(harmonicIndices, tf.float32)
    dimMulIndex = tf.math.multiply(array_dim, log2tf(harmonicIndicesDouble))

    toneAsDouble = tf.cast(tone, tf.float32)
    dimMulTone = tf.math.multiply(toneAsDouble, tf.math.divide(array_dim_float, num_harmonics_float))

    pcs = tf.math.mod(
      tf.math.add(
        tf.expand_dims(dimMulTone, 1),
        tf.expand_dims(dimMulIndex, 0)
      ), array_dim)

    weights = tf.math.divide(
      tf.expand_dims(weight, 1),
      tf.expand_dims(tf.math.pow(harmonicIndicesDouble, rho), 0)
    )

    pc_spectrum_template_2 = get_pc_spectrum_template_2(pcs, weights, tf.size(tone))
    return tf.reduce_sum(pc_spectrum_template_2, 1)
  



def get_milne_pc_spectrum(chord, weights):
    spectra = getComplexTone(chord, weights)
    milne_pc_spectrum = tf.reduce_sum(spectra, 0)
    return milne_pc_spectrum
  

def sweep_template(milne_pc_spectrum, template):
    indices = tf.range(0, array_dim, 1)

    colIndices = tf.reshape(
      tf.math.mod(
        tf.math.add(
          tf.reshape(
            tf.tile(
              indices,
              np.array([array_dim])
            ),
            (array_dim,-1)
          ),
          tf.expand_dims(indices, 1)
        ),
        array_dim
      ),
      -1
    )

    rowIndices = tf.reshape(
      tf.linalg.matrix_transpose(
        tf.reshape(
          tf.tile(indices, np.array([array_dim])),
          (array_dim,-1)
        )
      ),
      -1
    )

    matrixTemplate = tf.reshape(
        tf.tile(milne_pc_spectrum, np.array([array_dim])),
        (array_dim,-1)
      ) # np.sum(matrixTemplate) 9792.714; matrixTemplate[0] [0.12607166, 0.12472759, 0.12078077, ..., 0.11447829, 0.12078077, 0.12472759]; 
      #np.sum(matrixTemplate, axis=0) [151.28366, 149.6719 , 144.93605, ..., 137.37413, 144.93605, 149.6719 ]

    matrix = tf.reshape(
      tf.gather_nd(
        matrixTemplate,
        tf.linalg.matrix_transpose(
          tf.stack([rowIndices, colIndices])
        )
      ),
      (array_dim,array_dim)
    ) 

    matrixNorm = tf.math.pow(
      tf.linalg.einsum("ij,ij->i", matrix, matrix),
      0.5
    )

    vectorNorm = tf.math.pow(
      tf.linalg.einsum("i,i->", template, template),
      0.5
    )

    dot = tf.linalg.einsum("ij,i->j", matrix, template)

    cosineSimilarity = tf.math.divide(
      dot,
      tf.math.multiply(
        matrixNorm,
        vectorNorm
      )
    )

    return cosineSimilarity
  

def calcHarmonicity(chord, weights):
    #chord = np.array([0,7], dtype = np.int32)
    #weights = np.array([1.0, 1.0], np.float32)

    milne_pc_spectrum = get_milne_pc_spectrum(chord, weights) #[0.12607166, 0.12472759, 0.12078077, ..., 0.11447829, 0.12078077, 0.12472759] 8.160593
    template = get_milne_pc_spectrum(tf.constant([0], dtype=tf.float32), tf.constant([1.0], dtype=tf.float32)) #4.0802965
    y = sweep_template(milne_pc_spectrum, template)
    sum_y = tf.math.reduce_sum(y, 0)
    probs = tf.math.divide(y, sum_y) #[1200]

    uniform_probs = tf.math.divide(
      tf.constant(1.0),
      tf.cast(tf.size(probs), tf.float32)
    ) #[]
    mask = tf.math.greater(probs, tf.constant(0.0)) #[1200]
    nonzero_probs = tf.boolean_mask(probs, mask) #[1200]
    harmonicity = tf.math.reduce_sum(tf.math.multiply(
      nonzero_probs,
      log2tf(tf.math.divide(nonzero_probs, uniform_probs))
    ), 0) #[]

    return tf.expand_dims(harmonicity, 0)


def getGraph():
    chordPlaceHolder = tf.placeholder(tf.int32)
    weightsPlaceHolder = tf.placeholder(tf.float32)
    harmonicity = calcHarmonicity(chordPlaceHolder, weightsPlaceHolder)
    return (graph, harmonicity, chordPlaceHolder, weightsPlaceHolder)
