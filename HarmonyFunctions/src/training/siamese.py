import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import numpy as np
from sklearn.metrics import r2_score, mean_absolute_error
from sklearn.model_selection import train_test_split
from src.data_generators.BatchHarmonicity import BatchHarmonicity

# Data
c1_all = np.load('data/distance/0/c1_all.npy')
c2_all = np.load('data/distance/0/c2_all.npy')
relative_h_all = np.load('data/distance/0/relative_h_all.npy')
c1_train, c1_test, c2_train, c2_test, h_train, h_test = train_test_split(c1_all, c2_all, relative_h_all, test_size=0.1)


def euclidean_distance(vectors):
    x, y = vectors
    sum_square = tf.math.reduce_sum(tf.math.square(x-y), axis=1, keepdims=True)
    return tf.math.sqrt(tf.math.maximum(sum_square, tf.keras.backend.epsilon()))

# Transfer embedding model from harmonicity prediction
harm_pred_model = keras.models.load_model('models/harm_pred_model.h5')
harm_pred_model.summary()
harm_pred_model.trainable=False
normal = tf.keras.layers.BatchNormalization()(harm_pred_model.layers[-4].output)
d1 = layers.Dense(512, activation='tanh', name='embedding_d3')(normal)
d2 = layers.Dense(64, activation='tanh', name='embedding_d4')(d1)
embedding_layer = layers.Dense(8, activation='tanh', name='embedding')(d2)
embedding_network = keras.Model(harm_pred_model.input, embedding_layer, name="embedding_model")
embedding_network.summary()
[l.trainable for l in embedding_network.layers]


# Siamese network
input_1 = keras.Input(shape=(12,11,1), name="input_1")
input_2 = keras.Input(shape=(12,11,1), name="input_2")
tower_1 = embedding_network(input_1)
tower_2 = embedding_network(input_2)
merge = layers.Lambda(euclidean_distance)([tower_1, tower_2])
#merge = layers.Dot(axes=1, normalize=True)([tower_1, tower_2])
#output = layers.Dense(1, activation=None)(merge)
# normal = layers.BatchNormalization()(merge)
output = layers.Dense(1, activation="sigmoid")(merge)

# We want the Euclidean distance between the embeddings to be the harmonicity diff between the chords
siamese = keras.Model(inputs=[input_1, input_2], outputs=output)

def loss(y_true, y_pred):
    return tf.reduce_mean(tf.abs((y_true - y_pred))**3, axis=-1)

def contrastive_loss(margin=1):
    """Provides 'contrastive_loss' an enclosing scope with variable 'margin'.

    Arguments:
        margin: Integer, defines the baseline for distance for which pairs
                should be classified as dissimilar. - (default is 1).
    """
    def loss(y_true, y_pred):
        square_pred = tf.math.square(y_pred)
        margin_square = tf.math.square(tf.reduce_max(margin - (y_pred), 0))
        return tf.reduce_mean((1 - y_true) * square_pred + (y_true) * margin_square)

    return loss


siamese.compile(loss=contrastive_loss(margin=0.5), metrics=['mae'], optimizer=tf.keras.optimizers.Adam(learning_rate=0.1))

siamese.summary()

# Train
callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

siamese.fit(
    x=[c1_train, c2_train], 
    #x=[chords_onehot, weights_onehot], 
    y=h_train, 
    epochs=10000,  
    batch_size=1024,
    validation_data=([c1_test, c2_test], h_test), 
    callbacks=callback)

h_predicted = np.squeeze(siamese.predict([c1_test, c2_test]))
np.save('./data/siamese/h_predicted.npy', h_predicted)
np.save('./data/siamese/h_test.npy', h_test)
np.save('./data/siamese/c1_test.npy', c1_test)
np.save('./data/siamese/c2_test.npy', c2_test)

h_predicted.shape
h_test.shape

[l.trainable for l in siamese.get_layer("embedding_model").layers]
siamese.layers[2].name
siamese.layers[2].trainable=True
[l.trainable for l in siamese.get_layer("embedding_model").layers]

## Evaluate ##
# Known chord pair
c1 = np.zeros([1,12,11])
c2 = np.zeros([1,12,11])
c2[0,0,0] = 1
c2[0,4,0] = 1
c1c2 = ((c1 + c2)/2.0)
vc1 = embedding_network(c1)
vc2 = embedding_network(c2)
euclidean_distance([vc1, vc2])

bh = BatchHarmonicity(1)
hc1 = bh.calcHarmonicityFromWeightMatrix(c1, 11*12)
hc1c2 = bh.calcHarmonicityFromWeightMatrix(c1c2, 11*12)
relative_h = np.abs(hc1 - hc1c2)
relative_h

# Random chord pair
c1 = c1_all[1:2]
c2 = c2_all[1:2]
c1c2 = ((c1 + c2)/2.0)
vc1 = embedding_network(c1)
vc2 = embedding_network(c2)
euclidean_distance([vc1, vc2])

bh = BatchHarmonicity(1)
hc1 = bh.calcHarmonicityFromWeightMatrix(c1, 11*12)
hc1c2 = bh.calcHarmonicityFromWeightMatrix(c1c2, 11*12)
relative_h = np.abs(hc1 - hc1c2)
relative_h

#Conclusion: noisy chords have low relative harmonicity with each other.
# If that's what we're training on, we will have a poor idea of what more structured pairs look like
# Solution: investigate relative harmonicity distribution. 
# If that is the problem then simulate harmonic weight matrix decay based on random chord updates
# Investigate effect on harmonicity of clipping weights
num_chords_per_batch = 25
bh = BatchHarmonicity(num_chords_per_batch)
notes = 12
octaves = 11

chord_size = np.random.randint(1,13)
indices = np.random.randint(0,notes*octaves, chord_size*num_chords_per_batch).reshape(num_chords_per_batch, chord_size)
weights = np.random.random([num_chords_per_batch, chord_size])
c1 = np.zeros([num_chords_per_batch, notes*octaves])
np.put_along_axis(c1, indices, weights, axis=1)
c1 = c1.reshape([num_chords_per_batch, notes, octaves])

chord_size = np.random.randint(1,13)
indices = np.random.randint(0,notes*octaves, chord_size*num_chords_per_batch).reshape(num_chords_per_batch, chord_size)
weights = np.random.random([num_chords_per_batch, chord_size])
c2 = np.zeros([num_chords_per_batch, notes*octaves])
np.put_along_axis(c2, indices, weights, axis=1)
c2 = c2.reshape([num_chords_per_batch, notes, octaves])

#Calculate harmonicity
c1c2 = ((c1 + c2)/2.0)
hc1 = bh.calcHarmonicityFromWeightMatrix(c1, notes*octaves)
hc1c2 = bh.calcHarmonicityFromWeightMatrix(c1c2, notes*octaves)
relative_h = np.abs(hc1 - hc1c2)
relative_h


