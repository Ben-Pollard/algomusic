import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import numpy as np
from sklearn.metrics import r2_score, mean_absolute_error
from sklearn.model_selection import train_test_split
from src.data_generators.BatchHarmonicity import BatchHarmonicity

#
# hc1_all = np.load('data/distance/0/hc1_all.npy')
# hc1c2_all = np.load('data/distance/0/hc1c2_all.npy')
# relative_h_all = np.load('data/distance/0/relative_h_all.npy')

# Data
c1_all = np.load('data/distance/0/c1_all.npy')
c2_all = np.load('data/distance/0/c2_all.npy')
c1c2_all = ((c1_all + c2_all)/2.0)
hc1c2_all = np.load('data/distance/0/hc1c2_all.npy')
c1_train, c1_test, c2_train, c2_test, h_train, h_test = train_test_split(c1_all, c2_all, hc1c2_all, test_size=0.1)

c1_all.shape
c2_all.shape
hc1c2_all.shape

def euclidean_distance(vectors):
    x, y = vectors
    sum_square = tf.math.reduce_sum(tf.math.square(x-y), axis=1, keepdims=True)
    return tf.math.sqrt(tf.math.maximum(sum_square, tf.keras.backend.epsilon()))

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


# Embedding network
input = keras.Input(shape=(12,11,1), name="input")
input_norm = layers.BatchNormalization()(input)
conv1 = layers.Conv2D(32, kernel_size=(1,11), activation="tanh")(input_norm)
remap = layers.Conv2D(16, kernel_size=1, activation="tanh")(conv1)
conv2 = layers.Conv2D(32, kernel_size=(12,1), activation="tanh")(remap)
flat = keras.layers.Flatten()(conv2)

remapping_network = keras.Model(inputs=[input], outputs=flat, name="harmonicity_model")
remapping_network.summary()

input_1 = keras.Input(shape=(12,11,1), name="input_1")
input_2 = keras.Input(shape=(12,11,1), name="input_2")
tower_1 = remapping_network(input_1)
tower_2 = remapping_network(input_2)


merge = layers.Concatenate(axis=1)([tower_1, tower_2])
normal = layers.BatchNormalization()(merge)
dense1 = layers.Dense(32, activation="tanh")(normal)
dense2 = layers.Dense(16, activation=keras.layers.LeakyReLU(0.2))(dense1)
dense3 = layers.Dense(8, activation=keras.layers.LeakyReLU(0.2))(dense2)
output = layers.Dense(1, activation=keras.layers.LeakyReLU(0.2))(dense3)

harm_pred_model = keras.Model(inputs=[input_1, input_2], outputs=output)

harm_pred_model.compile(
    loss='mae', 
    optimizer=tf.keras.optimizers.Adam(learning_rate=0.0005))

callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

harm_pred_model.fit(
    x=[c1_train, c2_train], 
    y=h_train, 
    validation_data=[[c1_test, c2_test], h_test],
    batch_size=256,
    use_multiprocessing=True,
    workers=4,
    epochs=10000,  
    callbacks=callback)

predicted = np.squeeze(harm_pred_model.predict([c1_test, c2_test]))
print(f"R-squared: {r2_score(h_test, predicted)}")
print(f"MAE: {mean_absolute_error(h_test, predicted)}")


# embedding_network = keras.Model(input, dense1, name="embedding_model")
# embedding_network.summary()

# Siamese network
input_1 = keras.Input(shape=(12,11,1), name="input_1")
input_2 = keras.Input(shape=(12,11,1), name="input_2")
tower_1 = embedding_network(input_1)
tower_2 = embedding_network(input_2)
merge = layers.Lambda(euclidean_distance, output_shape=(1,))([tower_1, tower_2])
normal = layers.BatchNormalization()(merge)
output = layers.Dense(1, activation="tanh")(normal)
siamese_network = keras.Model(inputs=[input_1, input_2], outputs=merge)

siamese_network.compile(loss='mae', metrics=['mae'], optimizer=tf.keras.optimizers.Adam(learning_rate=0.1))
siamese_network.summary()
callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

siamese_network.fit(
    x=[c1_train, c2_train], 
    #x=[chords_onehot, weights_onehot], 
    y=h_train, 
    epochs=10000,  
    batch_size=1024,
    validation_data=([c1_test, c2_test], h_test), 
    callbacks=callback)

h_predicted = np.squeeze(siamese_network.predict([c1_test, c2_test]))
print(f"R-squared: {r2_score(h_test, h_predicted)}")
print(f"r: {np.corrcoef(h_test, h_predicted)[0,1]}")
print(f"MAE: {mean_absolute_error(h_test, h_predicted)}")

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


