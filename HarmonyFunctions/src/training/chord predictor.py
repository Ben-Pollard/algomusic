import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import numpy as np
from sklearn.metrics import r2_score, mean_absolute_error, mean_squared_error
from sklearn.model_selection import train_test_split
import os

# Data
data_folders = [f[0] for f in os.walk('data/distance')][1:]
lens = [len(np.load(f'{f}/hc1_all.npy')) for f in data_folders]
folders_training = data_folders[1:]
folders_validation = data_folders[0:1]
samples_training = sum(lens[1:]) * 2 #twice the length because we stack
samples_validation = sum(lens[0:1]) * 2

def countNotes(chord):
    isNote = tf.cast(chord > 0, tf.int32)
    return tf.math.reduce_sum(tf.math.reduce_sum(isNote, axis=1), axis=1)


def sumSquare(chord):
    return tf.math.reduce_sum(tf.math.reduce_sum(chord**2, axis=1), axis=1)

class Generator(keras.utils.Sequence):
    def __init__(self, batch_size, folders, samples, ablation_prob) -> None:
        self.folder_index = 0
        self.batch_size = batch_size
        self.folders = folders
        self.samples = samples
        self.ablation_prob = ablation_prob
        self.on_epoch_end()

    def __len__(self):
        return int(np.floor(self.samples / self.batch_size))
    
    def on_epoch_end(self):
        self.load()
        self.shuffle()

    def shuffle(self):
        p = np.random.permutation(self.examples_in_file)
        self.chords_2d = self.chords_2d[p]
        self.harmonicity = self.harmonicity[p]
        self.i = 0

    def load(self):
        c1 = np.load(f'{self.folders[self.folder_index]}/c1_all.npy')
        hc1 = np.load(f'{self.folders[self.folder_index]}/hc1_all.npy')
        c2 = np.load(f'{self.folders[self.folder_index]}/c2_all.npy')
        c1c2 = ((c1 + c2)/2.0)
        hc1c2 = np.load(f'{self.folders[self.folder_index]}/hc1c2_all.npy')
        self.chords_2d = np.vstack([c1, c1c2])
        self.harmonicity = np.hstack([hc1, hc1c2])
        self.examples_in_file = len(self.harmonicity)


    def __getitem__(self, idx):
        if self.i + self.batch_size > self.examples_in_file:
            if self.folder_index > len(self.folders): #check indexing
                self.folder_index = 0
            else:
                self.folder_index += 1
            self.load()
            self.shuffle()

        X_harmonicity = self.harmonicity[self.i:self.i+self.batch_size]
        y_harmonicity = X_harmonicity
        y_chord = self.chords_2d[self.i:self.i+self.batch_size]
        #ablation/noising of input chord
        y_chord = self.chords_2d[self.i:self.i+self.batch_size]
        X_chord = self.chords_2d[self.i:self.i+self.batch_size] * (np.random.random(y_chord.shape) > self.ablation_prob)
        X_num_notes = (y_chord > 0).sum(axis=1).sum(axis=1)
        x_sum_square = (y_chord**2).sum(axis=1).sum(axis=1)

        self.i += self.batch_size
        return [X_chord, X_harmonicity, X_num_notes], [y_chord, X_num_notes, x_sum_square, y_harmonicity]

[X_chord, X_harmonicity, X_num_notes], [y_chord, X_num_notes, x_sum_square, y_harmonicity] = training_generator.__getitem__(0)
np.round(X_chord[0], 1)
np.round(y_chord[0], 1)

### Model
# Re-use harmonicity predictor
harm_pred_model = keras.models.load_model('models/harm_pred_model.h5')
harm_pred_model.summary()
harm_pred_model.trainable=False

# Chord reconstructor
chord_input = keras.Input(shape=(12,11,1), name="input")
conv1 = layers.Conv2D(32, kernel_size=(1,11), activation=keras.layers.LeakyReLU(0.2))(chord_input)
remap = layers.Conv2D(16, kernel_size=1, activation=keras.layers.LeakyReLU(0.2))(conv1)
conv2 = layers.Conv2D(512, kernel_size=(12,1), activation=keras.layers.LeakyReLU(0.2))(remap)
flat = keras.layers.Flatten()(conv2)
harm_input = keras.Input(shape=(1))
num_notes_input = keras.Input(shape=(1))
concat = keras.layers.Concatenate()([flat, num_notes_input, harm_input])
deconv1 = keras.layers.Conv2DTranspose(512, kernel_size=(12,1), activation=keras.layers.LeakyReLU(0.2))(conv2)
deconv2 = keras.layers.Conv2DTranspose(512, kernel_size=(1,11), activation=keras.layers.LeakyReLU(0.2))(deconv1)
remapdeconv1 = layers.Conv2D(256, kernel_size=1, activation=keras.layers.LeakyReLU(0.2))(deconv2)
chord_out = layers.Conv2D(1, kernel_size=1, activation='sigmoid', name="predicted_chord")(remapdeconv1)
harm_out = harm_pred_model(chord_out)
chord_size_out = layers.Lambda(countNotes, name="num_notes")(chord_out)
sum_square_out = layers.Lambda(sumSquare, name="sum_square_notes")(chord_out)
encoder_decoder = keras.Model(inputs=[chord_input, harm_input, num_notes_input], outputs=[chord_out, chord_size_out, sum_square_out, harm_out], name="encoder_decoder")
denoising_encoder_decoder = keras.Model(inputs=encoder_decoder.inputs, outputs=encoder_decoder.outputs, name="denoising_encoder_decoder")

batch_size = 1024

def chordSimilarity(true, pred):
    pred_flat = tf.reshape(pred, [batch_size, -1])
    act_flat = tf.reshape(true, [batch_size, -1])
    return tf.keras.losses.cosine_similarity(y_true=pred_flat, y_pred=act_flat, axis=1)

callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

"""
Training process: 
- learn to recontruct input using cosine similarity
- ablate input and switch to other losses
- can be done by switching call to compile
"""

## Reconstruct input chords
encoder_decoder.compile(
    loss= {
            'predicted_chord': chordSimilarity,
            'num_notes': 'mae',
            'sum_square_notes': 'mae',
            'harmonicity_model': 'mae'
            },
    loss_weights = {
            'predicted_chord': 1,
            'num_notes': 0,
            'sum_square_notes': 0,
            'harmonicity_model': 0
            },
    optimizer=tf.keras.optimizers.Adam(learning_rate=0.0005))

training_generator = Generator(batch_size, folders_training, samples_training, ablation_prob=0)
validation_generator = Generator(batch_size, folders_validation, samples_validation, ablation_prob=0)

encoder_decoder.fit_generator(
    generator=training_generator, 
    validation_data=validation_generator, 
    use_multiprocessing=True,
    workers=6,
    epochs=3,  
    callbacks=callback)


## Guess ablated chords
denoising_encoder_decoder.compile(
    loss= {
            'predicted_chord': chordSimilarity,
            'num_notes': 'mae',
            'sum_square_notes': 'mae',
            'harmonicity_model': 'mae'
            },
    loss_weights = {
            'predicted_chord': 1,
            'num_notes': 1,
            'sum_square_notes': 1,
            'harmonicity_model': 1
            },
    optimizer=tf.keras.optimizers.Adam(learning_rate=0.0005))

training_generator = Generator(batch_size, folders_training, samples_training, ablation_prob=0.1)
validation_generator = Generator(batch_size, folders_validation, samples_validation, ablation_prob=0.1)

denoising_encoder_decoder.fit_generator(
    generator=training_generator, 
    validation_data=validation_generator, 
    use_multiprocessing=True,
    workers=6,
    epochs=1000,  
    callbacks=callback)

"""
Training
- Randomly ablate chords (no need to manually split)
- Input partial chord and target harmonicity and estimate both chord and harmonicity
- Harmonicity should match calculated harmonicity

- Input chord and evaluate against expected chord. Don't train the harmonicity prediction component
- Get output chord; calculate harmonicity; train harmonicity predictor

"""
