import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import keras_tuner as kt
import numpy as np
from sklearn.metrics import r2_score, mean_absolute_error
from sklearn.model_selection import train_test_split
import os

# Data
#chords_2d = np.load('data/chords_2d.npy') #example, note, octave
#harmonicity = np.load('data/harmonicity.npy')
data_folders = [f[0] for f in os.walk('data/distance')][1:]
lens = [len(np.load(f'{f}/hc1_all.npy')) for f in data_folders]
folders_training = data_folders[1:]
folders_validation = data_folders[0:1]
samples_training = sum(lens[1:]) * 2 #twice the length because we stack
samples_validation = sum(lens[0:1]) * 2

class Generator(keras.utils.Sequence):
    def __init__(self, batch_size, folders, samples) -> None:
        self.folder_index = 0
        self.batch_size = batch_size
        self.folders = folders
        self.samples = samples
        self.on_epoch_end()

    def __len__(self):
        return int(np.floor(self.samples / self.batch_size))
    
    def on_epoch_end(self):
        self.load()
        self.shuffle()

    def shuffle(self):
        p_train = np.random.permutation(self.train_records)
        p_test = np.random.permutation(self.test_records)
        self.X_train = self.X_train[p_train]
        self.y_train = self.y_train[p_train]
        self.X_test = self.X_test[p_test]
        self.y_test = self.y_test[p_test]
        self.i = 0

    def load(self):
        c1 = np.load(f'{self.folders[self.folder_index]}/c1_all.npy')
        hc1 = np.load(f'{self.folders[self.folder_index]}/hc1_all.npy')
        c2 = np.load(f'{self.folders[self.folder_index]}/c2_all.npy')
        c1c2 = ((c1 + c2)/2.0)
        hc1c2 = np.load(f'{self.folders[self.folder_index]}/hc1c2_all.npy')
        chords_2d = np.vstack([c1, c1c2])
        harmonicity = np.hstack([hc1, hc1c2])
        #todo don't split
        self.X_train, self.X_test, self.y_train, self.y_test = train_test_split(chords_2d, harmonicity, test_size=0.1, shuffle=False)
        self.train_records = len(self.y_train)
        self.test_records = len(self.y_test)

    def __getitem__(self, idx):
        if self.i + self.batch_size > self.train_records:
            if self.folder_index > len(self.folders): #check indexing
                self.folder_index = 0
            else:
                self.folder_index += 1
            self.load()
            self.shuffle()
        X_train = self.X_train[self.i:self.i+self.batch_size]
        y_train = self.y_train[self.i:self.i+self.batch_size]
        self.i += self.batch_size
        return X_train, y_train

training_generator = Generator(1024, folders_training, samples_training)
validation_generator = Generator(1024, folders_validation, samples_validation)

# Model
def transpose(x):
    return tf.transpose(x, perm=(0,2,1,3))

def transpose_last(x):
    return tf.transpose(x, perm=(0,2,3,1))


def squeeze1(x):
    return tf.squeeze(x,1)

def squeeze2(x):
    return tf.squeeze(x,2)


input = keras.Input(shape=(12,11,1), name="input")
conv1 = layers.Conv2D(32, name="conv1", kernel_size=(1,11), activation=keras.layers.LeakyReLU(0.2))(input)
remap = layers.Conv2D(16, name="remap", kernel_size=1, activation=keras.layers.LeakyReLU(0.2))(conv1)
conv2 = layers.Conv2D(32, name="conv2", kernel_size=(12,1), activation=keras.layers.LeakyReLU(0.2))(remap)
flat = keras.layers.Flatten()(conv2)
dense1 = layers.Dense(32, name="dense1", activation=keras.layers.LeakyReLU(0.2))(flat)
dense2 = layers.Dense(16, name="dense2", activation=keras.layers.LeakyReLU(0.2))(dense1)
dense3 = layers.Dense(8, name="dense3", activation=keras.layers.LeakyReLU(0.2))(dense2)
output = layers.Dense(1, activation=keras.layers.LeakyReLU(0.2), name="h1armonicity")(dense3)
harm_pred_model = keras.Model(inputs=[input], outputs=output, name="harmonicity_model")
harm_pred_model.summary()
harm_pred_model.compile(
    loss='mae', 
    optimizer=tf.keras.optimizers.Adam(learning_rate=0.0005))

# Train
callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

harm_pred_model.fit_generator(
    generator=training_generator, 
    validation_data=validation_generator, 
    use_multiprocessing=True,
    workers=4,
    epochs=10000,  
    callbacks=callback)

## Evaluate
predicted = np.squeeze(harm_pred_model.predict([validation_generator.X_test]))
print(f"R-squared: {r2_score(validation_generator.y_test, predicted)}")
print(f"MAE: {mean_absolute_error(validation_generator.y_test, predicted)}")

# Save
harm_pred_model.save('models/harm_pred_model.h5')
harm_pred_model = keras.models.load_model('models/harm_pred_model.h5')
np.save('data/predicted.npy', predicted)
np.save('data/y_test.npy', validation_generator.y_test)


X_17 = np.zeros(128)
X_17.shape
X_17[0] = 1
X_17[7] = 1
harm_pred_model.predict(np.expand_dims(X_17, 0))

import src.data_generators.Harmonicity as Harmonicity
chord = np.array([0,7], dtype = np.int32)
weights = np.array([1.0, 1.0], np.float32)
result = Harmonicity.calcHarmonicity(chord, weights)
print(result)

#predict noisy data
X_test_noisy = X_17.copy()
X_test_noisy[10] = 0.1
X_test_noisy[100] = 0.1
X_test_noisy[77] = 0.1
X_test_noisy[33] = 0.1
X_test_noisy[55] = 0.1
harm_pred_model.predict(np.expand_dims(X_test_noisy, 0))

## Gradient ascent
def loss(X, yhat, target):
    target_loss = tf.reduce_mean(tf.abs(yhat - target))
    binary_loss = tf.reduce_mean((input_data**2-input_data)**2)
    dual_loss = tf.reduce_mean(np.abs(input_data) - 2)
    return target_loss + binary_loss + dual_loss

#init = tf.convert_to_tensor(np.expand_dims(np.random.rand(num_indices), 0))
init = tf.convert_to_tensor(np.expand_dims(np.zeros([12,11,1]), 0))
input_data = tf.Variable(tf.cast(init, tf.float32))
step_size = 0.01
for _ in range(100):
    with tf.GradientTape() as g:
        yhat = harm_pred_model(input_data)
        loss_value = loss(input_data, yhat, 1.277445)
        print(loss_value)
        grads = g.gradient(loss_value, input_data)
        #normalized_grads = grads / (tf.sqrt(tf.reduce_mean(tf.square(grads))) + 1e-5)
        _ = input_data.assign_sub(grads * step_size)

harm_pred_model(input_data)
np.squeeze(np.round(input_data.numpy(), 3))

