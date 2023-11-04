import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
from tensorflow.keras import regularizers
import keras_tuner as kt
import numpy as np
from sklearn.metrics import r2_score, mean_absolute_error
from sklearn.model_selection import train_test_split


num_indices = 128

chords = np.load('data/chords.npy')
weights = np.load('data/weights.npy')
chords_onehot = np.load('data/chords_onehot.npy')
weights_onehot = np.load('data/weights_onehot.npy')
weights_onehot_notename_first = np.load('data/weights_onehot_notename_first.npy')
harmonicity = np.load('data/harmonicity.npy')

#Show model that noisy inputs are not harmonic
noise = (np.random.random(chords_onehot.shape) - 0.5) * 2
noise_y = np.random.random(harmonicity.shape) * 0.1
X_mixed = np.vstack([chords_onehot, noise])
y_mixed = np.hstack([harmonicity, noise_y])

#todo 
# https://argmax.ai/blog/setinvariance/
# check we're not using both sides of the diagonal
# speed test vs scala
# scale up to more intervals
X_train, X_test, y_train, y_test = train_test_split(chords_onehot, harmonicity, test_size=0.1)
X_train_m, X_test_m, y_train_m, y_test_m = train_test_split(X_mixed, y_mixed, test_size=0.1)


# Define hypermodel
def model_builder(hp):
    #Hyperparams
    # layer_1_units = hp.Int('layer_1_units', min_value=8, max_value=1024, step=128)
    # layer_2_units = hp.Int('layer_2_units', min_value=4, max_value=512, step=64)
    # layer_3_units = hp.Int('layer_3_units', min_value=2, max_value=256, step=32)
    layer_1_l2 = hp.Float('layer_1_units', min_value=0, max_value=1, step=0.001)
    layer_2_l2 = hp.Float('layer_2_units', min_value=0, max_value=1, step=0.001)
    layer_3_l2 = hp.Float('layer_3_units', min_value=0, max_value=1, step=0.001)

    #Model def
    weight_input = keras.Input(shape=(num_indices,), name="weight_input")
    dense_1 = layers.Dense(904, activation='relu', activity_regularizer=regularizers.L2(layer_1_l2))(weight_input)
    dense_2 = layers.Dense(452, activation='relu', activity_regularizer=regularizers.L2(layer_2_l2))(dense_1)
    dense_3 = layers.Dense(226, activation='relu', activity_regularizer=regularizers.L2(layer_3_l2))(dense_2)
    output = layers.Dense(1, activation='relu', name="harmonicity")(dense_3)
    model = keras.Model(inputs=[weight_input], outputs=output, name="harmonicity_model")
    model.summary()
    model.compile(
        loss='mae', 
        optimizer=tf.keras.optimizers.Adam(learning_rate=0.0005))
    
    return model

tuner = kt.BayesianOptimization(
    model_builder,
    objective='val_loss',
    max_trials=100,
    overwrite=True
)

callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=10, restore_best_weights=True)

#Training
tuner.search(
    x=X_train, 
    #x=[chords_onehot, weights_onehot], 
    y=y_train, 
    epochs=15,  
    batch_size=harmonicity.size,
    validation_data=(X_test, y_test), 
    callbacks=[callback])

tuner.results_summary()
tuner.get_best_models(1)
tuner.get_best_hyperparameters(1)[0].get('layer_3_units')

##Optimised Model
weight_input = keras.Input(shape=(num_indices,), name="weight_input")
dense_1 = layers.Dense(904, activation=keras.layers.LeakyReLU(0.2))(weight_input)
dense_2 = layers.Dense(452, activation=keras.layers.LeakyReLU(0.2))(dense_1)
dense_3 = layers.Dense(226, activation=keras.layers.LeakyReLU(0.2))(dense_2)
dense_4 = layers.Dense(100, activation=keras.layers.LeakyReLU(0.2))(dense_3)
dense_5 = layers.Dense(50, activation=keras.layers.LeakyReLU(0.2))(dense_4)
output = layers.Dense(1, activation='relu', name="harmonicity")(dense_5)
model = keras.Model(inputs=[weight_input], outputs=output, name="harmonicity_model")
model.summary()
model.compile(
    loss='mae', 
    optimizer=tf.keras.optimizers.Adam(learning_rate=0.0005))

#train on real data
model.fit(
    x=X_train, 
    #x=[chords_onehot, weights_onehot], 
    y=y_train, 
    epochs=10000,  
    batch_size=harmonicity.size,
    validation_data=(X_test, y_test), 
    callbacks=callback)

#include noise
model.fit(
    x=X_train_m, 
    #x=[chords_onehot, weights_onehot], 
    y=y_train_m, 
    epochs=10000,  
    batch_size=harmonicity.size,
    validation_data=(X_test_m, y_test_m), 
    callbacks=callback)




## Evaluate
predicted = np.squeeze(model.predict([X_test]))
np.save('data/predicted.npy', predicted)
np.save('data/y_test.npy', y_test)
print(f"R-squared: {r2_score(y_test, predicted)}")
print(f"MAE: {mean_absolute_error(y_test, predicted)}")


X_17 = np.zeros(128)
X_17.shape
X_17[0] = 1
X_17[7] = 1
model.predict(np.expand_dims(X_17, 0))

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
model.predict(np.expand_dims(X_test_noisy, 0))

## Gradient ascent
def loss(X, yhat, target):
    target_loss = tf.reduce_mean(tf.abs(yhat - target))
    binary_loss = tf.reduce_mean((input_data**2-input_data)**2)
    dual_loss = tf.reduce_mean(np.abs(input_data) - 2)
    return target_loss + binary_loss + dual_loss

#init = tf.convert_to_tensor(np.expand_dims(np.random.rand(num_indices), 0))
init = tf.convert_to_tensor(np.expand_dims(np.zeros(num_indices), 0))
input_data = tf.Variable(tf.cast(init, tf.float32))
step_size = 0.01
for _ in range(100):
    with tf.GradientTape() as g:
        yhat = model(input_data)
        loss_value = loss(input_data, yhat, 1.277445)
        print(loss_value)
        grads = g.gradient(loss_value, input_data)
        #normalized_grads = grads / (tf.sqrt(tf.reduce_mean(tf.square(grads))) + 1e-5)
        _ = input_data.assign_sub(grads * step_size)

model(input_data)
np.round(input_data.numpy(), 3)