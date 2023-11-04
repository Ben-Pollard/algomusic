#Translate from interval to note+concordancy using conditional GAN

import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import keras_tuner as kt
import numpy as np
from sklearn.metrics import r2_score, mean_absolute_error, mean_squared_error
from sklearn.model_selection import train_test_split
import math
import pickle
from src.data_generators.BatchHarmonicity import BatchHarmonicity

#tf.keras.mixed_precision.experimental.set_policy('mixed_float16')

num_indices = 128
latent_dim = 100

### MODELS ###

def clipChords(layerInput):
    chords, clip_to = layerInput
    clip_to_top_n_weights = clip_to[0]
    chord_flat = tf.reshape(chords, (-1, 132))
    to_keep = tf.argsort(chord_flat, axis=1)[:,-clip_to_top_n_weights:]
    keep_above = tf.math.reduce_min(tf.gather(chord_flat, to_keep, axis=1, batch_dims=1), axis=1)
    mask = tf.cast(tf.math.greater_equal(chord_flat, tf.expand_dims(keep_above, 1)), tf.float32)
    clipped_chord = tf.reshape(chord_flat * mask, (-1,12,11))
    return clipped_chord

def getClippingHarmPredModel():
    harm_pred_model = keras.models.load_model('models/harm_pred_model')

    pre_input = keras.Input(shape=(12,11,1), name="pre_input_to_clip")
    clip_to = keras.Input(shape=(), dtype=tf.int32, name="clip_to")
    clipping = keras.layers.Lambda(clipChords, name="clipping")((pre_input, clip_to))
    harm_pred = harm_pred_model(clipping)
    
    clipping_harm_pred_model = keras.Model(inputs=[pre_input, clip_to], outputs=harm_pred, name='clipping_harm_pred_model')
    
    clipping_harm_pred_model.summary()

    clipping_harm_pred_model.compile(
        loss='mae', 
        optimizer=tf.keras.optimizers.Adam(learning_rate=0.0005))
       
    return clipping_harm_pred_model


#todo structure models to create octave embeddings - try with ff model first
def add(x):
    return tf.math.add(x[0], x[1])

def subtract(x):
    return tf.math.subtract(x[0], x[1])

# Discriminator
def defineDiscriminator(harm_pred_model):
    harm_pred_model.trainable = False
    condition_input = keras.Input(shape=(12,11,1), name="condition_input")
    fixed_input = keras.Input(shape=(12,11,1), name="fixed_input")
    harmonicity_input = keras.Input(shape=(1,), name="harmonicity_input")
    clip_to_input = keras.Input(shape=(), dtype=tf.int32, name="discriminator_clip_to_input")
    addition = keras.layers.Lambda(add)((condition_input, fixed_input))
    harm_pred = harm_pred_model([addition, clip_to_input])
    conv = layers.Conv2D(4, kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="same")(addition)
    flat = keras.layers.Flatten()(conv)
    concat = keras.layers.Concatenate()([flat, harmonicity_input, harm_pred])
    dense1 = layers.Dense(64, activation=keras.layers.LeakyReLU(0.2))(concat)
    dropout1 = layers.Dropout(0.1)(dense1)
    dense2 = layers.Dense(32, activation=keras.layers.LeakyReLU(0.2))(dropout1)
    dropout2 = layers.Dropout(0.1)(dense2)
    output_discriminate = layers.Dense(1, activation='sigmoid', name="output_discriminate")(dropout2)
    discriminator = keras.Model(inputs=[condition_input, fixed_input, harmonicity_input, clip_to_input], outputs=[output_discriminate, harm_pred], name="GAN")
    discriminator.summary()
    discriminator.compile(
        loss= {
            'output_discriminate': 'binary_crossentropy',
            'clipping_harm_pred_model': 'mae'
            },
        loss_weights = {
            'output_discriminate':1.0,
            'clipping_harm_pred_model':0.0
            },
        optimizer=tf.keras.optimizers.Adam(learning_rate=0.0005, beta_1=0.5),
        metrics=['accuracy'])
    return discriminator


# Generator
# Condition and harmonicity go in, fixed comes out
def defineGenerator(harm_pred_model):
    harm_pred_model.trainable = False
    
    #Generate output chord
    noise_input_g = keras.Input(shape=(latent_dim,), name="generator_noise_input")
    condition_input_g = keras.Input(shape=(12,11,1), name="generator_condition_input")
    harmonicity_input_g = keras.Input(shape=(1,), name="generator_harmonicity_input")
    harm_embedding_g = keras.layers.Reshape((12,1,1), name="harm_embedding")(layers.Dense(12, activation=keras.layers.LeakyReLU(0.2))(harmonicity_input_g))
    noise_embedding_g = keras.layers.Reshape((12,1,1), name="noise_embedding")(layers.Dense(12, activation=keras.layers.LeakyReLU(0.2))(noise_input_g))
    concat_g = keras.layers.Concatenate(axis=2)([condition_input_g, harm_embedding_g, noise_embedding_g])
    deconv1_g = keras.layers.Conv2DTranspose(16, kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="same")(concat_g)
    deconv2_g = keras.layers.Conv2DTranspose(8, kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="same")(deconv1_g)
    deconv3_g = keras.layers.Conv2DTranspose(4, kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="same")(deconv2_g)
    conv_to_shape = keras.layers.Conv1D(4, data_format="channels_last", kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="valid")(deconv3_g)
    conv_out_g = layers.Conv2D(1, kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="same")(conv_to_shape)
    
    #Compare harmonicity of output chord to input harmonicity
    clip_to_input = keras.Input(shape=(), dtype=tf.int32, name="generator_clip_to_input")
    harm_pred_g = harm_pred_model([conv_out_g, clip_to_input])
    harm_diff_g = keras.layers.Reshape((1,1,1), name="harm_diff")(keras.layers.Lambda(subtract)((harmonicity_input_g, harm_pred_g)))
    
    # Further pass to correct for harmonicity
    deconv1_compare_g = keras.layers.Lambda(add)((deconv1_g, harm_diff_g))
    deconv2_compare_g = keras.layers.Conv2DTranspose(16, kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="same")(deconv1_compare_g)
    deconv3_compare_g = keras.layers.Conv2DTranspose(4, kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="same")(deconv2_compare_g)
    conv_to_shape_compare_g = keras.layers.Conv1D(4, data_format="channels_last", kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="valid")(deconv3_compare_g)
    conv_out_compare_g = layers.Conv2D(1, kernel_size=3, activation='relu', padding="same")(conv_to_shape_compare_g)

    generator = keras.Model(inputs=[condition_input_g, harmonicity_input_g, noise_input_g, clip_to_input], outputs=conv_out_compare_g, name="generator")
    generator.summary()
    return generator


def defineHarmonicityOptimisingGenerator(harm_pred_model):
    harm_pred_model.trainable = False
    
    #Generate output chord
    noise_input_g = keras.Input(shape=(latent_dim,), name="generator_noise_input")
    condition_input_g = keras.Input(shape=(12,11,1), name="generator_condition_input")
    harmonicity_input_g = keras.Input(shape=(1,), name="generator_harmonicity_input")
    harm_embedding_g = keras.layers.Reshape((12,1,1), name="harm_embedding")(layers.Dense(12, activation=keras.layers.LeakyReLU(0.2))(harmonicity_input_g))
    noise_embedding_g = keras.layers.Reshape((12,1,1), name="noise_embedding")(layers.Dense(12, activation=keras.layers.LeakyReLU(0.2))(noise_input_g))
    concat_g = keras.layers.Concatenate(axis=2)([condition_input_g, harm_embedding_g, noise_embedding_g])
    deconv1_g = keras.layers.Conv2DTranspose(32, kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="same")(concat_g)
    deconv2_g = keras.layers.Conv2DTranspose(4, kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="same")(deconv1_g)
    conv_to_shape = keras.layers.Conv1D(4, data_format="channels_last", kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="valid")(deconv2_g)
    conv_out_g = layers.Conv2D(1, kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="same")(conv_to_shape)
    
    #Compare harmonicity of output chord to input harmonicity
    addition = keras.layers.Lambda(add)((condition_input_g, conv_out_g))
    clip_to_input = keras.Input(shape=(), dtype=tf.int32, name="generator_clip_to_input")
    harm_pred_g = harm_pred_model([addition, clip_to_input])

    generator = keras.Model(inputs=[condition_input_g, harmonicity_input_g, noise_input_g, clip_to_input], outputs=[conv_out_g, harm_pred_g], name="generator")
    generator.summary()
    return generator


def defineGAN(discriminator, generator):
    #noise, condition and harm go in, fixed comes out
    discriminator.trainable = False
    gen_condition, gen_harm, gen_noise, gen_clip_to = generator.input
    gen_out_chord = generator.output
    discr_out_classify, discr_out_harmonicity = discriminator([gen_condition, gen_out_chord, gen_harm, gen_clip_to]) #is this right? do we want the clipping inputs tied?
    gan = keras.Model(inputs=[gen_condition, gen_harm, gen_noise, gen_clip_to], outputs=[discr_out_classify, discr_out_harmonicity])
    gan.compile(
        loss= {
            'GAN': 'binary_crossentropy', #output_discriminate
            'GAN_1': 'mae' #harmonicity
            },
        loss_weights = {
            'GAN':1.0,
            'GAN_1':1.0
            },
        optimizer=tf.keras.optimizers.Adam(learning_rate=0.0005, beta_1=0.5),
        metrics=['accuracy'])
    return gan

clipping_harm_pred_model = getClippingHarmPredModel()
discriminator = defineDiscriminator(clipping_harm_pred_model)
generator = defineGenerator(clipping_harm_pred_model)
#generator = defineHarmonicityOptimisingGenerator(clipping_harm_pred_model)
gan = defineGAN(discriminator, generator)
gan.summary()

### DATA ###

def reindex_2d(index):
    note = (index) % 12
    octave = math.floor(index / 12)
    return [note, octave]

num_records = 1000000
harmonicity = np.array(np.load('data/harmonicity.npy'), dtype=np.float32)[:num_records]
chords_2d = np.load('data/chords_2d.npy')[:num_records]
true_labels = np.ones(harmonicity.shape)

# condition_notes = np.array(np.load('data/chords_2d.npy'), dtype=np.float32)
# consequent_notes = np.array(np.load('data/chords_2d.npy'), dtype=np.float32)
condition_notes = np.copy(chords_2d)
consequent_notes = np.copy(chords_2d)
# Break chords into condition and consequent
top2 = np.argsort(chords_2d.transpose([0,2,1]).reshape([chords_2d.shape[0], -1]), axis=1).take([-1, -2], axis=1)
for i in range(len(chords_2d)):
    cond, cons = np.random.permutation(top2[i])
    note_cond, octave_cond = reindex_2d(cond)
    note_cons, octave_cons = reindex_2d(cons)
    condition_notes[i, note_cons, octave_cons] = 0.0 #remove consequent notes from condition chord
    consequent_notes[i, note_cond, octave_cond] = 0.0 #remove condition notes from consequent chord




# Generator for GAN training
class RealDataGenerator:
    records = len(true_labels)

    def shuffle(self):
        p = np.random.permutation(self.records)
        self.condition = condition_notes[p]
        self.fixed = consequent_notes[p]
        self.harm = harmonicity[p]
        self.labels = true_labels[p]
        self.i = 0
    
    def __init__(self, batch_size) -> None:
        self.batch_size = batch_size
        self.shuffle()

    def __next__(self):
        if self.i + self.batch_size > self.records:
            raise StopIteration
        c = self.condition[self.i:self.i+self.batch_size]
        f = self.fixed[self.i:self.i+self.batch_size]
        h = self.harm[self.i:self.i+self.batch_size]
        l = self.labels[self.i:self.i+self.batch_size]
        self.i += self.batch_size
        return c, f, h, l


#### TRAINING ####


## Train GAN
#https://machinelearningmastery.com/how-to-develop-a-conditional-generative-adversarial-network-from-scratch/
## Update discriminator on real data
batch_size = 128
clip_to_num = np.ones(batch_size, dtype=np.int32)*2
num_chords_per_batch = 64
bh = BatchHarmonicity(num_chords_per_batch)
report_every_n_iterations = 100
epochs = 25
clip_value = 0.01
dreal_hist, dfake_hist, areal_hist, afake_hist, g_total_hist, g_adversarial_hist, g_harm_loss_est_hist, g_harm_loss_actual_hist, harm_r_hist, harm_r2_hist, unique_fakes_hist, harm_pred_hist = [], [], [], [], [], [], [], [], [], [], [], []

for e in range(epochs):
    print(f'Epoch {e}')
    real_gen = RealDataGenerator(batch_size)
    iteration_counter = 0
    while True:
        try:
            
            ## Update discriminator on real data
            # Clipping: the discriminator should judge the generator based on the harmonicity of the chord clipped to
            # the same size as it would be when the generator is in usage.
            # The generator cannot then hack the harmonicity by providing chords that are of a different size
            # This is the real data, so the chords will always be of the correct size
            c, f, h, l = real_gen.__next__()
            _, d_loss_real, _, d_acc_real, _ = discriminator.train_on_batch(x=[c, f, h, clip_to_num], y=[l, l])

            ## Update discriminator on fake data
            # Clipping: this is the fake data, so the chords must be clipped to the correct size for the discriminator
            # The chords are also being clipped to the correct size for the generator's internal harmonicity evaluation
            c_for_fake, _, h_for_fake, _ = real_gen.__next__()
            noise = np.random.standard_normal([batch_size, latent_dim])
            fake_fixed = generator([c_for_fake, h_for_fake, noise, clip_to_num])
            y_false = np.zeros(fake_fixed.shape[0])
            _, d_loss_fake, _, d_acc_fake, _ = discriminator.train_on_batch(x=[c, fake_fixed, h, clip_to_num], y=[y_false, y_false])

            ## Update gan/generator
            # Clipping: the clip_to value gets passed to the generator input. It uses this to judge whether its fake chord has the correct harmonicity
            # It gets passed along to the discriminator, which clips the fake chord before estimating its harmonicity
            c, f, h, l = real_gen.__next__()
            noise = np.random.standard_normal([batch_size, latent_dim])
            y_true = np.ones(fake_fixed.shape[0])
            g_total_loss, g_adversarial_loss, g_harm_loss_est, g_adversarial_accuracy, _ = gan.train_on_batch(x=[c,h,noise,clip_to_num], y=[y_true, h]) # h is real, unmodelled harmonicity
            #gan.metrics_names
            # Updates from harmonicity function
            #Get the fake chord (conditional and fixed combined)
            harm_pred_X = c_for_fake + np.squeeze(fake_fixed)

            ##Generate calculated harmonicity
            max_num_notes_for_harmonicity_calc = 2
            clipped_harmonicity = bh.calcHarmonicityFromWeightMatrix(harm_pred_X, clip_to_top_n_weights=max_num_notes_for_harmonicity_calc)
            # Update harmonicity prediction model with noisy input from generator
            harm_pred_loss = clipping_harm_pred_model.train_on_batch(x=[harm_pred_X,clip_to_num], y=clipped_harmonicity)
            # harm_pred_loss=99


            if iteration_counter % report_every_n_iterations == 0:
                # Record history
                ## Ability of generated chords to create accurate generated harmonicity
                expected_num_notes = 2
                generated_harmonicity = bh.calcHarmonicityFromWeightMatrix(harm_pred_X, clip_to_top_n_weights=expected_num_notes)
                harm_r = np.corrcoef([h_for_fake, generated_harmonicity])[0,1]
                harm_r2 = r2_score(h_for_fake, generated_harmonicity)
                g_harm_loss_act = mean_absolute_error(h_for_fake, generated_harmonicity)
                harm_r_hist.append(harm_r)
                harm_r2_hist.append(harm_r2)

               
                # Variance in fake chords
                distinct, counts = np.unique(np.round(fake_fixed), axis=0, return_counts=True)
                unique_fakes = len(counts)
                unique_fakes_hist.append(unique_fakes)

                dreal_hist.append(d_loss_real)
                dfake_hist.append(d_loss_fake)
                g_total_hist.append(g_total_loss)
                g_adversarial_hist.append(g_adversarial_loss)
                g_harm_loss_est_hist.append(g_harm_loss_est)
                g_harm_loss_actual_hist.append(g_harm_loss_act) 
                areal_hist.append(d_acc_real)
                afake_hist.append(d_acc_fake)
                harm_pred_hist.append(harm_pred_loss)
                print(f'd_real {d_loss_real:.5f}; d_fake {d_loss_fake:.5f}; g_total_loss {g_total_loss:.5f}; g_adversarial_loss {g_adversarial_loss:.5f}; g_harm_loss_est {g_harm_loss_est:.5f}; g_harm_loss_act {g_harm_loss_act:.5f}; harm_r {harm_r:.5f}; harm_r2 {harm_r2:.5f}; harm_pred_loss {harm_pred_loss: .5f}; unique_fakes {unique_fakes}')

            iteration_counter += 1
        except StopIteration:
            break


## Save models
discriminator.save('models/discriminator')
generator.save('models/generator')
gan.save('models/gan')

discriminator = keras.models.load_model('models/discriminator')
generator = keras.models.load_model('models/generator')
gan = keras.models.load_model('models/gan')

## Save evaluation data
# Get output from generator
real_gen = RealDataGenerator(batch_size)
c, _, h, _ = real_gen.__next__()
f = generator([c, h, noise, clip_to_num])

#Get harmonicity from supervised model
generated_est_harmonicity = np.squeeze(clipping_harm_pred_model([c + np.squeeze(f), clip_to_num]))
generated_harmonicity_from_function = bh.calcHarmonicityFromWeightMatrix(c + np.squeeze(f), clip_to_top_n_weights=max_num_notes_for_harmonicity_calc)

np.corrcoef([h, generated_est_harmonicity])[0,1]
np.corrcoef([h, generated_harmonicity_from_function])[0,1]

#Save results
with open('data/gan_results.pickle', 'wb') as f:
    gan_results = {
    'dreal_hist': dreal_hist, 
    'dfake_hist': dfake_hist, 
    'areal_hist': areal_hist, 
    'afake_hist': afake_hist, 
    'g_total_hist': g_total_hist,
    'g_adversarial_hist': g_adversarial_hist,
    'g_harm_loss_est_hist': g_harm_loss_est_hist,
    'g_harm_loss_actual_hist': g_harm_loss_actual_hist,
    'harm_r_hist': harm_r_hist,
    'harm_r2_hist': harm_r2_hist,
    'unique_fakes_hist': unique_fakes_hist,
    'generated_est_harmonicity': generated_est_harmonicity,
    'generated_harmonicity_from_function': generated_harmonicity_from_function,
    'target_harmonicity': h
    }
    pickle.dump(gan_results, f)

""" Notes
Results
    - epoch 25 on a selection of 1m - g_harm_loss_est 0.05439; g_harm_loss_act 0.05330; harm_r 0.97345; harm_r2 0.86984; harm_pred_loss  0.00575; unique_fakes 35

Architecture Ideas:
    - Larger kernel sizes or more conv layers. 5 3x3 layers will build up to 11.
    - Assumption of locality of low-level features doesn't hold. What we are interested in is whole rows and whole columns. So try a parallel pair of 1x12 and 11x1 kernels (tuples to kernel_size)
    - Direct convnet with chord as output
    - Encoder-decoder. Encode the partial input + harm target, decode to output chord
"""
#todo is weight of c always 1?
c.max(axis=0)