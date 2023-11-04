#Translate from interval to note+concordancy using conditional GAN

import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import keras_tuner as kt
import numpy as np
from sklearn.metrics import r2_score, mean_absolute_error
from sklearn.model_selection import train_test_split
import math
import pickle

#tf.keras.mixed_precision.experimental.set_policy('mixed_float16')

num_indices = 128
latent_dim = 100

### MODELS ###
harm_pred_model = keras.models.load_model('models/harm_pred_model')

#todo structure models to create octave embeddings - try with ff model first
def add(x):
    return tf.math.add(x[0], x[1])

def wasserstein_loss(y_true, y_pred):
        return tf.reduce_mean(y_true * y_pred)

# Discriminator
def defineCritic(harm_pred_model):
    harm_pred_model.trainable = False
    condition_input = keras.Input(shape=(12,11,1), name="condition_input")
    fixed_input = keras.Input(shape=(12,11,1), name="fixed_input")
    harmonicity_input = keras.Input(shape=(1,), name="harmonicity_input")
    addition = keras.layers.Lambda(add)((condition_input, fixed_input))
    harm_pred = harm_pred_model(addition)
    conv = layers.Conv2D(4, kernel_size=3, activation=keras.layers.LeakyReLU(0.2), padding="same")(addition)
    flat = keras.layers.Flatten()(conv)
    concat = keras.layers.Concatenate()([flat, harmonicity_input, harm_pred])
    dense1 = layers.Dense(64, activation=keras.layers.LeakyReLU(0.2))(concat)
    dense2 = layers.Dense(32, activation=keras.layers.LeakyReLU(0.2))(dense1)
    output = layers.Dense(1, activation=None, name="harmonicity")(dense2)
    discriminator = keras.Model(inputs=[condition_input, fixed_input, harmonicity_input], outputs=output, name="GAN")
    discriminator.summary()
    discriminator.compile(
        loss=wasserstein_loss, 
        optimizer=tf.keras.optimizers.Adam(learning_rate=0.0005, beta_1=0.5),
        metrics=['accuracy'])
    return discriminator


# Generator
# Condition and harmonicity go in, fixed comes out
def defineGenerator():
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
    generator = keras.Model(inputs=[condition_input_g, harmonicity_input_g, noise_input_g], outputs=conv_out_g, name="generator")
    generator.summary()
    return generator


def defineGAN(discriminator, generator):
    #noise, condition and harm go in, fixed comes out
    discriminator.trainable = False
    gen_condition, gen_harm, gen_noise = generator.input
    gen_out = generator.output
    gan_out = discriminator([gen_condition, gen_out, gen_harm])
    gan = keras.Model(inputs=[gen_condition, gen_harm, gen_noise], outputs=gan_out)
    gan.compile(
        loss=wasserstein_loss, 
        optimizer=tf.keras.optimizers.Adam(learning_rate=0.0005, beta_1=0.5),
        metrics=['accuracy'])
    return gan

critic = defineCritic(harm_pred_model)
generator = defineGenerator()
gan = defineGAN(critic, generator)
gan.summary()


### DATA ###

# True Data
def reindex_2d(index):
    note = (index) % 12
    octave = math.floor(index / 12)
    return [note, octave]

harmonicity = np.load('data/harmonicity.npy')
true_labels = np.ones(harmonicity.shape)
chords = np.load('data/chords.npy')
num_chords = chords.shape[0]
num_octaves = 11
condition_notes = np.zeros([num_chords, 12, num_octaves])
consequent_notes = np.zeros([num_chords, 12, num_octaves])
for i in range(0, num_chords):
    note1 = chords[i,0]
    note1_name, octave1 = reindex_2d(note1)
    condition_notes[i, note1_name, octave1] = 1

    note2 = chords[i,1]
    note2_name, octave2 = reindex_2d(note2)
    consequent_notes[i, note2_name, octave2] = 1



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
report_every_n_iterations = 100
epochs = 500
clip_value = 0.01
dreal_hist, dfake_hist, areal_hist, afake_hist, g_hist, harm_r_hist, unique_fakes_hist = [], [], [], [], [], [], []

for e in range(epochs):
    print(f'Epoch {e}')
    real_gen = RealDataGenerator(batch_size)
    iteration_counter = 0
    while True:
        try:
            ## Update critic on real data
            c, f, h, l = real_gen.__next__()
            d_loss_real, d_acc_real = critic.train_on_batch(x=[c, f, h], y=l)

            ## Update critic on fake data
            c_for_fake, _, h_for_fake, _ = real_gen.__next__()
            noise = np.random.standard_normal([batch_size, latent_dim])
            fake_fixed = generator([c_for_fake, h_for_fake, noise])
            y_false = np.zeros(fake_fixed.shape[0])
            d_loss_fake, d_acc_fake = critic.train_on_batch(x=[c, fake_fixed, h], y=y_false)

            # Clip critic weights
            for l in critic.layers:
                weights = l.get_weights()
                weights = [np.clip(w, -clip_value, clip_value) for w in weights]
                l.set_weights(weights)

            ## Update gan/generator
            c, f, h, l = real_gen.__next__()
            noise = np.random.standard_normal([batch_size, latent_dim])
            y_true = np.ones(fake_fixed.shape[0])
            g_loss, g_acc = gan.train_on_batch(x=[c,h,noise], y=y_true)

            if iteration_counter % report_every_n_iterations == 0:
                # Record history
                ## Ability of generated chords to create accurate generated harmonicity
                generated_harmonicity = np.squeeze(harm_pred_model([c_for_fake + np.squeeze(fake_fixed)]))
                harm_r = np.corrcoef([h_for_fake, generated_harmonicity])[0,1]
                harm_r_hist.append(harm_r)

                # Variance in fake chords
                distinct, counts = np.unique(np.round(fake_fixed), axis=0, return_counts=True)
                unique_fakes = len(counts)
                unique_fakes_hist.append(unique_fakes)

                dreal_hist.append(d_loss_real)
                dfake_hist.append(d_loss_fake)
                g_hist.append(g_loss)
                areal_hist.append(d_acc_real)
                afake_hist.append(d_acc_fake)
                print(f'd_real {d_loss_real:.5f}; d_fake {d_loss_fake:.5f}; g_loss {g_loss:.5f}; harm_r {harm_r:.5f}; unique_fakes {unique_fakes}')
            
            iteration_counter += 1
        except StopIteration:
            break


#Save results
with open('data/gan_results.pickle', 'wb') as f:
    gan_results = {
    'dreal_hist': dreal_hist, 
    'dfake_hist': dfake_hist, 
    'areal_hist': areal_hist, 
    'afake_hist': afake_hist, 
    'g_hist': g_hist,
    'harm_r_hist': harm_r_hist,
    'unique_fakes_hist': unique_fakes_hist,
    'generated_harmonicity': generated_harmonicity,
    'target_harmonicity': h_for_fake
    }
    pickle.dump(gan_results, f)


#Evaluate GAN
c = np.zeros([1,12, num_octaves])
c[0,0,0]=1.0
np.squeeze(c)
h = np.array([0.5])
noise = np.random.standard_normal([1, latent_dim])
fake_fixed = np.squeeze(generator([c, h, noise]))
np.round(fake_fixed)

import src.data_generators.Harmonicity as Harmonicity
chord = np.array([c.transpose().reshape(-1).argmax(),fake_fixed.transpose().reshape(-1).argmax()], dtype = np.int32)
weights = np.array([1.0, 1.0], np.float32)
result = Harmonicity.calcHarmonicity(chord, weights)
print(result)

# h[0,7] == 1.277445
chord = np.array([0,7], dtype = np.int32)
Harmonicity.calcHarmonicity(chord, weights)

# c[0,7] is at position 6, with matching harmonicity
chords[6]
harmonicity[6]

# chord[0,7] is at position 6 in chords_2d
chords_2d[6]