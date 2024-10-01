# Data Generators

## Batch Harmonicity 
* Calculates harmonicity given midi indices and weights
* Indices can be expressed by their vector index - so just a vector or array of weights is required
* Can be clipped to the top n weights

## Generate Distances
* Distance is relative harmonicity i.e. the change in harmonicity of chord 1 (C1) when chord 2 (C2) is averaged with it.
* 1m chords stored in ./data/distance
* Individual chords have {1,12} notes with weights sampled at random


## Generate Intervals
* Every interval is represented from the MIDI space, each with 250 sets of random weights
* Stored in .data/chords.npy and .data/weights.npy and .data/harmonicity.npy

## Generate Frequency Intervals
* Not implemented
* Intention is to represent the continuous relationship between frequency and harmonicity


# Models
## Chord Predictor
* Uses distance data
* De-noiser that takes a noisy chord and reconstructs it from the harmonicity value

## GAN Conv / Gan Harm
* Uses chord data from generate_intervals.py
* Conditional GAN that takes a chord and a target harmonicity as input, outputs a generated chord
* Good correlation between conditional harmonicity actual harmonicity of generated interval / chord
* Would need to be much more complex if this is going to work - conditioning on multiple chords, chord leading space, and ability to generate from polyphonic chords

## Train conv/ff
* Uses distance data but just predicts harmonicity
* Very accurate
* Evaluated by eval_predictions.ipynb
* conv obsoletes ff
* Model stored at ./models/harm_pred_model.h5

## Siamese
* Attempt to build harmonicity space representation of chords
* Uses embeddings from harm_pred_model.h5
* Freeze embedding model + add trainable tanh layers
* Uses distance data
* Model outputs embeddings with a euclidean distance equal to the relative harmonicity of the two input chords
* Problems with overrpresentation of noisy chords in the training data?
* Needs an evaluation notebook
* Think about clipping the input and/or output


## WGAN


# Comments on sampling, representation and bias

Acceptable chords are discontinuous when unweighted

Solution 1: weight the chords
Solution 2: use frequencies instead of note indices