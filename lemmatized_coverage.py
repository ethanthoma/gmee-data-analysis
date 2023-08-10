import pandas as pd
import spacy
import json

# Load context words from CSV
context_words = pd.read_csv('lemmatized_content_words.csv')

# Load in sentences
root = '../generative-model-extrinsic-eval/experiments/generated_sentences/'
type = '-train-WITH-choicewords-noquestionwordlimit.json'
models = [
    't0_3b'
    ]

# Load the English language model for spacy
nlp = spacy.load('en_core_web_trf')

for model in models:
    file_path = root + model + type

    # Load and parse the JSON file
    with open(file_path, 'r') as json_file:
        data = json.load(json_file)
        
    # Iterate through the elements and lemmatize sentences
    for element in data:
        for index, sentence in enumerate(data[element]['sentences']):
            # Lemmatize the sentence
            with nlp.select_pipes(enable=['tok2vec', 'tagger', 'attribute_ruler', 'lemmatizer']):
                doc = nlp(sentence)
            lemmatized_sentence = ' '.join([token.lemma_ for token in sentence])

            data[element]['sentences'][index] = lemmatized_sentence

    # Save lemmatized sentences
    output_file = 'lemmatized' + model + '.json'
    with open(output_file, 'w') as json_output:
        json.dump(data, json_output, indent=4)

    print('Finished lemmatizing ' + model)

