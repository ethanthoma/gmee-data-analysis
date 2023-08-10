from datasets import load_dataset
import spacy

# Load in content words
dataset = load_dataset("liujqian/commonsenseqa_with_content_words")

# Load content words
list_of_content_words = dataset['train'].to_pandas().loc[:, ['id', 'question_content_words']]

# Load the English language model for spacy
nlp = spacy.load('en_core_web_trf')

# Goes rowwise to lemmatize the content words
def lemmatize_rows(row):
    joined_content_words = nlp(' '.join([word for word in row['question_content_words']]))

    # Lemmatize content words
    with nlp.select_pipes(enable=['tok2vec', 'tagger', 'attribute_ruler', 'lemmatizer']):
        lemmatized_content_words = [content_word.lemma_ for content_word in joined_content_words]

    return lemmatized_content_words

# Save lemmatized content words to CSV
list_of_content_words.apply(lemmatize_rows, axis=1).to_csv('lemmatized_content_words.csv', index=False)

