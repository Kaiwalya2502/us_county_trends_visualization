import pandas as pd
import numpy as np
data = pd.read_csv(r"C:\Users\Philip\Downloads\tweets_data.csv")
np.random.seed(0)  
data['Sentiment Score'] = np.random.choice([1, 0, -1], size=len(data))

# Mapping sentiment scores to sentiment labels
sentiment_mapping = {1: 'Positive',  0: 'Neutral', -1: 'Negative'}
data['Sentiment'] = data['Sentiment Score'].map(sentiment_mapping)
data.to_csv(r"C:\Users\Philip\Downloads\tweets_data2.csv.gz", compression = 'gzip')