# PERFORM TSNE EMBEDDING

from sklearn.manifold import TSNE

def tsne_embedding(X, n_components=2, random_state=123):
    X_embedded = TSNE(n_components=n_components, random_state=random_state).fit_transform(X)
    return X_embedded
