# PERFORMS DBSCAN CLUSTERING
from sklearn.cluster import DBSCAN

def cluster_dbscan(X, min_samples=5):
    db = DBSCAN(min_samples=min_samples).fit(X)
    return db.labels_
    

