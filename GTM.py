from ugtm import eGTM,eGTC
import numpy as np
import altair as alt
import pandas as pd
from sklearn import datasets
from sklearn import metrics
from sklearn import model_selection
from sklearn import manifold
from sklearn.utils import check_random_state

dataset_k14 = pd.read_csv('파일/위치/주소/파일명.csv')

man = manifold.TSNE(n_components=2, init='pca', random_state=0)
man = manifold.MDS(max_iter=100, n_init=1, random_state=0)
man = manifold.LocallyLinearEmbedding(n_neighbors=20, n_components=2,
                                      eigen_solver='auto',
                                      method="standard",
                                      random_state=0)

# Construct GTM
gtm_k14 = eGTM(k=14, model="responsibilities").fit(dataset_k14)

gtm_means_k14 = eGTM(k=14, model="means").fit_transform(dataset_k14)
gtm_modes_k14 = eGTM(k=14, model="modes").fit_transform(dataset_k14)

dgtm_modes_k14 = pd.DataFrame(gtm_modes_k14, columns=["x1", "x2"])
#dgtm_modes["label"] = y

gtm_modes_k14 = alt.Chart(dgtm_modes_k14).mark_circle().encode(
    x='x1',
    y='x2',
#    color=alt.Color('label:N',
#                    scale=alt.Scale(scheme='viridis')),
    size=alt.value(50),
    tooltip=['x1','x2','label:N']
).properties(title = "GTM (modes_k14)", width = 200, height = 200)

dgtm_means_k14 = pd.DataFrame(gtm_means_k14, columns=["x1", "x2"])
#dgtm_means["label"] = y

gtm_means_k14 = alt.Chart(dgtm_means_k14).mark_circle().encode(
    x='x1',
    y='x2',
#    color=alt.Color('label:N',
#                    scale=alt.Scale(scheme='viridis')),
    size=alt.value(50),
    tooltip=['x1','x2','label:N']
).properties(title = "GTM (means_k14)", width = 200, height = 200)



gtm14 = gtm_means_k14 | gtm_modes_k14

alt.vconcat(gtm14)