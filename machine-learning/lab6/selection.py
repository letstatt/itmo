import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from tqdm import tqdm

from sklearn.model_selection import KFold, StratifiedKFold, cross_val_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import mutual_info_classif, RFE, SelectFpr
from sklearn.calibration import LinearSVC
from sklearn.neighbors import KNeighborsClassifier


MAX_FEATURES_COUNT = 100

def plot_top_feature_scores(feature_importances_df, top=30):
    df = feature_importances_df.head(top)
    df = df.sort_values(by='score')
    df.plot(kind='barh', grid=True, figsize=(6, 10), legend=False)


def get_top_words(int2word, feature_importances_df, top=30):
    return [int2word[int(i[4:])] for i in feature_importances_df.head(top).index]


def get_top_features_df(feature_importances, top=MAX_FEATURES_COUNT):
    df = pd.DataFrame(data={'score': feature_importances}, index=["col_{}".format(i) for i in range(len(feature_importances))])
    df.sort_values(by='score', inplace=True, ascending=False)
    return df.head(top)


def embedded_method(data, target, wrappered=False):
    rfc = RandomForestClassifier(n_jobs=-1, class_weight='balanced', random_state=1337)
    skf = StratifiedKFold(n_splits=5)
    scores = cross_val_score(estimator=rfc, X=data, y=target, cv=skf, scoring='roc_auc', n_jobs=-1)
    if wrappered:
        return scores.mean()

    rfc.fit(data, target)
    return rfc.feature_importances_


def filter_method(data, target):
    return mutual_info_classif(data, target, random_state=1337)


def wrapper_method(data, target, n_neighbors=10, top=MAX_FEATURES_COUNT, method=lambda d, t: embedded_method(d, t, wrappered=True)):
    assert n_neighbors > 0

    rng = np.random.default_rng(seed=1337)
    all_indices = list(range(data.shape[1]))
    chosen_indices = []
    k = min(data.shape[1], top)

    for i in tqdm(list(range(k))):
        best_score = -np.inf
        best_idx = None

        # randomized
        for _ in range(min(n_neighbors, len(all_indices))):
            idx = rng.choice(all_indices)
            score = method(data[chosen_indices + [idx]], target)
            if score > best_score:
                best_score = score
                best_idx = idx

        assert best_idx is not None
        all_indices.remove(best_idx)
        chosen_indices.append(best_idx)

    n2 = k * (k + 1) / 2
    importances = [1 / n2 if i in chosen_indices else 0 for i in sorted(chosen_indices + all_indices)]
    return importances


def wrapper_method_sklearn(data, target, top=MAX_FEATURES_COUNT):
    rfc = RandomForestClassifier(n_jobs=-1, class_weight='balanced', random_state=1337)
    rfe = RFE(rfc, n_features_to_select=top, step=200)
    rfe.fit(data, target)
    return [1 / i for i in rfe.ranking_]


def embedded_method_sklearn(data, target):
    lsvc = LinearSVC(C=0.06, penalty="l1", dual=False, random_state=1337)
    lsvc.fit(data, target)
    return lsvc.coef_.reshape(-1)


def filter_method_sklearn(data, target):
    selector = SelectFpr()
    selector.fit(data, target)
    return selector.scores_


def do_classification(data, target):
    return [
        cross_val_score(estimator=classificator, X=data, y=target, cv=KFold(n_splits=5), scoring='roc_auc', n_jobs=-1).mean()
        for classificator in [
            RandomForestClassifier(n_jobs=-1, class_weight='balanced', random_state=1337),
            LinearSVC(dual=False, random_state=1337),
            KNeighborsClassifier(n_jobs=-1)
        ]
    ]

def do_cmp_classification(data, target, *features_masks):
    return [
        do_classification(X, target)
        for X in (
            data[[int(i[4:]) for i in features_mask.index]]
            for features_mask in features_masks
        )
    ]
