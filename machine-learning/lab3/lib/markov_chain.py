from collections import defaultdict
import numpy as np


class MarkovChain:
    def __init__(self, seed, seqlen=5):
        self.random = np.random.default_rng(seed)
        self.seqlen = seqlen

    def train(self, corpus):
        self.graph = {}
        self.words = defaultdict(float)

        words_count = 0

        for text in corpus:
            for i in range(0, len(text) - self.seqlen):
                v = text[i: i + self.seqlen]
                if v not in self.graph:
                    self.graph[v] = defaultdict(float)
                self.graph[v][text[i + self.seqlen]] += 1

        for seq in self.graph.keys():
            cnt = sum(self.graph[seq].values())
            for char in self.graph[seq].keys():
                self.graph[seq][char] /= cnt

        for text in corpus:
            for word in text.split():
                if word[-1] == '.':
                    word = word[:-1]
                self.words[word] += 1
                words_count += 1

        for word in self.words.keys():
            self.words[word] /= words_count

    def _predict(self, seq):
        if seq not in self.graph:
            return None
        p = list(self.graph[seq].values())
        idx = self.random.choice(len(p), p=p)
        return list(self.graph[seq].keys())[idx]

    def inference(self, prefix="", max_length=60):
        assert self.seqlen <= max_length

        s = prefix[:]
        stuck = False
        max_length_reached = True

        while len(s) < self.seqlen:
            p = list(self.words.values())
            idx = self.random.choice(len(p), p=p)
            s += list(self.words.keys())[idx] + " "

        while len(s) < max_length:
            suffix = s[-self.seqlen:]
            pred = self._predict(suffix)

            if pred is not None:
                s += pred
            else:
                stuck = s[-1] != '.'
                break

        if len(s) >= max_length:
            max_length_reached = True
            if s[-1] == ' ':
                s = s.rstrip()
            elif s[-1] != '.':
                # strip truncated words
                s = s[:s.rfind(' ')]

        return s, max_length_reached, stuck
