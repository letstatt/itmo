import re


def load(path, lowercase=True, no_commas=False, fullstop=True):
    """Accept path to a plain-text book, returns a list of sentences (corpus)"""

    # read entire text
    with open(path, "r") as f:
        text = f.read()

    # replace sentence-ending chars by '\n'
    ending = ["\\?+", "\\!+", "\\.+", "(\\?|\\!)+", "\\:", "\\;"]
    text = re.sub("|".join(ending), "\n", text)

    # place quoted sencences like '"What have I done" thought he' in a separated line
    enclosed = [('"', '"'), ("'", "'"), ("\\(", "\\)")]
    for l, r in enclosed:
        # '\s' is needed to allow "Pushkin's" or "o'clock"
        text = re.sub("(\\s){}(.+?){}".format(l, r), "\\1\\2\n", text, flags=re.DOTALL)

    # remove parenthesis and double-quote completely
    text = re.sub('"', "", text)
    text = re.sub('\\(', "", text)
    text = re.sub('\\)', "", text)

    # remove dashes
    text = re.sub("(\\s)—\\s", "\\1", text) # ord == 8212
    text = re.sub("—", "", text)            # ord == 8212
    text = re.sub("(\\s)-\\s", "\\1", text) # ord == 45
    text = re.sub("-", "", text)            # ord == 45

    # remove numbers
    text = re.sub("\\d+", "", text)

    # remove asterisks
    text = re.sub("\\*", "", text)

    # remove titles
    text = re.sub("CHAPTER .+\n", "\n", text)
    text = re.sub("BOOK .+\n", "\n", text)

    # strip lines
    text = map(lambda s: s.strip(), text.splitlines())

    # filter empty lines
    text = filter(lambda s: len(s) > 0, text)

    # strip trailing commas
    text = map(lambda s: s[:-1] if s[-1] == ',' else s, text)

    # filter empty lines again
    text = filter(lambda s: len(s) > 0, text)

    # strip heading commas
    text = map(lambda s: s[1:] if s[0] == ',' else s, text)

    # remove commas
    if no_commas:
        text = map(lambda s: re.sub(",", "", s), text)

    # strip lines again
    text = map(lambda s: s.strip(), text)

    # filter empty lines again!
    text = filter(lambda s: len(s) > 0, text)

    # make it lower
    if lowercase:
        text = map(lambda s: s.lower(), text)

    # remove duplicates
    set_ = set()
    text_ = []
    for t in text:
        if t not in set_:
            set_.add(t)
            text_.append(t)
    text = text_

    # add fullstop
    if fullstop:
        text = map(lambda s: s + ".", text)

    return list(text)


def alphabet_map(corpus):
    text = " ".join(corpus)
    chars = sorted(set(c for c in text))

    char2index = dict((c, i) for i, c in enumerate(chars))
    index2char = dict((i, c) for i, c in enumerate(chars))

    return char2index, index2char
