# -*- coding: utf-8 -*-

# glitcherature.py - Functions to glitch and generate text.
# Copyright (C) 2012  Rob Myers <rob@robmyers.org>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or 
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import random
import re
import string
from nltk.corpus import wordnet

################################################################################
# Utilities
################################################################################

def one_in(n):
    """ Return True 1/n times, False the rest of the time.
        n can be int or floate"""
    return random.random() * n <= 1

def maybe(fun, default, n):
    """ Return the results of calling fun() 1/n times, otherwise default"""
    result = default
    if one_in(n):
        result = fun()
    return result

################################################################################
# Generating text
################################################################################

def repeat_run(item, length):
    """Return a string repeating item for the given length"""
    return ''.join(item * length)

def random_run(items, length):
    """Return a string of length randomly chosen items"""
    return ''.join([random.choice(items) for i in xrange(length)])

################################################################################
# General: apply to words, sentences, paragraphs
################################################################################

def wrap(item, before, after):
    """Wrap item in before and after"""
    return "%s%s%s" % (before, item, after)

################################################################################
# Words
# Operate on (and generate) strings of characters without whitespace
################################################################################

def split_word(word, probability=1):
    # Find subword using nltk or something, e.g.: antidote -> anti, dote
    pass

def synset(word):
    """Try to get a Wordnet synset for word, trying noun then verb"""
    synset = None
    try:
        synset = wordnet.synset("%s.n.01" % word)
    except:
        try:
            result = wordnet.synset("%s.v.01" % word)
        except:
            pass
    return synset

def synset_name(synset):
    """Get the English word for the synset"""
    name = re.match(r"[^.]+", synset.name).group(0)
    name = re.sub("_", " ", name)
    return name

def hypernym(word):
    """Choose a random hypernym for the word"""
    result = word
    synset = synset(word)
    try:
        synonym = random.choice(synset.hypernyms())
        result = synset_name(synonym)
    except:
        pass
    return result

def hyponym(word):
    """Choose a random hyponym for the word"""
    result = word
    synset = wordnet_synset(word)
    try:
        hyponym = random.choice(synset.hyponyms)
        result = synset_name(hyponym)
    except:
        pass
    return result

def example(word):
    """Get a piece of text that is an example of using the word, or None"""
    result = None
    synset = wordnet_synset(word)
    if synset:
        result = random.choice(synset.examples)
    return result

################################################################################
# Sentences
# Operate on strings of whitespace-separated words with punctuation.
################################################################################

def replace_spaces(text, replacement, n=1):
    """Replace spaces with replacement 1/n times"""
    return re.sub(r" ",
                  lambda match: replacement if one_in(n) else match.group(0),
                  text)

def all_lower(text):
    """Make everything lower-case"""
    return text.lower()

def all_upper(text):
    """Make everything upper-case"""
    return text.upper()

def random_upper_letters(text, n=2):
    """Randomly set letters to upper-case"""
    return ''.join([letter.upper() if one_in(n) else letter for letter in text])

def random_upper_words(text, n=2):
    """Randomly set words to all upper-case"""
    return re.sub(r"\w+",
                  lambda word: word.group(0).upper() if one_in(n) \
                      else word.group(0),
                  text)

def sub_text(text, subs, n=1):
    """Substitute keys from subs for values from subs in text 1/n of the time"""
    return re.sub(r"\w+",
                  lambda word: subs.get(word, word) if one_in(n) else word,
                  text)

leet = {'a': ('@', '/\\', '/-\\', 'λ'),
        'b': ('8', '|3', '6', ']3'),
        'c': ('(', '{', '<', '©'),
        'd': ('|)', '|]', '])', '∂'),
        'e': ('3', '£', '€', '='),
        'f': ('ʃ', '|=', ']=', ')='),
        'g': ('6', '9', '&', 'C-'),
        'h': ('|-|', '#', '}{', ')-('),
        'i': ('!', '1', '|', '`|'),
        'j': ('_|', ']', '_/', '_)'),
        'k': ('|<', '|{', '|X', ']<'),
        'l': ('1', '7', '|_', '|'),
        'm': ('44', '/\\/\\', '|\\/|', '|v|'),
        'n': ('|\\|', '/\\/', 'И', '~'),
        'o': ('()', '[]', '0', 'Ø'),
        'p': ('|*', '?', '9', '|"'),
        'q': ('0_', '0,', '(),', '¶'),
        'r': ('®', 'Я', 'I^', '|2'),
        's': ('$', '5', '§', '_\-'),
        't': ('7', '+', '†', '|['),
        'u': ('\\/', '|_|', 'μ', '/_/'),
        'v': ('\\\\//', '\\/', '√', 'V'),
        'w': ('vv', '\\/\\/', 'Ш', '\\^/'),
        'x': ('%', '><', '*', 'Ж'),
        'y': ('`/', "'/", '`(', '-/'),
        'z': ('2', '3', '`/_', '%')}

leet_kinds_count = len(leet['a'])

# Make sure we have the same number of options for every letter

assert(all([len(leets) == leet_kinds_count for leets in leet.itervalues()]))

# Make sure no mappings appear twice in the same column

assert(all([len(set(row[i] for row in leet.itervalues())) == len(leet)
            for i in xrange(0, len(leet['a']))]))

def sub_1337_vowels(text, n=1):
    """Replace vowels in the text with 1337 1/n of the time"""
    trans = string.maketrans("aeioul", "@3|0\"1") 
    return ''.join([c.translate(trans) if one_in(n) else c for c in text])

def sub_1337_char(char, kind, n=1):
    """Substitute the char for 1337 kind 1/n of the time"""
    result = char
    if char in leet and one_in(n):
        result = leet[char][kind]
    return result

def sub_1337(text, kind, n=1):
    """Replace letters in the text with the given kind (1..4) of 1337 1/n"""
    return ''.join([sub_1337_char(c, kind, n) for c in text])

txt = {'are': 'r',
       'be': 'b',
       'hate': 'h8',
       'late': 'l8',
       'see': 'c',
       'you': 'u',}

def sub_txt(text, n=1):
    """Substitute words in text for their txt equivalents 1/n of the time"""
    for key in txt.iterkeys():
        text = re.sub("\\b%s" % key,
                  lambda x: txt[key] if one_in(n) else key,
                  text,
                  flags=re.IGNORECASE)
    return text

def sub_space_runs(text, sub, min_len, max_len):
    """Substitute spaces with runs of sub character, or choices from sub list"""
    if type(sub) != list:
        sub = [sub]
    return re.sub(r"\s+",
                  lambda spaces: repeat_run(random.choice(sub),
                                            random.randint(min_len, max_len)),
                  text)

def drop_definite(text, n=1):
    """Drop definite articles 1/n of the time"""
    return re.sub(r"\bthe\b\s*",
                  lambda the: "" if one_in(n) else the.group(0),
                  text,
                  flags=re.IGNORECASE)

def replace_and(text, repl, n=1):
    """Replace ' and ' with replacement 1/n or the time"""
    return re.sub(r"\s*\band\b\s*",
                  lambda et: repl if one_in(n) else et.group(0),
                  text,
                  flags=re.IGNORECASE)

def wrap_words(text, before, after, n=1):
    """Wrap 1/n words in text with before and after"""
    return re.sub(r"\w+",
                  lambda word: wrap(word.group(0), before, after) if one_in(n) \
                      else word.group(0),
                  text)

################################################################################
# Tests
################################################################################

t1 = "The quick brown fox jumped over the lazy dog."

t2 = "Rocks and pebbles wrapped in brown paper and tied with string, then returned to nature's bosom."

t3 = "No no no there is no way that can be no..."

t4 = "notational sublimity undermining expositional transitivity"

t5 = "I see you hate being late"

ts = [t1, t2, t3, t4, t5]

def test():
    for t in ts:
        print repeat_run('-', 80)
        print random_run(list('.#+-:@'), random.randint(10, 50))
        print wrap(t, "++++", "----")
        print replace_spaces(t, ".")
        print all_lower(t)
        print all_upper(t)
        print random_upper_letters(t)
        print random_upper_words(t)
        print sub_1337_vowels(t)
        print sub_1337(t, 2)
        print sub_txt(t)
        print sub_space_runs(t, ['.', '_'], 1, 5)
        print drop_definite(t)
        print replace_and(t, '+')
        print wrap_words(t, '[', ']', 1.4)

if __name__ == "__main__":
    test()

################################################################################
# PROJECT PLAN
################################################################################

#### BUGS

# FIXME: maintain case of letter and word substitutions

#### FEATURES

# Sources: file, url, url in url, rss feed
# BY-SA: tvtropes, Wikipedia (warn user, add license)
# PD: gutenberg.org

# Markov of order n

# Most frequent words from...
# Random text (word, words, sentence, para) from...

# tf-idf

# Generative grammars

# Pronoun swapping

# Product placement

# cut-ups

# Insert . or _ etc. randomly into words but not spaces
#   So 2 complimentary functions, one to replace spaces, one letters
# i.nternet intern_et, etc.

# Line noise

# Glitch across text

# html - bold, italic, size, face (also latex, make general)
