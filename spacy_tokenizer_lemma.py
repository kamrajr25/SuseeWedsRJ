# -*- coding: utf-8 -*-
"""
Created on Tue Jul 23 14:19:53 2019

@author: prithvi.kommula
"""

import typing
from typing import Any

from rasa_nlu.components import Component
from rasa_nlu.config import RasaNLUModelConfig
from rasa_nlu.tokenizers import Token, Tokenizer
from rasa_nlu.training_data import Message, TrainingData

if typing.TYPE_CHECKING:
    from spacy.tokens.doc import Doc


class SpacyTokenizer(Tokenizer, Component):
    #print("I am in custom component class SpacyTokenizer")
    name = "tokenizer_spacy_lemma"

    provides = ["tokens"]

    requires = ["tokens"]

    def train(self,
              training_data: TrainingData,
              config: RasaNLUModelConfig,
              **kwargs: Any)-> None:

        for example in training_data.training_examples:
            tokenizedform=self.tokenize(example.get("spacy_doc"))
            #for t in tokenizedform:                
                #print(t.text)
            #print("==============================================")
            example.set("tokens", tokenizedform)

    def process(self, message: Message, **kwargs: Any)-> None:
        inputstring=self.tokenize(message.get("spacy_doc"))
        #for item in inputstring:            
            #print("I am printing the input string here: ", item.text)
        #print("-------------------------------------")
        message.set("tokens", inputstring)

#    def tokenize(self, doc: 'Doc')-> typing.List[Token]:
#        return [Token(t.lemma_, t.idx) for t in doc]
        
    def tokenize(self, doc):
        #print("I am in tokenize")
        return [Token(t.lemma_, t.idx) for t in doc]