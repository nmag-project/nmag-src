#!/usr/bin/python
# Nmag micromagnetic simulator
# Copyright (C) 2010 University of Southampton
# Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
#
# WEB:     http://nmag.soton.ac.uk
# CONTACT: nmag@soton.ac.uk
#
# AUTHOR(S) OF THIS FILE: Matteo Franchin
# LICENSE: GNU General Public License 2.0
#          (see <http://www.gnu.org/licenses/>)

import os

def comment(text):
    return "\n".join(["# %s" % line for line in text.splitlines()])

class Script(object):
    def __init__(self):
        self.text = ""

    def writeln(self, line):
        self.text += line + "\n"

    def save(self, filename=""):
        fn = filename
        i = 0
        while True:
            if not os.path.exists(fn):
                f = open(fn, "w")
                f.write(self.text)
                f.close()
                return fn

            else:
                fn = "%s.%d" % (filename, i)
                i += 1

class ChoiceList(object):
    def __init__(self, desc):
        self.desc = desc
        self.choices = []

    def __str__(self):
        return "\n".join([str(c) for c in self.choices])

    def wait(self, text):
        print "="*40
        print text
        print "="*40
        print "Press [ENTER] to continue...",
        raw_input()

    def add_choice(self, choice):
        """Add a new choice to the Plan."""
        self.choices.append(choice)
        return choice

    def ask_user(self, script):
        self.wait(self.desc)

        for c in self.choices:
            c.ask_user(script)

    def save(self):
        pass

class Choice(object):
    def __init__(self, desc):
        self.desc = desc
        self.chosen = None
        self.should_ask_function = None
        self.alternatives = []

    def __str__(self):
        if self.chosen != None:
            return "Q: %s\n   %s" \
                   % (self.desc, str(self.alternatives[self.chosen]))
        else:
            return ""

    def when_should_ask(self, fn):
        self.should_ask_function = fn

    def add_alternative(self, alternative):
        self.alternatives.append(alternative)
        return alternative

    def ask_user(self, script):
        if self.should_ask_function != None:
            if not self.should_ask_function():
                return

        print "-"*40
        print self.desc
        for i, a in enumerate(self.alternatives):
            print "  (%d) %s" % (i, a.desc)

        while True:
            print "Please, enter a number between %d and %s:" \
                  % (0, len(self.alternatives)-1),

            try:
                choice = int(raw_input())
            except ValueError:
                pass
            else:
                if choice >= 0 and choice < len(self.alternatives):
                    self.chosen = choice
                    self.alternatives[choice].ask_user(script)
                    return

class OpenQuestion(object):
    def __init__(self, desc):
        self.desc = desc
        self.answer = None
        self.should_ask_function = None
        self._answer_is_ok = None

    def when_should_ask(self, fn):
        self.should_ask_function = fn

    def answer_checker(self, fn):
        self._answer_is_ok = fn

    def ask_user(self, script):
        if self.should_ask_function != None:
            if not self.should_ask_function():
                return

        print "-"*40
        print "QUESTION: %s" % self.desc

        while True:
            print "ANSWER: ",
            self.answer = ans = raw_input()
            if self._answer_is_ok == None:
                return
            else:
                problem = self._answer_is_ok(ans)
                if problem == None:
                    return
                print "BAD ANSWER: %s" % problem
                print "Please enter another answer."


class Alternative(object):
    def __init__(self, desc):
        self.desc = desc

    def __str__(self):
        return self.desc

    def ask_user(self, script):
        pass


