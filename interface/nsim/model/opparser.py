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

"""Provides a function to parse an equation of motion and return a tree
representation of the equation which can be used to simplify the equation,
examine the quantities involved in the equation and finally rewrite it as
text."""

from optree import *

__all__ = ['lexer', 'parser', 'parse']

tokens = ('INT')

# Tokens
t_STRING = r'[a-zA-Z_][a-zA-Z0-9_]*'

def t_FLOAT(t):
    r'\d*[.]\d+([eE]-?\d+)?|\d+[eE]-?\d+'
    try:
        t.value = float(t.value)
    except ValueError:
        print "Floating point number is too large", t.value
        t.value = 0
    return t

def t_INT(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print "Integer number is too large", t.value
        t.value = 0
    return t

# Ignored characters
t_ignore = " \t\r\n"

def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)

# Build the lexer
import ply.lex as lex
lexer = lex.lex(lextab='diffop_lextab')

def p_parse_localeqn(t):
    """parse_localeqn : local_and_range_defs assignments"""
    lt = len(t)
    if lt == 1:
        t[0] = LocalEqnNode()
    else:
        t[0] = LocalEqnNode([t[1], t[2]])

def p_error(t):
    if hasattr(t, 'value'):
        info = " (token='%s', value='%s')" % (str(t), t.value)
    else:
        info = " (token='%s')" % str(t)
    raise ValueError("Syntax error when parsing equation%s." % info)

import ply.yacc as yacc
parser = yacc.yacc(tabmodule='diffop_parsetab',
                   debugfile='diffop_parser.out')

def parse(s):
    return parser.parse(s, lexer=lexer)
