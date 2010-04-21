# -----------------------------------------------------------------------------
# calc.py
#
# A simple calculator with variables -- all in one file.
# -----------------------------------------------------------------------------

from eqtree import *

#| '-'? ['0'-'9']* '.' ['0'-'9']+ (['e' 'E'] '-'? ['0'-'9']+)? as lxm { FLOAT(float_of_string lxm) }
#| '-'? ['0'-'9']+ ['e' 'E'] '-'? ['0'-'9']+ as lxm { FLOAT(float_of_string lxm) }
#| eof            { EOF }

tokens = ('INT', 'FLOAT', 'STRING',
          'ASSIGN', 'LOCAL', 'RANGE',
          'LPAREN', 'RPAREN', 'LBRACKET','RBRACKET',
          'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
          'COLON','COMMA', 'SEMICOLON' )

# Tokens
t_STRING = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_ASSIGN = r'<-'
t_LOCAL = r'%local'
t_RANGE = r'%range'
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COMMA = r','
t_COLON = r':'
t_SEMICOLON = r';'

def t_FLOAT(t):
    r'\d*[.]\d+'
    try:
        t.value = float(t.value)
    except ValueError:
        print "Floating point number is too large", t.value
        t.value = 0
    return t

def t_INT(t):
    r'[-]?\d+'
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
lex.lex()

def p_parse_localeqn(t):
    """parse_localeqn : local_and_range_defs assignments"""
    lt = len(t)
    if lt == 1:
        t[0] = LocalEqnNode()
    else:
        t[0] = LocalEqnNode(t[1], t[2])

def p_local_and_range_defs(t):
    """local_and_range_defs :
                            | local_and_range_defs LOCAL num_tensors SEMICOLON
                            | local_and_range_defs RANGE ix_ranges SEMICOLON"""
    if len(t) == 1:
        t[0] = LocalAndRangeDefsNode()
    elif t[2] == '%local':
        t[0] = t[1].add_local(t[3])
    else:
        assert t[2] == '%range'
        t[0] = t[1].add_range(t[3])

# Tensors with number-only indices
def p_num_tensors(t):
    """num_tensors :
                   | num_tensor
                   | num_tensors COMMA num_tensor"""
    lt = len(t)
    if lt == 1:
        t[0] = NumTensorsNode()
    elif lt == 2:
        t[0] = NumTensorsNode().add(t[1])
    else:
        assert lt == 4
        t[0] = t[1].add(t[3])

def p_num_tensor(t):
    """num_tensor : STRING
                  | STRING LPAREN int_indices RPAREN"""
    lt = len(t)
    if lt == 2:
        t[0] = NumTensorNode(name=t[1])
    else:
        assert lt == 5
        t[0] = NumTensorNode(name=t[1], indices=t[3])

def p_int_indices(t):
    """int_indices :
            | INT
            | int_indices COMMA INT"""
    lt = len(t)
    if lt == 1:
        t[0] = IntsNode()
    elif lt == 2:
        t[0] = IntsNode().add(t[1])
    else:
        assert lt == 4
        t[0] = t[1].add(t[3])

def p_ix_ranges(t):
    """ix_ranges :
                 | STRING COLON INT
                 | ix_ranges COMMA STRING COLON INT"""
    lt = len(t)
    if lt == 1:
        t[0] = IxRangeNode()
    elif lt == 4:
        t[0] = IxRangeNode().add((t[1], t[3]))
    else:
        assert lt == 6
        t[0] = t[1].add((t[3], t[5]))

def p_assignments(t):
    """assignments :
                   | assignments lvalue ASSIGN tensor_sum SEMICOLON"""
    lt = len(t)
    if lt == 1:
        t[0] = AssignmentNode()
    else:
        assert lt == 6
        t[0] = t[1].add((t[2], t[4]))

def p_lvalue(t):
    """lvalue : tensor"""
    t[0] = t[1]

def p_tensor(t):
    """tensor : STRING
              | STRING LPAREN indices RPAREN"""
    lt = len(t)
    if lt == 2:
        t[0] = TensorNode(name=t[1])
    else:
        assert lt == 5
        t[0] = TensorNode(name=t[1], arg=t[3])

def p_indices(t):
    """indices :
               | index
               | indices COMMA index"""
    lt = len(t)
    if lt == 1:
        t[0] = IndicesNode()
    elif lt == 2:
        t[0] = IndicesNode().add(t[1])
    else:
        assert lt == 4
        t[0] = t[1].add(t[3])

def p_index(t):
    """index : INT
             | STRING"""
    if type(t[1]) == int:
        t[0] = NumIndexNode(t[1])
    else:
        t[0] = VarIndexNode(t[1])

def p_tensor_sum(t):
    """tensor_sum : tensor_product
                  | tensor_sum sign tensor_product"""
    lt = len(t)
    if lt == 2:
        t[0] = TensorSumNode().add(t[1])
    else:
        assert lt == 4
        t[0] = t[1].add((t[2], t[3]))

def p_tensor_product(t):
    """tensor_product : signed_tensor_atom
                      | tensor_product TIMES signed_tensor_atom
                      | tensor_product DIVIDE signed_tensor_atom"""
    lt = len(t)
    if lt == 2:
        t[0] = TensorProductNode().add(t[1])
    else:
        assert lt == 4
        t[0] = t[1].add((t[2], t[3]))

def p_sign(t):
    """sign : PLUS
            | MINUS"""
    if t[1] == '+':
        t[0] = 1
    else:
        assert t[0] == '-'
        t[0] = -1

def p_signed_tensor_atom(t):
    """signed_tensor_atom : tensor_atom
                          | sign signed_tensor_atom"""
    lt = len(t)
    if lt == 2:
        t[0] = SignedTensorAtomNode(t[1])
    else:
        assert lt == 3
        t[0] = t[2].sign(t[1])

# XXX TODO: change function and tensor argument parentheses: tensors use []
# indexing, functions use ()!
def p_tensor_atom(t):
    """tensor_atom : INT
                   | FLOAT
                   | LPAREN tensor_sum RPAREN
                   | STRING LPAREN indices RPAREN
                   | STRING LBRACKET tensor_sum RBRACKET"""
    lt = len(t)
    if lt == 2:
        assert isinstance(t[1], (int, float))
        t[0] = FloatNode(t[1].value)
    elif lt == 4:
        t[0] = ParenthesisNode(t[2])
    elif lt == 5:
        if isinstance(t[3], IndicesNode):
            t[0] = TensorNode(name=t[1], arg=t[3])
        else:
            t[0] = FunctionNode(name=t[1], arg=t[3])

def p_error(t):
    try:
        print "Syntax error at '%s'" % t.value
    except:
        print "Syntax error at '%s'" % t

import ply.yacc as yacc
yacc.yacc()

s = """%range j:3;
H_total_MAT(j) <- H_ext(j) + H_exch_MAT(j) + H_anis_MAT(j);"""

print yacc.parse(s)

