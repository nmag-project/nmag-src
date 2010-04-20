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
          'COLON','COMMA', 'SEMICOLON', 'EOF' )

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
    """parse_localeqn :
                      | local_and_range_defs"""
    lt = len(t)
    if lt == 1:
        t[0] = LocalEqnNode()
    else:
        t[0] = LocalEqnNode(t[1], None) #t[2])

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
                  | STRING LPAREN ints RPAREN"""
    lt = len(t)
    if lt == 2:
        t[0] = NumTensorNode(name=t[1])
    else:
        assert lt == 5
        t[0] = NumTensorNode(name=t[1], indices=t[3])

def p_ints(t):
    """ints :
            | INT
            | ints COMMA INT"""
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
'''
def p_assignments(t):
    """assignments:
                  | assignments lvalue ASSIGN tensor_term SEMICOLON"""
    lt = len(t)
    if lt == 1:
        t[0] = AssignmentNode()
    else:
        assert lt == 6
        t[0] = t[1].add((t[2], t[4]))

def p_lvalue(t):
    """lvalue: var_tensor  {$1}
             | STRING      {($1,[||])}"""
    if type(t[1]) == str:
        return t[1]
    elif lt == 4:
        return (t[1], [])

def p_var_tensor(t):
    """var_tensor: STRING
                 | STRING LPAREN indices_or_vars RPAREN"""
    lt = len(t)
    if lt == 1:
        return (t[1], [])
    else:
        assert lt == 4
        return (t[1], t[3])

def p_indices_or_vars(t):
    """indices_or_vars:
       | {[]}
       | index_or_var {[$1]}
       | index_or_var COMMA indices_or_vars {$1::$3}"""
    lt = len(t)
    if lt == 1:
        return []
    elif lt == 4:
        return [(t[1], t[3])]
    else:
        assert lt == 6
        return [(t[1], t[3])] + t[5]

def p_index_or_var(t):
    """index_or_var:
       | INT  {IX_int $1}
       | STRING  {IX_var $1}"""
    lt = len(t)
    if lt == 1:
        return []
    elif lt == 4:
        return [(t[1], t[3])]
    else:
        assert lt == 6
        return [(t[1], t[3])] + t[5]

def p_tensor_term(t):
    """tensor_term:
       | LPAREN tensor_term RPAREN {$2}
       | tensor_sum {$1}"""

def p_tensor_sum(t):
    """tensor_sum:
       | tensor_product {Tensor_sum [$1]}
       | tensor_product sign_tensor_sum {let Tensor_sum x = $2 in Tensor_sum ($1::x)}
       | sign_tensor_sum {$1}"""

def p_sign_tensor_sum(t):
    """sign_tensor_sum:
       | {Tensor_sum []}
       | SIGN tensor_product sign_tensor_sum {if $1 = 1.0
                                            then
                                              let Tensor_sum s = $3 in Tensor_sum ($2::s)
                                            else
                                              Tensor_product [(Tensor_float (-1.0));
                                                              let Tensor_sum s = $3 in Tensor_sum ($2::s)]
                                            }"""

def p_tensor_product(t):
    """tensor_product:
       | tensor_factor  {Tensor_product [$1]}
       | tensor_factor STAR tensor_product {let Tensor_product x = $3 in Tensor_product ($1::x)}"""

# XXX TODO: change function and tensor argument parentheses: tensors use []
# indexing, functions use ()!
def p_tensor_factor(t):
    """tensor_factor:
       | FLOAT          {Tensor_float $1}
       | var_tensor     {Tensor_varindexed $1}
       | STRING LBRACKET tensor_term RBRACKET {Tensor_func ($1,$3)}
       | LPAREN tensor_term RPAREN {$2}"""
'''

"""
def x():
  # Parsing rules

  precedence = ( ('left','PLUS','MINUS'), ('left','TIMES','DIVIDE'), ('right','UMINUS'), )

  # dictionary of names
  names = { }

  def p_statement_assign(t):
      'statement : NAME EQUALS expression'
      names[t[1]] = t[3]

  def p_statement_expr(t):
      'statement : expression'
      print t[1]

  def p_expression_binop(t):
      '''expression : expression PLUS expression
                    | expression MINUS expression
                    | expression TIMES expression
                    | expression DIVIDE expression'''
      if t[2] == '+' : t[0] = t[1] + t[3]
      elif t[2] == '-': t[0] = t[1] - t[3]
      elif t[2] == '*': t[0] = t[1] * t[3]
      elif t[2] == '/': t[0] = t[1] / t[3]

  def p_expression_uminus(t):
      'expression : MINUS expression %prec UMINUS'
      t[0] = -t[2]

  def p_expression_group(t):
      'expression : LPAREN expression RPAREN'
      t[0] = t[2]

  def p_expression_number(t):
      'expression : NUMBER'
      t[0] = t[1]

  def p_expression_name(t):
      'expression : NAME'
      try:
          t[0] = names[t[1]]
      except LookupError:
          print "Undefined name '%s'" % t[1]
          t[0] = 0

"""

def p_error(t):
    try:
        print "Syntax error at '%s'" % t.value
    except:
        print "Syntax error at '%s'" % t

import ply.yacc as yacc
yacc.yacc()

s = """%range j:3;
H_total_MAT(j) <- H_ext(j) + H_exch_MAT(j) + H_anis_MAT(j);"""

s = """%range j:3, k:2;%local m(3), q(4, 5);"""
print yacc.parse(s)

