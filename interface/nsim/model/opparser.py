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

from os.path import split, realpath

from optree import *

__all__ = ['lexer', 'parser', 'parse']

tokens = ('ENTRY_DIFFOP', 'ENTRY_RESTRICTION', 'ENTRY_REGION_LOGIC',
          'INT', 'FLOAT', 'DOF_REGION_AND', 'DOF_REGION_OR',
          'DOF_REGION_NOT', 'DOF_REGION_SOME', 'DOF_REGION_ALL', 'DDX_VOL',
          'DDX_BOUNDARY', 'MXDIM', 'GAUGEFIX', 'PERIODIC', 'LPAREN', 'RPAREN',
          'LANGLE', 'RANGLE', 'VBAR', 'LBRACKET', 'RBRACKET', 'COMMA',
          'COLON', 'SEMICOLON', 'EQUALS', 'HASH', 'STAR', 'SIGN', 'STRING')

# Tokens
t_MXDIM           = r'\(L\|\|R\)'
t_LPAREN          = r'\('
t_RPAREN          = r'\)'
t_LANGLE          = r'<'
t_RANGLE          = r'>'
t_VBAR            = r'\|'
t_LBRACKET        = r'\['
t_RBRACKET        = r'\]'
t_COMMA           = r','
t_COLON           = r':'
t_SEMICOLON       = r';'
t_EQUALS          = r'='
t_HASH            = r'\#'
t_STAR            = r'\*'

def t_DDX_VOL(t):
    r'd/dx'
    return t

def t_DDX_BOUNDARY(t):
    r'D/Dx'
    return t

def t_GAUGEFIX(t):
    r'gauge_fix:'
    return t

def t_PERIODIC(t):
    r'periodic:'
    return t

keywords = {
  'and': 'DOF_REGION_AND',
  'or': 'DOF_REGION_OR',
  'not': 'DOF_REGION_NOT',
  'some': 'DOF_REGION_SOME',
  'all': 'DOF_REGION_ALL'
}

def t_STRING(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = keywords.get(t.value, 'STRING')
    return t

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

def t_SIGN(t):
    r'[+-]'
    t.value = 1.0 if t.value == '+' else -1.0
    return t

# Ignored characters
t_ignore = " \t\r\n"

def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)

# Build the lexer
import ply.lex as lex
lexer = lex.lex(lextab='diffop_lextab')

def p_entry(t):
    """entry : ENTRY_DIFFOP parse_ddiffop
             | ENTRY_RESTRICTION short_vector_restriction
             | ENTRY_REGION_LOGIC region_logic"""
    t[0] = t[2]

def p_parse_ddiffop(t):
    """parse_ddiffop : contribs opt_amendment_specs opt_sum_specs"""
    t[0] = OperatorNode(contribs=t[1], amendments=t[2], sums=t[3])

def p_contribs(t):
    """contribs : contrib
                | contribs SIGN contrib"""
    if len(t) == 2:
        t[0] = ContribsNode().add2(t[1], SignSym())
    else:
        t[0] = t[1].add2(t[3], SignSym(t[2]))

def p_contrib(t):
    """contrib : unsigned_contrib
               | SIGN contrib"""
    if len(t) == 2:
        t[0] = t[1]
    else:
        t[0] = ContribNode(children=t[2], data=SignSym(t[1]))

def p_unsigned_contrib(t):
    """unsigned_contrib : bracket
                        | expr bracket
                        | expr STAR bracket"""
    lt = len(t)
    if lt == 2:
        term = ScaledBraKetNode([t[1], ScalarNode(data=1.0)])
    elif lt == 3:
        term = ScaledBraKetNode([t[2], t[1]])
    elif lt == 4:
        term = ScaledBraKetNode([t[3], t[1]])

    t[0] = UContribNode().add(term)

def p_expr(t):
    """expr : unsigned_scalar
            | LPAREN signed_scalar RPAREN
            | LPAREN expr RPAREN"""
    if len(t) == 2:
        t[0] = t[1]
    else:
        t[0] = t[2]

def p_signed_scalar(t):
    """signed_scalar : SIGN unsigned_scalar
                     | SIGN signed_scalar"""
    t[0] = t[2]
    t[0].data[0] *= t[1]

def p_unsigned_scalar(t):
    """unsigned_scalar : INT
                       | FLOAT
                       | STRING"""
    t[0] = ScalarNode(data=t[1])

def p_bracket(t):
    """bracket : LANGLE opt_diff_field VBAR VBAR opt_diff_field RANGLE
               | LANGLE opt_diff_field VBAR field VBAR opt_diff_field RANGLE"""
    lt = len(t)
    if lt == 7:
        t[0] = BraKetNode([t[2], MiddleFieldNode(), t[5]])
    else:
        assert lt == 8
        t[0] = BraKetNode([t[2], MiddleFieldNode(t[4]), t[6]])

def p_opt_diff_field(t):
    """opt_diff_field : field
                      | diff field"""
    if len(t) == 2:
        t[0] = DiffFieldNode(t[1], None)
    else:
        t[0] = DiffFieldNode(t[2], t[1])

def p_diff(t):
    """diff : DDX_VOL      diff_index
            | DDX_BOUNDARY diff_index"""
    t[0] = DiffNode(t[2], t[1])

def p_diff_index(t):
    """diff_index : INT
                  | STRING"""
    t[0] = DiffIndexNode(data=t[1])

def p_field(t):
    """field : field_name opt_bspec
             | field_name opt_bspec LPAREN field_indices RPAREN"""
    if len(t) == 3:
        t[0] = FieldNode([t[2], None], t[1])
    else:
        t[0] = FieldNode([t[2], t[4]], t[1])

def p_opt_amendment_specs(t):
    """opt_amendment_specs :
                           | SEMICOLON amendment_spec opt_amendment_specs"""
    lt = len(t)
    if lt == 1:
        t[0] = AmendSpecNode()
    else:
        assert lt == 4
        t[0] = t[3].add(t[2])

def p_amendment_spec(t):
    """amendment_spec :
                      | amdmt_mxdim
                      | amdmt_diagonal_ones
                      | amdmt_gauge_fix
                      | amdmt_periodic"""
    t[0] = t[1]

def p_amdmt_mxdim(t):
    """amdmt_mxdim : MXDIM EQUALS LPAREN opt_fields VBAR VBAR opt_fields RPAREN"""
    t[0] = AmendMxDimNode([t[4], t[7]])

def p_amdmt_diagonal_ones(t):
    """amdmt_diagonal_ones : field EQUALS field"""
    t[0] = DiagAmendNode([t[1], t[3]])

def p_amdmt_gauge_fix(t):
    """amdmt_gauge_fix : GAUGEFIX field"""
    # { AMDMT_GAUGE_FIX $2 }
    pass

def p_amdmt_periodic(t):
    """amdmt_periodic : PERIODIC field"""
    #{ AMDMT_PERIODIC $2 }
    pass


def p_opt_fields(t):
    """opt_fields :
                  | STAR
                  | fields"""
    lt = len(t)
    if lt == 1:
        t[0] = OptFieldsNode()
    else:
        if t[1] == '*':
            t[0] = OptFieldsNode()
        else:
            t[0] = OptFieldsNode(t[1])

def p_fields(t):
    """fields : field
              | fields COMMA field"""
    lt = len(t)
    if lt == 2:
        t[0] = FieldsNode(t[1])
    else:
        assert lt == 4
        t[0] = t[1].add(t[3])

def p_opt_sum_specs(t):
    """opt_sum_specs :
                     | COMMA sum_specs"""
    if len(t) == 1:
        t[0] = None
    else:
        t[0] = t[2]

def p_sum_specs(t):
    """sum_specs :
                 | sum_spec
                 | sum_specs COMMA sum_spec"""
    lt = len(t)
    if lt == 1:
        t[0] = None
    elif lt == 2:
        t[0] = SumSpecsNode().add(t[1])
    else:
        assert lt == 4
        t[0] = t[1].add(t[3])

def p_sum_spec(t):
    """sum_spec : STRING COLON INT"""
    t[0] = SumSpecNode(data=(t[1], t[3]))

def p_opt_bspec(t):
    """opt_bspec :
                 | LBRACKET region_logic RBRACKET"""
    lt = len(t)
    if lt == 1:
        t[0] = BSpecsNode()
    else:
        t[0] = BSpecsNode(t[2])

def p_region_identifier(t):
    """region_identifier : INT
                         | STRING"""
    t[0] = RegionNode(data=t[1])

def p_region_logic_atomic(t):
    """region_logic_atomic : region_identifier
                           | LPAREN region_logic RPAREN
                           | DOF_REGION_ALL EQUALS region_identifier
                           | DOF_REGION_SOME EQUALS region_identifier
                           | HASH EQUALS INT"""
    lt = len(t)
    if lt == 2:                        # rule 1
        t[0] = RegLogSomeNode(t[1])
    else:
        assert lt == 4
        t1t = t[1]
        if t1t == '(':                 # rule 2
            t[0] = RegLogParenthesisNode(t[2])
        elif t1t == 'all':             # rule 3
            t[0] = RegLogAllNode(t[3])
        elif t1t == 'some':            # rule 4
            t[0] = RegLogSomeNode(t[3])
        else:                          # rule 5
            assert t1t == '#'
            t[0] = RegLogNRegsNode(data=t[3])

def p_region_logic_opt_not(t):
    """region_logic_opt_not : region_logic_atomic
                            | DOF_REGION_NOT region_logic_atomic"""
    lt = len(t)
    if lt == 2:
        t[0] = t[1]
    else:
        assert lt == 3
        t[0] = RegLogNotNode(t[2])

def p_region_logic_and(t):
    """region_logic_and : region_logic_opt_not
                        | region_logic_and DOF_REGION_AND region_logic_opt_not"""
    lt = len(t)
    if lt == 2:
        t[0] = RegLogAndNode().add(t[1])
    else:
        assert lt == 4
        t[0] = t[1].add(t[3])

def p_region_logic_or(t):
    """region_logic_or : region_logic_and
                       | region_logic_or DOF_REGION_OR region_logic_and"""
    lt = len(t)
    if lt == 2:
        t[0] = RegLogOrNode().add(t[1])
    else:
        assert lt == 3
        t[0] = t[1].add(t[3])

def p_region_logic(t):
    """region_logic :
                    | region_logic_or"""
    if len(t) == 1:
        t[0] = None # should be DLOG_true
    else:
        t[0] = t[1]

def p_field_name(t):
    """field_name : STRING"""
    t[0] = t[1]

def p_field_indices(t):
    """field_indices : field_index
                     | field_indices COMMA field_index"""
    if len(t) == 2:
        t[0] = FieldIndicesNode()
        if t[1] != None:
            t[0] = t[0].add(t[1])
    else:
        if t[1] != None:
            t[0] = t[1].add(t[3])

def p_field_index(t):
    """field_index :
                   | INT
                   | STRING"""
    lt = len(t)
    if lt == 1:
        t[0] = None
    else:
        assert lt == 2
        t[0] = FieldIndexNode(data=t[1])

def p_short_vector_restriction(t):
    """short_vector_restriction :
                                | opt_fields opt_sum_specs"""
    #short_vector_restriction:
    #|  opt_fields opt_sum_specs    { match $1 with
    #                                 | None -> None
    #                                 | Some fields ->
    #                                     let v_fields = expand_fields $2 fields in
    #                                       (* XXX NOTE: this actually is somewhat broken: expanding the
    #                                          field indices wrt sum specs will not give us what we expected
    #                                          in the next step, when we again throw away all indices:
    #                                       *)
    #                                       Some (Array.map (fun field -> (field.f_name, field.f_bspec)) v_fields)
    #                             }
    t[0] = None


def p_error(t):
    if hasattr(t, 'value'):
        info = " (token='%s', value='%s')" % (str(t), t.value)
    else:
        info = " (token='%s')" % str(t)
    raise ValueError("Syntax error when parsing equation%s." % info)

import ply.yacc as yacc

try:
    import diffop_parsetab
    tabmodule = diffop_parsetab
except:
    tabmodule = "diffop_parsetab"

parser = yacc.yacc(tabmodule=tabmodule,
                   #debugfile='diffop_parser.out',
                   outputdir=split(realpath(__file__))[0])
raw_input()

def parse_with_start_token(s, start_tokens=[]):
    def token(token_type):
        t = yacc.YaccSymbol()
        t.type = token_type
        t.value = None
        return t

    start_tokens = [token(t) for t in start_tokens]

    def lexer_function():
        if len(start_tokens) > 0:
            return start_tokens.pop()
        else:
            return lexer.token()

    lexer.input(s) # prepare lexer for input

    return parser.parse(tokenfunc=lexer_function)

def parse_diffop(s):
    return parse_with_start_token(s, start_tokens=['ENTRY_DIFFOP'])

def parse_restriction(s):
    return parse_with_start_token(s, start_tokens=['ENTRY_RESTRICTION'])

def parse_region_logic(s):
    return parse_with_start_token(s, start_tokens=['ENTRY_REGION_LOGIC'])

parse = parse_diffop
