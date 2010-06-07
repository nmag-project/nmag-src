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

tokens = ('INT', 'FLOAT', 'STRING', 'DOF_REGION_AND', 'DOF_REGION_OR',
          'DOF_REGION_NOT', 'DOF_REGION_SOME', 'DOF_REGION_ALL', 'DDX_VOL',
          'DDX_BOUNDARY', 'MXDIM', 'GAUGEFIX', 'PERIODIC', 'LPAREN', 'RPAREN',
          'LANGLE', 'RANGLE', 'VBAR', 'LBRACKET', 'RBRACKET', 'COMMA',
          'COLON', 'SEMICOLON', 'EQUALS', 'HASH', 'STAR', 'SIGN'
          )

# Tokens
t_STRING = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_DOF_REGION_AND  = r'and'
t_DOF_REGION_OR   = r'or'
t_DOF_REGION_NOT  = r'not'
t_DOF_REGION_SOME = r'some'
t_DOF_REGION_ALL  = r'all'
t_DDX_VOL         = r'd/dx'
t_DDX_BOUNDARY    = r'D/Dx'
t_MXDIM           = r'\(L\|\|R\)'
t_GAUGEFIX        = r'gauge_fix:'
t_PERIODIC        = r'periodic:'
t_LPAREN          = r'\('
t_RPAREN          = r'\)'
t_LANGLE          = r'<'
t_RANGLE          = r'>'
t_VBAR            = r'\|'
t_LBRACKET        = r'<'
t_RBRACKET        = r'>'
t_COMMA           = r','
t_COLON           = r':'
t_SEMICOLON       = r';'
t_EQUALS          = r'='
t_HASH            = r'\#'
t_STAR            = r'\*'

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

def p_parse_ddiffop(t):
    """parse_ddiffop : contribs opt_amendment_specs opt_sum_specs"""
    #{ build_ddiffop $1 $3 $2 }
    pass

def p_contribs(t):
    """contribs : contrib
                | contribs SIGN contrib"""
    # { [$1] }
    # { let (c1, t2) = $3 in ($2*.c1, t2)::$1 }
    pass

def p_contrib(t):
    """contrib : unsigned_contrib
               | SIGN contrib"""
    # { $1 }
    # { let (c, t) = $2 in ($1*.c, t) }
    pass

def p_unsigned_contrib(t):
    """unsigned_contrib : bracket
                        | expr bracket
                        | expr STAR bracket"""
    #            {(1.0,$1) }
    #            { ($1,$2) }
    #            { ($1,$3) }
    pass

def p_expr(t):
    """expr : unsigned_scalar
            | LPAREN signed_scalar RPAREN
            | LPAREN expr RPAREN"""
    #  { $1 }
    #  { $2 }
    #  { $2 }
    pass

def p_signed_scalar(t):
    """signed_scalar : SIGN unsigned_scalar
                     | SIGN signed_scalar"""
    #{ $1*.$2 }
    #{ $1*.$2 }
    pass


def p_unsigned_scalar(t):
    """unsigned_scalar : INT
                       | FLOAT"""
    #{ float_of_int $1 }
    #             { $1 }
    pass

def p_bracket(t):
    """bracket : LANGLE opt_diff_field VBAR VBAR opt_diff_field RANGLE
               | LANGLE opt_diff_field VBAR field VBAR opt_diff_field RANGLE"""
    # { ($2,$5,None) }
    # { ($2,$6,Some $4) }
    pass

def p_opt_diff_field(t):
    """opt_diff_field : field
                      | diff field
    """
    #    { ($1,PDIFF_none) }
    #    { ($2,$1) }
    pass


def p_diff(t):
    """diff : DDX_VOL     diff_index
            | DDX_BOUNDARY diff_index
    """
    #    { PDIFF_vol $2 }
    #     { PDIFF_boundary $2 }
    pass

def p_diff_index(t):
    """diff_index : INT
                  | STRING
    """
    #{ IX_int $1 }
    #{ IX_name $1 }
    pass

def p_field(t):
    """field : field_name opt_bspec
             | field_name opt_bspec LPAREN field_indices RPAREN
    """
    # { {fnsi_name=$1;fnsi_indices=[||];fnsi_bspec=$2;} }
    # { {fnsi_name=$1;fnsi_indices=Array.of_list $4;fnsi_bspec=$2;} }
    pass


def p_opt_amendment_specs(t):
    """opt_amendment_specs :
                           | SEMICOLON amendment_spec opt_amendment_specs"""
    #{ [] }
    #{ $2::$3 }
    pass

def p_amendment_spec(t):
    """amendment_spec :
                      | amdmt_mxdim
                      | amdmt_diagonal_ones
                      | amdmt_gauge_fix
                      | amdmt_periodic"""
    #{ $1 }
    #{ $1 }
    #{ $1 }
    #{ $1 }
    pass

def p_amdmt_mxdim(t):
    """amdmt_mxdim : MXDIM EQUALS LPAREN opt_fields VBAR VBAR opt_fields RPAREN"""
    #{ AMDMT_MXDIM ($4,$7) }
    pass

def p_amdmt_diagonal_ones(t):
    """amdmt_diagonal_ones : field EQUALS field"""
    #{ AMDMT_DIAGONAL_ONES ($1,$3) }
    pass

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
    #{ None }
    #{ Some $1 }
    pass

def p_fields(t):
    """fields : field
              | field COMMA fields"""
    #{ [$1] }
    #{ $1::$3 }
    pass

def notused_p_short_vector_restriction(t):
    """short_vector_restriction :
                                |  opt_fields opt_sum_specs"""
    #{ match $1 with
    #                            | None -> None
    #                            | Some fields ->
    #                                     let v_fields = expand_fields $2 fields in
    #                                       (* XXX NOTE: this actually is somewhat broken: expanding the
    #                                          field indices wrt sum specs will not give us what we expected
    #                                          in the next step, when we again throw away all indices:
    #                                       *)
    #                                       Some (Array.map (fun field -> (field.f_name, field.f_bspec)) v_fields)
    #                             }
    pass

def p_opt_sum_specs(t):
    """opt_sum_specs :
                     | COMMA sum_specs"""
    #{ [] }
    #{ $2 }
    pass

def p_sum_specs(t):
    """sum_specs :
                 | sum_spec
                 | sum_specs COMMA sum_spec"""
    #{ [$1] }
    #{ $3::$1 }
    pass

def p_sum_spec(t):
    """sum_spec : STRING COLON INT"""
    #{ ($1,$3) }
    pass

def p_opt_bspec(t):
    """opt_bspec :
                 | LBRACKET region_logic RBRACKET"""
    #{$2}
    #{ DLOG_true }
    pass

def p_region_logic_atomic(t):
    """region_logic_atomic : LPAREN region_logic RPAREN
                           | INT
                           | STRING
                           | DOF_REGION_ALL EQUALS INT
                           | DOF_REGION_SOME EQUALS INT
                           | DOF_REGION_ALL EQUALS STRING
                           | DOF_REGION_SOME EQUALS STRING
                           | HASH EQUALS INT
    """
    # {$2}
    # {DLOG_some (string_of_int $1)}
    # {DLOG_some $1}
    # {DLOG_all (string_of_int $3)}
    # {DLOG_some (string_of_int $3)}
    # {DLOG_all $3}
    # {DLOG_some $3}
    # {DLOG_nregions $3}
    pass

def p_region_logic_opt_not(t):
    """region_logic_opt_not : DOF_REGION_NOT region_logic_atomic
                            | region_logic_atomic
    """
    # {DLOG_not $2}
    # {$1}
    pass

def p_region_logic_and(t):
    """region_logic_and : region_logic_opt_not DOF_REGION_AND region_logic_and
                        | region_logic_opt_not
    """
    #  {DLOG_and [$1;$3]}
    #  {$1}
    pass

def p_region_logic_or(t):
    """region_logic_or : region_logic_and DOF_REGION_OR region_logic_or
                       | region_logic_and"""
    # {DLOG_or [$1;$3]}
    # {$1}
    pass


def p_region_logic(t):
    """region_logic :
                    | region_logic_or"""
    #   {$1}
    pass


def p_field_name(t):
    """field_name : STRING"""
    #  { $1 }
    pass


def p_field_indices(t):
    """field_indices : field_index
                     | field_index COMMA field_indices
    """
    # { [$1] }
    #    { $1::$3 }
    pass


def p_field_index(t):
    """field_index :
                   | INT
                   | STRING
    """
    #{ IX_int $1 }
    #{ IX_name $1 }
    pass

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
