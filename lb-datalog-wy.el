;;; lb-datalog-wy.el --- Generated parser support file

;; Copyright (C) 2002, 2003, 2004, 2005, 2006 David Ponce

;; Author: George Balatsouras <machinist@machinarium>
;; Created: 2015-12-01 15:40:53+0200
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file lb-datalog.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;

;;; Declarations
;;
(defconst wisent-lb-datalog-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("boolean" . BOOLEAN)
     ("float" . FLOAT)
     ("float[32]" . FLOAT32)
     ("float[64]" . FLOAT64)
     ("int" . INT)
     ("int[8]" . INT8)
     ("int[16]" . INT16)
     ("int[32]" . INT32)
     ("int[64]" . INT64)
     ("uint" . UINT)
     ("uint[8]" . UINT8)
     ("uint[16]" . UINT16)
     ("uint[32]" . UINT32)
     ("uint[64]" . UINT64)
     ("string" . STRING)
     ("color" . COLOR)
     ("datetime" . DATETIME)
     ("decimal" . DECIMAL)
     ("decimal[64]" . DECIMAL64)
     ("decimal[128]" . DECIMAL128)
     ("agg" . AGG))
   '(("decimal[128]" summary "Decimal primitive type (128-bit)")
     ("decimal[64]" summary "Decimal primitive type (64-bit)")
     ("decimal" summary "Decimal primitive type")
     ("datetime" summary "Datetime primitive type")
     ("color" summary "Color primitive type")
     ("string" summary "String primitive type")
     ("uint[64]" summary "Unsigned integral primitive type (64-bit)")
     ("uint[32]" summary "Unsigned integral primitive type (32-bit)")
     ("uint[16]" summary "Unsigned integral primitive type (16-bit)")
     ("uint[8]" summary "Unsigned integral primitive type (8-bit)")
     ("uint" summary "Unsigned integral primitive type")
     ("int[64]" summary "Integral primitive type (64-bit)")
     ("int[32]" summary "Integral primitive type (32-bit)")
     ("int[16]" summary "Integral primitive type (16-bit)")
     ("int[8]" summary "Integral primitive type (8-bit)")
     ("int" summary "Integral primitive type")
     ("float[64]" summary "Primitive floating-point type (double-precision 64-bit IEEE 754)")
     ("float[32]" summary "Primitive floating-point type (single-precision 32-bit IEEE 754)")
     ("float" summary "Primitive floating-point type (single-precision 32-bit IEEE 754)")
     ("boolean" summary "Primitive logical quantity type (true or false)")))
  "Table of language keywords.")

(defconst wisent-lb-datalog-wy--token-table
  (semantic-lex-make-type-table
   '(("number"
      (NUMBER_LITERAL))
     ("string"
      (STRING_LITERAL))
     ("symbol"
      (IDENTIFIER)
      (PHASE_LITERAL . "final")
      (PHASE_LITERAL . "initial")
      (PHASE_LITERAL . "init")
      (PHASE_LITERAL . "previous")
      (PHASE_LITERAL . "prev")
      (BOOLEAN_LITERAL . "\\`true\\'")
      (BOOLEAN_LITERAL . "\\`false\\'"))
     ("close-paren"
      (RAGG . ">>")
      (RBRACKET . "]")
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LAGG . "<<")
      (LBRACKET . "[")
      (LBRACE . "{")
      (LPAREN . "("))
     ("block"
      (BRACKET_BLOCK . "(LBRACKET RBRACKET)")
      (BRACE_BLOCK . "(LBRACE RBRACE)")
      (PAREN_BLOCK . "(LPAREN RPAREN)"))
     ("punctuation"
      (UPSERT . "^")
      (AT . "@")
      (LARROW . "<-")
      (RARROW . "->")
      (GTEQ . ">=")
      (GT . ">")
      (EQ . "=")
      (LTEQ . "<=")
      (LT . "<")
      (SEMICOLON . ";")
      (COLON . ":")
      (DIV . "/")
      (DOT . ".")
      (MINUS . "-")
      (COMMA . ",")
      (PLUS . "+")
      (MULT . "*")
      (NOTEQ . "!=")
      (NOT . "!")))
   '(("number" :declared t)
     ("string" :declared t)
     ("symbol" :declared t)
     ("block" :declared t)
     ("punctuation" :declared t)
     ("keyword" :declared t)))
  "Table of lexical tokens.")

(defconst wisent-lb-datalog-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((BOOLEAN FLOAT FLOAT32 FLOAT64 INT INT8 INT16 INT32 INT64 UINT UINT8 UINT16 UINT32 UINT64 STRING COLOR DATETIME DECIMAL DECIMAL64 DECIMAL128 AGG NOT NOTEQ MULT PLUS COMMA MINUS DOT DIV COLON SEMICOLON LT LTEQ EQ GT GTEQ RARROW LARROW AT UPSERT PAREN_BLOCK BRACE_BLOCK BRACKET_BLOCK LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET LAGG RAGG BOOLEAN_LITERAL PHASE_LITERAL IDENTIFIER STRING_LITERAL NUMBER_LITERAL)
       nil
       (compilation_unit
        ((clause)))
       (clause
        ((rule))
        ((constraint)))
       (rule
        ((formula LARROW formula DOT))
        ((formula LARROW agg_head DOT))
        ((formula LARROW DOT)))
       (constraint
        ((formula RARROW formula DOT))
        ((formula RARROW DOT))
        ((formula DOT)))
       (formula
        ((disjunctive_formula)))
       (disjunctive_formula
        ((disjunctive_formula SEMICOLON conjunctive_formula))
        ((conjunctive_formula)))
       (conjunctive_formula
        ((conjunctive_formula COMMA negated_formula))
        ((negated_formula)))
       (negated_formula
        ((NOT simple_formula))
        ((simple_formula)))
       (simple_formula
        ((LPAREN formula RPAREN))
        ((expr comp_op expr))
        ((atom_ext)))
       (expr
        ((arithm_expr)))
       (comp_op
        ((LT))
        ((LTEQ))
        ((GT))
        ((GTEQ))
        ((EQ))
        ((NOTEQ)))
       (arithm_expr
        ((additive_expr)))
       (additive_expr
        ((additive_expr PLUS multiplicative_expr))
        ((additive_expr MINUS multiplicative_expr))
        ((multiplicative_expr)))
       (multiplicative_expr
        ((multiplicative_expr MULT unary_expr))
        ((multiplicative_expr DIV unary_expr))
        ((unary_expr)))
       (unary_expr
        ((IDENTIFIER))
        ((literal))
        ((LPAREN expr RPAREN)))
       (literal
        ((BOOLEAN_LITERAL))
        ((NUMBER_LITERAL))
        ((STRING_LITERAL)))
       (agg_head
        ((AGG LAGG IDENTIFIER EQ IDENTIFIER LPAREN var_list RPAREN RAGG)))
       (atom_ext
        ((simple_atom))
        ((refmode_atom))
        ((functional_atom))
        ((primitive_type_atom)))
       (simple_atom
        ((pred LPAREN var_list_ext RPAREN)))
       (functional_atom
        ((pred LBRACKET var_list_ext RBRACKET EQ IDENTIFIER)))
       (refmode_atom
        ((pred LPAREN var_ext COLON var_ext RPAREN)))
       (primitive_type_atom
        ((primitive_type LPAREN var_ext RPAREN)))
       (pred
        ((delta_pred)))
       (delta_pred
        ((simple_pred))
        ((PLUS simple_pred))
        ((MINUS simple_pred))
        ((MULT simple_pred))
        ((UPSERT simple_pred)))
       (simple_pred
        ((IDENTIFIER))
        ((IDENTIFIER AT PHASE_LITERAL)))
       (var
        ((IDENTIFIER)))
       (var_ext
        ((IDENTIFIER LBRACKET var_list_ext RBRACKET))
        ((IDENTIFIER))
        ((literal)))
       (vars
        ((LPAREN)
         nil)
        ((RPAREN)
         nil)
        ((var COMMA))
        ((var RPAREN)))
       (vars_ext
        ((LPAREN)
         nil)
        ((RPAREN)
         nil)
        ((var_ext COMMA))
        ((var_ext RPAREN)))
       (var_list
        ((PAREN_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'vars 1)))
       (var_list_ext
        ((PAREN_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'vars_ext 1)))
       (primitive_type
        ((numeric_type))
        ((BOOLEAN))
        ((STRING))
        ((COLOR))
        ((DATETIME)))
       (numeric_type
        ((integral_type))
        ((floating_point_type)))
       (integral_type
        ((INT))
        ((INT8))
        ((INT16))
        ((INT32))
        ((INT64))
        ((UINT))
        ((UINT8))
        ((UINT16))
        ((UINT32))
        ((UINT64)))
       (floating_point_type
        ((FLOAT))
        ((FLOAT32))
        ((FLOAT64))
        ((DECIMAL))
        ((DECIMAL64))
        ((DECIMAL128))))
     '(compilation_unit clause var var_ext vars_ext vars)))
  "Parser table.")

(defun wisent-lb-datalog-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic--parse-table wisent-lb-datalog-wy--parse-table
        semantic-debug-parser-source "lb-datalog.wy"
        semantic-flex-keywords-obarray wisent-lb-datalog-wy--keyword-table
        semantic-lex-types-obarray wisent-lb-datalog-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(define-lex-string-type-analyzer wisent-lb-datalog-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((UPSERT . "^")
    (AT . "@")
    (LARROW . "<-")
    (RARROW . "->")
    (GTEQ . ">=")
    (GT . ">")
    (EQ . "=")
    (LTEQ . "<=")
    (LT . "<")
    (SEMICOLON . ";")
    (COLON . ":")
    (DIV . "/")
    (DOT . ".")
    (MINUS . "-")
    (COMMA . ",")
    (PLUS . "+")
    (MULT . "*")
    (NOTEQ . "!=")
    (NOT . "!"))
  'punctuation)

(define-lex-block-type-analyzer wisent-lb-datalog-wy--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("(" LPAREN PAREN_BLOCK)
     ("{" LBRACE BRACE_BLOCK)
     ("[" LBRACKET BRACKET_BLOCK))
    (")" RPAREN)
    ("}" RBRACE)
    ("]" RBRACKET))
  )

(define-lex-regex-type-analyzer wisent-lb-datalog-wy--<symbol>-regexp-analyzer
  "regexp analyzer for <symbol> tokens."
  "\\(\\sw\\|\\s_\\)+"
  '((PHASE_LITERAL . "final")
    (PHASE_LITERAL . "initial")
    (PHASE_LITERAL . "init")
    (PHASE_LITERAL . "previous")
    (PHASE_LITERAL . "prev")
    (BOOLEAN_LITERAL . "\\`true\\'")
    (BOOLEAN_LITERAL . "\\`false\\'"))
  'IDENTIFIER)

(define-lex-regex-type-analyzer wisent-lb-datalog-wy--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'NUMBER_LITERAL)

(define-lex-sexp-type-analyzer wisent-lb-datalog-wy--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\s\""
  'STRING_LITERAL)

(define-lex-keyword-type-analyzer wisent-lb-datalog-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")


;;; Epilogue
;;
;; Define the lexer for this grammar
(define-lex wisent-lb-datalog-lexer
  "Lexical analyzer that handles LB Datalog buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  ;;;; Auto-generated analyzers.
  wisent-lb-datalog-wy--<number>-regexp-analyzer
  wisent-lb-datalog-wy--<string>-sexp-analyzer
  ;; Must detect keywords before other symbols
  wisent-lb-datalog-wy--<keyword>-keyword-analyzer
  wisent-lb-datalog-wy--<symbol>-regexp-analyzer
  wisent-lb-datalog-wy--<punctuation>-string-analyzer
  wisent-lb-datalog-wy--<block>-block-analyzer
  ;;;;
  semantic-lex-default-action)

(provide 'wisent/lb-datalog-wy)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; lb-datalog-wy.el ends here
