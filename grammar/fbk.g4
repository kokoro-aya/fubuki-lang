grammar fbk;

top_level: type EOF;

fragment DIGIT_HEAD: [1-9];
fragment DIGIT: [0-9];
INT: DIGIT_HEAD DIGIT* | DIGIT;

fragment IDENT_HEAD: [A-Z] | [a-z] | '_' | '$';
fragment IDENT_CHAR: [0-9] | IDENT_HEAD;
fragment CHAR: ~['"\\EOF\n];
fragment ESC: '\\' | '\\\\';

STR_LITERAL: '"' (ESC | .)*? '"';
CHAR_LITERAL: '\'' CHAR '\'';

WS: [\n\r\t ] -> skip;

nat: INT;
real: INT '.' INT;
bool: 'true' | 'false';
str: STR_LITERAL;
chr: CHAR_LITERAL;
IDENTIFIER: IDENT_HEAD IDENT_CHAR*;

literal: nat | real | bool | str | chr;

// Expressions

expr: expr10 (switch_expression | );
expr10 : expr9 | pattern ('+=' | '-=' | '*=' | '/=' | '%=' | '=') expr9 | lambda_expression;
expr9 : expr8 | expr8 '||' expr9;
expr8 : expr7 | expr7 '&&' expr8;
expr7 : expr6 | expr6 '^^' expr7; // xor
expr6 : expr5 | expr5 ('==' | '!=') expr6;
expr5 : expr4 | expr4 ('<' | '<=' | '>' | '>=') expr5;
expr4 : expr3 | expr3 ('...' | '..<' | '>>.' | '>..' | '@') expr4;
expr3 : expr2 | expr2 '++' expr3;
expr2 : expr1 | expr1 ('<<' | '>>') expr2;
expr1 : <assoc=right> expr0 | expr0 '^' expr1; // exponen
expr0: term | term ('+' | '-') expr0;
term: subterm | subterm ('*' | '/' | '%') term;
subterm: factor | ('!' | '+' | '-') factor;
factor: primary (chained_method_invocation | );
primary: '(' expr (',' expr)* ','? ')' | subprimary;

// Primaries

subprimary: literal_primary
       | function_call_primary
       | variable_primary
       | function_declaration;

literal_primary: literal | array_literal;
array_literal: '[' expr (',' expr)* ','? ']';

variable_primary: pattern type_annotation?;

function_call_primary: IDENTIFIER function_call_argument_clause;
function_call_argument_clause: '(' function_call_argument_list? ')';
function_call_argument_list: function_call_argument (',' function_call_argument)*;
function_call_argument: (IDENTIFIER ':')? expr;

chained_method_invocation: ('.' function_call_primary)+;

// Types

type: function_type | array_type | type_identifier | tuple_type;
function_type: tuple_type '->' type;
tuple_type: '(' tuple_argument_list? ')';
tuple_argument_list: type (',' type)*;
array_type: '[' type ']';
type_identifier: IDENTIFIER;
type_annotation: ':' type;

// Patterns

pattern: wildcard_pattern
       | identifier_pattern type_annotation?
       | tuple_pattern
       | subscript_pattern;

identifier_pattern: IDENTIFIER;
wildcard_pattern: '_';
tuple_pattern: '(' tuple_pattern_elements ')';
tuple_pattern_elements: tuple_pattern_element (',' tuple_pattern_element)*;
tuple_pattern_element: (wildcard_pattern | subscript_pattern | identifier_pattern) type_annotation? | '_';
subscript_pattern: IDENTIFIER ('[' subscript ']')+;
subscript: IDENTIFIER | literal | reverse_subscript | slice_subscript | expr;
reverse_subscript: '^' expr;
slice_subscript: slice_subscript_part '..' slice_subscript_part
               | '..' slice_subscript_part
               | slice_subscript_part '..';
slice_subscript_part: reverse_subscript | expr;

switch_expression: 'switch' '{' switch_expr_arm+ '}';
switch_expr_arm: literal_arm ','? | default_arm;
default_arm: '_' '=>' expr;
literal_arm: literal '=>' expr;

lambda_expression: function_declaration; // lambda only

// Statements

statements: statement+;

statement: declaration ';'?
         | expr ';'?
         | loop_statement ';'?
         | branch_statement ';'?
         | control_transfer_statement ';'?
         | do_statement ';'?
         ;

loop_statement: for_in_statement | while_statement | repeat_while_statement;

for_in_statement: 'for' pattern 'in' expr code_block;

while_statement: 'while' condition_list code_block;
condition_list: condition (',' condition)*;
condition: expr;

repeat_while_statement: 'repeat' code_block 'while' expr;

branch_statement: if_statement | switch_statement;

if_statement: 'if' condition_list code_block else_clause?;
else_clause: 'else' code_block | 'else' if_statement;

switch_statement: 'switch' expr '{' switch_cases? '}';
switch_cases: switch_case+;
switch_case: case_label '{'? statements '}'?
           | default_label '{'? statement '}'?;
case_label: 'case' case_item_list ':';
case_item_list: case_item (',' case_item)*;
case_item: literal;
default_label: 'default' ':';

control_transfer_statement: break_statement | continue_statement | fallthrough_statement | return_statement;
break_statement: 'break';
continue_statement: 'continue';
fallthrough_statement: ':||' | 'fallthrough';
return_statement: 'return' expr?;

do_statement: 'do' code_block;

code_block: '{' statements? '}';

// Declarations

declaration: constant_declaration
           | variable_declaration
           | function_declaration
           ;

constant_declaration: 'val' pattern_initializer_list;
variable_declaration: 'var' pattern_initializer_list;

pattern_initializer_list: pattern_initializer (',' pattern_initializer)*;
pattern_initializer: simple_pattern_initializer | destruct_pattern_initializer;
simple_pattern_initializer: IDENTIFIER type_annotation? initializer;
destruct_pattern_initializer: tuple_pattern initializer;
initializer: '=' expr;

// Functions

function_declaration: function_head function_name? generic_parameter_clause? function_signature function_body;

function_head: 'fn';
function_name: IDENTIFIER;

function_signature: parameter_clause function_result?;
function_result: type_annotation;
function_body: code_block | '=>' expr;

parameter_clause: '(' parameter_list? ')';
parameter_list: parameter (',' parameter)*;
parameter: default_name? parameter_name type_annotation? default_argument_clause?; // allow for lambda usage
default_name: '_' | IDENTIFIER;
parameter_name: '?' | IDENTIFIER;
default_argument_clause: initializer;

generic_parameter_clause: '<' generic_parameter_list '>';
generic_parameter_list: generic_parameter (',' generic_parameter)*;
generic_parameter: IDENTIFIER;