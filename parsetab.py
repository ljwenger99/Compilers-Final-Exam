
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftPLUSMINUSleftTIMESDIVIDEASSIGN COMMA DIVIDE DONE ID INTEGER LBRACE LPAREN MINUS PLUS PRINT RBRACE READ RETURN RPAREN SEMI STRING TIMES TYPEprogram : declarations DONEdeclarations : declarations declaration\n                        | declarationdeclaration : gvardecl \n                        | funcdeclgvardecl : TYPE ID gvarinitgvarinit : SEMIgvarinit : ASSIGN INTEGER SEMIgvarinit : ASSIGN STRING SEMIfuncdecl : funcname LPAREN paramlist RPAREN blockfuncname : TYPE IDparamlist : paramlist : paramsparams : TYPE IDparams : TYPE ID COMMA paramsblock : LBRACE stmtlist RBRACEstmtlist : stmtlist : stmtlist stmt SEMIstmtlist : stmtlist stmtstmt : PRINT LPAREN expr RPARENstmt : READ LPAREN ID RPARENstmt : TYPE IDstmt : TYPE ID ASSIGN exprstmt : RETURN exprstmt : ID ASSIGN exprstmt : callexpr : INTEGERexpr : STRINGexpr : IDexpr : expr PLUS exprexpr : expr MINUS exprexpr : expr TIMES exprexpr : expr DIVIDE exprexpr : MINUS exprexpr : callcall ::= ID LPAREN arglist RPARENarglist : arglist : argsargs : exprargs : args COMMA expr'
    
_lr_action_items = {'TYPE':([0,2,3,4,5,9,11,12,13,22,23,24,25,26,27,29,30,36,37,42,43,44,45,46,48,51,60,61,62,63,65,66,67,68,69,],[6,6,-3,-4,-5,-2,17,-6,-7,-8,-9,-10,-17,17,34,-16,-19,-26,-18,-22,-24,-27,-28,-29,-35,-25,-34,-20,-21,-36,-23,-30,-31,-32,-33,]),'$end':([1,8,],[0,-1,]),'DONE':([2,3,4,5,9,12,13,22,23,24,29,],[8,-3,-4,-5,-2,-6,-7,-8,-9,-10,-16,]),'ID':([6,17,25,27,30,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,51,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,],[10,21,-17,33,-19,42,46,-26,-18,46,50,46,46,-22,-24,-27,-28,-29,46,-35,-25,46,46,46,46,46,-34,-20,-21,-36,46,-23,-30,-31,-32,-33,]),'LPAREN':([7,10,31,32,33,46,],[11,-11,38,39,41,41,]),'SEMI':([10,18,19,30,36,42,43,44,45,46,48,51,60,61,62,63,65,66,67,68,69,],[13,22,23,37,-26,-22,-24,-27,-28,-29,-35,-25,-34,-20,-21,-36,-23,-30,-31,-32,-33,]),'ASSIGN':([10,33,42,],[14,40,55,]),'RPAREN':([11,15,16,21,28,41,44,45,46,48,49,50,52,53,54,60,63,66,67,68,69,70,],[-12,20,-13,-14,-15,-37,-27,-28,-29,-35,61,62,63,-38,-39,-34,-36,-30,-31,-32,-33,-40,]),'INTEGER':([14,35,38,40,41,47,55,56,57,58,59,64,],[18,44,44,44,44,44,44,44,44,44,44,44,]),'STRING':([14,35,38,40,41,47,55,56,57,58,59,64,],[19,45,45,45,45,45,45,45,45,45,45,45,]),'LBRACE':([20,],[25,]),'COMMA':([21,44,45,46,48,53,54,60,63,66,67,68,69,70,],[26,-27,-28,-29,-35,64,-39,-34,-36,-30,-31,-32,-33,-40,]),'RBRACE':([25,27,30,36,37,42,43,44,45,46,48,51,60,61,62,63,65,66,67,68,69,],[-17,29,-19,-26,-18,-22,-24,-27,-28,-29,-35,-25,-34,-20,-21,-36,-23,-30,-31,-32,-33,]),'PRINT':([25,27,30,36,37,42,43,44,45,46,48,51,60,61,62,63,65,66,67,68,69,],[-17,31,-19,-26,-18,-22,-24,-27,-28,-29,-35,-25,-34,-20,-21,-36,-23,-30,-31,-32,-33,]),'READ':([25,27,30,36,37,42,43,44,45,46,48,51,60,61,62,63,65,66,67,68,69,],[-17,32,-19,-26,-18,-22,-24,-27,-28,-29,-35,-25,-34,-20,-21,-36,-23,-30,-31,-32,-33,]),'RETURN':([25,27,30,36,37,42,43,44,45,46,48,51,60,61,62,63,65,66,67,68,69,],[-17,35,-19,-26,-18,-22,-24,-27,-28,-29,-35,-25,-34,-20,-21,-36,-23,-30,-31,-32,-33,]),'MINUS':([35,38,40,41,43,44,45,46,47,48,49,51,54,55,56,57,58,59,60,63,64,65,66,67,68,69,70,],[47,47,47,47,57,-27,-28,-29,47,-35,57,57,57,47,47,47,47,47,-34,-36,47,57,-30,-31,-32,-33,57,]),'PLUS':([43,44,45,46,48,49,51,54,60,63,65,66,67,68,69,70,],[56,-27,-28,-29,-35,56,56,56,-34,-36,56,-30,-31,-32,-33,56,]),'TIMES':([43,44,45,46,48,49,51,54,60,63,65,66,67,68,69,70,],[58,-27,-28,-29,-35,58,58,58,58,-36,58,58,58,-32,-33,58,]),'DIVIDE':([43,44,45,46,48,49,51,54,60,63,65,66,67,68,69,70,],[59,-27,-28,-29,-35,59,59,59,59,-36,59,59,59,-32,-33,59,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'declarations':([0,],[2,]),'declaration':([0,2,],[3,9,]),'gvardecl':([0,2,],[4,4,]),'funcdecl':([0,2,],[5,5,]),'funcname':([0,2,],[7,7,]),'gvarinit':([10,],[12,]),'paramlist':([11,],[15,]),'params':([11,26,],[16,28,]),'block':([20,],[24,]),'stmtlist':([25,],[27,]),'stmt':([27,],[30,]),'call':([27,35,38,40,41,47,55,56,57,58,59,64,],[36,48,48,48,48,48,48,48,48,48,48,48,]),'expr':([35,38,40,41,47,55,56,57,58,59,64,],[43,49,51,54,60,65,66,67,68,69,70,]),'arglist':([41,],[52,]),'args':([41,],[53,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> declarations DONE','program',2,'p_program','C--Starter.py',246),
  ('declarations -> declarations declaration','declarations',2,'p_declarations','C--Starter.py',261),
  ('declarations -> declaration','declarations',1,'p_declarations','C--Starter.py',262),
  ('declaration -> gvardecl','declaration',1,'p_declaration','C--Starter.py',267),
  ('declaration -> funcdecl','declaration',1,'p_declaration','C--Starter.py',268),
  ('gvardecl -> TYPE ID gvarinit','gvardecl',3,'p_gvardecl','C--Starter.py',284),
  ('gvarinit -> SEMI','gvarinit',1,'p_gvarinit_none','C--Starter.py',307),
  ('gvarinit -> ASSIGN INTEGER SEMI','gvarinit',3,'p_gvarinit_int','C--Starter.py',311),
  ('gvarinit -> ASSIGN STRING SEMI','gvarinit',3,'p_gvarinit_str','C--Starter.py',315),
  ('funcdecl -> funcname LPAREN paramlist RPAREN block','funcdecl',5,'p_funcdecl','C--Starter.py',351),
  ('funcname -> TYPE ID','funcname',2,'p_funcname','C--Starter.py',378),
  ('paramlist -> <empty>','paramlist',0,'p_paramlist_none','C--Starter.py',398),
  ('paramlist -> params','paramlist',1,'p_paramlist_some','C--Starter.py',402),
  ('params -> TYPE ID','params',2,'p_params_one','C--Starter.py',406),
  ('params -> TYPE ID COMMA params','params',4,'p_params_some','C--Starter.py',416),
  ('block -> LBRACE stmtlist RBRACE','block',3,'p_block','C--Starter.py',430),
  ('stmtlist -> <empty>','stmtlist',0,'p_stmtlist_none','C--Starter.py',434),
  ('stmtlist -> stmtlist stmt SEMI','stmtlist',3,'p_stmtlist_some','C--Starter.py',438),
  ('stmtlist -> stmtlist stmt','stmtlist',2,'p_stmtlist_last','C--Starter.py',448),
  ('stmt -> PRINT LPAREN expr RPAREN','stmt',4,'p_stmt_print','C--Starter.py',471),
  ('stmt -> READ LPAREN ID RPAREN','stmt',4,'p_stmt_read','C--Starter.py',483),
  ('stmt -> TYPE ID','stmt',2,'p_stmt_vardecl_noinit','C--Starter.py',495),
  ('stmt -> TYPE ID ASSIGN expr','stmt',4,'p_stmt_vardecl_init','C--Starter.py',513),
  ('stmt -> RETURN expr','stmt',2,'p_stmt_return','C--Starter.py',529),
  ('stmt -> ID ASSIGN expr','stmt',3,'p_stmt_assign','C--Starter.py',535),
  ('stmt -> call','stmt',1,'p_stmt_call','C--Starter.py',549),
  ('expr -> INTEGER','expr',1,'p_expr_int','C--Starter.py',572),
  ('expr -> STRING','expr',1,'p_expr_str','C--Starter.py',579),
  ('expr -> ID','expr',1,'p_expr_var','C--Starter.py',586),
  ('expr -> expr PLUS expr','expr',3,'p_expr_plus','C--Starter.py',596),
  ('expr -> expr MINUS expr','expr',3,'p_expr_minus','C--Starter.py',615),
  ('expr -> expr TIMES expr','expr',3,'p_expr_times','C--Starter.py',628),
  ('expr -> expr DIVIDE expr','expr',3,'p_expr_divide','C--Starter.py',641),
  ('expr -> MINUS expr','expr',2,'p_expr_negate','C--Starter.py',654),
  ('expr -> call','expr',1,'p_expr_call','C--Starter.py',668),
  ('call -> ID LPAREN arglist RPAREN','call',4,'p_function_call','C--Starter.py',689),
  ('arglist -> <empty>','arglist',0,'p_arglist_none','C--Starter.py',707),
  ('arglist -> args','arglist',1,'p_arglist_some','C--Starter.py',711),
  ('args -> expr','args',1,'p_args_one','C--Starter.py',715),
  ('args -> args COMMA expr','args',3,'p_args_some','C--Starter.py',720),
]