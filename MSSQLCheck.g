grammar MSSQLCheck;

// Version: 0.8
// ANTLR Version: 2.7.2
// Date: 2003.08.25
//
// Description: This is a MS SQL Server 2000 SELECT statement grammar.
//
// =======================================================================================
// Author: Tomasz Jastrzebski
// Contact: tdjastrzebski@yahoo.com
// Working parser/lexer generated based on this grammar will available for some time at:
// http://jastrzebski.europe.webmatrixhosting.net/mssqlparser.aspx 

options {
    //testLiterals = false;
    k = 2;
    //caseSensitive = false;
    //caseSensitiveLiterals = false;
}

tokens {
    ADD = 'add' ;
    ALL = 'all' ;
    ALTER = 'alter' ;
    AND = 'and' ;
    ANY = 'any' ;
    AS = 'as' ;
    ASC = 'asc' ;
    AUTHORIZATION = 'authorization' ;
    AUTO = 'auto' ;
    BACKUP = 'backup' ;
    BASE64 = 'base64' ;
    BEGIN = 'begin' ;
    BETWEEN = 'between' ;
    BINARY = 'binary' ;
    BREAK = 'break' ;
    BROWSE = 'browse' ;
    BULK = 'bulk' ;
    BY = 'by' ;
    CASCADE = 'cascade' ;
    CASE = 'case' ;
    CAST = 'cast' ;
    CHECK = 'check' ;
    CHECKPOINT = 'checkpoint' ;
    CLOSE = 'close' ;
    CLUSTERED = 'clustered' ;
    // COALESCE = 'coalesce' ;
    COLLATE = 'collate' ;
    COLUMN = 'column' ;
    COMMIT = 'commit' ;
    COMPUTE = 'compute' ;
    CONCAT = 'concat' ;
    CONSTRAINT = 'constraint' ;
    CONTAINS = 'contains' ;
    CONTAINSTABLE = 'containstable' ;
    CONTINUE = 'continue' ;
    // CONVERT = 'convert' ;
    CREATE = 'create' ;
    CROSS = 'cross' ;
    CUBE = 'cube' ;
    CURRENT = 'current' ;
    CURRENT_DATE = 'current_date' ;
    CURRENT_TIME = 'current_time' ;
    CURRENT_TIMESTAMP = 'current_timestamp' ;
    CURRENT_USER = 'current_user' ;
    CURSOR = 'cursor' ;
    DATABASE = 'database' ;
    DBCC = 'dbcc' ;
    DEALLOCATE = 'deallocate' ;
    DECLARE = 'declare' ;
    DEFAULT = 'default' ;
    DELETE = 'delete' ;
    DENY = 'deny' ;
    DESC = 'desc' ;
    DISK = 'disk' ;
    DISTINCT = 'distinct' ;
    DISTRIBUTED = 'distributed' ;
    DOUBLE = 'double' ;
    DROP = 'drop' ;
    // DUMMY = 'dummy' ;
    DUMP = 'dump' ;
    ELEMENTS = 'elements' ;
    ELSE = 'else' ;
    END = 'end' ;
    ERRLVL = 'errlvl' ;
    ESCAPE = 'escape' ;
    EXCEPT = 'except' ;
    EXEC = 'exec' ;
    EXECUTE = 'execute' ;
    EXISTS = 'exists' ;
    EXIT = 'exit' ;
    EXPAND = 'expand' ;
    EXPLICIT = 'explicit' ;
    FAST = 'fast' ;
    FASTFIRSTROW = 'fastfirstrow' ;
    FETCH = 'fetch' ;
    FILE = 'file' ;
    FILLFACTOR = 'fillfactor' ;
    FOR = 'for' ;
    FORCE = 'force' ;
    FOREIGN = 'foreign' ;
    FREETEXT = 'freetext' ;
    FREETEXTTABLE = 'freetexttable' ;
    FROM = 'from' ;
    FULL = 'full' ;
    FUNCTION = 'function' ;
    GOTO = 'goto' ;
    GRANT = 'grant' ;
    GROUP = 'group' ;
    HASH = 'hash' ;
    HAVING = 'having' ;
    HOLDLOCK = 'holdlock' ;
    IDENTITY = 'identity' ;
    IDENTITY_INSERT = 'identity_insert' ;
    IDENTITYCOL = 'identitycol' ;
    IF = 'if' ;
    IN = 'in' ;
    INDEX = 'index' ;
    INNER = 'inner' ;
    INSERT = 'insert' ;
    INTERSECT = 'intersect' ;
    INTO = 'into' ;
    IS = 'is' ;
    JOIN = 'join' ;
    KEEP = 'keep' ;
    KEEPFIXED = 'keepfixed' ;
    KEY = 'key' ;
    KILL = 'kill' ;
    LEFT = 'left' ;
    LIKE = 'like' ;
    LINENO = 'lineno' ;
    LOAD = 'load' ;
    LOOP = 'loop' ;
    MAXDOP = 'maxdop' ;
    MERGE = 'merge' ;
    NATIONAL = 'national' ;
    NOCHECK = 'nocheck' ;
    NOLOCK = 'nolock' ;
    NONCLUSTERED = 'nonclustered' ;
    NOT = 'not' ;
    NULL = 'null' ;
    // NULLIF = 'nullif' ;
    OF = 'of' ;
    OFF = 'off' ;
    OFFSETS = 'offsets' ;
    ON = 'on' ;
    OPEN = 'open' ;
    OPENDATASOURCE = 'opendatasource' ;
    OPENQUERY = 'openquery' ;
    OPENROWSET = 'openrowset' ;
    OPENXML = 'openxml' ;
    OPTION = 'option' ;
    OR = 'or' ;
    ORDER = 'order' ;
    OUTER = 'outer' ;
    OVER = 'over' ;
    PAGLOCK = 'paglock' ;
    PERCENT = 'percent' ;
    PLAN = 'plan' ;
    PRECISION = 'precision' ;
    PRIMARY = 'primary' ;
    PRINT = 'print' ;
    PROC = 'proc' ;
    PROCEDURE = 'procedure' ;
    PUBLIC = 'public' ;
    RAISERROR = 'raiserror' ;
    RAW = 'raw' ;
    READ = 'read' ;
    READCOMMITED = 'readcommited' ;
    READPAST = 'readpast' ;
    READTEXT = 'readtext' ;
    READUNCOMMITED = 'readuncommited' ;
    RECONFIGURE = 'reconfigure' ;
    REFERENCES = 'references' ;
    REMOTE = 'remote' ;
    REPEATABLEREAD = 'repeatableread' ;
    REPLICATION = 'replication' ;
    RESTORE = 'restore' ;
    RESTRICT = 'restrict' ;
    RETURN = 'return' ;
    REVOKE = 'revoke' ;
    RIGHT = 'right' ;
    ROBUST = 'robust' ;
    ROLLBACK = 'rollback' ;
    ROLLUP = 'rollup' ;
    ROWCOUNT = 'rowcount' ;
    ROWGUIDCOL = 'rowguidcol' ;
    ROWLOCK = 'rowlock' ;
    RULE = 'rule' ;
    SAVE = 'save' ;
    SCHEMA = 'schema' ;
    SELECT = 'select' ;
    SERIALIZABLE = 'serializable' ;
    SESSION_USER = 'session_user' ;
    SET = 'set' ;
    SETUSER = 'setuser' ;
    SHUTDOWN = 'shutdown' ;
    SOME = 'some' ;
    STATISTICS = 'statistics' ;
    SYSTEM_USER = 'system_user' ;
    TABLE = 'table' ;
    TABLOCK = 'tablock' ;
    TABLOCKX = 'tablockx' ;
    TEXTSIZE = 'textsize' ;
    THEN = 'then' ;
    TIES = 'ties' ;
    TO = 'to' ;
    TOP = 'top' ;
    TRAN = 'tran' ;
    TRANSACTION = 'transaction' ;
    TRIGGER = 'trigger' ;
    TRUNCATE = 'truncate' ;
    TSEQUAL = 'tsequal' ;
    UNION = 'union' ;
    UNIQUE = 'unique' ;
    UPDATE = 'update' ;
    UPDATETEXT = 'updatetext' ;
    UPDLOCK = 'updlock' ;
    USE = 'use' ;
    USER = 'user' ;
    VALUES = 'values' ;
    VARYING = 'varying' ;
    VIEW = 'view' ;
    VIEWS = 'views' ;
    WAITFOR = 'waitfor' ;
    WHEN = 'when' ;
    WHERE = 'where' ;
    WHILE = 'while' ;
    WITH = 'with' ;
    WRITETEXT = 'writetext' ;
    XLOCK = 'xlock' ;
    XML = 'xml' ;
    XMLDATA = 'xmldata' ;
    
    // system variables
    F_CONNECTIONS = '@@connections' ;
    F_CPU_BUSY = '@@cpu_busy' ;
    F_CURSOR_ROWS = '@@cursor_rows' ;
    F_DATEFIRST = '@@datefirst' ;
    F_DBTS = '@@dbts' ;
    F_ERROR = '@@error' ;
    F_FETCH_STATUS = '@@fetch_status' ;
    F_IDENTITY = '@@identity' ;
    F_IDLE = '@@idle' ;
    F_IO_BUSY = '@@io_busy' ;
    F_LANGID = '@@langid' ;
    F_LANGUAGE = '@@language' ;
    F_LOCK_TIMEOUT = '@@lock_timeout' ;
    F_MAX_CONNECTIONS = '@@max_connections' ;
    F_MAX_PRECISION = '@@max_precision' ;
    F_NESTLEVEL = '@@nestlevel' ;
    F_OPTIONS = '@@options' ;
    F_PACK_RECEIVED = '@@pack_received' ;
    F_PACK_SENT = '@@pack_sent' ;
    F_PACKET_ERRORS = '@@packet_errors' ;
    F_PROCID = '@@procid' ;
    F_REMSERVER = '@@remserver' ;
    F_ROWCOUNT = '@@rowcount' ;
    F_SERVERNAME = '@@servername' ;
    F_SERVICENAME = '@@servicename' ;
    F_SPID = '@@spid' ;
    F_TEXTSIZE = '@@textsize' ;
    F_TIMETICKS = '@@timeticks' ;
    F_TOTAL_ERRORS = '@@total_errors' ;
    F_TOTAL_READ = '@@total_read' ;
    F_TOTAL_WRITE = '@@total_write' ;
    F_TRANCOUNT = '@@trancount' ;
    F_VERSION = '@@version' ;
}


// starting rule
statement
    : selectStatement (SEMICOLON)? EOF
    ;

selectStatement
    :
    queryExpression
    (computeClause)?
    (forClause)?
    (optionClause)?
    ;

queryExpression
    : subQueryExpression (unionOperator subQueryExpression)* (orderByClause)?
    ;

subQueryExpression
    :
      querySpecification
    | LPAREN queryExpression RPAREN
    ;

querySpecification
    :
    selectClause
    (fromClause)?
    (whereClause)?
    (groupByClause (havingClause)? )?
    ;
        
selectClause
    : SELECT (ALL | DISTINCT)? (TOP Integer (PERCENT)? (WITH TIES)? )? selectList
    ;

whereClause
    : WHERE searchCondition
    ;

orderByClause
    : ORDER BY expression (ASC | DESC)? (COMMA expression (ASC | DESC)? )*
    ;

groupByClause
    : GROUP BY (ALL)? expression (COMMA expression)* (WITH (CUBE | ROLLUP) )?
    ;

havingClause
    : HAVING searchCondition
    ;

optionClause
    : OPTION LPAREN queryHint (COMMA queryHint)* RPAREN
    ;

queryHint
    :
      (HASH | ORDER) GROUP
    | (CONCAT | HASH | MERGE) UNION
    | (LOOP | MERGE | HASH) JOIN
    | FAST Integer
    | FORCE ORDER
    | MAXDOP Integer
    | ROBUST PLAN
    | KEEP PLAN
    | KEEPFIXED PLAN
    | EXPAND VIEWS
    ;

forClause
    :
    FOR (
      BROWSE
    | XML (RAW | AUTO | EXPLICIT) (COMMA XMLDATA)? (COMMA ELEMENTS)? (COMMA BINARY BASE64)
    )
    ;

computeClause
    :
    COMPUTE
    // only allowed functions are: AVG, COUNT, MAX, MIN, STDEV, STDEVP, VAR, VARP, SUM
    identifier LPAREN expression RPAREN
    (COMMA identifier LPAREN expression RPAREN)*
    (BY expression (COMMA expression)* )?
    ;

searchCondition
    : subSearchCondition ( (AND | OR) subSearchCondition )*
    ;

subSearchCondition
    :
    (NOT)? (
          (LPAREN searchCondition RPAREN) => LPAREN searchCondition RPAREN
        | predicate
        )
    ;

predicate
    :
    (
      expression (
        // expression comparisonOperator expression
        comparisonOperator (
            expression
          | (ALL | SOME | ANY) LPAREN selectStatement RPAREN
          )
      | IS (NOT)? NULL
      | (NOT)? (
            LIKE expression (ESCAPE expression)? // only single char
          | BETWEEN expression AND expression
          | IN LPAREN (
                (selectStatement) => selectStatement
              | expression (COMMA expression)*
              ) RPAREN
          )
      | CONTAINS LPAREN (dbObject | STAR) COMMA (stringLiteral | Variable) RPAREN
      | FREETEXT LPAREN (dbObject | STAR) COMMA (stringLiteral | Variable) RPAREN
      )
    | EXISTS LPAREN selectStatement RPAREN
    )
    ;

selectList
    : selectItem ( COMMA selectItem )*
    ;

selectItem
    :
      STAR // "*, *" is a valid select list
    | (
      // starts with: "alias = column_name"
      (alias2) => (
          (alias2 dbObject COMMA) => alias2 column
        | (alias2 dbObject (binaryOperator | LPAREN)) => alias2 expression
        | (alias2 column) => alias2 column
        | (alias2 expression) => alias2 expression
        )

      // all table columns: "table.*"
    | (tableColumns) => tableColumns
    
      // some shortcuts:
    | (dbObject (alias1)? COMMA) => column (alias1)?
    | (dbObject (binaryOperator | LPAREN) ) => expression (alias1)?
    
      // less obvious cases:
    | (column) => column (alias1)?
    | (expression) => expression (alias1)?
    )
    ;

fromClause
    : FROM tableSource (COMMA tableSource)*
    ;

tableSource
    : subTableSource (joinedTable)*
    ;

subTableSource
    :
    (
      LPAREN (
            (joinedTables) => joinedTables RPAREN
          | (queryExpression) => queryExpression RPAREN alias1 // "derived table", mandatory alias
          )
    | (function) => function (alias1)?
    | dbObject (alias1)? ( (WITH)? LPAREN tableHint (COMMA tableHint)* RPAREN )?
    | Variable (alias1)?
    | (CONTAINSTABLE | FREETEXTTABLE) LPAREN
        dbObject COMMA (dbObject | STAR) COMMA (stringLiteral | Variable) (COMMA Integer)?
        RPAREN (alias1)?
    | COLON COLON function (alias1)? // built-in function
    )
    ;

joinedTable
    :
      CROSS JOIN subTableSource
      // "joinHint JOIN" is invalid join expression
    | ( (INNER | (LEFT | RIGHT | FULL) (OUTER)? ) (joinHint)? )? JOIN tableSource ON searchCondition
    ;    

joinedTables
    : subTableSource (joinedTable)+
    ;

joinHint
    :
      LOOP
    | HASH
    | MERGE
    | REMOTE
    ;

tableHint
    :
      INDEX (
          LPAREN (identifier | Integer) ( COMMA (identifier | Integer) )* RPAREN
        | ASSIGNEQUAL identifier    // old index hint syntax
        )
    | FASTFIRSTROW
    | HOLDLOCK
    | NOLOCK
    | PAGLOCK
    | READCOMMITED
    | READPAST
    | READUNCOMMITED
    | REPEATABLEREAD
    | ROWLOCK
    | SERIALIZABLE
    | TABLOCK
    | TABLOCKX
    | UPDLOCK
    | XLOCK
    ;

collate
    : COLLATE identifier
    ;
    
alias1
    : // alias name can also be single-quoted literal (but not for table names)
    (AS)? (
          identifier
        | stringLiteral
        | keywordAsIdentifier
        )
    ;

alias2
    :
    (
      identifier
    | stringLiteral
    | keywordAsIdentifier
    )
    ASSIGNEQUAL
    ;
    
tableColumns
    :
    //o:
    dbObject DOT_STAR
    ;

column
    :
    (PLUS)* // "++column_name" is valid and updatable column name
    (
      dbObject
      // for expression like "(column)" SQL Server returns updatable column
    | LPAREN column RPAREN
    )
    (collate)? // it is not well documented but COLLATE can be used almost anywhere ...
    ;

expression
    : // current definition ignores operator precedence
      subExpression (binaryOperator subExpression)*
    ;

subExpression
    :
    (unaryOperator)?
    (
      constant
    | Variable
    | (function) => function
    | LPAREN (
          (selectStatement) => selectStatement // select statement returning a single value
        | expression
        ) RPAREN
    | dbObject    // column
    | parameterlessFunction
    | caseFunction
    | castFunction
    )
    (collate)?    // it is not well documented but COLLATE can be used almost everywhere ...
    ;

// todo: create a separate rule for aggregate functions
function
    : // LEFT and RIGHT keywords are also function names
    (dbObject | LEFT | RIGHT) LPAREN (
          expression (COMMA expression)*
        | STAR    // aggregate functions like Count(), Checksum() accept "*" as a parameter
        | (ALL | DISTINCT) (STAR | expression) // aggregate function
        | Variable ASSIGNEQUAL expression (COMMA Variable ASSIGNEQUAL expression)*
        )?
    RPAREN
    ;

caseFunction
    : CASE (
          expression (WHEN expression THEN expression)+
        | (WHEN searchCondition THEN expression)+    // boolean expression
        )
    (ELSE expression)? END
    ;

castFunction
    : CAST LPAREN expression AS identifier (LPAREN Integer (COMMA Integer)? RPAREN)? RPAREN
    ;

dbObject
    // server.catalog.schema.object
    // server.catalog..object
    :
    (identifier | IDENTITYCOL | ROWGUIDCOL | keywordAsIdentifier) (
          DOT (identifier | IDENTITYCOL | ROWGUIDCOL | keywordAsIdentifier)
        | (DOT DOT) => DOT DOT (identifier | IDENTITYCOL | ROWGUIDCOL | keywordAsIdentifier)
    )*
    ;

parameterlessFunction
    : // any others ?
      CURRENT_TIMESTAMP
    | CURRENT_USER
    | SESSION_USER
    | SYSTEM_USER
    ;

systemVariable
    :
      F_CONNECTIONS 
    | F_CPU_BUSY 
    | F_CURSOR_ROWS 
    | F_DATEFIRST 
    | F_DBTS 
    | F_ERROR 
    | F_FETCH_STATUS 
    | F_IDENTITY 
    | F_IDLE 
    | F_IO_BUSY 
    | F_LANGID 
    | F_LANGUAGE 
    | F_LOCK_TIMEOUT 
    | F_MAX_CONNECTIONS 
    | F_MAX_PRECISION 
    | F_NESTLEVEL 
    | F_OPTIONS 
    | F_PACK_RECEIVED 
    | F_PACK_SENT 
    | F_PACKET_ERRORS 
    | F_PROCID 
    | F_REMSERVER 
    | F_ROWCOUNT 
    | F_SERVERNAME 
    | F_SERVICENAME 
    | F_SPID 
    | F_TEXTSIZE 
    | F_TIMETICKS 
    | F_TOTAL_ERRORS 
    | F_TOTAL_READ 
    | F_TOTAL_WRITE 
    | F_TRANCOUNT 
    | F_VERSION
    ;    

keywordAsIdentifier
    :
    (
      AUTO
    | BASE64
    | BINARY
    | CAST
    | CONCAT
    | CUBE
    | ELEMENTS
    | EXPAND
    | EXPLICIT
    | FAST
    | FASTFIRSTROW
    | FORCE
    | HASH
    | KEEP
    | KEEPFIXED
    | LOOP
    | MAXDOP
    | MERGE
    | NOLOCK
    | PAGLOCK
    | RAW
    | READCOMMITED
    | READPAST
    | READUNCOMMITED
    | REMOTE
    | REPEATABLEREAD
    | ROBUST
    | ROLLUP
    | ROWLOCK
    | SERIALIZABLE
    | TABLOCK
    | TABLOCKX
    | TIES
    | UPDLOCK
    | VIEWS
    | XLOCK
    | XML
    | XMLDATA
    )
    ;

stringLiteral
    :
      UnicodeStringLiteral
    | ASCIIStringLiteral
    ;

identifier
    :
      NonQuotedIdentifier
    | QuotedIdentifier
    ;

constant
    : Integer | Real | NULL | stringLiteral | HexLiteral | Currency | ODBCDateTime | systemVariable
    ;

unaryOperator
    : PLUS | MINUS | TILDE
    ;
    
binaryOperator
    : arithmeticOperator | bitwiseOperator
    ;
    
arithmeticOperator
    : PLUS | MINUS | STAR | DIVIDE | MOD
    ;

bitwiseOperator
    : AMPERSAND | TILDE | BITWISEOR | BITWISEXOR
    ;

comparisonOperator
    :
      ASSIGNEQUAL | NOTEQUAL1 | NOTEQUAL2 | LESSTHANOREQUALTO1 | LESSTHANOREQUALTO2 
    | LESSTHAN | GREATERTHANOREQUALTO1 | GREATERTHANOREQUALTO2 | GREATERTHAN
    ;
        
logicalOperator
    : ALL | AND | ANY | BETWEEN | EXISTS | IN | LIKE | NOT | OR | SOME
    ;

unionOperator
    : UNION (ALL)?
    ;
    



// Operators

protected 
DOT : '.' ; // generated as a part of Number rule
COLON : ':' ;
COMMA : ',' ;
SEMICOLON : ';' ;

LPAREN : '(' ;
RPAREN : ')' ;
//LSQUARE : '[' ;
//RSQUARE : ']' ;

ASSIGNEQUAL : '=' ;
NOTEQUAL1 : '<>' ;
NOTEQUAL2 : '!=' ;
LESSTHANOREQUALTO1 : '<=' ;
LESSTHANOREQUALTO2 : '!>' ;
LESSTHAN : '<' ;
GREATERTHANOREQUALTO1 : '>=' ;
GREATERTHANOREQUALTO2 : '!<' ;
GREATERTHAN : '>' ;

DIVIDE : '/' ;
PLUS : '+' ;
MINUS : '-' ;
STAR : '*' ;
MOD : '%' ;

AMPERSAND : '&' ;
TILDE : '~' ;
BITWISEOR : '|' ;
BITWISEXOR : '^' ;
DOT_STAR : '.*' ;

Whitespace
    : (' ' | '\t' | '\n' | '\r')
    { $channel = HIDDEN; }
    ;

// COMMENTS
SingleLineComment
    : '--'( ~('\r' | '\n') )*
    { $channel = HIDDEN; }
    ;

MultiLineComment
    : '/*' (~'*')* '*' ('*' | ( ~('*' | '/') (~'*')* '*') )* '/'
    { $channel = HIDDEN; }
    ;

// LITERALS

protected
Letter
    : 'a'..'z' | '_' | '#' | '@' | '\u0080'..'\ufffe'
    ;

protected
Digit
    : '0'..'9'
    ;

protected
Integer 
    : (Digit)+
    ;

protected
Real 
    :
    ( (Digit)+ ('.' | 'e') ) => (Digit)+ ( '.' (Digit)* (Exponent)? | Exponent)
    ;

protected
Exponent
    : 'e' ( '+' | '-' )? (Digit)+
    ;

protected
HexLiteral // generated as a part of Number rule
    : // "0x" ('0'..'9' | 'a'..'f')*
    '0x' ('a'..'f' | Digit)*
    ;

Number
    :
      ( (Digit)+ ('.' | 'e') ) => (Digit)+ ( '.' (Digit)* (Exponent)? | Exponent) {  }
    | '.' {  } ( (Digit)+ (Exponent)? {  } )?
    | (Digit)+ {  }
    | '0x' ('a'..'f' | Digit)* {  } // "0x" is valid hex literal
    ;

protected
Currency
    : // generated as a part of NonQuotedIdentifier rule
    ('$' | '\u00a3'..'\u00a5' | '\u09f2'..'\u09f3' | '\u0e3f' | '\u20a0'..'\u20a4' | '\u20a6'..'\u20ab')
    ((Digit)+ ('.' (Digit)* )? | '.' (Digit)+)
    ;

ODBCDateTime
    : '{' (Whitespace)? ('ts' | 't' | 'd') (Whitespace)?
        ('n')? '\'' (~'\'')* '\'' ( '\'' (~'\'')* '\'' )* (Whitespace)? '}'
    ;

NonQuotedIdentifier
    //options { testLiterals = true; }
    :
      (Currency) => Currency {  }
    | ('a'..'z' | '_' | '#' | '\u0080'..'\ufffe') (Letter | Digit)* // first char other than '@'
    ;

QuotedIdentifier
    :
    (
      '[' (~']')* ']' (']' (~']')* ']')*
    | '"' (~'"')* '"' ('"' (~'"')* '"')*
    )
    ;
    
Variable
    // test for literals in case of a function begining with '@' (eg. "@@ERROR")
    //options { testLiterals = true; }
    : '@' (Letter | Digit)+
    ;
    
ASCIIStringLiteral
    :
    '\'' (~'\'')* '\'' ( '\'' (~'\'')* '\'' )*
    ;

UnicodeStringLiteral
    :
    'n' '\'' (~'\'')* '\'' ( '\'' (~'\'')* '\'' )*
    ;

fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');
// Numeric Constants