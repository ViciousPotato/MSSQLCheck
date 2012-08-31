parser grammar SQLCheck;

options
{
    language=Java; // get to C# when ready
    tokenVocab=sql2003Lexer;
}

identifier   	   
	:  
	/*( non_reserved_word )=> non_reserved_word |*/ Regular_Identifier | Unicode_Identifier;
	
table_name                       			 
	:
	( MODULE Period identifier
	| identifier  ( Period  identifier (  Period  identifier )? )?
	)
	;

query_specification
	:  SELECT ( set_quantifier  )? select_list  table_expression 
	;
	
set_quantifier   
	:  DISTINCT | ALL
	;

select_list
	:	Asterisk
	|	select_sublist  ( Comma select_sublist  )*
	;


select_sublist
	:	derived_column
	|	qualified_asterisk 
	;


derived_column // doesn't handle table.column
	:	value_expression
		( as_clause  )?
	;


as_clause
	:	( AS )? identifier 
	;

qualified_asterisk
	:	schema_name  Period Asterisk
	|	all_fields_reference
	;

schema_name // table.column                       				 
	:  identifier  ( Period  identifier )? 
	;
	
all_fields_reference // Concerning value_expression, ignored currently
	:		
	;

table_expression
	:	from_clause
		( where_clause  )?
		( group_by_clause  )?
		( having_clause  )?
		( window_clause  )?
		;

// from table1, table2, ...
from_clause
	:	FROM
		table_reference_list 
	;


table_reference_list
	:	table_reference
		( Comma table_reference  )*
	;


table_reference
	:	(	table_primary
		|	joined_table
		)
		( sample_clause )?
	;
		


// Value expression: the big part
// Simplified for the moment
value_expression
	:	Number Plus_Sign Number
	;
