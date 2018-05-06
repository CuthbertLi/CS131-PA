

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
	arg,
	arg2,
	Bool,
	concat,
	cool_abort,
	copy,
	Int,
	in_int,
	in_string,
	IO,
	length,
	Main,
	main_meth,
	No_class,
	No_type,
	Object,
	out_int,
	out_string,
	prim_slot,
	self,
	SELF_TYPE,
	Str,
	str_field,
	substr,
	type_name,
	val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
	arg         = idtable.add_string("arg");
	arg2        = idtable.add_string("arg2");
	Bool        = idtable.add_string("Bool");
	concat      = idtable.add_string("concat");
	cool_abort  = idtable.add_string("abort");
	copy        = idtable.add_string("copy");
	Int         = idtable.add_string("Int");
	in_int      = idtable.add_string("in_int");
	in_string   = idtable.add_string("in_string");
	IO          = idtable.add_string("IO");
	length      = idtable.add_string("length");
	Main        = idtable.add_string("Main");
	main_meth   = idtable.add_string("main");
	//   _no_class is a symbol that can't be the name of any 
	//   user-defined class.
	No_class    = idtable.add_string("_no_class");
	No_type     = idtable.add_string("_no_type");
	Object      = idtable.add_string("Object");
	out_int     = idtable.add_string("out_int");
	out_string  = idtable.add_string("out_string");
	prim_slot   = idtable.add_string("_prim_slot");
	self        = idtable.add_string("self");
	SELF_TYPE   = idtable.add_string("SELF_TYPE");
	Str         = idtable.add_string("String");
	str_field   = idtable.add_string("_str_field");
	substr      = idtable.add_string("substr");
	type_name   = idtable.add_string("type_name");
	val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

	/* Fill this in */

}

void ClassTable::install_basic_classes() {

	// The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
	Symbol filename = stringtable.add_string("<basic class>");
	
	// The following demonstrates how to create dummy parse trees to
	// refer to basic Cool classes.  There's no need for method
	// bodies -- these are already built into the runtime system.
	
	// IMPORTANT: The results of the following expressions are
	// stored in local variables.  You will want to do something
	// with those variables at the end of this method to make this
	// code meaningful.

	// 
	// The Object class has no parent class. Its methods are
	//        abort() : Object    aborts the program
	//        type_name() : Str   returns a string representation of class name
	//        copy() : SELF_TYPE  returns a copy of the object
	//
	// There is no need for method bodies in the basic classes---these
	// are already built in to the runtime system.

	Class_ Object_class =
	class_(Object, 
		   No_class,
		   append_Features(
				   append_Features(
						   single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
						   single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
				   single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
		   filename);

	// 
	// The IO class inherits from Object. Its methods are
	//        out_string(Str) : SELF_TYPE       writes a string to the output
	//        out_int(Int) : SELF_TYPE            "    an int    "  "     "
	//        in_string() : Str                 reads a string from the input
	//        in_int() : Int                      "   an int     "  "     "
	//
	Class_ IO_class = 
	class_(IO, 
		   Object,
		   append_Features(
				   append_Features(
						   append_Features(
								   single_Features(method(out_string, single_Formals(formal(arg, Str)),
											  SELF_TYPE, no_expr())),
								   single_Features(method(out_int, single_Formals(formal(arg, Int)),
											  SELF_TYPE, no_expr()))),
						   single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
				   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
		   filename);  

	//
	// The Int class has no methods and only a single attribute, the
	// "val" for the integer. 
	//
	Class_ Int_class =
	class_(Int, 
		   Object,
		   single_Features(attr(val, prim_slot, no_expr())),
		   filename);

	//
	// Bool also has only the "val" slot.
	//
	Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

	//
	// The class Str has a number of slots and operations:
	//       val                                  the length of the string
	//       str_field                            the string itself
	//       length() : Int                       returns length of the string
	//       concat(arg: Str) : Str               performs string concatenation
	//       substr(arg: Int, arg2: Int): Str     substring selection
	//       
	Class_ Str_class =
	class_(Str, 
		   Object,
		   append_Features(
				   append_Features(
						   append_Features(
								   append_Features(
										   single_Features(attr(val, Int, no_expr())),
										   single_Features(attr(str_field, prim_slot, no_expr()))),
								   single_Features(method(length, nil_Formals(), Int, no_expr()))),
						   single_Features(method(concat, 
									  single_Formals(formal(arg, Str)),
									  Str, 
									  no_expr()))),
				   single_Features(method(substr, 
							  append_Formals(single_Formals(formal(arg, Int)), 
									 single_Formals(formal(arg2, Int))),
							  Str, 
							  no_expr()))),
		   filename);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
	return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
	error_stream << filename << ":" << t->get_line_number() << ": ";
	return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
	semant_errors++;                            
	return error_stream;
} 



/*   This is the entry point to the semantic checker.

	 Your checker should do the following two things:

	 1) Check that the program is semantically correct
	 2) Decorate the abstract syntax tree with type information
		by setting the `type' field in each Expression node.
		(see `tree.h')

	 You are free to first do 1), make sure you catch all semantic
	 errors. Part 2) can be done in a second stage, when you want
	 to build mycoolc.
 */

Class_ ClassTable::get_class(Symbol name) {
	if (name == SELF_TYPE || name == self)
		name = symbol_table.lookup(self);

	for (int i = classes_->first(); classes_->more(i); i = classes_->next(i)) {
		Class_ cur_class =classes_->nth(i);
		if (name == cur_class->get_name())
			return cur_class;
	}
}

void method_class::add(ClassTableP classtable) {
	SymbolTable<Symbol, Entry> temp;
	temp.enterscope();

	for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
		Formal_class *formal = formals->nth(i);

		if (temp.lookup(formal->get_name()) != NULL) {
			classtable->semant_error(classtable->get_current_class()) << "Name " << formal->get_name() << "already exist" << std::endl;
			return;
		}

		temp.addid(formal->get_name(), formal->get_name());
		formal->add(classtable);
	}
	
	temp.exitscope();
}

void attr_class::add(ClassTableP classtable) {
	if (classtable->symbol_table.lookup(name) != NULL) {
		classtable->semant_error(classtable->get_current_class()) << "Attribute " << name << "already exist" << std::endl;
		return;
	}

	classtable->symbol_table.addid(name, type_decl);
}

void formal_class::add(ClassTableP classtable) {
	if (name == self) {
		classtable->semant_error(classtable->get_current_class()) << "Formal " << name << "is used as name" << std::endl;
		return;
	}
	
	classtable->symbol_table.addid(name, type_decl);
}

void ClassTable::add_variables(Class_ c) {
    Features features = c->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)){
        attr_class *attr = dynamic_cast<attr_class*>(features->nth(i));
        if (attr == NULL) continue;
        attr->add(this);
    }

    Class_ cc = get_class(c->get_parent());
    if (cc != NULL) 
    {
        add_variables(cc);
    }
}


method_class* ClassTable::get_method(Class_ c, Symbol name) {
	for (int i = c->get_features()->first(); c->get_features()->more(i); i = c->get_features()->next(i)) {
		method_class *m = dynamic_cast<method_class*>(c->get_features()->nth(i));

		if (m == NULL) continue;

		if (m->get_name() == name) return m;
	}

	Class_ cc = get_class(c->get_parent());
	
	if (cc == NULL) return NULL;

	return get_method(cc, name);
}

bool ClassTable::is_subclass(Class_ c1, Class_ c2) {
	if (c1->get_name() == c2->get_name()) return true;

	Class_ c = get_class(c1->get_parent());

	if (c == NULL) return false;

	return is_subclass(c, c2);
}

Symbol ClassTable::get_lca(Symbol name1, Symbol name2, ClassTableP classtable) {
	if (name1 == name2)
		return name1;
	
	if (name1 == SELF_TYPE)
		name1 = get_current_class()->get_name();
	
	if (name2 == SELF_TYPE)
		name2 = get_current_class()->get_name();

	return classtable->get_lca(name1, name2, classtable);
}



void program_class::semant()
{
	initialize_constants();

	/* ClassTable constructor may do some semantic analysis */
	ClassTable *classtable = new ClassTable(classes);
	if (classtable->errors()) {
		cerr << "Compilation halted due to static semantic errors." << endl;
		exit(1);
	}
	/* some semantic analysis code may go here */
	classtable->symbol_table.enterscope();

	for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
		Class_ cur_class = classes->nth(i);
		Symbol name = cur_class->get_name();
		classtable->symbol_table.addid(name, name);
		cur_class->semant(classtable);		
	}

	classtable->symbol_table.exitscope();

	if (classtable->errors()) {
		cerr << "Compilation halted due to static semantic errors." << endl;
		exit(1);
	}
}

void class__class::semant(ClassTableP classtable) {
	if (parent == SELF_TYPE || classtable->get_class(parent) == NULL) {
		classtable->semant_error(classtable->get_current_class()) << "Invalid parent" << parent << std::endl;
		return;
	}

	classtable->symbol_table.enterscope();
	classtable->set_current_class(this);
	classtable->symbol_table.addid(self, classtable->get_current_class()->get_name());
	classtable->add_variables(this);

	for (int i = features->first(); features->more(i); i = features->next(i)) {
		Feature cur_feature = features->nth(i);
		cur_feature->semant(classtable);
	}

	classtable->symbol_table.exitscope();
}

void method_class::semant(ClassTableP classtable) {
	classtable->symbol_table.enterscope();
	add(classtable);

	Symbol type = expr->semant(classtable);

	method_class *parent_method = classtable->get_method(classtable->get_class(classtable->get_current_class()->get_parent()), name);

	Formals parent_formals = NULL;
	if (parent_method != NULL) parent_formals = parent_method->get_formals();

	if (return_type == SELF_TYPE && type != SELF_TYPE) {
		classtable->semant_error(classtable->get_current_class()) << "Return type error in method " << name << std::endl;
		classtable->symbol_table.exitscope();
		return;
	}
	
	if (type == SELF_TYPE)
		type = classtable->symbol_table.lookup(self);

	if (classtable->get_class(return_type) == NULL) {
		classtable->semant_error(classtable->get_current_class()) << "Return type undefined in method " << name << std::endl;
		classtable->symbol_table.exitscope();
		return;
	}

	if (!classtable->is_subclass(classtable->get_class(type), classtable->get_class(return_type))) {
		classtable->semant_error(classtable->get_current_class()) << "Return type error in method " << name << std::endl;
		classtable->symbol_table.exitscope();
		return;
	}

	if (parent_method && parent_method->get_formals()->len() != formals->len()) {
		classtable->semant_error(classtable->get_current_class()) << "Incompatible number of formal parameters in redefined method " << name << std::endl;
		classtable->symbol_table.exitscope();
		return;
	}

	for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
		if (formals->nth(i)->get_type() == SELF_TYPE) {
			classtable->semant_error(classtable->get_current_class()) << "Formal parameter has type SELF_TYPE" << std::endl;
			classtable->symbol_table.exitscope();
			return;
		}

		if (parent_method && parent_formals->nth(i)->get_type() != formals->nth(i)->get_type()) {
			classtable->semant_error(classtable->get_current_class()) << "Wrong parameter type in redefined method" << std::endl;
			classtable->symbol_table.exitscope();
			return;
		}
	}
}

void attr_class::semant(ClassTableP classtable) {
	if (name == self) {
		classtable->semant_error(classtable->get_current_class()) << "Keyword used as attribute name: " << name  << std::endl;
		return;
	}

	Symbol type = init->semant(classtable);

	if (type == No_type)
		return;
	
	if (!classtable->is_subclass(classtable->get_class(type), classtable->get_class(type_decl))) {
		classtable->semant_error(classtable->get_current_class()) << "Wrong type while initializing attribute " << name << std::endl;
		return;
	}
} 

Symbol branch_class::semant(ClassTableP classtable) {
	classtable->symbol_table.enterscope();
	classtable->symbol_table.addid(name, type_decl);

	Symbol type = expr->semant(classtable);

	classtable->symbol_table.exitscope();

	return type;
}

Symbol assign_class::semant(ClassTableP classtable) {
	Symbol type1 = classtable->symbol_table.lookup(name);

	if (type == NULL) {
		classtable->semant_error(classtable->get_current_class()) << "Undeclared: " << name  << std::endl;
		type = Object;
		return type;
	}

	Symbol type2 = expr->semant(classtable);

	if (type2 != type1) {
		classtable->semant_error(classtable->get_current_class()) << "Wrong type in assign expression" << std::endl;
		type = Object;
		return type;
	}

	type = type1;
	return type;
}

Symbol static_dispatch_class::semant(ClassTableP classtable) {
	Symbol type1 = expr->semant(classtable);
	Class_ c;

	if (type1 == No_type || type1 == SELF_TYPE)
		c = classtable->get_current_class();
	else
		c = classtable->get_class(type1);
	
	if (c == NULL) {
		classtable->semant_error(classtable->get_current_class()) << "Class is undefined: " << type_name << std::endl;
		type = Object;
		return type;
	}

	Class_ p = classtable->get_class(c->get_parent());

    if (!classtable->is_subclass(c, p)) {
        classtable->semant_error(classtable->get_current_class()) << "Static dispatch to non-parent class" << std::endl;
        type = Object;
        return Object;
    }

	method_class *m = classtable->get_method(p, name);

	if (m = NULL) {
		classtable->semant_error(classtable->get_current_class()) << "Method:  " << name << "is undefined in class: " << type1 << std::endl;
		type = Object;
		return type;
	}
	
	Formals formals_ = m->get_formals();
	if (actual->len() != formals_->len()) {
		classtable->semant_error(classtable->get_current_class()) << "Lenght of Actuals is not equal to the formals" << std::endl;
		type = Object;
		return type;
	}

	for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
		Symbol actual_type = actual->nth(i)->semant(classtable);
		Symbol formal_type = formals_->nth(i)->get_type();

		if (actual_type != formal_type && !classtable->is_subclass(classtable->get_class(actual_type), classtable->get_class(formal_type))) {
			classtable->semant_error(classtable->get_current_class()) << "Actuals does not conforms with the Formals" << std::endl;
			type = Object;
			return type;
		}
	}

	type = m->get_return_type();
	if (type == SELF_TYPE)
		type = type1;
	
	return type;
}

Symbol dispatch_class::semant(ClassTableP classtable) {
	Symbol type1 = expr->semant(classtable);
	Class_ c;

	if (type1 == No_type || type1 == SELF_TYPE)
		c = classtable->get_current_class();
	else
		c = classtable->get_class(type1);

	if (c == NULL) {
		classtable->semant_error(classtable->get_current_class()) << "Class is undefined: " << type_name << std::endl;
		type = Object;
		return type;
	}

	method_class *m = classtable->get_method(c, name);

	if (m = NULL) {
		classtable->semant_error(classtable->get_current_class()) << "Method:  " << name << "is undefined in class: " << type1 << std::endl;
		type = Object;
		return type;
	}

	Formals formals_ = m->get_formals();
	if (actual->len() != formals_->len()) {
		classtable->semant_error(classtable->get_current_class()) << "Lenght of Actuals is not equal to the formals" << std::endl;
		type = Object;
		return type;
	}

	for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
		Symbol actual_type = actual->nth(i)->semant(classtable);
		Symbol formal_type = formals_->nth(i)->get_type();

		if (actual_type != formal_type && !classtable->is_subclass(classtable->get_class(actual_type), classtable->get_class(formal_type))) {
			classtable->semant_error(classtable->get_current_class()) << "Actuals does not conforms with the Formals" << std::endl;
			type = Object;
			return type;
		}
	}

	type = m->get_return_type();
	if (type == SELF_TYPE)
		type = type1;
	
	return type;
}

Symbol cond_class::semant(ClassTableP classtable) {
	Symbol pred_type = pred->semant(classtable);
	Symbol then_type = then_exp->semant(classtable);
	Symbol else_type = else_exp->semant(classtable);

	if (pred_type != Bool) {
		classtable->semant_error(classtable->get_current_class()) << "Predictive expression type must be Bool" << std::endl;
		type = Object;
		return type;
	}

	type = classtable->get_lca(then_type, else_type, classtable);
	return type;
}

Symbol loop_class::semant(ClassTableP classtable) {
	Symbol pred_type = pred->semant(classtable);

	if (pred_type != Bool) {
		classtable->semant_error(classtable->get_current_class()) << "Predictive expression type must be Bool" << std::endl;
		type = Object;
		return type;
	}

	body->semant(classtable);

	type = Object;
	return type;
}

Symbol typcase_class::semant(ClassTableP classtable) {
	Symbol type1 = expr->semant(classtable);
	Symbol type2 = NULL;
	Symbol lca = NULL;

	SymbolTable<Symbol, Entry> temp;
	temp.enterscope();

	for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
		Symbol case_type = cases->nth(i)->get_type();
		type2 = cases->nth(i)->semant(classtable);

		if (temp.lookup(case_type) != NULL) {
			classtable->semant_error(classtable->get_current_class()) << "Branches type error" << std::endl;
			type = Object;
			return type;
		}
		
		temp.addid(case_type, case_type);

		if (lca == NULL) 
			lca = type2;
		
		lca = classtable->get_lca(lca, type2, classtable);
	}

	type = lca;
	return type;
}

Symbol block_class::semant(ClassTableP classtable) {
	for (int i = body->first(); body->more(i); i = body->next(i)) {
		type = body->nth(i)->semant(classtable);
	}
	return type;
}

Symbol let_class::semant(ClassTableP classtable) {
	if (identifier == self) {
		classtable->semant_error(classtable->get_current_class()) << "'Self' cannot be used in a let expression" << std::endl;
		type = Object;
		return type;
	}
	
	Symbol init_type = init->semant(classtable);

	if (init_type != No_type && init_type != type_decl) {
		classtable->semant_error(classtable->get_current_class()) << "Type error while initializing let exression" << std::endl;
		type = Object;
		return type;
	}

	classtable->symbol_table.enterscope();
	classtable->symbol_table.addid(identifier, type_decl);

	type = body->semant(classtable);

	classtable->symbol_table.exitscope();

	return type;

}

Symbol plus_class::semant(ClassTableP classtable) {
	Symbol e1_type = e1->semant(classtable);
	Symbol e2_type = e2->semant(classtable);

	if (e1_type != Int || e2_type != Int) {
		classtable->semant_error(classtable->get_current_class()) << "Arithmetical operators only accept Int" << std::endl;
		type = Object;
		return type;
	}

	type = Int;
	return type;
}

Symbol sub_class::semant(ClassTableP classtable) {
	Symbol e1_type = e1->semant(classtable);
	Symbol e2_type = e2->semant(classtable);

	if (e1_type != Int || e2_type != Int) {
		classtable->semant_error(classtable->get_current_class()) << "Arithmetical operators only accept Int" << std::endl;
		type = Object;
		return type;
	}

	type = Int;
	return type;
}

Symbol mul_class::semant(ClassTableP classtable) {
	Symbol e1_type = e1->semant(classtable);
	Symbol e2_type = e2->semant(classtable);

	if (e1_type != Int || e2_type != Int) {
		classtable->semant_error(classtable->get_current_class()) << "Arithmetical operators only accept Int" << std::endl;
		type = Object;
		return type;
	}

	type = Int;
	return type;
}

Symbol divide_class::semant(ClassTableP classtable) {
	Symbol e1_type = e1->semant(classtable);
	Symbol e2_type = e2->semant(classtable);

	if (e1_type != Int || e2_type != Int) {
		classtable->semant_error(classtable->get_current_class()) << "Arithmetical operators only accept Int" << std::endl;
		type = Object;
		return type;
	}

	type = Int;
	return type;
}

Symbol neg_class::semant(ClassTableP classtable) {
	if (e1->semant(classtable) != Int) {
		classtable->semant_error(classtable->get_current_class()) << "Negative operator only accepts Int" << std::endl;
		type = Object;
		return type;
	}

	type = Int;
	return type;
}

Symbol lt_class::semant(ClassTableP classtable) {
	Symbol e1_type = e1->semant(classtable);
	Symbol e2_type = e2->semant(classtable);

	if (e1_type != Int || e2_type != Int) {
		classtable->semant_error(classtable->get_current_class()) << "Less-than operator only accepts Int" << std::endl;
		type = Object;
		return type;
	}

	type = Bool;
	return type;
}

Symbol eq_class::semant(ClassTableP classtable) {
	Symbol e1_type = e1->semant(classtable);
	Symbol e2_type = e2->semant(classtable);

	if ((e1_type == Int && e2_type != Int) || (e1_type == Bool && e2_type != Bool) || (e1_type == Str && e2_type != Str)) {
		classtable->semant_error(classtable->get_current_class()) << "Compare operator only accepts Int, Bool or Str" << std::endl;
		type = Object;
		return type;
	}

	type = Bool;
	return type;
}

Symbol leq_class::semant(ClassTableP classtable) {
	Symbol e1_type = e1->semant(classtable);
	Symbol e2_type = e2->semant(classtable);

	if (e1_type != Int || e2_type != Int) {
		classtable->semant_error(classtable->get_current_class()) << "Less-equal operator only accepts Int" << std::endl;
		type = Object;
		return type;
	}

	type = Bool;
	return type;
}

Symbol comp_class::semant(ClassTableP classtable) {
	Symbol expr_type = e1->semant(classtable);

	if (type != Bool) {
		classtable->semant_error(classtable->get_current_class()) << "Not expression only accecpt Bool" << std::endl;
		type = Object;
		return type;
	}

	type = Bool;
	return type;
}

Symbol int_const_class::semant(ClassTableP classtable) {
	type = Int;
	return type;
}

Symbol string_const_class::semant(ClassTableP classtable) {
	type = Str;
	return type;
}

Symbol new__class::semant(ClassTableP classtable) {
	if (classtable->get_class(type_name) == NULL) {
		classtable->semant_error(classtable->get_current_class()) << "Undefined type for new expression" << std::endl;
		type = Object;
		return type;
	}

	type = type_name;
	return type;
}

Symbol isvoid_class::semant(ClassTableP classtable) {
	e1->semant(classtable);
	type = Bool;
	return type;
}

Symbol no_expr_class::semant(ClassTableP classtable) {
	type = No_type;
	return type;
}

Symbol object_class::semant(ClassTableP classtable) {
	if (name == self) {
		type = SELF_TYPE;
		return type;
	}

	type = classtable->symbol_table.lookup(name);

	if (type == NULL) {
		classtable->semant_error(classtable->get_current_class()) << "Undefined identifier: " << name << std::endl;
		type = Object;
	}

	return type;
}
