#ifndef SEMANT_H_
#define SEMANT_H_
#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// class Graph {
// private:
// public:
// 	void insert(Class_ c1, Class_ c2){
// 		adjacency_matrix[c1][c2] = 1;
// 	}
// };

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph. You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods. 
class ClassTable {
private:
	int semant_errors;
	void install_basic_classes();
	ostream& error_stream;
	
	Classes classes_;
	Class_ cur_class_;

	// Graph inheritance_graph;
	// void get_inheritance_graph(Classes);
	SymbolTable<Symbol, Entry> topological_sort;

	ostream& report_inheritance_cycle_error(Class_ c);
	ostream& report_redefined_error(Class_ c);
	ostream& report_inheritance_undefined_error(Class_ c);

	bool is_cyclic(Class_ c1, Class_ c2);
	bool cyclic_inheritance(Class_ c, Classes classes);
	bool is_defined(Symbol s, Classes classes);
	bool has_been_defined(Class_ c, Classes classes, int k);
	bool is_basic_class(Class_ c);
	bool is_basic_class(Symbol name);

	void insert(Class_);
	void get_topological_sort(Classes);
	// void get_classes;
public:
	ClassTable(Classes);
	int errors() { return semant_errors; }
	ostream& semant_error();
	ostream& semant_error(Class_ c);
	ostream& semant_error(Symbol filename, tree_node *t);

	SymbolTable<Symbol, Entry> symbol_table;

	void set_current_class(Class_ c) {cur_class_ = c;}
	Class_ get_current_class() {return cur_class_;}
	Class_ get_class(Symbol name);

	method_class* get_method(Class_ c, Symbol name);
	void add_variables(Class_ c);
	bool is_subclass(Class_ c1, Class_ c2);
	Symbol get_lca(Symbol name1, Symbol name2, ClassTableP classtable);
};

#endif
