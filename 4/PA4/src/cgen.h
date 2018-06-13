#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <list>
enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   void code_prototype_objects();
   void code_class_name_table();
   void code_dispatch_table();
   void code_object_initializers();
   void code_class_methods();
// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

   void build_feature_map();
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   CgenNodeP lookup_tag(int tag);
   CgenNodeP lookup_name(Symbol name);
};

class ObjectLocation {
  private:
    char *reg;
    int offset;
  public:
    ObjectLocation(char *reg_, int offset_) {
      reg= reg_;
      offset = offset_;
    };
    char * get_register() { return reg; };
    int get_offset() { return offset; };
};

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   std::list<Feature> *methods;
   std::list<Feature> *attrs;
   SymbolTable<Symbol, ObjectLocation> *vars;

   int tag;
   void set_tag();
public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   std::list<Feature> *get_methods() { return methods; }
   std::list<Feature> *get_attrs() { return attrs; }
   SymbolTable<Symbol, ObjectLocation> *get_vars() { return vars; }
   int get_tag() { return tag; }

   void build_feature_map();
   void code_prototype_object(ostream& s);
   void code_class_name_table(ostream& s);
   void code_dispatch_table(ostream& s);
   void code_object_initializer(ostream& s);
   void code_class_methods(ostream& s);
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};


