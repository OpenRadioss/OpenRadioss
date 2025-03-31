#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <map>

// This is the first gcc header to be included
#include "gcc-plugin.h"
#include "plugin-version.h"
#include "tree-pass.h"
#include "context.h"
#include "function.h"
#include "tree.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "is-a.h"
#include "predict.h"
#include "basic-block.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "gimple-pretty-print.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"

// We must assert that this plugin is GPL compatible
int plugin_is_GPL_compatible;
static struct plugin_info my_gcc_plugin_info = { "1.0", "Fortran Signature Extractor" };

// Structures to hold function info
struct ArgumentInfo {
	std::string name;
	std::string type;
	std::string intent;  // IN, OUT, INOUT
	bool isOptional;
};

struct SubroutineInfo {
	std::string name;
	std::string sourceFile;
	int sourceLine;
	std::vector<ArgumentInfo> arguments;
};

struct CallInfo {
	std::string callerName;
	std::string calleeName;
	std::string sourceFile;
	int sourceLine;
	std::vector<std::string> arguments;
	std::vector<std::string> argumentTypes; // Add this to store types
};


// Containers for all collected information
std::vector<SubroutineInfo> allSubroutines;
std::vector<CallInfo> allCalls;


std::string getTypeName(tree type) {
    if (!type)
        return "unknown";
        
    enum tree_code code = TREE_CODE(type);
    
    // Handle arrays - recursively check dimensions
    if (code == ARRAY_TYPE) {
        // Get element type (potentially multi-dimensional)
        tree element_type = type;
        int dimensions = 0;
        
        // Count dimensions by traversing through nested ARRAY_TYPEs
        while (TREE_CODE(element_type) == ARRAY_TYPE) {
            dimensions++;
            element_type = TREE_TYPE(element_type);
        }
        
        // Get the base type name
        std::string base_type;
        
        // Special handling for CHARACTER arrays
        if (TREE_CODE(element_type) == INTEGER_TYPE && TYPE_STRING_FLAG(element_type)) {
            base_type = "CHARACTER";
        } else {
            // Recursive call for the element type, but without the array part
            base_type = getTypeName(element_type);
        }
        
        // Format with Fortran-style array dimensions
        std::string array_suffix = "(";
        for (int i = 0; i < dimensions; i++) {
            array_suffix += ":";
            if (i < dimensions - 1) {
                array_suffix += ",";
            }
        }
        array_suffix += ")";
        
        return base_type + array_suffix;
    }
    
    // Check for CHARACTER type
    if (code == INTEGER_TYPE && TYPE_STRING_FLAG(type)) {
        return "CHARACTER";
    }
    
    // Handle numeric types with KIND information
    switch (code) {
        case INTEGER_TYPE: {
            // Get precision in bits
            int precision = TYPE_PRECISION(type);
            int kind = precision / 8;  // Convert bits to bytes for Fortran KIND
            if (kind > 0) {
                return "INTEGER(KIND=" + std::to_string(kind) + ")";
            }
            return "INTEGER";
        }
        
        case REAL_TYPE: {
            // Get precision for REAL types
            int precision = TYPE_PRECISION(type);
            
            // Map precision to Fortran KIND
            int kind = 0;
            if (precision <= 32) kind = 4;       // Single precision
            else if (precision <= 64) kind = 8;  // Double precision
            else kind = 16;                      // Extended precision
            
            return "REAL(KIND=" + std::to_string(kind) + ")";
        }
        
        case COMPLEX_TYPE: {
            // Get precision of the component type
            tree component = TREE_TYPE(type);  // COMPLEX is made of two REALs
            int precision = TYPE_PRECISION(component);
            
            // Map precision to Fortran KIND
            int kind = 0;
            if (precision <= 32) kind = 4;
            else if (precision <= 64) kind = 8;
            else kind = 16;
            
            return "COMPLEX(KIND=" + std::to_string(kind) + ")";
        }
        
        case BOOLEAN_TYPE: return "LOGICAL";
        
        case POINTER_TYPE: {
            std::string pointed_type = getTypeName(TREE_TYPE(type));
            return pointed_type + ", POINTER";
        }
        
        case RECORD_TYPE: {
            // Enhanced handling for derived types
            tree name = TYPE_NAME(type);
            if (name) {
                if (TREE_CODE(name) == TYPE_DECL) {
                    if (DECL_NAME(name)) {
                        return "TYPE(" + std::string(IDENTIFIER_POINTER(DECL_NAME(name))) + ")";
                    }
                } else if (TREE_CODE(name) == IDENTIFIER_NODE) {
                    return "TYPE(" + std::string(IDENTIFIER_POINTER(name)) + ")";
                }
            }
            
            if (TYPE_IDENTIFIER(type)) {
                return "TYPE(" + std::string(IDENTIFIER_POINTER(TYPE_IDENTIFIER(type))) + ")";
            }
            
            return "DERIVED_TYPE";
        }
        
        case VOID_TYPE:
            return "VOID";
            
        case FUNCTION_TYPE:
            return "PROCEDURE";
            
        case REFERENCE_TYPE: {
            std::string ref_type = getTypeName(TREE_TYPE(type));
            return ref_type;
        }
        
        default: 
            // For any types we haven't explicitly handled, include the tree code name
            return std::string(get_tree_code_name(code));
    }
}

/*std::string getTypeName(tree type) {
    if (!type)
        return "unknown";
        
    enum tree_code code = TREE_CODE(type);
    
    // Handle arrays - recursively check dimensions
    if (code == ARRAY_TYPE) {
        // Get element type (potentially multi-dimensional)
        tree element_type = type;
        int dimensions = 0;
        
        // Count dimensions by traversing through nested ARRAY_TYPEs
        while (TREE_CODE(element_type) == ARRAY_TYPE) {
            dimensions++;
            element_type = TREE_TYPE(element_type);
        }
        
        // Get the base type name
        std::string base_type;
        
        // Special handling for CHARACTER arrays
        if (TREE_CODE(element_type) == INTEGER_TYPE && TYPE_STRING_FLAG(element_type)) {
            base_type = "CHARACTER";
        } else {
            // Recursive call for the element type, but without the array part
            base_type = getTypeName(element_type);
        }
        
        // Format with Fortran-style array dimensions
        std::string array_suffix = "(";
        for (int i = 0; i < dimensions; i++) {
            array_suffix += ":";
            if (i < dimensions - 1) {
                array_suffix += ",";
            }
        }
        array_suffix += ")";
        
        return base_type + array_suffix;
    }
    
    // Check for CHARACTER type
    if (code == INTEGER_TYPE && TYPE_STRING_FLAG(type)) {
        return "CHARACTER";
    }
    
    // Handle other types
    switch (code) {
        case INTEGER_TYPE: return "INTEGER";
        case REAL_TYPE: return "REAL";
        case COMPLEX_TYPE: return "COMPLEX";
        case BOOLEAN_TYPE: return "LOGICAL";
        case POINTER_TYPE: {
            std::string pointed_type = getTypeName(TREE_TYPE(type));
            return pointed_type + ", POINTER";
        }
        case RECORD_TYPE: {
            // Enhanced handling for derived types
            tree name = TYPE_NAME(type);
            if (name) {
                if (TREE_CODE(name) == TYPE_DECL) {
                    if (DECL_NAME(name)) {
                        return "TYPE(" + std::string(IDENTIFIER_POINTER(DECL_NAME(name))) + ")";
                    }
                } else if (TREE_CODE(name) == IDENTIFIER_NODE) {
                    return "TYPE(" + std::string(IDENTIFIER_POINTER(name)) + ")";
                }
            }
            
            if (TYPE_IDENTIFIER(type)) {
                return "TYPE(" + std::string(IDENTIFIER_POINTER(TYPE_IDENTIFIER(type))) + ")";
            }
            
            return "DERIVED_TYPE";
        }
        case VOID_TYPE:
            return "VOID";
        case FUNCTION_TYPE:
            return "PROCEDURE";
        case REFERENCE_TYPE: {
            std::string ref_type = getTypeName(TREE_TYPE(type));
            return ref_type;
        }
        default: 
            // For any types we haven't explicitly handled, include the tree code name
            return std::string(get_tree_code_name(code));
    }
} */

namespace
{
	const pass_data signature_extractor_pass_data =
	{
		GIMPLE_PASS,
		"signature_extractor_pass",  /* name */
		OPTGROUP_NONE,               /* optinfo_flags */
		TV_NONE,                     /* tv_id */
		PROP_gimple_any,             /* properties_required */
		0,                           /* properties_provided */
		0,                           /* properties_destroyed */
		0,                           /* todo_flags_start */
		0                            /* todo_flags_finish */
	};

	struct signature_extractor_pass : gimple_opt_pass
	{
		signature_extractor_pass(gcc::context *ctx)
			: gimple_opt_pass(signature_extractor_pass_data, ctx)
		{
		}

		virtual unsigned int execute(function *fun) override
		{
			// Extract subroutine signature
			tree fndecl = fun->decl;
			if (!fndecl || TREE_CODE(fndecl) != FUNCTION_DECL)
				return 0;

			const char *name_ptr = IDENTIFIER_POINTER(DECL_NAME(fndecl));
			if (!name_ptr)
				return 0;

			SubroutineInfo subroutine;
			subroutine.name = name_ptr;

			// Get source location
			location_t loc = DECL_SOURCE_LOCATION(fndecl);
			expanded_location xloc = expand_location(loc);

			subroutine.sourceFile = xloc.file ? xloc.file : "unknown";
			subroutine.sourceLine = xloc.line;

			// Process parameters
			for (tree param = DECL_ARGUMENTS(fndecl); param; param = DECL_CHAIN(param)) {
				ArgumentInfo arg;

				if (DECL_NAME(param))
					arg.name = IDENTIFIER_POINTER(DECL_NAME(param));
				else
					arg.name = "unnamed";

				arg.type = getTypeName(TREE_TYPE(param));

				// Check for Fortran-specific attributes in the TREE_TYPE
				// Note: This is a simplified approach - full intent extraction
				// may require examining additional Fortran-specific structures
				arg.intent = "unknown";  // Default
				arg.isOptional = false;  // Default

				// Simplified intent detection
				// This would need refinement for accurate Fortran intent matching
				if (TREE_READONLY(param))
					arg.intent = "IN";

				subroutine.arguments.push_back(arg);
			}

			allSubroutines.push_back(subroutine);

			// Extract call information
			gimple_seq gimple_body = fun->gimple_body;
			struct walk_stmt_info walk_stmt_info;
			memset(&walk_stmt_info, 0, sizeof(walk_stmt_info));
			walk_stmt_info.info = fun;  // Store current function for callback use

			walk_gimple_seq(gimple_body, callback_stmt, callback_op, &walk_stmt_info);

			return 0;
		}

		virtual signature_extractor_pass* clone() override
		{
			return this;
		}

		private:


		static tree callback_stmt(gimple_stmt_iterator *gsi, bool *handled_all_ops, struct walk_stmt_info *wi)
		{
			gimple* stmt = gsi_stmt(*gsi);
			location_t loc = gimple_location(stmt);
			function *current_fun = (function *)wi->info;

			// Check for call statements
			if (is_gimple_call(stmt)) {
				CallInfo call;

				// Get caller info
				call.callerName = IDENTIFIER_POINTER(DECL_NAME(current_fun->decl));
				call.sourceFile = LOCATION_FILE(loc) ? LOCATION_FILE(loc) : "unknown";
				call.sourceLine = LOCATION_LINE(loc);

				// Get callee info
				tree callee = gimple_call_fn(stmt);
				if (callee) {
					if (TREE_CODE(callee) == ADDR_EXPR) {
						tree fndecl = TREE_OPERAND(callee, 0);
						if (fndecl && TREE_CODE(fndecl) == FUNCTION_DECL && DECL_NAME(fndecl)) {
							call.calleeName = IDENTIFIER_POINTER(DECL_NAME(fndecl));
						}
					}
				}

				if (!call.calleeName.empty()) {
					// Get arguments
					unsigned int num_args = gimple_call_num_args(stmt);
					for (unsigned int i = 0; i < num_args; i++) {
						tree arg = gimple_call_arg(stmt, i);
						std::string arg_str = "<expr>";
						std::string arg_type = "unknown";

						// Extract type information - always available regardless of tree node type
						if (arg) {
							arg_type = getTypeName(TREE_TYPE(arg));
						}

						// For debugging - you can uncomment this to see the TREE_CODE for each argument
						// fprintf(stderr, "Arg %d has tree code: %s\n", i, get_tree_code_name(TREE_CODE(arg)));

						// Enhanced argument handling with deeper inspection
						switch (TREE_CODE(arg)) {
							case VAR_DECL:
							case PARM_DECL:
								if (DECL_NAME(arg)) {
									arg_str = IDENTIFIER_POINTER(DECL_NAME(arg));
								}
								break;

							case ADDR_EXPR: {
										// Handle Fortran pass-by-reference arguments
										tree op = TREE_OPERAND(arg, 0);
										if (op) {
											// Update the type to be the type of what's being pointed to
											arg_type = getTypeName(TREE_TYPE(op));

											if (TREE_CODE(op) == VAR_DECL || TREE_CODE(op) == PARM_DECL) {
												if (DECL_NAME(op)) {
													arg_str = IDENTIFIER_POINTER(DECL_NAME(op));
												}
											} else if (TREE_CODE(op) == ARRAY_REF) {
												tree array = TREE_OPERAND(op, 0);
												if (array && TREE_CODE(array) == VAR_DECL && DECL_NAME(array)) {
													arg_str = std::string(IDENTIFIER_POINTER(DECL_NAME(array))) + "()";
												}
											} else if (TREE_CODE(op) == COMPONENT_REF) {
												tree field = TREE_OPERAND(op, 1);
												if (field && DECL_NAME(field)) {
													tree object = TREE_OPERAND(op, 0);
													if (object && TREE_CODE(object) == VAR_DECL && DECL_NAME(object)) {
														arg_str = std::string(IDENTIFIER_POINTER(DECL_NAME(object))) + 
															"%" + IDENTIFIER_POINTER(DECL_NAME(field));
													}
												}
											}
										}
										break;
									}

							case SSA_NAME: {
									       // For SSA names, try multiple approaches
									       tree var = SSA_NAME_VAR(arg);
									       if (var && DECL_NAME(var)) {
										       arg_str = IDENTIFIER_POINTER(DECL_NAME(var));
									       } else {
										       // Try to get the definition statement
										       gimple *def_stmt = SSA_NAME_DEF_STMT(arg);
										       if (def_stmt) {
											       // Check statement type
											       if (gimple_code(def_stmt) == GIMPLE_ASSIGN) {
												       tree rhs = gimple_assign_rhs1(def_stmt);
												       if (rhs) {
													       if (TREE_CODE(rhs) == VAR_DECL || TREE_CODE(rhs) == PARM_DECL) {
														       if (DECL_NAME(rhs)) {
															       arg_str = IDENTIFIER_POINTER(DECL_NAME(rhs));
														       }
													       } else if (TREE_CODE(rhs) == SSA_NAME && SSA_NAME_VAR(rhs) && DECL_NAME(SSA_NAME_VAR(rhs))) {
														       arg_str = IDENTIFIER_POINTER(DECL_NAME(SSA_NAME_VAR(rhs)));
													       } else if (TREE_CODE(rhs) == INTEGER_CST) {
														       arg_str = std::to_string(TREE_INT_CST_LOW(rhs));
													       }
												       }
											       } else if (gimple_code(def_stmt) == GIMPLE_PHI) {
												       // For PHI nodes, try to get any of the arguments
												       unsigned n = gimple_phi_num_args(def_stmt);
												       for (unsigned j = 0; j < n; ++j) {
													       tree phi_arg = gimple_phi_arg_def(def_stmt, j);
													       if (phi_arg && TREE_CODE(phi_arg) == VAR_DECL && DECL_NAME(phi_arg)) {
														       arg_str = IDENTIFIER_POINTER(DECL_NAME(phi_arg));
														       break;
													       }
												       }
											       } else if (gimple_code(def_stmt) == GIMPLE_CALL) {
												       // For call results, indicate the function
												       tree fn = gimple_call_fn(def_stmt);
												       if (fn && TREE_CODE(fn) == ADDR_EXPR) {
													       tree fndecl = TREE_OPERAND(fn, 0);
													       if (fndecl && DECL_NAME(fndecl)) {
														       arg_str = std::string(IDENTIFIER_POINTER(DECL_NAME(fndecl))) + "_result";
													       }
												       }
											       }
										       }

										       // If we still couldn't get a name, use a generic identifier with the SSA version
										       if (arg_str == "<expr>") {
											       int version = SSA_NAME_VERSION(arg);
											       // Try to get at least part of the original name from the debug info
											       if (current_fun && current_fun->decl) {
												       const char *fun_name = IDENTIFIER_POINTER(DECL_NAME(current_fun->decl));
												       if (fun_name) {
													       if (i == 0) arg_str = "arg1";
													       else if (i == 1) arg_str = "arg2";
													       else if (i == 2) arg_str = "arg3";
													       else arg_str = "arg" + std::to_string(i+1);
												       }
											       }
										       }
									       }
									       break;
								       }

							case INTEGER_CST:
								       arg_str = std::to_string(TREE_INT_CST_LOW(arg));
								       break;

							case REAL_CST: {
									       // Try to get a more readable representation of real constants
									       char buf[64];
									       real_to_decimal(buf, &TREE_REAL_CST(arg), sizeof(buf), 0, 1);
									       arg_str = buf;
									       break;
								       }

							case STRING_CST:
								       arg_str = "\"" + std::string(TREE_STRING_POINTER(arg)) + "\"";
								       break;

							default:
								       // As a last resort for cases we haven't handled specifically,
								       // just use a position-based name
								       arg_str = "arg" + std::to_string(i+1);
						}

						call.arguments.push_back(arg_str);
						call.argumentTypes.push_back(arg_type);
					}

					allCalls.push_back(call);
				}
			}

			return NULL;
		}

		static tree callback_op(tree *t, int *, void *data)
		{
			// We're not doing anything with operands in this pass
			return NULL;
		}
	};
}

// Write all collected information to a JSON file
void writeToJson(const std::string& filename) {
	std::ofstream outFile(filename);

	outFile << "{\n";

	// Write subroutines
	outFile << "  \"subroutines\": [\n";
	for (size_t i = 0; i < allSubroutines.size(); i++) {
		const SubroutineInfo& sub = allSubroutines[i];

		outFile << "    {\n";
		outFile << "      \"name\": \"" << sub.name << "\",\n";
		outFile << "      \"file\": \"" << sub.sourceFile << "\",\n";
		outFile << "      \"line\": " << sub.sourceLine << ",\n";
		outFile << "      \"arguments\": [\n";

		for (size_t j = 0; j < sub.arguments.size(); j++) {
			const ArgumentInfo& arg = sub.arguments[j];

			outFile << "        {\n";
			outFile << "          \"name\": \"" << arg.name << "\",\n";
			outFile << "          \"type\": \"" << arg.type << "\",\n";
			outFile << "          \"intent\": \"" << arg.intent << "\",\n";
			outFile << "          \"optional\": " << (arg.isOptional ? "true" : "false") << "\n";
			outFile << "        }";

			if (j < sub.arguments.size() - 1)
				outFile << ",";
			outFile << "\n";
		}

		outFile << "      ]\n";
		outFile << "    }";

		if (i < allSubroutines.size() - 1)
			outFile << ",";
		outFile << "\n";
	}

	outFile << "  ],\n";

	// Write calls
	outFile << "  \"calls\": [\n";
	for (size_t i = 0; i < allCalls.size(); i++) {
		const CallInfo& call = allCalls[i];

		outFile << "    {\n";
		outFile << "      \"caller\": \"" << call.callerName << "\",\n";
		outFile << "      \"callee\": \"" << call.calleeName << "\",\n";
		outFile << "      \"file\": \"" << call.sourceFile << "\",\n";
		outFile << "      \"line\": " << call.sourceLine << ",\n";
		outFile << "      \"arguments\": [";


		for (size_t j = 0; j < call.arguments.size(); j++) {
			outFile << "        {\n";
			outFile << "          \"name\": \"" << call.arguments[j] << "\",\n";
			outFile << "          \"type\": \"" << call.argumentTypes[j] << "\"\n";
			outFile << "        }";

			if (j < call.arguments.size() - 1)
				outFile << ",";
			outFile << "\n";
		}


		outFile << "]\n";
		outFile << "    }";

		if (i < allCalls.size() - 1)
			outFile << ",";
		outFile << "\n";
	}

	outFile << "  ]\n";
	outFile << "}\n";

	outFile.close();
}

// Modify the plugin_init function:
int plugin_init(struct plugin_name_args *plugin_info,
               struct plugin_gcc_version *version)
{
    // Check version compatibility
    if (!plugin_default_version_check(version, &gcc_version)) {
        std::cerr << "This GCC plugin is for version " << GCCPLUGIN_VERSION_MAJOR << "." << GCCPLUGIN_VERSION_MINOR << "\n";
        return 1;
    }
    
    // We'll decide the output filename at the end of compilation
    // based on the file being processed
    
    register_callback(plugin_info->base_name,
            PLUGIN_INFO,
            NULL, 
            &my_gcc_plugin_info);
    
    // Register our pass
    struct register_pass_info pass_info;
    pass_info.pass = new signature_extractor_pass(g);
    pass_info.reference_pass_name = "cfg";
    pass_info.ref_pass_instance_number = 1;
    pass_info.pos_op = PASS_POS_INSERT_BEFORE;
    
    register_callback(plugin_info->base_name, 
                     PLUGIN_PASS_MANAGER_SETUP, 
                     NULL, 
                     &pass_info);
    
    // Register finish callback to write output file
    register_callback(plugin_info->base_name,
                     PLUGIN_FINISH,
                     [](void *gcc_data, void *user_data) {
                         // Get the main input filename
                         const char* input_filename = main_input_filename;
                         if (!input_filename || strlen(input_filename) == 0) {
                             // Fallback if main_input_filename is not available
                             writeToJson("fortran_signatures.json");
                             std::cout << "Wrote " << allSubroutines.size() << " subroutines and " 
                                      << allCalls.size() << " calls to fortran_signatures.json" << std::endl;
                             return;
                         }
                         
                         // Create output filename by adding .json extension
                         std::string output_file = std::string(input_filename) + ".json";
                         
                         // Write the output
                         writeToJson(output_file);
                         std::cout << "Wrote " << allSubroutines.size() << " subroutines and " 
                                  << allCalls.size() << " calls to " << output_file << std::endl;
                     },
                     NULL);
    
    return 0;
}
