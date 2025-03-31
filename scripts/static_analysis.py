#!/usr/bin/env python3
import re
import sys
from pathlib import Path
from collections import defaultdict
import logging
import hashlib
import multiprocessing
from functools import partial
import json

class CustomFormatter(logging.Formatter):
    """Custom formatter to add emojis based on log levels."""
    EMOJIS = {
        logging.INFO: "",     # Info messages
        logging.WARNING: "",  # Warnings
        logging.ERROR: "",     # Errors
        logging.DEBUG: "",     # Debug messages
        logging.CRITICAL: ""   # Added critical level
    }
    
    def format(self, record):
        emoji = self.EMOJIS.get(record.levelno, "")
        record.msg = f"{emoji} {record.msg}"
        return super().format(record)


class MessageCollector(logging.Handler):
    """Custom handler that stores log messages for later display."""
    
    def __init__(self):
        super().__init__()
        self.messages = defaultdict(list)
        self.suppressed_hashes = set()
        self.raw_messages = defaultdict(list)  # Store raw messages by level
        
#       
    def emit(self, record):
        # Get the raw message (without emoji)
        raw_msg = record.getMessage()
        
        # Create a hash for this message
        msg_hash = hashlib.md5(raw_msg.encode()).hexdigest()
        
        # Store the message regardless of suppression status
        level_name = record.levelname
        formatted_msg = self.format(record)
        
        # Store raw message for potential future use
        self.raw_messages[level_name].append(raw_msg)
        
        # Store formatted message (with emoji) for display
        # Mark messages that are in the suppression file for the summary
        is_suppressed = msg_hash in self.suppressed_hashes
        self.messages[level_name].append((formatted_msg, is_suppressed))
        
    def get_summary(self):
        """Return a summary of all new messages (not in suppression file)."""
        summary = []
        
        # Count new messages (not suppressed)
        new_message_count = sum(
            len([msg for msg, suppressed in msgs if not suppressed]) 
            for msgs in self.messages.values()
        )
        
        if new_message_count == 0:
            return "No new issues found! All issues are in the suppression file."
            
        
        # Order by severity
        for level in ["CRITICAL", "ERROR", "WARNING"]:
            if level in self.messages:
                new_msgs = [msg for msg, suppressed in self.messages[level] if not suppressed]
                if new_msgs:
                    for msg in new_msgs:
                        summary.append(f"  {msg}")
                    
        return "\n".join(summary)


def fortranize_type(type_str):
    #removes "*" and "&" if they are the last character of the string
    new_type_str = type_str.rstrip("&")
    new_type_str = new_type_str.rstrip("*")
    #remove tailing '[]' from the type, only if they are at the end of the string
    new_type_str = new_type_str.rstrip("[]")
    #remove ", POINTER" from the name
    new_type_str = new_type_str.replace(", POINTER", "")
    if new_type_str == "CHARACTER(:,:)" or new_type_str == "CHARACTER(:)":
        new_type_str = "CHARACTER"
    return new_type_str

def fortranize_name(name):
    return name


# === Type Simplifier ===
def simplify_type(type_str):
    return type_str

def split_fir_arguments(arglist):
    """Split arguments at top-level commas, respecting nested brackets/braces."""
    return arglist 

def parse_argument(arg):
    return arg

def parse_call_arguments(type_list_str):
    return type_list_str

# Load JSON file
def load_json(filename):
    with open(filename, 'r') as f:
        return json.load(f)


# === Main Parser ===
def process_fir_file(fir_path: Path):
    if not fir_path.exists():
        logging.error(f"Error: File not found: {fir_path}")
        sys.exit(1)

    data = {}
    json_file = fir_path
    json_data = load_json(json_file)
    # Extract subroutine details
    for sub in json_data["subroutines"]:
        func_name = sub["name"]
        arg_names = [arg["name"] for arg in sub["arguments"]]
        arg_types = [arg["type"] for arg in sub["arguments"]]
        
        data[func_name] = {
            "ArgNames": arg_names,
            "ArgTypes": arg_types,
            "Callees": defaultdict(dict)
        }
    
    call_counters = defaultdict(lambda: defaultdict(int))
    # Extract call details
    for call in json_data["calls"]:
        caller = call["caller"]
        callee = call["callee"]
        line = call["line"]
        
        arg_names = [arg["name"] for arg in call["arguments"]]
        arg_types = [arg["type"] for arg in call["arguments"]]

        if caller in data:
            counter = call_counters[caller][callee]
            data[caller]["Callees"][callee][counter] = {
                    "ArgNames": arg_names,
                    "ArgTypes": arg_types,
                    "Line": line
            }
            call_counters[caller][callee] += 1
        else:
            logging.error(f"Caller {caller} of {callee} not found in data!")





    return data

def find_object_files(directory, common_dir):
    """Recursively find all .o files in the given directory."""
    return list(Path(directory).rglob("*.json")) + list(Path(common_dir).rglob("*.json"))

def process_file(obj_file, process_fir_file_func):
    try:
        logging.info(f"Parsing {obj_file} ...")
        return process_fir_file_func(obj_file)
    except Exception as e:
        logging.error(f"Error processing {obj_file}: {e}")
        return {}


if __name__ == "__main__":

    root_logger = logging.getLogger()
    root_logger.setLevel(logging.WARNING)
    
    # Console handler for immediate display
    formatter = CustomFormatter("%(levelname)s: %(message)s")

    console = logging.StreamHandler()
    console.setFormatter(formatter)
    root_logger.addHandler(console)
    
    # Create our collector for summary
    collector = MessageCollector()
    collector.setFormatter(formatter)   
    root_logger.addHandler(collector)
        
    
    handler = logging.StreamHandler()
    logging.basicConfig(level=logging.ERROR, handlers=[handler])

    # Set the root directory where .o files are stored
    # if argument is "starter", use the starter directory
    software = "starter"
    if len(sys.argv) > 1 and sys.argv[1] == "engine":
        root_dir = "../engine/"
        software = "engine"
    else: #if argument is not starter, then it is "engine"
        root_dir = "../starter/"
        software = "starter"


    common_dir= "../common_source/"

    #check if root_dir exists
    if not Path(root_dir).exists():
        logging.critical(f"Error: Directory not found: {root_dir}")
        exit(1)
    # root_dir = "./"

    logging.info(f"Searching for .o files in {root_dir}...")
    object_files = find_object_files(root_dir, common_dir)
    if not object_files:
        logging.critical("No .fir files found.")
        exit(1)
    #count the number of .fir files
    print(f"Number of .fir files found: {len(object_files)}")

    call_data = dict()


    num_processes = multiprocessing.cpu_count() 
    num_processes = min(4, num_processes)  # Limit to 4 processes, because it's mostly i/o
    pool = multiprocessing.Pool(processes=num_processes)

    # Create a partial function that already knows about process_fir_file
    process_func = partial(process_file, process_fir_file_func=process_fir_file)
    
    # Process files in parallel and get results
    results = pool.map(process_func, object_files)
    
    # Close the pool to free resources
    pool.close()
    pool.join()
    
    # Combine the results into the final call_data dictionary
    call_data = dict()
    for call_data_local in results:
        for fname, info in call_data_local.items():
            if fname in call_data:
                call_data[fname]["Callees"].update(info["Callees"])
            else:
                call_data[fname] = info



    # compile regular expressions for normalizing function names
    #remove fir.array<?x
    for fname, info in call_data.items():
        normalized_fname = fortranize_name(fname)

        logging.info(f"\nFunction: {normalized_fname}")
        for i, (t, n) in enumerate(zip(info["ArgTypes"], info["ArgNames"])):
            logging.info(f"    arg[{i}]: {t}  name: {n}")

        if info["Callees"]:
            logging.info("  Calls:")
            for callee, instances in info["Callees"].items():
                normalized_callee = fortranize_name(callee)
                # skip if callee or caller starts with "_"
                if normalized_callee.startswith("_") or normalized_fname.startswith("_"):
                    continue
#               if "operator" in normalized_callee or "operator" in normalized_fname:
#                   continue
#               if "emplace_back" == normalized_callee or "emplace_back" == normalized_fname:
#                   continue
#               if "push_back" == normalized_callee or "push_back" == normalized_fname:
#                   continue
#               if "pop_back" == normalized_callee or "pop_back" == normalized_fname:
#                   continue
#               if "insert" == normalized_callee or "insert" == normalized_fname:
#                   continue
#               if "forward" == normalized_callee or "forward" == normalized_fname:
#                   continue
#               if "construct" == normalized_callee or "construct" == normalized_fname:
#                   continue
#               if 'regex_replace' == normalized_callee or 'regex_replace' == normalized_fname:
#                   continue
#               if "empty" == normalized_callee or "empty" == normalized_fname:
#                   continue
#               if "format" == normalized_callee or "format" == normalized_fname:
#                   continue
#               if "lenghth" == normalized_callee or "length" == normalized_fname:
#                   continue
#               if "end" == normalized_callee or "end" == normalized_fname:
#                   continue
#               if "begin" == normalized_callee or "begin" == normalized_fname:
#                   continue
#               if "destroy" == normalized_callee or "destroy" == normalized_fname:
#                   continue
#               if "allocate" == normalized_callee or "allocate" == normalized_fname:
#                   continue
#               if "deallocate" == normalized_callee or "deallocate" == normalized_fname:
#                   continue
#               if "size" == normalized_callee or "size" == normalized_fname:
#                   continue
#               if "swap" == normalized_callee or "swap" == normalized_fname:
#                   continue
#               if "clear" == normalized_callee or "clear" == normalized_fname:
#                   continue
#               if "find" == normalized_callee or "find" == normalized_fname:
#                   continue
#               if "at" == normalized_callee or "at" == normalized_fname:
#                   continue
#               if "reserve" == normalized_callee or "reserve" == normalized_fname:
#                   continue
#               if "stlsort" == normalized_callee or "stlsort" == normalized_fname:
#                   continue
#               if "max_size" == normalized_callee or "max_size" == normalized_fname:
#                   continue
#               if "reader" == normalized_callee or "reader" == normalized_fname:
#                   continue
#               if "readr" == normalized_callee or "readr" == normalized_fname:
#                   continue
#               if "writer" == normalized_callee or "writer" == normalized_fname:
#                   continue
#               if "str" == normalized_callee or "str" == normalized_fname:
#                   continue
#               if "back" == normalized_callee or "back" == normalized_fname:
#                   continue
#               if "key_comp" == normalized_callee or "key_comp" == normalized_fname:
#                   continue

                for callnum, callinfo in instances.items():
                    logging.info(f"    -> {normalized_callee} (call #{callnum}):")
                    for i, t in enumerate(callinfo["ArgTypes"]):
                        logging.info(f"         arg[{i}]: {t}")
                        #checks if the arguments of the Call are the same as the arguments of the function
                        #check if info["ArgTypes"] == call_data[callee]["ArgTypes"]
                    callee_name = callee
                    #check if callee_name is in the call_data
                    if callee_name in call_data:
                      #same number of arguments
                        if callinfo["ArgTypes"] == call_data[callee_name]["ArgTypes"]:
                            logging.info(f"Call {normalized_callee} from {normalized_fname} : OK")
                        elif len(callinfo["ArgTypes"]) != len(call_data[callee_name]["ArgTypes"]):
                            logging.critical(f"NNUMBER OF ARGUMENTS DO NOT MATCH {normalized_fname}->{callnum}:{normalized_callee} {len(callinfo['ArgTypes'])} != {len(call_data[callee_name]['ArgTypes'])}")
                        else:
                            for i, (arg1, arg2) in enumerate(zip(callinfo["ArgTypes"], call_data[callee_name]["ArgTypes"])):
                                arg1s = arg1 
                                arg2s = arg2
                                fortran_arg1 = fortranize_type(arg1s)
                                fortran_arg2 = fortranize_type(arg2s)
                                #remove "(:)" from the types
                                fortran_arg1_base = fortran_arg1.replace("(:)", "")
                                fortran_arg2_base = fortran_arg2.replace("(:)", "")
                                if fortran_arg1_base != fortran_arg2_base and not fortran_arg1 == "VOID" and not fortran_arg2 == "VOID":
                                    logging.critical(f"TYPE MISMATCH {normalized_fname}->{callnum}:{normalized_callee}({call_data[callee_name]['ArgNames'][i]}) {fortran_arg2} != {fortran_arg1}")
                                elif fortran_arg1_base == fortran_arg2_base and fortran_arg1 != fortran_arg2 and not fortran_arg1 == "VOID" and not fortran_arg2 == "VOID":
                                    logging.warning(f"ARRAY VS SCALAR {normalized_fname}->{callnum}:{normalized_callee}({call_data[callee_name]['ArgNames'][i]}) {fortran_arg2} != {fortran_arg1}")

                    else:
                        logging.info(f"callee {callee_name} not found in the call_data")
                        
    # Display the summary
    summary = collector.get_summary()
    #size of summary
    #filename is ../starter/static_analysis.log
    #         or ../engine/static_analysis.log
    fname = f"{software}_static_analysis.log"
    print(f"Writing summary to {fname}...")
    with open(fname, "w") as f:
        #write the summary to the file
        f.write(summary)

    #checks if fname exists
    print(f"Checking if {fname} exists...")
    if Path(fname).exists():
        print(f"{fname} exists")
    else:
        print(f"{fname} does not exist")

    #open static_analysis.log and static_analysis.supp simultaneously

    list_of_hashes = []
    # dictionnary "hash" : "message"
    new_errors = {}
    #if file exists
    if Path(fname).exists():
        with open(fname, "r") as f:
            for line in f:
                # if there is no "\n" at the end of the line, add it
                if not line.endswith("\n"):
                    line = line + "\n"
                message_hash = hashlib.md5(re.sub(r'\s+', '', line).encode()).hexdigest()
                list_of_hashes.append(message_hash)
                new_errors[message_hash] = line

    #print the size of the list of hashes
    print(f"Number of errors in the log file: {len(list_of_hashes)}")


    old_errors = {}
    list_of_old_hashes = []
    fname = f"{software}_static_analysis.supp"
    with open(fname, "r") as f:
        for line in f:
            if not line.endswith("\n"):
                line = line + "\n"
            message_hash = hashlib.md5(re.sub(r'\s+', '', line).encode()).hexdigest()
            list_of_old_hashes.append(message_hash)
            old_errors[message_hash] = line


    print(f"Number of errors in the suppression file: {len(list_of_old_hashes)}")

    print("\n\n")
    print("=== SUMMARY ===")
    count_new_errors = 0 
    #compare the two lists
    for hash in list_of_hashes:
        if hash not in list_of_old_hashes:
            count_new_errors += 1
            print("New issue found: ", hash)
            print(new_errors[hash])
#       else:
#           print("Issue already known: ")
#           print(new_errors[hash])
#           print(old_errors[hash])

    print("Total number of new issues: ", count_new_errors)
