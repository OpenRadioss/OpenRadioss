#install the following packages 
#pip install subprocess
#pip install flashtext
#run from the script directory, run "python headers.py" to update all the files.
#Compile the code, add and commit the changes
import os
import re
from enum import Enum
import subprocess
from flashtext import KeywordProcessor


debug = False
debug_name = 'i22aera'


def get_encoding(file_path):
    result = subprocess.run(['file', '--mime-encoding', file_path], capture_output=True, text=True)
    encoding = result.stdout.split(': ')[1].strip()
    return encoding
class Subroutine:
    def __init__(self, name, path):
        self.name = name.lower()
        self.path = path
        self.callees_name = []
        self.modules_name = []
        self.callers_name = []
        self.header = ""                                                                           
        self.is_fortran_function = False

    def add_callee(self, callee):
        #do not duplicate entries if already exists
        if(callee.lower() not in self.callees_name):
            self.callees_name.append(callee.lower())
            if debug and self.name == debug_name:
                print(f'Adding callee {callee} to {self.name}')
            if debug and callee.lower() == debug_name:
                print(f'Adding caller {self.name} to {callee}')
    def add_module(self, module):
        if(module.lower() not in self.modules_name):
            self.modules_name.append(module.lower())
            if debug and self.name == debug_name:
                print(f'Adding module {module} to {self.name}')

    def add_caller(self, caller):
        if(caller.lower() not in self.callers_name):
            self.callers_name.append(caller.lower())
            if debug and self.name == debug_name:
                print(f'Adding caller {caller} to {self.name}')
            if debug and caller.lower() == debug_name:
                print(f'Adding callee {self.name} to {caller}')

    def print(self):
        print(f'{self.name} in {self.path}')
        print(f'Called by: {self.callers_name}')
        print(f'Calls: {self.callees_name}')
        print(f'Uses: {self.modules_name}')

    def fill_callers(self, subroutines):
        for callee in self.callees_name:
            if callee in subroutines:
                subroutines[callee].add_caller(self.name)
        for module in self.modules_name:
            if module in subroutines:
                subroutines[module].add_caller(self.name)

    def create_header(self,subroutines):
        #if subroutine is *.c or *.cpp, do not create header
        if self.path.endswith('.c') or self.path.endswith('.cpp'):
            return
        max_len = len(self.name)
        #if self.callees_name is not empty
        if self.callees_name:
            max_len = max(max_len, max([len(sub) for sub in self.callees_name]))
        if self.callers_name:
            max_len = max(max_len, max([len(sub) for sub in self.callers_name]))
        if self.modules_name:
            max_len = max(max_len, max([len(sub) for sub in self.modules_name]))
        self.header = f'      !||====================================================================\n'
        self.header += f'      !||    {self.name.ljust(max_len)}   {self.path}\n'
        if self.callers_name:
            self.header += f'      !||--- called by ------------------------------------------------------\n'
        elif debug : # warning about dead code
            print(f'Warning: Subroutine {self.name} in {self.path} is dead')

        # loop over all callers by alphabetical order
        for caller in sorted(self.callers_name):
            #check if caller is in subroutines
            if caller in subroutines and caller != self.name:
                path=subroutines[caller].path
                self.header += f'      !||    {caller.ljust(max_len)}   {path}\n'
            else:
                pass
                #self.header += f'      !||    {caller.ljust(max_len)}   \n'
        #if callees_name is not empty
        if self.callees_name:
            self.header += f'      !||--- calls      -----------------------------------------------------\n'
             
            for callee in sorted(self.callees_name):
                if(callee in subroutines) and callee != self.name:
                    path=subroutines[callee].path
                    self.header += f'      !||    {callee.ljust(max_len)}   {path}\n'
                else:   
                    #self.header += f'      !||    {callee.ljust(max_len)}   \n'
                    pass
        # if modules_name is not empty
        if self.modules_name:
            self.header += f'      !||--- uses       -----------------------------------------------------\n' 
            for module in sorted(self.modules_name):
                if(module in subroutines):
                    path=subroutines[module].path
                    self.header += f'      !||    {module.ljust(max_len)}   {path}\n'
                else:
                    pass
                    #self.header += f'      !||    {module.ljust(max_len)}\n'
        self.header += f'      !||====================================================================\n'

class CodeAnalyzer:
    def __init__(self):
        #fill callees
        self.starter_subroutines = self.parse_directory('../starter')
        self.engine_subroutines = self.parse_directory('../engine')
        self.common_source_subroutines = self.parse_directory('../common_source')
        self.parse_directory_function_calls('../starter',self.starter_subroutines)
        self.parse_directory_function_calls('../engine',self.engine_subroutines)
        self.parse_directory_function_calls('../common_source',self.common_source_subroutines)
        # in order to fill also the callers for the function calls, we need to parse again the files
        # looking for function calls with the "subroutine" names found before
        #fill callers
        self.fill_callers()
        self.create_headers()
        self.update_headers('../starter')
        self.update_headers('../engine')
        self.update_headers('../common_source')


    # write a copy of the .F and .F90 files (adding suffix .new_header) with the headers for all subroutines and modules
    def update_headers(self, path):
        subroutine_regex = re.compile(r'^\s*subroutine\s+(\w+)', re.IGNORECASE)
        recursive_subroutine_regex=re.compile(r'^\s*recursive\s+subroutine\s+(\w+)', re.IGNORECASE)
        function_regex = re.compile(r'^\s*function\s+(\w+)', re.IGNORECASE)
        function2_regex = re.compile(r'^\s*\w+\s+function\s+(\w+)', re.IGNORECASE)
        function_end_regex = re.compile(r'^\s*END\s+function\s+\w+', re.IGNORECASE)
        program_regex = re.compile(r'^\s*program\s+(\w+)', re.IGNORECASE)
        module_regex = re.compile(r'^\s*module\s+(?!procedure\s)(\w+)', re.IGNORECASE)
        nocomment_regex = re.compile(r'^ {4}.*')
        begin_interface_regex = re.compile(r'^\s*interface\s*$', re.IGNORECASE)
        end_interface_regex = re.compile(r'^\s*end\s+interface\s*$', re.IGNORECASE)
        is_interface = False


        header_regex = re.compile(r'^\s*!\|\|', re.IGNORECASE)
        old_header_regex = re.compile(r'^Chd', re.IGNORECASE)
        for root, _, files in os.walk(path):
            # skip if the path contains the word "extlib"
            if any(exclude in root for exclude in ['extlib', 'MUMPS', 'cbuild']):
                continue
            for file in files:
                # skip ISO-8859 text files
                if (file.endswith('.F') or file.endswith('.F90')):
                    file_path = os.path.join(root, file)
                    is_interface = False
                    encoding = get_encoding(os.path.join(root, file))
                    if not "8859" in encoding:
                        try:
                            with open(file_path, 'r', errors='ignore') as f:
                                new_file_path = file_path.replace('.F90', '.new_headerF90').replace('.F', '.new_headerF90')
                                with open(new_file_path, 'w') as fnew:
                                    for line in f:
                                        match = subroutine_regex.match(line)
                                        match_nocomment = nocomment_regex.match(line)
                                        if end_interface_regex.match(line):
                                            is_interface = False
                                        if begin_interface_regex.match(line):
                                            is_interface = True
                                        if not match:
                                            match = recursive_subroutine_regex.match(line)
                                        if not match:
                                            match = function_regex.match(line)
                                        if not match and not function_end_regex.match(line): 
                                            match = function2_regex.match(line)
                                        if not match:
                                            match = program_regex.match(line)
                                        if not match:
                                            match = module_regex.match(line)
                                        #if at least one match is found
                                        if match and match_nocomment and not is_interface: 
                                            subroutine_name = ''
                                            subroutine_name = match.group(1)
                                            subroutine_name = subroutine_name.strip().lstrip().lower()
                                            if subroutine_name in self.starter_subroutines and 'starter' in root:
                                                fnew.write(self.starter_subroutines[subroutine_name].header)
                                            if subroutine_name in self.engine_subroutines and 'engine' in root:
                                                fnew.write(self.engine_subroutines[subroutine_name].header)
                                            elif subroutine_name in self.common_source_subroutines and 'common_source' in root:
                                                fnew.write(self.common_source_subroutines[subroutine_name].header)
                                        if not header_regex.match(line) and not old_header_regex.match(line):
                                            fnew.write(line)
                            #remove original file, replacing it with the modified file
                            os.remove(file_path)
                            os.rename(new_file_path, file_path)

                        except UnicodeDecodeError:
                            print(f'Error reading file {file_path} due to encoding issues. Skipping...')
                    else:
                        print(f'Skipping file {file_path} due to encoding issues.')

    def fill_callers(self):
        #starter subroutines can be called by starter and common source subroutines
        values = self.starter_subroutines.values()
        for subroutine in values:                                  
            subroutine.fill_callers(self.starter_subroutines)
            subroutine.fill_callers(self.common_source_subroutines)
        #engine subroutines can be called by engine and common source subroutines

        values = self.engine_subroutines.values()
        for subroutine in values:                                 
            subroutine.fill_callers(self.engine_subroutines)
            subroutine.fill_callers(self.common_source_subroutines)
        #common source subroutines can be called by starter, engine and common source subroutines
        values = self.common_source_subroutines.values()
        for subroutine in values:
            subroutine.fill_callers(self.starter_subroutines)
            subroutine.fill_callers(self.engine_subroutines)
            subroutine.fill_callers(self.common_source_subroutines)

    def create_headers(self):
        #starter: union of starter_subroutines and common_source_subroutines

        #get a sorted (by name) copy of the dictionary
        subroutines = self.starter_subroutines.copy()
        for subroutine in self.starter_subroutines.values():
            subroutine.create_header(subroutines)
        #engine: union of engine_subroutines and common_source_subroutines
        subroutines = self.engine_subroutines.copy()
        subroutines.update(self.common_source_subroutines)
        for subroutine in self.engine_subroutines.values():
            subroutine.create_header(subroutines)
        #common_source: union of starter_subroutines, engine_subroutines and common_source_subroutines
        subroutines = self.starter_subroutines.copy()
        subroutines.update(self.engine_subroutines)
        subroutines.update(self.common_source_subroutines)
        for subroutine in self.common_source_subroutines.values():
            subroutine.create_header(subroutines)

    def print(self):
        for subroutine in self.starter_subroutines.values():
            subroutine.print()
        for subroutine in self.engine_subroutines.values():
            subroutine.print()
        for subroutine in self.common_source_subroutines.values():
            subroutine.print()
    #parse all files in the given directory and its subdirectories
    def parse_directory(self,path):
        subroutines = {}
        for root, _, files in os.walk(path):
            if 'extlib' in root:
                continue
            if 'MUMPS' in root:
                continue
            if 'cbuild' in root:
                continue
            for file in files:
                if file.endswith('.F') or file.endswith('.F90'):
                    self.parse_fortran(subroutines, root, file)
                elif file.endswith('.c') or file.endswith('.cpp'):
                    self.parse_c(subroutines, root, file)
                    #print(f'Found C/C++ file: {file}')
        return subroutines
    def parse_directory_function_calls(self,path,subroutines):
        for root, _, files in os.walk(path):
            if 'extlib' in root:
                continue
            if 'MUMPS' in root:
                continue
            if 'cbuild' in root:
                continue
            for file in files:
                if file.endswith('.F') or file.endswith('.F90'):
                    self.parse_fortran_function_calls(subroutines, root, file)

    #parce c/c++ files to find functions called as Fortran subroutine
    def parse_c(self, subroutines, root, file):
        file_path=os.path.join(root, file)
        void_function_regex = re.compile(r'^\s*void\s+(\w+)', re.IGNORECASE)        
        try:
            with open(file_path, 'r', errors='ignore') as f:
                for line in f:
                    line = line.split('//')[0]
                    match = void_function_regex.match(line)
                    #only function definition, exclude lines that have ';'"
                    if match and not ';' in line:
                        subroutine_name = match.group(1).strip().lstrip().lower()
                        subroutines[subroutine_name] = Subroutine(subroutine_name, file_path)
        except UnicodeDecodeError:
            print(f'Error reading file {file_path} due to encoding issues. Skipping...')

    #parse fortran files to find function calls, and add them to the subroutines dictionary
    def parse_fortran_function_calls(self, subroutines, root, file):
        #create a diectionnary from the names of the subroutines that are functions
        functions = {subroutine.name:subroutine for subroutine in subroutines.values() if subroutine.is_fortran_function}
        subroutine_regex = re.compile(r'^\s*subroutine\s+(\w+)', re.IGNORECASE)
        recursive_subroutine_regex=re.compile(r'^\s*recursive\s+subroutine\s+(\w+)', re.IGNORECASE)
        function_regex = re.compile(r'^\s*function\s+(\w+)', re.IGNORECASE)
        function2_regex = re.compile(r'^\s*\w+\s+function\s+(\w+)', re.IGNORECASE)
        function_end_regex = re.compile(r'^\s*END\s+function\s+\w+', re.IGNORECASE)
        program_regex = re.compile(r'^\s*program\s+(\w+)', re.IGNORECASE)
        call_regex = re.compile(r'.*call\s+([\w%]+)', re.IGNORECASE)
        module_regex = re.compile(r'^\s*module\s+(\w+)', re.IGNORECASE)
        use_regex = re.compile(r'.*use\s+(\w+)', re.IGNORECASE)
        nocomment = re.compile(r'^ {4}.*')
        begin_interface_regex = re.compile(r'^\s*interface\s*$', re.IGNORECASE)
        end_interface_regex = re.compile(r'^\s*end\s+interface\s*$', re.IGNORECASE)
        is_interface = False

        #function_patterns = {re.compile(r'\b' + re.escape(name) + r'\b', re.IGNORECASE): name for name in functions.keys()}
        # Build the KeywordProcessor
        keyword_processor = KeywordProcessor(case_sensitive=False)
        for function_name in functions.keys():
            keyword_processor.add_keyword(function_name)

        file_path = os.path.join(root, file)
        subroutine_name = ''
        module_name = ''
        try:
             with open(file_path, 'r', errors='ignore') as f:
                for line in f:
                    # truncate the line to the first "!"
                    line = line.split('!')[0]
                    if end_interface_regex.match(line):
                        is_interface = False
                    if begin_interface_regex.match(line):
                        is_interface = True
                    match = subroutine_regex.match(line)
                    if not match:
                        match = recursive_subroutine_regex.match(line)
                    if not match and not function_end_regex.match(line):
                        match = function_regex.match(line)
                    if not match and not function_end_regex.match(line):
                        match = function2_regex.match(line)
                    if not match:
                        match = program_regex.match(line)
                    if not match:
                        match = module_regex.match(line)
                    if match and not is_interface:
                        subroutine_name = match.group(1).strip().lstrip().lower()
                    elif nocomment.match(line) and not is_interface:
                        # check if the line contains a call to a function in the functions dictionary
                        # loop over all functions 
#                       for function in functions.values():
#                           # ignore the case of the function name, the function name must be a word
#                           # and not a part of a word
#                           if re.search(r'\b' + function.name + r'\b', line, re.IGNORECASE):
#                               subroutines[function.name].add_caller(subroutine_name)
#                               #if subroutine_name in subroutines: add callee
#                               if(subroutine_name in subroutines):
#                                   subroutines[subroutine_name].add_callee(function.name)
#                       for pattern, function_name in function_patterns.items():
#                           if pattern.search(line):
#                               subroutines[function_name].add_caller(subroutine_name)
#                               if subroutine_name in subroutines:
#                                   subroutines[subroutine_name].add_callee(function_name)
                        found_keywords = keyword_processor.extract_keywords(line)
                        for function_name in found_keywords:
                            if(function_name != subroutine_name):
                                subroutines[function_name].add_caller(subroutine_name)
                            if (subroutine_name == debug_name or function_name == debug_name) and debug:
                                print(f'Adding caller {subroutine_name} to {function_name}')
                                #print the line
                                print(line)
                            if subroutine_name in subroutines:
                                if(subroutine_name != function_name):
                                    subroutines[subroutine_name].add_callee(function_name)
                                if (subroutine_name == debug_name or function_name == debug_name) and debug:
                                    print(f'Adding callee {subroutine_name} to {function_name}')
                                    #print the line
                                    print(line)


        except UnicodeDecodeError:
            print(f'Error reading file {file_path} due to encoding issues. Skipping...')


    #parse fortran files to find subroutines and modules
    def parse_fortran(self, subroutines, root, file):
        subroutine_regex = re.compile(r'^\s*subroutine\s+(\w+)', re.IGNORECASE)
        recursive_subroutine_regex=re.compile(r'^\s*recursive\s+subroutine\s+(\w+)', re.IGNORECASE)
        function_regex = re.compile(r'^\s*function\s+(\w+)', re.IGNORECASE)
        function2_regex = re.compile(r'^\s*\w+\s+function\s+(\w+)', re.IGNORECASE)
        function_end_regex = re.compile(r'^\s*END\s+function\s+\w+', re.IGNORECASE)
        program_regex = re.compile(r'^\s*program\s+(\w+)', re.IGNORECASE)
        call_regex = re.compile(r'.*call\s+([\w%]+)', re.IGNORECASE)
        module_regex = re.compile(r'^\s*module\s+(\w+)', re.IGNORECASE)
        use_regex = re.compile(r'.*use\s+(\w+)', re.IGNORECASE)
        nocomment = re.compile(r'^ {4}.*')
        begin_interface_regex = re.compile(r'^\s*interface\s*$', re.IGNORECASE)
        end_interface_regex = re.compile(r'^\s*end\s+interface\s*$', re.IGNORECASE)
        is_interface = False
        file_path = os.path.join(root, file)
        subroutine_name = ''
        module_name = ''
        try:
            with open(file_path, 'r', errors='ignore') as f:
                for line in f:
                    # truncate the line to the first "!"
                    line = line.split('!')[0]
                    if end_interface_regex.match(line):
                        is_interface = False
                    if begin_interface_regex.match(line):
                        is_interface = True
                    match = subroutine_regex.match(line)
                    if not match:
                        match = recursive_subroutine_regex.match(line)
                    if not match and not function_end_regex.match(line):
                        match = function_regex.match(line)
                    if not match and not function_end_regex.match(line):
                        match = function2_regex.match(line)
                    if not match:
                        match = program_regex.match(line)
                    if not match:
                        match = module_regex.match(line)
                    if match and nocomment.match(line) and not is_interface:
                        subroutine_name = match.group(1).strip().lstrip().lower()
                        subroutines[subroutine_name] = Subroutine(subroutine_name, file_path)
                        if function_regex.match(line):
                            subroutines[subroutine_name].is_fortran_function = True
                        if function2_regex.match(line):
                            subroutines[subroutine_name].is_fortran_function = True
                        if debug and subroutine_name == debug_name:
                            print(f'Found subroutine {subroutine_name} in file {file_path}')
                    elif nocomment.match(line):
                        match2 = call_regex.match(line)
                        if(match2):
                            if(subroutine_name == ''):
                                print(f'Error: Call statement found before subroutine definition in file {file_path}')
                            elif(match2.group(1) != "this" and match2.group(1) != "THIS" and "%" not in match2.group(1)):
                                subroutines[subroutine_name].add_callee(match2.group(1))
                                if debug and (subroutine_name == debug_name or match2.group(1) == debug_name):
                                    print(f'- Adding callee {match2.group(1)} to {subroutine_name}')
                                    print(line)
                        elif(use_regex.match(line)):
                            if(subroutine_name== ''):
                                print(f'Error: USE statement found before subroutine definition in file {file_path}')
                            else:
                                module_name= use_regex.match(line).group(1).split(',')[0]
                                subroutines[subroutine_name].add_module(module_name)
        except UnicodeDecodeError:
            print(f'Error reading file {file_path} due to encoding issues. Skipping...')

    #parse fortran files to find function calls

#if __name__ == '__main__':
#    import cProfile
#    import pstats
#    cProfile.run('code_analyzer = CodeAnalyzer()', 'profile.stats')
#    #code_analyzer = CodeAnalyzer()
#    p = pstats.Stats('profile.stats',stream=open('profile.txt','w'))
#    p.sort_stats('cumulative').print_stats()
#if __name__ == '__main__':
#    code_analyzer = CodeAnalyzer()
if __name__ == '__main__':
    code_analyzer = CodeAnalyzer()

