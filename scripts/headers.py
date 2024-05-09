import os
import re
import subprocess
from enum import Enum


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
    def add_callee(self, callee):
        #do not duplicate entries if already exists
        if(callee.lower() not in self.callees_name):
            self.callees_name.append(callee.lower())
    def add_module(self, module):
        if(module.lower() not in self.modules_name):
            self.modules_name.append(module.lower())
    def add_caller(self, caller):
        if(caller.lower() not in self.callers_name):
            self.callers_name.append(caller.lower())
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

        # loop over all callers by alphabetical order
        for caller in sorted(self.callers_name):
            #check if caller is in subroutines
            if caller in subroutines:
                path=subroutines[caller].path
                self.header += f'      !||    {caller.ljust(max_len)}   {path}\n'
            else:
                pass
                #self.header += f'      !||    {caller.ljust(max_len)}   \n'
        #if callees_name is not empty
        if self.callees_name:
            self.header += f'      !||--- calls      -----------------------------------------------------\n'
             
            for callee in sorted(self.callees_name):
                if(callee in subroutines):
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
        module_regex = re.compile(r'^\s*module\s+(\w+)', re.IGNORECASE)

        header_regex = re.compile(r'^\s*!\|\|', re.IGNORECASE)
        old_header_regex = re.compile(r'^Chd', re.IGNORECASE)
        for root, _, files in os.walk(path):
            for file in files:
                # skip ISO-8859 text files
                if (file.endswith('.F') or file.endswith('.F90')):
                    file_path = os.path.join(root, file)
                    encoding = get_encoding(os.path.join(root, file))
                    if not "8859" in encoding:
                        try:
                            with open(file_path, 'r', errors='ignore') as f:
                                new_file_path = file_path.replace('.F90', '.new_headerF90').replace('.F', '.new_headerF90')
                                with open(new_file_path, 'w') as fnew:
                                    for line in f:
                                        match = subroutine_regex.match(line)
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
                                        if match: 
                                            subroutine_name = ''
                                            subroutine_name = match.group(1)
                                            subroutine_name = subroutine_name.strip().lstrip().lower()
                                            if subroutine_name in self.starter_subroutines:
                                                fnew.write(self.starter_subroutines[subroutine_name].header)
                                            elif subroutine_name in self.engine_subroutines:
                                                fnew.write(self.engine_subroutines[subroutine_name].header)
                                            elif subroutine_name in self.common_source_subroutines:
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
        for subroutine in self.starter_subroutines.values():
            subroutine.fill_callers(self.starter_subroutines)
            subroutine.fill_callers(self.common_source_subroutines)
        #engine subroutines can be called by engine and common source subroutines
        for subroutine in self.engine_subroutines.values():
            subroutine.fill_callers(self.engine_subroutines)
            subroutine.fill_callers(self.common_source_subroutines)
        #common source subroutines can be called by starter, engine and common source subroutines
        for subroutine in self.common_source_subroutines.values():
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
            for file in files:
                if file.endswith('.F') or file.endswith('.F90'):
                    self.parse_fortran(subroutines, root, file)
                elif file.endswith('.c') or file.endswith('.cpp'):
                    self.parse_c(subroutines, root, file)
                    #print(f'Found C/C++ file: {file}')
        return subroutines
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

    #parse fortran files to find subroutines and modules
    def parse_fortran(self, subroutines, root, file):
        subroutine_regex = re.compile(r'^\s*subroutine\s+(\w+)', re.IGNORECASE)
        recursive_subroutine_regex=re.compile(r'^\s*recursive\s+subroutine\s+(\w+)', re.IGNORECASE)
        function_regex = re.compile(r'^\s*function\s+(\w+)', re.IGNORECASE)
        function2_regex = re.compile(r'^\s*\w+\s+function\s+(\w+)', re.IGNORECASE)
        program_regex = re.compile(r'^\s*program\s+(\w+)', re.IGNORECASE)
        call_regex = re.compile(r'.*call\s+([\w%]+)', re.IGNORECASE)
        module_regex = re.compile(r'^\s*module\s+(\w+)', re.IGNORECASE)
        use_regex = re.compile(r'.*use\s+(\w+)', re.IGNORECASE)
        nocomment = re.compile(r'^ {4}.*')

        file_path = os.path.join(root, file)
        subroutine_name = ''
        module_name = ''
        try:
            with open(file_path, 'r', errors='ignore') as f:
                for line in f:
                    # truncate the line to the first "!"
                    line = line.split('!')[0]
                    match = subroutine_regex.match(line)
                    if not match:
                        match = recursive_subroutine_regex.match(line)
                    if not match:
                        match = function_regex.match(line)
                    if not match:
                        match = function2_regex.match(line)
                    if not match:
                        match = program_regex.match(line)
                    if not match:
                        match = module_regex.match(line)
                    if match:
                        subroutine_name = match.group(1).strip().lstrip().lower()
                        subroutines[subroutine_name] = Subroutine(subroutine_name, file_path)
                    elif nocomment.match(line):
                        match2 = call_regex.match(line)
                        if(match2):
                            if(subroutine_name == ''):
                                print(f'Error: Call statement found before subroutine definition in file {file_path}')
                            elif(match2.group(1) != "this" and match2.group(1) != "THIS" and "%" not in match2.group(1)):
                                subroutines[subroutine_name].add_callee(match2.group(1))
                        elif(use_regex.match(line)):
                            if(subroutine_name== ''):
                                print(f'Error: USE statement found before subroutine definition in file {file_path}')
                            else:
                                module_name= use_regex.match(line).group(1).split(',')[0]
                                subroutines[subroutine_name].add_module(module_name)
        except UnicodeDecodeError:
            print(f'Error reading file {file_path} due to encoding issues. Skipping...')

if __name__ == '__main__':
    code_analyzer = CodeAnalyzer()