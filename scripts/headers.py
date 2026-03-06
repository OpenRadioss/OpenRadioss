#run from the script directory, run "python headers.py" to update all the files.
#Compile the code, add and commit the changes
import os
import re
import shutil


debug = False
debug_name = 'i22aera'


def is_iso8859(file_path):
    """Return True if the file is NOT valid UTF-8 (likely ISO-8859 encoded).
    Valid UTF-8 files with Unicode characters (lambda, degree sign, etc.) return False."""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            f.read()
        return False
    except UnicodeDecodeError:
        return True
    except (IOError, OSError):
        return False


# ---- Pre-compiled regexes (compiled once, reused everywhere) ----
_SUBROUTINE_RE = re.compile(r'^\s*subroutine\s+(\w+)', re.IGNORECASE)
_RECURSIVE_SUB_RE = re.compile(r'^\s*recursive\s+subroutine\s+(\w+)', re.IGNORECASE)
_FUNCTION_RE = re.compile(r'^\s*function\s+(\w+)', re.IGNORECASE)
_FUNCTION2_RE = re.compile(r'^\s*\w+\s+function\s+(\w+)', re.IGNORECASE)
_FUNCTION_END_RE = re.compile(r'^\s*END\s+function\s+\w+', re.IGNORECASE)
_PROGRAM_RE = re.compile(r'^\s*program\s+(\w+)', re.IGNORECASE)
_MODULE_RE = re.compile(r'^\s*module\s+(?!procedure\s)(\w+)', re.IGNORECASE)
_CALL_RE = re.compile(r'.*call\s+([\w%]+)', re.IGNORECASE)
_USE_RE = re.compile(r'.*use\s+(\w+)', re.IGNORECASE)
_NOCOMMENT_RE = re.compile(r'^ {4}.*')
_BEGIN_INTERFACE_RE = re.compile(r'^\s*interface\s*$', re.IGNORECASE)
_END_INTERFACE_RE = re.compile(r'^\s*end\s+interface\s*$', re.IGNORECASE)
_HEADER_RE = re.compile(r'^\s*!\|\|', re.IGNORECASE)
_OLD_HEADER_RE = re.compile(r'^Chd', re.IGNORECASE)
_VOID_FUNC_RE = re.compile(r'^\s*void\s+(\w+)', re.IGNORECASE)

# Combined regex for definition matching (subroutine, recursive subroutine, function, program, module)
# Group 1: subroutine name, Group 2: recursive subroutine name, Group 3: plain function name,
# Group 4: typed function name, Group 5: program name, Group 6: module name
_DEFINITION_RE = re.compile(
    r'^\s*(?:'
    r'subroutine\s+(\w+)'                           # group 1: subroutine
    r'|recursive\s+subroutine\s+(\w+)'              # group 2: recursive subroutine
    r'|function\s+(\w+)'                             # group 3: plain function
    r'|\w+\s+function\s+(\w+)'                       # group 4: typed function
    r'|program\s+(\w+)'                              # group 5: program
    r'|module\s+(?!procedure\s)(\w+)'                # group 6: module
    r')', re.IGNORECASE)

# First chars (both cases) that can start a definition line
_DEFINITION_FIRST_CHARS = frozenset('srfpmidlcSRFPMIDLC')
# First chars that indicate end interface/begin interface
_END_CHARS = frozenset('eE')
_IFACE_CHARS = frozenset('iI')
_NONE_RESULT = (None, False, False)


def _match_definition(line):
    """Try to match a definition line. Returns (match, is_function, is_end_function) tuple.
    Uses early-exit: most lines don't start with definition keywords."""
    stripped = line.lstrip()
    if not stripped:
        return _NONE_RESULT
    fc = stripped[0]
    # Check 'end function' first
    if fc in _END_CHARS:
        if _FUNCTION_END_RE.match(line):
            return None, False, True
        return _NONE_RESULT
    # Quick skip for non-definition lines
    if fc not in _DEFINITION_FIRST_CHARS:
        return _NONE_RESULT
    m = _DEFINITION_RE.match(line)
    if m:
        groups = m.groups()
        is_func = groups[2] is not None or groups[3] is not None
        return m, is_func, False
    return _NONE_RESULT


def _get_definition_name(m):
    """Extract the matched name from a _DEFINITION_RE match."""
    for g in m.groups():
        if g is not None:
            return g.strip().lower()
    return ''

class Subroutine:
    __slots__ = ('name', 'path', 'callees_name', 'modules_name', 'callers_name', 'header', 'is_fortran_function')

    def __init__(self, name, path):
        self.name = name.lower()
        self.path = path
        self.callees_name = set()
        self.modules_name = set()
        self.callers_name = set()
        self.header = ""
        self.is_fortran_function = False

    def add_callee(self, callee):
        callee_lower = callee.lower()
        if callee_lower not in self.callees_name:
            self.callees_name.add(callee_lower)
            if debug and self.name == debug_name:
                print(f'Adding callee {callee} to {self.name}')
            if debug and callee_lower == debug_name:
                print(f'Adding caller {self.name} to {callee}')

    def add_module(self, module):
        module_lower = module.lower()
        if module_lower not in self.modules_name:
            self.modules_name.add(module_lower)
            if debug and self.name == debug_name:
                print(f'Adding module {module} to {self.name}')

    def add_caller(self, caller):
        caller_lower = caller.lower()
        if caller_lower not in self.callers_name:
            self.callers_name.add(caller_lower)
            if debug and self.name == debug_name:
                print(f'Adding caller {caller} to {self.name}')
            if debug and caller_lower == debug_name:
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

    def create_header(self, subroutines):
        if self.path.endswith('.c') or self.path.endswith('.cpp'):
            return
        max_len = len(self.name)
        if self.callees_name:
            max_len = max(max_len, max(len(sub) for sub in self.callees_name))
        if self.callers_name:
            max_len = max(max_len, max(len(sub) for sub in self.callers_name))
        if self.modules_name:
            max_len = max(max_len, max(len(sub) for sub in self.modules_name))

        parts = []
        parts.append(f'!||====================================================================\n')
        parts.append(f'!||    {self.name.ljust(max_len)}   {self.path}\n')
        if self.callers_name:
            parts.append(f'!||--- called by ------------------------------------------------------\n')
        elif debug:
            print(f'Warning: Subroutine {self.name} in {self.path} is dead')

        for caller in sorted(self.callers_name):
            if caller in subroutines and caller != self.name:
                path = subroutines[caller].path
                parts.append(f'!||    {caller.ljust(max_len)}   {path}\n')
        if self.callees_name:
            parts.append(f'!||--- calls      -----------------------------------------------------\n')
            for callee in sorted(self.callees_name):
                if callee in subroutines and callee != self.name:
                    path = subroutines[callee].path
                    parts.append(f'!||    {callee.ljust(max_len)}   {path}\n')
        if self.modules_name:
            parts.append(f'!||--- uses       -----------------------------------------------------\n')
            for module in sorted(self.modules_name):
                if module in subroutines:
                    path = subroutines[module].path
                    parts.append(f'!||    {module.ljust(max_len)}   {path}\n')
        parts.append(f'!||====================================================================\n')
        self.header = ''.join(parts)

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
        # Fix copyright headers before updating subroutine headers
        self.fix_copyright('../starter')
        self.fix_copyright('../engine')
        self.fix_copyright('../common_source')
        self.update_headers('../starter')
        self.update_headers('../engine')
        self.update_headers('../common_source')


    def fix_copyright(self, path):
        """Walk the directory and fix copyright headers on .F and .F90 files."""
        # Copyright template files are located in the copyright/ subdirectory
        copyright_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'copyright')
        for root, _, files in os.walk(path):
            if any(exclude in root for exclude in ['extlib', 'MUMPS', 'cbuild', 'CMake']):
                continue
            for file in files:
                if file.endswith('.F') or file.endswith('.F90'):
                    file_path = os.path.join(root, file)
                    self.add_copyright(file_path, copyright_dir)

    def add_copyright(self, filename, copyright_dir):
        """Check and fix the copyright header of a single file."""
        if filename.endswith('.F90'):
            template = os.path.join(copyright_dir, 'F90_copyright.txt')
        elif filename.endswith('.F'):
            template = os.path.join(copyright_dir, 'f_COPYRIGHT.txt')
        else:
            return

        # Count the number of lines in the copyright template
        with open(template) as f:
            nbl = sum(1 for _ in f)

        # Check if the file already has the correct copyright header
        try:
            with open(filename, encoding='latin1') as f1, open(template, encoding='latin1') as f2:
                ok_header = True
                for _ in range(nbl):
                    if f1.readline() != f2.readline():
                        ok_header = False
                        break
        except (IOError, OSError):
            return

        if not ok_header:
            print(f'WARNING: {filename} has no copyright -- fixing')
            # Prepend the copyright template, stripping any old Copyright> lines
            shutil.copy(template, filename + '.bak')
            with open(filename, encoding='latin1') as f1, open(filename + '.bak', 'a', encoding='latin1') as f2:
                for line in f1:
                    if not re.search('Copyright>', line):
                        f2.write(line)
            shutil.move(filename + '.bak', filename)

    # write a copy of the .F and .F90 files (adding suffix .new_header) with the headers for all subroutines and modules
    def update_headers(self, path):
        # Local references for speed
        def_re = _DEFINITION_RE
        end_iface_re = _END_INTERFACE_RE
        begin_iface_re = _BEGIN_INTERFACE_RE
        def_first = _DEFINITION_FIRST_CHARS
        end_ch = _END_CHARS
        iface_ch = _IFACE_CHARS
        starter_subs = self.starter_subroutines
        engine_subs = self.engine_subroutines
        common_subs = self.common_source_subroutines

        for root, _, files in os.walk(path):
            if any(exclude in root for exclude in ['extlib', 'MUMPS', 'cbuild']):
                continue
            has_starter = 'starter' in root
            has_engine = 'engine' in root
            has_common = 'common_source' in root
            for file in files:
                if file.endswith('.F') or file.endswith('.F90'):
                    file_path = os.path.join(root, file)
                    # Skip files that are not valid UTF-8 (ISO-8859 encoded)
                    if is_iso8859(file_path):
                        print(f'Skipping file {file_path} due to encoding issues (not valid UTF-8).')
                        # Show problematic lines to help fix them
                        try:
                            with open(file_path, 'rb') as fenc:
                                for line_no, raw_line in enumerate(fenc, 1):
                                    try:
                                        raw_line.decode('utf-8')
                                    except UnicodeDecodeError:
                                        text = raw_line.decode('latin-1').rstrip()
                                        print(f'  Line {line_no}: {text}')
                        except (IOError, OSError):
                            pass
                        continue
                    is_interface = False
                    try:
                        with open(file_path, 'r', errors='ignore') as f:
                            lines = f.readlines()
                        output_parts = []
                        out_append = output_parts.append
                        for line in lines:
                            stripped = line.lstrip()
                            if not stripped:
                                out_append(line)
                                continue
                            fc = stripped[0]
                            # Interface tracking
                            if fc in end_ch and end_iface_re.match(line):
                                is_interface = False
                                out_append(line)
                                continue
                            if fc in iface_ch and begin_iface_re.match(line):
                                is_interface = True
                                out_append(line)
                                continue
                            # Header/old-header line filtering
                            if fc == '!' and stripped[1:3] == '||':
                                continue  # skip header lines
                            if line[0] in ('C', 'c') and line[0:3] in ('Chd', 'CHD', 'chd'):
                                continue  # skip old header lines
                            # Try definition match (inlined) — only for code lines
                            if line[0:4] == '    ' and not is_interface and fc in def_first:
                                m = def_re.match(line)
                                if m:
                                    for g in m.groups():
                                        if g is not None:
                                            subroutine_name = g.strip().lower()
                                            break
                                    if subroutine_name in starter_subs and has_starter:
                                        out_append(starter_subs[subroutine_name].header)
                                    if subroutine_name in engine_subs and has_engine:
                                        out_append(engine_subs[subroutine_name].header)
                                    elif subroutine_name in common_subs and has_common:
                                        out_append(common_subs[subroutine_name].header)
                            out_append(line)
                        # Write all at once
                        with open(file_path, 'w') as fnew:
                            fnew.write(''.join(output_parts))
                    except UnicodeDecodeError:
                        print(f'Error reading file {file_path} due to encoding issues. Skipping...')

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
    def parse_directory(self, path):
        subroutines = {}
        for root, _, files in os.walk(path):
            if any(exclude in root for exclude in ['extlib', 'MUMPS', 'cbuild']):
                continue
            for file in files:
                if file.endswith('.F') or file.endswith('.F90'):
                    self.parse_fortran(subroutines, root, file)
                elif file.endswith('.c') or file.endswith('.cpp'):
                    self.parse_c(subroutines, root, file)
        return subroutines
    def parse_directory_function_calls(self, path, subroutines):
        # Build the function pattern ONCE for the whole directory
        functions = {s.name: s for s in subroutines.values() if s.is_fortran_function}
        if functions:
            pattern = r'\b(' + '|'.join(re.escape(name) for name in functions.keys()) + r')\b'
            function_pattern = re.compile(pattern, re.IGNORECASE)
        else:
            function_pattern = None

        for root, _, files in os.walk(path):
            if any(exclude in root for exclude in ['extlib', 'MUMPS', 'cbuild']):
                continue
            for file in files:
                if file.endswith('.F') or file.endswith('.F90'):
                    self.parse_fortran_function_calls(subroutines, root, file, function_pattern)

    #parse c/c++ files to find functions called as Fortran subroutine
    def parse_c(self, subroutines, root, file):
        file_path = os.path.join(root, file)
        try:
            with open(file_path, 'r', errors='ignore') as f:
                for line in f:
                    line = line.split('//')[0]
                    match = _VOID_FUNC_RE.match(line)
                    if match and ';' not in line:
                        subroutine_name = match.group(1).strip().lower()
                        subroutines[subroutine_name] = Subroutine(subroutine_name, file_path)
        except UnicodeDecodeError:
            print(f'Error reading file {file_path} due to encoding issues. Skipping...')

    #parse fortran files to find function calls, and add them to the subroutines dictionary
    def parse_fortran_function_calls(self, subroutines, root, file, function_pattern):
        file_path = os.path.join(root, file)
        subroutine_name = ''
        is_interface = False
        # Local references for speed
        def_re = _DEFINITION_RE
        end_iface_re = _END_INTERFACE_RE
        begin_iface_re = _BEGIN_INTERFACE_RE
        func_end_re = _FUNCTION_END_RE
        def_first = _DEFINITION_FIRST_CHARS
        end_ch = _END_CHARS
        iface_ch = _IFACE_CHARS
        try:
            with open(file_path, 'r', errors='ignore') as f:
                for line in f:
                    line = line.split('!')[0]
                    stripped = line.lstrip()
                    if not stripped:
                        continue
                    fc = stripped[0]
                    # Interface tracking
                    if fc in end_ch and end_iface_re.match(line):
                        is_interface = False
                        continue
                    if fc in iface_ch and begin_iface_re.match(line):
                        is_interface = True
                        continue
                    if is_interface:
                        continue
                    # Try definition match (inlined)
                    if fc in def_first:
                        m = def_re.match(line)
                        if m:
                            for g in m.groups():
                                if g is not None:
                                    subroutine_name = g.strip().lower()
                                    break
                            continue
                    elif fc in end_ch:
                        if func_end_re.match(line):
                            continue
                    # Regular code line — check for function calls
                    if line[0:4] == '    ' and function_pattern:
                        found_keywords = function_pattern.findall(line)
                        for function_name in found_keywords:
                            function_name = function_name.lower()
                            if function_name != subroutine_name:
                                subroutines[function_name].add_caller(subroutine_name)
                            if debug and (subroutine_name == debug_name or function_name == debug_name):
                                print(f'Adding caller {subroutine_name} to {function_name}')
                                print(line)
                            if subroutine_name in subroutines:
                                if subroutine_name != function_name:
                                    subroutines[subroutine_name].add_callee(function_name)
                                if debug and (subroutine_name == debug_name or function_name == debug_name):
                                    print(f'Adding callee {subroutine_name} to {function_name}')
                                    print(line)
        except UnicodeDecodeError:
            print(f'Error reading file {file_path} due to encoding issues. Skipping...')


    #parse fortran files to find subroutines and modules
    def parse_fortran(self, subroutines, root, file):
        is_interface = False
        file_path = os.path.join(root, file)
        subroutine_name = ''
        # Local references for speed
        def_re = _DEFINITION_RE
        end_iface_re = _END_INTERFACE_RE
        begin_iface_re = _BEGIN_INTERFACE_RE
        func_end_re = _FUNCTION_END_RE
        call_re = _CALL_RE
        use_re = _USE_RE
        def_first = _DEFINITION_FIRST_CHARS
        end_ch = _END_CHARS
        iface_ch = _IFACE_CHARS
        try:
            with open(file_path, 'r', errors='ignore') as f:
                for line in f:
                    line = line.split('!')[0]
                    stripped = line.lstrip()
                    if not stripped:
                        continue
                    fc = stripped[0]
                    # Interface tracking
                    if fc in end_ch and end_iface_re.match(line):
                        is_interface = False
                        continue
                    if fc in iface_ch and begin_iface_re.match(line):
                        is_interface = True
                        continue
                    # Only process lines starting with 4 spaces (code lines)
                    if line[0:4] != '    ':
                        continue
                    # Try definition match (inlined)
                    if fc in def_first:
                        m = def_re.match(line)
                        if m and not is_interface:
                            groups = m.groups()
                            for g in groups:
                                if g is not None:
                                    subroutine_name = g.strip().lower()
                                    break
                            subroutines[subroutine_name] = Subroutine(subroutine_name, file_path)
                            # Check if it's a function (groups 2 and 3)
                            if groups[2] is not None or groups[3] is not None:
                                subroutines[subroutine_name].is_fortran_function = True
                            if debug and subroutine_name == debug_name:
                                print(f'Found subroutine {subroutine_name} in file {file_path}')
                            continue
                    elif fc in end_ch:
                        if func_end_re.match(line):
                            continue
                    # Regular code line — process call/use even inside interface blocks
                    # (use statements inside interfaces belong to the enclosing module/subroutine)
                    match2 = call_re.match(line)
                    if match2:
                        if subroutine_name == '':
                            print(f'Error: Call statement found before subroutine definition in file {file_path}')
                        elif match2.group(1) != "this" and match2.group(1) != "THIS" and "%" not in match2.group(1):
                            subroutines[subroutine_name].add_callee(match2.group(1))
                            if debug and (subroutine_name == debug_name or match2.group(1) == debug_name):
                                print(f'- Adding callee {match2.group(1)} to {subroutine_name}')
                                print(line)
                    else:
                        use_match = use_re.match(line)
                        if use_match:
                            if subroutine_name == '':
                                print(f'Error: USE statement found before subroutine definition in file {file_path}')
                            else:
                                module_name = use_match.group(1).split(',')[0]
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

