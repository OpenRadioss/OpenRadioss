#!/usr/bin/env python3
import os
import re
import shutil

starter_deck_ext = ['_0000.rad']
engine_deck_ext = ['_0001.rad']

def is_starter_deck(f):
    results = [f.endswith(ext) for ext in starter_deck_ext]
    return True in results

def is_engine_deck(f):
    results = [f.endswith(ext) for ext in engine_deck_ext]
    return True in results

def is_rad_file(f):
    return is_starter_deck(f) or is_engine_deck(f)

def is_copyright_block_start(line):
    """Check if a line is the start of the specific copyright block"""
    # Match: # Copyright (C) 202X Altair Engineering Inc. ("Holder")
    pattern = r'#\s*Copyright\s*\(C\)\s*202\d.*Altair.*Holder'
    return bool(re.search(pattern, line, re.IGNORECASE))

def remove_copyright_from_file(filename):
    """Remove copyright header from a .rad file"""
    
    try:
        with open(filename, encoding='latin1') as f:
            lines = f.readlines()
        
        if not lines:
            return
        
        new_lines = []
        removed_count = 0
        
        # For starter decks, preserve the first line (#RADIOSS STARTER)
        start_idx = 0
        if is_starter_deck(filename):
            if lines and lines[0].strip().startswith('#RADIOSS'):
                new_lines.append(lines[0])
                start_idx = 1
        
        # Process remaining lines
        i = start_idx
        while i < len(lines):
            line = lines[i]
            
            # Check if this is the start of a copyright block
            if is_copyright_block_start(line):
                # Skip this line and the next 2 lines (the 3-line copyright block)
                # But verify the next lines look like copyright continuation
                if (i + 2 < len(lines) and 
                    'licensed' in lines[i+1].lower() and 
                    'creativecommons.org' in lines[i+2].lower()):
                    # Skip all 3 lines
                    i += 3
                    removed_count += 1
                else:
                    # Doesn't match expected pattern, keep the line
                    new_lines.append(line)
                    i += 1
            else:
                # Not a copyright line, keep it
                new_lines.append(line)
                i += 1
        
        # Check if anything was removed
        if removed_count > 0:
            print(f"Removing {removed_count} copyright block(s) from: {filename}")
            
            # Create backup
            shutil.copy(filename, filename + ".backup")
            
            # Write cleaned content
            with open(filename, 'w', encoding='latin1') as f:
                f.writelines(new_lines)
        else:
            print(f"No copyright found in: {filename}")
    
    except Exception as e:
        print(f"Error processing {filename}: {e}")

def process_directory(base_path="../../"):
    """Walk through directory and process all .rad files"""
    
    # Exclusions from the original script
    exclusions = ["/com/", "/extlib/", "CMake", "th_to_csv", "anim_to_vtk"]
    
    count = 0
    for root, dirs, files in os.walk(base_path):
        # Skip excluded directories
        if any(excl in root for excl in exclusions):
            continue
        
        for filename in files:
            if is_rad_file(filename):
                filepath = os.path.join(root, filename)
                remove_copyright_from_file(filepath)
                count += 1
    
    print(f"\nProcessed {count} .rad files")

if __name__ == "__main__":
    print("Removing copyright headers from .rad files...")
    print("Backups will be created with .backup extension")
    print()
    process_directory()
    print("\nDone!")
