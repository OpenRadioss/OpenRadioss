import os
import json
import shutil
import urllib.request
import zipfile
import argparse
# ================================================================
# Script to download the external libraries for OpenRadioss build
# ================================================================
if __name__ == "__main__":
   
   source_root =  os.path.dirname(os.path.abspath(__file__))+"/../.."
   print("Load external libraries for OpenRadioss build\n")
   need_library_file=source_root+"/EXTLIB_VERSION.json"
   extlib_directory=source_root+"/extlib"
   have_library_file=extlib_directory+"/EXTLIB_VERSION.json"
   old_tracked_file=extlib_directory+"/license/hm_reader_license.txt"

   # Load needed library JSON file
   with open(need_library_file, "r") as file:
        needed_library = json.load(file)

   version=needed_library["version"]
   url=needed_library["url"]

   # Check if the needed version of libraries is already downloaded
   new_download=False
   try:
       with open(have_library_file, "r") as file:
        have_library = json.load(file)
        got_extlib=True
        new_download=False
   except:
        print("Need to download version {"+version+"} of libraries\n")
        got_extlib=False
        new_download=True

   # Try to find one file of the library which was previously tracked by git.
   if got_extlib:
       try:
          with open(old_tracked_file, "r") as file:
             got_extlib=True
             new_download=False
             file.close()
       except IOError as e:
             print("Missing libraries detected: \nNeed to download version {"+version+"} of libraries\n")
             got_extlib=False
             new_download=True

   # Control need & have version of libraries
   if got_extlib:
        have_version=have_library["version"]
        have_url=have_library["url"]
        if have_version == version:
            print("    Already have needed version of libraries\n")
            new_download=False
        else:
            print("Need to download version "+version+" of libraries")
            print("--- Delete current extlib directory")
            shutil.rmtree(extlib_directory)
            new_download=True

   if new_download:
       print("--- Download need version of extlib")
       filename = url.split('/')[-1]
       download_path = os.path.join(source_root, filename)
       try:
          urllib.request.urlretrieve(url, download_path)
       except:
            print("\n    Download failed")
            print("    "+url)
            print("    Please retry")
            exit(1)

       # unzip file
       print("--- Unzip downloaded file")
       zip_file_path = source_root+"/extlib.zip"
       extract_to_directory = source_root

       # Unzip the file
       with zipfile.ZipFile(zip_file_path, 'r') as zip_ref:
           try:
              zip_ref.extractall(extract_to_directory)
              message="    Unzipped '{"+zip_file_path+"}'"
              print(message)
           except:
              print("\nUnzip failed")
              print("Please retry")
              exit(1)

