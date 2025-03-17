/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/


#ifndef MEC_READ_FILE_H
#define MEC_READ_FILE_H

#include <memory>

#include <UTILS/mv_cstdio.h>
#include <UTILS/str_utils.h>
#include <UTILS/memory_utils.h>



#include "mec_position.h"
#include "hcio.h"

class HCIO_DATA_DLL_API MECReadFile {
 protected: /** @name Constructors & destructor */
  //@{
  /// Default constructor
  MECReadFile(); 
  /// Constructor from a path name
  MECReadFile(const char *full_name);
  /// Constructor from an open file
  MECReadFile(const char *full_name, _HC_LONG currentLine);

public:
  /// Destructor
  virtual ~MECReadFile();
  //@}
  
 public: /** @name File management */
  //@{
  /// Opening
  virtual void open() = 0;
  virtual bool isOpen() = 0;
  /// Closing
  virtual void close() = 0;
  /// Gets the full name
  inline const char *GetFullName() const { return myFullName; } 

  /// Gets the relative name

  inline const char *GetRelativeName() const {return myRelativeName; } 

  /// Sets the relative name

  void  SetRelativeName(const char* relative_name);  
  /// Gets the file
  /// Get and Set the RADIOSS Version 
  MvFileFormat_e GetVersion()  const { return myVersion;}
  void SetVersion(MvFileFormat_e version) {myVersion = version ;}
  /// Gets the status

  inline int GetStatus() const { return myStatus;}

  /// Sets the status

  inline void SetStatus(int a_status){myStatus = a_status;}
  /// Gets the current position
  virtual _HC_LONG GetCurrentLocation() const = 0;
  /// Gets the current line
  inline _HC_LONG GetCurrentLine() const { return myCurrentPositionPtr->GetLine(); } 
   /// Gets the previous (before last read) line
  inline _HC_LONG GetPreviousLine() const { return myPreviousPositionPtr->GetLine(); } 
   /// Gets the previous (before last read) position

  inline _HC_LONG GetPreviousLocation() const { return myPreviousPositionPtr->GetLocation(); } 

  /// Gets the line in the parent file
  inline _HC_LONG GetParentLine () const  {return myParentPositionPtr->GetLine();} 
 /// Gets the position in the parent file

  inline _HC_LONG GetParentLocation () const  {return myParentPositionPtr->GetLocation();} 

  /// Get the include parent index

  inline int GetParentIndex() const {return myIncudeParentIndex;}  

 /// Get the component index

  inline int GetComponentIndex() const {return myComponentIndex;}  

  

  /// Sets the current position (line , location)

  //void SetCurrentPosition(const CKeywordData& a_pos);

  /// Sets the current location

  virtual void SetCurrentLocation(_HC_LONG a_loc) = 0;

  /// Sets the current line
  void SetCurrentLine(_HC_LONG cur_line);  
  /// Set the line in the parent file

  inline void SetParentLine(_HC_LONG a_line) {myParentPositionPtr->SetLine(a_line);} 

  /// Set the position in the parent file
  inline void SetParentLocation(_HC_LONG a_loc) {myParentPositionPtr->SetLocation(a_loc);} 
   /// Set the include parent index

  inline void SetParentIndex(int an_index) {myIncudeParentIndex = an_index;} 

  /// Set the include parent index

  inline void SetComponentIndex(int an_index) {myComponentIndex = an_index;} 

 



  /// Returns true if first line was read in the file

  inline bool IsFirstLineRead() {return myIsFirstLineRead; }  
 
  //@}

  // FILE functionality:

  virtual int seek(long offset, int origin) = 0;

  virtual char* gets(char* buffer, int maxCount) = 0;

public:
     
  
   static int fileNum;

 public: /** @name Reading */
  //@{
  
  virtual bool eof() const = 0;
  /// Reads a line
  virtual bool readBuffer(int nb_chars,char *buffer);
  //@}
  
 protected: /* Data */
  char          *myFullName;

  char          *myRelativeName;  
  bool           myDoCloseOnDestruction; 
  MvFileFormat_e myVersion ;

  

  int           myStatus;

  int           myIncudeParentIndex;

  int           myCompnentIndex;

  bool			myIsFirstLineRead;

  CKeywordData  *myCurrentPositionPtr;

  CKeywordData  *myPreviousPositionPtr;

  CKeywordData  *myParentPositionPtr;

  int           myComponentIndex;


  

  
};

template<class MYFILE>
class TMECReadFile: public MECReadFile
{
public:
    /// Default constructor
    TMECReadFile() : MECReadFile() {}

    /// Constructor from a path name
    TMECReadFile(const char *full_name, bool do_open=true):
        MECReadFile(full_name)
    {
        if(do_open) 
        {
            open();
            myDoCloseOnDestruction = true;
        }
    }

    /// Constructor from an open file
    TMECReadFile(MYFILE *filePtr, const char *full_name, _HC_LONG currentLine = 0) :
        MECReadFile(full_name, currentLine), myFilePtr(filePtr)
    {}

    virtual ~TMECReadFile()
    {
        if (myDoCloseOnDestruction)
            close();
    }

    MYFILE* GetFilePtr() const { return myFilePtr; }
    void upateFilePtr(MYFILE* file){ myFilePtr = file;}

    // implementations of MECReadFile methods
    virtual void open()
    {
        if(myFilePtr!=NULL) close();

        myFilePtr = fopen(myFullName, "rb");
        myDoCloseOnDestruction = true;
    }

    virtual bool isOpen()
    {
        return myFilePtr != nullptr;
    }

    virtual void close()
    {
        if(myFilePtr!=NULL) 
        {
            fclose(myFilePtr);
            myFilePtr=NULL;
        }
        myDoCloseOnDestruction = false; 
    }

    virtual _HC_LONG GetCurrentLocation() const
    {
#ifdef OS_WIN
        fpos_t pos;
        fgetpos(myFilePtr, &pos);
        return pos;
#else
        return ftell(myFilePtr);
#endif
    }

    virtual void SetCurrentLocation(_HC_LONG a_loc) { 

#ifdef OS_WIN
        fsetpos(myFilePtr, &a_loc);
#else
        fseek(myFilePtr,(long int)a_loc,SEEK_SET);
#endif
    }

    virtual bool eof() const {
        return feof((MYFILE*)myFilePtr) ? true : false;
    }

    // FILE functionality:

    virtual int seek(long offset, int origin)
    {
        return fseek(myFilePtr, offset, origin);
    }

    virtual char* gets(char* buffer, int maxCount)
    {
        return fgets(buffer, maxCount, myFilePtr);
    }

private:
    MYFILE *myFilePtr = nullptr;
};

class IReadFileFactory
{
public:
    virtual MECReadFile* open(const char *full_name) = 0;
};

typedef std::shared_ptr<IReadFileFactory> ReadFileFactorySP;

template<class MYFILE>
class TReadFileFactory: public IReadFileFactory
{
public:
    virtual MECReadFile* open(const char *full_name)
    {
        return new TMECReadFile<MYFILE>(full_name);
    }
};

typedef TReadFileFactory<FILE> ReadFILEFactory;

#endif //MEC_READ_FILE_H
