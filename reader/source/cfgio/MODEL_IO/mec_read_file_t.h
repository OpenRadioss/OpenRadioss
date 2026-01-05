//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#ifndef MEC_READ_FILE_T_H
#define MEC_READ_FILE_T_H

#include "mec_read_file.h"

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

#endif //MEC_READ_FILE_T_H
