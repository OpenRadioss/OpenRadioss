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

#ifndef MV_HEADER_POSITIONS_H
#define MV_HEADER_POSITIONS_H

#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>

#include "mec_position.h"
#include "meci_input_infos.h"
#include "hcio.h"

#if defined _WIN32 || defined WIN32 
#pragma warning(disable:4251)    
#endif


#define MV_NEW_POS
#ifdef MV_NEW_POS
typedef vector<CKeywordData> MvPositions_t;
typedef map<int, MvPositions_t> MvFilePositions_t; /* int = index of file */
typedef map<int, MvFilePositions_t> MvComponentFilePositions_t; /* int = index of submodel */
typedef map<string, MvComponentFilePositions_t> MvTypeComponentFilePositions_t;
//typedef multimap<string,int> ObjectsToSubdeck_t;

class HCIO_DATA_DLL_API MvHeaderPositions_t {
private:
    MvTypeComponentFilePositions_t* myTypeComponentFilePositionsPtr;
    int myFileIndex;
    int myComponentIndex;
    string myTypeString;

public:

    inline MvHeaderPositions_t() :
        myTypeComponentFilePositionsPtr(new MvTypeComponentFilePositions_t()),
        myFileIndex(0),
        myComponentIndex(0),
        myTypeString("") {  }
    ~MvHeaderPositions_t()
    {
        ClearAllComponentFilePosition();
        if (myTypeComponentFilePositionsPtr)
            delete myTypeComponentFilePositionsPtr;
    }

    MvComponentFilePositions_t* findType(string type);
    inline void       setFileIndex(const int anIndex) { myFileIndex = anIndex; }
    inline const int  getFileIndex() const { return myFileIndex; }

    inline void       setComponentIndex(const int anIndex) { myComponentIndex = anIndex; }
    inline const int  getComponentIndex() const { return myComponentIndex; }

    inline void       setTypeString(const string aString) { myTypeString = aString; }
    inline const string getTypeString() const { return myTypeString; }

    void addKeywordData(_HC_LONG line, _HC_LONG pos, const int subdeckIdx, string deckheader, InputInfos::IdentifierValuePairList& keywrodcommentdata, const CUserNameTypeInfo* headerdata, int linecounts);
    void addCommentData(string& typestr, int comp_indx, int file_index, vector<string>& keywrodcommentdata);
    string displayInfo() const;
    void ClearAllComponentFilePosition() { myTypeComponentFilePositionsPtr->clear(); }
};
#endif
#endif //MV_HEADER_POSITIONS_H
