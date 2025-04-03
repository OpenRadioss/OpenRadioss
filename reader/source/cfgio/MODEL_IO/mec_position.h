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
#ifndef MEC_POSITION_H

#define MEC_POSITION_H
#include <UTILS/file_utils.h>
#include <KERNEL/cfg_kernel.h>
#include "hcio.h"
#include "meci_input_infos.h"
class HCIO_DATA_DLL_API CKeywordData {

public:
    inline CKeywordData(_HC_LONG cur_line, _HC_LONG cur_location, int subdeckIdx = -1, string deckheader="", 
                        const CUserNameTypeInfo* headerdata = nullptr, int nlines=0, InputInfos::IdentifierValuePairList keyworddata = {}) 
                        : myLine(cur_line), myLocation(cur_location), mySubdeckIdx(subdeckIdx), myLineCounts(nlines),
                        myDeckHeader(deckheader),myHeaderData(headerdata), myKeywordCommentData(keyworddata) {}

    inline ~CKeywordData() {}
public:

    inline _HC_LONG GetLine() const {
        return myLine;
    }

    inline _HC_LONG          GetLocation()  const {
        return myLocation;
    }

    inline void          SetLine(_HC_LONG a_line) {
        myLine = a_line;
    }

    inline void          SetLocation(_HC_LONG a_loc) {
        myLocation = a_loc;
    }

    inline void SetCommentData(InputInfos::IdentifierValuePairList& KeywordData) { myKeywordCommentData = KeywordData; }

    inline void          Init() {
        myLocation = 0L, myLine = 0L;
    }

    inline void          IncreaseLine(_HC_LONG a_incr) {
        myLine += a_incr;
    }

    int GetSubdeckIdx() const { return mySubdeckIdx; }

    void SetSubdeckIdx(int SDidx) { mySubdeckIdx = SDidx; }

    inline void GetCommentData(const InputInfos::IdentifierValuePairList** keywordData) const
    {
        if (keywordData)
            *keywordData = &myKeywordCommentData;
    }
    inline const CUserNameTypeInfo* GetHeaderInfo() const { return myHeaderData; }
    inline string GetDeckHeader() const { return myDeckHeader; }
    inline int GetLineCount() const { return myLineCounts; }
    inline void SetLoadedFlag(bool flag) { myIsLoaded = flag; }
    inline bool GetLoadedFlag() const { return myIsLoaded; }
public:

    inline bool operator<(const CKeywordData& pos) const { return myLocation < pos.myLocation; }

private:

    _HC_LONG            myLine;
    _HC_LONG            myLocation;
    int                 mySubdeckIdx;
    InputInfos::IdentifierValuePairList myKeywordCommentData;
    const CUserNameTypeInfo* myHeaderData;
    string                   myDeckHeader;
    int                      myLineCounts;
    bool                     myIsLoaded = false;
};

#endif
