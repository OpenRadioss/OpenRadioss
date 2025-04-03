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

#include <iostream>
#include <UTILS/win32_utils.h>

#include "mv_header_positions.h"

#include <UTILS/str_utils.h>

//ObjectsToSubdeck_t MvHeaderPositions_t::ObjectToSubdeck;  

void MvHeaderPositions_t::addKeywordData(_HC_LONG line, _HC_LONG pos,const int subdeckIdx, string deckheader, InputInfos::IdentifierValuePairList &keywrodcommentdata, const CUserNameTypeInfo* headerdata, int linecounts)
{
	CKeywordData aMvPosition(line, pos, subdeckIdx, deckheader, headerdata, linecounts, keywrodcommentdata);
	((*myTypeComponentFilePositionsPtr)[myTypeString][myComponentIndex][myFileIndex]).push_back(aMvPosition);
}

void MvHeaderPositions_t::addCommentData(string &typestr, int comp_indx, int file_index, vector<string>& keywrodcommentdata)
{
	//CKeywordData& MvPosition = ((*myTypeComponentFilePositionsPtr)[typestr][comp_indx][file_index]);

	//MvPosition.SetCommentData(keywrodcommentdata);

}

string MvHeaderPositions_t::displayInfo() const
{
    string aString = "";
    MvTypeComponentFilePositions_t::const_iterator a_ite_beg = myTypeComponentFilePositionsPtr->begin();
    MvTypeComponentFilePositions_t::const_iterator a_ite_end = myTypeComponentFilePositionsPtr->end();
    for (MvTypeComponentFilePositions_t::const_iterator a_ite = a_ite_beg; a_ite != a_ite_end; ++a_ite)
    {
        string a_type_string = (*a_ite).first;
        aString += str_printf("\n%s  \n", a_type_string.c_str());
        const MvComponentFilePositions_t& a_sub_map = (*a_ite).second;
        MvComponentFilePositions_t::const_iterator a_sub_ite_beg = a_sub_map.begin();
        MvComponentFilePositions_t::const_iterator a_sub_ite_end = a_sub_map.end();
        for (MvComponentFilePositions_t::const_iterator a_sub_ite = a_sub_ite_beg; a_sub_ite != a_sub_ite_end; ++a_sub_ite)
        {
            
            int a_comp_index = (*a_sub_ite).first;
            aString += str_printf("  Submodel Index %d  \n", a_comp_index);
            const MvFilePositions_t& a_sub2_map = (*a_sub_ite).second;
            MvFilePositions_t::const_iterator a_sub2_ite_beg = a_sub2_map.begin();
            MvFilePositions_t::const_iterator a_sub2_ite_end = a_sub2_map.end();
            for (MvFilePositions_t::const_iterator a_sub2_ite = a_sub2_ite_beg; a_sub2_ite != a_sub2_ite_end; ++a_sub2_ite)
            {
                int a_file_index = (*a_sub2_ite).first;
                aString += str_printf("      File Index %d  ", a_file_index);
                const MvPositions_t& a_sub3_map = (*a_sub2_ite).second;
                MvPositions_t::const_iterator a_sub3_ite_beg = a_sub3_map.begin();
                MvPositions_t::const_iterator a_sub3_ite_end = a_sub3_map.end();
                bool first = true;
                for (MvPositions_t::const_iterator a_sub3_ite = a_sub3_ite_beg; a_sub3_ite != a_sub3_ite_end; ++a_sub3_ite)
                {
                    const CKeywordData& a_pos = (*a_sub3_ite);
                    const CUserNameTypeInfo* a_headerinfo = a_pos.GetHeaderInfo();
                    if (!first)
                        aString += string("                    ");

                    string akey("UNKNOWN");
                    if (a_headerinfo)
                        akey = a_headerinfo->myusername;

                    aString += str_printf("      Keyword:  %s, Username:  %s, Position:   %d,  Line No:   %d\n", a_pos.GetDeckHeader().c_str(), akey.c_str(), a_pos.GetLocation(), a_pos.GetLine());
                    first = false;
                }
            }
        }

    }
    //std::cout << aString;
    return aString;

}

MvComponentFilePositions_t* MvHeaderPositions_t::findType(string type_str)
{
	MvTypeComponentFilePositions_t::iterator a_ite =
		myTypeComponentFilePositionsPtr->find(type_str);
	if(a_ite!=myTypeComponentFilePositionsPtr->end())
	{
		MvComponentFilePositions_t* a_comp_file_pos_p =&((*a_ite).second);
		return a_comp_file_pos_p;
	}
	return NULL;
}

