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

#include <string>
#include <hcioi_utils.h>
#include "meci_syntax_infos.h"
#include "HCDI/hcdi_multicfgkernelmgr.h"
#include "mec_subdeck.h"



using namespace std;

char* strdupN(const char* strp, size_t n) 
{
    // allocate memory for the new string
    char* newString = (char*)malloc((n + 1) * sizeof(char));

    // copy the first n characters from the original string
    strncpy(newString, strp, n);
    newString[n] = '\0'; // null-terminate the new string

    return newString;
}

void removeLeadingBlanks(char* str) 
{
    size_t len = strlen(str);
    size_t offset = strspn(str, " \t"); // get the number of leading whitespace characters

    // shift the string to remove leading characters
    memmove(str, str + offset, len - offset + 1);
}


SolverSyntaxInfos::SolverSyntaxInfos()
{
    const CFGKernel* a_cfg_kernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (nullptr == a_cfg_kernel)
        throw "CFG Kernel Not Loaded";

    myAppMode = a_cfg_kernel->getUserProfile();


    /*these solver specific information can be loaded from cfg kernel or any other external file*/
    switch (myAppMode)
    {
        case HCDI_SOLVER_RADIOSS:
        {
            static vector<const char* > a_headers{ "/" };
            myheader              = &a_headers;

            const char* c_lst[] = { "#", "$" };
            mysizecomment = static_cast<int>(sizeof(c_lst) / sizeof(c_lst[0]));
            mycomment = new const char* [mysizecomment];
            mycommentlen = new int[mysizecomment];
            for (int i = 0; i < mysizecomment; i++)
            {
                mycomment[i] = strdup(c_lst[i]);
                mycommentlen[i] = static_cast<int>(strlen(mycomment[i]));
            }


            myfreeformat          = ',';
            myparameterSymbol     = "&";
            myInitialOffset       = 0;
            myCellLength          = 10;
            myLineLength          = 100;
            myHeaderKeyCellLength = 0;
            myHeaderSeparator     = '/';
            myBaseFormatType      = io_types::FORMAT_SHORT;
        }
        break;
        case HCDI_SOLVER_LSDYNA:
        {
            static vector<const char* > a_headers{ "*" };
            myheader              = &a_headers;


            const char* c_lst[] = { "$" };
            mysizecomment = static_cast<int>(sizeof(c_lst) / sizeof(c_lst[0]));
            mycomment = new const char* [mysizecomment];
            mycommentlen = new int[mysizecomment];
            for (int i = 0; i < mysizecomment; i++)
            {
                mycomment[i] = strdup(c_lst[i]);
                mycommentlen[i] = static_cast<int>(strlen(mycomment[i]));
            }


            myfreeformat          = ',';
            myparameterSymbol     = "&";
            myInitialOffset       = 0;
            myCellLength          = 10;
            myLineLength          = 80;
            myHeaderKeyCellLength = 0;
            myHeaderSeparator     = '_';
            myBaseFormatType      = io_types::FORMAT_SHORT;
        }
        break;
        default:
        {
            static vector<const char* > a_headers{ "" };
            myheader              = &a_headers;

            myfreeformat          = '\0';
            myparameterSymbol     = "";
            myInitialOffset       = 0;
            myCellLength          = 0;
            myLineLength          = 0;
            myHeaderKeyCellLength = 0;
            myHeaderSeparator     = '\0';
            myBaseFormatType      = io_types::FORMAT_SHORT;
        }
        break;
    }

    myheadersize = 0;
    if (myheader)
        myheadersize = (int)myheader->size();
    myheaderlen = new std::vector<int>();

    for (int i = 0; i < myheadersize; i++)
    {
        if ((*myheader)[i])
            myheaderlen->push_back((int)strlen((*myheader)[i]));
        else
            myheaderlen->push_back(0);
    }
}


bool SolverSyntaxInfos::isHeader(const char* buffer, char** keyword_p) const
{
    if (buffer == NULL) return false;

    bool  isheader = true;
    int offsetSize = -1;
    if (myheadersize > 0)
    {
        int i = 0;
        
        if (myIsHeaderFromStart)
        {
            offsetSize = 0;
            const char* stripped = buffer;
            while (*stripped == ' ')
            {
                ++stripped;
                offsetSize++;
            }
            for (i = 0; i < myheadersize; i++)
            {
                if (!strncmp((*myheader)[i], stripped, (*myheaderlen)[i]))
                {
                    offsetSize += (*myheaderlen)[i];
                    break;
                }
            }
            if (i == myheadersize)
                return false;
        }
        else
        {
            for (i = 0; i < myheadersize; i++)
            {
                if (strstr(buffer, (*myheader)[i]))
                    break;
            }
            if (i == myheadersize)
                return false;
        }
    }
    else
    {
        int count = 0;
        const char* a_header_char_p = buffer;
        while (*a_header_char_p != '\0')
        {
            if (IsSpaceORContinueChars(a_header_char_p))
                count++;
            else
                break;

            ++a_header_char_p;
        }
        if ((*a_header_char_p == ',' && count <= myHeaderKeyCellLength) || (count >= myHeaderKeyCellLength))
            return false;
    }

    if (NULL != keyword_p) {

        if(myheadersize > 0 && offsetSize > 0)
           *keyword_p = strdup(buffer + offsetSize);
        else if(myheadersize <= 0 && myHeaderKeyCellLength > 0)
        {
            *keyword_p = strdupN(buffer, myHeaderKeyCellLength); /*incase header is keyword itself*/
            removeLeadingBlanks(*keyword_p);
        }
        else /*default case*/
        {
            *keyword_p = strdup(buffer);
            removeLeadingBlanks(*keyword_p);
        }
        char* a_char_p = strchr(*keyword_p, ' ');
        if (a_char_p != NULL) *a_char_p = '\0';
    }
    return true;
}
io_types::format_type_e SolverSyntaxInfos::updateLineFormatType(const char* line)
{ 
    myLineFormatType = io_types::FORMAT_UNDEFINED;
    return myLineFormatType;
}
bool SolverSyntaxInfos::IsSpaceORContinueChars(const char* cp) const
{
    if (*cp == ' ')
        return true;
    return false;
}

const char* SolverSyntaxInfos::GetFormatSize(const char* fmt_p, bool is_free_size, int& fmt_size) const
{
    if (is_free_size || !fmt_p)
    {
        fmt_size = 0;
        return fmt_p;
    }


    io_types::format_type_e  fmt_type = getFormatType();


    int base_cell_width = myCellLength, long_cell_width = base_cell_width * 2;

    static const string fmt_base_d = "%" + to_string(base_cell_width) + "d";
    static const string fmt_base_s = "%" + to_string(base_cell_width) + "s";
    static const string fmt_base_s_l = "%" + string("-") + to_string(base_cell_width) + "s";
    static const string fmt_base_lg = "%" + to_string(base_cell_width) + "lg";
    static const string fmt_base_lf = "%" + to_string(base_cell_width) + "lf";

    static const string fmt_d = "%d";
    static const string fmt_s = "%s";
    static const string fmt_s_l = "%-s";
    static const string fmt_lg = "%lg";
    static const string fmt_lf = "%lf";

    static const string fmt_long_d = "%" + to_string(long_cell_width) + "d";
    static const string fmt_long_s = "%" + to_string(long_cell_width) + "s";
    static const string fmt_long_s_l = "%" + string("-") + to_string(long_cell_width) + "s";
    static const string fmt_long_lg = "%" + to_string(long_cell_width) + "lg";
    static const string fmt_long_lf = "%" + to_string(long_cell_width) + "lf";
    static const string fmt_16lf = "%" + to_string(long_cell_width) + "lf";
    static const string fmt_16lg = "%" + to_string(long_cell_width) + "lg";


    if (fmt_p[1] == 'F') /*in-case %F10d: fixed format is defined*/
    {
        /*static char fmt_fixed[5];*/
        //int a_size = atoi(fmt.substr(2, fmt.find_first_of(".sdilfeg") - 1).c_str());
        //fmt_size = a_size < 0 ? (-a_size) : a_size;

        const char* start = fmt_p + 2;
        const char* end = start;

        while (*end && *end != '.' && *end != 's' && *end != 'd' && *end != 'i' &&
            *end != 'l' && *end != 'f' && *end != 'e' && *end != 'g')
        {
            ++end;
        }

        int a_size = atoi(start);
        fmt_size = a_size < 0 ? -a_size : a_size;

        size_t len = strlen(fmt_p);

        bool left_align = false;
        if (len >= 3 && fmt_p[2] == '-')
            left_align = true;

        if (fmt_p[len - 1] == 'd')
        {
            if (fmt_size == 0)
                fmt_p = fmt_d.c_str();
            else if (fmt_size == base_cell_width)
                fmt_p = fmt_base_d.c_str();
        }
        else if (fmt_p[len - 1] == 'f')
        {
            if (fmt_size == 0)
                fmt_p = fmt_lf.c_str();
            else if (fmt_size == base_cell_width)
                fmt_p = fmt_base_lf.c_str();
        }
        else if (fmt_p[len - 1] == 'g')
        {
            if (fmt_size == 0)
                fmt_p = fmt_lg.c_str();
            else if (fmt_size == base_cell_width)
                fmt_p = fmt_base_lg.c_str();
        }
        else if (fmt_p[len - 1] == 's')
        {
            if (!left_align)
            {
                if (fmt_size == 0)
                    fmt_p = fmt_s.c_str();
                else if (fmt_size == base_cell_width)
                    fmt_p = fmt_base_s.c_str();
            }
            else
            {
                if (fmt_size == 0)
                    fmt_p = fmt_s_l.c_str();
                else if (fmt_size == base_cell_width)
                    fmt_p = fmt_base_s_l.c_str();
            }
        }
        return fmt_p;
    }

    if (!IsScalableFormat() || !fmt_type)
    {
        const char* start = fmt_p + 1;
        const char* end = start;
        while (*end && *end != '.' && *end != 's' && *end != 'd' && *end != 'i' &&
            *end != 'l' && *end != 'f' && *end != 'e' && *end != 'g')
        {
            ++end;
        }
        fmt_size = atoi(start);
        fmt_size = fmt_size < 0 ? (-fmt_size) : fmt_size;
        return fmt_p;
    }
    const string& fmt = fmt_p;
    switch (fmt_type)
    {
    case io_types::format_type_e::FORMAT_SHORT:
    {
        if (fmt == "%d")
        {
            fmt_size = base_cell_width;
            return fmt_base_d.c_str();
        }
        else if (fmt == "%lf") /*for %10lf it is same*/
        {
            fmt_size = base_cell_width;
            return fmt_16lf.c_str();
        }
        else if (fmt == "%lg")
        {
            fmt_size = base_cell_width;
            return fmt_16lg.c_str();
        }
    }
    break;
    case io_types::format_type_e::FORMAT_LSDYNA_SHORT_I10:
    {
        if (fmt == "%d" || fmt == "%8d")
        {
            fmt_size = 10;
            return fmt_base_d.c_str();
        }
        else if (fmt == "%lf")
        {
            fmt_size = 16;
            return fmt_16lf.c_str();
        }
        else if (fmt == "%lg")
        {
            fmt_size = 16;
            return fmt_16lg.c_str();
        }
    }
    break;
    case io_types::format_type_e::FORMAT_LONG:
    {
        if (fmt == "%d" || fmt == "%8d" || fmt == fmt_base_d)
        {
            fmt_size = long_cell_width;
            return fmt_long_d.c_str();
        }
        else if (fmt == "%lf" || fmt == "%16lf" || fmt == fmt_base_lf)
        {
            fmt_size = long_cell_width;
            return fmt_long_lf.c_str();
        }
        else if (fmt == "%lg" || fmt == "%16lg" || fmt == fmt_base_lg)
        {
            fmt_size = long_cell_width;
            return fmt_long_lg.c_str();
        }
        else if (fmt == fmt_base_s)
        {
            fmt_size = long_cell_width;
            return fmt_base_s.c_str();
        }
        else if (fmt == fmt_base_s_l)
        {
            fmt_size = long_cell_width;
            return fmt_base_s_l.c_str();
        }
    }
    break;
    default:
        break;
    }
    
    int a_size = atoi(fmt.substr(1, fmt.find_first_of(".sdilfeg") - 1).c_str());
    fmt_size = a_size < 0 ? (-a_size) : a_size;

    return fmt_p;
}

io_types::format_type_e SolverSyntaxInfos::getFormatType()  const
{
    if (io_types::FORMAT_UNDEFINED != myLineFormatType)
        return myLineFormatType;

    if (MECSubdeck::curSubdeckIdx >= 0 && MECSubdeck::curSubdeckIdx < myDeckFormat.size())
    {
        io_types::format_type_e fmt_type = myDeckFormat[MECSubdeck::curSubdeckIdx];

        return fmt_type == io_types::FORMAT_UNDEFINED ? myBaseFormatType : fmt_type;
    }

    return myBaseFormatType;
}



io_types::format_type_e SolverSyntaxInfos::getFormatType(MECReadFile* fp)
{
    if (!fp)
        return getFormatType();


    if (getAppMode() == HCDI_SOLVER_LSDYNA)
    {
        bool a_ok = true;
        int subdeck_index = MECSubdeck::curSubdeckIdx;

        if(myDeckFormat.size() <= subdeck_index)
            myDeckFormat.resize(subdeck_index + 1);

        if (subdeck_index >= 0)
            myDeckFormat[subdeck_index] = getFormatType();
        else
        {
            subdeck_index = 0;
            myDeckFormat.push_back(getFormatType());
        }
    }

    return getFormatType();
}

void SolverSyntaxInfos::updateFormatType(int subdeck_index, io_types::format_type_e fmt_type)
{
    if (myDeckFormat.size() <= subdeck_index)
        myDeckFormat.resize(subdeck_index + 1);
    if (subdeck_index >= 0)
    {
        if((int)myDeckFormat.size() == 0)
            myDeckFormat.push_back(fmt_type);
        else
            myDeckFormat[subdeck_index] = fmt_type;
    }
    else
    {
        myDeckFormat.push_back(fmt_type);
    }
}
