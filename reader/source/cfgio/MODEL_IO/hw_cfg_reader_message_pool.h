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




////////////////////////////////////////////////////////////////////////////////////

#if !defined(HWCFGReaderMessagePool__INCLUDED_)
#define HWCFGReaderMessagePool__INCLUDED_

#include "hcio.h"

#include <map>
#include <string>
#pragma warning(push)
#pragma warning(disable: 4251) // STL containers don't need DLL interface when private
class HCIO_DATA_DLL_API HWCFGReaderMessagePool
{
public:
    void Add(unsigned int id, const std::string &description,
             const std::string &title = "", const std::string &solution = "");

    void AddDescription(unsigned int id, const std::string &description);
    void AddTitle(unsigned int id, const std::string &title);
    void AddSolution(unsigned int id, const std::string &solution);

    const std::string& GetDescription(unsigned int id) const;
    const std::string& GetTitle(unsigned int id) const;
    const std::string& GetSolution(unsigned int id) const;

    friend class HWCFGReaderMessageList;

private:
    std::map<unsigned int, std::string> p_descriptions;
    std::map<unsigned int, std::string> p_titles;
    std::map<unsigned int, std::string> p_solutions;
};
#pragma warning(pop)

#endif //! !defined(HWCFGReaderMessagePool__INCLUDED_)
