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
#include <string>
#include "hw_cfg_reader_message_pool.h"

static std::string p_emptystring;

void HWCFGReaderMessagePool::Add(unsigned int id, const std::string &description,
                                 const std::string &title, const std::string &solution)
{
    p_descriptions[id] = description;
    p_titles[id] = title;
    p_solutions[id] = solution;
}

void HWCFGReaderMessagePool::AddDescription(unsigned int id, const std::string &description)
{
    p_descriptions[id] = description;
}

void HWCFGReaderMessagePool::AddTitle(unsigned int id, const std::string &title)
{
    p_titles[id] = title;
}

void HWCFGReaderMessagePool::AddSolution(unsigned int id, const std::string &solution)
{
    p_solutions[id] = solution;
}

const std::string& HWCFGReaderMessagePool::GetDescription(unsigned int id) const
{
    if(p_descriptions.count(id)) return p_descriptions.at(id);
    else return p_emptystring;
}

const std::string& HWCFGReaderMessagePool::GetTitle(unsigned int id) const
{
    if(p_titles.count(id)) return p_titles.at(id);
    else return p_emptystring;
}

const std::string& HWCFGReaderMessagePool::GetSolution(unsigned int id) const
{
    if(p_solutions.count(id)) return p_solutions.at(id);
    else return p_emptystring;
}
