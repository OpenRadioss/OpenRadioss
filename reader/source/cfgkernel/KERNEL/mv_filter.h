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
#ifndef MV_FILTER_H
#define MV_FILTER_H
#include <UTILS/mv_string.h>
#include <UTILS/vector_utils.h>

typedef vector<string> StringVect_t;

typedef enum FilterType_e
{
    FILTER_UNKNOWN = 0,
    FILTER_SUBTYPE,
    FILTER_ATTRIBUTE_VALUE,
    FILTER_ENTITY_OPERATION,
    FILTER_ENTITY_CONTAINED_OPERATION
} FilterType_t;

class CFilterCriteria
{
public:
    CFilterCriteria(FilterType_t type, string &attrib, string &value, string &criteria, string &unit, string& messg);
    FilterType_t getFilterType() { return p_type; }
    const string &getAttribString() const { return p_attrib; }
    const string &getValueString() const { return p_value; }
    const string &getCriteriaString() const { return p_criteria; }
    const string &getUnitString() const { return p_unit; }
    const string& getMessgString() const { return p_messg; }
private:
    FilterType_t p_type;
    string p_attrib;
    string p_value;
    string p_criteria;
    string p_unit;
    string p_messg;
};

class EntityFilter
{
public:
    EntityFilter(int nb, StringVect_t &attribs, StringVect_t &values, StringVect_t &criterias, StringVect_t &units, StringVect_t& messgs);
    ~EntityFilter();
    vector<CFilterCriteria *> &getFilterVect() { return p_filter_vect; }
private:

    vector<CFilterCriteria *> p_filter_vect;
};

#endif /*MV_FILTER_H*/
