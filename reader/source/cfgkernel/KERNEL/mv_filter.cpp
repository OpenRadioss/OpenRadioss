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
#include "mv_filter.h"

EntityFilter::EntityFilter(int nb, StringVect_t &attribs, StringVect_t &values, StringVect_t &criterias, StringVect_t &units, StringVect_t& messgs)
{
    int nb_attribs = (int)attribs.size();
    int nb_values = (int)values.size();
    int nb_criterias = (int)criterias.size();
    int nb_units = (int)units.size();
    int nb_mssgs = (int)messgs.size();

    for (int i = 0; i < nb; i++)
    {
        string a_attrib = "";
        string a_value = "";
        string a_criteria = "";
        string a_unit = "";
        string a_mssg = "";
        if (i < nb_attribs)
        {
            a_attrib = attribs[i];
        }

        if (i < nb_values)
        {
            a_value = values[i];
        }

        if (i < nb_criterias)
        {
            a_criteria = criterias[i];
        }

        if (i < nb_units)
        {
            a_unit = units[i];
        }

        if (i < nb_mssgs)
        {
            a_mssg = messgs[i];
        }
        FilterType_t type = FILTER_UNKNOWN;
        if (a_attrib == "EXCLUSION" || a_attrib == "INCLUSION")
        {
            type = FILTER_ENTITY_CONTAINED_OPERATION;
        }
        else if (a_attrib == "COMBINE" && (a_value == "AND" || a_value == "OR"))
        {
            type = FILTER_ENTITY_OPERATION;
        }
        else
        {
            type = FILTER_ATTRIBUTE_VALUE;
        }
        CFilterCriteria *criteria = new CFilterCriteria(type, a_attrib, a_value, a_criteria, a_unit, a_mssg);
        p_filter_vect.push_back(criteria);
    }
}

EntityFilter::~EntityFilter()
{
    int size = (int)p_filter_vect.size();
    for (int i = 0; i < size; i++)
    {
        CFilterCriteria *criteria = p_filter_vect[i];
        delete criteria;
    }
    p_filter_vect.clear();
}

CFilterCriteria::CFilterCriteria(FilterType_e type, string &attrib, string &value, string &criteria, string &unit, string &messg):
    p_type(type),
    p_attrib(attrib),
    p_value(value),
    p_criteria(criteria),
    p_unit(unit),
    p_messg(messg)
{
}

