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
#ifndef STRUCTURE_FILEFORMAT_OTHERS_H
#define STRUCTURE_FILEFORMAT_OTHERS_H

#include <KERNEL_BASE/Structure_fileformat.h>
#include <KERNEL_BASE/Structure_descriptor.h>
#ifdef __cplusplus

#include <UTILS/mv_string.h>
#include <UTILS/mv_vector.h>
#include <UTILS/mv_stl_various.h>

class AppendOptions_t
{
public:
    AppendOptions_t() { vtype = VTYPE_UNKNOWN; }
    ~AppendOptions_t() {}
    void setVtype(value_type_e value) { vtype = value; }
    value_type_e getVtype() { return vtype; }
    value_type_e vtype;
};

template <typename T>
class AppendOptionsOthers_t:public AppendOptions_t
{
public:
    AppendOptionsOthers_t(string &my_skey, vector<T> &my_values, vector<string> &my_options) :
        AppendOptions_t(),
        skeyword(my_skey)
    {
        int size = (int)my_values.size();
        for (int i = 0; i < size; i++)
        {
            string opt = my_options[i];
            T value = my_values[i];
            options_values_vect.push_back(make_pair(opt, value));
        }
    }
    AppendOptionsOthers_t() {}
    ~AppendOptionsOthers_t() {}
    string &getSkeyword() { return skeyword; }
    vector<pair <string, T>> &getOptionsValuesVect() { return options_values_vect; }

public:
    string                   skeyword;
    vector<pair <string, T>> options_values_vect;
};

typedef struct ff_append_option_cell_s {
    ff_formated_cell_t        cell_part;
    vector<AppendOptions_t*> options;
} ff_append_option_cell_t;

template <typename T>
void MCDS_add_ff_cell_attributes(ff_cell_t *cell, string &skw, vector<T> &values, vector<string> &options, value_type_e a_value)
{
    if (cell == NULL)
        return;
    ff_append_option_cell_t *a_cell_p = (ff_append_option_cell_t *)cell;

    AppendOptionsOthers_t<T> *data = new AppendOptionsOthers_t<T>(skw, values, options);
    data->setVtype(a_value);
    a_cell_p->options.push_back((AppendOptions_t *)data);
}

#endif

#ifdef __cplusplus
extern "C" {
#endif

 void MCDS_new_ff_cell_app_opt(ff_cell_t **cell_pfp);
 void MCDS_free_app_opt(ff_cell_t *cell_p);
#ifdef __cplusplus
}
#endif

#endif /* STRUCTURE_FILEFORMAT_OTHERS_H */
