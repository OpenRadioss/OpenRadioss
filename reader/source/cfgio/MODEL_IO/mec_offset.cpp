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

#include <UTILS/win32_utils.h>

#include <UTILS/mv_stl_various.h>
#include <UTILS/mv_string.h>
#include <UTILS/mv_cstring.h>
#include <KERNEL/mv_utils.h>
#include <KERNEL/mv_type.h>

#include "mec_offset.h"

void Loc_get_min_max_type(int *type_min, int*type_max)
{
	bool doOnce = true;
	const MvObjectTypeSet_t a_set_all_types = HCDI_get_object_type_set();
	MvObjectTypeSet_t::const_iterator a_ite_beg = a_set_all_types.begin();
	MvObjectTypeSet_t::const_iterator a_ite_end = a_set_all_types.end();
	MvObjectTypeSet_t::const_iterator a_ite;

	for(a_ite=a_ite_beg; a_ite!=a_ite_end; ++a_ite)
	{
		int a_value = (int)(*a_ite);
		if(doOnce)
		{
			*type_min=a_value;
			*type_max=a_value;
			doOnce = false;
		}
		else
		{
			if(a_value<*type_min)
				*type_min = a_value;
			if(a_value>*type_max)
				*type_max = a_value;
		}
	}
}

typedef vector<int>        LocVectInt_t;     
typedef string             LocString_t;

MECOffset::MECOffset():
myOffsetValues((PseudoVectInt_t*)(new LocVectInt_t())),
myLastString((PseudoString_t*)(new LocString_t()))
{
	myLastIndex=-1;
	Loc_get_min_max_type(&myMinType, &myMaxType);
	myNbTypes = (myMaxType-myMinType)+1;

	LocVectInt_t* a_offset_values_p = (LocVectInt_t*)(myOffsetValues);
	a_offset_values_p->reserve(myNbTypes);
	for(int i=0; i<myNbTypes; i++)
		a_offset_values_p->push_back(0);

}


MECOffset::~MECOffset()
{
	LocVectInt_t* a_offset_values_p = (LocVectInt_t*)(myOffsetValues);
	delete a_offset_values_p;
}


void MECOffset::SetOffset(const char* a_type, int offset_value)
{
	int a_loc_index=GetIndex(a_type);
	SetOffsetValue(a_loc_index, offset_value);

}

int MECOffset::GetOffset(const char* a_type) const
{
	int an_offset = 0;
	int a_loc_index=GetIndex(a_type);
	return GetOffsetValue(a_loc_index);
}


void MECOffset::Add(const MECOffset* an_off_p)
{
	for(int i=0; i<myNbTypes; i++)
	{
		int this_off = GetOffsetValue(i);
		int val_off  = an_off_p->GetOffsetValue(i);

		SetOffsetValue(i, val_off+this_off);
	}
}


int MECOffset::GetIndex(const char* type) const
{
	LocString_t* lastString =(LocString_t*)(myLastString);
	if(strcmp(lastString->c_str(), type)==0)
		return myLastIndex;
	obj_type_e aType = MV_get_type(type); 
	int index = ((int)aType)-myMinType;
	*lastString = type;

	myLastIndex = index;
	
	return index;
}


int MECOffset::GetOffsetValue(int a_loc_index) const
{
	const LocVectInt_t& a_offset_values = (*((LocVectInt_t*)myOffsetValues));

	if(a_loc_index>=0 && a_loc_index<a_offset_values.size())
		return a_offset_values[a_loc_index];
	return 0;
}

void MECOffset::SetOffsetValue(int a_loc_index, int offset_value)
{
	LocVectInt_t* a_offset_values_p = (LocVectInt_t*)(myOffsetValues);

	if(a_loc_index>=0 && a_loc_index<a_offset_values_p->size())
		(*a_offset_values_p)[a_loc_index]=offset_value;
}


const char* MECOffset::GetOffsetType(int index) const
{
	if(myLastIndex==index)
		return (const char*)myLastString;
	int loc_index = index+myMinType;
	const char* a_type = (MV_get_type((obj_type_e)loc_index)).c_str();
	return a_type;
}

int MECOffset::GetNbOffsets() const
{
	LocVectInt_t* a_offset_values_p = (LocVectInt_t*)(myOffsetValues);
	if(a_offset_values_p)
		return (int)(a_offset_values_p->size());
	return 0;
}
