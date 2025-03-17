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
#include <UTILS/mv_cstring.h>


#include <UTILS/win32_utils.h>

#include <UTILS/mv_stl_various.h>
#include <UTILS/memory_utils.h>
#include <KERNEL/mv_utils.h>
#include <KERNEL/mv_type.h>
#include <UTILS/set_utils.h>

#include <UTILS/mv_iostream.h>
#include "mec_component.h"
#include "mec_offset.h"


#define NB_ORIG_OFFSET_MAX 8  

MECComponent::MECComponent(const char* atitle, int a_id, int a_unit_id, int a_version) :
myTitle(strdup(atitle)),
myComponentID(a_id),
myUnitID(a_unit_id),
myVersion(a_version),
myGlobalOffset( (MECOffset*)new MECOffset()),
myLocalOffset( (MECOffset*)new MECOffset()),
myOriginalOffset(NULL), 
myNbOriginalOffset(0)
{
	myParentIndex = -1;
	myFileIndex = -1;
    myOriginalOffset=(int*)mymalloc(NB_ORIG_OFFSET_MAX*sizeof(int)); 
}

MECComponent::~MECComponent()
{
	myfree(myTitle);
	delete ((MECOffset*)myGlobalOffset);
	delete ((MECOffset*)myLocalOffset);
    if(myOriginalOffset)
        myfree(myOriginalOffset); 
}


void MECComponent::SetTitle(const char* a_name)
{
    if (myTitle)
        myfree(myTitle);
	myTitle=strdup(a_name);
}


void MECComponent::	SetGlobalOffsetValue(const char* a_type, int an_offset)
{
	MECOffset* a_offset_p=((MECOffset*)myGlobalOffset);
	if(a_offset_p)
		a_offset_p->SetOffset(a_type,an_offset);
}

int  MECComponent::GetGlobalOffsetValue(const char* a_type) const
{
	const MECOffset& a_offset=(*((MECOffset*)myGlobalOffset));
	return a_offset.GetOffset(a_type);
}

void MECComponent::	SetLocalOffsetValue(const char* a_type, int an_offset)
{
	MECOffset* a_offset_p=((MECOffset*)myLocalOffset);
	if(a_offset_p)
		
	  
	  a_offset_p->SetOffset(a_type,an_offset);
}

int  MECComponent::GetLocalOffsetValue(const char* a_type) const
{
	const MECOffset& a_offset=(*((MECOffset*)myLocalOffset));
	return a_offset.GetOffset(a_type);
}

void MECComponent::	SetOriginalOffsetValue(int itype, int an_offset)
{
	myOriginalOffset[itype]=an_offset;
}


int  MECComponent::GetOriginalOffsetValue(int itype) const
{
    return myOriginalOffset[itype];
}



void MECComponent::InitGlobalOffset(const MECComponent* a_comp_p)
{
	MECOffset* this_glob_off=(MECOffset*)GetGlobalOffset();
	MECOffset* comp_glob_off=(MECOffset*)((a_comp_p)->GetGlobalOffset());
	this_glob_off->Add(comp_glob_off);
	#if 0
	cout<<"InitGlobalOffset de "<<myTitle<<endl;
	int nbOffType =GetNbOffsetTypes();
	for (int idbg=0; idbg<nbOffType; idbg++)
	{
		const char* otype = GetOffsetType(idbg);
		cout<<"type   ["<<idbg<<"] = "<<otype<<" loc = "<<GetLocalOffsetValue(otype)<<" glob = "<<GetGlobalOffsetValue(otype)<<endl;
	}
#endif
}
	

void MECComponent::ComputeGlobalOffset()
{
	MECOffset* this_glob_off=(MECOffset*)GetGlobalOffset();
	MECOffset* this_loc_off=(MECOffset*)GetLocalOffset();
	this_glob_off->Add(this_loc_off);
#if 0
	cout<<"ComputeGlobalOffset de "<<myTitle<<endl;
	int nbOffType =GetNbOffsetTypes();
	for (int idbg=0; idbg<nbOffType; idbg++)
	{
		const char* otype = GetOffsetType(idbg);
		cout<<"type   ["<<idbg<<"] = "<<otype<<" loc = "<<GetLocalOffsetValue(otype)<<" glob = "<<GetGlobalOffsetValue(otype)<<endl;
	}
#endif

}



int  MECComponent::GetNbOffsetTypes() const
{
	MECOffset* glob_off=(MECOffset*)GetGlobalOffset();
	if(glob_off)
		return glob_off->GetNbOffsets();
	return 0;
}



const char* MECComponent::GetOffsetType(int index) const
{
	MECOffset* glob_off=(MECOffset*)GetGlobalOffset();
	if(glob_off)
		return glob_off->GetOffsetType(index);
	return "";
}

