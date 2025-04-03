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

#ifndef MEC_COMPONENT_H
#define MEC_COMPONENT_H
#include "hcio.h"
#define PseudoOffset_t void

class HCIO_DATA_DLL_API MECComponent
{
private:
    char* myTitle;
    int  myComponentID;
    int  myParentIndex;
    int  myUnitID;  
    int  myVersion;  
    PseudoOffset_t* myGlobalOffset;
    PseudoOffset_t* myLocalOffset;
    int* myOriginalOffset; 
    int  myNbOriginalOffset; 
	int  myFileIndex;
public:
	MECComponent(const char* a_title, int a_id, int a_unit_id, int a_version=0); 

	~MECComponent();

	inline void SetID(int an_id) {myComponentID=an_id;}
	inline int GetId() const     {return myComponentID;}
    
    inline void SetUnitId(int a_unit_id) {myUnitID=a_unit_id;}
    inline int GetUnitID() const         {return myUnitID;}
    inline int GetVersion() const     {return myVersion;}
    
	void SetTitle(const char* a_name);
	inline const char* GetTitle() const {return myTitle;} 
	
	void SetGlobalOffsetValue(const char* a_type, int an_offset);
	int  GetGlobalOffsetValue(const char* a_type) const;

	void SetLocalOffsetValue(const char* a_type, int an_offset); 
	int  GetLocalOffsetValue(const char* a_type) const;

    int  GetNbOriginalOffset() const {return myNbOriginalOffset;} 
    void SetNbOriginalOffset(int nb)  {myNbOriginalOffset=nb;} 
    void SetOriginalOffsetValue(int itype, int an_offset); 
	int  GetOriginalOffsetValue(int itype) const; 

	inline void SetParentIndex(int a_parent_index) {myParentIndex = a_parent_index;}
	inline int  GetParentIndex() const {return myParentIndex;}

	inline void SetFileIndex(int a_file_index) {myFileIndex = a_file_index;}
	inline int  GetFileIndex() const {return myFileIndex;}
         
	void InitGlobalOffset(const MECComponent* a_comp_p);

	void ComputeGlobalOffset();

	inline PseudoOffset_t* GetGlobalOffset() const {return myGlobalOffset;}
	
	inline PseudoOffset_t* GetLocalOffset() const {return myLocalOffset;}

	int GetNbOffsetTypes() const;

	const char* GetOffsetType(int index) const;
	
};
#endif
