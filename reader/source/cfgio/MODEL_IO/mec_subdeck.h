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
#ifndef MEC_SUBDECK_H
#define MEC_SUBDECK_H

#include <UTILS/mv_stl_various.h>
#include <HCDI/hcdi_mec_pre_object.h>
#include <KERNEL/mv_utils.h>
#include <KERNEL/mv_type.h>
#include "io_types.h"
#include "hcio.h"


class MECSubdeck;
typedef vector<MECSubdeck*> subdeckVector;

// can be better name for this class IncludeContainer or other??
class HCIO_DATA_DLL_API MECSubdeck
{
private:

    //file or submodel... name
    string myName;
    //relative file name
    string myRelativeName;
    //subtype 
    object_type_e mySubtype;
    //parent index
    int parentIdx;
    //submodelId
    int submodelId;
    //unitId
    int unitId;
    //Version_Number
    int versionNbr;
    //Card Header
    string myheader;
    //
    IMECPreObject* myPreObject = NULL;
    io_types::format_type_e mydeckFormat = io_types::FORMAT_UNDEFINED;
public:
    MECSubdeck(string name, object_type_e subtype, int Id = 0, int uId = 0, int vNbr = 0, IMECPreObject* PreObject = NULL, string header="", 
                const std::vector<const char*> *vec = nullptr, io_types::format_type_e deckformat = io_types::FORMAT_UNDEFINED);

    ~MECSubdeck();

    // Get methods
public:
    const string& GetName()const { return myName; }
    const string& GetFileRelativeName()const { return myRelativeName; }
    object_type_e GetSubtype() { return mySubtype; }
    int GetSubmodelId() { return submodelId; }
    int GetParentIdx() { return parentIdx; }
    int GetUnitID() const { return unitId; }
    int GetVersNbr() const { return versionNbr; }
    IMECPreObject* GetPreObject() { return  myPreObject; }
    string getSubdeckInfo();
    string GetHeaderStr() { return myheader; }
    void SetHeaderStr(string str) { myheader = str; }
    //Set methods
public:
    void SetName(string name) { myName = myName; }
    void SetFileRelativeName(string rname) { myRelativeName = rname; }
    void SetSubmodelId(int Id) { submodelId = Id; }

    void SetUnitId(int unit_id) { unitId = unit_id; }
    void SetVersNbr(int vers_nbr) { versionNbr = vers_nbr; }
    void SetPreObject(IMECPreObject* preobj) { myPreObject = preobj; }

    void SetFileFormatType(io_types::format_type_e fmt_type) { mydeckFormat = fmt_type;  }
    io_types::format_type_e GetFileFormatType() { return mydeckFormat; }

    //static methods
public:
    static int curSubdeckIdx;
    static subdeckVector mySubdeckVector;
    static int PopSubDeck();
    static void clearSubdeckVector();
};
#endif
