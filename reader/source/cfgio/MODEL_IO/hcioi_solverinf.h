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
#ifndef HCIOI_SOLVERINF_H
#define HCIOI_SOLVERINF_H
#include "hcio.h"
#include <limits.h>
#include <float.h>
#include "HCDI/hcdi_mv_descriptor.h"
#include "HCDI/hcdi_mec_pre_object.h"

class hwHCSolverInf
{
public: 
    //@{
    /** Constructor */
    hwHCSolverInf() {};
    /** Destructor */
    virtual ~hwHCSolverInf() {};
    //@}

public: 
    //@{
    virtual void GetEntityPreobject(const IDescriptor *descrp,  IMECPreObject *preObjectOut, map< int, vector<MvIKeywordList_t> >  *mapTypeKeywordLst=NULL, MvIKeywordList_t *iden_lst=NULL,
                                    MvIKeywordSet_t *main_ikws=NULL, bool has_object_name = false, bool has_parameter=true,  bool has_offset = true, 
                                    bool updatehmcomment = true, const unsigned int subobj_indx = UINT_MAX) const = 0;
    virtual void UpdatePreObject(IMECPreObject &pre_object, const IDescriptor *descrp, const string &attrib_to_update, const string &dataname_ent,
                                                            string &dataname, unsigned int row = UINT_MAX, unsigned int coloumn = UINT_MAX) const = 0;
    virtual void UpdateProbjectFromEntType(IMECPreObject &pre_object, const IDescriptor *descrp, const string &attrib_to_update, unsigned int ent_type,
                                                                      string &dataname, unsigned int row = UINT_MAX, unsigned int coloumn = UINT_MAX) const = 0;
    virtual bool UpdatePreObjectValue(IMECPreObject &preobject, const IDescriptor *descrp, const string &dataname, int index = -1) const = 0;

    virtual double EvaluateDataCode(const std::string &expression) = 0 ;
    virtual unsigned int GetEntityNameGetType(const std::string &type) = 0;
    virtual unsigned int GetEntityColor(unsigned int entity, void *entity_p) = 0;
    virtual std::string GetEntityTypeGetName(unsigned int entities, int type) = 0;
	virtual int GetEntityIdTitleColor(unsigned int ent_type, unsigned int &a_id, std::string &title, unsigned int &color) = 0;
    virtual bool GetColor(unsigned int ent_type, unsigned int& color) = 0;
    virtual bool GetTitle(unsigned int ent_type, std::string& title) = 0;
    virtual unsigned int GetNextMaxAvailableId() = 0;
    virtual unsigned int GetMaxIdPoolNo() const = 0;
    virtual unsigned int GetLoadEngineeringType() const = 0;
    virtual int GetFormatType() const = 0;
    virtual void SetCurrentFeOutputFileManager(unsigned int ent_type, void *entity_p, FILE *file) const = 0;
    virtual void SetCurrentEntity(unsigned int ent_type, void *entity_p) = 0;
    virtual void *GetCurrentEntityPtr() const = 0;
    virtual unsigned int GetCurrentEntityType() const = 0;
	virtual bool SetEntitybyId(unsigned int ent_type, MYOBJ_INT id, bool apply_offset = true) = 0; // Applies offset(if (apply_offset == true)) of the submodel in which it is referred(not created)
    virtual unsigned int GetNlookupValue(unsigned int table_num1, unsigned int table_num2, unsigned int obj_id) const = 0;
	virtual int GetUserComments(const char *UserCommentFlagSkey, int &UserCommentFlag, const char *NumCommentsSkey, int &NumComments, const char *CommentsSkey, std::vector<std::string> &Comments) const = 0;
    virtual bool IsValidEntityToExport(void* entity_p, unsigned int ent_type) = 0;
   
    virtual bool CanSolverEntitiesHaveName() const = 0;
    virtual bool IsParameterSupported(unsigned int etype, unsigned int config = 0, unsigned int solvertype = 0)=0;
    virtual int  UpdateDataEnum(const IDescriptor* a_descr_p, const unsigned int& entities) = 0;
    virtual int GetDataNameEnumCFG(const unsigned int& entities, const char* dataname, int ikey = -1, unsigned int subobjindx = UINT_MAX) const = 0;
    //@}
};

#endif /* HCIOI_SOLVERINF_H */
