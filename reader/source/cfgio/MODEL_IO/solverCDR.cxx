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




////////////////////////////////////////////////////////////////////////////////////


#include <string>
#include "solverCDR.h"
#include "MODEL_IO/hw_cfg_reader.h"
#include <HCDI/hcdi_mec_pre_object.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include <KERNEL_BASE/Structure_types.h>
#include <HCDI/hcdi_utils.h>
#include <MODEL_IO/cdr_reserveattribs.h>
#include <MODEL_IO/hcioi_utils.h>
#include <HCDI/hcdi_utils.h>
#include <assert.h>



ParameterPOImp::ParameterPOImp()
{

}
ParameterPOImp::~ParameterPOImp()
{

}

void ParameterPOImp::SetFileIndex(int fileindex)
{
   // evaluateHandler.SetIncludeId(fileindex);
}

void ParameterPOImp::ClearProcessedParameters()
{
   // evaluateHandler.ClearProcessedParameters();
}
int ParameterPOImp::GetFileIndex() const
{
    return 0;// evaluateHandler.GetIncludeId();
}
int ParameterPOImp::GetIntValue() const
{
    int valueInt = 0;
    if (pCurParameterObj)
    {
        int att_indx = pCurParameterObj->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, cur_paramvalintskey.c_str());
        valueInt = att_indx >= 0 ? pCurParameterObj->GetIntValue(att_indx) : 0;
    }
    return valueInt;
}
double ParameterPOImp::GetDoubleValue() const
{
    double dvalue = 0.0;
    if (pCurParameterObj)
    {
        int att_indx = pCurParameterObj->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_FLOAT, cur_paramvaldoubleskey.c_str());
        dvalue = att_indx >= 0 ? pCurParameterObj->GetFloatValue(att_indx) : 0;
    }
    return dvalue;
}
std::string ParameterPOImp::GetStringValue() const
{
    std::string str("");
    if (pCurParameterObj)
    {
        int att_indx = pCurParameterObj->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, cur_paramvaltextskey.c_str());
        str = att_indx >= 0 ? pCurParameterObj->GetStringValue(att_indx) : "";
    }
    return str;
}

IParameter::Type ParameterPOImp::GetType() const
{
    int atype = -1;
    if (pCurParameterObj)
    {
        int att_indx = pCurParameterObj->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_UINT, cur_paramvaltypeskey.c_str());
        if (att_indx >= 0)
            atype = att_indx >= 0 ? pCurParameterObj->GetUIntValue(att_indx) : -1;
        else
        {
            att_indx = pCurParameterObj->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, cur_paramvaltypeskey.c_str());
            atype = att_indx >= 0 ? pCurParameterObj->GetIntValue(att_indx) : -1;
        }
    }
    return (IParameter::Type)atype;
}
IParameter::Keywordtype ParameterPOImp::GetKeywordType() const
{
    return IParameter::REGULAR;
}

static int get_paramname_beginindex(vector<IMECPreObject *> &objlst, int low, int high, string param_find, int n_size, string &paramname, string &paramscope)
{
    if (high >= low)
    {
        int mid = (high + low) / 2;
        if (mid == 0)
            return 0;
        IMECPreObject	*a_mv_param_obj = objlst[mid - 1];

        int att_indx = a_mv_param_obj->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, paramname.c_str());
        string param_name_mid_minus = att_indx >= 0 ? a_mv_param_obj->GetStringValue(att_indx) : "";

        a_mv_param_obj = objlst[mid];

        int att_indx1 = a_mv_param_obj->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, paramname.c_str());
        string param_name_mid = att_indx1 >= 0 ? a_mv_param_obj->GetStringValue(att_indx1) : "";

        if ((mid == 0 || param_find > param_name_mid_minus) && param_name_mid == param_find)
            return mid;
        else if (param_find > param_name_mid)
            return get_paramname_beginindex(objlst, (mid + 1), high, param_find, n_size, paramname, paramscope);
        else if (param_find == param_name_mid_minus && mid == 1)
            return mid - 1;
        else
            return get_paramname_beginindex(objlst, low, (mid - 1), param_find, n_size, paramname, paramscope);
    }
    return -1;
}

IParameter* ModelFactoryReaderPO::GetParameterObject(const char* param_str, const int file_index, void** ent_ptr)
{
    parameter_obj->ResetVariables();
    
    vector< IMECPreObject* >& a_vec = GetPreObjectLst(HCDI_OBJ_TYPE_PARAMETERS);

    size_t a_nb_objects = a_vec.size();
    if(!a_nb_objects)
        return parameter_obj;

    IDescriptor* pdescrp = HCDI_GetDescriptorHandle(a_vec[0]->GetKernelFullType());

    if (!pdescrp)
        return parameter_obj;

    string paramname_skey = GetAttribNameFromDrawable(pdescrp, cdr::g_AttribParamName);

    string paramvalueint_skey = GetAttribNameFromDrawable(pdescrp, cdr::g_AttribParamValueInteger);
    string paramvaluedouble_skey = GetAttribNameFromDrawable(pdescrp, cdr::g_AttribParamValueDouble);
    string paramvaluetext_skey = GetAttribNameFromDrawable(pdescrp, cdr::g_AttribParamValueString);
    string scope_skey = GetAttribNameFromDrawable(pdescrp, cdr::g_AttribParamScope);
    string type_skey = GetAttribNameFromDrawable(pdescrp, cdr::g_AttribParamType);

    int first_indx = get_paramname_beginindex(a_vec, 0, (int)a_vec.size() - 1, param_str, (int)a_vec.size(), paramname_skey, scope_skey);
    if (first_indx < 0 || first_indx >= a_nb_objects)
        return parameter_obj;

    IMECPreObject* found = nullptr;
    for (int i = first_indx; i < a_nb_objects; i++) 
    {
        IMECPreObject* obj = a_vec[i];
        string cur_paramval(obj->GetStringValue(paramname_skey.c_str()));
        if (cur_paramval == param_str) {
            string cur_pscope(obj->GetStringValue(scope_skey.c_str()));
            if (cur_pscope == "LOCAL") {
                int ifileindx = obj->GetFileIndex();
                if (ifileindx == file_index) {
                    found = obj; // Found a local object with matching a
                    break;
                }
            }
            else if (found == nullptr) {
                found = obj; // Found a global object
            }
        }
        else if(first_indx)
            break;
    }
    parameter_obj->SetCurParameterObj(found);

    parameter_obj->SetCurParamValIntSkey(paramvalueint_skey);
    parameter_obj->SetCurParamValDoubleSkey(paramvaluedouble_skey);
    parameter_obj->SetCurParamValTextSkey(paramvaluetext_skey);
    parameter_obj->SetCurParamValScopeSkey(scope_skey);
    parameter_obj->SetCurParamValTypeSkey(type_skey);

    return parameter_obj;
}


void ModelFactoryReaderPO::AddSubDeckObjects()
{
    vector<MECSubdeck*>::iterator It = MECSubdeck::mySubdeckVector.begin();
    int fileid = 0, submodelid = 1;
    for (It; It != MECSubdeck::mySubdeckVector.end(); ++It)
    {
        IMECPreObject* a_preobj = (*It)->GetPreObject();
        if (a_preobj)
        {
            StorePreObject(HCDI_OBJ_TYPE_INCLUDEFILES, a_preobj);
        }
        else
            assert(0);
    }
}


bool EntityHasNoId(IMECPreObject* preObj)
{
    bool noId = false;
    int hm_type = preObj->GetEntityType();
    switch (hm_type)
    {
    case HCDI_OBJ_TYPE_INITIALGEOMETRIES:
        noId = true;
        break;
    }
    if (noId && preObj->GetId() == 0)
        return true;

    return false;
}

void ModelFactoryReaderPO::PreTreatObject(const char* otype)
{
}

void ModelFactoryReaderPO::PostTreatObject(const char* otype)
{
    // 1. remove duplicate id, renumber
    int objtype = HCDI_get_entitytype(otype);
    if (objtype <= HCDI_OBJ_TYPE_NULL && objtype >= HCDI_OBJ_TYPE_HC_MAX)
        return;
    std::vector<IMECPreObject *>& a_vec = GetPreObjectLst(objtype);

    std::vector< IMECPreObject *>::iterator itr_beg = a_vec.begin();
    std::vector< IMECPreObject *>::iterator itr_end = a_vec.end();
    std::vector< IMECPreObject *>::iterator itr;

    //2. Associate parameter usage 
    for (itr = itr_beg; itr != itr_end; ++itr)
    {
        IMECPreObject* a_pre_obj_p = (IMECPreObject*)(*itr);
        //SetParameterObjectUsageData(a_pre_obj_p);
    }
}

int ModelFactoryReaderPO::AddObject(const IMECPreObject& pre_object, const InputInfos::IdentifierValuePairList* metaarg)
{
    return 0;
}


/* Implemetation of CLoadCFGKernel */
CLoadCFGKernel::CLoadCFGKernel(const std::string& profile, const std::string& subprofile, bool unload, const std::string &cfg_dir_path)
{
    m_delete_kernel = unload;
    m_hm_prev_appl_fileformat = MultiCFGKernelMgr::getInstance().GetActiveUserProfile();
    const char* rdir = nullptr;
    
    if (cfg_dir_path.empty())
        rdir = getenv("HW_ROOTDIR");
    else
        rdir = cfg_dir_path.c_str();

    if (rdir == nullptr)
        exit(0);

    string rootDir(rdir);
    string str_error("");

    m_loaded_fileformat = MV_get_file_format(subprofile);
    if (unload == true)
    {
        if (m_loaded_fileformat != FF_UNKNOWN)
        {
            std::vector<MvFileFormat_e> a_loadedprofiles = MultiCFGKernelMgr::getInstance().GetLoadedProfiles();
            m_delete_kernel = false;
            if (std::find(a_loadedprofiles.begin(), a_loadedprofiles.end(), m_loaded_fileformat) == a_loadedprofiles.end())
                m_delete_kernel = true;
        }
    }
    const CFGKernel* cfgKernel = MultiCFGKernelMgr::getInstance().InitCFGKernel(rootDir, profile, subprofile, "", false, { "HM_SUPPORTED" }, str_error);
    if (!cfgKernel || str_error != "")
        throw str_error;
}
CLoadCFGKernel::~CLoadCFGKernel()
{
    if (m_delete_kernel)
        MultiCFGKernelMgr::getInstance().DeleteCFGKernel(m_loaded_fileformat);

    MultiCFGKernelMgr::getInstance().SetActiveUserProfile(m_hm_prev_appl_fileformat);
    MvFileFormat_e a_appl_fileformat = MultiCFGKernelMgr::getInstance().GetActiveUserProfile();
    if (m_hm_prev_appl_fileformat != FF_UNKNOWN)
        assert(a_appl_fileformat == m_hm_prev_appl_fileformat);
}

/* Implemetation of CommonDataReaderCFG  */
CommonDataReaderCFG::CommonDataReaderCFG(const std::string& profile, const std::string& subprofile,
                                         const string& cfg_dir_path, bool set_cur_kernel,
                                         const ReadFileFactorySP& p_fileFactory)
{
    m_set_cur_kernel = set_cur_kernel;
    m_prev_loaded_fileformat = MultiCFGKernelMgr::getInstance().GetActiveUserProfile();

    string str_error("");

    m_prev_loaded_fileformat = MV_get_file_format(subprofile);

    const CFGKernel* cfgKernel = MultiCFGKernelMgr::getInstance().InitCFGKernel(cfg_dir_path, profile, subprofile, "", false, { "HM_SUPPORTED" }, str_error);
    if (!cfgKernel || str_error != "")
        throw str_error;

    m_loaded_fileformat = MV_get_file_format(subprofile);

    m_psyntaxInfos = std::make_unique<SolverSyntaxInfos>();
    m_pinputInfo = std::make_unique<SolverInputInfo>();
}

CommonDataReaderCFG::~CommonDataReaderCFG()
{
    if (m_pmodel)
        delete m_pmodel;

    if (m_set_cur_kernel == false)
    {
        MultiCFGKernelMgr::getInstance().DeleteCFGKernel(m_loaded_fileformat);
        if (m_prev_loaded_fileformat != FF_UNKNOWN)
            MultiCFGKernelMgr::getInstance().SetActiveUserProfile(m_prev_loaded_fileformat);
    }
}

void CommonDataReaderCFG::ReadModel(const std::string& filepath, vector<IMECPreObject*>* preobjLst)
{
    //
    m_pmodel = new ModelFactoryReaderPO(preobjLst);
    MvFileFormat_e a_fileformat = MultiCFGKernelMgr::getInstance().GetActiveUserProfile();

    // Due to historical reasons, the CFG files for LSDyna are inconsistent. 
    // To handle this, we need to override the reader to correctly parse the deck.
    // In the future, as the CFG files are incrementally corrected, this check can be removed.
    if (m_psyntaxInfos->getAppMode() == HCDI_SOLVER_LSDYNA)
    {
        HWCFGReaderLSDyna  a_reader(filepath.c_str(), a_fileformat, *m_psyntaxInfos, *m_pinputInfo);
        a_reader.readModel(m_pmodel);
    }
    else
    {
        HWCFGReader  a_reader(filepath.c_str(), a_fileformat, *m_psyntaxInfos, *m_pinputInfo);
        a_reader.readModel(m_pmodel);
    }

    CFGResolveEntitiesSubObjectReferences(m_pinputInfo.get()->getlUserNamesSolverInfo(), preobjLst);
}


std::vector<IMECPreObject *>& CommonDataReaderCFG::GetPreObjectLst(unsigned int etype)
{
    return m_pmodel->GetPreObjectLst(etype);
}
unsigned int CommonDataReaderCFG::GetEntityCount(unsigned int etype)
{
    std::vector<IMECPreObject *>& objlst = m_pmodel->GetPreObjectLst(etype);
    return (unsigned int)(objlst.size());
}
IMECPreObject* CommonDataReaderCFG::GetPreObject(unsigned int etype, unsigned int indx)
{
    std::vector<IMECPreObject *>& objlst = m_pmodel->GetPreObjectLst(etype);
    if (indx >= objlst.size())
        return nullptr;
    return objlst[indx];
}
