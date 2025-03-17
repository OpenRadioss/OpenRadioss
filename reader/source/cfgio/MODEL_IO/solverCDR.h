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

// SolverCDR
// ------------------------------------------------------
//

#ifndef SOLVERCDR_H
#define SOLVERCDR_H

#include <MODEL_IO/meci_parameter.h>
#include <MODEL_IO/mv_model_factory.h>
#include <HCDI/hcdi_mec_pre_object.h>
#include <MODEL_IO/meci_syntax_infos.h>
#include <MODEL_IO/meci_input_infos.h>
#include <MODEL_IO/mv_solver_input_infos.h>
// Forward declaration
class HWCFGReader;

class ParameterPOImp : public IParameter
{

public:
    ParameterPOImp();
    ~ParameterPOImp();

    void SetFileIndex(int fileindex);
    void ClearProcessedParameters();
    int GetFileIndex() const;
    int GetIntValue() const;
    double GetDoubleValue() const;
    std::string GetStringValue() const;

    IParameter::Type GetType() const;
    IParameter::Keywordtype GetKeywordType() const;

    std::string GetName() const { return ""; }
    void* GetPointer() { return nullptr; }
    void SetCurParameterObj(IMECPreObject* pobj)
    {
        pCurParameterObj = pobj;
    }
    void SetCurParamValIntSkey(string val_str)    { cur_paramvalintskey    = val_str; }
    void SetCurParamValDoubleSkey(string val_str) { cur_paramvaldoubleskey = val_str; }
    void SetCurParamValTextSkey(string val_str)   { cur_paramvaltextskey   = val_str; }
    void SetCurParamValScopeSkey(string val_str)  { cur_paramscopeskey     = val_str; }
    void SetCurParamValTypeSkey(string val_str)   { cur_paramvaltypeskey = val_str; }


    void ResetVariables()
    {
        pCurParameterObj       = nullptr;
        cur_paramvalintskey    = "";
        cur_paramvaldoubleskey = "";
        cur_paramvaltextskey = "";
        cur_paramvaltypeskey   = "";
        cur_paramscopeskey     = "";
    }
private:
    
  //  sdi::HandleParameterRead curParameterHandle;
    IMECPreObject* pCurParameterObj;

    string         cur_paramvalintskey;
    string         cur_paramvaldoubleskey;
    string         cur_paramvaltextskey;
    string         cur_paramscopeskey;
    string         cur_paramvaltypeskey;

};
class ModelFactoryReaderPO : public MvModelFactory_t
{
public:
    //typedef std::vector< std::pair<std::string, cfgkernel::Variant> > IdentifierValuePairList;
    ModelFactoryReaderPO(vector<IMECPreObject*>* PreobjLst)
    {
        SetPreObjectLst(PreobjLst);
        parameter_obj = new ParameterPOImp();
    }
    ~ModelFactoryReaderPO()
    {
        delete parameter_obj;
    }
public:
    int AddObject(const IMECPreObject& pre_object, const InputInfos::IdentifierValuePairList* metaarg = NULL);

    void PreTreatObject(const char* otype);
    void PostTreatObject(const char* otype);

    void AddSubDeckObjects();

    IParameter* GetParameterObject(const char* param_str, const int file_index, void** ent_ptr = nullptr);
    bool SetParameterObjectUsageData(IMECPreObject* pre_object);
    bool SetIdParameterData(IMECPreObject* pre_object, IMECPreObject* main_pre_object = NULL);
    void SetParameterAttributeData(IMECPreObject* pre_object, IDescriptor* descr_p, IMECPreObject* main_pre_object = NULL);

private:

    ParameterPOImp* parameter_obj = nullptr;
};

/*class to read data using common data reader interface*/
class HCIO_DATA_DLL_API CommonDataReaderCFG
{
public:
    CommonDataReaderCFG(const std::string& profile, const std::string& subprofile,
                        const string& cfg_dir_path = "", bool set_cur_kernel = false,
                        const ReadFileFactorySP& p_fileFactory=nullptr);

   ~CommonDataReaderCFG();

public:

    std::vector<IMECPreObject *>& GetPreObjectLst(unsigned int etype);

    unsigned int GetEntityCount(unsigned int etype);

    IMECPreObject* GetPreObject(unsigned int etype, unsigned int indx);

    void ReadModel(const std::string& filepath, vector<IMECPreObject*>* preobjLst);

    ISyntaxInfos* GetSyntaxInfo() { return m_psyntaxInfos.get();  }

    InputInfos* GetInputInfo()  { return m_pinputInfo.get(); }

private:
    HWCFGReader*                    m_preader = nullptr;
    ModelFactoryReaderPO*           m_pmodel = nullptr;
    std::unique_ptr<ISyntaxInfos>   m_psyntaxInfos;
    std::unique_ptr<InputInfos>     m_pinputInfo;
    MvFileFormat_e                  m_prev_loaded_fileformat;
    MvFileFormat_e                  m_loaded_fileformat;
    bool                            m_set_cur_kernel = false;
};

/*class to load kernel and unload it by setting prevoius kernel active*/
class HCIO_DATA_DLL_API CLoadCFGKernel
{
public:
    CLoadCFGKernel(const std::string& profile, const std::string& subprofile, bool unload = true, const std::string &cfg_dir_path="");
    ~CLoadCFGKernel();
private:
    MvFileFormat_e                  m_hm_prev_appl_fileformat;
    MvFileFormat_e                  m_loaded_fileformat;
    bool                            m_delete_kernel = true;
};


#endif // SOLVERCDR_H
