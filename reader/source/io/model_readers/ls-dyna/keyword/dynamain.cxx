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
#include "dynamain.h"
DynakeyMessages dynakeymessages;


// DynakeyMessages
void DynakeyMessages::ShowMessage(const sdiMessageHandler::Level &level, int code, va_list args) const
{
    int type = 0;
    switch(level)
    {
    case sdiMessageHandler::Warning: type = 1; break;
    case sdiMessageHandler::Error:   type = 2; break;
    default:                         type = 0;
    }

    const hwReaderMessage& message = const_cast<DynakeyMessages*>(this)->Add(code, type, args,
        "", "", "", 0);
}
 

#include <sdiCFGTypeMapper.h> 
#include <sdiModelViewPO.h>

static std::vector<IMECPreObject *> pre_obj_lst[HCDI_OBJ_TYPE_HC_MAX];

#include <MODEL_IO/solverCDR.h>

extern "C" DYNAKEY_DECLS
sdi::ModelViewEdit* DynakeyReadModel(const char *filename)
{
    std::string msgfilename = getenv("HM_MSG_DIR");
#if defined WIN32 || _WIN32
    msgfilename += "\\CONFIG\\";
#else
    msgfilename += "/CONFIG/";
#endif
    msgfilename += "msg_hw_radioss_reader.txt";

    // Initialize message pool
    dynakeymessages.ReadMessageFile(msgfilename);
    dynakeymessages.SetOffset(100000);

    std::string str_error, filenameLoc;
    // This makes relying on the environment variables
    string rootDir;

    vector<string>  allowable_vec{ "HM_SUPPORTED" };
    string str_version("Keyword971_R13.1");
    MultiCFGKernelMgr::getInstance().InitCFGKernel(rootDir, "", str_version, filenameLoc, false, allowable_vec, str_error);

    // if(str_error.size() > 0) throw dynakeymessages.Add(9001, 3, "", 0, str_error.c_str());

    SolverSyntaxInfos syntaxSolverInfos;
    SolverInputInfo solverInf;
    CommonDataReaderCFG reader("", str_version, "", true);
    reader.ReadModel(filename, pre_obj_lst);
    const CFGKernel* cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    sdi::ModelViewPO* pModelViewSDI =  new sdi::ModelViewPO(
        //sdi::SDICFGTypeMapper(cfgkernel, "*"),
        sdi::SDICFGTypeMapper(),
        pre_obj_lst, HCDI_OBJ_TYPE_HC_MAX, cfgkernel,
        {"*DEFINE_CURVE"});
    pModelViewSDI->ApplyIdOffsets("INCLUDE_TRANSFORM");

    return pModelViewSDI;
}

extern "C" DYNAKEY_DECLS
DynakeyMessages& DynakeyGetMessages()
{
    return dynakeymessages;
}

