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

#include "GlobalModelSDI.h"

#include <typedef.h>
#include <sdiModelView.h>

#include <HCDI/hcdi_mv_descriptor.h>
#include <HCDI/hcdi_mec_pre_object.h>

#include <cfgio/MODEL_IO/mec_data_writer.h>
#include <cfgio/MODEL_IO/mec_single_file_writer.h>
#include <cfgio/MODEL_IO/mv_model_scanner.h>

#include <cfgkernel/sdi/sdiModelViewPO.h>


using namespace sdi;

void PrintPreObject(const IMECPreObject *pPreObject, MECDataWriter &datawriter,
    const MvModelScanner_t &a_model_scanner, MECIWriteContext &writecontext)
{
    
    ModelViewPO *pModelViewPO = static_cast<ModelViewPO*>(Get_ModelViewSDI());
    const char *kernel_ftype_str = pPreObject->GetKernelFullType();
    if(NULL == kernel_ftype_str) return;
    const IDescriptor *descr_p = pModelViewPO->GetDescriptor(pPreObject);
    if(!descr_p)
    {
        return;
    }

    const fileformat_t *a_format_p =     a_format_p=descr_p->getRadiossFileFormatPtr(FF_D00_20200);

    datawriter.WriteObjectData(a_format_p, *pPreObject, descr_p, &a_model_scanner);
}


void PrintEntity(
    FILE *file, ModelViewRead *pModelView, const EntityRead &entity, int *is_dyna)
{

    EntityType type = entity.GetType();

    MECSingleFileWriter filewriter(file, 100);
    MECDataWriter datawriter(&filewriter, 100);
    datawriter.setFormatId(FF_D00_20200);

    datawriter.setCompressDouble(1);
    datawriter.setRoundDouble(0);
    MvModelScanner_t a_model_scanner((hwHCSolverInf *)NULL, "");

    IMECPreObject *pPreObject = (IMECPreObject*)entity.GetHandle().GetPointer();
    if(!pPreObject) return;

    PrintPreObject(pPreObject,datawriter,a_model_scanner,filewriter);
}

void PrintPreObjectReport(
    FILE *file, ModelViewRead *pModelView, const EntityRead &entity)
{
    EntityType type = entity.GetType();
    
    SpecializationType Stype = pModelView->GetSpecializationType(type);

    if (Stype != SPECIALIZATION_TYPE_GENERAL) 
        return;

    IMECPreObject *pPreObject = (IMECPreObject*)entity.GetHandle().GetPointer();
    if(!pPreObject) return;
    const char *kft = pPreObject->GetKernelFullType();
    const IDescriptor *descr_p = HCDI_GetDescriptorHandle(kft);
    fprintf(file,"%s \n",pPreObject->GetReport(descr_p));
}
