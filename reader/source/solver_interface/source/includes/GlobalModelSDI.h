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

#if !defined(GlobalModelSDI__INCLUDED_)
#define GlobalModelSDI__INCLUDED_
#include <sdiDefs.h>

#include <limits.h>

namespace sdi
{
class ModelViewEdit;
}

sdi::ModelViewEdit * Get_ModelViewSDI();

extern sdi::ModelViewEdit *g_pModelViewSDI;

void GlobalModelSDISetModel(void *pModelView);

void GlobalModelSDISelectionStart(const sdiString& keyword);
void GlobalModelSDISelectionCount(unsigned int *pCount);
void GlobalModelSDISelectionNext(bool *pIsOk);

void GlobalModelSDISelectionType(int *valueType);

void GlobalModelSDIFindById(const sdiString& keyword, int id, bool *pIsOk);

void GlobalEntitySDIGetValueDoubleNoDim(const char *dataname, double *pValue, bool *pIsOk);
void GlobalEntitySDIGetValueDouble(const char *dataname, double *pValue, bool *pIsOk, 
                                int *lengthDim=NULL, int *massDim=NULL, int *timeDim=NULL, unsigned int index=UINT_MAX, unsigned int index2=UINT_MAX);
void GlobalEntitySDIGetValueDoubleDimDouble(const char *dataname, double *pValue, bool *pIsOk, 
                                double *lengthDim=NULL, double *massDim=NULL, double *timeDim=NULL, unsigned int index=UINT_MAX, unsigned int index2=UINT_MAX);
void GlobalEntitySDIGetValueInt(const char *dataname, int *pValue, bool *pIsOk, unsigned int index_1=UINT_MAX,
                             sdiString *p_objtype_str=NULL, unsigned int index_2=UINT_MAX);
void GlobalEntitySDIGetId(int *pValue, bool *pIsOk);
void GlobalEntitySDIGetUnitId(int *pValue, bool *pIsOk);
void GlobalEntitySDIGetSubmodelId(int *pValue, bool *pIsOk);
void GlobalEntitySDIGetValueInt1(const char *dataname, int *pValue, bool *pIsOk, int *valueType);
void GlobalEntitySDIGetValueString(const char *dataname, char *buffer, int *size, bool *pIsOk, unsigned int index=UINT_MAX);
void GlobalEntitySDIGetValueBool(const char *dataname, bool *pValue, bool *pIsOk, unsigned int index=UINT_MAX);
void GlobalEntitySDIIsCrypted(bool *pIsCrypted);
void GlobalEntitySDIWrite(int *is_dyna);
void GlobalModelOpenFile(char *fileName,int *s_fileName);
void GlobalModelCloseFile();
void GlobalDebugEntitySDIWrite();
void GlobalDebugModelOpenFile(char *fileName,int *s_fileName);
void GlobalDebugModelCloseFile();
void GlobalEntitySDICreateEntity();

void GlobalModelSDISelectOptionByName(const sdiString& keyword,const sdiString& optionName,int *subIndex);
void GlobalEntitySDISetValueInt(const sdiString& dataname, int *pValue, bool *pIsOk);
void GlobalEntitySDISetValueDouble(const sdiString& dataname, double *pValue, bool *pIsOk);


void GlobalEntitySDIConvertInterType19(int *INTER_MAXID,int *GRNOD_MAXID,int *LINE_MAXID,int *OFFSET,int *isFirst);


void GlobalModelSDISelectionStartList(const sdiString& keyword);
void GlobalModelSDISelectionClearList();
void GlobalModelPositionSelection(int pos);
void GlobalEntitySDIGetArrayDouble(const char *dataname, double *pValue, bool *pIsOk, 
                                double *lengthDim, double *massDim, double *timeDim, int *index);
void GlobalEntitySDIConvertFailTab(int *TABLE_MAXID,int *FAIL_MAXID,int *OFFSET);
void GlobalEntitySDIConvert2dElementSeatbelt(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET);
void GlobalEntitySDIConvert2dElementsSeatbelt(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET,int *SEATBELT_CONVERTED_ELEMENTS,int *ELEM_INDEX);
void GlobalEntitySDIConvertTh2dElementSeatbelt(int *TH_MAXID,int *OFFSET,int *SEATBELT_CONVERTED_ELEMENTS,int *NB_SEATBELT_SHELLS);
void GlobalEntitySDICountElementsInPart(int *NB_ELEMS);

void GlobalModelSDIEntityReferencesNumber(char *type, int *id, int *refNumber);

void GlobalModelSDIApplyOffsets();
void GlobalModelSDIUnapplyOffsets();

void GlobalModelSDICountIncludeFiles(int *nbIncludes);
void GlobalModelSDIGetIncludesList(char **includeFiles);
void GlobalModelSDIIsGroupUsed(char *type,int *id, bool *isUsed);

void GlobalEntitySDIdeleteEntity();


#endif /* !defined(GlobalModelSDI__INCLUDED_) */

