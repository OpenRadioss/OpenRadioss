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

#if !defined(RADIOSSBLK__INCLUDED_)
#define RADIOSSBLK__INCLUDED_

// *************************************************************************************
// Windows export macro.
// *************************************************************************************
#if defined(OS_WIN) && !defined(NO_DECLS)
#ifdef RADIOSSBLK_EXPORT
#undef RADIOSSBLK_DECLS
#define RADIOSSBLK_DECLS __declspec(dllexport)
#else
#undef RADIOSSBLK_DECLS
#define RADIOSSBLK_DECLS __declspec(dllimport)
#endif  //! SDI_EXPORT
#else
#undef RADIOSSBLK_DECLS
#define RADIOSSBLK_DECLS
#endif //! OS_WIN

#include <hwReaderMessage.h>

#include <limits.h>

extern "C" RADIOSSBLK_DECLS
void RadiossblkSetUserProfileVersion(unsigned int version);

// forward declaration
namespace sdi
{
class EntityRead;
class ModelViewRead;
class ModelViewEdit;
typedef unsigned int EntityType;
}

extern "C" RADIOSSBLK_DECLS
sdi::ModelViewEdit* RadiossblkReadModel(const char *filename);

extern "C" RADIOSSBLK_DECLS
bool RadiossblkReadInclude(sdi::ModelViewEdit *pModelView, const char *filename, const char *directoryname = nullptr);

// deprecated
extern "C" RADIOSSBLK_DECLS
sdi::ModelViewEdit* RadiossblkReadModelSDI(const char *filename);

extern "C" RADIOSSBLK_DECLS
sdi::ModelViewEdit* RadiossblkNewModel();

extern "C" RADIOSSBLK_DECLS
void RadiossblkApplyOffsets(sdi::ModelViewEdit *pModelView, bool doUnOffset = false);

// convenience, same as RadiossblkApplyOffsets(pModelView, true)
extern "C" RADIOSSBLK_DECLS
void RadiossblkUnApplyOffsets(sdi::ModelViewEdit *pModelView);

RADIOSSBLK_DECLS
bool RadiossblkGetDimensions(
    const sdi::EntityRead &entity, const char *dataname, double *lengthDim, double *massDim, double *timeDim);

RADIOSSBLK_DECLS
bool RadiossblkGetValueDouble(
    const sdi::EntityRead &entity, const char *dataname, double *pValue,
    double *lengthDim=NULL, double *massDim=NULL, double *timeDim=NULL, unsigned int index=UINT_MAX, unsigned int col=UINT_MAX);

RADIOSSBLK_DECLS
bool RadiossblkGetValueDouble(
    const sdi::EntityRead& entity, const char *dataname, double *pValue, 
    int *lengthDim=NULL, int *massDim=NULL, int *timeDim=NULL, unsigned int index=UINT_MAX, unsigned int col = UINT_MAX);

RADIOSSBLK_DECLS
bool RadiossblkIsGroupUsed(
    sdi::ModelViewRead *pModelView,
    const sdi::EntityType type,
    const unsigned int id);

struct RadiossblkEntityReference
{
    unsigned int id;
    const char* keyword;
    std::string attributeName;
    std::string attributeSolverLabel;
    unsigned int row;

    RadiossblkEntityReference(unsigned int _id, const char* _keyword,
        std::string _attributeName, std::string _attributeSolverLabel,
        unsigned int _row = UINT_MAX) :
        id(_id), keyword(_keyword), attributeName(_attributeName), 
        attributeSolverLabel(_attributeSolverLabel), row(_row)
    {}

    RadiossblkEntityReference(const RadiossblkEntityReference& other) :
        id(other.id), keyword(other.keyword), attributeName(other.attributeName), 
        attributeSolverLabel(other.attributeSolverLabel), row(other.row)
    {}
};

RADIOSSBLK_DECLS
const std::vector<RadiossblkEntityReference>& RadiossblkGetEntityReferences(
    sdi::ModelViewRead *pModelView, const sdi::EntityType type, const unsigned int id);

extern "C" RADIOSSBLK_DECLS
const hwReaderMessageList& RadiossblkGetMessages();

#endif //! !defined(RADIOSSBLK__INCLUDED_)
