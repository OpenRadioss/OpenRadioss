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




////////////////////////////////////////////////////////////////////

#ifndef CONVERT_H
#define CONVERT_H

// *************************************************************************************
// Windows export macro.
// *************************************************************************************
#if defined(OS_WIN) && !defined(NO_DECLS)
#ifdef SDICONVERT_EXPORT
#undef SDICONVERT_DECLS
#define SDICONVERT_DECLS __declspec(dllexport)
#else
#undef SDICONVERT_DECLS
#define SDICONVERT_DECLS __declspec(dllimport)
#endif  //! SDICONVERT_EXPORT
#else
#undef SDICONVERT_DECLS
#define SDICONVERT_DECLS
#endif

#include <typedef.h>

#include <sdiModelView.h>

namespace sdiConvert
{

    class SDICONVERT_DECLS Convert
    {
    protected:
        sdiString srcSolver;
        sdiString destSolver;
        sdi::ModelViewRead* srcModelViewSDI;
        sdi::ModelViewEdit* destModelViewSDI;
        static LogQueryHandle conversionLog;
    public:
        Convert(const sdiString& readSolver, const sdiString& outSolver, sdi::ModelViewRead*& srcMVSDIPtr, 
            sdi::ModelViewEdit*& destMVSDIPtr) :
            srcSolver(readSolver),
            destSolver(outSolver),
            srcModelViewSDI(srcMVSDIPtr),
            destModelViewSDI(destMVSDIPtr)
        {
        }
        virtual void CallConvert() = 0;

        virtual void GetConversionLog(LogQueryHandle& conversionLog) const = 0;

        static void PushToConversionLog(std::pair<sdi::HandleRead, SDIHandlReadList> pairToPush);

        static void RemoveFromConversionLog(sdi::ModelViewEdit* sdiMVEditPtr, const sdi::HandleEdit& destEntEdit);

        static void GetSourceHandles(sdi::ModelViewEdit* sdiMVEditPtr, const sdi::HandleRead& destEntEdit, SDIHandlReadList& dynaEntEdit);

        static void GetConvertedHandles(const sdi::HandleRead& srcHandle, SDIHandlReadList& destHandles,
                                        sdi::EntityType destType = sdi::ENTITY_TYPE_NONE);

        virtual void PrintLog(LogQueryHandle& conversionLog) const = 0;

        virtual ~Convert() { }

        // TBD: clean-up: make non-static and non-public
        static bool p_doConvertParameters;

    private:
        // hide copy constructor and assignment operator
        Convert(const Convert& other);
        Convert& operator=(const Convert& other);
    };

    // Interface used by to retrieve "client" information about an entity
    class SDICONVERT_DECLS ClientInfo
    {
    public:
        virtual bool GetEntityInfo(const sdi::EntityType type, const unsigned int id, bool& info) { return false; }

        virtual ~ClientInfo() {}
    };
} // namespace sdiConvert
#endif //CONVERT_H
