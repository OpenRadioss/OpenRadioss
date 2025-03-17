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

#if !defined(DYNAMAIN__INCLUDED_)
#define DYNAMAIN__INCLUDED_

// *************************************************************************************
// Windows export macro.
// *************************************************************************************
#if defined(OS_WIN) && !defined(NO_DECLS)
#ifdef DYNAKEY_EXPORT
#undef DYNAKEY_DECLS
#define DYNAKEY_DECLS __declspec(dllexport)
#else
#undef DYNAKEY_DECLS
#define DYNAKEY_DECLS __declspec(dllimport)
#endif  //! DYNAKEY_EXPORT
#else
#undef DYNAKEY_DECLS
#define DYNAKEY_DECLS
#endif //! OS_WIN

#include <hwReaderMessage.h>
#include <sdiMessageHandler.h>

class DynakeyMessages: public hwReaderMessageList, public sdiMessageHandler
{
public:
    // constructor and destructor
    DynakeyMessages() : hwReaderMessageList(), sdiMessageHandler() {}
    virtual ~DynakeyMessages() {}

    // Implementation of sdiMessageHandler
    virtual void ShowMessage(const sdiMessageHandler::Level &level, int code, va_list args) const;
};

// forward declaration
namespace sdi
{
class ModelViewEdit;
}

extern "C" DYNAKEY_DECLS
sdi::ModelViewEdit* DynakeyReadModel(const char *filename);

extern "C" DYNAKEY_DECLS
DynakeyMessages& DynakeyGetMessages();

#endif //! !defined(DYNAMAIN__INCLUDED_)
