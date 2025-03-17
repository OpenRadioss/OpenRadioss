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

#ifndef SDI_MESSAGE_HANDLER_H_INCLUDED
#define SDI_MESSAGE_HANDLER_H_INCLUDED 1

//! sdiMessageHandler provides an interface for message handling, to be used by SDI based modules.

#include <stdarg.h>

class sdiMessageHandler
{
public:

    //! Constructor
    sdiMessageHandler() {}

    //! Destructor.
    virtual ~sdiMessageHandler() {}

    //! Specifies the level of the message.
    enum Level{
        Normal      = 1,                             //0000 0001
        Warning     = 1 << 1,                        //0000 0010
        Error       = 1 << 2                         //0000 0100
    };

    //! Show Message - with the given variable arguments
    //! code is a message id, using the naming of hwStatus here
    void ShowMessage(const sdiMessageHandler::Level &level, int code, ...) const
    {
        va_list args;
        va_start(args, code);
        ShowMessage(level, code, args);
        va_end(args);
    }

    //! Show Message - with the given arglist
    //! code is a message id, using the naming of hwStatus here
    virtual void ShowMessage(const sdiMessageHandler::Level &level, int code, va_list args) const = 0;
};


#endif /*SDI_MESSAGE_HANDLER_H_INCLUDED*/
