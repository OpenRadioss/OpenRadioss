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

#ifndef MECI_READ_CONTEXT_H
#define MECI_READ_CONTEXT_H

#include "mec_msg_manager.h"
#include "hcio.h"

#include <UTILS/file_utils.h>

/// Base class for reading input in any context
class HCIO_DATA_DLL_API MECIReadContext : public MECMsgManager
{
public: /** @name Constructors & destructor */
    //@{
	/*Default constructor*/
	MECIReadContext():myLineBuffer(NULL), myBufferNbChars(0), myLineNbChars(0) {}
	/** Constructor */
    MECIReadContext(int line_nb_chars, int buffer_nb_chars = -1);
    /** Destructor */
    virtual ~MECIReadContext();
    //@}
    
public: /** @name Parsing */
    //@{
    /** Reads the next line including comments */
    virtual char *readBuffer(bool do_check_eof=true,int nb_chars=-1, bool skip_comment = true) = 0;
    /** Goes back to the previous line */
    virtual void unreadBuffer() = 0;    
    /// Save a position by pushing it on a stack of positions, in order to get back to it with popPosition().
    virtual void pushPosition() = 0;
    /// Get back to a saved position by popping it from the stack of positions filled with pushPosition().
    virtual void popPosition() = 0;
    
    /// Kills new line (end of the buffer)
    void killNLEnd(char *buffer) const;
    
    void killBlanksNLEnd(char *buffer) const;
    
    const char *killBlanksBegin(const char *buffer) const;
    
    void killBlanksEnd(char *buffer) const;
    
    void completeWithBlanks(char *buffer,int nb_chars=-1) const;
    /// Changes the buffer to upper case.
    void upcase(char *buffer) const;        
    //@}
    void SetLineNbChars(int line_nb_chars);

public: /** @name Messages */
    //@{
    /** Displays current location (as message).
     * The implementation in this base class doesn't do anything.
     * It just avoids that derived classes which don't need it have to implement it. */
    virtual void displayCurrentLocation(MyMsgType_e msg_type) const;
    virtual void displayMessage(MyMsgType_e msg_type,const char *format,...) const;
    /// Get the full name of the file currently read
    virtual const char* getCurrentFullName() const;
    /// Get the number of the line currently read
    virtual _HC_LONG getCurrentLine() const;
    //@}
    /// Notify the readcontext that a keyword has been read as a subobject.
    // It is not really a clean design to have such a method here, it doesn't fit into the
    // concept of a readcontext. To be cleaned up later if desired...
    virtual void updateSubKeywordRead(const char* keyword) const {  }

protected: // Data
    int                      myBufferNbChars;
    int                      myLineNbChars;
    char                    *myLineBuffer;
};

#endif /* MECI_READ_CONTEXT_H */
