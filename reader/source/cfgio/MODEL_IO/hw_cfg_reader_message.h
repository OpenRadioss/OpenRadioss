//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.




////////////////////////////////////////////////////////////////////////////////////

#if !defined(HWCFGReaderMessage__INCLUDED_)
#define HWCFGReaderMessage__INCLUDED_

#include "hcio.h"

#include <map>
#include <string>
#include <vector>

#ifdef OS_WIN
// disable warnings on needs to have dll-interface to be used by clients of class
#pragma warning(disable : 4251 4275)
#endif

class HWCFGReaderMessagePool;

class HCIO_DATA_DLL_API HWCFGReaderMessage
{
public:
    HWCFGReaderMessage(unsigned int id, int type,
                    const std::string &filename, unsigned int linenumber,
                    const std::string &description, const std::string &title = "", const std::string &solution = "");
    HWCFGReaderMessage(unsigned int id, int type,
        const std::string &block, const std::string &line,
        const std::string &filename, unsigned int linenumber,
        const std::string &description, const std::string &title = "", const std::string &solution = "");
    HWCFGReaderMessage();

    inline unsigned int GetId() const { return p_id; }
    inline int GetType() const { return p_type; } // info, warning, error
    inline const std::string& GetFilename() const { return p_filename; }
    inline unsigned int GetLinenumber() const { return p_linenumber; }
    inline const std::string& GetBlock() const { return p_block; }
    inline const std::string& GetLine() const { return p_line; }
    inline const std::string& GetTitle() const { return p_title; }
    inline const std::string& GetDescription() const { return p_description; }
    inline const std::string& GetSolution() const { return p_solution; }

private:
    static void CleanEnd(std::string& text);
    unsigned int p_id;
    int p_type; // info, warning, error
    std::string p_filename;
    unsigned int p_linenumber;
    std::string p_block;
    std::string p_line;
    std::string p_title;
    std::string p_description;
    std::string p_solution;
};

class HCIO_DATA_DLL_API HWCFGReaderMessageList : public std::vector<HWCFGReaderMessage>
{
public:
    HWCFGReaderMessageList(const HWCFGReaderMessagePool *pMessagePool = NULL, bool owningMessagePool = false)
        : p_pMessagePool(pMessagePool), p_owningMessagePool(owningMessagePool)
    {}

    // copy constructor only copies the messages, not the message pool
    HWCFGReaderMessageList(const HWCFGReaderMessageList& other) :
        std::vector<HWCFGReaderMessage>(other), p_pMessagePool(nullptr), p_owningMessagePool(false)
    {}

    // assignement only copies the messages, not the message pool
    HWCFGReaderMessageList & operator =(const HWCFGReaderMessageList &other);

    virtual ~HWCFGReaderMessageList();

    void SetMessagePool(const HWCFGReaderMessagePool *pMessagePool, bool owningMessagePool = false);

    // "ready-to-use" message
    const HWCFGReaderMessage& Add(unsigned int id, int type,
                               const std::string &description,
                               const std::string &filename, unsigned int linenumber,
                               const std::string &title = "", const std::string &solution = "")
    {
        HWCFGReaderMessage message(id, type, filename, linenumber,
                                description, title, solution);
        push_back(message);
        return back();
    }

    // message to be fetched from message file
    const HWCFGReaderMessage& Add(unsigned int id, int type,
        const std::string &block, const std::string &line,
        const std::string &filename, unsigned int linenumber,
        ...);

    // message to be fetched from message file (using va_list)
    const HWCFGReaderMessage& Add(unsigned int id, int type,
        va_list args,
        const std::string &block, const std::string &line,
        const std::string &filename, unsigned int linenumber);

    // message to be searched for in message file
    const HWCFGReaderMessage& Add(
        const char *format, va_list args, int defaulttype,
        const std::string &block, const std::string &line,
        const std::string &filename, unsigned int linenumber);

    // message to be fetched from message file, obsolete version
    const HWCFGReaderMessage& Add(unsigned int id, int type,
        const std::string &filename, unsigned int linenumber,
        ...);

    // for internal messages only
    inline const HWCFGReaderMessage& Add(int type, const std::string &description)
    {
        HWCFGReaderMessage message(0, type, "", 0, description);
        push_back(message);
        return back();
    }

    // utility for the storage used in some places
    void Add(const std::map<int, std::vector<std::string>>& messages);

    void SetOffset(unsigned int offset) { p_offset = offset; }

    bool IsInitialized() { return nullptr != p_pMessagePool; }

protected:

    virtual int GetErrorCode(std::string errorString) { return 0; }
    virtual int GetDefaultId(int type) { return 0; }

    const HWCFGReaderMessagePool *p_pMessagePool = NULL;
    bool p_owningMessagePool = false;
    unsigned int p_offset = 0;
};

#endif //! !defined(HWCFGReaderMessage__INCLUDED_)
