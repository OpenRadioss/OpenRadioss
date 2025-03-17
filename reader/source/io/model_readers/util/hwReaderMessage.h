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

#if !defined(hwReaderMessage__INCLUDED_)
#define hwReaderMessage__INCLUDED_

#include <map>
#include <string>
#include <vector>
#include <cstdarg>
#include <fstream>

class hwReaderMessagePool
{
public:
    inline void Add(unsigned int id, const std::string &description,
             const std::string &title = "", const std::string &solution = "") {
        p_descriptions[id] = description;
        p_titles[id] = title;
        p_solutions[id] = solution;
    }

    inline void AddDescription(unsigned int id, const std::string &description) {
        p_descriptions[id] = description; }
    inline void AddTitle(unsigned int id, const std::string &title) {
        p_titles[id] = title; }
    inline void AddSolution(unsigned int id, const std::string &solution) {
        p_solutions[id] = solution; }

    inline const std::string& GetDescription(unsigned int id) const {
        if(p_descriptions.count(id)) return p_descriptions.at(id); else return p_emptystring; }
    inline const std::string& GetTitle(unsigned int id) const {
        if(p_titles.count(id)) return p_titles.at(id); else return p_emptystring; }
    inline const std::string& GetSolution(unsigned int id) const {
        if(p_solutions.count(id)) return p_solutions.at(id); else return p_emptystring; }

    friend class hwReaderMessageList;

private:
    std::map<unsigned int, std::string> p_descriptions;
    std::map<unsigned int, std::string> p_titles;
    std::map<unsigned int, std::string> p_solutions;

    static std::string p_emptystring;
};

class hwReaderMessage
{
public:
    hwReaderMessage(unsigned int id, int type,
                    const std::string &filename, unsigned int linenumber,
                    const std::string &description, const std::string &title = "", const std::string &solution = "");
    hwReaderMessage(unsigned int id, int type,
        const std::string &block, const std::string &line,
        const std::string &filename, unsigned int linenumber,
        const std::string &description, const std::string &title = "", const std::string &solution = "");
    hwReaderMessage();

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

class hwReaderMessageList : public std::vector<hwReaderMessage>
{
public:
    hwReaderMessageList(const hwReaderMessagePool *pMessagePool = NULL, bool owningMessagePool = false)
        : p_pMessagePool(pMessagePool), p_owningMessagePool(owningMessagePool)
    {}

    hwReaderMessageList(const hwReaderMessageList& other) :
        std::vector<hwReaderMessage>(other)
    {
        // this is not safe, to be revisited...
        p_pMessagePool = other.p_pMessagePool;
        p_owningMessagePool = false;
    }

    hwReaderMessageList & operator =(const hwReaderMessageList &other)
    {
        std::vector<hwReaderMessage>::operator=(other);
        // this is not safe, to be revisited...
        p_pMessagePool = other.p_pMessagePool;
        p_owningMessagePool = false;
        return *this;
    }

    ~hwReaderMessageList()
    {
        if(p_owningMessagePool && p_pMessagePool) delete p_pMessagePool;
    }

    void SetMessagePool(const hwReaderMessagePool *pMessagePool, bool owningMessagePool = false)
    {
        if(p_owningMessagePool && p_pMessagePool) delete p_pMessagePool;
        p_pMessagePool = pMessagePool;
        p_owningMessagePool = owningMessagePool;
    }

    // "ready-to-use" message
    const hwReaderMessage& Add(unsigned int id, int type,
                               const std::string &description,
                               const std::string &filename, unsigned int linenumber,
                               const std::string &title = "", const std::string &solution = "")
    {
        hwReaderMessage message(id, type, filename, linenumber,
                                description, title, solution);
        push_back(message);
        return back();
    }

    // message to be fetched from message file
    const hwReaderMessage& Add(unsigned int id, int type,
        const std::string &block, const std::string &line,
        const std::string &filename, unsigned int linenumber,
        ...);

    // message to be fetched from message file (using va_list)
    const hwReaderMessage& Add(unsigned int id, int type,
        va_list args,
        const std::string &block, const std::string &line,
        const std::string &filename, unsigned int linenumber);

    const hwReaderMessage& Add(
        const char *format, va_list args,
        unsigned int defaultid, int defaulttype,
        const std::string &block, const std::string &line,
        const std::string &filename, unsigned int linenumber);

    // message to be fetched from message file, obsolete version
    const hwReaderMessage& Add(unsigned int id, int type,
        const std::string &filename, unsigned int linenumber,
        ...);

    // for internal messages only
    inline const hwReaderMessage& Add(int type, const std::string &description)
    {
        hwReaderMessage message(0, type, "", 0, description);
        push_back(message);
        return back();
    }

    // utility for the storage used in some places
    void Add(const std::map<int, std::vector<std::string>>& messages);

    bool ReadMessageFile(std::string filename);

    void SetOffset(unsigned int offset) { p_offset = offset; }

    bool IsInitialized() { return nullptr != p_pMessagePool; }

private:

    void ReadMessageBlock(std::ifstream& file, std::string& buffer);
    int GetErrorCode(std::string errorString);

    const hwReaderMessagePool *p_pMessagePool = NULL;
    bool p_owningMessagePool = false;
    unsigned int p_offset = 0;
};

#endif //! !defined(hwReaderMessage__INCLUDED_)
