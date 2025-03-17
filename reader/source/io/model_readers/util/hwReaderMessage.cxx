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
#include <cstring>
#include <map>
#include <string>
#include <sstream>
#include "hwReaderMessage.h"

std::string hwReaderMessagePool::p_emptystring;

hwReaderMessage::hwReaderMessage(unsigned int id, int type,
    const std::string &filename, unsigned int linenumber,
    const std::string &description, const std::string &title, const std::string &solution)
    : p_id(id), p_type(type),
    p_filename(filename),  p_linenumber(linenumber),
    p_block(""), p_line(""),
    p_title(title), p_description(description), p_solution(solution)
{
    CleanEnd(p_filename);
    CleanEnd(p_title);
    CleanEnd(p_description);
    CleanEnd(p_solution);
}

hwReaderMessage::hwReaderMessage(unsigned int id, int type,
    const std::string &block, const std::string &line,
    const std::string &filename, unsigned int linenumber,
    const std::string &description, const std::string &title, const std::string &solution)
    : p_id(id), p_type(type),
    p_filename(filename),  p_linenumber(linenumber),
    p_block(block), p_line(line),
    p_title(title), p_description(description), p_solution(solution)
{
    CleanEnd(p_block);
    CleanEnd(p_line);
    CleanEnd(p_filename);
    CleanEnd(p_title);
    CleanEnd(p_description);
    CleanEnd(p_solution);
}

hwReaderMessage::hwReaderMessage()
    : p_id(0), p_type(0),
    p_filename(""),  p_linenumber(0),
    p_block(""), p_line(""),
    p_title(""), p_description(""), p_solution("")
{}

void hwReaderMessage::CleanEnd(std::string& text)
{ 
    while(!text.empty() && (text.back()=='\n' || text.back()=='\r' || text.back()==' '))
    {
        text.pop_back();
    }
}

// message to be fetched from message file
const hwReaderMessage& hwReaderMessageList::Add(unsigned int id, int type,
                            const std::string &block, const std::string &line,
                            const std::string &filename, unsigned int linenumber,
                            ...)
{
    va_list args;
    va_start(args,linenumber);

    const hwReaderMessage& message = Add(id, type, args,
        block, line, filename, linenumber);

    va_end(args);

    return message;
}

// message to be fetched from message file (using va_list)
const hwReaderMessage& hwReaderMessageList::Add(unsigned int id, int type,
    va_list args,
    const std::string &block, const std::string &line,
    const std::string &filename, unsigned int linenumber)
{
    if(!p_pMessagePool) return Add(-1, "Internal error");

    if(p_offset > id) id += p_offset;

    std::string description = p_pMessagePool->GetDescription(id);
    std::string title = p_pMessagePool->GetTitle(id);
    std::string solution = p_pMessagePool->GetSolution(id);

    char buffer[5000];
    if(!description.empty()) vsprintf(buffer, description.c_str(), args);
    else                     buffer[0] = '\0';

    hwReaderMessage message(id, type, block, line, filename, linenumber,
        buffer,title,solution);
    push_back(message);
    return back();
}

// message to be fetched from message file, obsolete version
const hwReaderMessage& hwReaderMessageList::Add(unsigned int id, int type,
    const std::string &filename, unsigned int linenumber,
    ...)
{
    va_list args;
    va_start(args,linenumber);

    const hwReaderMessage& message = Add(id, type, args,
        "", "", filename, linenumber);

    va_end(args);

    return message;
}

/* message to be searched in message file */
const hwReaderMessage& hwReaderMessageList::Add(
    const char *format, va_list args,
    unsigned int defaultid, int defaulttype,
    const std::string &block, const std::string &line,
    const std::string &filename, unsigned int linenumber)
{
    if(!p_pMessagePool) return Add(-1, "Internal error");

    bool found = false;
    unsigned int id = defaultid;
    int type = defaulttype;
    char buffer[5000];
    buffer[0] = '\0';


    int errorCode = GetErrorCode(std::string(format));
    if (errorCode != 0)
    {
        found = true;
        id = errorCode;
    }
    else
    {
        // first search matching message in pool
        for (std::map<unsigned int, std::string>::const_iterator it = p_pMessagePool->p_descriptions.begin();
            it != p_pMessagePool->p_descriptions.end(); ++it)
        {
            unsigned int itId = it->first; // to be cleaned up: Add an iterator to hwReaderMessagePool

            const std::string& description = p_pMessagePool->GetDescription(itId);
            if (description.size() > 5 && // skip "%s" and alike, with some safety margin
                strstr(format, description.c_str()) != NULL)
            {
                found = true;
                id = itId;
                break;
            }
        }
    }
    if (found)
    {
        // override given type by type in the pool
        const std::string& title = p_pMessagePool->GetTitle(id);
        if (title.find("error") != title.npos ||
            title.find("Error") != title.npos ||
            title.find("ERROR") != title.npos)
        {
            type = 2;
        }
        else if (title.find("warning") != title.npos ||
            title.find("Warning") != title.npos ||
            title.find("WARNING") != title.npos)
        {
            type = 1;
        }
        else if (title.find("info") != title.npos ||
            title.find("Info") != title.npos ||
            title.find("INFO") != title.npos)
        {
            type = 0;
        }

        // skip args that are used in format but not in description, and print to buffer
        const std::string& description = p_pMessagePool->GetDescription(id);
        vsprintf(buffer, description.c_str(), args);
    }
    else
    // if no matching message in pool, use the given one,
    // taking the given description
    {
        if(p_offset > id) id += p_offset;

        if(format) vsprintf(buffer,format,args);
    }

    const std::string &title = p_pMessagePool->GetTitle(id);
    const std::string &solution = p_pMessagePool->GetSolution(id);

    hwReaderMessage message(id, type, block, line, filename, linenumber,
        buffer,title,solution);
    push_back(message);
    return back();
}

// utility for the storage used in some places
void hwReaderMessageList::Add(const std::map<int, std::vector<std::string>> &messages)
{
    for(auto it=messages.begin();it!=messages.end();it++)
    {
        for(auto it2=it->second.begin(); it2 != it->second.end(); ++it2)
        {
            hwReaderMessage message(0, it->first, "", 0, *it2);
            push_back(message);
        }
    }
}

bool hwReaderMessageList::ReadMessageFile(std::string filename)
{
    hwReaderMessagePool *messages = new hwReaderMessagePool();

    std::ifstream file(filename);
    if (!file.is_open())
    {
        std::string message("Cannot open message file ");
        message += filename;
        this->Add(109002, 2, message.c_str(),
            "", 0, "** ERROR IN ENVIRONMENT");
        delete messages;
        return false;
    }

    std::string line;
    std::getline(file, line, '/'); // extract everything before and including the first '/'
    while (std::getline(file, line))
    {
        std::stringstream ss(line);
        std::string buffer;
        std::getline(ss, buffer, '/'); // extract "MESSAGE/"
        int id;
        ss >> id;
        ss.get(); // extract '/'
        std::getline(ss, buffer); // extract rest of line
        if (buffer.find("TITLE") != buffer.npos)
        {
            ReadMessageBlock(file, buffer);
            messages->AddTitle(id, buffer);
        }
        else if (buffer.find("DESCRIPTION") != buffer.npos)
        {
            ReadMessageBlock(file, buffer);
            // if the description starts with "-- FILE :" and "-- LINE :" we ignore this
            // same for "-- BLOCK" and "-- LINE".
            size_t tokenpos = buffer.find("-- FILE");
            if (buffer.npos == tokenpos) tokenpos = buffer.find("-- BLOCK");
            if (buffer.npos != tokenpos)
            {
                size_t eolpos = buffer.find('\n');
                if (buffer.npos != eolpos && eolpos > tokenpos)
                {
                    buffer.erase(0, eolpos + 1); // + 1 to erase also '\n'
                    tokenpos = buffer.find("-- LINE");
                    if (buffer.npos != tokenpos)
                    {
                        eolpos = buffer.find('\n');
                        if (buffer.npos != eolpos && eolpos > tokenpos)
                        {
                            buffer.erase(0, eolpos + 1); // + 1 to erase also '\n'
                        }
                    }
                }
            }
            messages->AddDescription(id, buffer);
        }
        else if (buffer.find("SOLUTION") != buffer.npos)
        {
            ReadMessageBlock(file, buffer);
            messages->AddSolution(id, buffer);
        }
        else
        {
            std::getline(file, buffer, '/'); // unknown, extract everything up to the next '/' and continue
        }
    }

    if (p_owningMessagePool && p_pMessagePool) delete p_pMessagePool;
    p_pMessagePool = messages;
    p_owningMessagePool = true;
    return true;
}

void hwReaderMessageList::ReadMessageBlock(std::ifstream& file, std::string& buffer)
{
    // extract everything up to the next '/'
    std::string buffer2;
    std::getline(file, buffer2, '/');
    // copy non-comment and non-empty lines
    buffer.resize(0);
    std::stringstream ss(buffer2);
    std::string buffer3;
    while (std::getline(ss, buffer3))
    {
        if (buffer3.size() > 0 && buffer3[0] != '#')
        {
            if (buffer.size() > 0) buffer.append("\n");
            buffer.append(buffer3);
        }
    }
    
    while (buffer.size() > 0 && (buffer.back() == '\n' || buffer.back() == '\r' || buffer.back() == ' ')) buffer.pop_back();
}

int hwReaderMessageList::GetErrorCode(std::string errorString)
{
    std::size_t found = errorString.find("ERROR CODE:");
    if (found != std::string::npos)
    {
        std::size_t nextfound = errorString.find(" ", found + 12);
        std::string codestr = errorString.substr(found + 12, nextfound - (found + 11));
        int codeNo = atoi(codestr.c_str());
        return codeNo;
    }
    return 0;
}
