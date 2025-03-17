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
#ifndef MECI_SYNTAX_INFOS_H
#define MECI_SYNTAX_INFOS_H
#include <string>
#include "hcio.h"
#include "io_types.h"
#include <UTILS/mv_cstring.h>
#include <KERNEL_BASE/GlobalApplication_enum.h>
#include <vector>
#include "mec_read_file.h"

#pragma warning(disable: 4251)
/// Interface providing syntax information
class HCIO_DATA_DLL_API ISyntaxInfos
{
public: /** @name Constructors & destructor */
    //@{
    /** Default constructor */
    ISyntaxInfos(){} 	
    /** Destructor */
    virtual ~ISyntaxInfos(){}
    //@}
    
public: /** @name Parsing */
    //@{
    /// Returns true, if the line is commented
    virtual bool isComment(const char *buffer)const=0;
    /// Returns true if the line is a header
    virtual bool isHeader(const char *buffer,char **keyword_p = NULL) const=0;    
    /// if card has free format [like a comma in the line in LS-Dyna]
    virtual bool isFreeSizeCard(const char * buffer) const {return false;};

    virtual const char* getHeader(int indx=0) const { return nullptr; }
    virtual const std::vector<const char*> *getHeaderLst() const { return nullptr; }

    //virtual const char* getComment() const { return nullptr; }
    virtual char getFreeFormatSpecifier() const { return ','; }
    virtual const int getCellLength() const { return 0; }
    virtual const int getLineLength() const { return 0; }
    virtual int getHeaderSize(int indx=-1) const { return 0; }

    virtual const char* getNormalisedHeader(const char* header) const { return header; }

    virtual const std::string& getHeaderSkipStr() const { static std::string str("");  return str; }
    virtual int getHeaderKeywordCellLength() const { return 0; }
    virtual const char* getParameterSymbol() const { static const char* psym = "&";  return psym; }
    //virtual int getCommentSize() const { return 0; }
    virtual int getInitialOffset() const { return 0; }


    virtual void SetHeaderFromStart(bool fromStart) { return; }
    virtual bool GetHeaderFromStartFlag() const { return true; }
    virtual void setApplicationMode(ApplicationMode_e mode) { }
    virtual ApplicationMode_e getAppMode() const { return HCDI_SOLVER_NONE; }
    virtual const char* GetFormatSize(const char* fmt_p, bool is_free_size, int& fmt_size) const { return nullptr; }
    virtual io_types::format_type_e getFormatType() const { return io_types::FORMAT_UNDEFINED; }
    virtual io_types::format_type_e getFormatType(MECReadFile* fp)  { return io_types::FORMAT_UNDEFINED; }
    virtual bool IsScalableFormat() const { return false; }
    virtual bool IsFormatSupportedForContinueNextLine() const { return false; }
    virtual bool HasLengthReachedForNextLine(int len) const { return false; }
    virtual bool IsSupportedForNamedEntity() const { return false; }
    virtual io_types::format_type_e updateLineFormatType(const char* line) { return io_types::FORMAT_UNDEFINED; }
    virtual bool IsSpaceORContinueChars(const char* cp) const { return false; }
    virtual char getHeaderSeparator() const { return '_'; }
    virtual void updateFormatType(int subdeck_index, io_types::format_type_e fmt_type) {}
    //@}
};


class HCIO_DATA_DLL_API SolverSyntaxInfos : public ISyntaxInfos
{
public: /** @name Constructors & destructor */
    //@{
    /*Default constructor*/
    SolverSyntaxInfos();
    /** Destructor */
    virtual ~SolverSyntaxInfos()
    {
        // if (myheader)
        //     delete myheader;
        if (mysizecomment > 0)
        {
            if (mycomment)
                delete[] mycomment;
            if (mycommentlen)
                delete[] mycommentlen;
        }
        mysizecomment = 0;
        mycomment = nullptr;
        mycommentlen = nullptr;
        if (myheaderlen)
            delete myheaderlen;
    }
    //@}

public: /** @name Parsing */
    //@{
    /// Returns true, if the line is commented
    virtual bool isComment(const char* buffer) const
    {
        if (buffer == NULL) return false;
       
        // Check for matching comment prefixes
        for (size_t i = 0; i < mysizecomment; ++i) {
            if (strncmp(buffer, mycomment[i], mycommentlen[i]) == 0) {
                return true; // Line starts with a comment prefix
            }
        }
        return false;
    }
    bool IsSupportedForNamedEntity() const
    {
        return false;
    }

    bool IsScalableFormat() const
    { 
        if ((getFormatType() != io_types::FORMAT_SHORT) && (myAppMode == HCDI_SOLVER_LSDYNA))
           return true; 
    
        return false;
    }
    bool IsFormatSupportedForContinueNextLine() const 
    { 
        return false;
    }

    virtual bool HasLengthReachedForNextLine(int len) const
    {
        return false; 
    }


    bool IsSpaceORContinueChars(const char* cp) const;


    /// Returns true if the line is a header
    virtual bool isHeader(const char* buffer, char** keyword_p = NULL) const;

    /// if card has free format [a comma in the line]
    virtual bool isFreeSizeCard(const char* buffer) const
    {
        return (strchr(buffer, myfreeformat) != NULL);
        //return (strstr(buffer, &myfreeformat) != NULL);
    }
    virtual const char* getHeader(int indx=0) const { return myheadersize ?  (*myheader)[indx] : nullptr; }
    const std::vector<const char*> *getHeaderLst() const override { return myheader;  }
    //virtual const char* getComment() const { return mycomment; }
    virtual char getFreeFormatSpecifier() const { return myfreeformat; }
    virtual const int getCellLength() const { return myCellLength; }
    virtual const int getLineLength() const { return myLineLength; }
    virtual int getHeaderKeywordCellLength() const { return myHeaderKeyCellLength; }

    virtual int getHeaderSize(int indx = -1) const 
    { 
        if (!myheaderlen || !myheadersize)
            return 0;
        if (indx == -1)
            return myheadersize;
        return (*myheaderlen)[indx];
    }

    virtual const char* getNormalisedHeader(const char* header) const
    {
        if (!myIsHeaderFromStart)
        {
            for (int i = 0; i < myheadersize; i++)
            {
                if (strstr(header, (*myheader)[i]))
                    return (*myheader)[i];
            }
        }
        else
        {
            for (int i = 0; i < myheadersize; i++)
            {
                if (!strncmp((*myheader)[i], header, (*myheaderlen)[i]))
                    return header + (*myheaderlen)[i];
            }
        }
        return header; 
    }

    //virtual int getCommentSize() const { return mycommentlen; }
    virtual const char* getParameterSymbol() const { return myparameterSymbol; }
    virtual void setApplicationMode(ApplicationMode_e mode) { myAppMode = mode; }
    virtual ApplicationMode_e getAppMode() const { return myAppMode; }
    const char* GetFormatSize(const char* fmt_p, bool is_free_size, int& fmt_size) const;

    int getInitialOffset() const { return myInitialOffset;  }

    void setFormatType(io_types::format_type_e fmt_type) { myBaseFormatType = fmt_type; }
    void SetHeaderFromStart(bool fromStart) { myIsHeaderFromStart = fromStart; }

    bool GetHeaderFromStartFlag() const { return myIsHeaderFromStart; }

    char getHeaderSeparator() const { return myHeaderSeparator; }

    io_types::format_type_e getFormatType()  const;

    io_types::format_type_e updateLineFormatType(const char* line);

    io_types::format_type_e getFormatType(MECReadFile* fp);
    virtual void updateFormatType(int subdeck_index, io_types::format_type_e fmt_type);

    //@}

private:
    const char                         **mycomment;
    int                                 *mycommentlen;
    int                                  mysizecomment;
    const std::vector<const char *>     *myheader;
    std::vector<int>*                    myheaderlen;
    char                                 myfreeformat;
    const char*                          myparameterSymbol;
    int                                  myLineLength;
    int                                  myCellLength;
    int                                  myHeaderKeyCellLength;
    int                                  myInitialOffset;

    int                                  myheadersize;
    bool                                 myIsHeaderFromStart = true;
    char                                 myHeaderSeparator;
    io_types::format_type_e              myBaseFormatType = io_types::FORMAT_UNDEFINED;//should be define only one place
    io_types::format_type_e              myLineFormatType = io_types::FORMAT_UNDEFINED;
    std::vector< io_types::format_type_e>     myDeckFormat;
    ApplicationMode_e                    myAppMode;
};

#endif /* MECI_SYNTAX_INFOS_H */
