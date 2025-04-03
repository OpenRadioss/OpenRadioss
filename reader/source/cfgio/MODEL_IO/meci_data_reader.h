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

#ifndef MECI_DATA_READER_H
#define MECI_DATA_READER_H

#include "meci_read_context.h"
#include <HCDI/hcdi_mec_pre_object.h>

#include "meci_syntax_infos.h"
#include <KERNEL_BASE/Structure_types.h>
#include "io_types.h"
#include "hcio.h"
#include <limits.h>

#define PseudoFileFormat_t     void
#define PseudoFileFormat_e     int  
#define PseudoFileFormatCard_t void
#define PseudoFileFormatCell_t void
#define PseudoDescriptor_t     void

class DataReaderUpdateManager_t;

/// Base class for reading data
class HCIO_DATA_DLL_API MECIDataReader {
public: /** @name Constructors & destructor */
    //@{
    /** Constructor */
    MECIDataReader (MECIReadContext *readContext_p,
                    ISyntaxInfos *syntaxInfos_p,
                    int LineLength, io_types::format_type_e formatType = io_types::format_type_e::FORMAT_UNDEFINED);
    /** Destructor */
    virtual ~MECIDataReader();
    //@}

private: /** @name Copies */
    //@{
    /** Copies this (i.e. is a sort of a copy-constructor of this), to be used to read subobjects.
     * The basic implementation returns NULL, which means that it cannot read subobjects. */
    virtual MECIDataReader * newSubobjectReader() const;
    //@}
    
public: /** @name Read data. NB: All public read... methods have to call setObject, because they might be called with a new object_p! */
    //@{
    /** Reads an object. */
    virtual bool readObjectData(const PseudoFileFormat_t *format_p,
                                IMECPreObject             *object_p,
                                void                     *model_p,
                                const PseudoDescriptor_t *descr_p,
                                int                       card_ind0=0,
                                int                       offset_card_ind0=0);
    /** Reads all following comment cards and the first single card. <br>
     * This allows to read the first data card, do a special treatment, and then
     * resume reading. <br>
     * NB: Make sure that the first data card is a CARD (single card)! */
    virtual bool readNextSingleCard(const PseudoFileFormat_t *format_p,
                                    IMECPreObject             *object_p,
                                    void                     *model_p,
                                    const PseudoDescriptor_t *descr_p,
                                    int                      *index_next_card_p,
                                    unsigned int              offset,
                                    bool                     *do_continue_p=NULL);
    /// Gets the number of lines before next header
	virtual int getNbFreeLines(int nb_token = 0, char **token = NULL);
    /// Reads the next line, skipping comments
    virtual char* readBuffer(bool do_check_eof = true, int nb_chars = -1, bool skip_comment = true) const;
public: /** @name check cell and/or card(public) */
    //@{
    /// Check if parameter is used for the cell
    virtual bool isParameterCell(const char *cell,int &nb_chars,string &param_str, bool *is_negated_p = NULL);
    /// Replace text parameters. Returns whether a replacement has been done.
    /// The default implementation doesn't do anything and returns false.
    /// The Radioss reader needs this, because Radioss text parameters may span over multiple cells.
    virtual bool replaceTextParameter(const char *cell,int nb_chars,string &param_str,
        void *model_p, const int file_index, bool* a_ok_p = NULL);
    /// if card has free format [like a comma in the line]
    bool isFreeSizeCard(const char * buffer) const ;

    virtual bool isEncrypted(const char* line) { return false; }

    //@}
private: /** @name Utility methods for reading FREE_CELL_LIST. */
    //@{
    
    virtual int getNbBlocks(void* model_p, const PseudoFileFormatCard_t *card_format_p,const PseudoDescriptor_t     *descr_p,
                            IMECPreObject            *object_p, bool *is_free_format, int& card_size, const PseudoFileFormat_t     *format_p = NULL,
                            const PseudoFileFormatCard_t* prev_card_format_p = NULL);
    /** Gets the number of lines after card_format_p.
	 * NB: Does not yet take into account all card types! To be improved if needed!
     */
    virtual int getNbLinesAfter(const PseudoFileFormatCard_t *card_format_p, const PseudoFileFormat_t *format_p,  
                                const PseudoDescriptor_t     *descr_p, IMECPreObject                 *object_p, bool &check_next_card);
    /** Gets the number of lines for card_format_p.
	 * NB: Does not yet take into account all card types! To be improved if needed!
     */
    virtual int getNbLines(const PseudoFileFormatCard_t *card_format_p, IMECPreObject  *object_p, const PseudoDescriptor_t  *descr_p, bool& check_next_card);
    //@}

public: 
    //@{
    /** Sets the current object.
     * Has to be called in all public or protected read... methods,
     * in order to check whether reading of a new object has been started. */
    void setObject (IMECPreObject *object_p);

    /// Posttreat the current object. To be called by readObjectData when reading has been finished.


    /// Reading a card
    virtual bool readNextCard(const PseudoFileFormatCard_t *card_format_p,
                              IMECPreObject                 *object_p,
                              void             *model_p,
                              const PseudoDescriptor_t     *descr_p,
                              int                           ind=-1, 
                              bool                         *do_continue_p=NULL,
                              const PseudoFileFormat_t     *format_p=NULL,
                              const PseudoFileFormatCard_t *next_card_format_p = NULL,
                              int                          *cur_s_card_indx_p = nullptr);
    /// Reading cells of a single card
    virtual bool readSingleCard(const PseudoFileFormatCard_t *card_format_p,
                                IMECPreObject                 *object_p,
                                void       *model_p,
                                const PseudoDescriptor_t     *descr_p,
                                int                           ind=-1,
                                unsigned int                  offset=0,
                                bool                         *do_continue_p=NULL);
    /// Reading Comment card
    virtual bool readCommentCard(const PseudoFileFormatCard_t* card_format_p,
                                 IMECPreObject* object_p,
                                 void* model_p,
                                 const PseudoDescriptor_t* descr_p,
                                 int                           ind = -1,
                                 unsigned int                  offset = 0,
                                 bool* do_continue_p = NULL);
    /// Reading the cells of Header card
    virtual bool readHeaderCard(const PseudoFileFormatCard_t *card_format_p,
                                IMECPreObject                 *object_p,
                                void       *model_p,
                                const PseudoDescriptor_t     *descr_p,
                                int                           ind=-1,
                                unsigned int                  offset=0,
                                bool                         *do_continue_p=NULL);
    /// Tries to read 1 or more subobjects
    virtual bool readSubobjects(const PseudoFileFormatCard_t *card_format_p,
                                IMECPreObject                 *object_p,
                                void             *model_p,
                                const PseudoDescriptor_t     *descr_p);
    /// Reads a subobject and adds it to the factory. Returns the id of the subobject.
    virtual MYOBJ_INT readSubobject(MECIDataReader               *subobj_ireader_p,
                              const PseudoFileFormat_t     *subobj_format_p,
                              void             *model_p,
                              const PseudoDescriptor_t     *subobj_descr_p,
                              const char                   *kfulltype,
                              IMECPreObject                 *object_p) const;
    
    virtual bool checkCard(const PseudoFileFormat_t *format_p,
                           int                       card_ind=0) const;

// private: ** @name Read data (private) * 

    /// Allocating arrays of a cell list in the pre-object
    virtual void reserveCellList(const PseudoFileFormatCard_t *card_format_p,
                                 IMECPreObject                 *object_p,
                                 const PseudoDescriptor_t     *descr_p,
                                 int                           nb_values);
    /// Allocating array of a cell in the pre-object
    virtual void reserveCell(const PseudoFileFormatCell_t *cell_format_p,
                             IMECPreObject                 *object_p,
                             const PseudoDescriptor_t     *descr_p,
                             int                           nb_values);
    /// Reading a LIST card
    virtual bool readList(const PseudoFileFormatCard_t *card_format_p,
                          IMECPreObject                 *object_p,
                          void       *model_p,
                          const PseudoDescriptor_t     *descr_p,
                          int                       ind=-1,
                          const PseudoFileFormatCard_t* prev_card_format_p=NULL);

    /// Reading a free cell list
    virtual bool readFreeCellList(const PseudoFileFormatCard_t *card_format_p,
                                  IMECPreObject                 *object_p,
                                  void       *model_p,
                                  const PseudoDescriptor_t     *descr_p,
                                  const PseudoFileFormat_t     *format_p = NULL,
                                  const PseudoFileFormatCard_t *prev_card_format_p = NULL);
    /// Reading a cell list
    virtual bool readCellList(const PseudoFileFormatCard_t *card_format_p,
                              IMECPreObject                 *object_p,
                              void       *model_p,
                              const PseudoDescriptor_t     *descr_p,
                              int                           ind=-1,
                              const PseudoFileFormatCard_t *prev_card_format_p = NULL);
    /// Reading an object list
    virtual bool readObjectList(const PseudoFileFormatCard_t *card_format_p,
                                IMECPreObject                 *object_p,
                                void       *model_p,
                                const PseudoDescriptor_t     *descr_p,
                                bool                          is_free);
    /// Reading a card list
    virtual bool readCardList(const PseudoFileFormatCard_t *card_format_p,
                              IMECPreObject                 *object_p,
                              void             *model_p, 
                              const PseudoDescriptor_t     *descr_p,
                              bool                         *do_continue_p=NULL,
                              bool                          is_free = false,
                              int                          *cur_s_card_indx_p = nullptr);  
    /// Reading a "if" card
    virtual bool readIfCard(const PseudoFileFormatCard_t *card_format_p,
                            IMECPreObject                 *object_p,
                            void             *model_p,
                            const PseudoDescriptor_t     *descr_p,
                            int                           ind,
                            bool                         *do_continue_p=NULL, 
                            const PseudoFileFormat_t     *format_p = NULL,
                            int                          *cur_s_card_indx_p = nullptr); //CS#13_05_11
    // Reading if inside cell
    virtual const char *readIfCell(const char                   *cell,
                            const PseudoFileFormatCell_t *cell_format_p,
                            IMECPreObject                 *object_p,
                            void       *model_p, 
                            const PseudoDescriptor_t     *descr_p,
                            int                           ind = -1,
                            bool                         *a_ok_p=NULL);

    // Reading  cell_ArrayList
    virtual const char* readCellArrayList(const char* cell,
                                          const PseudoFileFormatCell_t* cell_format_p,
                                          IMECPreObject* object_p,
                                          void* model_p,
                                          const PseudoDescriptor_t* descr_p,
                                          int      ind = -1,
                                          bool  is_free_size_format = false,
                                          bool* a_ok_p = NULL);

    /// Reading a cell
    virtual const char *readCell(const char                    *cell,
                                 const PseudoFileFormatCell_t  *cell_format_p,
                                 IMECPreObject                 *object_p,
                                 void                          *model_p,
                                 const PseudoDescriptor_t      *descr_p,
                                 int                            ind=-1,
                                 bool                           is_free_size_format=false,
                                 int                            card_type = -1);
    /// Reading a CELL_VALUE-type cell. If an ikeyword is given, method can be used to read other values than CELL_IKEYWORD.
    virtual const char *readCell_VALUE(const char                   *cell,
                                       const PseudoFileFormatCell_t *cell_format_p,
                                       IMECPreObject                 *object_p,
                                       void       *model_p,
                                       const PseudoDescriptor_t     *descr_p,
                                       int                           ind=-1,
                                       bool                          is_free_size_format=false, 
                                       int                           ikeyword=END_ARGS);

    virtual const char *readCell_ID(const char                   *cell,
                                       const PseudoFileFormatCell_t *cell_format_p,
                                       IMECPreObject                 *object_p,
                                       void             *model_p,
                                       bool                         is_free_size_format=false); 

    /// Reading a COMMENT-type cell. The contents is checked.
    virtual const char *readCell_COMMENT(const char                   *cell,
                                         const PseudoFileFormatCell_t *cell_format_p,
                                         IMECPreObject                 *object_p = NULL,
                                         bool                          is_free_size_format=false,
                                         int                           card_type = -1); 
    /// Reading a CELL_SCALAR_OR_OBJECT-type cell
    virtual const char *readCell_SCALAR_OR_OBJECT(const char                   *cell,
                                                  const PseudoFileFormatCell_t *cell_format_p,
                                                  IMECPreObject                 *object_p,
                                                  void       *model_p,
                                                  const PseudoDescriptor_t     *descr_p,
                                                  int                           ind=-1,
                                                  bool                          is_free_size_format=false); 
    /// Set the "object" and "flag" attributes while reading a CELL_SCALAR_OR_OBJECT-type cell
    virtual void readCell_SCALAR_OR_OBJECT_setObject(MYOBJ_INT                           id,
                                                     const PseudoFileFormatCell_t *cell_format_p,
                                                     IMECPreObject                 *object_p,
                                                     void       *model_p,
                                                     const PseudoDescriptor_t     *descr_p,
                                                     int                           ind=-1);

    virtual const char* readPairCell(const char* cell,
                                     const PseudoFileFormatCell_t* cell_format_p,
                                     IMECPreObject* object_p,
                                     void* model_p,
                                     const PseudoDescriptor_t* descr_p,
                                     int     ind,
                                     bool* a_ok_p,
                                     bool     is_free_size_format = false);

    /// Assign value after evaluting expression 
    void assign(const PseudoFileFormatCard_t    *card_format_p,
                IMECPreObject                 	*object_p,
                void             	*model_p, 
                const PseudoDescriptor_t     	*descr_p,
                int                          	 ind=-1);
    //@}
 
public:  
    //@{
    /// Reading an integer
    virtual int scanInt(const char *cell,const char *format,int &nb_chars,bool *a_ok_p=NULL,bool do_check_format=false, bool *is_empty_field=NULL) const;
    virtual unsigned int scanUInt(const char *cell,const char *format,int &nb_chars,bool *a_ok_p=NULL,bool do_check_format=false) const;
    virtual MYOBJ_INT scanObject(const char *cell, const char *format, int &nb_chars, bool *a_ok_p = NULL, bool do_check_format = false, bool *is_empty_field = NULL) const;
    /// Reading a double
    virtual double scanDouble(const char *cell,const char *format,int &nb_chars,bool *a_ok_p=NULL, bool *is_empty_field=NULL) const;
    /// Reading a string
    virtual char *scanString(const char *cell,const char *format,int &nb_chars,char *value,bool *a_ok_p=NULL) const;
protected:
    /// Gets the size of a "free size" cell. Counts and returns the number of read characters, in order to find the beginning of the next cell.
    virtual int GetCellFreeSize (const char *cell) const;
    /// Tests whether a character is whitespace. Base implementation here: c<=0x20, i.e. all control characters and ' '
    virtual bool IsWhitespace (char c) const;
    /// Tests whether a character is a separator between cells. Base implementation here: ',' or whitespace
    virtual bool IsCellSeparator (char c) const;
    //@}
protected: /** @name Others */
     
    //@{
    int getMultidimensionalArraySize(const PseudoFileFormatCard_t *card_format_p,
                                     IMECPreObject                 *object_p,
                                     const PseudoDescriptor_t     *descr_p);
    int isMultiArray(const PseudoFileFormatCard_t *card_format_p,
                                     const PseudoDescriptor_t     *descr_p);
    //@}
     
protected: /** @name Messages */
    //@{
    /// Gets the message of the given index
    const char *getMsg(int ind) const;
    //@}
public:
    void setFormatId(int FormatId) {myFileFormatId=FormatId;}
    int getFormatId(){return myFileFormatId;}
    virtual const char *GetFormatSize(const char *fmt_p, bool is_free_size, int &fmt_size);
    unsigned int GetCellSize(const PseudoFileFormatCell_t *cell_format_p);
    io_types::format_type_e GetKeywordFormatType() { return myKeywordFormatType; }
    void SetKeywordFormatType(io_types::format_type_e formattype) {  myKeywordFormatType = formattype ;  }
    bool getDoReadAgainFlag() { return myDoReadAgainFlag; }

    
    /// If wants to allow reader to read again set it as false (default)
    /// If don't want to allow reader to read again and display message then set as true.
    /// During first parse myDoReadAgainFlag is set as true incase if it is required to read it again
    void setDoReadAgainFlag(bool do_readagain) 
    { 
        myDoReadAgainFlag = do_readagain; 
        if (mySubobjDataReader) mySubobjDataReader->setDoReadAgainFlag(myDoReadAgainFlag);
    }
    virtual int GetLineLength(int max_len, int myLineLength);
    void setLineLength(int linelength) { myLineLength = linelength; }
    void setSkipHeaderCardReadingState(bool flag) { mySkipHeaderCardReading = flag; }
    bool getSkipHeaderCardReadingState() { return mySkipHeaderCardReading; }
    void setCurKeyTotalLineCount(int count) { myCurKeyTotalLineCount = count; }
    int getCurKeyTotalLineCount() { return myCurKeyTotalLineCount; }
    bool getIsFreeArrayCard() { return myIsFreeArrayCard; }
    void setIsFreeArrayCard(bool flag) { myIsFreeArrayCard = flag; }

    bool getIsSubobjectReader() { return myIsSubobjectReader; }
    void setIsSubobjectReader(bool flag) { myIsSubobjectReader = flag; }
protected:
    MECIReadContext *myReadContext_p;
    PseudoFileFormat_e myFileFormatId;
    ISyntaxInfos *mySyntaxInfos_p;
    io_types::format_type_e myKeywordFormatType;
    int myNoParametersBitmask = -1;
    int myNoParametersFlag = -1;
    bool                       myCanReadAgainFlag = false;
private:
    int                        myLineLength;
    DataReaderUpdateManager_t *myUpdateManager_p;
    IMECPreObject             *myObject_p;
    bool                       myIsArrayOfSubobjects;/* Flag will be set to true when  reading array of subobjects*/
    bool                       myDoReadAgainFlag;
    MECIDataReader*            mySubobjDataReader = nullptr;
    bool                       mySkipHeaderCardReading = false;
    int                        myCurKeyTotalLineCount = 0;
    bool                       myIsFreeArrayCard = false;
    bool                       myIsSubobjectReader = false;

};

#endif /* MECI_DATA_READER_H */
