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

#ifndef MECI_DATA_WRITER_H
#define MECI_DATA_WRITER_H



#include "meci_write_context.h"
#include <HCDI/hcdi_mec_pre_object.h>
#include "meci_model_scanner.h" 
#include "io_types.h"
#include <unordered_map>
#include "hcioi_solverinf.h"
#include "hcio.h"
#include "hcioi_utils.h"
#include <limits.h>
#include <float.h>

#define PseudoFileFormat_t     void
#define PseudoFileFormat_e     int  
#define PseudoFileFormatCard_t void
#define PseudoFileFormatCell_t void
#define PseudoDescriptor_t     void
#if defined _WIN32 || defined WIN32 
#pragma warning(disable:4251)    
#endif

typedef struct card_cells_temp_s {
    ff_cell_t       *a_cell_p;
    ff_cell_type_e   cell_type;
    int              ikeyword;
    string           skeyword;
    int              width;
    const char      *fmt;
    value_type_e     val_type;
    attribute_type_e att_type;
    int              att_indx;
    int              att_iden_val;
    bool             att_default_bool;
    int              att_default_int;
    double           att_default_double;
    string           att_default_string;
    bool             has_default;
} card_cells_temp_t;

typedef void (*func_ptr)(char *string, int width);
typedef std::vector < std::pair< const IMECPreObject *, std::vector<std::pair<ff_card_t *, std::pair<int, bool>>>>> VecPreobjCardvsDisplaystatus;
/// Base class for writing data
class HCIO_DATA_DLL_API MECDataWriter
{
public: /** @name Constructors & destructor */
    //@{
    /** Constructor */
    MECDataWriter (MECIWriteContext *writeContext_p,
                   int LineLength,bool writeFreeFormatFlag=false, const char *delm=" ", int removeE=1, double zeroTol=0., 
                   bool datalinecommentFlag=true, bool writeDefaultValue = false, bool hasparameter = true,bool userCommentFlag=true);
    /** Destructor */
    virtual ~MECDataWriter();
    //@}

public: /** @name Configuration */
    //@{
    /** Sets the newline string (which is by default "\n" */
    void SetNewline(const char *newline) {myNewlineString = newline;}
    //@}

public: /** @name Writing pre-objects data (public) */
    //@{
    /// Writing pre-object data. Backward compatibility version which does not work for SUBOBJECTS.
    virtual void WriteObjectData(const PseudoFileFormat_t *format_p,
                                 const IMECPreObject       &pre_object,
                                 const PseudoDescriptor_t *descr_p,
                                 int                       card_ind0=0,
                                 int                       card_ind1=-1);
    /// Writing pre-object data. New version which uses MECIModelScanner to write SUBOBJECTS.
    virtual void WriteObjectData(const PseudoFileFormat_t *format_p,
                                 const IMECPreObject       &pre_object,
                                 const PseudoDescriptor_t *descr_p,
                                 const MECIModelScanner   *model_p,
                                 int                       card_ind0=0,
                                 int                       card_ind1=-1);
    
    
    virtual int WriteComments(const PseudoFileFormat_t *format_p,
                              const IMECPreObject       &pre_object,
                              const PseudoDescriptor_t *descr_p,
                              int                       card_ind0=0,
                              int                       card_ind1=-1,
                              int                       do_write_header = 1);
    //@}

public: 
    //@{
    /// Writing a card of a file format
    virtual bool WriteCard(const PseudoFileFormatCard_t *card_p,
                           const IMECPreObject           &pre_object,
                           const PseudoDescriptor_t     *descr_p,
                           const MECIModelScanner       *model_p = NULL,
                           int                           ind = -1,
                           const PseudoFileFormatCard_t *next_card_p = NULL,
                           std::unordered_map< ff_card_t *, vector<card_cells_temp_t> > *umap_card_cells=NULL);
    /// Writing a single card of a file format
    virtual bool WriteSingleCard(const PseudoFileFormatCard_t *card_p,
                                 const IMECPreObject           &pre_object,
                                 const PseudoDescriptor_t     *descr_p,
                                 int                           ind=-1,
                                 int                          cell_ind0=0);

    virtual bool AppendHeaderCardOptions(const PseudoFileFormatCard_t*  card_p,
                                     const IMECPreObject&           pre_object,
                                     const PseudoDescriptor_t*      descr_p,
                                     int                            ind = -1,
                                     int                            cell_ind0 = 0) { return true; }

    virtual bool WriteSingleCardList(const PseudoFileFormatCard_t *card_p,
                                    ff_card_type_e            card_type,
                                    const IMECPreObject       &pre_object,
                                    const PseudoDescriptor_t *descr_p,
                                    int                       ind,
                                    int                       cell_ind0,
                                    vector<card_cells_temp_t>  &card_cells);

    /// Writing a list card
    virtual void WriteList(const PseudoFileFormatCard_t *card_p,
                           const IMECPreObject           &pre_object,
                           const PseudoDescriptor_t     *descr_p);
    /// Writing a list of cells
    virtual void WriteCellList(const PseudoFileFormatCard_t *card_p,
                               const IMECPreObject           &pre_object,
                               const PseudoDescriptor_t     *descr_p,
                               int                           ind = -1,
                               const PseudoFileFormatCard_t *prev_card_p = NULL);
    /// Writing a list of Objects
    virtual void WriteObjectList(const PseudoFileFormatCard_t *card_p,
                                 const IMECPreObject           &pre_object,
                                 const PseudoDescriptor_t     *descr_p);
    /// Writing a list of cards
    virtual void WriteCardList(const PseudoFileFormatCard_t *card_p,
                               const IMECPreObject           &pre_object,
                               const PseudoDescriptor_t     *descr_p,
							   const MECIModelScanner       *model_p);
    /// Writes subobjects
    virtual void WriteSubobjects(const PseudoFileFormatCard_t *card_p,
                                 const IMECPreObject           &pre_object,
                                 const PseudoDescriptor_t     *descr_p,
                                 const MECIModelScanner       *model_p,
                                       bool                    update_validCard = false);
    /// Writes 1 subobject
    virtual void WriteSubobject(const PseudoFileFormat_t *subobj_format_p,
                                const IMECPreObject&      pre_object,
                                const char               *otype,
                                int                       index,
                                const PseudoDescriptor_t *subobj_descr_p,
                                const MECIModelScanner   *model_p,
                                const string              fulltype = "",
                                MYOBJ_INT id = 0,
                                IMECPreObject* sub_pre_object=NULL,
                                unsigned int            subobj_indx = UINT_MAX);
    /// Writing conditionnal cards
    virtual void WriteIfCard(const PseudoFileFormatCard_t *card_p,
                             const IMECPreObject           &pre_object,
                             const PseudoDescriptor_t     *descr_p,
                             const MECIModelScanner           *model_p = NULL, 
                             int                               ind = -1,
                             std::unordered_map< ff_card_t *, vector<card_cells_temp_t> > *umap_card_cells=NULL);

    virtual void WriteIfCell(const PseudoFileFormatCell_t        *cell_p,
				             const IMECPreObject                  &pre_object,
        const PseudoDescriptor_t            *descr_p,
        int                           ind = -1);

    virtual void WritePairCell(const PseudoFileFormatCell_t        *cell_p,
				             const IMECPreObject                  &pre_object,
        const PseudoDescriptor_t            *descr_p,
        int                           ind = -1);

    virtual void WriteCellArrayList(const PseudoFileFormatCell_t* cell_p,
        const IMECPreObject& pre_object,
        const PseudoDescriptor_t* descr_p,
        int                           ind = -1);

    /// Writing cell of a card. Generic method which writes all types of cells.
    virtual void WriteCell(const PseudoFileFormatCell_t *cell_p,
                           const IMECPreObject           &pre_object,
                           const PseudoDescriptor_t     *descr_p,
                           int                           ind=-1);
    /// Writing CELL_VALUE-type cell. If an ikeyword is given, method can be used to write other values than CELL_IKEYWORD.
    virtual void WriteCell_VALUE(const PseudoFileFormatCell_t *cell_p,
                                 const IMECPreObject           &pre_object,
                                 const PseudoDescriptor_t     *descr_p,
                                 int                           ind=-1,
                                 int                           ikeyword=0);


    void WriteCell_VALUE_LIST(const PseudoFileFormatCell_t *cell_p,
                                    const IMECPreObject          &pre_object,
                                    const PseudoDescriptor_t    *descr_p,
                                    card_cells_temp_t           *cell_det,
                                    int                          ind=-1,
                                    int                          ikeyword=0);

    /// Writing CELL_SCALAR_OR_OBJECT-type cell.
    virtual void WriteCell_SCALAR_OR_OBJECT(const PseudoFileFormatCell_t *cell_p,
                                            const IMECPreObject           &pre_object,
                                            const PseudoDescriptor_t     *descr_p,
                                            int                           ind=-1);

    virtual void WriteNameValueCell(const PseudoFileFormatCell_t        *cell_p,
        const IMECPreObject                  &pre_object,
        const PseudoDescriptor_t            *descr_p,
        int                           ind = -1);

    /// Writes an integer value
    virtual void WriteInteger(const char *cell_fmt, int value);
    /// Writes an unsigned integer value
    virtual void WriteUInteger(const char *cell_fmt, unsigned int value);
    /// Writes a double value in a given format
    virtual void WriteDouble(const char *cell_fmt, unsigned int fmt_size, double value);
    /// Writes a double value in a given length
    virtual void WriteDouble(int nb_chars, double value, bool left_align=false, bool write_exp=false);
    /// Writes object id
    virtual void WriteObjectId(const char *cell_fmt, long long int value);
    /// Writes a newline string, i.e. continues the card on the following line.
    virtual void WriteNewline();
    ///Assigns value after evaluating the expression
    void Assign(const PseudoFileFormatCard_t *card_p,
                IMECPreObject                 &pre_object,
                const PseudoDescriptor_t     *descr_p,
                const MECIModelScanner       *model_p,
                int                           ind=-1);
    virtual bool WriteCommentCard(const PseudoFileFormatCard_t* card_p,
                                  const IMECPreObject& pre_object,
                                  const PseudoDescriptor_t* descr_p,
                                  int    ind = -1,
                                  int     cell_ind0 = 0);
     
    int getMultidimensionalArraySize(const PseudoFileFormatCard_t *card_format_p,
                                   const IMECPreObject                 *object_p,
                                   const PseudoDescriptor_t     *descr_p);
    int getMultidimensionalArraySize(int                       a_cell_ikw,
                                     const IMECPreObject       *object_p,
                                     const PseudoDescriptor_t *descr_p);
    int isMultiArray(const PseudoFileFormatCard_t *card_format_p,
                                   const PseudoDescriptor_t     *descr_p);
     
    /// Writes parameter string reference
    virtual void WriteParameterCell(const char *cell_fmt, const char *param_name, bool is_negated = false);
    //@}

public: /** @name Updating valid cards through pre-objects data */
    //@{
    /// Returns the reference of the vector that contains the pair of pre-objects and the valid cards.
    virtual const VecPreobjCardvsDisplaystatus &GetValidCards() const { return myVecPreCardDisplayStatus; }
    ///
    virtual void ComputeValidCards(const PseudoFileFormat_t *format_p,
                                   const IMECPreObject       &pre_object,
                                   const PseudoDescriptor_t *descr_p,
                                   const MECIModelScanner   *model_p,
                                   int                       card_ind0=0,
                                   int                       card_ind1=-1);

private:
    ///  Based on the types of card it will check if any dataname display status is ON in the card.
    virtual void UpdateValidCards(const PseudoFileFormatCard_t  *card_p,
                                  const IMECPreObject           &pre_object,
                                  const PseudoDescriptor_t      *descr_p,
                                  const MECIModelScanner        *model_p = NULL,
                                  int                            ind = -1);
    void UpdateListCardArray(const PseudoFileFormatCard_t* card_p,
                                  const IMECPreObject& pre_object,
                                  const PseudoDescriptor_t* descr_p,
                                  const MECIModelScanner* model_p,
                                  int                     ind);
    ///   Push the pre-object and card pointer to the vector which also holds the display status of each card.
    virtual void PushToListofValidCards(const IMECPreObject       &pre_object,
                                        ff_card_t                 *v_card_p,
                                        bool                       cardStatus, int ind);
    ///    Return the display status for CARD_SINGLE and HEADER card.
    virtual bool GetSingleCardDisplayStatus(const PseudoFileFormatCard_t  *card_p,
                                            const IMECPreObject           &pre_object,
                                            const PseudoDescriptor_t      *descr_p,
                                            int                            ind = -1,
                                            int                            cell_ind0 = 0);
    ///     Return each cell type of a single card.
    virtual bool GetCellDisplayStatus(const PseudoFileFormatCell_t  *cell_p,
                                      const IMECPreObject           &pre_object,
                                      const PseudoDescriptor_t      *descr_p,
                                      int                            ind = -1);
    ///     Return each cell display status of a single card.
    virtual bool GetCellDisplayStatus_VALUE(const PseudoFileFormatCell_t  *cell_p,
                                            const IMECPreObject           &pre_object,
                                            const PseudoDescriptor_t      *descr_p,
                                            int                            ind = -1,
                                            int                            ikeyword = 0);
    ///     Check for the status of the if condition.
    virtual void UpdateIfValidCards(const PseudoFileFormatCard_t     *card_p,
                                    const IMECPreObject              &pre_object,
                                    const PseudoDescriptor_t         *descr_p,
                                    const MECIModelScanner           *model_p = NULL,
                                    int                               ind = -1);
    ///     Parse the sub-object card and update the cards in the vector which holds the pre-object and card pointer.
    virtual void UpdateSubobjectValidCards(const PseudoFileFormat_t    *subobj_format_p,
                                           const char                  *otype,
                                           int                          index,
                                           const PseudoDescriptor_t    *subobj_descr_p,
                                           const MECIModelScanner      *model_p,
                                           const string                 fulltype = "",
                                           int                          id = -1,
                                           unsigned int                 subobj_indx = UINT_MAX);
    ///     Check for the status of the cell condition in the card(If present).
    virtual void GetIfCellDisplayStatus(const PseudoFileFormatCell_t        *cell_p,
                                        const IMECPreObject                 &pre_object,
                                        const PseudoDescriptor_t            *descr_p,
                                        int                                  ind = -1);
    ///     Check for the status of the cell SCALAR_OR_OBJECT.
    virtual bool Getcell_SCALAR_OR_OBJECT(const PseudoFileFormatCell_t *cell_p,
                                          const IMECPreObject           &pre_object,
                                          const PseudoDescriptor_t     *descr_p,
                                          int                           ind = -1);

    //@}

public: /** @name Utilities. */
    //@{
    /// Gets the ikeyword of the cell (or a "representative" one, if multiple).
    int GetCellIkeyword(const PseudoFileFormatCell_t *cell_p) const;
    //@}

    void setFormatId(int FormatId) {myFileFormatId=FormatId;}

    void GetIKeywordLst(const PseudoFileFormat_t *format_p,
        const IMECPreObject       &pre_object,
        const PseudoDescriptor_t *descr_p,
        const MECIModelScanner   *model_p,
        set<int>                 &ikeywordifchecklst,
        set<int>                 &keywordlst);
    bool GetCardIkeywords(const PseudoFileFormatCard_t *card_p,
        const IMECPreObject           &pre_object,
        const PseudoDescriptor_t     *descr_p,
        const MECIModelScanner       *model_p,
        set<int>                     &ikeywordifchecklst,
        set<int>                     &ikeywordlst );

    void GetIfCardIkeywords(const PseudoFileFormatCard_t *card_p,
        const IMECPreObject           &pre_object,
        const PseudoDescriptor_t     *descr_p,
        const MECIModelScanner       *model_p, 
        set<int>                     &ikeywordifchecklst,
        set<int>                     &ikeywordlst);
    void  GetIfCellIkeyword(const PseudoFileFormatCell_t        *cell_p,
        const IMECPreObject                  &pre_object,
        const PseudoDescriptor_t            *descr_p,
        set<int>                            &ikeywordifchecklst,
        set<int>                            &ikeywordlst);
    bool GetFilteredIkeywordlistFromPreScan(const MECIModelScanner  *a_model_scanner, const IDescriptor &descrp,  
                                                       const fileformat_t *format_p, IMECPreObject &preobject, 
                                                       map< int, vector<MvIKeywordList_t> > &atype_vtypeikeywordlst);
public:

    void GetSingleCardCellInfo(const PseudoFileFormatCard_t *card_p,
                               const IMECPreObject          &pre_object,
                               const PseudoDescriptor_t     *descr_p,
                               vector<card_cells_temp_t>    &loc_cell_lst);

    bool GetIfCardCellInfo(const PseudoFileFormatCard_t *card_p,
                           const IMECPreObject          &pre_object,
                           const PseudoDescriptor_t     *descr_p,
                           const MECIModelScanner       *model_p, vector<int>  *AsgnIkeylst,
                           std::unordered_map< ff_card_t *, vector<card_cells_temp_t> > &umap_card_cells,
                           vector<const ff_card_t *>    *subcardlst, int   ind);
public:

    void setFreeFormatFlag(bool flag) {  myWriteFreeFormat = flag ;  }
    bool getFreeFormatFlag() { return myWriteFreeFormat; }
    virtual void setFormatType(io_types::format_type_e type) { myKeywordFormatType = type ;  }
    io_types::format_type_e getFormatType() { return myKeywordFormatType; }
    virtual void WriteHmComments(const IMECPreObject       &pre_object, const MECIModelScanner       *model_p, bool after_header = true, char comment = '$') { }
    virtual bool getDataLineCommentFlag() { return myWriteDataLineFlag;  }
    virtual void setZeroTol(double tol) { myzeroTol = tol; }
    virtual double getZeroTol() { return myzeroTol; }
    virtual void setRemoveE(int removeE) { myremoveE = removeE; }
    virtual int getRemoveE() { return myremoveE; }
    virtual void setCompressDouble(int compressDouble) { mycompressDouble = compressDouble; }
    virtual int getCompressDouble() { return mycompressDouble; }
    virtual void setRoundDouble(int roundDouble) { myroundDouble = roundDouble; }
    virtual int getRoundDouble() { return myroundDouble; }
    virtual void setWriteDefaultValueFlag(bool flag) {  mywriteDefaultValue = flag; }
    virtual bool getWriteDefaultValueFlag() { return mywriteDefaultValue; }
public:
    virtual unsigned int GetCellSize(const ff_cell_t *cell_format_p);
    virtual const char *GetFormatSize(const char *fmt_p, int &fmt_size);
    virtual bool GetUserCommentFlag() { return myWriteUserCommentFlag;  }
    void setHasParameterFlag(bool flag) {  myhasParameter = flag; }
    inline bool getHasParameterFlag() { return myhasParameter; }
    void SetFileopFieldOverflowPtr(func_ptr ftype) { myFileopFieldOverflow =  ftype;  }
    func_ptr GetFileopFieldOverflowPtr() { return myFileopFieldOverflow;  }

public:

    virtual int GetEntityPreobject(hwHCSolverInf* solverinf_p, const IDescriptor* descr_p, IMECPreObject* preobject_p, const unsigned int subobj_indx=UINT_MAX);
    virtual int GetLineLength(int a_line_length, int myLineLength);

    virtual void PreTreatPreObject(obj_type_e etype, const IDescriptor* pdescrip, IMECPreObject& pre_object, const MECIModelScanner* model_p, IMECPreObject* parent_pre_object = NULL);

    bool getIsLastCellFlag() const { return myIsLastCell; }
private:
    virtual string GetSolverName(const IDescriptor* descrp, int ikey, int total_cell_size, int fmt_size, char* p_prev_comment);
    virtual string GetCommentCard(const char *comment_card, int total_cell_size, int fmt_size);
protected:
    io_types::format_type_e   myKeywordFormatType;
    MECIWriteContext *myWriteContext_p;
    int               myLineLength;
    int               myCurActiveLineLength;
    PseudoFileFormat_e myFileFormatId;
    string            myNewlineString;
    bool              myWriteFreeFormat;
    const char*       mydelimiter;
    int               myremoveE;
    double            myzeroTol;
    int               mycompressDouble;
    int               myroundDouble;
    bool              myWriteDataLineFlag;
    bool              mywriteDefaultValue;
    bool              myhasParameter;
    bool              myIsLastCell;
    bool              myWriteUserCommentFlag;
    func_ptr          myFileopFieldOverflow;
    VecPreobjCardvsDisplaystatus	 myVecPreCardDisplayStatus;

    /*Caching list incase of array of subobjects*/
    bool                                    myIsArraySubobjects=false;
    MvIKeywordList_t                        mySubobjVecIkeyIdenVal;
    map< int, vector<MvIKeywordList_t> >    mySubobjMapDefaultTypeKeywordLst;
    MvIKeywordSet_t                         mySubobjMainIkws;
    std::unordered_map< ff_card_t*, vector<card_cells_temp_t> > myumap_card_cells;
};



#endif /* MECI_DATA_WRITER_H */
