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
#include <UTILS/mv_cstring.h>
#include <UTILS/win32_utils.h>

#include <stdlib.h>
#include <stdio.h> 

#include <UTILS/mv_string.h>
#include <UTILS/memory_utils.h>
#include <UTILS/set_utils.h>
#include <UTILS/direction_utils.h>
#include <UTILS/str_utils.h>    
#include <KERNEL/mv_type.h>
#include <KERNEL/mv_model_descriptors.h>
#include <MESSAGE/mv_messages.h> 
#include <KERNEL_BASE/fileformat_API.h>

#include "meci_data_reader.h"
#include "meci_model_factory.h"
#include "mec_pre_object_expression_evaluator.h"
#include <KERNEL/mv_descriptor.h>
#include <KERNEL/Structure_fileformat_others.h>
#include <KERNEL/mv_data_radio_feature.h>
#include <HCDI/hcdi_multicfgkernelmgr.h>
#include <HCDI/hcdi_utils.h>
#include <hcioi_utils.h>
#include <cmath>
#include <assert.h>
#include <regex>

#define BufferSize 2560

using std::abs;


typedef vector<const char *>    LocOTypeList_t;

static int    loc_get_fmt_size(const string &fmt, bool is_free_size);//CS#25_11_09

static bool   loc_is_ikeyword_valid(int                    ikeyword,
                                    const IMECPreObject&   object,
                                    const IDescriptor*     descr_p);
static int    loc_ff_get_fmt_size(const string& linebuff = "");

typedef deque<const ff_cell_t *> LocCellFormatList_t;
typedef deque<int>               LocCellIndexList_t;

vector<MYOBJ_INT> g_subobjectMaxID(NB_CONTAINER_MODEL -1, 0);

static void DescTypeToPreobjType(attribute_type_e desc_a_type, value_type_e desc_v_type, IMECPreObject::MyAttributeType_e& atype, IMECPreObject::MyValueType_e& vtype)
{
    if (desc_a_type == ATYPE_VALUE || desc_a_type == ATYPE_SIZE)
        atype = IMECPreObject::ATY_SINGLE;
    else if (desc_a_type == ATYPE_STATIC_ARRAY || desc_a_type == ATYPE_DYNAMIC_ARRAY)
        atype = IMECPreObject::ATY_ARRAY;

    switch (desc_v_type)
    {
    case VTYPE_BOOL:
        vtype = IMECPreObject::VTY_BOOL;
        break;
    case VTYPE_INT:
        vtype = IMECPreObject::VTY_INT;
        break;
    case VTYPE_UINT:
        vtype = IMECPreObject::VTY_UINT;
        break;
    case VTYPE_FLOAT:
        vtype = IMECPreObject::VTY_FLOAT;
        break;
    case VTYPE_STRING:
        vtype = IMECPreObject::VTY_STRING;
        break;
    case VTYPE_OBJECT:
        vtype = IMECPreObject::VTY_OBJECT;
        break;
    }
    return;
}

class DataReaderUpdateManager_t
{
public:
    //DataReaderUpdateManager_t ();
    //~DataReaderUpdateManager_t ();

    void clear ();
    void addCell (const ff_cell_t *cell_p, int cell_index);
    void update (IMECPreObject *object_p);

private:
    LocCellFormatList_t myCellFormatList;
    LocCellIndexList_t  myCellIndexList;
};

static int loc_reserve_array(int                           cell_ikw,
                             const string&                 cell_skw,
                             IMECPreObject::MyValueType_e  vtype,
                             const IDescriptor*            descr_p,
                             IMECPreObject*                object_p);
/* --------- Constructors & destructor --------- */

MECIDataReader::MECIDataReader(MECIReadContext* readContext_p,
    ISyntaxInfos* syntaxInfos_p, 
    int LineLength, io_types::format_type_e formatType) : 
    myReadContext_p(readContext_p),
    myFileFormatId((PseudoFileFormat_e)FF_UNKNOWN), 
    myLineLength(LineLength), 
    myObject_p(NULL), 
    mySyntaxInfos_p(syntaxInfos_p), 
    myKeywordFormatType(formatType),
    myIsArrayOfSubobjects(false),
    myDoReadAgainFlag(false)
{
    myUpdateManager_p = new DataReaderUpdateManager_t ();
}

MECIDataReader::~MECIDataReader()
{
    if (myUpdateManager_p) delete myUpdateManager_p; 
    //if (mySyntaxInfos_p) delete mySyntaxInfos_p;
}


MECIDataReader * MECIDataReader::newSubobjectReader() const
{
    MECIDataReader *a_reader = new MECIDataReader
        (myReadContext_p, mySyntaxInfos_p, myLineLength, myKeywordFormatType);
    a_reader->setIsSubobjectReader(true);
    a_reader->setFormatId(myFileFormatId) ;
    return a_reader;
}

void MECIDataReader::setObject (IMECPreObject *object_p)
{
    if (myObject_p != object_p) {
        myObject_p = object_p;
        if (myUpdateManager_p) myUpdateManager_p->clear();
        myNoParametersFlag = -1;
    }
}
/*
void MECIDataReader::postTreatObject ()
{
    if (myUpdateManager_p) myUpdateManager_p->update(myObject_p);
}
*/

int MECIDataReader::getNbLines(const PseudoFileFormatCard_t *card_format_p, IMECPreObject  *object_p, const PseudoDescriptor_t  *descr_p, bool& check_next_card)
{
    int nb_cards = 0, a_card_type = 0;
    const ff_card_t *a_card_format_p = (const ff_card_t *)card_format_p;
    MCDS_get_ff_card_attributes(a_card_format_p,CARD_TYPE,&a_card_type,END_ARGS);
    if(CARD_SINGLE == a_card_type)
    {
        nb_cards++;
    }
    else if(a_card_type == CARD_IF)
    {
        bool a_ok=true;
        int  a_nb_ccls = 0;
        bool a_checked = false;

        MCDS_get_ff_card_attributes(a_card_format_p,CARD_NB_COND_CARD_LISTS,&a_nb_ccls,END_ARGS);
        //
        for(int j=0;a_ok && (!a_checked) && j<a_nb_ccls;++j) {
            ff_condcardlist_t *a_ccl_p  = NULL;
            expression_t      *a_expr_p = NULL;
            MCDS_get_ff_card_tab(a_card_format_p,CARD_COND_CARD_LIST,j,&a_ccl_p);
            MCDS_get_ff_condcardlist_expression(a_ccl_p,&a_expr_p);
            //
            a_checked=(a_expr_p==NULL);
            if(!a_checked) {
                MvExpression_t a_expr(a_expr_p,false);
                a_checked=object_p->EvaluateExpression((PseudoExpression_t *)(&a_expr),descr_p);
            }
            //
            if(a_checked) {
                int a_nb_cards=0;
                MCDS_get_ff_condcardlist_nb_cards(a_ccl_p,&a_nb_cards);
                for(int k=0;a_ok && k<a_nb_cards;++k) {
                    ff_card_t *a_sub_card_format_p=NULL;
                    MCDS_get_ff_condcardlist_card(a_ccl_p,k,&a_sub_card_format_p);		
                    nb_cards += getNbLines(a_sub_card_format_p, object_p, descr_p, check_next_card);
                }
            }
        }
    }
    else if (a_card_type != CARD_COMMENT)
    {
        /* If any other cards except "CARD" is present after FREE_CELL_LIST,
           enable this flag to check the data in the imported deck against any "offset(if applicable) or change of datatype"
           so as to understand how many lines come under the FREE_CELL_LIST card */
        check_next_card = true;
    }

    return nb_cards;
}
int MECIDataReader::getNbLinesAfter(const PseudoFileFormatCard_t *card_format_p, const PseudoFileFormat_t *format_p,  
                                const PseudoDescriptor_t     *descr_p, IMECPreObject                 *object_p, bool& check_next_card)
{
  int  j=0, nb_cards_after=0;
  if(format_p)
  {
      int a_nb_cards = 0;
      const fileformat_t *a_format_p  =  (const fileformat_t *)format_p;
      MCDS_get_fileformat_nb_cards(a_format_p, &a_nb_cards);
      for(j=0; j<a_nb_cards; ++j) {
          ff_card_t *a_card_format_p;
          MCDS_get_fileformat_card(a_format_p,j,&a_card_format_p);
          if((const ff_card_t *)card_format_p == a_card_format_p)
              break;
      }
      // Suppose Free_cell_list is inside an if condition, in that case free_cell_list card pointer won't be found in the above for loop
      
      if (j == a_nb_cards)
          check_next_card = true;
      //check if there are cards following FREE_CELL_LIST
      if((j+1) < a_nb_cards)
      {
          for(j=j+1; j<a_nb_cards; ++j) {
              ff_card_t *a_card_format_p;
              MCDS_get_fileformat_card(a_format_p,j,&a_card_format_p);
              nb_cards_after += getNbLines(a_card_format_p, object_p, descr_p, check_next_card);
          }
      }
      
  }
  if (myIsArrayOfSubobjects) 
      check_next_card = true;
  return nb_cards_after;
}



int MECIDataReader::getNbBlocks(void* model_p, const PseudoFileFormatCard_t* card_format_p, const PseudoDescriptor_t* descr_p,
                                IMECPreObject* object_p, bool* is_free_format, int& card_size, const PseudoFileFormat_t* format_p,
                                const PseudoFileFormatCard_t* prev_card_format_p)
{
    int nb_blocks = 0, nb_lines_after = 0;
    int i = 0, j = 0, count = 0;
    *is_free_format = false;
    int a_nb_free_lines = getNbFreeLines();

    int a_card_size = 0, a_nb_cells = 0, a_nb_value_cell = 0, a_tot_card_size = 0;
    const ff_card_t* a_card_format_p = (const ff_card_t*)card_format_p;
    // Flags check portion for 'card_format_p' start here
    int flag = 0;
    const char* a_offset_fmt = NULL;
    const char* a_offset_val = NULL;
    bool has_offset = false;
    MCDS_get_ff_card_attributes(a_card_format_p, CARD_FLAGS, &flag, END_ARGS);
    if (flag)
        has_offset = (flag & CARD_FLAG_OFFSET) ? true : false;
    if (has_offset)
        MCDS_get_ff_card_attributes(a_card_format_p, CARD_CELL_LIST_OFFSET_FORMAT, &a_offset_fmt, CARD_CELL_LIST_OFFSET_VALUE, &a_offset_val, END_ARGS);
    // Flags check portion for 'card_format_p' end here
    MCDS_get_ff_card_attributes(a_card_format_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);

    for (j = 0; j < a_nb_cells; ++j) {
        ff_cell_t* a_cell_format_p = NULL;
        int a_cell_type = 0;
        MCDS_get_ff_card_tab(a_card_format_p, CARD_CELL, j, (void*)(&a_cell_format_p));
        //
        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_TYPE, &a_cell_type, END_ARGS);

        int a_size = GetCellSize(a_cell_format_p);
        if (!a_size && a_cell_type != CELL_COMMENT)
        {
            *is_free_format = true;
            a_nb_value_cell++;
        }
        a_card_size += a_size;
    }
    card_size = a_card_size;
    int max_len = 0;
    MCDS_get_ff_card_attributes(a_card_format_p, CARD_LENGTH_MAX, &max_len, END_ARGS);
    int line_length = 0;
    line_length = GetLineLength(max_len, myLineLength);
    // Computing the previous card length(if it has NO_END flag)
    int a_prev_card_len = 0, a_prev_card_nb_cells = 0;
    if (prev_card_format_p)
    {
        const ff_card_t* a_prev_card_format_p = (const ff_card_t*)prev_card_format_p;
        int pflag = 0;
        MCDS_get_ff_card_attributes(a_prev_card_format_p, CARD_FLAGS, &pflag, END_ARGS);
        if (pflag)
        {
            bool no_end_flag = (pflag & CARD_FLAG_NO_END) ? true : false;
            if (no_end_flag)
            {
                MCDS_get_ff_card_attributes(a_prev_card_format_p, CARD_NB_CELLS, &a_prev_card_nb_cells, END_ARGS);
                for (i = 0; i < a_prev_card_nb_cells; ++i)
                {
                    ff_cell_t* a_cell_p = NULL;
                    MCDS_get_ff_card_tab(a_prev_card_format_p, CARD_CELL, i, (void*)(&a_cell_p));
                    a_prev_card_len += GetCellSize(a_cell_p);
                }
                
                
            }
        }
    }
    int a_offset_length = 0;
    if (has_offset)
        a_offset_fmt = GetFormatSize(a_offset_fmt, false, a_offset_length);

    /* Each line is supposed to be filled, except last one */
    int nb_max_block_per_line = 1;
    /*For fixed format*/
    if (*is_free_format == false)
        nb_max_block_per_line = line_length / a_card_size;

    bool a_check_next_card = false; //Flag to decide whether Offset or datatype change needs to be checked. More details about the flag can be found in getNbLines() method 
    nb_lines_after = getNbLinesAfter(card_format_p, format_p, descr_p, object_p, a_check_next_card);

    if (nb_max_block_per_line < 1)
    {
        /* A block is longer than one line, so counted by number of lines */
        nb_blocks = (a_nb_free_lines - nb_lines_after) * line_length / a_card_size;
    }
    else
    {
        if (a_nb_free_lines - nb_lines_after == 0)
            return 0;
        /*Get the format of the first cell of the FREE_CELL_LIST in order to understand the number of characters
        in the new line of input deck to be checked for Offset or datatype change*/
        ff_cell_t* a_cell_format_p = NULL;
        MCDS_get_ff_card_tab(a_card_format_p, CARD_CELL, 0, (void*)(&a_cell_format_p)); // Index '0' is been passed to get the details of first cell
        const char* a_cell_fmt = NULL;
        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_FORMAT, &a_cell_fmt, END_ARGS);
        /* Read till last line */
        int nb_fixed_free_line = 0, nb_free_free_line=0;
        myReadContext_p->pushPosition();
        bool a_continue = true;
        bool is_first_line = true; 
        char* a_card = NULL;
        size_t a_values_total_length = 0;
        while (a_continue)
        {
            a_card = readBuffer(false);
            if (a_card == NULL)
                break;
            myReadContext_p->killNLEnd(a_card);
            myReadContext_p->completeWithBlanks(a_card, line_length);

            a_continue = (a_card != NULL && !mySyntaxInfos_p->isHeader(a_card));
            if (a_continue && !is_first_line && a_check_next_card)
            {
                /*  Check for offset or datatype change in order to correctly get the number of lines in the input deck that corresponds to the current free_cell_list
                    1. Offset: If offset is defined against the current FREE_CELL_LIST, for current Line(a_card)
                             a: Continue reading if the same offset is present
                             b: Continue reading if empty characters are present in the offset length(Since Offset can be optional)
                          else Stop Reading(a_continue = false)
                    2. DataType Change: If offset is not defined, check if there is difference in the datatype of the current Line(a_card) to that of the Free_cell_list datatype */
                    //
                int fmt_size = 0; // Number of charecters(First cell length) in the current line that needs to be checked.
                if (!*is_free_format)
                {
                    if (has_offset)
                        fmt_size = a_offset_length; // If offset exists, get the 'fmt_size' from a_offset_fmt
                    else
                        a_cell_fmt = GetFormatSize(a_cell_fmt, false, fmt_size);// If offset doesn't exist, get the 'fmt_size' from a_cell_fmt(First cell of Free_Cell_list)
                }
                else
                    fmt_size = GetCellFreeSize(a_card) - 1; // If free format, get the 'fmt_size' from the input deck line(a_card) itself
                //
                std::string a_value_str(a_card, fmt_size);
                a_value_str = std::regex_replace(a_value_str, std::regex("^ +| +$|( ) +"), "$1");

                
                
                if (has_offset)
                {
                    // since offset is optional, a_temp can be "" and in that case we should not compare it with a_offset_val
                    if (a_value_str != "" && strcmp(a_value_str.c_str(), a_offset_val) != 0)
                        a_continue = false;
                }
                else
                {
                    //DataType change will be checked here
                    if (a_value_str == "")
                        a_continue = false;
                    else
                    {
                        int a_value = 0;
                        string param_str = "";
                        bool is_parameter_negated = false;
                        bool is_param_cell = isParameterCell(a_value_str.c_str(), fmt_size, param_str, &is_parameter_negated);


                        IParameter::Type value_type = IParameter::TYPE_UNKNOWN;
                        if (is_param_cell)
                        {
                            if (model_p != NULL)
                            {
                                MECIModelFactory* nc_model_p = (MECIModelFactory*)model_p;
                                value_type = nc_model_p->GetParameterValueType(param_str.c_str(), object_p->GetFileIndex());
                            }
                        }

                        // String vs Other datatypes(Int/Uint/Double/Float) will be checked
                        size_t k = strlen(a_cell_fmt);
                        if (a_cell_fmt[k - 1] == 's')
                        {
                            if (is_param_cell && value_type != IParameter::TYPE_STRING)
                                a_continue = false;
                            else if(a_value_str[0] <= '9' && a_value_str[0] >= '0')
                                a_continue = false;
                        }
                        else
                        {
                            if (is_param_cell && (value_type != IParameter::TYPE_INTEGER && value_type != IParameter::TYPE_DOUBLE))
                                a_continue = false;
                            else if (a_value_str[0] > '9' || a_value_str[0] < '0')
                                a_continue = false;
                        }
                    }
                }
            }
            if (a_continue)
            {
                // Valid line to be read through current FREE_CELL_LIST
               
                myReadContext_p->killBlanksEnd(a_card);              
                size_t len = strlen(a_card);
                if (len > line_length) //incase line length is more than solver max
                {
                    myReadContext_p->completeWithBlanks(a_card, line_length); /*first make it size of solver max length*/
                }

                
                *is_free_format = isFreeSizeCard(a_card);
                if (*is_free_format)
                {
                    ++nb_free_free_line;
                    size_t a_card_len = strlen(a_card);
                    size_t card_len_read = 0;
                    while (card_len_read < a_card_len)
                    {
                        int fmt_size = GetCellFreeSize(a_card);
                        a_card = a_card + fmt_size;
                        card_len_read = card_len_read + fmt_size;
                        nb_blocks++;
                    }
                }
                else
                {
                    ++nb_fixed_free_line;
                    int a_card_length = (int)strlen(a_card);
                    // The below treatment is needed in case of negative format specifiers and left aligned data in the input deck
                    
                    int rem = 0;
                    if (is_first_line)
                        rem = (a_card_length - a_prev_card_len) % a_card_size;
                    else
                        rem = (a_card_length - a_offset_length) % a_card_size;
                    if (rem != 0)
                        a_card_length = a_card_length + (a_card_size - rem);
                    a_values_total_length += a_card_length;
                }
            }
            if ((nb_free_free_line + nb_fixed_free_line) == a_nb_free_lines - nb_lines_after)
                a_continue = false;
            if (is_first_line)
                is_first_line = false;
        }
        
        
        if (nb_free_free_line>0)
        {
            if (has_offset)
                nb_blocks = (nb_blocks - a_prev_card_nb_cells - (nb_free_free_line - 1));
            else
                nb_blocks = nb_blocks - a_prev_card_nb_cells;
        }
        if(nb_fixed_free_line > 0)
            nb_blocks = nb_blocks + ((int)a_values_total_length - (a_prev_card_len + (nb_fixed_free_line - 1) * a_offset_length)) / a_card_size;

        if (nb_blocks == 0 && (nb_free_free_line + nb_fixed_free_line) == 1)
            nb_blocks = 1;
        myReadContext_p->popPosition();
    }
    return nb_blocks;
}

// Base implementation 
bool MECIDataReader::readObjectData (const PseudoFileFormat_t *format_p,
                                     IMECPreObject             *object_p,
                                     void         *model_p,
                                     const PseudoDescriptor_t *descr_p,
                                     int                       card_ind0,
                                     int                       offset_card_ind0)
{
    setObject (object_p); 
  
    bool                a_ok       = true;
    const fileformat_t *a_format_p = (const fileformat_t *)format_p;
    // Parsing and filling
    int a_nb_cards=0;
    MCDS_get_fileformat_nb_cards(a_format_p,&a_nb_cards);
 
    int index_next_card = card_ind0;

    // Special treatment for the first card if there is an offset
    if (0 < offset_card_ind0)
    {
        bool a_do_continue = true;
        a_ok=readNextSingleCard(format_p,object_p,model_p,descr_p,
                                &index_next_card,offset_card_ind0,
                                &a_do_continue);
        if (!a_ok || !a_do_continue) return a_ok;
    }
    int cur_s_card_indx = 0;
    // Loop over all the cards
    for(int i=index_next_card; i<a_nb_cards; ++i) {
        ff_card_t *a_card_format_p;
        ff_card_t* a_next_card_format_p = NULL;
        MCDS_get_fileformat_card(a_format_p, i, &a_card_format_p);
        int flag = 0;
        MCDS_get_ff_card_attributes(a_card_format_p, CARD_FLAGS, &flag, END_ARGS);
        if (flag)
        {
            bool no_end_flag = (flag & CARD_FLAG_NO_END) ? true : false;
            if (no_end_flag && i < (a_nb_cards - 1))
            {
                i++;
                MCDS_get_fileformat_card(a_format_p, i, &a_next_card_format_p);
            }
        }
        //
        bool a_do_continue = true;
        
        a_ok = readNextCard((const PseudoFileFormatCard_t*)a_card_format_p, object_p, model_p, descr_p,
            -1, 
            &a_do_continue, format_p, (const PseudoFileFormatCard_t*)a_next_card_format_p, &cur_s_card_indx);
        if (false == a_do_continue) break;
    }


    
    return a_ok;
}

bool MECIDataReader::readNextCard(const PseudoFileFormatCard_t *card_format_p,
                                  IMECPreObject                 *object_p,
                                  void             *model_p, 
                                  const PseudoDescriptor_t     *descr_p,
                                  int                           ind, 
                                  bool                         *do_continue_p, 
                                  const PseudoFileFormat_t     *format_p,
                                  const PseudoFileFormatCard_t *next_card_format_p,
                                  int                          *cur_s_card_indx_p)
{
  // setObject (object_p); 
  
  bool             a_ok            = true;
  bool             a_do_continue   = true; 
  const ff_card_t *a_card_format_p = (const ff_card_t *)card_format_p;
  bool doReadAgainFlagOld = getDoReadAgainFlag();
  //
  ff_card_type_e a_card_type=CARD_UNKNOWN;
  MCDS_get_ff_card_attributes(a_card_format_p,CARD_TYPE,&a_card_type,END_ARGS);
  //
  
  switch(a_card_type) {
  case CARD_BLANK:
    {
        char *a_card=readBuffer();
        if(a_card==NULL) return false;
        int a_is_header = mySyntaxInfos_p->isHeader(a_card);
        if(a_is_header)
        {
            a_ok = false;
        }
        else
        {
            a_ok = true;
            if (cur_s_card_indx_p && (*cur_s_card_indx_p) >= 0) (*cur_s_card_indx_p)++;
        }
    }
    break;
  case CARD_COMMENT:
  {
      a_ok = readCommentCard(card_format_p, object_p, model_p, descr_p,
                             ind, 0, &a_do_continue);
  }
    break;
  case CARD_SINGLE:
    {
        a_ok=readSingleCard(card_format_p,object_p,model_p,descr_p,
                            ind, 0, &a_do_continue); 
        if (a_ok && cur_s_card_indx_p && (*cur_s_card_indx_p) >= 0) (*cur_s_card_indx_p)++;
        if (next_card_format_p && a_do_continue)
        {
            const ff_card_t* a_next_card_format_p = (const ff_card_t*)next_card_format_p;
            ff_card_type_e a_next_card_type = CARD_UNKNOWN;
            MCDS_get_ff_card_attributes(a_next_card_format_p, CARD_TYPE, &a_next_card_type, END_ARGS);
            assert(a_next_card_type == CARD_CELL_LIST);
            if (a_next_card_type == CARD_CELL_LIST)
            {
                int a_is_free = 0;
                MCDS_get_ff_card_attributes(a_next_card_format_p, CARD_IS_FREE, &a_is_free, END_ARGS);
                //
                if (a_is_free) a_ok = readFreeCellList(next_card_format_p, object_p, model_p, descr_p, format_p, card_format_p);
                else          a_ok = readCellList(next_card_format_p, object_p, model_p, descr_p, ind, card_format_p);
            }
            else
                readNextCard(next_card_format_p, object_p, model_p, descr_p, -1, &a_do_continue, format_p, nullptr, cur_s_card_indx_p);
        }
    }
    break;
  case CARD_HEADER:
    {
      if (getSkipHeaderCardReadingState())
          return true;

      bool issubobjreader = getIsSubobjectReader();
      if (object_p->GetHeaderLine() == "" && !issubobjreader)
          myReadContext_p->unreadBuffer();

      if (issubobjreader)
      {
          const string& a_otype_str = MV_get_type((obj_type_e)object_p->GetEntityType());
          myReadContext_p->updateSubKeywordRead(a_otype_str.c_str());
      }

      a_ok = readHeaderCard(card_format_p, object_p, model_p, descr_p, -1, 0, &a_do_continue);
    }
    break;
  case CARD_PREREAD: 
    {
        
        a_ok=readSingleCard(card_format_p,object_p,model_p,descr_p,
                            ind, 0, &a_do_continue);  
        if(a_ok) myReadContext_p->unreadBuffer();
        // else we will stop reading here, so we must not unread

    }
    break;
  case CARD_ASSIGN:
      {
            assign(card_format_p, object_p, model_p, descr_p, ind);
      }
      break;
  case CARD_LIST:
    {
      a_ok=readList(card_format_p,object_p,model_p,descr_p,ind,next_card_format_p);
    }
    break;
  case CARD_CELL_LIST:
    {
      int a_is_free  = 0;
      MCDS_get_ff_card_attributes(a_card_format_p,CARD_IS_FREE,&a_is_free,END_ARGS);
      //
      if(a_is_free) a_ok=readFreeCellList(card_format_p,object_p,model_p,descr_p, format_p);
      else          a_ok=readCellList(card_format_p,object_p,model_p,descr_p, ind, next_card_format_p);
    }
    break;
  case CARD_OBJECT_LIST:
    {
      int a_is_free  = 0;
      MCDS_get_ff_card_attributes(a_card_format_p,CARD_IS_FREE,&a_is_free,END_ARGS);
      //
      a_ok=readObjectList(card_format_p,object_p,model_p,descr_p,a_is_free!=0);
    }
    break;
  case CARD_CARD_LIST:
    {
	  int a_is_free = 0;
	  MCDS_get_ff_card_attributes(a_card_format_p, CARD_IS_FREE, &a_is_free, END_ARGS);
	  a_ok = readCardList(card_format_p, object_p, model_p, descr_p, &a_do_continue, a_is_free != 0, cur_s_card_indx_p);
  }
    break;
  case CARD_IF:
    {
      a_ok=readIfCard(card_format_p,object_p,model_p,descr_p,ind, &a_do_continue, format_p, cur_s_card_indx_p);
    }
    break;
  case CARD_SUBOBJECTS:
    {
      if (cur_s_card_indx_p) (*cur_s_card_indx_p) = -1;  // for subobject just make it zero

      a_ok=readSubobjects(card_format_p,object_p,model_p,descr_p);
    }
    break;
  default:
    break;
  }
  //
  if (!a_ok) {
      std::string err_msg = "";
      char buffer[BufferSize];
      const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
      _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
      if (a_cur_full_name)
      {
          sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
          err_msg.append(buffer);
          err_msg.append("\n");
      }
      //
    /* In case of error, we now allow to create the object, we just stop reading of the object.
    * An error here can have multiple reasons:
    * - A mandatory card is missing, i.e. we encounter an unexpected header card.
    *   In this case, we have to unread the header card in order to read the next object.
    * - Another problem is encountered, e.g. a comment cell which doesn't match (S_243069).
    *   In this case, we must not unread the card, this might lead to an infinite loop
    *   (if this is the first card).
       * - A crypted card is encountered, but the key is not yet read,
       *   or a parameter is encountered that has not yet been read.
    *   If this happens in the initial reading pass, the object has been flagged as
    *   "to be read again", but no error must be displayed. */
      if(doReadAgainFlagOld || !myDoReadAgainFlag) // has not just been flagged as "read again"
      {
          bool isHeader = false;
          if(doReadAgainFlagOld == myDoReadAgainFlag)
          {
              // Reread the card in order to check whether it is a header card
              myReadContext_p->unreadBuffer();
              char* a_card = readBuffer();
              isHeader = mySyntaxInfos_p->isHeader(a_card);
          }
          if(isHeader)
          {
              // The erroneous card is the header of the next object.
              err_msg.append(getMsg(17)); // "ERROR: Card missing. Object might be incomplete."
              myReadContext_p->unreadBuffer();
          }
          else
          {
              // The erroneous card isn't the header of the next object, so we consider it as a card of
              // the current object and do NOT unread it.
              err_msg.append(getMsg(30)); // "ERROR: Card cannot be read. Object might be incomplete."
          }
          myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
      }
      a_do_continue = false; 
      a_ok = true;
  }
  //
  if (do_continue_p) *do_continue_p = a_do_continue; 
  return a_ok;
}

void MECIDataReader::assign(const PseudoFileFormatCard_t       *card_format_p,
                                  IMECPreObject                 *object_p,
                                  void             *model_p, 
                                  const PseudoDescriptor_t     *descr_p,
                                  int                           ind)
{
    double value = 0.0;
    const IDescriptor *a_descr_p       = (const MvDescriptor_t *)descr_p;
    ff_card_assign_header_t* a_assign_card_format_p = (ff_card_assign_header_t*)card_format_p;

    if(a_assign_card_format_p->mode == ASSIGN_MODE_EXPORT )
        return;

    MECIModelFactory* mv_model = (MECIModelFactory*)model_p;
    if (!mv_model)
        return;

    value_type_e a_result = VTYPE_UNKNOWN;
    a_result = a_descr_p->getValueType(a_assign_card_format_p->attribute_ikw);
    string skeyword = a_descr_p->getSKeyword(a_assign_card_format_p->attribute_ikw);

    assign_operator_e atype = (*a_assign_card_format_p).assign_card_type;
    int assign_card_attrib_ikey = a_assign_card_format_p->attribute_ikw;

    if (VTYPE_STRING == a_result && !(atype == ASSIGN_COMBINE || atype == ASSIGN_ERASE))
    {
        string exp_str;
        const char *p_exp_str = a_assign_card_format_p->exp_str;

        if (!(atype == ASSIGN_GET_ENTITY_VALUE || atype == ASSIGN_GET_CURRENT_ENTITY || atype == ASSIGN_PUSH
            || atype == ASSIGN_ATTRIB))
        { 
            int a_ikeyword = a_descr_p->getIKeyword(p_exp_str);

            if (a_ikeyword > 0)
            {
                int a_ind = -1;
        if (ind < 0)
        {
                    a_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, p_exp_str);
                    if(a_ind >= 0)
                        p_exp_str = object_p->GetStringValue(a_ind);
                    else
                    {
                        exp_str = a_descr_p->getStringDefaultValue(a_ikeyword, DOM_COMMON);
                        p_exp_str = exp_str.c_str();
        }
                }
        else
        {
                    a_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, p_exp_str);
                    if (a_ind >= 0)
                    {
                        p_exp_str = object_p->GetStringValue(a_ind, ind);
                    }
                    else
                    {
                        exp_str = a_descr_p->getStringDefaultValue(a_ikeyword, DOM_COMMON);
                        p_exp_str = exp_str.c_str();
                    }
                }
            }
        }

        if (ind < 0)
        {
            object_p->AddStringValue(skeyword.c_str(), p_exp_str);
        }
        else
        {
            attribute_type_e a_atype = a_descr_p->getAttributeType(a_assign_card_format_p->attribute_ikw);
            int a_cell_ind = -1;
            if (a_atype == ATYPE_VALUE)
            {
                object_p->AddStringValue(skeyword.c_str(), ind, p_exp_str);
            }
            else if (a_atype == ATYPE_DYNAMIC_ARRAY)
            {
                a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, skeyword);
                if (a_cell_ind >= 0)
                    object_p->SetStringValue(a_cell_ind, ind, p_exp_str);
            }
        }
        return;
    }


    switch (atype)
    {
    case ASSIGN_EXPRESSION:
    {
        ExpressionEvaluatorExprTk evaluator;
        MECPreObjectExpressionEvaluator pre_object_handler(object_p, a_descr_p, &evaluator, ind);
        if(a_result != VTYPE_STRING && a_assign_card_format_p->exp_str && a_assign_card_format_p->exp_str[0] != '[')
            value = pre_object_handler.Evaluate(a_assign_card_format_p->exp_str);

        break;
    }
    case ASSIGN_GET_CURRENT_ENTITY:
    case ASSIGN_GET_ENTITY_VALUE:
    {
        const ff_card_assign_entity_value_t* a_ent_val_assign_card_format_p = (const ff_card_assign_entity_value_t*)a_assign_card_format_p;

        
        int entity_ikeyword = a_ent_val_assign_card_format_p->entity_ikey;
        string entity_skeyword = a_descr_p->getSKeyword(entity_ikeyword);
        string obj_value = a_ent_val_assign_card_format_p->value_Skey;
        int row_ikeyword = a_ent_val_assign_card_format_p->row_ikey;
        int coloumn_ikeyword = a_ent_val_assign_card_format_p->col_ikey;
        string assign_card_attrib_skey = a_descr_p->getSKeyword(assign_card_attrib_ikey);
        unsigned int row = UINT_MAX;
        unsigned int coloumn = UINT_MAX;
        if (row_ikeyword > 0)
        {
            attribute_type_e a_row_atype = a_descr_p->getAttributeType(row_ikeyword);
            if (a_row_atype == ATYPE_VALUE)
            {
                string row_skey = a_descr_p->getSKeyword(row_ikeyword);
                IMECPreObject::MyValueType_e a_row_vtype = object_p->GetValueType(IMECPreObject::ATY_SINGLE, row_skey.c_str());
                if (a_row_vtype == IMECPreObject::VTY_INT)
                    row = object_p->GetIntValue(row_skey.c_str());
                else if (a_row_vtype == IMECPreObject::VTY_UINT)
                    row = object_p->GetUIntValue(row_skey.c_str());
            }
        }
        if (coloumn_ikeyword > 0)
        {
            attribute_type_e a_coloumn_atype = a_descr_p->getAttributeType(coloumn_ikeyword);
            if (a_coloumn_atype == ATYPE_VALUE)
            {
                string coloumn_skey = a_descr_p->getSKeyword(coloumn_ikeyword);
                IMECPreObject::MyValueType_e a_coloumn_vtype = object_p->GetValueType(IMECPreObject::ATY_SINGLE, coloumn_skey.c_str());
                if (a_coloumn_vtype == IMECPreObject::VTY_INT)
                    coloumn = object_p->GetIntValue(coloumn_skey.c_str());
                else if (a_coloumn_vtype == IMECPreObject::VTY_UINT)
                    coloumn = object_p->GetUIntValue(coloumn_skey.c_str());
            }
        }
        if (a_ent_val_assign_card_format_p->objTypeStr)
        {
            string obj_type_str = a_ent_val_assign_card_format_p->objTypeStr;
            std::size_t found = obj_type_str.find("/");
            if (found != std::string::npos)
            {
                IMECPreObject* a_preobj = mv_model->FindByFullType(obj_type_str);
                if (a_preobj)
                {
                    attribute_type_e a_atype = a_descr_p->getAttributeType(assign_card_attrib_ikey);
                    if (a_atype == ATYPE_VALUE)
                    {
                        CopyPreObjectAttribFromPreObject(*object_p, assign_card_attrib_skey, a_descr_p, *a_preobj, obj_value, ind);
                    }
                }
                else
                   myDoReadAgainFlag = true;
            }
        }
        else
        {
            int att_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, entity_skeyword);
            if (att_ind >= 0)
            {
                const char* type = object_p->GetObjectType(att_ind);
                if (type == NULL)
                    return;
                int a_id = object_p->GetObjectId(att_ind);
                obj_type_e ent_type = HCDI_get_entitytype(string(type));
                IMECPreObject* a_preobj = nullptr;

                if (ent_type == HCDI_OBJ_TYPE_MULTIOBJECT)
                {
                    int atttype_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, (entity_skeyword + "_type"));
                    if (atttype_ind >= 0)
                    {
                        string obj_type_str = object_p->GetStringValue(atttype_ind);
                        ent_type = HCDI_get_entitytype(obj_type_str);
                        a_preobj = mv_model->FindByObjectId(ent_type, a_id);
                    }
                }
                else
                    a_preobj = mv_model->FindByObjectId(ent_type, a_id);

                if (a_preobj)
                {
                    attribute_type_e a_atype = a_descr_p->getAttributeType(assign_card_attrib_ikey);
                    if (a_atype == ATYPE_VALUE)
                    {
                        CopyPreObjectAttribFromPreObject(*object_p, assign_card_attrib_skey, a_descr_p, *a_preobj, obj_value, ind);
                    }
                }
                else
                     myDoReadAgainFlag = true;
            }
        }
        return;
    }
    break;
    case ASSIGN_GET_NB_FREE_CARDS:
    {
        value = getNbFreeLines();
        break;
    }
    case ASSIGN_GET_DISPLAY_STATUS:
    {
        const ff_card_assign_displaystatus_t* a_displaystatus_assign_card_format_p = (const ff_card_assign_displaystatus_t*)card_format_p;
        int f_ikey = a_displaystatus_assign_card_format_p->att_ikey;
        string f_skey = a_descr_p->getSKeyword(a_displaystatus_assign_card_format_p->att_ikey);
        attribute_type_e desc_a_type = a_descr_p->getAttributeType(f_ikey);
        value_type_e     desc_v_type = a_descr_p->getValueType(f_ikey);
        IMECPreObject::MyAttributeType_e a_attype = IMECPreObject::ATY_UNKNOWN;
        IMECPreObject::MyValueType_e vtype = IMECPreObject::VTY_UNKNOWN;
        DescTypeToPreobjType(desc_a_type, desc_v_type, a_attype, vtype);

        int a_array_index = object_p->GetIndex(a_attype, vtype, f_skey);
        if (a_array_index >= 0)
            value = 1;
        else
            value = 0;
    }
    break;
    case ASSIGN_COMBINE:
    case ASSIGN_FIND:
    case ASSIGN_ERASE:
    {
        const ff_card_assign_string_t* a_assign_string_card_p = (const ff_card_assign_string_t*)card_format_p;
        int f_ikey = a_assign_string_card_p->first_ikey;
        int s_ikey = a_assign_string_card_p->second_ikey;
        std::string first_str = "";
        std::string second_str = "";
        if (f_ikey > 0)
        {
            string          fskeyword = a_descr_p->getSKeyword(f_ikey);
            attribute_type_e a_atype = a_descr_p->getAttributeType(f_ikey);
            int a_index = -1;
            if (a_atype == ATYPE_STATIC_ARRAY || a_atype == ATYPE_DYNAMIC_ARRAY)
                a_index = ind;
            int a_attrib_index = -1;
            a_attrib_index = a_index < 0 ? object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, fskeyword) :
                object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, fskeyword);
            if (a_attrib_index >= 0)
            {
                const char* a_str_val = a_index < 0 ?
                    object_p->GetStringValue(a_attrib_index) :
                    object_p->GetStringValue(a_attrib_index, a_index);
                first_str = a_str_val;
            }
            else
            {
                first_str = a_descr_p->getStringDefaultValue(f_ikey, DOM_COMMON);
            }
        }
        else
            first_str = a_assign_string_card_p->value_str;
        //
        if (s_ikey > 0)
        {
            string          sskeyword = a_descr_p->getSKeyword(s_ikey);
            attribute_type_e a_atype = a_descr_p->getAttributeType(s_ikey);
            int a_index = -1;
            if (a_atype == ATYPE_STATIC_ARRAY || a_atype == ATYPE_DYNAMIC_ARRAY)
                a_index = ind;
            int a_attrib_index = -1;
            a_attrib_index = a_index < 0 ? object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, sskeyword) :
                object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, sskeyword);
            if (a_attrib_index >= 0)
            {
                const char* a_str_val = a_index < 0 ?
                    object_p->GetStringValue(a_attrib_index) :
                    object_p->GetStringValue(a_attrib_index, a_index);
                second_str = a_str_val;
            }
            else
            {
                second_str = a_descr_p->getStringDefaultValue(s_ikey, DOM_COMMON);
            }
        }
        else
            second_str = a_assign_string_card_p->value_str;
        //
        if (atype == ASSIGN_COMBINE)
        {
            std::string final_str = "";
            final_str.append(first_str);
            final_str.append(second_str);
            int a_skey_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, skeyword);
            if (a_skey_ind >= 0)
                object_p->SetStringValue(a_skey_ind, final_str.c_str());
            else
                object_p->AddStringValue(skeyword.c_str(), final_str.c_str());
        }
        else if (atype == ASSIGN_FIND)
        {
            int found = 0;
            size_t pos = first_str.find(second_str);
            if (pos != std::string::npos)
                found = 1;
            int a_skey_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, skeyword);
            if (a_skey_ind >= 0)
                object_p->SetIntValue(a_skey_ind, found);
            else
                object_p->AddIntValue(skeyword.c_str(), found);
        }
        else if (atype == ASSIGN_ERASE)
        {
            size_t pos = first_str.find(second_str);
            if (pos != std::string::npos)
                first_str.erase(pos, second_str.length());
            int a_skey_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, skeyword);
            if (a_skey_ind >= 0)
                object_p->SetStringValue(a_skey_ind, first_str.c_str());
            else
                object_p->AddStringValue(skeyword.c_str(), first_str.c_str());
        }
        return;
    }
    case ASSIGN_ADD:
    case ASSIGN_SUB:
    case ASSIGN_DIV:
    case ASSIGN_MUL:
    case ASSIGN_ATTRIB:
        {
            const ff_card_assign_basic_operations_t* a_basic_opera_assign_card_format_p = (const ff_card_assign_basic_operations_t*)card_format_p;
            double first_val = 0.0, sec_val = 0.0;
            int f_ikey = a_basic_opera_assign_card_format_p->first_ikey;
            int s_ikey = a_basic_opera_assign_card_format_p->second_ikey;
            int ind_l = ind;
            int ind_r = ind;

            if(ind >= 0)
            {
                attribute_type_e a_atype_l = a_descr_p->getAttributeType(a_basic_opera_assign_card_format_p->first_ikey);
                attribute_type_e a_atype_r = a_descr_p->getAttributeType(a_basic_opera_assign_card_format_p->second_ikey);
                if(a_atype_l == ATYPE_VALUE)
                    ind_l = -1;
                if(a_atype_r == ATYPE_VALUE)
                    ind_r = -1;
            }

            if(f_ikey > 0)
                first_val = object_p->GetExpressionValue(a_descr_p, f_ikey, ind_l);
            else
                first_val = a_basic_opera_assign_card_format_p->firstVal;

            if(ASSIGN_ATTRIB != atype)
            {
                if(s_ikey > 0)
                    sec_val = object_p->GetExpressionValue(a_descr_p, s_ikey, ind_r);
                else 
                    sec_val = a_basic_opera_assign_card_format_p->secondVal;
            }
            switch(atype)
            {
            case ASSIGN_DIV:
                {
                    if(sec_val == 0)
                        return;
                    else
                        value = first_val / sec_val;
                }
                break;
            case ASSIGN_MUL:
                value = first_val * sec_val;
                break;
            case ASSIGN_ADD:
                value = first_val + sec_val;
                break;
            case ASSIGN_SUB:
                value = first_val - sec_val;
                break;
            case ASSIGN_ATTRIB:
                value = first_val;
                break;
            default:
                break;
            }
        }
        break;
    default:
        break;
    }



    switch(a_result) 
    {
    case VTYPE_BOOL:
        {
            bool a_var = ((value > 0.) ? true : false);
            if(ind<0) 
            {
                object_p->AddBoolValue(skeyword.c_str(), a_var);
            }
            else 
            {
                attribute_type_e a_atype = a_descr_p->getAttributeType(a_assign_card_format_p->attribute_ikw);
                int a_cell_ind = -1;
                if(a_atype == ATYPE_VALUE)
                {
                    object_p->AddBoolValue(skeyword.c_str(),a_var);
                }
                else if(a_atype == ATYPE_DYNAMIC_ARRAY)
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_BOOL,skeyword);
                    if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_assign_card_format_p->attribute_ikw,skeyword,IMECPreObject::VTY_BOOL,a_descr_p,object_p);
                    object_p->SetBoolValue(a_cell_ind,ind,a_var);
                }
            }
        }
        break;
    case VTYPE_INT:
        {
            if(ind<0) 
            {
                object_p->AddIntValue(skeyword.c_str(), (int)value);
            }
            else 
            {
                attribute_type_e a_atype = a_descr_p->getAttributeType(a_assign_card_format_p->attribute_ikw);
                int a_cell_ind = -1;
                if(a_atype == ATYPE_VALUE || a_atype == ATYPE_SIZE)
                {
                    object_p->AddIntValue(skeyword.c_str(),(int)value);
                    if (a_atype == ATYPE_SIZE)
                    {
                        MvIKeywordSet_t       a_array_ikws;
                        a_descr_p->getSizeConnectedIKeywords(a_assign_card_format_p->attribute_ikw, &a_array_ikws);
                        if (a_array_ikws.size())
                        {
                            
                            MvIKeywordSet_t::iterator a_aikw_it_begin = a_array_ikws.begin();
                            MvIKeywordSet_t::iterator a_aikw_it_end = a_array_ikws.end();
                            MvIKeywordSet_t::iterator a_aikw_it;

                            for (a_aikw_it = a_aikw_it_begin; a_aikw_it != a_aikw_it_end; ++a_aikw_it) {
                                int    a_arr_ikw = (*a_aikw_it);
                                ResizeArrayAttributesToPreObject(*object_p, a_descr_p, a_arr_ikw, (int)value);
                            }
                        }
                    }
                }
                else if(a_atype == ATYPE_DYNAMIC_ARRAY)
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,skeyword);
                    if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_assign_card_format_p->attribute_ikw,skeyword,IMECPreObject::VTY_INT,a_descr_p,object_p);
                    object_p->SetIntValue(a_cell_ind,ind,(int)value);
                }
            }
        }
        break;
    case VTYPE_UINT: 
        {
            if(ind<0) 
            {
                object_p->AddUIntValue(skeyword.c_str(), (unsigned int)value);
            }
            else 
            {
                attribute_type_e a_atype = a_descr_p->getAttributeType(a_assign_card_format_p->attribute_ikw);
                int a_cell_ind = -1;
                if(a_atype == ATYPE_VALUE)
                {
                    object_p->AddUIntValue(skeyword.c_str(),(unsigned int)value);
                }
                else if(a_atype == ATYPE_DYNAMIC_ARRAY)
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_UINT,skeyword);
                    if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_assign_card_format_p->attribute_ikw,skeyword,IMECPreObject::VTY_UINT,a_descr_p,object_p);
                    object_p->SetUIntValue(a_cell_ind,ind,(unsigned int)value);
                }
            }
        }
        break;
    case VTYPE_FLOAT:
        {
            if(ind<0) 
            {
                object_p->AddFloatValue(skeyword.c_str(), value);
            }
            else 
            {
                attribute_type_e a_atype = a_descr_p->getAttributeType(a_assign_card_format_p->attribute_ikw);
                int a_cell_ind = -1;
                if(a_atype == ATYPE_VALUE)
                {
                    object_p->AddFloatValue(skeyword.c_str(), value);
                }
                else if(a_atype == ATYPE_DYNAMIC_ARRAY)
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_FLOAT,skeyword);
                    if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_assign_card_format_p->attribute_ikw,skeyword,IMECPreObject::VTY_FLOAT,a_descr_p,object_p);
                    object_p->SetFloatValue(a_cell_ind,ind,value);
                }
            }
        }
        break;
    case VTYPE_OBJECT:
        {
            object_type_e  a_cell_otype     = a_descr_p->getObjectType(a_assign_card_format_p->attribute_ikw);
            const string  &a_cell_otype_str = MV_get_type(a_cell_otype); 
            bool           a_is_treated     = false;//model_p->IsTreated(a_cell_otype_str.c_str());
            //
            if(a_is_treated) 
            {

            } 
            else 
            {
                if(ind<0)
                {
                    object_p->AddObjectValue(skeyword.c_str(),a_cell_otype_str.c_str(),(MYOBJ_INT)value);
                }
                else 
                {
                    int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_OBJECT,skeyword);
                    if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_assign_card_format_p->attribute_ikw,skeyword,IMECPreObject::VTY_OBJECT,a_descr_p,object_p);
                    object_p->SetObjectValue(a_cell_ind,ind,a_cell_otype_str.c_str(),(MYOBJ_INT)value);
                }
            }
        }
        break;
    case VTYPE_STRING:
        {
            string a_svalue(a_assign_card_format_p->exp_str);
            if(ind<0) 
            {
                object_p->AddStringValue(skeyword.c_str(), a_svalue.c_str());
            }
            else 
            {
                attribute_type_e a_atype = a_descr_p->getAttributeType(a_assign_card_format_p->attribute_ikw);
                int a_cell_ind = -1;
                if(a_atype == ATYPE_VALUE)
                {
                    object_p->AddStringValue(skeyword.c_str(), a_svalue.c_str());
                }
                else if(a_atype == ATYPE_DYNAMIC_ARRAY)
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_STRING,skeyword);
                    if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_assign_card_format_p->attribute_ikw,skeyword,IMECPreObject::VTY_STRING,a_descr_p,object_p);
                    object_p->SetStringValue(a_cell_ind,ind,a_svalue.c_str());
                }
            }
        }
        break;

    }
}

bool MECIDataReader::readNextSingleCard(
    const PseudoFileFormat_t *format_p,
    IMECPreObject             *object_p,
    void         *model_p,
    const PseudoDescriptor_t *descr_p,
    int                      *index_next_card_p,
    unsigned int              offset,
    bool                     *do_continue_p)
{
    if (NULL == object_p) return false;
    setObject (object_p); 
    
    bool a_ok = true;

    const fileformat_t *a_format_p = (const fileformat_t *)format_p;
    int a_nb_cards=0;
    MCDS_get_fileformat_nb_cards(a_format_p,&a_nb_cards);
    int index_current_card = *index_next_card_p;

    // loop reading cards until we have found a CARD_SINGLE or the end
    ff_card_t *a_card_format_p = NULL;
    ff_card_type_e a_card_type=CARD_UNKNOWN;
    bool do_continue = true;
    while ((index_current_card < a_nb_cards)
           && do_continue)
    {
        MCDS_get_fileformat_card(a_format_p,index_current_card,&a_card_format_p);
        MCDS_get_ff_card_attributes(a_card_format_p,CARD_TYPE,&a_card_type,END_ARGS);

        switch(a_card_type) {
        case CARD_COMMENT:
            a_ok = readCommentCard(a_card_format_p, object_p, model_p, descr_p,
                                   -1, offset, do_continue_p);
            *index_next_card_p = index_current_card + 1;
            return a_ok;
        case CARD_HEADER:
            a_ok = readHeaderCard(a_card_format_p,object_p,model_p,descr_p,
                                  -1, offset, do_continue_p);
            *index_next_card_p = index_current_card + 1;
            return a_ok;
        case CARD_SINGLE:
            a_ok = readSingleCard(a_card_format_p,object_p,model_p,descr_p,
                                  -1, offset, do_continue_p);
            *index_next_card_p = index_current_card + 1;
            return a_ok;
        default:
            
            return false;
        }
    }

    // if we get here , we haven't found our CARD_SINGLE
    if (NULL!=do_continue_p) *do_continue_p = false;
    return true;
}

int MECIDataReader::getNbFreeLines(int nb_token, char **token) {
  int a_nb_free_lines=0;
  //
  myReadContext_p->pushPosition();
  //
  bool a_continue=true;
  while(a_continue) {
      char* a_card = readBuffer(false, -1, false);
      //
      bool match_token = false;
      if (nb_token && a_card != NULL)
      {
          myReadContext_p->killNLEnd(a_card);
          myReadContext_p->completeWithBlanks(a_card, myLineLength);
          for (int i = 0; i < nb_token; i++)
          {
              int len = (int)strlen(token[i]);

              if (strncmp(a_card, token[i], len) == 0)
              {
                  if (a_nb_free_lines == 0)
                  {
                      // possibility of token in first line
                      continue;
                  }
                  match_token = true;
                  if (match_token)
                  {
                      break;
                  }
              }
          }
      }
      a_continue = (a_card != NULL && !mySyntaxInfos_p->isHeader(a_card) && !match_token);
      if (a_continue && !mySyntaxInfos_p->isComment(a_card)) ++a_nb_free_lines;
  }
  //
  myReadContext_p->popPosition();
  //
  return a_nb_free_lines;
}

/*Skipping comment lines is now moved from MECIReadContext to here.*/
char * MECIDataReader::readBuffer(bool do_check_eof,int nb_chars, bool skip_comment) const
{
    char* a_line_Buffer = NULL;
    if (skip_comment == false)
    {
        a_line_Buffer = myReadContext_p->readBuffer(do_check_eof, nb_chars, skip_comment);
        return a_line_Buffer;
    }
    do
    {
        a_line_Buffer = myReadContext_p->readBuffer(do_check_eof,nb_chars);        
    }while (mySyntaxInfos_p->isComment(a_line_Buffer));
    mySyntaxInfos_p->updateLineFormatType(a_line_Buffer);
    return a_line_Buffer ;
}

bool MECIDataReader::readSingleCard(const PseudoFileFormatCard_t *card_format_p,
                                    IMECPreObject                 *object_p,
                                    void       *model_p,
                                    const PseudoDescriptor_t     *descr_p,
                                    int                           ind,
                                    unsigned int                  offset,
                                    bool                         *do_continue_p) 
{
  // setObject (object_p); 
  
  bool                  a_ok            = true;
  const ff_card_t      *a_card_format_p = (const ff_card_t *)card_format_p;
  const IDescriptor *a_descr_p       = (const MvDescriptor_t *)descr_p;

  int a_free_ikw = END_ARGS;
  MCDS_get_ff_card_attributes(a_card_format_p, CARD_IS_FREE, &a_free_ikw, END_ARGS);
  if (END_ARGS != a_free_ikw) {
      myReadContext_p->pushPosition();
  }

  //
  char *a_card=readBuffer();
  if(a_card==NULL) return false;
  myReadContext_p->killNLEnd(a_card);
  myReadContext_p->completeWithBlanks(a_card,myLineLength); 
  int flag = 0;
  MCDS_get_ff_card_attributes(a_card_format_p, CARD_FLAGS, &flag, END_ARGS);
  bool no_end_flag = false;
  if (flag)
      no_end_flag = (flag & CARD_FLAG_NO_END) ? true : false;
  // 
  int a_is_header = mySyntaxInfos_p->isHeader(a_card);

  if (END_ARGS!=a_free_ikw) {
      // the card is a FREE_CARD
      string a_free_skw   = a_descr_p->getSKeyword(a_free_ikw);
      value_type_e a_cell_vtype = a_descr_p->getValueType(a_free_ikw);
      bool isencrypted = isEncrypted(a_card);
      if (a_is_header || isencrypted) { // ... but does not exist
          if(a_cell_vtype == VTYPE_BOOL)
              object_p->AddBoolValue(a_free_skw.c_str(),false);
          else if(a_cell_vtype == VTYPE_UINT)
              object_p->AddUIntValue(a_free_skw.c_str(),0);
          else
              object_p->AddIntValue(a_free_skw.c_str(),0);
          //myReadContext_p->unreadBuffer();
          myReadContext_p->popPosition();
          if (do_continue_p) *do_continue_p = false;
          return true;
      } else { // the card exists

          if(a_cell_vtype == VTYPE_BOOL)
              object_p->AddBoolValue(a_free_skw.c_str(),true);
          else if(a_cell_vtype == VTYPE_UINT)
              object_p->AddUIntValue(a_free_skw.c_str(),1);
          else
              object_p->AddIntValue(a_free_skw.c_str(),1);
      }
  } else {
      // the card is mandatory
      if ((0 == offset) && /* ... we have not yet started reading the card (if we have, that means
                           * it is fine, which is the case for Pam data on header cards) ... */
          a_is_header)      /* ... but the card does not exist, as we find here the header card
                            * of the next object ... */
      {
          if (!getIsFreeArrayCard())
          {
              string sObj;
              MYOBJ_INT IdObj = 0;
              if (object_p != NULL)
              {
                  sObj = object_p->GetKernelFullType();
                  IdObj = object_p->GetId();
              }
              std::string err_msg = "";
              char buffer[BufferSize];
              const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
              _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
              if (a_cur_full_name)
              {
                  sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
                  err_msg.append(buffer);
                  err_msg.append("\n");
              }
              sprintf(buffer, getMsg(28), sObj.c_str(), IdObj);
              err_msg.append(buffer);
              err_msg.append(getMsg(29));
              //
              myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
              // but might be incomplete: card is missing\n"
              /*          myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR,
                  getMsg(17));*/ // "ERROR: Card missing! Preceding object might be incomplete!"
              myReadContext_p->unreadBuffer();
          }

          if (do_continue_p) *do_continue_p = false;
          return true;
      }
  }

  //
  const char *a_cur_cell = a_card;
  bool is_free_size_format = false;
  int   a_nb_cells = 0;
  ff_cell_t *a_cell_format_p=NULL;
  
  MCDS_get_ff_card_attributes(a_card_format_p,CARD_NB_CELLS,&a_nb_cells,END_ARGS);

  is_free_size_format = isFreeSizeCard(a_card);
  
  /*check to make is_free_size_format flag false if no. of cell is ONE and it is of STRING type */
  if(a_nb_cells == 1 && is_free_size_format)
  {
      int   a_cell_ikw=END_ARGS;
      MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,0,(void *)(&a_cell_format_p));
      MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_IKEYWORD,&a_cell_ikw,END_ARGS);
      value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);
      if(a_cell_vtype == VTYPE_STRING)
      {
         is_free_size_format = false;
      }
  }

  if (0 < offset) {
      if (strlen (a_card) < offset) return false;
      a_cur_cell = a_card + offset;
  }
  ff_card_type_e a_card_type = CARD_UNKNOWN;
  MCDS_get_ff_card_attributes(a_card_format_p, CARD_TYPE, &a_card_type, END_ARGS);
  //
  int j = 0;
  int cur_fmt_size = 0;
  for (j = 0; a_ok && j < a_nb_cells; j++) {

    MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,j,(void *)(&a_cell_format_p));
    //
    const char* a_cur_cell_0 = a_cur_cell;
    a_cur_cell=readCell(a_cur_cell_0,
			(const PseudoFileFormatCell_t *)a_cell_format_p,
			object_p,
			model_p,
			descr_p,
          ind, is_free_size_format, a_card_type);

    if (mySyntaxInfos_p->IsFormatSupportedForContinueNextLine())
    {
        const char* a_cell_fmt = NULL;
        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_FORMAT, &a_cell_fmt, END_ARGS);
        const string& fmt = a_cell_fmt;
        int a_size = atoi(fmt.substr(1, fmt.find_first_of(".sdilfeg") - 1).c_str());
        int fmt_size = a_size < 0 ? (-a_size) : a_size;
        cur_fmt_size += fmt_size;

        //cur_fmt_size += a_cur_cell - a_cur_cell_0;
        if (mySyntaxInfos_p->HasLengthReachedForNextLine(cur_fmt_size))
        {
            a_card = readBuffer();
            if (a_card == NULL)
                return false;

            myReadContext_p->killNLEnd(a_card);
            myReadContext_p->completeWithBlanks(a_card, myLineLength);
            a_cur_cell = a_card;
            int a_is_header = mySyntaxInfos_p->isHeader(a_cur_cell);
            if (a_is_header)
            {
                a_ok = false;
                myReadContext_p->unreadBuffer();
                break;
            }
            //
            is_free_size_format = isFreeSizeCard(a_card);
            offset = mySyntaxInfos_p->getInitialOffset(); // Need to get based of solver... should take care of free format as well with long format
            if (offset && is_free_size_format)
            {
                offset = 1;
                for (const char* p = a_card; *p; ++p) {
                    if (*p == mySyntaxInfos_p->getFreeFormatSpecifier()) {
                        offset++;
                        break;
                    }
                }
            }

            if (strlen(a_cur_cell) < offset)
                return false;
            a_cur_cell = a_cur_cell + offset;
            cur_fmt_size = 0;
        }
    }
    a_ok=(a_cur_cell!=NULL);
  }
  if (a_ok && j == a_nb_cells && !no_end_flag)
  {
      if (a_card_type == CARD_SINGLE)
      {
          int a_cell_size = static_cast<int>(strlen(a_cur_cell));
          for(int i=0;i<a_cell_size;i+= 10)
          {
              int size = a_cell_size - i;
              if (size <= 0)
                  break;
              size = size < 10 ? size : 10;
              string str(a_cur_cell + i, size);
              bool isWhitespaceOrCellSeparator = std::all_of(str.begin(), str.end(), [this](char c) 
              {
                  return IsWhitespace(c) || IsCellSeparator(c);
              });
              if(!isWhitespaceOrCellSeparator)
              {
                  bool isFloatVal = true;
                  float fVal = 0.0;
                  try
                  {
                      fVal = stof(str);
                  }
                  catch(...)
                  {
                      isFloatVal = false;
                  }
                  if (!isFloatVal || (isFloatVal && fVal != 0.0) )
                  {
                  std::string war_msg = "";
                  char buffer[BufferSize];
                  const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
                  _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
                  if (a_cur_full_name)
                  {
                      sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
                  war_msg.append(buffer);
                      war_msg.append("\n");
                  }
                  const char *a_fulltype = object_p->GetKernelFullType();
                  string substr_key("");
                  if (a_fulltype)
                  {
                      string sfulltype(a_fulltype);
                      substr_key = sfulltype.substr(sfulltype.rfind("/") + 1);
                  }
                      char* a_buff = (char*)myReadContext_p->killBlanksBegin(a_cur_cell + i);
                  myReadContext_p->killBlanksEnd(a_buff);
                  sprintf(buffer, getMsg(35), substr_key.c_str(), a_buff);
                  war_msg.append(buffer);
                  myReadContext_p->displayMessage(myReadContext_p->MSG_WARNING, war_msg.c_str());
                  break;
              }
          }
      }
  }
  }
  // if no_end_flag is there, do an unread buffer
  // While reading the next card, skipping the already read data will be taken care of
  if (no_end_flag)
      myReadContext_p->unreadBuffer();
  //
  return a_ok;
}

bool MECIDataReader::readCommentCard(const PseudoFileFormatCard_t *card_format_p,
                                     IMECPreObject                 *object_p,
                                     void       *model_p,
                                     const PseudoDescriptor_t     *descr_p,
                                     int                           ind,
                                     unsigned int                  offset,
                                     bool                         *do_continue_p)
{
    if (!card_format_p)
        return false;
    const ff_card_t* a_card_format_p = (const ff_card_t*)card_format_p;
    int   a_nb_cells = 0;
    MCDS_get_ff_card_attributes(a_card_format_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);
    //If NB_CELLS == 1, it is just a simple comment card and so the reading gets skipped 
    if (a_nb_cells == 1)
        return true;
    //If NB_CELLS > 1, some attributes are meant to be read
    ff_cell_t* a_cell_format_p = NULL;
    MCDS_get_ff_card_tab(a_card_format_p, CARD_CELL, 0, (void*)(&a_cell_format_p));
    const char* a_cell_string = NULL;
    MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_STRING, &a_cell_string, END_ARGS);
    
    //If there is only one single comment line, then it ought to be read
    //Else Get the First CELL_STRING from CFG COMMENT CARD  and compare with all the comment lines to see which line needs to be read
    char* a_card = NULL;
    a_card = readBuffer(true, -1, false);
    if (a_card == NULL)
        return false;
    if (!(mySyntaxInfos_p->isComment(a_card)))
    {
        myReadContext_p->unreadBuffer();
        return true;
    }
    myReadContext_p->killNLEnd(a_card);
    const char* a_card_assigned = a_card;
    //Making a local copy of the first comment line
    std::string card(a_card_assigned);
    a_card = readBuffer(true, -1, false);
    if (a_card != NULL)
    {
        
        if (mySyntaxInfos_p->isComment(a_card))
        {
            a_card_assigned = card.c_str();
            myReadContext_p->unreadBuffer();
            bool do_while_flag = true;
            do
            {
                if (mySyntaxInfos_p->isComment(a_card_assigned))
                {
                    size_t len = strlen(a_cell_string);
                    //Comparision happens here
                    if (!strncmp(a_cell_string, a_card_assigned, len))
                        do_while_flag = false;
                }
                else
                {
                    myReadContext_p->unreadBuffer();
                    do_while_flag = false;
                    a_card_assigned = NULL;
                }
                if (do_while_flag)
                {
                    char* a_loc_card = readBuffer(true, -1, false);
                    if (a_loc_card == NULL)
                        return false;
                    myReadContext_p->killNLEnd(a_loc_card);
                    a_card_assigned = a_loc_card;
                }
            } while (do_while_flag);
            if (a_card_assigned == NULL)
                return true;
            card = a_card_assigned;// get the line that needs to be read
        }
        else
            myReadContext_p->unreadBuffer();
    }
    else
        myReadContext_p->unreadBuffer();
    size_t len = strlen(a_cell_string);
    size_t str_len = strlen(card.c_str());
    if (str_len >= len)
    {
        //Comparision happens here to see if the correct comment card is going to be read or not
        if (strncmp(a_cell_string, card.c_str(), len))
            return true;
    }
    else
        return true;
    if (str_len < myLineLength)
        card.append(myLineLength - str_len, ' ');
    bool is_free_size_format = false; // isFreeSizeCard(card.c_str());
    const char* a_cell = card.c_str();
    bool a_ok = true;
    for (int j = 0; a_ok && j < a_nb_cells; j++)
    {
        ff_cell_t* a_lcell_format_p = NULL;
        MCDS_get_ff_card_tab(a_card_format_p, CARD_CELL, j, (void*)(&a_lcell_format_p));
        //
        a_cell = readCell(a_cell, (const PseudoFileFormatCell_t*)a_lcell_format_p,
            object_p, model_p, descr_p, ind, is_free_size_format, CARD_COMMENT);
        a_ok = (a_cell != NULL);
    }
    return a_ok;
}
//
bool MECIDataReader::readHeaderCard(const PseudoFileFormatCard_t *card_format_p,
                                    IMECPreObject                 *object_p,
                                    void       *model_p,
                                    const PseudoDescriptor_t     *descr_p,
                                    int                           ind,
                                    unsigned int                  offset,
                                    bool                         *do_continue_p)
{
    bool                  a_ok            = true;
    const ff_card_t      *a_card_format_p = (const ff_card_t *)card_format_p;
    const IDescriptor *a_descr_p       = (const MvDescriptor_t *)descr_p;
    //

    char *a_card= NULL;
    char aa_card[200];
    if(object_p->GetHeaderLine() == "") 
        a_card = readBuffer();
    else
    {
        a_card =  (char *)(object_p->GetHeaderLine().c_str());
        mySyntaxInfos_p->updateLineFormatType(a_card);
    }
    if(a_card==NULL )
        return false;
    
    strcpy(aa_card, a_card);
    a_card = aa_card;

    myReadContext_p->killNLEnd(a_card);

    if(getIsSubobjectReader())
    {
        object_p->SetInputFullType(a_card);
    }

    myReadContext_p->completeWithBlanks(a_card, myLineLength);
    //
    const char *a_cur_cell = a_card;
    if (0 < offset)
    {
        if (strlen (a_card) < offset)
            return false;
        a_cur_cell = a_card + offset;
    }
    int   a_nb_cells = 0;
    MCDS_get_ff_card_attributes(a_card_format_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);

    bool is_free_size_format = false;

    is_free_size_format = isFreeSizeCard(a_card);


    /*check to make is_free_size_format flag false if no. of cell is ONE and it is of STRING type*/
    if (a_nb_cells == 1 && is_free_size_format)
    {
        int   a_cell_ikw = END_ARGS;
        ff_cell_t* a_cell_format_p = NULL;
        MCDS_get_ff_card_tab(a_card_format_p, CARD_CELL, 0, (void*)(&a_cell_format_p));
        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_IKEYWORD, &a_cell_ikw, END_ARGS);
        value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);
        if (a_cell_vtype == VTYPE_STRING)
        {
            is_free_size_format = false;
        }
    }

    //Usually, in the header card consists of CELL_COMMENT, CELL_VALUE, CELL_COMMENT ... so on
    //For the comment card we can use directly readCell_COMMENT(), for value cards we have
    // have to process the string using exactly until the next comment
    int cur_fmt_size = 0;
    for(int j=0; a_ok && j<a_nb_cells; ++j)
    {
        ff_cell_t *a_cell_format_p = NULL;
        MCDS_get_ff_card_tab(a_card_format_p, CARD_CELL, j, (void *)(&a_cell_format_p));
        ff_cell_type_e  a_cell_type = CELL_UNKNOWN;
        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_TYPE, &a_cell_type, END_ARGS);

        if(a_cell_type == CELL_COMMENT)
        {
            const char* a_cur_cell_0 = a_cur_cell;
            a_cur_cell =  readCell_COMMENT(a_cur_cell, a_cell_format_p);
            cur_fmt_size += a_cur_cell - a_cur_cell_0;


            
            //For /BCS/ROT/X/2 last card is an optional and it used to fail while reading this card
            if(a_cur_cell == NULL)
            {
                return true;
            }
        }
        else
        {
            char        buff[201];
            const char *a_cell_string = NULL;
            //string temp_str(a_cur_cell);
            const char *temp = NULL;
            buff[0]='\0';
            if(j < a_nb_cells-1)
            {   //get the next comment for knowing the number of characters
                ff_cell_t *a_cell_cmt_format_p = NULL;
                MCDS_get_ff_card_tab(a_card_format_p, CARD_CELL, j+1, (void *)(&a_cell_cmt_format_p));
                MCDS_get_ff_cell_attributes(a_cell_cmt_format_p, CELL_STRING, &a_cell_string, END_ARGS);
            }
            //find the position of the next comment start
            int a_size = 0;
            if(a_cell_string)
                temp = strstr(a_cur_cell, a_cell_string);
            //if(a_cell_string)
                //a_size = temp_str.find(a_cell_string);
            //if it reached to string end
            //if(!a_size)
                //a_size = strlen(temp_str.c_str());

            if(temp)
                a_size = (int)strlen(a_cur_cell)-(int)strlen(temp);
            else//if it reached to string end
                a_size = (int)strlen(a_cur_cell);

            if(a_size < 201)
            {
                memmove(buff, a_cur_cell, a_size*sizeof(char));
                buff[a_size]='\0';
            }
            const char* a_buff_0 = buff;
            const char *a_str = readCell (buff, a_cell_format_p, object_p, model_p, descr_p, ind, is_free_size_format);
            //cur_fmt_size += a_str - a_buff_0;

            const char* a_cell_fmt = NULL;
            MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_FORMAT, &a_cell_fmt, END_ARGS);
            const string& fmt = a_cell_fmt;
            int a_size1 = atoi(fmt.substr(1, fmt.find_first_of(".sdilfeg") - 1).c_str());
            int fmt_size = a_size1 < 0 ? (-a_size1) : a_size1;
            cur_fmt_size += fmt_size;



            if(temp == NULL && a_str)
                a_cur_cell = a_str;
            else
                a_cur_cell += a_size;
        }

        if (mySyntaxInfos_p->IsFormatSupportedForContinueNextLine())
        {
            if (mySyntaxInfos_p->HasLengthReachedForNextLine(cur_fmt_size))
            {
                a_card = readBuffer();
                if (a_card == NULL)
                    return false;

                myReadContext_p->killNLEnd(a_card);
                myReadContext_p->completeWithBlanks(a_card, myLineLength);
                a_cur_cell = a_card;
                int a_is_header = mySyntaxInfos_p->isHeader(a_cur_cell);
                if (a_is_header)
                {
                    a_ok = false;
                    myReadContext_p->unreadBuffer();
                    break;
                }
                is_free_size_format = isFreeSizeCard(a_card);
                //
                offset = mySyntaxInfos_p->getInitialOffset(); // Need to get based of solver... should take care of free format as well with long format
                if (offset && is_free_size_format)
                {
                    offset = 1;
                    for (const char* p = a_cur_cell; *p; ++p) {
                        if (*p == mySyntaxInfos_p->getFreeFormatSpecifier()) {
                            offset++;
                            break;
                        }
                    }
                }
                if (strlen(a_cur_cell) < offset)
                    return false;
                a_cur_cell = a_cur_cell + offset;
                cur_fmt_size = 0;
            }
        }


        a_ok=(a_cur_cell!=NULL);
    }
    //
    return a_ok;
}

void MECIDataReader::reserveCellList(const PseudoFileFormatCard_t *card_format_p,
				      IMECPreObject                 *object_p,
				      const PseudoDescriptor_t     *descr_p,
				      int                           nb_values)   
{
  const ff_card_t      *a_card_format_p = (const ff_card_t *)card_format_p;
  const IDescriptor *a_descr_p       = (const MvDescriptor_t *)descr_p;
  //
  int a_size_ikw = END_ARGS;
  int a_nb_cells = 0;
  ff_card_type_e a_card_type = CARD_UNKNOWN;
  MCDS_get_ff_card_attributes(a_card_format_p,
			      CARD_NB_CELLS,&a_nb_cells,
			      CARD_SIZE,    &a_size_ikw,
                  CARD_TYPE,    &a_card_type,
			      END_ARGS);
  if(a_size_ikw>0) {
    string a_size_skw=a_descr_p->getSKeyword(a_size_ikw);
    if(isMultiArray(card_format_p,descr_p) ==-1) { 
        if (a_card_type == CARD_CELL_LIST)
        {
            int a_index = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_size_skw);
            if (a_index<0)
            {
                object_p->AddIntValue(a_size_skw.c_str(), nb_values);
                //int cur_val = object_p->GetIntValue(a_index);
                //if (cur_val < nb_values) /*incase of CELL_LIST inside CARD_LIST, make sure size is not less otherwise it will overwrite with CARD_LIST size*/
                //    object_p->SetIntValue(a_index, nb_values);
            }
        }
        else
            object_p->AddIntValue(a_size_skw.c_str(), nb_values);
    }
  }
  //
  for(int j=0;j<a_nb_cells;++j) {
    ff_cell_t *a_cell_format_p=NULL;
    MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,j,(void *)(&a_cell_format_p));
    //

    reserveCell ((PseudoFileFormatCell_t *) a_cell_format_p, object_p, descr_p, nb_values);
  }
}

void MECIDataReader::reserveCell(const PseudoFileFormatCell_t *cell_format_p,
                                 IMECPreObject                 *object_p,
                                 const PseudoDescriptor_t     *descr_p,
                                 int                           nb_values)
{
    const IDescriptor *a_descr_p       = (const IDescriptor *)descr_p;
    const ff_cell_t      *a_cell_format_p = (const ff_cell_t *)cell_format_p;

    //
    ff_cell_type_e a_cell_type=CELL_UNKNOWN;
    MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_TYPE,&a_cell_type,END_ARGS);
    //
    switch(a_cell_type) {
    case CELL_VALUE:
    case CELL_PAIR:
      {
	int a_cell_ikw=END_ARGS;
	MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_IKEYWORD,&a_cell_ikw,END_ARGS);
        attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
        if (ATYPE_STATIC_ARRAY == a_atype || ATYPE_DYNAMIC_ARRAY == a_atype)
        {
	//
	string       a_cell_skw   = a_descr_p->getSKeyword(a_cell_ikw);
	value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);
    if (a_cell_type == CELL_PAIR)
        nb_values = nb_values * 2;
    IMECPreObject::MyValueType_e a_vtype = HCDIGetPVTypeFromDVType(a_cell_vtype);
    int a_attrib_index = object_p->GetIndex(IMECPreObject::ATY_ARRAY, a_vtype, a_cell_skw);

    if (a_attrib_index >= 0)
    {
       object_p->resizeArray(a_vtype, a_attrib_index, nb_values);
    }
    else
    {
	//
	switch(a_cell_vtype) {
	case VTYPE_BOOL:    object_p->AddBoolArray(a_cell_skw.c_str(),nb_values);    break;
	case VTYPE_INT:    object_p->AddIntArray(a_cell_skw.c_str(),nb_values);    break;
	case VTYPE_UINT:   object_p->AddUIntArray(a_cell_skw.c_str(),nb_values);    break;
	case VTYPE_FLOAT:  object_p->AddFloatArray(a_cell_skw.c_str(),nb_values);  break;
	case VTYPE_STRING: object_p->AddStringArray(a_cell_skw.c_str(),nb_values); break;
	case VTYPE_OBJECT: object_p->AddObjectArray(a_cell_skw.c_str(),nb_values); break;
	default:           break;
	}
        }
    }
	//
	
      }
      break;
    case CELL_DIR_RADIO:
      {
	int a_cell_ikw=END_ARGS;
	MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_IKEYWORD,&a_cell_ikw,END_ARGS);
        attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
        if (ATYPE_STATIC_ARRAY == a_atype || ATYPE_DYNAMIC_ARRAY == a_atype)
        {
	//
	string a_cell_skw=a_descr_p->getSKeyword(a_cell_ikw);
	object_p->AddIntArray(a_cell_skw.c_str(),nb_values);
        }
	//
	
      }
      break;
    case CELL_DIR_FLAGS:
      {
	int a_xdir_ikw=END_ARGS;
	int a_ydir_ikw=END_ARGS;
	int a_zdir_ikw=END_ARGS;
	MCDS_get_ff_cell_attributes(a_cell_format_p,
				    CELL_DIRX_IKW,&a_xdir_ikw,
				    CELL_DIRY_IKW,&a_ydir_ikw,
				    CELL_DIRZ_IKW,&a_zdir_ikw,
				    END_ARGS);
	//

        attribute_type_e a_atype = a_descr_p->getAttributeType(a_xdir_ikw);
        if (ATYPE_STATIC_ARRAY == a_atype || ATYPE_DYNAMIC_ARRAY == a_atype)
        {
	string a_xdir_skw=a_descr_p->getSKeyword(a_xdir_ikw);
            object_p->AddIntArray(a_xdir_skw.c_str(), nb_values);
        }
        a_atype = a_descr_p->getAttributeType(a_ydir_ikw);
        if (ATYPE_STATIC_ARRAY == a_atype || ATYPE_DYNAMIC_ARRAY == a_atype)
        {
	string a_ydir_skw=a_descr_p->getSKeyword(a_ydir_ikw);
            object_p->AddIntArray(a_ydir_skw.c_str(), nb_values);
        }

        a_atype = a_descr_p->getAttributeType(a_zdir_ikw);
        if (ATYPE_STATIC_ARRAY == a_atype || ATYPE_DYNAMIC_ARRAY == a_atype)
        {
	string a_zdir_skw=a_descr_p->getSKeyword(a_zdir_ikw);
	object_p->AddIntArray(a_zdir_skw.c_str(),nb_values);
        }
	//
	
      }
      break;
      
    case CELL_DIGITS:
      {
	int a_nb_ikws=0;
	MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_NB_IKEYWORDS,&a_nb_ikws,END_ARGS);
	//
	for(int i=0;i<a_nb_ikws;++i) {
	  int a_ikeyword=END_ARGS;
	  MCDS_get_ff_cell_tab(a_cell_format_p,CELL_IKEYWORD,i,(void *)(&a_ikeyword));
	  //
	  bool a_is_valid=loc_is_ikeyword_valid(a_ikeyword,*object_p,a_descr_p);
	  if(a_is_valid) {
	    string a_skeyword=a_descr_p->getSKeyword(a_ikeyword);
	    object_p->AddIntArray(a_skeyword.c_str(),nb_values);
	  }
	}
      }
      break;
    case CELL_SCALAR_OR_OBJECT:
        {
            int   a_flag_ikw=END_ARGS,a_scalar_ikw=END_ARGS,a_object_ikw=END_ARGS;
            MCDS_get_ff_cell_attributes(a_cell_format_p,
                CELL_FLAG_IKW,  &a_flag_ikw,
                CELL_SCALAR_IKW,&a_scalar_ikw,
                CELL_OBJECT_IKW,&a_object_ikw,
                END_ARGS);

            string       a_flag_skw       = a_descr_p->getSKeyword(a_flag_ikw);
            string       a_scalar_skw     = a_descr_p->getSKeyword(a_scalar_ikw);
            string       a_object_skw     = a_descr_p->getSKeyword(a_object_ikw);

            value_type_e a_flag_vtype = a_descr_p->getValueType(a_flag_ikw);
            if (a_flag_vtype == VTYPE_BOOL)
                object_p->AddBoolArray(a_flag_skw.c_str(), nb_values);
            else if (a_flag_vtype == VTYPE_INT)
                object_p->AddIntArray(a_flag_skw.c_str(), nb_values);
            else if (a_flag_vtype == VTYPE_UINT)
                object_p->AddUIntArray(a_flag_skw.c_str(), nb_values);
            else if (a_flag_vtype == VTYPE_FLOAT)
                object_p->AddFloatArray(a_flag_skw.c_str(), nb_values);

            object_p->AddObjectArray(a_object_skw.c_str(),nb_values);

            value_type_e a_scalar_vtype = a_descr_p->getValueType(a_scalar_ikw);
            if(a_scalar_vtype == VTYPE_BOOL)
                object_p->AddBoolArray(a_scalar_skw.c_str(),nb_values); 
            else if(a_scalar_vtype == VTYPE_INT)
                object_p->AddIntArray(a_scalar_skw.c_str(),nb_values); 
            else if(a_scalar_vtype == VTYPE_UINT)
                object_p->AddUIntArray(a_scalar_skw.c_str(),nb_values); 
            else if(a_scalar_vtype == VTYPE_FLOAT) 
                object_p->AddFloatArray(a_scalar_skw.c_str(),nb_values); 
            break;
        }
    default:
      break;
    }
}

bool MECIDataReader::readList(const PseudoFileFormatCard_t *card_format_p,
                              IMECPreObject                 *object_p,
                              void       *model_p,
                              const PseudoDescriptor_t     *descr_p,
                              int                       ind,
                              const PseudoFileFormatCard_t* prev_card_format_p)
{
  bool                  a_ok            = true;
  const ff_card_t      *a_card_format_p = (const ff_card_t *)card_format_p;
  const IDescriptor *a_descr_p       = (const MvDescriptor_t *)descr_p;
  //
  int a_nb_cells      = 0;
  MCDS_get_ff_card_attributes(a_card_format_p,CARD_NB_CELLS,&a_nb_cells,END_ARGS);
  //
  int   a_line_length = -1, ikey = -1;
  MCDS_get_ff_card_attributes(a_card_format_p,CARD_LENGTH_MAX,&a_line_length,END_ARGS);
  MCDS_get_ff_card_attributes(a_card_format_p, CARD_IKEYWORD_NB_BLOCKS, &ikey, END_ARGS);

  int a_card_size = 0;
  if(a_line_length<=0) 
  {
      a_line_length = myLineLength;
  }

  //
  bool is_free_size_format = false;
  char *a_card        = NULL;
  const char *a_cell  = NULL;
  for(int i=0;a_ok && i<a_nb_cells;++i) {
      ff_cell_t *a_cell_format_p=NULL;
      MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,i,(void *)(&a_cell_format_p));
      //
      if (ikey > 0)
      {
          string skey = a_descr_p->getSKeyword(ikey);
          int nb_blocks = object_p->GetIntValue(skey.c_str());
          a_card_size = GetCellSize(a_cell_format_p);
          a_line_length = a_card_size * nb_blocks;
          if (a_line_length <= 0)
          {
              a_line_length = myLineLength;
          }
      }
      int a_cell_ikw=END_ARGS;
      MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_IKEYWORD,&a_cell_ikw,END_ARGS);
      //
      if (END_ARGS == a_cell_ikw) continue; // for comment cells (CS#744#24_04_06)
      
      
      /*if it is multidemensial array*/
      int a_nb_values = getMultidimensionalArraySize(card_format_p,object_p,descr_p);
      if(a_nb_values ==0){
      attribute_type_e a_attr_type = a_descr_p->getAttributeType(a_cell_ikw);
      if (a_attr_type == ATYPE_STATIC_ARRAY) {
          a_nb_values = a_descr_p->getSize(a_cell_ikw);
      } else if (a_attr_type == ATYPE_DYNAMIC_ARRAY) {
          int a_size_ikw = a_descr_p->getSizeIKeyword(a_cell_ikw);
          string a_size_skw = a_descr_p->getSKeyword(a_size_ikw);
          value_type_e a_cell_vtype = a_descr_p->getValueType(a_size_ikw);
          if(a_cell_vtype == VTYPE_UINT)
          {
              int a_size_index = object_p->GetIndex (IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_UINT, a_size_skw);
              if (0 > a_size_index) return false; // The size is not yet read, this must not happen!
              a_nb_values = (int)object_p->GetUIntValue(a_size_index);
          }
          else
          {
              int a_size_index = object_p->GetIndex (IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_INT, a_size_skw);
              if (0 > a_size_index) return false; // The size is not yet read, this must not happen!
              a_nb_values = object_p->GetIntValue(a_size_index);
          }
         }
      }

      reserveCell ((PseudoFileFormatCell_t *) a_cell_format_p, object_p, descr_p, a_nb_values);
      
      //

      int start = 0;
      int end = a_nb_values;
      if (ind >= 0)
      {
          int a_cell_ikw = isMultiArray(card_format_p, descr_p);
          int n_val = 0;
          string a_index_skw = "";
          if (a_cell_ikw > 0)
          {
              MvSizeVector  sizeArrayVector;
              a_descr_p->getDimensionSize(a_cell_ikw, sizeArrayVector);
              int size = (int)sizeArrayVector.size();
              
              if (size > 0)
              {
                  if (!sizeArrayVector[size-1].isRealSize)
                  {
                      string a_index_skw = a_descr_p->getSKeyword(sizeArrayVector[size - 1].size);
                      int a_val = object_p->GetIntValue(a_index_skw.c_str());
                      n_val = a_val;
                  }
                  else
                  {
                      n_val = sizeArrayVector[size -1].size;
                  }
                  start = start + n_val * ind;
                  end = n_val + n_val * ind;
              }
          }
      }
      for(int j=start;a_ok && j<end;j++) {
          if(a_cell==NULL) {
              a_card = readBuffer(false);
              a_ok   = (a_card!=NULL && !mySyntaxInfos_p->isHeader(a_card));
              if(a_ok) {
                  is_free_size_format = isFreeSizeCard(a_card);
                  myReadContext_p->killNLEnd(a_card);

                  if(a_line_length <= myLineLength)
                      myReadContext_p->completeWithBlanks(a_card, a_line_length);
                  else
                      myReadContext_p->completeWithBlanks(a_card, myLineLength);

                  a_cell=const_cast<const char *>(a_card);
              }
          }
          //
          if(a_ok) {
              if (a_nb_values>1) {
                  a_cell=readCell(a_cell,(const PseudoFileFormatCell_t *)a_cell_format_p,
                                  object_p,model_p,descr_p,j,is_free_size_format);
              } else {
                  a_cell=readCell(a_cell,(const PseudoFileFormatCell_t *)a_cell_format_p,
                                  object_p,model_p,descr_p,-1,is_free_size_format);
              }
              a_ok=(a_cell!=NULL);
              if(a_ok) {
                  if(a_line_length <= myLineLength)
                  {
                      if ((a_cell - a_card) >= a_line_length)  a_cell = NULL;
                  }              
                  else
                  {
                      if (a_cell)
                      {
                          size_t len = strlen(a_cell);
                          if (!len)  a_cell = NULL;

                          if (a_cell)
                          {
                              int den = ((j + 1) * a_card_size) % a_line_length;
                              if (!den) a_cell = NULL;
                          }
                      }
                  }
              }
          }
      }
  }
  //
  return a_ok;
}

bool MECIDataReader::readFreeCellList(const PseudoFileFormatCard_t *card_format_p,
                                      IMECPreObject                 *object_p,
                                      void       *model_p,
                                      const PseudoDescriptor_t     *descr_p,
                                      const PseudoFileFormat_t     *format_p,
                                      const PseudoFileFormatCard_t *prev_card_format_p)
{
    bool a_ok=true;
    //
    const ff_card_t *a_card_format_p = (const ff_card_t *)card_format_p;
    //
    bool is_free_format = false;
    int card_size = 0;
    int a_nb_values = getNbBlocks(model_p, a_card_format_p, descr_p, object_p, &is_free_format, card_size, format_p, prev_card_format_p);
    if (a_nb_values<=0) 
    {
        // no line to read
        if (prev_card_format_p)
        {
            const ff_card_t* a_prev_card_format_p = (const ff_card_t*)prev_card_format_p;
            int flag = 0;
            MCDS_get_ff_card_attributes(a_prev_card_format_p, CARD_FLAGS, &flag, END_ARGS);
            if (flag)
            {
                bool no_end_flag = (flag & CARD_FLAG_NO_END) ? true : false;
                if (no_end_flag)
                {
                    readBuffer(false);
                }
            }
        }
        return a_ok ;
    }
    int a_nb_cells      = 0;
    MCDS_get_ff_card_attributes(a_card_format_p,CARD_NB_CELLS,&a_nb_cells,END_ARGS);
    //
    int *a_cell_size_tabf=(int *)mymalloc(a_nb_cells*sizeof(int));
    //
    reserveCellList(card_format_p,object_p,descr_p,a_nb_values);
    //
    int ac_line_length = 0;
    MCDS_get_ff_card_attributes(a_card_format_p, CARD_LENGTH_MAX, &ac_line_length, END_ARGS);
    int a_line_length = 0;
    a_line_length = GetLineLength(ac_line_length, myLineLength);
    //
    int flag = 0;
    const char* a_offset_fmt = NULL;
    bool has_offset = false;
    MCDS_get_ff_card_attributes(a_card_format_p, CARD_FLAGS, &flag, END_ARGS);
    if (flag)
        has_offset = (flag & CARD_FLAG_OFFSET) ? true : false;
    if (has_offset)
        MCDS_get_ff_card_attributes(a_card_format_p, CARD_CELL_LIST_OFFSET_FORMAT, &a_offset_fmt, END_ARGS);
    //
    char *a_card        = readBuffer(false);
    bool is_free_size_format = isFreeSizeCard(a_card); 
    bool  a_continue    = (a_card!=NULL && !mySyntaxInfos_p->isHeader(a_card));
    /*
     In case previous card has NO_END flag, the card_pointer will be pointing to the starting data(which belongs to the previous card)
     Compute the length of the previous card in order advance the card pointer
     In case of free format compute the length by using the number of cells of the previous card
    */
    int a_prev_card_len = 0;
    if (prev_card_format_p && a_continue)
    {
        const ff_card_t* a_prev_card_format_p = (const ff_card_t*)prev_card_format_p;
        int pflag = 0;
        MCDS_get_ff_card_attributes(a_prev_card_format_p, CARD_FLAGS, &pflag, END_ARGS);
        if (pflag)
        {
            bool no_end_flag = (pflag & CARD_FLAG_NO_END) ? true : false;
            if (no_end_flag)
            {
                int a_pnb_cells = 0;
                MCDS_get_ff_card_attributes(a_prev_card_format_p, CARD_NB_CELLS, &a_pnb_cells, END_ARGS);
                if (is_free_size_format)
                {
                    char* a_card_local = a_card;
                    myReadContext_p->killNLEnd(a_card_local);
                    myReadContext_p->completeWithBlanks(a_card_local, a_line_length);
                    int len = 0;
                    // For free format, length is being computed using a_pnb_cells of prev card
                    for (int i = 0; i < a_pnb_cells; ++i)
                    {
                        len = GetCellFreeSize(a_card_local);
                        a_prev_card_len += len;
                        if (i < a_pnb_cells - 1)
                            a_card_local = a_card_local + len;
                    }
                }
                else
                {
                    for (int i = 0; i < a_pnb_cells; ++i)
                    {
                        ff_cell_t* a_cell_p = NULL;
                        MCDS_get_ff_card_tab(a_prev_card_format_p, CARD_CELL, i, (void*)(&a_cell_p));
                        a_prev_card_len += GetCellSize(a_cell_p);
                    }
                }
            }
        }
    }
    if (a_prev_card_len >= a_line_length)
    {
        a_card = readBuffer(false);
        a_prev_card_len = 0;
    }

    const char* a_cell = a_card + a_prev_card_len;
    if (!a_prev_card_len)
    {
        int offset = mySyntaxInfos_p->getInitialOffset(); // Need to get based of solver... should take care of free format as well with long format
        if (offset && is_free_size_format)
        {
            offset = 1;
            for (const char* p = a_cell; *p; ++p) {
                if (*p == mySyntaxInfos_p->getFreeFormatSpecifier()) {
                    offset++;
                    break;
                }
            }
        }
        a_cell = a_cell + offset;
    }

    int   a_ind         = 0, a_line_length_new = a_line_length;
    while(a_continue) 
    {
        //myReadContext_p->killNLEnd(a_card);
        
        myReadContext_p->killNLEnd(a_card);
        int len = (int)strlen(a_card);
        if (len < (card_size + a_prev_card_len))
        {
            if (a_line_length < (card_size + a_prev_card_len))
                myReadContext_p->completeWithBlanks(a_card, a_line_length);
            else
            {
                myReadContext_p->completeWithBlanks(a_card, card_size + a_prev_card_len);
                a_line_length_new = card_size + a_prev_card_len;
            }
        }
        else
        {
            
            myReadContext_p->completeWithBlanks(a_card, a_line_length);
            len = (int)strlen(a_card);
            a_line_length_new = len;
        }

        //
        for(int i=0;a_continue && i<a_nb_cells;++i) 
        {
            if(a_cell==NULL) 
            {
                a_card     = readBuffer(false);
                is_free_size_format = isFreeSizeCard(a_card);
                //	a_continue = (a_card!=NULL && a_card[0]!='/');
                a_continue = (a_card!=NULL && !mySyntaxInfos_p->isHeader(a_card));
                if(a_continue) 
                {
                    myReadContext_p->killNLEnd(a_card);
                    int len = (int)strlen(a_card);
                    if (len < (card_size + a_prev_card_len))
                    {
                        if (a_line_length < (card_size + a_prev_card_len))
                            myReadContext_p->completeWithBlanks(a_card, a_line_length);
                        else
                        {
                            myReadContext_p->completeWithBlanks(a_card, card_size + a_prev_card_len);
                            a_line_length_new = card_size + a_prev_card_len;
                        }
                    }
                    else
                    {
                        
                        myReadContext_p->completeWithBlanks(a_card, a_line_length);
                        len = (int)strlen(a_card);
                        a_line_length_new = len;
                    }
                    a_cell=const_cast<const char *>(a_card);
                }
                else if(a_card!=NULL) 
                {
                    myReadContext_p->unreadBuffer();
                }
                
                // It doesn't include offset in it and hence we needs to take care of advancing the card_pointer by a_offset_length.
                if (has_offset) //has offset will overide any initial offset
                {
                    int a_offset_length = 0;
                    if (!is_free_size_format)
                        a_offset_fmt = GetFormatSize(a_offset_fmt, false, a_offset_length);
                    else
                        a_offset_length = GetCellFreeSize(a_card); // This includes free format 
                    a_cell += a_offset_length;
                }
                else
                {
                    a_cell += mySyntaxInfos_p->getInitialOffset();
                }
            }
            //
            if(a_continue) 
            {
                ff_cell_t *a_cell_format_p=NULL;
                MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,i,(void *)(&a_cell_format_p));
                //
                a_cell=readCell(a_cell,(const PseudoFileFormatCell_t *)a_cell_format_p,
                    object_p,model_p,descr_p,a_ind,is_free_size_format);
                a_continue=a_ok=(a_cell!=NULL);
                if(a_continue) 
                {
                    a_cell+=a_cell_size_tabf[i];
                    if((a_cell-a_card)>=a_line_length_new) a_cell=NULL;
                    /*For free format*/
                    if(is_free_format) 
                    {
                        ff_cell_type_e  cell_type = CELL_UNKNOWN;
                        MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_TYPE,&cell_type,END_ARGS);

                        /*check for next vtype string, if present remove leading spaces*/
                        if(a_cell && !is_blank(a_cell) && i < a_nb_cells-1)
                        {
                            const MvDescriptor_t *a_descr_p       = (const MvDescriptor_t *)descr_p;
                            MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL, i+1,(void *)(&a_cell_format_p));
                            int   a_cell_ikw=END_ARGS, ikeyword = -1;
                            MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_IKEYWORD,&a_cell_ikw,END_ARGS);
                            if (a_cell_ikw > 0) 
                            {
                                string       a_cell_skw   = a_descr_p->getSKeyword(a_cell_ikw);
                                value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);
                                if(a_cell_vtype == VTYPE_STRING)
                                {
                                    a_cell = myReadContext_p->killBlanksBegin(a_cell);
                                }
                            }
                        }          
                        else if(a_cell && is_blank(a_cell)) /*If rest of the line is blank for free format*/
                        {
                            /*if current cell type is value check for last cell type*/
                            if(cell_type == CELL_VALUE) 
                            {
                                MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,a_nb_cells-1,(void *)(&a_cell_format_p));
                                MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_TYPE,&cell_type,END_ARGS);
                                /*If last cell type is value then make current a_cell NULL and start reading values from next line*/
                                if(cell_type == CELL_VALUE) 
                                {
                                    a_cell=NULL;
                                    if(i < a_nb_cells -1)
                                    {
                                        MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,i+1,(void *)(&a_cell_format_p));
                                        MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_TYPE,&cell_type,END_ARGS);
                                        /*advance i to next if next cell type is comment to avoid reading comment for VALUE*/
                                        if(cell_type == CELL_COMMENT) 
                                        {
                                            i++;
                                        }
                                    }
                                }
                            }
                            else if(cell_type == CELL_COMMENT) 
                            {
                                a_cell=NULL;
                                i = a_nb_cells -1;
                            }
                        }
                        else if(a_cell == NULL)/*if a_cell is  NULL and followed cell type is COMMENT*/
                        {
                            if(i < a_nb_cells -1) 
                            {
                                MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,a_nb_cells-1,(void *)(&a_cell_format_p));
                                MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_TYPE,&cell_type,END_ARGS);
                                /*If last cell type is value then make current a_cell NULL and start reading values from next line*/
                                if(cell_type == CELL_VALUE) 
                                {
                                    MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,i+1,(void *)(&a_cell_format_p));
                                    MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_TYPE,&cell_type,END_ARGS);
                                    /*advance i to next if next cell type is comment to avoid reading comment for VALUE*/
                                    if(cell_type == CELL_COMMENT) 
                                    {
                                        i++;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        //
        ++a_ind;
        if(a_ind == a_nb_values)
            a_continue = false;
    }
    if(a_ind > 0 && a_ind < a_nb_values)
        reserveCellList(card_format_p, object_p, descr_p, a_ind-1);

    //
    myfree(a_cell_size_tabf);
    //
    return a_ok;
}

bool MECIDataReader::readCellList(const PseudoFileFormatCard_t* card_format_p,
                                  IMECPreObject* object_p,
                                  void* model_p,
                                  const PseudoDescriptor_t* descr_p,
                                  int                       ind,
                                  const PseudoFileFormatCard_t* prev_card_format_p)
{
    bool a_ok = true;
    //
    const ff_card_t* a_card_format_p = (const ff_card_t*)card_format_p;
    const IDescriptor* a_descr_p = (const MvDescriptor_t*)descr_p;
    //
    char* a_card = readBuffer(false);
    bool is_free_size_format = isFreeSizeCard(a_card);
    bool  a_continue = (a_card != NULL && !mySyntaxInfos_p->isHeader(a_card));
    if (!a_continue)
    {
        myReadContext_p->unreadBuffer();
        return a_ok;
    }
    //
    int a_card_size = 0;
    int a_nb_cells = 0;
    MCDS_get_ff_card_attributes(a_card_format_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);
    //
    for (int j = 0; j < a_nb_cells; ++j) {
        ff_cell_t* a_cell_format_p = NULL;
        MCDS_get_ff_card_tab(a_card_format_p, CARD_CELL, j, (void*)(&a_cell_format_p));
        //
        a_card_size += GetCellSize(a_cell_format_p);
    }
    //
    int a_nb_values_ikw = END_ARGS;
    int a_nb_values = getMultidimensionalArraySize(card_format_p, object_p, descr_p);
    if (a_nb_values == 0) {
        MCDS_get_ff_card_attributes(a_card_format_p, CARD_SIZE, &a_nb_values_ikw, END_ARGS);
        a_nb_values = (a_nb_values_ikw < 0 ? (-a_nb_values_ikw) : 0);
        if (a_nb_values_ikw > 0) {
            string a_nb_values_skw = a_descr_p->getSKeyword(a_nb_values_ikw);
            value_type_e a_cell_vtype = a_descr_p->getValueType(a_nb_values_ikw);
            if (a_cell_vtype == VTYPE_UINT)
            {
                int    a_nb_values_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_UINT, a_nb_values_skw);
                a_nb_values = (int)object_p->GetUIntValue(a_nb_values_ind);
            }
            else
            {
                int    a_nb_values_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_nb_values_skw);
                a_nb_values = object_p->GetIntValue(a_nb_values_ind);

            }
        }
    }
    
    int a_nb_values1 = a_nb_values;
    int start = 0;
    int end = a_nb_values;
    if (ind >= 0)
    {
        int a_cell_ikw = isMultiArray(card_format_p, descr_p);
        int n_val = 0;
        string a_index_skw = "";
        if (a_cell_ikw > 0)
        {
            MvSizeVector  sizeArrayVector;
            a_descr_p->getDimensionSize(a_cell_ikw, sizeArrayVector);
            int size = (int)sizeArrayVector.size();

            if (size > 0)
            {
                if (!sizeArrayVector[size - 1].isRealSize)
                {
                    string a_index_skw = a_descr_p->getSKeyword(sizeArrayVector[size - 1].size);
                    int a_val = object_p->GetIntValue(a_index_skw.c_str());
                    n_val = a_val;
                }
                else
                {
                    n_val = sizeArrayVector[size - 1].size;
                }
                start = start + n_val * ind;
                end = n_val + n_val * ind;
                a_nb_values1 = end;
            }
        }
        else
        {
            if (a_nb_values && ind)
            {
                //start = a_nb_values;
                //end = start + a_nb_values/ind;
                start = a_nb_values * ind;
                end = start + a_nb_values;
            }
            a_nb_values1 = end;
        }
    }
    reserveCellList(card_format_p, object_p, descr_p, a_nb_values1);
    
    int   a_line_length = -1;
    MCDS_get_ff_card_attributes(a_card_format_p, CARD_LENGTH_MAX, &a_line_length, END_ARGS);
    
    /*  if(a_line_length<0) */
    if (a_line_length <= 0)
    {
        a_line_length = myLineLength; 
    }
    const ff_card_t* a_prev_card_format_p = (const ff_card_t*)prev_card_format_p;
    int flag = 0;
    MCDS_get_ff_card_attributes(a_prev_card_format_p, CARD_FLAGS, &flag, END_ARGS);
    int a_prev_card_len = 0;
    const char* a_offset_fmt = NULL;
    bool has_offset = false;
    //computing the offset
    if (flag)
    {
        has_offset = (flag & CARD_FLAG_OFFSET) ? true : false;
        if (has_offset)
            MCDS_get_ff_card_attributes(a_card_format_p, CARD_CELL_LIST_OFFSET_FORMAT, &a_offset_fmt, END_ARGS);
    }
    /*
       In case previous card has NO_END flag, the card_pointer will be pointing to the starting data(which belongs to the previous card)
       Compute the length of the previous card in order advance the card pointer
       In case of free format compute the length by using the number of cells of the previous card
      */
    if (prev_card_format_p && a_continue && flag)
    {
        bool no_end_flag = (flag & CARD_FLAG_NO_END) ? true : false;
        if (no_end_flag)
        {
            int a_pnb_cells = 0;
            MCDS_get_ff_card_attributes(a_prev_card_format_p, CARD_NB_CELLS, &a_pnb_cells, END_ARGS);
            if (is_free_size_format)
            {
                char* a_card_local = a_card;
                myReadContext_p->killNLEnd(a_card_local);
                myReadContext_p->completeWithBlanks(a_card_local, a_line_length);
                int len = 0;
                for (int i = 0; i < a_pnb_cells; ++i)
                {
                    len = GetCellFreeSize(a_card_local);
                    a_prev_card_len += len;
                    if (i < a_pnb_cells - 1)
                        a_card_local = a_card_local + len;
                }
            }
            else
            {
                for (int i = 0; i < a_pnb_cells; ++i)
                {
                    ff_cell_t* a_cell_p = NULL;
                    MCDS_get_ff_card_tab(a_prev_card_format_p, CARD_CELL, i, (void*)(&a_cell_p));
                    a_prev_card_len += GetCellSize(a_cell_p);
                }
            }
        }
    }
    if (a_prev_card_len >= a_line_length)
    {
        a_card = readBuffer(false);
        a_continue = (a_card != NULL && !mySyntaxInfos_p->isHeader(a_card));
        if (!a_continue)
        {
            myReadContext_p->unreadBuffer();
            return a_ok;
        }
        a_prev_card_len = 0;
    }
    myReadContext_p->killNLEnd(a_card);
    myReadContext_p->completeWithBlanks(a_card, a_line_length);
    const char* a_cell = a_card + a_prev_card_len;
    

    for (int k = start; a_ok && k < end; ++k) {
        for (int i = 0; a_ok && i < a_nb_cells; ++i) {
            if (a_cell == NULL) {
                a_card = readBuffer();
                
                a_ok = (a_card != NULL && !mySyntaxInfos_p->isHeader(a_card));
                if (a_ok) {
                    is_free_size_format = isFreeSizeCard(a_card);
                    myReadContext_p->killNLEnd(a_card);
                    myReadContext_p->completeWithBlanks(a_card, a_line_length);
                    a_cell = const_cast<const char*>(a_card);
                    
                   // It doesn't include offset in it and hence we needs to take care of advancing the card_pointer by a_offset_length.
                    if (has_offset)
                    {
                        int a_offset_length = 0;
                        if (!is_free_size_format)
                            a_offset_fmt = GetFormatSize(a_offset_fmt, false, a_offset_length);
                        else
                            a_offset_length = GetCellFreeSize(a_card); // This includes free format 
                        a_cell += a_offset_length;
                    }
                }
            }
            //
            if (a_ok) {
                ff_cell_t* a_cell_format_p = NULL;
                MCDS_get_ff_card_tab(a_card_format_p, CARD_CELL, i, (void*)(&a_cell_format_p));
                //
                a_cell = readCell(a_cell, (const PseudoFileFormatCell_t*)a_cell_format_p,
                    object_p, model_p, descr_p, k, is_free_size_format);
                a_ok = (a_cell != NULL);
                if (a_ok && (a_cell - a_card) >= a_line_length) a_cell = NULL;
            }
        }
    }
    //
    return a_ok;
}

bool MECIDataReader::readObjectList(const PseudoFileFormatCard_t *card_format_p,
                                    IMECPreObject                *object_p,
                                    void                         *model_p,
                                    const PseudoDescriptor_t     *descr_p,
                                    bool                          is_free)
{
  bool a_ok=true;
  //
  const IDescriptor *a_descr_p       = (const MvDescriptor_t *)descr_p;
  const ff_card_t      *a_card_format_p = (const ff_card_t *)card_format_p;
  //
  const char *a_cell_format    = NULL;
  int         a_nb_pos_ids_ikw = END_ARGS;
  int         a_pos_ids_ikw    = END_ARGS;
  int         a_nb_neg_ids_ikw = END_ARGS;
  int         a_neg_ids_ikw    = END_ARGS;
  int         a_nb_otypes      = 0;
  //
  MCDS_get_ff_card_attributes(a_card_format_p,
                              CARD_CELL_FORMAT,&a_cell_format,
                              CARD_POS_SIZE,   &a_nb_pos_ids_ikw,
                              CARD_POS_ARRAY,  &a_pos_ids_ikw,
                              CARD_NEG_SIZE,   &a_nb_neg_ids_ikw,
                              CARD_NEG_ARRAY,  &a_neg_ids_ikw,
                              CARD_NB_OTYPES,  &a_nb_otypes,
                              END_ARGS);
  //
  LocOTypeList_t a_otypes; a_otypes.reserve(a_nb_otypes);
  for(int i=0;i<a_nb_otypes;++i) {
    object_type_e a_otype= HCDI_OBJ_TYPE_NULL;
    MCDS_get_ff_card_tab(a_card_format_p,CARD_OTYPE,i,&a_otype);
    const string &a_otype_str=MV_get_type(a_otype);
    a_otypes.push_back(a_otype_str.c_str());
  }
  LocOTypeList_t::iterator a_it_begin = a_otypes.begin();
  LocOTypeList_t::iterator a_it_end   = a_otypes.end();
  LocOTypeList_t::iterator a_it;
  const char *a_main_otype = (*a_it_begin);
  bool        a_is_treated = false;//model_p->IsTreated(a_main_otype);
  //
  string a_nb_pos_ids_skw = a_descr_p->getSKeyword(a_nb_pos_ids_ikw);
  string a_nb_neg_ids_skw = "";
  if(a_nb_neg_ids_ikw!=END_ARGS) a_nb_neg_ids_skw=a_descr_p->getSKeyword(a_nb_neg_ids_ikw);
  //
  int a_line_length = myLineLength; 
  int a_cell_length = 0;
  a_cell_format = GetFormatSize(a_cell_format, false, a_cell_length);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);
  int a_nb_pos_ids  = 0;
  int a_nb_neg_ids  = 0;
  if(is_free) {
    int a_nb_free_lines = getNbFreeLines();
    if (0==a_nb_free_lines) return a_ok; 
    a_nb_pos_ids=(a_nb_free_lines*a_line_length)/a_cell_length;
    if(a_nb_neg_ids_ikw!=END_ARGS) a_nb_neg_ids=a_nb_pos_ids;
  } else {
     a_nb_pos_ids=object_p->GetIntValue(a_nb_pos_ids_skw.c_str());
     if(a_nb_neg_ids_ikw!=END_ARGS) a_nb_neg_ids=object_p->GetIntValue(a_nb_neg_ids_skw.c_str());
     if((0==a_nb_pos_ids) && (0==a_nb_neg_ids)) return a_ok; 
  }
  //
  const char **a_pos_otypes = (a_is_treated ? (const char **)mymalloc(a_nb_pos_ids*sizeof(const char *)) : NULL);
  MYOBJ_INT         *a_pos_ids    = (MYOBJ_INT *)mymalloc(a_nb_pos_ids*sizeof(MYOBJ_INT));
  int         *a_pos_inds   = (a_is_treated ? (int *)mymalloc(a_nb_pos_ids*sizeof(int)) : NULL);
  const char **a_neg_otypes = (a_is_treated ? (const char **)mymalloc(a_nb_neg_ids*sizeof(const char *)) : NULL);
  MYOBJ_INT         *a_neg_ids = (MYOBJ_INT *)mymalloc(a_nb_neg_ids * sizeof(MYOBJ_INT));
  int         *a_neg_inds   = (a_is_treated ? (int *)mymalloc(a_nb_neg_ids*sizeof(int)) : NULL);
  //
  int a_nb_pos_ids_max = a_nb_pos_ids;
  int a_nb_neg_ids_max = a_nb_neg_ids;
  a_nb_pos_ids=a_nb_neg_ids=0;
  //
  string a_pos_ids_skw = a_descr_p->getSKeyword(a_pos_ids_ikw);
  string a_neg_ids_skw = a_descr_p->getSKeyword(a_neg_ids_ikw);
  bool a_continue=true;
  while(a_continue) {
    char *a_card=readBuffer(false);
    if (!a_card)
        return a_ok;
    bool is_free_size_format = isFreeSizeCard(a_card);
    int a_cell_length_local = a_cell_length;


    a_continue = (a_card!=NULL && !mySyntaxInfos_p->isHeader(a_card));
    // Unread the card, if it is the following header card (CS#19_02_09)
    if(mySyntaxInfos_p->isHeader(a_card)) myReadContext_p->unreadBuffer();
    //
    if(a_continue) {
      myReadContext_p->killNLEnd(a_card);
      myReadContext_p->completeWithBlanks(a_card,a_line_length);
      char *a_cell     = a_card;
      char *a_cell_end = a_cell+a_line_length;
      //
      for(int j=0;a_continue && a_cell<a_cell_end;++j) {
          if(is_free_size_format)
          {
              a_cell_length_local = GetCellFreeSize(a_cell);
          }
          int a_sgn = 0;
          int i = 0;
          int a_id = 0;
          string param_str("");
          bool is_parameter_negated = false;
          bool is_param_cell = isParameterCell(a_cell, a_cell_length_local, param_str, &is_parameter_negated);
          if (is_param_cell)
          {
              if (model_p != NULL) {
                    MECIModelFactory* nc_model_p = (MECIModelFactory*)model_p;
                    a_id = nc_model_p->GetIntParameter(param_str.c_str(), object_p->GetFileIndex());
                    if(0 == a_id)
                    {
                        if(getDoReadAgainFlag())
                        {
                            // if trying to read again, but still not finding: Error message, but continue
                            std::string err_msg = "";
                            char buffer[BufferSize];
                            const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
                            _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
                            if(a_cur_full_name)
                            {
                                sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
                                err_msg.append(buffer);
                                err_msg.append("\n");
                            }
                            sprintf(buffer, getMsg(31), param_str.c_str());
                            err_msg.append(buffer);
                            myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
                        }
                        else
                        {
                            setDoReadAgainFlag(true); // toggle flag for reading again
                            return false;
                        }
                    }
                    if (is_parameter_negated && a_id < 0) {
                        a_id = abs(a_id); //negative and negative becomes positive.
                    }
                    else if (is_parameter_negated && a_id > 0) {
                        a_sgn = -1;
                    }
                    else if (!is_parameter_negated && a_id < 0) {
                        a_id = -1 * a_id;
                        a_sgn = -1;
                    }
              }
              a_ok = true;
          }
          else
          {
              for (i = 0; i < a_cell_length_local; i++)
              {
                  if (a_cell[i] == ' ')
                      continue;
                  else
                  {
                      if (a_cell[i] == '-')
                      {
                          a_sgn = -1;
                          a_cell[i] = ' ';
                      }
                      break;
                  }
              }
              a_id = scanObject(a_cell, a_cell_format, a_cell_length_local);
          }
          if (a_id > 0 && a_sgn == 0)
              a_sgn = 1;
          //
          const char *a_otype = NULL;
          int         a_ind   = -1;
          if(a_is_treated) {
          }
          //
          if(!is_free || !a_is_treated || a_ind>=-1) {
              if(a_sgn<0 && a_id>0) {
                  if(a_nb_neg_ids<a_nb_neg_ids_max) {
                      if(a_is_treated) a_neg_otypes[a_nb_neg_ids]=a_otype;
                      if (is_param_cell)
                      {
                          // This not actually stores whether the parameter is negated or not, 
                          // it rather stores whether, when propagating the parameter value to the
                          // refering object, the sign has to be switched. We come here for
                          // - a positive parameter that is used negated: sign must not be switched or
                          // - a negative parameter that is used non-negated: sign must be switched.
                          // There we are passing "!is_parameter_negated" here:
                          object_p->SetParameterName(param_str.c_str(),
                              a_neg_ids_skw.c_str(), a_nb_neg_ids, !is_parameter_negated);
                      }
                      a_neg_ids[a_nb_neg_ids++]=a_id;
                  }
              }
              else if(a_id > 0) {
                  if(a_nb_pos_ids<a_nb_pos_ids_max) {
                      if(a_is_treated) a_pos_otypes[a_nb_pos_ids]=a_otype;
                      if (is_param_cell)
                      {
                          object_p->SetParameterName(param_str.c_str(),
                              a_pos_ids_skw.c_str(), a_nb_pos_ids, is_parameter_negated);
                      }
                      a_pos_ids[a_nb_pos_ids++]=a_id;
                  }
              }
          }
          //
          a_continue=(a_nb_pos_ids<a_nb_pos_ids_max || a_nb_neg_ids<a_nb_neg_ids_max);
          a_cell+=a_cell_length_local;
      }
    }
  }
  //
  if(is_free) {
    while(a_nb_pos_ids>0 && a_pos_ids[a_nb_pos_ids-1]==0) --a_nb_pos_ids;
    object_p->AddIntValue(a_nb_pos_ids_skw.c_str(),a_nb_pos_ids);    
    //
    if(a_nb_neg_ids_ikw!=END_ARGS) {
      while(a_nb_neg_ids>0 && a_neg_ids[a_nb_neg_ids-1]==0) --a_nb_neg_ids;
      object_p->AddIntValue(a_nb_neg_ids_skw.c_str(),a_nb_neg_ids);
    }
  }
  //
  object_p->AddObjectArray(a_pos_ids_skw.c_str(),a_nb_pos_ids);
  if(a_is_treated) {
    object_p->AddObjectValues(a_pos_ids_skw.c_str(),0,a_nb_pos_ids,a_pos_otypes,a_pos_ids,a_pos_inds);
  } else {
    object_p->AddObjectValues(a_pos_ids_skw.c_str(),0,a_main_otype,a_nb_pos_ids,a_pos_ids);
  }
  //
  if(a_nb_neg_ids_ikw!=END_ARGS) {
    object_p->AddObjectArray(a_neg_ids_skw.c_str(),a_nb_neg_ids);
    if(a_is_treated) {
      object_p->AddObjectValues(a_neg_ids_skw.c_str(),0,a_nb_neg_ids,a_neg_otypes,a_neg_ids,a_neg_inds);
    } else {
      object_p->AddObjectValues(a_neg_ids_skw.c_str(),0,a_main_otype,a_nb_neg_ids,a_neg_ids);
    }
  }
  //
  myfree(a_pos_otypes); myfree(a_neg_otypes);
  myfree(a_pos_ids);    myfree(a_neg_ids);
  myfree(a_pos_inds);   myfree(a_neg_inds);
  //
  return a_ok;
}

bool MECIDataReader::readCardList(const PseudoFileFormatCard_t* card_format_p,
                                    IMECPreObject*              object_p,
                                    void*                       model_p,
                                    const PseudoDescriptor_t*   descr_p,
                                    bool*                       do_continue_p,
                                    bool                        is_free,
                                    int*                        cur_s_card_indx_p)
{
    bool a_ok = true;
    //
    const ff_card_t* a_card_format_p = (const ff_card_t*)card_format_p;
    //
    int  a_loc_nb_cards = 0;
    int  a_size_ikw = END_ARGS;
    int    a_size_ind = -1;
    int nb_token = 0;
    char** token_list = NULL;
    MCDS_get_ff_card_attributes(a_card_format_p, CARD_SIZE, &a_size_ikw, CARD_NB_CARDS, &a_loc_nb_cards, END_ARGS);
    MCDS_get_ff_card_attributes(a_card_format_p, CARD_FREE_CARD_LIST_TOKEN_NB, &nb_token, END_ARGS);

    if (nb_token)
    {
        token_list = (char**)malloc(nb_token * sizeof(char*));
        for (int i = 0; i < nb_token; i++)
        {
            MCDS_get_ff_card_tab(a_card_format_p, CARD_FREE_CARD_LIST_TOKEN_STR, i, &(token_list[i]));
        }
    }
    //
    const IDescriptor* a_descr_p = (const MvDescriptor_t*)descr_p;
    int                   a_size = 0;
    bool is_preallocation_candidate = true;
    bool is_free_card_list = true;
    if (a_size_ikw > 0) {
        string a_size_skw = a_descr_p->getSKeyword(a_size_ikw);
        value_type_e a_cell_vtype = a_descr_p->getValueType(a_size_ikw);
        if (a_cell_vtype == VTYPE_UINT)
        {
            a_size_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_UINT, a_size_skw);
            if (a_size_ind >= 0)
            {
                a_size = (int)object_p->GetUIntValue(a_size_ind);
                is_free_card_list = false;
            }
        }
        else
        {
            a_size_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_size_skw);
            if (a_size_ind >= 0)
            {
                a_size = object_p->GetIntValue(a_size_ind);
                is_free_card_list = false;
            }
        }
        //
        if ((a_size_ind < 0) && is_free == true)
        {
            int nb_cards = 0;
            for (int i = 0; i < a_loc_nb_cards; ++i) {
                ff_card_t* a_loc_card_format_p;
                MCDS_get_ff_card_tab(a_card_format_p, CARD_CARD, i, (void*)(&a_loc_card_format_p));
                if (a_loc_card_format_p->type == CARD_SINGLE)
                {
                    nb_cards++;
                    int a_nb_cells = 0;
                    ff_cell_type_e  a_cell_type = CELL_UNKNOWN;
                    MCDS_get_ff_card_attributes(a_loc_card_format_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);
                    for (int j = 0; a_ok && j < a_nb_cells; j++) {
                        ff_cell_t* a_cell_format_p = NULL;
                        MCDS_get_ff_card_tab(a_loc_card_format_p, CARD_CELL, j, (void*)(&a_cell_format_p));
                        //
                        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_TYPE, &a_cell_type, END_ARGS);
                        if (a_cell_type == CELL_PAIR)
                        {
                            is_preallocation_candidate = false;
                            break;
                        }
                    }
                }
                // The below part of code has to be enabled if an "if else" condition has to be inside the FREE_CARD_LIST or CARD_LIST.
                // Only a static variable can be checked inside the expression part of if condition.
                else if (a_loc_card_format_p->type == CARD_IF)
                {
                    bool a_lok = true;
                    //
                    const ff_card_t* a_card_format_p1 = (const ff_card_t*)a_loc_card_format_p;
                    //
                    int  a_nb_ccls = 0;
                    bool a_checked = false;

                    MCDS_get_ff_card_attributes(a_card_format_p1, CARD_NB_COND_CARD_LISTS, &a_nb_ccls, END_ARGS);
                    //
                    int j = 0;
                    for (j = 0; a_lok && (!a_checked) && j < a_nb_ccls; ++j) {
                        ff_condcardlist_t* a_ccl_p = NULL;
                        expression_t* a_expr_p = NULL;
                        MCDS_get_ff_card_tab(a_card_format_p1, CARD_COND_CARD_LIST, j, &a_ccl_p);
                        MCDS_get_ff_condcardlist_expression(a_ccl_p, &a_expr_p);
                        //
                        a_checked = (a_expr_p == NULL);
                        if (!a_checked) {
                            MvExpression_t a_expr(a_expr_p, false);
                            a_checked = object_p->EvaluateExpression((PseudoExpression_t*)(&a_expr), a_descr_p); 
                        }
                        //
                        if (a_checked) {
                            int a_nb_cards = 0, k = 0;
                            MCDS_get_ff_condcardlist_nb_cards(a_ccl_p, &a_nb_cards);
                            for (k = 0; a_lok && k < a_nb_cards; ++k) {
                                ff_card_t* a_sub_card_format_p = NULL;
                                ff_card_type_e a_sub_card_type = CARD_UNKNOWN;
                                MCDS_get_ff_condcardlist_card(a_ccl_p, k, &a_sub_card_format_p);
                                MCDS_get_ff_card_attributes(a_sub_card_format_p, CARD_TYPE, &a_sub_card_type, END_ARGS);
                                if (a_sub_card_type == CARD_SINGLE)
                                {
                                    nb_cards++;
                                    int a_nb_cells = 0;
                                    ff_cell_type_e  a_cell_type = CELL_UNKNOWN;
                                    MCDS_get_ff_card_attributes(a_loc_card_format_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);
                                    for (int j = 0; a_ok && j < a_nb_cells; j++) {
                                        ff_cell_t* a_cell_format_p = NULL;
                                        MCDS_get_ff_card_tab(a_loc_card_format_p, CARD_CELL, j, (void*)(&a_cell_format_p));
                                        //
                                        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_TYPE, &a_cell_type, END_ARGS);
                                        if (a_cell_type == CELL_PAIR)
                                        {
                                            is_preallocation_candidate = false;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            int a_nb_free_lines = 0;
            int t_line_Count = getCurKeyTotalLineCount();
            if ((nullptr != cur_s_card_indx_p && *cur_s_card_indx_p < 0) ||
                t_line_Count <= 0 || nb_token || !is_preallocation_candidate)
            {
                a_nb_free_lines = getNbFreeLines(nb_token, token_list);
                is_preallocation_candidate = false;
            }
            else if (nullptr != cur_s_card_indx_p && *cur_s_card_indx_p > 0)
            {
                a_nb_free_lines = t_line_Count - *cur_s_card_indx_p;
            }
            else
            {
                a_nb_free_lines = t_line_Count;
            }

            if (nb_cards > 0)
            {
                if (mySyntaxInfos_p->IsFormatSupportedForContinueNextLine())
                {
                    nb_cards = nb_cards / 2;
                }
                a_size = a_nb_free_lines / nb_cards;
                if ((a_nb_free_lines % nb_cards) != 0)
                    a_size++;
            }
            object_p->AddIntValue(a_size_skw.c_str(), a_size);
        }

        if (0 == a_size) //in case of parameter reference for size attribute, we may get into this scenario.
        {
            bool isParameterCell = object_p->IsParameter(a_size_skw.c_str());
        }
    }
    else {
        a_size = (-a_size_ikw);
    }
    //
    for (int i = 0; i < a_loc_nb_cards; ++i) {
        ff_card_t* a_loc_card_format_p;
        MCDS_get_ff_card_tab(a_card_format_p, CARD_CARD, i, (void*)(&a_loc_card_format_p));
        reserveCellList((const PseudoFileFormatCard_t*)a_loc_card_format_p, object_p, descr_p, a_size);
    }
    //
    bool a_do_continue = true; 
    int j = 0;
    if (is_preallocation_candidate && is_free_card_list)
        setIsFreeArrayCard(true);//only FREE_CARD_LIST not CARD_LIST

    for (j = 0; a_ok && a_do_continue && j < a_size; ++j) for (int k = 0; a_ok && a_do_continue && k < a_loc_nb_cards; ++k) {
        ff_card_t* a_loc_card_format_p;
        MCDS_get_ff_card_tab(a_card_format_p, CARD_CARD, k, (void*)(&a_loc_card_format_p));
        a_ok = readNextCard((const PseudoFileFormatCard_t*)a_loc_card_format_p, object_p, model_p, descr_p, j, &a_do_continue);

        if (!a_do_continue && (k != a_loc_nb_cards-1 || (j < a_size && is_free_card_list))  )
        {
            if (k != a_loc_nb_cards-1) // if card_if ???
            {
                string sObj;
                MYOBJ_INT IdObj = 0;
                if (object_p != NULL)
                {
                    sObj = object_p->GetKernelFullType();
                    IdObj = object_p->GetId();
                }
                std::string err_msg = "";
                char buffer[BufferSize];
                const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
                _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
                if (a_cur_full_name)
                {
                    sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
                    err_msg.append(buffer);
                    err_msg.append("\n");
                }
                sprintf(buffer, getMsg(28), sObj.c_str(), IdObj);
                err_msg.append(buffer);
                err_msg.append(getMsg(29));
                //
                myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
            }
            myReadContext_p->unreadBuffer();
            goto EXIT_LOOPS;
        }

    }
    setIsFreeArrayCard(false);

EXIT_LOOPS:
    if (a_ok && !a_do_continue || is_preallocation_candidate)
    {
        a_size_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_descr_p->getSKeyword(a_size_ikw));
        if (a_size_ind >= 0 && j != a_size)
        {
            MvIKeywordSet_t a_secondary_ikws;
            a_descr_p->getSizeConnectedIKeywords(a_size_ikw, &a_secondary_ikws);

            MvIKeywordSet_t::iterator a_it_begin = a_secondary_ikws.begin();
            MvIKeywordSet_t::iterator a_it_end = a_secondary_ikws.end();
            MvIKeywordSet_t::iterator a_it;

            for (a_it = a_it_begin; a_it != a_it_end; ++a_it) {

                int ikeyword = *a_it;
                string a_secondary_skw = a_descr_p->getSKeyword(ikeyword);
                IMECPreObject::MyValueType_e  a_secondary_vtype = object_p->GetValueType(IMECPreObject::ATY_ARRAY, a_secondary_skw.c_str());
                int            a_secondary_index = object_p->GetIndex(IMECPreObject::ATY_ARRAY, a_secondary_vtype, a_secondary_skw);
                if (a_secondary_index >= 0)
                    object_p->resizeArray(a_secondary_vtype, a_secondary_index, j);
            }
            object_p->SetIntValue(a_size_ind, j);
        }
    }


    //
    if (do_continue_p) *do_continue_p = a_do_continue;
    if (nb_token)
    {
        free(token_list);
    }
    return a_ok;
}

bool MECIDataReader::readIfCard(const PseudoFileFormatCard_t *card_format_p,
                                IMECPreObject                 *object_p,
                                void             *model_p, 
                                const PseudoDescriptor_t     *descr_p,
                                int                           ind, 
                                bool                         *do_continue_p, 
                                const PseudoFileFormat_t     *format_p,
                                int                          *cur_s_card_indx_p)
{
  bool a_ok=true;
  //
  const ff_card_t *a_card_format_p = (const ff_card_t *)card_format_p;
  //
  int  a_nb_ccls = 0;
  bool a_checked = false;
  MCDS_get_ff_card_attributes(a_card_format_p,CARD_NB_COND_CARD_LISTS,&a_nb_ccls,END_ARGS);
  //
  bool a_do_continue=true; 
  for(int j=0;a_ok && a_do_continue && (!a_checked) && j<a_nb_ccls;++j) {
    ff_condcardlist_t *a_ccl_p  = NULL;
    expression_t      *a_expr_p = NULL;
    MCDS_get_ff_card_tab(a_card_format_p,CARD_COND_CARD_LIST,j,&a_ccl_p);
    MCDS_get_ff_condcardlist_expression(a_ccl_p,&a_expr_p);
    //
    a_checked=(a_expr_p==NULL);
    if(!a_checked) {
      MvExpression_t a_expr(a_expr_p,false);
      a_checked=object_p->EvaluateExpression((PseudoExpression_t *)(&a_expr),descr_p,ind);
    }
    //
    if(a_checked) {
      int a_nb_cards=0;
      MCDS_get_ff_condcardlist_nb_cards(a_ccl_p,&a_nb_cards);
      for(int k=0;a_ok && a_do_continue && k<a_nb_cards;++k) {
        ff_card_t *a_sub_card_format_p=NULL;
        ff_card_t* a_next_sub_card_p = NULL;
        MCDS_get_ff_condcardlist_card(a_ccl_p,k,&a_sub_card_format_p);
        //
        int flag = 0;
        MCDS_get_ff_card_attributes(a_sub_card_format_p, CARD_FLAGS, &flag, END_ARGS);
        if (flag)
        {
            bool no_end_flag = (flag & CARD_FLAG_NO_END) ? true : false;
            if (no_end_flag && k < (a_nb_cards - 1))
            {
                k++;
                MCDS_get_ff_condcardlist_card(a_ccl_p, k, &a_next_sub_card_p);
            }
        }
        a_ok=readNextCard((const PseudoFileFormatCard_t *)a_sub_card_format_p,object_p,model_p,descr_p,ind,&a_do_continue, format_p, a_next_sub_card_p, cur_s_card_indx_p);
      }
    }
  }    
  //
  if (do_continue_p) *do_continue_p = a_do_continue;
  return a_ok;
}


const char * MECIDataReader::readIfCell(const char                   *cell,
                                        const PseudoFileFormatCell_t *cell_format_p,
                                        IMECPreObject                 *object_p,
                                        void       *model_p, 
                                        const PseudoDescriptor_t     *descr_p,
                                        int                           ind,
                                        bool                          *a_ok_p)
{
    bool    a_ok=true;
    int     a_nb_ccls = 0;
    bool    a_checked = false;
    //
    const ff_cell_t *a_cell_format_p = (const ff_cell_t *)cell_format_p;
    //
    MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_NB_COND_CELL,&a_nb_ccls,END_ARGS);
    //
    for(int j=0;a_ok && (!a_checked) && j<a_nb_ccls;++j) {
        ff_condcell_t       *a_ccl_p  = NULL;
        expression_t        *a_expr_p = NULL;

        MCDS_get_ff_cell_tab(a_cell_format_p,CELL_COND_CELL,j,&a_ccl_p);
        MCDS_get_ff_condcell_expression(a_ccl_p,&a_expr_p);
        //
        a_checked=(a_expr_p==NULL);
        if(!a_checked) {
            MvExpression_t a_expr(a_expr_p,false);
            a_checked=object_p->EvaluateExpression((PseudoExpression_t *)(&a_expr),descr_p, ind);
        }
        //
        if(a_checked) {
	        ff_cell_t    *a_sub_cell_p=NULL;
            MCDS_get_ff_condcell_cell(a_ccl_p, &a_sub_cell_p);

            cell = readCell(cell,
                (const PseudoFileFormatCell_t *)a_sub_cell_p,
                object_p,
                model_p,
                descr_p,
                ind);
        }
    }    
    //
    if(a_ok_p)
        *a_ok_p = a_ok;
    return cell;
}

const char* MECIDataReader::readPairCell(const char* cell,
                                         const PseudoFileFormatCell_t* cell_format_p,
                                         IMECPreObject* object_p,
                                         void* model_p,
                                         const PseudoDescriptor_t* descr_p,
                                         int  ind,
                                         bool* a_ok_p,
                                         bool is_free_size_format)
{
    bool    a_ok = true;
    const char* a_cur_cell = cell;
    int a_ind = ind * 2;
    for (int i = 0; i < 2 && a_ok; i++)
    {
        a_cur_cell = readCell_VALUE(a_cur_cell, cell_format_p, object_p, model_p, descr_p, a_ind + i, is_free_size_format);
        a_ok = (a_cur_cell != NULL);
    }
    if (a_ok_p)
        * a_ok_p = a_ok;
    return a_cur_cell;
}
const char* MECIDataReader::readCellArrayList(const char* cell,                     const PseudoFileFormatCell_t* cell_format_p,
                              IMECPreObject* object_p,              void* model_p,
                              const PseudoDescriptor_t* descr_p,    int   ind,
                              bool     is_free_size_format,         bool* a_ok_p)
{
    bool    a_ok = true;
    const MvDescriptor_t* a_descr_p = (const MvDescriptor_t*)descr_p;
    const ff_cell_t* a_cell_format_p = (const ff_cell_t*)cell_format_p;

    const char* a_cell_fmt = NULL;
    int         a_cell_ikw = END_ARGS;
    int         a_index = -1;
    int i = 0, a_size = 0;
    MCDS_get_ff_cell_attributes(a_cell_format_p,
                                CELL_FORMAT, &a_cell_fmt,
                                CELL_IKEYWORD, &a_cell_ikw,
                                CELL_LIST_INDEX, &a_index,
                                END_ARGS);
    //
    value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);
    attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
    if (a_atype == ATYPE_VALUE || a_atype == ATYPE_SIZE)
        return cell;
    
    bool a_multi = a_descr_p->isMultiDimensionalArray(a_cell_ikw);
    int a_nb_values = 0, start = 0, end = 0;
    if (ind >= 0 && a_multi)
    {
        MvSizeVector sizeArrayVector;
        a_descr_p->getDimensionSize(a_cell_ikw, sizeArrayVector);
        size_t array_dimension = sizeArrayVector.size();
        if (array_dimension == 0 || array_dimension > 2)
            return cell;
        vector<int> vec_size;
        for (int i = 0; i < array_dimension; i++)
        {
            int size = 0;
            bool is_real_size = sizeArrayVector[i].isRealSize;
            if (is_real_size)
                size = sizeArrayVector[i].size;
            else
            {
                int size_ikey = sizeArrayVector[i].size;
                if (size_ikey)
                {
                    string size_skey = a_descr_p->getSKeyword(size_ikey);
                    int a_size_index = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, size_skey);
                    if (a_size_index >= 0)
                        size = object_p->GetIntValue(size_skey.c_str());
                }
            }
            vec_size.push_back(size);
        }

        a_nb_values = vec_size[0] * vec_size[1];

        if (a_index < 0)
        {
            start = ind * vec_size[1];
            end = ind * vec_size[1] + vec_size[1];
        }
        else if (a_index < a_nb_values)
        {
            start = ind * vec_size[1] + a_index;
            end = start + 1;
        }
    }
	else if (ind >= 0)
	{
		static vector<int> vec_size;

        int size = 0;

		if (vec_size.size() && ind == 0)
			vec_size.clear();
        int a_size_ikw = a_descr_p->getSizeIKeyword(a_cell_ikw);
		if (a_size_ikw)
		{
			// Need to check how to get correct size for hybrid variable size(no 2);
			string size_skey = a_descr_p->getSKeyword(a_size_ikw);
			int a_size_index = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, size_skey);
			if (a_size_index >= 0)
				size = object_p->GetIntValue(size_skey.c_str());

            vec_size.push_back(size);
		}
        int vsize = (int)vec_size.size();
        if (vsize < ind)
        {
            return "";
        }

        for (int j = 0; j < vsize; j++)
        {
            a_nb_values += vec_size[j];
        }

		if (a_index < 0)
		{
            //ind 0  11
            //ind 1  11
			start = a_nb_values - vec_size[ind];

			end = a_nb_values;
		}
		else if (a_index < a_nb_values)
		{
			start = a_nb_values - vec_size[ind] + a_index;
			end = start + 1;
		}
	}
    else
    {
        if (a_atype == ATYPE_STATIC_ARRAY)
            a_nb_values = a_descr_p->getSize(a_cell_ikw);
        else
        {
            int a_size_ikw = a_descr_p->getSizeIKeyword(a_cell_ikw);
            string a_size_skw = a_descr_p->getSKeyword(a_size_ikw);
            a_nb_values = object_p->GetIntValue(a_size_skw.c_str());
        }
        start = 0;
        if (a_index < 0)
        {
            start = 0;
            a_size = a_nb_values;
        }
        else if (a_index < a_nb_values)
        {
            start = a_index;
            a_size = a_index + 1;
        }
        end = a_size;
    }
    string a_cell_skw = a_descr_p->getSKeyword(a_cell_ikw);
    switch (a_cell_vtype) {
    case VTYPE_BOOL:
    {
        int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_cell_skw);
        if (a_cell_ind >= 0)
        {
            int a_arr_size = object_p->GetNbValues(IMECPreObject::VTY_BOOL, a_cell_ind);
            if (a_arr_size < a_nb_values)
                object_p->resizeArray(IMECPreObject::VTY_BOOL, a_cell_ind, a_nb_values);
        }
        else
            object_p->AddBoolArray(a_cell_skw.c_str(), a_nb_values);
    }
    break;
    case VTYPE_INT:
    {
        int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_cell_skw);
        if (a_cell_ind >= 0)
        {
            int a_arr_size = object_p->GetNbValues(IMECPreObject::VTY_INT, a_cell_ind);
            if (a_arr_size < a_nb_values)
                object_p->resizeArray(IMECPreObject::VTY_INT, a_cell_ind, a_nb_values);
        }
        else
            object_p->AddIntArray(a_cell_skw.c_str(), a_nb_values);
    }
    break;
    case VTYPE_UINT:
    {
        int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_cell_skw);
        if (a_cell_ind >= 0)
        {
            int a_arr_size = object_p->GetNbValues(IMECPreObject::VTY_UINT, a_cell_ind);
            if (a_arr_size < a_nb_values)
                object_p->resizeArray(IMECPreObject::VTY_UINT, a_cell_ind, a_nb_values);
        }
        else
            object_p->AddUIntArray(a_cell_skw.c_str(), a_nb_values);
    }
    break;
    case VTYPE_FLOAT:
    {
        int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_cell_skw);
        if (a_cell_ind >= 0)
        {
            int a_arr_size = object_p->GetNbValues(IMECPreObject::VTY_FLOAT, a_cell_ind);
            if (a_arr_size < a_nb_values)
                object_p->resizeArray(IMECPreObject::VTY_FLOAT, a_cell_ind, a_nb_values);
        }
        else
            object_p->AddFloatArray(a_cell_skw.c_str(), a_nb_values);
    }
    break;
    case VTYPE_STRING:
    {
        int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_cell_skw);
        if (a_cell_ind >= 0)
        {
            int a_arr_size = object_p->GetNbValues(IMECPreObject::VTY_STRING, a_cell_ind);
            if (a_arr_size < a_nb_values)
                object_p->resizeArray(IMECPreObject::VTY_STRING, a_cell_ind, a_nb_values);
        }
        else
            object_p->AddStringArray(a_cell_skw.c_str(), a_nb_values);
    }
    break;
    case VTYPE_OBJECT:
    {
        int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_cell_skw);
        if (a_cell_ind >= 0)
        {
            int a_arr_size = object_p->GetNbValues(IMECPreObject::VTY_OBJECT, a_cell_ind);
            if (a_arr_size < a_nb_values)
                object_p->resizeArray(IMECPreObject::VTY_OBJECT, a_cell_ind, a_nb_values);
        }
        else
            object_p->AddObjectArray(a_cell_skw.c_str(), a_nb_values);
    }
    break;
    default:
        break;
    }

	char* a_loc_card = nullptr;
	for (i = start; i < end; i++)
	{
		if (!cell)
			continue;
		cell = readCell_VALUE(cell, cell_format_p, object_p, model_p, descr_p, i, is_free_size_format);
		if ((strcmp(cell, "") == 0) && (i != (end - 1)))
		{
			a_loc_card = readBuffer();
			a_ok = (a_loc_card != NULL && !mySyntaxInfos_p->isHeader(a_loc_card));
			if (a_ok) {
				is_free_size_format = isFreeSizeCard(a_loc_card);
				myReadContext_p->killNLEnd(a_loc_card);
				myReadContext_p->completeWithBlanks(a_loc_card, myLineLength);
				cell = a_loc_card;
			}
		}
	}


    if(a_ok_p)
        * a_ok_p = a_ok;
    return cell;
}

bool MECIDataReader::readSubobjects(const PseudoFileFormatCard_t *card_format_p,
                                    IMECPreObject                 *object_p,
                                    void             *model_p,
                                    const PseudoDescriptor_t     *descr_p)
{
  bool                  a_ok            = true;
  const ff_card_t      *a_card_format_p = (const ff_card_t *)card_format_p;
  const IDescriptor *a_descr_p       = (const IDescriptor *)descr_p;
  
  // Get info about subobject(s)
  int    a_subobj_ikw    = END_ARGS;
  const char *a_kfulltype = NULL;
  MCDS_get_ff_card_attributes(a_card_format_p,CARD_OBJECTS_IKW,&a_subobj_ikw,CARD_KFULLTYPE,&a_kfulltype,END_ARGS);
  if (END_ARGS==a_subobj_ikw)
  {
      readBuffer(); // If we come here there is a cfg file problem. Reading a card may avoid infinite loops
      return false;
  }
  string       a_subobj_skw   = a_descr_p->getSKeyword(a_subobj_ikw);
  value_type_e a_subobj_vtype = a_descr_p->getValueType(a_subobj_ikw);
  if (VTYPE_OBJECT != a_subobj_vtype)
  {
      readBuffer(); // If we come here there is a cfg file problem. Reading a card may avoid infinite loops
      return false;
  }
  object_type_e a_subobj_otype = a_descr_p->getObjectType(a_subobj_ikw);
  const string &a_subobj_otype_str = MV_get_type(a_subobj_otype);
  const IDescriptor *a_subobj_descr_p =  HCDI_GetDescriptorHandle((char *)a_kfulltype);
  if (NULL == a_subobj_descr_p)
  {
      readBuffer(); // If we come here there is a cfg file problem. Reading a card may avoid infinite loops
      return false;
  }
  const PseudoFileFormat_t *a_subobj_format_p =
      (const PseudoFileFormat_t *) a_subobj_descr_p->getFileFormatPtr((MvFileFormat_e) myFileFormatId);
  if (a_subobj_format_p == NULL) 
  {
      a_subobj_format_p =
          (const PseudoFileFormat_t *) a_subobj_descr_p->getLowerFileFormatPtr((MvFileFormat_e) myFileFormatId);
      if (a_subobj_format_p == NULL) 
      {
          readBuffer(); // If we come here there is a cfg file problem. Reading a card may avoid infinite loops
          return false;
  }
  }
  attribute_type_e a_atype = a_descr_p->getAttributeType(a_subobj_ikw);

  // Create a reader for the subobject(s)
  mySubobjDataReader = newSubobjectReader();
  if (NULL == mySubobjDataReader)
  {
      readBuffer(); // If we come here there is a cfg file problem. Reading a card may avoid infinite loops
      return false;
  }
  mySubobjDataReader->setDoReadAgainFlag(myDoReadAgainFlag);

  // Read 1 or more objects, corresponding to the attribute type of a_objects_ikw
  switch (a_atype)
  {
  case ATYPE_VALUE: // Read 1 object
  {
      MYOBJ_INT a_id = readSubobject (mySubobjDataReader, a_subobj_format_p, model_p, a_subobj_descr_p, a_kfulltype, object_p);

      if (0 < a_id) { // subobject could be read
          object_p->AddObjectValue(a_subobj_skw.c_str(),a_subobj_otype_str.c_str(),a_id);
          // (no need to check whether subobjects are already posttreated, we suppose they aren't)
      }

      break;
  }
  case ATYPE_STATIC_ARRAY: // Read a fixed number of objects
  {
      int a_nb_values = a_descr_p->getSize(a_subobj_ikw);
      //object_p->AddObjectArray(a_subobj_skw.c_str(), a_nb_values);
      for (int i = 0; i < a_nb_values; i++) {
          MYOBJ_INT a_id = readSubobject (mySubobjDataReader, a_subobj_format_p, model_p, a_subobj_descr_p, a_kfulltype, object_p);
          if (0 < a_id) { // subobject could be read
              object_p->AddObjectValue(a_subobj_skw.c_str(),i,a_subobj_otype_str.c_str(),a_id);
              // (no need to check whether subobjects are already posttreated, we suppose they aren't)
          } else {
              a_ok = false;
          }
      }
      break;
  }
  case ATYPE_DYNAMIC_ARRAY: /* Read a fixed number (if the array size has already been read) or a free number (if the
                             * array size has not yet been read) of objects in a dynamic array */
  {
      myIsArrayOfSubobjects = true; // Flag set to true when reading array of subobjects
      string a_size_skw = a_descr_p->getSizeSKeyword(a_subobj_ikw);
      value_type_e a_cell_vtype = a_descr_p->getValueType(a_subobj_ikw);
      int a_size_index = -1;
      if(a_cell_vtype == VTYPE_UINT)
          a_size_index = object_p->GetIndex (IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_UINT, a_size_skw);
      else
          a_size_index = object_p->GetIndex (IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_INT, a_size_skw);

      if (0 <= a_size_index) {
          // Array size has already been read: Read a fixed number of objects
          int a_nb_subobj =0;
          if(a_cell_vtype == VTYPE_UINT)
              a_nb_subobj = object_p->GetUIntValue(a_size_index);
          else
              a_nb_subobj = object_p->GetIntValue(a_size_index);
          if (0 < a_nb_subobj) { 
              object_p->AddObjectArray(a_subobj_skw.c_str(), a_nb_subobj);
              for (int i = 0; i < a_nb_subobj; i++) {
                  MYOBJ_INT a_id = readSubobject (mySubobjDataReader, a_subobj_format_p, model_p, a_subobj_descr_p, a_kfulltype, object_p);
                  if (0 < a_id) { // subobject could be read
                      object_p->AddObjectValue(a_subobj_skw.c_str(),i,a_subobj_otype_str.c_str(),a_id);
                      // (no need to check whether subobjects are already posttreated, we suppose they aren't)
                  } else {
                      a_ok = false;
                      break;
                  }
              }
          } 

      } else {
          //To respect blank and zero, need to check whether we have valid card after subobject
          const fileformat_t *a_format_p = a_descr_p->getFileFormatPtr((MvFileFormat_e)myFileFormatId);
          if (a_format_p == NULL) 
          {
              a_format_p = a_descr_p->getLowerFileFormatPtr((MvFileFormat_e) myFileFormatId);
              if (a_format_p == NULL) 
                  break;
              int a_nb_cards=0;
              MCDS_get_fileformat_nb_cards(a_format_p,&a_nb_cards);
              int i = 0;
              bool found = false;
              for(i=0; i<a_nb_cards; ++i) {
                  ff_card_t *a_lcard_format_p;
                  MCDS_get_fileformat_card(a_format_p,i,&a_lcard_format_p);
                  ff_card_type_e a_card_type=CARD_UNKNOWN;
                  MCDS_get_ff_card_attributes(a_lcard_format_p,CARD_TYPE,&a_card_type,END_ARGS);
                  if (a_lcard_format_p == card_format_p)
                  {
                      found = true;
                      continue;
                  }
                  if (found == true && a_card_type != CARD_COMMENT && a_card_type != CARD_BLANK && a_card_type != CARD_ASSIGN)
                  {
                      break;
                  }
              }
              if (i < a_nb_cards)
              {
                  break;
              }
          }

          // Array size has not yet been read: Read as long as there are objects to read
          deque<int> a_id_list;
          bool subobj_is_found = mySubobjDataReader->checkCard (a_subobj_format_p);
          while (subobj_is_found) {
              MYOBJ_INT a_id = readSubobject (mySubobjDataReader, a_subobj_format_p, model_p, a_subobj_descr_p, a_kfulltype, object_p);
              if (0 < a_id) { // subobject could be read
                  a_id_list.push_back (a_id);
                  subobj_is_found = mySubobjDataReader->checkCard (a_subobj_format_p);
              } else {
                  a_ok = false;
                  break;
              }
          }
          // All subobjects are read, put them in object_p
          int a_nb_subobj = (int) a_id_list.size();
          object_p->AddIntValue(a_size_skw.c_str(), a_nb_subobj);
          if (0 < a_nb_subobj) {
              object_p->AddObjectArray(a_subobj_skw.c_str(), a_nb_subobj);
              for (int i = 0; i < a_nb_subobj; i++) {
                  MYOBJ_INT a_id = a_id_list[i];
                  object_p->AddObjectValue(a_subobj_skw.c_str(),i,a_subobj_otype_str.c_str(),a_id);
                  // (no need to check whether subobjects are already posttreated, we suppose they aren't)
              }
          }
      }
      myIsArrayOfSubobjects = false; // Flag Set to false after array of subobjects reading gets finished.
      break;
  }
  default: 
      a_ok = false;
  }
  //
  delete mySubobjDataReader;
  mySubobjDataReader = nullptr;
  return a_ok;
}

MYOBJ_INT MECIDataReader::readSubobject(MECIDataReader               *subobj_reader_p,
                                  const PseudoFileFormat_t     *subobj_format_p,
                                  void             *model_p,
                                  const PseudoDescriptor_t     *subobj_descr_p,
                                  const char                   *kfulltype,
                                  IMECPreObject                 *object_p) const
{
    //MECPreObject a_subobj(kfulltype,kfulltype,"",0);
    IMECPreObject *a_subobj = HCDI_GetPreObjectHandle(kfulltype,kfulltype,"",0, 0);
    a_subobj->Init((const PseudoDescriptor_t *)subobj_descr_p);
    //MvFullType_t ftp(nullptr, kfulltype); 
    obj_type_e type = (obj_type_e)HCDI_GetHCObjectType(kfulltype); //ftp.getType();
    MYOBJ_INT a_id = object_p->GetId();
    if (type != HCDI_OBJ_TYPE_SUBOBJECT)
    {
        g_subobjectMaxID[type]++;
        a_id = g_subobjectMaxID[type];
        a_subobj->SetId(a_id);
        a_subobj->SetEntityType(type);
    }
    else
    {
        g_subobjectMaxID[type]++;
        a_id = g_subobjectMaxID[type];
        a_subobj->SetId(a_id);
    }
    a_subobj->SetHeaderLine(object_p->GetHeaderLine());
    int a_ok = subobj_reader_p->readObjectData (subobj_format_p,
                                                a_subobj,
                                                model_p,
                                                subobj_descr_p);
    object_p->SetSubobject(a_subobj);
    a_id = a_subobj->GetId();
    if (!a_ok) return 0;

    //int a_id = model_p->AddObject(*a_subobj);
    return a_id;
}

bool MECIDataReader::checkCard(const PseudoFileFormat_t *format_p,
                               int                       card_ind) const
{
    const fileformat_t *a_format_p = (const fileformat_t *)format_p;
    bool a_ok = false;
    int a_nb_cards=0;
    ff_card_t *a_card_format_p = NULL;
    ff_card_type_e a_card_type=CARD_UNKNOWN;

    char *a_card=readBuffer();
    if(a_card==NULL) 
    {
        return false;
    }
    int a_is_header = mySyntaxInfos_p->isHeader(a_card);
    if(a_is_header)
    {
         goto end_of_function;
    }

    // Get card no. card_ind and its type

    MCDS_get_fileformat_nb_cards(a_format_p,&a_nb_cards);
    if (a_nb_cards <= card_ind)  
        goto end_of_function;

    MCDS_get_fileformat_card(a_format_p,card_ind,&a_card_format_p);

    MCDS_get_ff_card_attributes(a_card_format_p,CARD_TYPE,&a_card_type,END_ARGS);
    while(a_card_type == CARD_COMMENT || a_card_type == CARD_ASSIGN)
    {
       card_ind++;
       MCDS_get_fileformat_card(a_format_p,card_ind,&a_card_format_p);
       MCDS_get_ff_card_attributes(a_card_format_p,CARD_TYPE,&a_card_type,END_ARGS); 
    }
    switch(a_card_type) {
    case CARD_SINGLE:
    case CARD_LIST:
    case CARD_PREREAD:
    {
        // Get first cell, test whether it is a comment, and get the comment
        int a_nb_cells = 0;
        MCDS_get_ff_card_attributes(a_card_format_p,CARD_NB_CELLS,&a_nb_cells,END_ARGS);
        if (1 > a_nb_cells) 
            goto end_of_function;

        ff_cell_t *a_cell_format_p=NULL;
        MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,0,(void *)(&a_cell_format_p));

        ff_cell_type_e  a_cell_type = CELL_UNKNOWN;
        MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_TYPE,&a_cell_type,END_ARGS);

        if (CELL_COMMENT == a_cell_type) {
            char *a_comment=NULL;
            MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_STRING,&a_comment,END_ARGS);
            if (NULL==a_comment)  
                goto end_of_function;
             // compare line to the comment cell            
            if(0==strncmp(a_comment,a_card,strlen(a_comment))) a_ok=true; // The card is ok!
        }
        else /*if card doesn't start with comment cell then everything is OK */
           a_ok = true;
    }
        break;
    default:
         goto end_of_function;
    }

end_of_function:
    myReadContext_p->unreadBuffer();

    return a_ok;
}


const char* MECIDataReader::readCell(const char* cell,
    const PseudoFileFormatCell_t* cell_format_p,
    IMECPreObject* object_p,
    void* model_p,
    const PseudoDescriptor_t* descr_p,
    int                           ind,
    bool                          is_free_size_format,
    int                           card_type)
{

    string a_cell_skw;
    const IDescriptor* a_descr_p = (const MvDescriptor_t*)descr_p;
    const ff_cell_t* a_cell_format_p = (const ff_cell_t*)cell_format_p;
    //
    const char* a_cur_cell = cell;
    ff_cell_type_e  a_cell_type = CELL_UNKNOWN;
    MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_TYPE, &a_cell_type, END_ARGS);
    //
    bool a_ok = true;
    switch (a_cell_type) {
    case CELL_COMMENT: 
    {
        a_cur_cell = readCell_COMMENT(cell, cell_format_p, object_p, is_free_size_format, card_type);
    }
    break;
    case CELL_VALUE:
    {
        
        a_cur_cell = readCell_VALUE(cell, cell_format_p, object_p, model_p, descr_p, ind, is_free_size_format);
        
    }
    break;
    case CELL_ID:/*Reads an id of a keyword as in FORMAT block it is defined as _ID means it has id*/
    {
        a_cur_cell = readCell_ID(cell, cell_format_p, object_p, model_p, is_free_size_format);
    }
    break;
    case CELL_BLANK:
    {
        const char* a_cell_fmt = NULL;

        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_FORMAT, &a_cell_fmt, END_ARGS);
        int a_cell_size = 0;// loc_get_fmt_size(a_cell_fmt, is_free_size_format);
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);
        bool has_data = false;
        for (int i = 0; i < a_cell_size; i++)
        {
            if (cell[i] == '\0')
                break;
            if (cell[i] != ' ' && cell[i] != '0')
            {
                has_data = true;
                break;
            }
        }
        if (has_data) {
            if (!is_free_size_format && (ff_card_type_e)card_type == CARD_SINGLE)
            {
                char buff[512];
                strncpy(buff, cell, a_cell_size);
                buff[a_cell_size] = '\0';

                string str;
                str.assign(cell, a_cell_size);

                bool isFloatVal = true;
                float fVal = 0.0;
                try
                {
                    fVal = stof(str);
                }
                catch (...)
                {
                    isFloatVal = false;
                }
                if (!isFloatVal || (isFloatVal && fVal != 0.0))
                {
                    string substr_key("");
                    if (object_p)
                    {
                        const char* a_fulltype = object_p->GetKernelFullType();
                        if (a_fulltype)
                        {
                            string sfulltype(a_fulltype);
                            substr_key = sfulltype.substr(sfulltype.rfind("/") + 1);
                        }
                    }
                    char* a_buff = (char*)myReadContext_p->killBlanksBegin(buff);
                    myReadContext_p->killBlanksEnd(a_buff);
                    std::string war_msg = "";
                  char buffer[BufferSize];
                    const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
                    _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
                    if (a_cur_full_name)
                    {
                        sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
                        war_msg.append(buffer);
                        war_msg.append("\n");
                    }
                    sprintf(buffer, getMsg(36), substr_key.c_str(), a_buff);
                    war_msg.append(buffer);
                    myReadContext_p->displayMessage(myReadContext_p->MSG_WARNING, war_msg.c_str());
                }
            }
        }
        if (a_cell_size > 0)
        {
            a_cur_cell = cell;
            a_cur_cell += a_cell_size;
        }
        if (is_free_size_format)
        {
            unsigned int i = 0;
            a_cell_size = GetCellFreeSize(a_cur_cell);
            if (a_cell_size>0) {
                a_cur_cell += a_cell_size;
            }
        }

        break;
    }
    case CELL_DIR_RADIO:
    {
        const char* a_cell_fmt = NULL;
        int   a_cell_ikw = END_ARGS;
        int   a_cell_is_extended = 0;
        MCDS_get_ff_cell_attributes(a_cell_format_p,
            CELL_FORMAT, &a_cell_fmt,
            CELL_IKEYWORD, &a_cell_ikw,
            CELL_IS_EXTENDED, &a_cell_is_extended,
            END_ARGS);
        //
        
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format); 
        static char a_value[500];
        scanString(a_cur_cell, a_cell_fmt, a_cell_size, a_value, &a_ok);
        
        if (a_ok) {
            a_cur_cell += a_cell_size;
            myReadContext_p->killBlanksNLEnd(a_value);
            //
            dir_type_e a_dir_value = HCDI_MV_get_direction(a_value, a_cell_is_extended);
            if (a_dir_value == DIR_UNKNOWN) {
                std::string err_msg = "";
              char buffer[BufferSize];
                const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
                _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
                if (a_cur_full_name)
                {
                    sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
                    err_msg.append(buffer);
                    err_msg.append("\n");
                }
                sprintf(buffer, getMsg(19), a_value);
                err_msg.append(buffer);
                //
                myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
          } else {
                a_cell_skw = a_descr_p->getSKeyword(a_cell_ikw);
                value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);
                
                if (a_cell_vtype == VTYPE_UINT)
                {
                    if (ind < 0) {
                        object_p->AddUIntValue(a_cell_skw.c_str(), (unsigned int)a_dir_value);
                  } else {
                        int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_cell_skw);
                        if (a_cell_ind < 0) a_cell_ind = loc_reserve_array(a_cell_ikw, a_cell_skw, IMECPreObject::VTY_UINT, a_descr_p, object_p);
                        object_p->SetUIntValue(a_cell_ind, ind, (unsigned int)a_dir_value);
                    }
                }
                else
                {
                    if (ind < 0) {
                        object_p->AddIntValue(a_cell_skw.c_str(), a_dir_value);
                  } else {
                        int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_cell_skw);
                        if (a_cell_ind < 0) a_cell_ind = loc_reserve_array(a_cell_ikw, a_cell_skw, IMECPreObject::VTY_INT, a_descr_p, object_p);
                        object_p->SetIntValue(a_cell_ind, ind, a_dir_value);
                    }
                }

            }
        }
    }
    break;
    case CELL_DIR_FLAGS:
    {
        const char* a_cell_fmt = NULL;
        int   a_xdir_ikw = END_ARGS, a_ydir_ikw = END_ARGS, a_zdir_ikw = END_ARGS;
        int   a_xdir = 0, a_ydir = 0, a_zdir = 0;
        MCDS_get_ff_cell_attributes(a_cell_format_p,
            CELL_FORMAT, &a_cell_fmt,
            CELL_DIRX_IKW, &a_xdir_ikw,
            CELL_DIRY_IKW, &a_ydir_ikw,
            CELL_DIRZ_IKW, &a_zdir_ikw,
            END_ARGS);
        //
        
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);
        static char a_value[500];
        scanString(a_cur_cell, a_cell_fmt, a_cell_size, a_value, &a_ok);
        
        //
        if (a_ok) {
            a_cur_cell += a_cell_size;
            myReadContext_p->killBlanksNLEnd(a_value);
            //
            if (!HCDI_MV_get_directions(a_value, &a_xdir, &a_ydir, &a_zdir)) {
                std::string err_msg = "";
        char buffer[BufferSize];
                const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
                _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
                if (a_cur_full_name)
                {
                    sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
                    err_msg.append(buffer);
                    err_msg.append("\n");
                }
                sprintf(buffer, getMsg(19), a_value);
                err_msg.append(buffer);
                //
                myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
	} else {
                string a_xdir_skw = a_descr_p->getSKeyword(a_xdir_ikw);
                string a_ydir_skw = a_descr_p->getSKeyword(a_ydir_ikw);
                string a_zdir_skw = a_descr_p->getSKeyword(a_zdir_ikw);
                //
                if (ind < 0) {
                    object_p->AddIntValue(a_xdir_skw.c_str(), a_xdir);
                    object_p->AddIntValue(a_ydir_skw.c_str(), a_ydir);
                    object_p->AddIntValue(a_zdir_skw.c_str(), a_zdir);
	  } else {
                    
                    value_type_e a_cell_vtype = a_descr_p->getValueType(a_xdir_ikw);
                    if (a_cell_vtype == VTYPE_UINT)
                    {
                        int a_xdir_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_xdir_skw);
                        int a_ydir_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_ydir_skw);
                        int a_zdir_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_zdir_skw);
                        if (a_xdir_ind < 0) a_xdir_ind = loc_reserve_array(a_xdir_ikw, a_xdir_skw, IMECPreObject::VTY_UINT, a_descr_p, object_p);
                        if (a_ydir_ind < 0) a_xdir_ind = loc_reserve_array(a_ydir_ikw, a_ydir_skw, IMECPreObject::VTY_UINT, a_descr_p, object_p);
                        if (a_zdir_ind < 0) a_xdir_ind = loc_reserve_array(a_zdir_ikw, a_zdir_skw, IMECPreObject::VTY_UINT, a_descr_p, object_p);
                        object_p->SetUIntValue(a_xdir_ind, ind, (unsigned int)a_xdir);
                        object_p->SetUIntValue(a_ydir_ind, ind, (unsigned int)a_ydir);
                        object_p->SetUIntValue(a_zdir_ind, ind, (unsigned int)a_zdir);
                    }
                    else
                    {
                        int a_xdir_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_xdir_skw);
                        int a_ydir_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_ydir_skw);
                        int a_zdir_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_zdir_skw);
                        if (a_xdir_ind < 0) a_xdir_ind = loc_reserve_array(a_xdir_ikw, a_xdir_skw, IMECPreObject::VTY_INT, a_descr_p, object_p);
                        if (a_ydir_ind < 0) a_xdir_ind = loc_reserve_array(a_ydir_ikw, a_ydir_skw, IMECPreObject::VTY_INT, a_descr_p, object_p);
                        if (a_zdir_ind < 0) a_xdir_ind = loc_reserve_array(a_zdir_ikw, a_zdir_skw, IMECPreObject::VTY_INT, a_descr_p, object_p);
                        object_p->SetIntValue(a_xdir_ind, ind, a_xdir);
                        object_p->SetIntValue(a_ydir_ind, ind, a_ydir);
                        object_p->SetIntValue(a_zdir_ind, ind, a_zdir);

                    }
                }
            }
        }
    }
    break;
    case CELL_DIGITS:
    {
        int         i = 0;
        int         a_nb_ikws = 0;
        const char* a_cell_fmt = NULL;
        //
        MCDS_get_ff_cell_attributes(a_cell_format_p,
            CELL_FORMAT, &a_cell_fmt,
            CELL_NB_IKEYWORDS, &a_nb_ikws,
            END_ARGS);
        
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);

        value_type_e a_cell_vtype = a_descr_p->getValueType(a_nb_ikws);
        int a_value = 0;
        if (a_cell_vtype == VTYPE_UINT)
        {
            a_value = (int)scanUInt(a_cur_cell, a_cell_fmt, a_cell_size, &a_ok,
                true); 
        }
        else
        {
            a_value = scanInt(a_cur_cell, a_cell_fmt, a_cell_size, &a_ok,
                true); 
        }
        
        //
        if (a_ok) {
            typedef vector<string> LocSKeywords_t;
            LocSKeywords_t a_skeywords;
            //
            a_cur_cell += a_cell_size;
            //
            a_skeywords.reserve(a_nb_ikws);
            for (i = 0; i < a_nb_ikws; ++i) {
                int a_ikeyword = END_ARGS;
                MCDS_get_ff_cell_tab(a_cell_format_p, CELL_IKEYWORD, i, (void*)(&a_ikeyword));
                //
                if (ind < 0) {
                    if (loc_is_ikeyword_valid(a_ikeyword, *object_p, a_descr_p)) {
                        string a_skeyword = a_descr_p->getSKeyword(a_ikeyword);
                        a_skeywords.push_back(a_skeyword);
                    }
                  } else {
                    string a_skeyword = a_descr_p->getSKeyword(a_ikeyword);
                    value_type_e a_lcell_vtype = a_descr_p->getValueType(a_ikeyword);
                    if (a_lcell_vtype == VTYPE_UINT)
                    {
                        int    a_attrib_index = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_skeyword);
                        
                        if (a_attrib_index < 0) a_attrib_index = loc_reserve_array(a_ikeyword, a_skeyword, IMECPreObject::VTY_UINT, a_descr_p, object_p);
                    }
                    else
                    {
                        int    a_attrib_index = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_skeyword);
                        
                        if (a_attrib_index < 0) a_attrib_index = loc_reserve_array(a_ikeyword, a_skeyword, IMECPreObject::VTY_INT, a_descr_p, object_p);
                    }
                    a_skeywords.push_back(a_skeyword);
                }
            }
            //
            a_nb_ikws = (int)a_skeywords.size();
            for (i = a_nb_ikws - 1; i >= 0; --i) {
                int a_ikeyword = a_descr_p->getIKeyword(a_skeywords[i]);
                value_type_e a_lcell_vtype = a_descr_p->getValueType(a_ikeyword);
                if (a_lcell_vtype == VTYPE_UINT)
                {
                    unsigned int a_digit_value = (unsigned int)(a_value % 10);
                    if (ind < 0) object_p->AddUIntValue(a_skeywords[i].c_str(), a_digit_value);
                    else      object_p->AddUIntValue(a_skeywords[i].c_str(), ind, a_digit_value);
                }
                else
                {
                    int a_digit_value = a_value % 10;
                    if (ind < 0) object_p->AddIntValue(a_skeywords[i].c_str(), a_digit_value);
                    else      object_p->AddIntValue(a_skeywords[i].c_str(), ind, a_digit_value);
                }
                a_value /= 10;
            }
        }
    }
    break;
    
    case CELL_SCALAR_OR_OBJECT:
    {
        
        a_cur_cell = readCell_SCALAR_OR_OBJECT(cell, cell_format_p, object_p, model_p, descr_p, ind, is_free_size_format);
        
    }
    break;
    case CELL_FLAGGED_OBJECT:
    {
        // get info about format and object
        const char* a_cell_fmt = NULL;  // Revisit if MYOBJ_INT type is changed and see if CELL_FLAGGED_OBJECT case needs any changes to be done.
        bool has_empty_field = true;
        int is_flagged = 0;
        int   a_object_ikw = END_ARGS, a_flag_ikw = END_ARGS;
        MCDS_get_ff_cell_attributes(a_cell_format_p,
            CELL_FORMAT, &a_cell_fmt,
            CELL_OBJECT_IKW, &a_object_ikw,
            CELL_FLAG_IKW, &a_flag_ikw,
            END_ARGS);
        string       a_object_skw = a_descr_p->getSKeyword(a_object_ikw);
        string       a_flag_skw = a_descr_p->getSKeyword(a_flag_ikw);
        // read the value, which has to be an integer
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format); 
        //check if the cell is parameter  
        MYOBJ_INT a_id = 0;
        bool is_parameter_negated = false;
        string param_str = "";
        bool is_param_cell = isParameterCell(a_cur_cell, a_cell_size, param_str, &is_parameter_negated);
        if (is_param_cell)
        {
            if (model_p != NULL)
            {
                MECIModelFactory* nc_model_p = (MECIModelFactory*)model_p;
                a_id = nc_model_p->GetIntParameter(param_str.c_str(), object_p->GetFileIndex());
                if (is_parameter_negated)
                {
                    a_id = -1 * a_id;
                }
            }
            a_ok = true;
        }
        else
        {
            char* a_cell = const_cast<char*>(a_cur_cell);
            int i = 0;
            for (i = 0; i < a_cell_size; i++)
            {
                if (a_cell[i] == ' ')
                    continue;
                else
                {
                    if (a_cell[i] == '-')
                    {
                        is_flagged = 1;
                        a_cell[i] = ' ';
                    }
                    break;
                }
            }
            a_id = scanObject(a_cell, a_cell_fmt, a_cell_size, &a_ok, true, &has_empty_field);
        }
        //
        if (a_ok) {
            a_cur_cell += a_cell_size;

            // Check whether the object is flagged
            // This is made for Dyna:
            // If we read a negative ID, the object is flagged.
            // This might be Dyna specific, but I don't see another way to flag an ID than to have its negative value!
            if (is_param_cell) //Check if cell is paramaterized
            {
                if (is_parameter_negated)
                    is_flagged = 1;
            }

            if (is_param_cell)
            {
                object_p->SetParameterName(param_str.c_str(), a_object_skw.c_str(), ind, is_parameter_negated);
            }
            // Put object in object_p
            if ((a_id >= 0 && !has_empty_field) || is_param_cell) { 
                object_type_e  a_cell_otype = a_descr_p->getObjectType(a_object_ikw);
                const string& a_cell_otype_str = MV_get_type(a_cell_otype);
                bool           a_is_treated = false;//model_p->IsTreated(a_cell_otype_str.c_str());
                //
                if (a_is_treated) {

              } else {
                    if (ind < 0)
                    {
                        object_p->AddObjectValue(a_object_skw.c_str(), a_cell_otype_str.c_str(), a_id);
                    }
                    //else      object_p->AddObjectValue(a_object_skw.c_str(),ind,a_cell_otype_str.c_str(),a_id);
                    else
                    {
                        int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_object_skw);
                        if (a_cell_ind < 0) a_cell_ind = loc_reserve_array(a_object_ikw, a_object_skw, IMECPreObject::VTY_OBJECT, a_descr_p, object_p);
                        object_p->SetObjectValue(a_cell_ind, ind, a_cell_otype_str.c_str(), a_id);
                    }
                }
            } 

            value_type_e a_cell_vtype = a_descr_p->getValueType(a_flag_ikw);
            // Put flag in object_p
            if (ind < 0)
            {
                if (a_cell_vtype == VTYPE_BOOL)
                    object_p->AddBoolValue(a_flag_skw.c_str(), (is_flagged > 0 ? true : false));
                else if (a_cell_vtype == VTYPE_INT)
                {
                    object_p->AddIntValue(a_flag_skw.c_str(), is_flagged);
                }
            }
            else
            {
                //object_p->AddIntValue(a_flag_skw.c_str(),ind,is_flagged);
                if (a_cell_vtype == VTYPE_BOOL)
                {
                    int a_flag_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_flag_skw);
                    if (a_flag_ind < 0) a_flag_ind = loc_reserve_array(a_flag_ikw, a_flag_skw, IMECPreObject::VTY_BOOL, a_descr_p, object_p);
                    object_p->SetBoolValue(a_flag_ind, ind, (is_flagged > 0 ? true : false));
                }
                else if (a_cell_vtype == VTYPE_INT)
                {
                    int a_flag_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_flag_skw);
                    if (a_flag_ind < 0) a_flag_ind = loc_reserve_array(a_flag_ikw, a_flag_skw, IMECPreObject::VTY_INT, a_descr_p, object_p);
                    object_p->SetIntValue(a_flag_ind, ind, is_flagged);
                }
            }
        }
    }
    break;
    
    
    case CELL_SCALAR_OR_STRING:
    {
        // get info about format and object
        const char* a_cell_fmt = NULL;
        int   a_flag_ikw = END_ARGS, a_scalar_ikw = END_ARGS, a_string_ikw = END_ARGS;
        MCDS_get_ff_cell_attributes(a_cell_format_p,
            CELL_FORMAT, &a_cell_fmt,
            CELL_FLAG_IKW, &a_flag_ikw,
            CELL_SCALAR_IKW, &a_scalar_ikw,
            CELL_STRING_IKW, &a_string_ikw,
            END_ARGS);
        value_type_e a_scalar_vtype = a_descr_p->getValueType(a_scalar_ikw);
        string       a_flag_skw = a_descr_p->getSKeyword(a_flag_ikw);
        string       a_scalar_skw = a_descr_p->getSKeyword(a_scalar_ikw);
        string       a_string_skw = a_descr_p->getSKeyword(a_string_ikw);

        // we first read the cell as a string
        
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);
        static char a_string[500];
        scanString(a_cur_cell, a_cell_fmt, a_cell_size, a_string, &a_ok);
        
        if (a_ok) {
            a_cur_cell += a_cell_size;

            

             // try to scan a value, and if successful add it to object_p
            int a_nb_read = 0;
            switch (a_scalar_vtype) {
            case VTYPE_INT:
            {
                int a_value = 0;
                a_nb_read = sscanf(a_string, "%d", &a_value);
                if (a_nb_read > 0)
                {
                    if (ind < 0)
                    {
                        object_p->AddIntValue(a_scalar_skw.c_str(), a_value);
                    }
                    //else      object_p->AddIntValue(a_scalar_skw.c_str(),ind,a_value);
                    else
                    {
                        int a_scalar_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_scalar_skw);
                        if (a_scalar_ind < 0) a_scalar_ind = loc_reserve_array(a_scalar_ikw, a_scalar_skw, IMECPreObject::VTY_INT, a_descr_p, object_p);
                        object_p->SetIntValue(a_scalar_ind, ind, a_value);
                    }
                }
            }
            break;
            case VTYPE_UINT:
            {
                unsigned int a_value = 0;
                a_nb_read = sscanf(a_string, "%u", &a_value);
                if (a_nb_read > 0)
                {
                    if (ind < 0)
                    {
                        object_p->AddUIntValue(a_scalar_skw.c_str(), a_value);
                    }
                    //else      object_p->AddIntValue(a_scalar_skw.c_str(),ind,a_value);
                    else
                    {
                        int a_scalar_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_scalar_skw);
                        if (a_scalar_ind < 0) a_scalar_ind = loc_reserve_array(a_scalar_ikw, a_scalar_skw, IMECPreObject::VTY_UINT, a_descr_p, object_p);
                        object_p->SetUIntValue(a_scalar_ind, ind, a_value);
                    }
                }
            }
            break;
            case VTYPE_FLOAT:
            {
                double a_value = 0.;
                a_nb_read = sscanf(a_string, "%lg", &a_value);
                if (a_nb_read > 0)
                {
                    if (ind < 0)
                    {
                        object_p->AddFloatValue(a_scalar_skw.c_str(), a_value);
                    }
                    //else      object_p->AddFloatValue(a_scalar_skw.c_str(),ind,a_value);
                    else
                    {
                        int a_scalar_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_scalar_skw);
                        if (a_scalar_ind < 0) a_scalar_ind = loc_reserve_array(a_scalar_ikw, a_scalar_skw, IMECPreObject::VTY_FLOAT, a_descr_p, object_p);
                        object_p->SetFloatValue(a_scalar_ind, ind, a_value);
                    }
                }
            }
            break;
            case VTYPE_OBJECT:
            {
                object_type_e  a_cell_otype = a_descr_p->getObjectType(a_scalar_ikw);
                const string& a_cell_otype_str = MV_get_type(a_cell_otype);
                MYOBJ_INT a_value = 0;
                a_nb_read = sscanf(a_string, "%u", &a_value);
                if (a_nb_read > 0)
                {
                    if (ind < 0)
                    {
                        object_p->AddObjectValue(a_scalar_skw.c_str(), a_cell_otype_str.c_str(), a_value, -1);
                    }
                    //else      object_p->AddIntValue(a_scalar_skw.c_str(),ind,a_value);
                    else
                    {
                        int a_obj_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_scalar_skw);
                        if (a_obj_ind < 0) a_obj_ind = loc_reserve_array(a_scalar_ikw, a_scalar_skw, IMECPreObject::VTY_OBJECT, a_descr_p, object_p);
                        object_p->SetObjectValue(a_obj_ind, 0, a_cell_otype_str.c_str(), a_value, ind);
                    }
                }
            }
            break;
            default:
                break;
            }

            value_type_e a_cell_vtype = a_descr_p->getValueType(a_flag_ikw);
            // If a value has been scanned, we set the flag to false (0), and true (1) otherwise
            // (the flag could be called "is_string")
            if (a_nb_read > 0) {
                if (ind < 0)
                {
                    if (a_cell_vtype == VTYPE_BOOL)
                        object_p->AddBoolValue(a_flag_skw.c_str(), false);
                    else if (a_cell_vtype == VTYPE_INT)
                        object_p->AddIntValue(a_flag_skw.c_str(), 0);
                }
                //else      object_p->AddIntValue(a_flag_skw.c_str(),ind,0);
                else
                {
                    if (a_cell_vtype == VTYPE_BOOL)
                    {
                        int a_flag_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_flag_skw);
                        if (a_flag_ind < 0) a_flag_ind = loc_reserve_array(a_flag_ikw, a_flag_skw, IMECPreObject::VTY_BOOL, a_descr_p, object_p);
                        object_p->SetIntValue(a_flag_ind, ind, false);
                    }
                    else if (a_cell_vtype == VTYPE_INT)
                    {
                        int a_flag_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_flag_skw);
                        if (a_flag_ind < 0) a_flag_ind = loc_reserve_array(a_flag_ikw, a_flag_skw, IMECPreObject::VTY_INT, a_descr_p, object_p);
                        object_p->SetIntValue(a_flag_ind, ind, 0);
                    }
                }
          } else {
                if (ind < 0)
                {
                    if (a_cell_vtype == VTYPE_BOOL)
                        object_p->AddBoolValue(a_flag_skw.c_str(), true);
                    else if (a_cell_vtype == VTYPE_INT)
                        object_p->AddIntValue(a_flag_skw.c_str(), 1);
                }
                //else      object_p->AddIntValue(a_flag_skw.c_str(),ind,1);
                else
                {
                    if (a_cell_vtype == VTYPE_BOOL)
                    {
                        int a_flag_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_flag_skw);
                        if (a_flag_ind < 0) a_flag_ind = loc_reserve_array(a_flag_ikw, a_flag_skw, IMECPreObject::VTY_BOOL, a_descr_p, object_p);
                        object_p->SetBoolValue(a_flag_ind, ind, true);
                    }
                    else if (a_cell_vtype == VTYPE_INT)
                    {
                        int a_flag_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_flag_skw);
                        if (a_flag_ind < 0) a_flag_ind = loc_reserve_array(a_flag_ikw, a_flag_skw, IMECPreObject::VTY_INT, a_descr_p, object_p);
                        object_p->SetIntValue(a_flag_ind, ind, 1);
                    }
                }
                
                if (ind < 0)
                {
                    object_p->AddStringValue(a_string_skw.c_str(), a_string);
                }
                //else      object_p->AddStringValue(a_string_skw.c_str(),ind,a_string);
                else
                {
                    int a_string_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_string_skw);
                    if (a_string_ind < 0) a_string_ind = loc_reserve_array(a_string_ikw, a_string_skw, IMECPreObject::VTY_STRING, a_descr_p, object_p);
                    object_p->SetStringValue(a_string_ind, ind, a_string);
                }
                
            }
        }
    }
    break;
    case CELL_COND:
        a_cur_cell = readIfCell(a_cur_cell, cell_format_p, object_p, model_p, descr_p, ind, &a_ok);
        break;
    case CELL_PAIR:
        a_cur_cell = readPairCell(a_cur_cell, cell_format_p, object_p, model_p, descr_p, ind, &a_ok, is_free_size_format);
        break;
    case CELL_NAME_VALUE:
    {
        const ff_cell_t* a_cell_format_p = (const ff_cell_t*)cell_format_p;
        const char* a_cell_fmt = NULL;
        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_FORMAT, &a_cell_fmt, END_ARGS);

        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);

        static char a_value[500];
        myReadContext_p->killBlanksEnd((char *)a_cur_cell);
        strcpy(a_value, a_cur_cell);
        char pair_char, separator;
        MCDS_get_ff_cell_attributes(a_cell_format_p, CARD_NAME_VALUE_PAIR_CHAR, &pair_char, CARD_NAME_VALUE_PAIR_SEPARATOR, &separator, END_ARGS);
        int nb_cells = 0;
        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_NAME_VALUE_NUMBER, &nb_cells, END_ARGS);
        vector< pair <int, string> > ikw_str_vect;
        for (int i = 0; i < nb_cells; i++)
        {
            int ikw = 0;
            MCDS_get_ff_cell_tab(a_cell_format_p, CELL_NAME_VALUE_IKEYWORD, i, &ikw);
            char* a_skeyword = NULL;
            MCDS_get_ff_cell_tab(a_cell_format_p, CELL_NAME_VALUE_STRING, i, &a_skeyword);
            ikw_str_vect.push_back(make_pair(ikw, string(a_skeyword)));
        }
        char* token = NULL;
        token = strtok(a_value, &separator);
        vector<string> a_vect;
        while (token)
        {
            string name_value = token;
            a_vect.push_back(name_value);
            token = strtok(NULL, &separator);
        }
        int size = (int)a_vect.size();
        for(int i=0; i < size; i++)
        {
            string name_value = a_vect.at(i);
            size_t result = name_value.find(pair_char);
            if (result == string::npos)
            {
                for (int j = 0; j < nb_cells; j++)
                {
                    pair <int, string> a_pair = ikw_str_vect.at(j);
                    if (a_pair.second == "")
                    {
                        // search if any radio feature of type string present in gui block
                        MvDataFeatureList_t* list = a_descr_p->getDataFeatures(DOM_COMMON, DFT_RADIO);
                        MvDataFeatureList_t::const_iterator it;
                        MvDataFeatureList_t::const_iterator it_begin = list->begin();
                        MvDataFeatureList_t::const_iterator it_end = list->end();
                        bool if_found = false;
                        for (it = it_begin; it != it_end; ++it)
                        {
                            const MvDataFeature_t* feat = *it;
                            int aikw = feat->getIKeyword();
                            value_type_e a_vtype = a_descr_p->getValueType(aikw);
                            if (a_vtype == VTYPE_STRING)
                            {
                                const MvDataRadioFeature_t* rad_feat = dynamic_cast<const MvDataRadioFeature_t*>(feat);
                                if (rad_feat == NULL) continue;
                                int nb_items = rad_feat->getNumber();
                                for (int k = 0; k < nb_items; k++)
                                {
                                    string item = rad_feat->getRadioStringValue(k);
                                    if (item == name_value)
                                    {
                                        string a_skw = a_descr_p->getSKeyword(aikw);
                                        object_p->AddStringValue(a_skw.c_str(), name_value.c_str());
                                        if_found = true;
                                        break;
                                    }
                                }
                                if (if_found)
                                    break;
                            }
                        }
                        if (if_found)
                            break;

                        // get the data type in name_value, scan the ikeywords with no string value, set to a matched data type
                        int a_nb_read = 0;
                        int a_ivalue = 0;
                        a_nb_read = sscanf(name_value.c_str(), "%d", &a_ivalue);
                        if (a_nb_read > 0)
                        {
                            for (int k = 0; k < nb_cells; k++)
                            {
                                pair <int, string> a_pair = ikw_str_vect.at(k);
                                if (a_pair.second == "")
                                {
                                    int ikw = a_pair.first;
                                    value_type_e a_vtype = a_descr_p->getValueType(ikw);
                                    if (a_vtype != VTYPE_INT && a_vtype != VTYPE_BOOL)
                                        continue;
                                    string a_skw = a_descr_p->getSKeyword(ikw);
                                    object_p->AddIntValue(a_skw.c_str(), a_ivalue);
                                    break;
                                }
                            }
                            break;
                        }
                        double a_fvalue = 0.;
                        a_nb_read = sscanf(name_value.c_str(), "%lg", &a_fvalue);
                        if (a_nb_read > 0)
                        {
                            for (int k = 0; k < nb_cells; k++)
                            {
                                pair <int, string> a_pair = ikw_str_vect.at(k);
                                if (a_pair.second == "")
                                {
                                    int ikw = a_pair.first;
                                    value_type_e a_vtype = a_descr_p->getValueType(ikw);
                                    if (a_vtype != VTYPE_FLOAT)
                                        continue;
                                    string a_skw = a_descr_p->getSKeyword(ikw);
                                    object_p->AddFloatValue(a_skw.c_str(), a_fvalue);
                                    break;
                                }
                            }
                            break;
                        }
                        for (int k = 0; k < nb_cells; k++)
                        {
                            pair <int, string> a_pair = ikw_str_vect.at(k);
                            if (a_pair.second == "")
                            {
                                int ikw = a_pair.first;
                                value_type_e a_vtype = a_descr_p->getValueType(ikw);
                                if (a_vtype != VTYPE_STRING)
                                    continue;
                                string a_skw = a_descr_p->getSKeyword(ikw);
                                object_p->AddStringValue(a_skw.c_str(), name_value.c_str());
                                if_found = true;
                                break;
                            }
                        }
                        if (if_found)
                            break;
                        int ikw = a_pair.first;
                        value_type_e a_vtype = a_descr_p->getValueType(ikw);
                        string a_skw = a_descr_p->getSKeyword(ikw);
                        a_nb_read = 0;
                        switch (a_vtype)
                        {
                            case VTYPE_INT:
                            case VTYPE_BOOL:
                            {
                                int a_ivalue = 0;
                                a_nb_read = sscanf(name_value.c_str(), "%d", &a_ivalue);
                                if (a_nb_read > 0)
                                {
                                    object_p->AddIntValue(a_skw.c_str(), a_ivalue);
                                }
                            }
                            break;
                            case VTYPE_UINT:
                            {
                                unsigned int a_uvalue = 0;
                                a_nb_read = sscanf(name_value.c_str(), "%u", &a_uvalue);
                                if (a_nb_read > 0)
                                {
                                    object_p->AddUIntValue(a_skw.c_str(), a_uvalue);
                                }
                            }
                            break;
                            case VTYPE_FLOAT:
                            {
                                double a_fvalue = 0.;
                                a_nb_read = sscanf(name_value.c_str(), "%lg", &a_fvalue);
                                if (a_nb_read > 0)
                                {
                                    object_p->AddFloatValue(a_skw.c_str(), a_fvalue);
                                }
                            }
                            break;
                            case VTYPE_STRING:
                                object_p->AddStringValue(a_skw.c_str(), name_value.c_str());
                            break;
                            case VTYPE_OBJECT:
                            {
                                object_type_e  a_cell_otype = a_descr_p->getObjectType(ikw);
                                const string& a_cell_otype_str = MV_get_type(a_cell_otype);
                                MYOBJ_INT a_ovalue = 0;
                                a_nb_read = sscanf(name_value.c_str(), "%u", &a_ovalue);
                                if (a_nb_read > 0)
                                {
                                    object_p->AddObjectValue(a_skw.c_str(), a_cell_otype_str.c_str(), a_ovalue, -1);
                                }
                            }
                            break;
                        }
                        break;
                    }
                }
            }
            else
            {
                char* token2 = strtok((char *)name_value.c_str(), &pair_char);
                while (token2)
                {
                    string skw_to_search = token2;
                    token2 = strtok(NULL, &pair_char);
                    string name_value2 = token2;
                    for (int j = 0; j < nb_cells; j++)
                    {
                        pair <int, string> a_pair = ikw_str_vect.at(j);
                        if (a_pair.second == skw_to_search)
                        {
                            int ikw = a_pair.first;
                            value_type_e a_vtype = a_descr_p->getValueType(ikw);
                            string a_skw = a_descr_p->getSKeyword(ikw);
                            int a_nb_read = 0;
                            switch (a_vtype)
                            {
                                case VTYPE_INT:
                                case VTYPE_BOOL:
                                {
                                    int a_ivalue = 0;
                                    a_nb_read = sscanf(name_value2.c_str(), "%d", &a_ivalue);
                                    if (a_nb_read > 0)
                                    {
                                        object_p->AddIntValue(a_skw.c_str(), a_ivalue);
                                    }
                                }
                                break;
                                case VTYPE_UINT:
                                {
                                    unsigned int a_uvalue = 0;
                                    a_nb_read = sscanf(name_value2.c_str(), "%u", &a_uvalue);
                                    if (a_nb_read > 0)
                                    {
                                        object_p->AddUIntValue(a_skw.c_str(), a_uvalue);
                                    }
                                }
                                break;
                                case VTYPE_FLOAT:
                                {
                                    double a_fvalue = 0.;
                                    a_nb_read = sscanf(name_value2.c_str(), "%lg", &a_fvalue);
                                    if (a_nb_read > 0)
                                    {
                                        object_p->AddFloatValue(a_skw.c_str(), a_fvalue);
                                    }
                                }
                                break;
                                case VTYPE_STRING:
                                    object_p->AddStringValue(a_skw.c_str(), name_value2.c_str());
                                break;
                                case VTYPE_OBJECT:
                                {
                                    object_type_e  a_cell_otype = a_descr_p->getObjectType(ikw);
                                    const string& a_cell_otype_str = MV_get_type(a_cell_otype);
                                    MYOBJ_INT a_ovalue = 0;
                                    a_nb_read = sscanf(name_value2.c_str(), "%u", &a_ovalue);
                                    if (a_nb_read > 0)
                                    {
                                        object_p->AddObjectValue(a_skw.c_str(), a_cell_otype_str.c_str(), a_ovalue, -1);
                                    }
                                }
                                break;
                            }
                            break;
                        }
                    }
                    token2 = strtok(NULL, &pair_char);
                }
            }
        }

        if (a_ok) {
            a_cur_cell += a_cell_size;
        }
    }
    break;
    case CELL_LIST:
    {
        a_cur_cell = readCellArrayList(a_cur_cell, cell_format_p, object_p, model_p, descr_p, ind, is_free_size_format, &a_ok);
    }
    break;
    case CELL_APPEND_OPTIONS:
    {
        int a_cell_size = 0;
        bool is_param_cell = false;
        bool is_parameter_negated = false;
        static char a_value[500];
        const char* a_cell_fmt = NULL;
        string test_str = "";
        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_FORMAT, &a_cell_fmt, END_ARGS);

        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);
        string param_str = "";
        is_param_cell = isParameterCell(a_cur_cell, a_cell_size, param_str);
        if (is_param_cell)
        {
            if (model_p != NULL)
            {
                MECIModelFactory* nc_model_p = (MECIModelFactory*)model_p;
                string str_val = nc_model_p->GetTextParameter(param_str.c_str(), object_p->GetFileIndex());
                strcpy(a_value, str_val.c_str());
            }
            a_ok = true;
            test_str = a_value;
        }
        else
        {
            test_str = a_cur_cell;
        }
        ff_append_option_cell_t* a_cell_p = (ff_append_option_cell_t*)a_cell_format_p;
        if (a_cell_p)
        {
            vector<AppendOptions_t*>& a_vect = a_cell_p->options;
            int size = (int)a_vect.size();
            int total_size = 0;
            string total_str = "";
            for (int i = 0; i < size; i++)
            {
                AppendOptions_t* data = a_vect[i];
                if (data == NULL)
                    continue;

                value_type_e vtype = data->vtype;
                if (vtype == VTYPE_INT)
                {
                    AppendOptionsOthers_t<int>* data2 = ((AppendOptionsOthers_t<int>*)data);
                    const string& skw = data2->getSkeyword();
                    vector<pair <string, int>>& options_values_vect = data2->getOptionsValuesVect();
                    vector<pair <string, int>>::iterator iter_b = options_values_vect.begin();
                    vector<pair <string, int>>::iterator iter_e = options_values_vect.end();
                    vector<pair <string, int>>::iterator iter;

                    for (iter = iter_b; iter != iter_e; ++iter)
                    {
                        int a_lcell_size = 0;
                        string text = (*iter).first;
                        size_t pos = test_str.find(text);
                        int a_lvalue = (*iter).second;
                        if (pos != std::string::npos)
                        {
                            object_p->AddIntValue(skw.c_str(), a_lvalue);
                            a_lcell_size = (int)text.size();
                            total_size += a_lcell_size;
                            total_str += text;
                            test_str.erase(pos, pos + a_lcell_size);
                            a_cur_cell = test_str.c_str();
                        }
                    }
                }
                else if (vtype == VTYPE_FLOAT)
                {
                    AppendOptionsOthers_t<double>* data2 = ((AppendOptionsOthers_t<double>*)data);
                    const string& skw = data2->getSkeyword();
                    vector<pair <string, double>>& options_values_vect = data2->getOptionsValuesVect();
                    vector<pair <string, double>>::iterator iter_b = options_values_vect.begin();
                    vector<pair <string, double>>::iterator iter_e = options_values_vect.end();
                    vector<pair <string, double>>::iterator iter;

                    for (iter = iter_b; iter != iter_e; ++iter)
                    {
                        string text = (*iter).first;
                        size_t pos = test_str.find(text);
                        double a_lvalue = (*iter).second;
                        if (pos != std::string::npos)
                        {
                            int a_lcell_size = 0;
                            object_p->AddFloatValue(skw.c_str(), a_lvalue);
                            a_lcell_size = (int)text.size();
                            total_size += a_lcell_size;
                            total_str += text;
                            test_str.erase(pos, pos + a_lcell_size);
                            a_cur_cell = test_str.c_str();
                        }
                    }
                }
                else if (vtype == VTYPE_BOOL)
                {
                    AppendOptionsOthers_t<bool>* data2 = ((AppendOptionsOthers_t<bool>*)data);
                    const string& skw = data2->getSkeyword();
                    vector<pair <string, bool>>& options_values_vect = data2->getOptionsValuesVect();
                    vector<pair <string, bool>>::iterator iter_b = options_values_vect.begin();
                    vector<pair <string, bool>>::iterator iter_e = options_values_vect.end();
                    vector<pair <string, bool>>::iterator iter;

                    for (iter = iter_b; iter != iter_e; ++iter)
                    {
                        int a_lcell_size = 0;
                        string text = (*iter).first;
                        size_t pos = test_str.find(text);
                        bool a_lvalue = (*iter).second;
                        if (pos != std::string::npos)
                        {
                            object_p->AddBoolValue(skw.c_str(), a_lvalue);
                            a_lcell_size = (int)text.size();
                            total_size += a_lcell_size;
                            total_str += text;
                            test_str.erase(pos, pos + a_lcell_size);
                            a_cur_cell = test_str.c_str();
                        }
                    }
                }
                else if (vtype == VTYPE_UINT)
                {
                    AppendOptionsOthers_t<unsigned int>* data2 = ((AppendOptionsOthers_t<unsigned int>*)data);
                    const string& skw = data2->getSkeyword();
                    vector<pair <string, unsigned int>>& options_values_vect = data2->getOptionsValuesVect();
                    vector<pair <string, unsigned int>>::iterator iter_b = options_values_vect.begin();
                    vector<pair <string, unsigned int>>::iterator iter_e = options_values_vect.end();
                    vector<pair <string, unsigned int>>::iterator iter;

                    for (iter = iter_b; iter != iter_e; ++iter)
                    {
                        int a_lcell_size = 0;
                        string text = (*iter).first;
                        size_t pos = test_str.find(text);
                        unsigned int a_lvalue = (*iter).second;
                        if (pos != std::string::npos)
                        {
                            object_p->AddUIntValue(skw.c_str(), a_lvalue);
                            a_lcell_size = (int)text.size();
                            total_size += a_lcell_size;
                            total_str += text;
                            test_str.erase(pos, pos + a_lcell_size);
                            a_cur_cell = test_str.c_str();
                        }
                    }
                }
                else if (vtype == VTYPE_STRING)
                {
                    AppendOptionsOthers_t<string>* data2 = ((AppendOptionsOthers_t<string>*)data);
                    const string& skw = data2->getSkeyword();
                    vector<pair <string, string>>& options_values_vect = data2->getOptionsValuesVect();
                    vector<pair <string, string>>::iterator iter_b = options_values_vect.begin();
                    vector<pair <string, string>>::iterator iter_e = options_values_vect.end();
                    vector<pair <string, string>>::iterator iter;

                    for (iter = iter_b; iter != iter_e; ++iter)
                    {
                        int a_lcell_size = 0;
                        string text = (*iter).first;
                        size_t pos = test_str.find(text);
                        string a_lvalue = (*iter).second;
                        if (pos != std::string::npos)
                        {
                            object_p->AddStringValue(skw.c_str(), a_lvalue.c_str());
                            a_lcell_size = (int)text.size();
                            total_size += a_lcell_size;
                            total_str += text;
                            test_str.erase(pos, pos + a_lcell_size);
                            a_cur_cell = test_str.c_str();
                        }
                    }
                }
            }
            if (!is_free_size_format)
            {
                int pending_spaces = a_cell_size - total_size;
                if (pending_spaces < 0)
                {
                    string err_msg = "";
                  char buffer[BufferSize];
                    const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
                    _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
                    if (a_cur_full_name)
                    {
                        sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
                        err_msg.append(buffer);
                        err_msg.append("\n");
                    }
                    sprintf(buffer, getMsg(37), total_str.c_str(), a_cell_size);
                    err_msg.append(buffer);
                    err_msg.append("\n");
                    myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
                    a_ok = false;
                }
                else
                {
                    cell += a_cell_size;
                    a_cur_cell = cell;
                }
            }
        }
    }
    break;
    default:
        break;
    }
    //
    return a_ok ? a_cur_cell : NULL;
}

const char *MECIDataReader::readCell_VALUE(const char                   *cell,
                                           const PseudoFileFormatCell_t *cell_format_p,
                                           IMECPreObject                *object_p,
                                           void                         *model_p,
                                           const PseudoDescriptor_t     *descr_p,
                                           int                           ind,
                                           bool                          is_free_size_format, 
                                           int                           ikeyword)
{
    const IDescriptor* a_descr_p = (const MvDescriptor_t*)descr_p;
    const ff_cell_t* a_cell_format_p = (const ff_cell_t*)cell_format_p;
  void* ent_param_ptr = nullptr;
    //
    const char* a_cur_cell = cell;
    //
    bool a_ok = true, has_empty_field = true;
    bool is_param_cell = false;
    string param_str = "";
    bool is_parameter_negated = false;
    /* This is the original code of the CELL_VALUE block in the switch in readCell(),
     * except for flagged modifications (CS#787#28_06_06) */
    const char* a_cell_fmt = NULL;
    int   a_cell_ikw = END_ARGS;
    MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_FORMAT, &a_cell_fmt, CELL_IKEYWORD, &a_cell_ikw, END_ARGS);

    if (END_ARGS != ikeyword) a_cell_ikw = ikeyword; /* use ikeyword, if given (CS#787#28_06_06) */
    //
    const  char* a_cell_skw=nullptr;
    a_descr_p->getSKeyword(a_cell_ikw, &a_cell_skw);

    value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);
    //
    switch (a_cell_vtype) {
    case VTYPE_BOOL:
    case VTYPE_INT:
    {
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);
        int a_value = 0;

        is_param_cell = isParameterCell(a_cur_cell, a_cell_size, param_str, &is_parameter_negated);
              bool is_text_param_replaced = is_param_cell ?
                  replaceTextParameter(a_cur_cell,a_cell_size,param_str,model_p,object_p->GetFileIndex(), &a_ok) :
                  false;
              if(is_param_cell && !is_text_param_replaced && a_ok)
        {
            if (model_p != NULL)
            {
                MECIModelFactory* nc_model_p = (MECIModelFactory*)model_p;
                a_value = nc_model_p->GetIntParameter(param_str.c_str(), object_p->GetFileIndex());
                if (is_parameter_negated)
                {
                    a_value = -1 * a_value;
                }
            }
            a_ok = true;
        }
              else if(a_ok)
        {
            a_value = scanInt(a_cur_cell, a_cell_fmt, a_cell_size, &a_ok,
                true, &has_empty_field);
        }
        if (a_ok) {
            a_cur_cell += a_cell_size;
            if (ind < 0 && (!has_empty_field || is_param_cell))
            {
                object_p->AddIntValue(a_cell_skw, a_value);
            }
            else if (!has_empty_field || is_param_cell)
            {
                int a_cell_ind = -1;
                attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
                if (ATYPE_STATIC_ARRAY == a_atype || ATYPE_DYNAMIC_ARRAY == a_atype)
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_cell_skw);
                    if (a_cell_ind < 0)
                        a_cell_ind = loc_reserve_array(a_cell_ikw, a_cell_skw, IMECPreObject::VTY_INT, a_descr_p, object_p);

                    object_p->SetIntValue(a_cell_ind, ind, a_value);
                }
                else
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_cell_skw);
                    if (a_cell_ind < 0)
                        object_p->AddIntValue(a_cell_skw, a_value);
                    else
                        object_p->SetIntValue(a_cell_ind, a_value);
                }
            }
        }
        if (is_param_cell)
        {
            object_p->SetParameterName(param_str.c_str(), a_cell_skw, ind, is_parameter_negated);
        }
    }
    break;
    case VTYPE_UINT:
    {
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);

        int a_value = 0;

        is_param_cell = isParameterCell(a_cur_cell, a_cell_size, param_str, &is_parameter_negated);
      bool is_text_param_replaced = is_param_cell ?
          replaceTextParameter(a_cur_cell,a_cell_size,param_str,model_p,object_p->GetFileIndex(), &a_ok) :
          false;
      if(is_param_cell && !is_text_param_replaced && a_ok)
        {
            if (model_p != NULL)
            {
                MECIModelFactory* nc_model_p = (MECIModelFactory*)model_p;
                a_value = nc_model_p->GetIntParameter(param_str.c_str(), object_p->GetFileIndex());
                if (is_parameter_negated)
                {
                    a_value = -1 * a_value;
                }
            }
            //a_value = 0;  //add dummy value          
            a_ok = true;
        } 
      else if(a_ok)
        {
            a_value = (int)scanUInt(a_cur_cell, a_cell_fmt, a_cell_size, &a_ok,
                true); 
        }
        if (a_ok) {
            a_cur_cell += a_cell_size;
            
            if (ind < 0)
            {
                object_p->AddUIntValue(a_cell_skw, (unsigned int)a_value);
            }
            //else      object_p->AddIntValue(a_cell_skw.c_str(),ind,a_value);
            else
            {
                // int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_cell_skw.c_str());
                //  if (a_cell_ind < 0) a_cell_ind = loc_reserve_array(a_cell_ikw, a_cell_skw, IMECPreObject::VTY_UINT, a_descr_p, object_p);
                //  object_p->SetUIntValue(a_cell_ind, ind, (unsigned int)a_value);

                int a_cell_ind = -1;
                attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
                if (ATYPE_STATIC_ARRAY == a_atype || ATYPE_DYNAMIC_ARRAY == a_atype)
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_cell_skw);
                    if (a_cell_ind < 0)
                        a_cell_ind = loc_reserve_array(a_cell_ikw, a_cell_skw, IMECPreObject::VTY_UINT, a_descr_p, object_p);

                    object_p->SetUIntValue(a_cell_ind, ind, a_value);
                }
                else
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_UINT, a_cell_skw);
                    if (a_cell_ind < 0)
                        object_p->AddUIntValue(a_cell_skw, a_value);
                    else
                        object_p->SetUIntValue(a_cell_ind, a_value);
                }
            }
        }
        if (is_param_cell)
        {
            object_p->SetParameterName(param_str.c_str(), a_cell_skw, ind, is_parameter_negated);
        }
    }
    break;
    case VTYPE_FLOAT:
    {
        
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);
        double a_value = 0;  

        if (is_free_size_format && !a_cell_size)
            a_cell_size = GetCellFreeSize(a_cur_cell);

        is_param_cell = isParameterCell(a_cur_cell, a_cell_size, param_str, &is_parameter_negated);
      bool is_text_param_replaced = is_param_cell ?
          replaceTextParameter(a_cur_cell,a_cell_size,param_str,model_p,object_p->GetFileIndex(), &a_ok) :
          false;
      if(is_param_cell && !is_text_param_replaced && a_ok)
        {
            if (model_p != NULL)
            {
                MECIModelFactory* nc_model_p = (MECIModelFactory*)model_p;
          a_value = nc_model_p->GetFloatParameter(param_str.c_str(), object_p->GetFileIndex(), &ent_param_ptr);
                if (is_parameter_negated)
                {
                    a_value = -1 * a_value;
                }
            }
            //a_value = 0;  //add dummy value          
            a_ok = true;
        } 
      else if(a_ok)
        {
            a_value = scanDouble(a_cur_cell, a_cell_fmt, a_cell_size, &a_ok, &has_empty_field);
        }
        if (a_ok)
        {
            a_cur_cell += a_cell_size;
            
            if (ind < 0 && (!has_empty_field || is_param_cell))
            {
                object_p->AddFloatValue(a_cell_skw, a_value);
            }
            else if (!has_empty_field || is_param_cell)
            {
                // int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_cell_skw.c_str());
                // if (a_cell_ind < 0) a_cell_ind = loc_reserve_array(a_cell_ikw, a_cell_skw, IMECPreObject::VTY_FLOAT, a_descr_p, object_p);
                // object_p->SetFloatValue(a_cell_ind, ind, a_value);
                //
                int a_cell_ind = -1;
                attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
                if (ATYPE_STATIC_ARRAY == a_atype || ATYPE_DYNAMIC_ARRAY == a_atype)
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_cell_skw);
                    if (a_cell_ind < 0)
                        a_cell_ind = loc_reserve_array(a_cell_ikw, a_cell_skw, IMECPreObject::VTY_FLOAT, a_descr_p, object_p);

                    object_p->SetFloatValue(a_cell_ind, ind, a_value);
                }
                else
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_FLOAT, a_cell_skw);
                    if (a_cell_ind < 0)
                        object_p->AddFloatValue(a_cell_skw, a_value);
                    else
                        object_p->SetFloatValue(a_cell_ind, a_value);
                }
            }
        }
        if (is_param_cell)
        {
         object_p->SetParameterName(param_str.c_str(), a_cell_skw, ind, is_parameter_negated, ent_param_ptr);
        }
    }
    break;
    case VTYPE_STRING:
    {
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);
        static char a_value[500];
        
        MvDataFeatureType_e feature_type = DFT_UNKNOWN;
        const MvDataFeature_t* data_ftr_p = a_descr_p->getIkeywordDataFeature(DOM_COMMON, a_cell_ikw);
        if (data_ftr_p)
        {
            feature_type = data_ftr_p->getType();
        }
            is_param_cell = isParameterCell(a_cur_cell, a_cell_size, param_str);
      bool is_text_param_replaced = is_param_cell ?
          replaceTextParameter(a_cur_cell,a_cell_size,param_str,model_p,object_p->GetFileIndex(), &a_ok) :
          false;

      if(is_param_cell && !is_text_param_replaced)
      {
          // sometimes string cells are "misused" to read dummy data which is not (yet) needed.
          // If this data is parametrized with a non-string parameter, we must not consider this as
          // an issue, so we just ignore the parameter and keep the cell as it is
          assert(nullptr != model_p);
          MECIModelFactory* nc_model_p = (MECIModelFactory *)model_p;
          IParameter::Type param_type = 
              nc_model_p->GetParameterValueType(param_str.c_str(), object_p->GetFileIndex());
          if(IParameter::TYPE_STRING != param_type && IParameter::TYPE_UNKNOWN != param_type)
          {
              is_param_cell = false;
              a_ok = true;
          }
      }

      if(is_param_cell && !is_text_param_replaced && a_ok && myCanReadAgainFlag)
      {
          // replaceTextParameter should have replaced the parameter. If it hasn't set a_ok to false,
          // it hasn't detected the error, so we print a warning here (but go on) or trigger read-again
          if(getDoReadAgainFlag())
        {
              // if trying to read again, but still not finding: Warning
              std::string err_msg = "";
              char buffer[BufferSize];
              const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
              _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
              if(a_cur_full_name)
            {
                  sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
                  err_msg.append(buffer);
                  err_msg.append("\n");
            }
              err_msg.append(getMsg(39));
              err_msg.append("\n");
              myReadContext_p->displayMessage(myReadContext_p->MSG_WARNING, err_msg.c_str());
        }
        else
        {
              setDoReadAgainFlag(true); // toggle flag for reading again
              a_ok = false;
          }
      }
      if(a_ok)
      {      
            scanString(a_cur_cell, a_cell_fmt, a_cell_size, a_value, &a_ok);
        }

        if (a_ok) {
            a_cur_cell += a_cell_size;
            if (ind < 0)
            {
                if (strncmp(a_cell_skw, "TITLE", 5) == 0)
                {
                    myReadContext_p->killBlanksEnd(a_value);
                    object_p->SetTitle(a_value);
                }
                else
                {
                    char* a_value_p = a_value;
                    if (a_cell_size < myLineLength)
                        a_value_p = (char*)myReadContext_p->killBlanksBegin(a_value);
                    object_p->AddStringValue(a_cell_skw, a_value_p);
                }
            }
            else
            {
                char* a_value_p = a_value;
                if (a_cell_size < myLineLength)
                    a_value_p = (char*)myReadContext_p->killBlanksBegin(a_value);
                int a_cell_ind = -1;
                attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
                if (ATYPE_STATIC_ARRAY == a_atype || ATYPE_DYNAMIC_ARRAY == a_atype)
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_cell_skw);
                    if (a_cell_ind < 0)
                        a_cell_ind = loc_reserve_array(a_cell_ikw, a_cell_skw, IMECPreObject::VTY_STRING, a_descr_p, object_p);

                    object_p->SetStringValue(a_cell_ind, ind, a_value_p);
                }
                else
                {
                    a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, a_cell_skw);
                    if (a_cell_ind < 0)
                        object_p->AddStringValue(a_cell_skw, a_value_p);
                    else
                        object_p->SetStringValue(a_cell_ind, a_value_p);
                }
            }
        }
        if (is_param_cell)
        {
            object_p->SetParameterName(param_str.c_str(), a_cell_skw, ind, is_parameter_negated);
        }
    }
    break;
    case VTYPE_OBJECT:
    {
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);

        MYOBJ_INT a_id = 0;
     
        is_param_cell = isParameterCell(a_cur_cell, a_cell_size, param_str, &is_parameter_negated);
      bool is_text_param_replaced = is_param_cell ?
          replaceTextParameter(a_cur_cell,a_cell_size,param_str,model_p,object_p->GetFileIndex(), &a_ok) :
          false;
      if(is_param_cell && !is_text_param_replaced && a_ok)
        {
            if (model_p != NULL)
            {
                MECIModelFactory* nc_model_p = (MECIModelFactory*)model_p;
                a_id = nc_model_p->GetIntParameter(param_str.c_str(), object_p->GetFileIndex());
                if (is_parameter_negated)
                {
                    a_id = -1 * a_id;
                }
            }
            //a_id = 0;
            a_ok = true;
        }
      else if(a_ok)
        {
            a_id = scanObject(a_cur_cell, a_cell_fmt, a_cell_size, &a_ok, true, &has_empty_field);
        }
        //
        if (a_ok) {
            a_cur_cell += a_cell_size;
            if ((a_id >= 0 && !has_empty_field) || is_param_cell)
            {
                object_type_e  a_cell_otype = a_descr_p->getObjectType(a_cell_ikw);
                const string& a_cell_otype_str = MV_get_type(a_cell_otype);
                bool           a_is_treated = false;//model_p->IsTreated(a_cell_otype_str.c_str());
                //
                if (a_is_treated) {

	      } else {
                    
                    if (ind < 0)
                    {
                        object_p->AddObjectValue(a_cell_skw, a_cell_otype_str.c_str(), a_id);
                    }
                    //else      object_p->AddObjectValue(a_cell_skw,ind,a_cell_otype_str.c_str(),a_id);
                    else
                    {
                        // int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_cell_skw.c_str());
                        // if (a_cell_ind < 0) a_cell_ind = loc_reserve_array(a_cell_ikw, a_cell_skw, IMECPreObject::VTY_OBJECT, a_descr_p, object_p);
                        // object_p->SetObjectValue(a_cell_ind, ind, a_cell_otype_str.c_str(), a_id);


                        int a_cell_ind = -1;
                        attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
                        if (ATYPE_STATIC_ARRAY == a_atype || ATYPE_DYNAMIC_ARRAY == a_atype)
                        {
                            a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_cell_skw);
                            if (a_cell_ind < 0)
                                a_cell_ind = loc_reserve_array(a_cell_ikw, a_cell_skw, IMECPreObject::VTY_OBJECT, a_descr_p, object_p);

                            object_p->SetObjectValue(a_cell_ind, ind, a_cell_otype_str.c_str(), a_id);
                        }
                        else
                        {
                            a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, a_cell_skw);
                            if (a_cell_ind < 0)
                                object_p->AddObjectValue(a_cell_skw, a_cell_otype_str.c_str(), a_id);
                            else
                                object_p->SetObjectValue(a_cell_ind, a_cell_otype_str.c_str(), a_id);
                        }
                    }
                }
            }
        }
        if (is_param_cell)
        {
          object_p->SetParameterName(param_str.c_str(), a_cell_skw, ind, is_parameter_negated, ent_param_ptr);
        }
    }
    break;
    default:
        break;
      }

      return a_ok ? a_cur_cell : NULL; 
}

/*This function reads an id of a keyword and updates to preobject because in 
 FORMAT block it is defined as _ID means it has id*/
const char *MECIDataReader::readCell_ID(const char                      *cell,
                                        const PseudoFileFormatCell_t    *cell_format_p,
                                        IMECPreObject                   *object_p,
                                        void                            *model_p,
                                        bool                          is_free_size_format
                                        )
{
    const ff_cell_t     *a_cell_format_p = (const ff_cell_t *)cell_format_p;
    const char          *a_cur_cell  = cell;
    bool                 a_ok=true;
    const char          *a_cell_fmt=NULL;
    string               param_str;
     
    MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_FORMAT,&a_cell_fmt,END_ARGS);
    int a_cell_size = 0;
    a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);
    //Check if the id field is parameter
    bool is_param_cell = isParameterCell(a_cur_cell, a_cell_size, param_str);
    MYOBJ_INT a_value = 0;
    if (is_param_cell)
    {
        a_ok = true;
        if (model_p != NULL)
        {
            
            string id_param_name = object_p->GetParameterIdName();
            if (id_param_name != param_str)
                object_p->SetParameterIdName(param_str.c_str());

            MECIModelFactory* nc_model_p = (MECIModelFactory*)model_p;
            a_value = nc_model_p->GetIntParameter(param_str.c_str(), object_p->GetFileIndex());
        }
    }
    else
    {
        a_value = scanInt(a_cur_cell, a_cell_fmt, a_cell_size, &a_ok, true);

        if (!a_ok && mySyntaxInfos_p->IsSupportedForNamedEntity())
        {
            static char a_value_str[500];
            scanString(a_cur_cell, a_cell_fmt, a_cell_size, a_value_str, &a_ok);
            if (a_ok)
                object_p->SetTitle(a_value_str);
        }
    }

    if (a_ok)
    {
        object_p->SetId(a_value);
        a_cur_cell += a_cell_size;
    }
    return a_ok ? a_cur_cell : NULL;
}


const char *MECIDataReader::readCell_COMMENT(const char                   *cell,
                                             const PseudoFileFormatCell_t *cell_format_p,
                                             IMECPreObject                *object_p,
                                             bool                          is_free_size_format,
                                             int                           card_type)
{
  const ff_cell_t      *a_cell_format_p = (const ff_cell_t *)cell_format_p;
  //
  const char *a_cell_string=NULL;
  unsigned int a_cell_size = 0;
  MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_STRING,&a_cell_string,END_ARGS);
  if (NULL != a_cell_string) {
      if(is_free_size_format)
      {
          /*a_cell_string will have the whitespaces as per card format length
          However we want it same as cell*/
          a_cell_string = cell;
          a_cell_size = GetCellFreeSize(cell);
      }
      else
      {
         a_cell_size = (int) strlen (a_cell_string);
      }
      if (0 == strncmp (a_cell_string, cell, a_cell_size)) {
          return cell + a_cell_size;
      }
      else
      {
          if (!is_free_size_format && (ff_card_type_e)card_type == CARD_SINGLE)
          {
              char buff[256];
              strncpy(buff, cell, a_cell_size);
              buff[a_cell_size] = '\0';
              string substr_key("");
              if (object_p)
              {
                  const char *a_fulltype = object_p->GetKernelFullType();
                  if (a_fulltype)
                  {
                      string sfulltype(a_fulltype);
                      substr_key = sfulltype.substr(sfulltype.rfind("/") + 1);
                  }
              }
              char *a_buff = (char *)myReadContext_p->killBlanksBegin(buff);
              myReadContext_p->killBlanksEnd(a_buff);
              std::string war_msg = "";
              char buffer[BufferSize];
              const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
              _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
              if (a_cur_full_name)
              {
                  sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
              war_msg.append(buffer);
                  war_msg.append("\n");
              }
              sprintf(buffer, getMsg(36), substr_key.c_str(), a_buff);
              war_msg.append(buffer);
              myReadContext_p->displayMessage(myReadContext_p->MSG_WARNING, war_msg.c_str());
          }
      }
      // if the comment is only whitespace, it is ok, too:
      // If the comment contains separators, it is ok, too.
      unsigned int i =0; 
      for (i=0; i<a_cell_size; i++) {
          if (!IsWhitespace(a_cell_string[i]) &&
              !IsCellSeparator(a_cell_string[i]))
          {
              break;
          }
      }
      if (i==a_cell_size) { // loop has not been left by break, so there was only whitspace
          return cell + a_cell_size;
      }
  }
  return NULL;
}


const char *MECIDataReader::readCell_SCALAR_OR_OBJECT(const char                   *cell,
                                                      const PseudoFileFormatCell_t *cell_format_p,
                                                      IMECPreObject                 *object_p,
                                                      void       *model_p,
                                                      const PseudoDescriptor_t     *descr_p,
                                                      int                           ind,
                                                      bool                          is_free_size_format) 
{
    const IDescriptor *a_descr_p       = (const MvDescriptor_t *)descr_p;
    const ff_cell_t      *a_cell_format_p = (const ff_cell_t *)cell_format_p;
    //
    const char     *a_cur_cell  = cell;
    //
    bool a_ok=true,has_empty_field=true;
    bool is_param_cell = false;
    bool is_parameter_negated = false;
    string param_str;

  
    /* This is the original code of the CELL_SCALAR_OR_OBJECT block in the switch in readCell(),
     * except for flagged modifications (CS#787#28_06_06) */

    // get info about format and object
    const char *a_cell_fmt=NULL;
    int   a_scalar_ikw=END_ARGS, a_object_ikw=END_ARGS;
    MCDS_get_ff_cell_attributes(a_cell_format_p,
                                CELL_FORMAT,    &a_cell_fmt,
                                CELL_SCALAR_IKW,&a_scalar_ikw,
                                CELL_OBJECT_IKW,&a_object_ikw, 
                                END_ARGS);
    value_type_e a_scalar_vtype = a_descr_p->getValueType(a_scalar_ikw);
    string       a_scalar_skw   = a_descr_p->getSKeyword(a_scalar_ikw);
    string       a_object_skw   = a_descr_p->getSKeyword(a_object_ikw); 
    // read the value, put scalar into the object, keep id in mind (if there is one)
    MYOBJ_INT a_id = 0;  // Revisit if MYOBJ_INT type is changed and see if SCALAR_OR_OBJECT reading needs any changes to be done.
    switch(a_scalar_vtype) {
    case VTYPE_INT:
    {
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);
        int a_value = 0;
        is_param_cell = isParameterCell(a_cur_cell,a_cell_size, param_str, &is_parameter_negated);
        if(is_param_cell)
        { 
            if (model_p != NULL)
            {
            MECIModelFactory* nc_model_p = (MECIModelFactory *)model_p;
            IParameter::Type value_type = nc_model_p->GetParameterValueType(param_str.c_str(), object_p->GetFileIndex());
            if (value_type == IParameter::TYPE_INTEGER)
                a_value = nc_model_p->GetIntParameter(param_str.c_str(), object_p->GetFileIndex());  /*If UINT parameter is supported in future, this code needs to be modified*/
            else
                a_value = (int)nc_model_p->GetFloatParameter(param_str.c_str(), object_p->GetFileIndex());
            if(is_parameter_negated)
            {
                a_value = -1*a_value;
            }
        }
        }
        else
        {
            a_value = scanInt(a_cur_cell, a_cell_fmt, a_cell_size, &a_ok, true, &has_empty_field);
        }
        if(a_ok) {
            a_cur_cell+=a_cell_size;
            if(ind<0 && (!has_empty_field || is_param_cell)) object_p->AddIntValue(a_scalar_skw.c_str(),a_value);
            else if(!has_empty_field || is_param_cell)
            {
                int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,a_scalar_skw);
                if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_scalar_ikw,a_scalar_skw,IMECPreObject::VTY_INT,a_descr_p,object_p);
                object_p->AddIntValue(a_scalar_skw.c_str(),ind,a_value);
            }      
            // this is Dyna specific: if the value is negative, its abs value is the ID of an object
            if (0 > a_value) a_id = abs(a_value);
        }
    }
    break;
    case VTYPE_UINT:
    {
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);

        unsigned int a_value = 0;
        int a_ivalue = 0;
        is_param_cell = isParameterCell(a_cur_cell,a_cell_size, param_str, &is_parameter_negated);
        if(is_param_cell)
        { 
            if (model_p != NULL)
            {
            MECIModelFactory* nc_model_p = (MECIModelFactory *)model_p;
            IParameter::Type value_type = nc_model_p->GetParameterValueType(param_str.c_str(), object_p->GetFileIndex());
            if(value_type == IParameter::TYPE_INTEGER)
                a_value = nc_model_p->GetIntParameter(param_str.c_str(), object_p->GetFileIndex());
            else
                a_value = (int)nc_model_p->GetFloatParameter(param_str.c_str(), object_p->GetFileIndex());
            if(is_parameter_negated)
            {
                a_value = -1*a_value;
            }
        }
		}
        else
        {
            a_ivalue = scanInt(a_cur_cell, a_cell_fmt, a_cell_size, &a_ok, true, &has_empty_field);
        }
        if(a_ok) {
            a_cur_cell+=a_cell_size;
            if (ind < 0) object_p->AddUIntValue(a_scalar_skw.c_str(), (unsigned int)a_ivalue);
            else      
            {
                int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_UINT,a_scalar_skw);
                if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_scalar_ikw,a_scalar_skw,IMECPreObject::VTY_UINT,a_descr_p,object_p);
                object_p->AddUIntValue(a_scalar_skw.c_str(), ind, (unsigned int)a_ivalue);
            }      
            // this is Dyna specific: if the value is negative, its abs value is the ID of an object
            if (0 > a_value) a_id = a_value;
        }
    }
    break;
    case VTYPE_FLOAT:
    {
        int a_cell_size = 0;
        a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);
        double a_value = 0.0;
        is_param_cell = isParameterCell(a_cur_cell,a_cell_size, param_str, &is_parameter_negated);
        
        if(is_param_cell)
        {  
            if (model_p != NULL)
            {
            MECIModelFactory* nc_model_p = (MECIModelFactory *)model_p;
            IParameter::Type value_type = nc_model_p->GetParameterValueType(param_str.c_str(), object_p->GetFileIndex());
            if (value_type == IParameter::TYPE_INTEGER)
                a_value = nc_model_p->GetIntParameter(param_str.c_str(), object_p->GetFileIndex());  
            else
                a_value = nc_model_p->GetFloatParameter(param_str.c_str(), object_p->GetFileIndex());
            if(is_parameter_negated)
            {
                a_value = -1*a_value;
        }
        }
		}
        else
        {
            a_value=scanDouble(a_cur_cell,a_cell_fmt,a_cell_size,&a_ok,&has_empty_field);
        }
        
        if(a_ok) {
            a_cur_cell+=a_cell_size;
            if(ind<0 && (!has_empty_field || is_param_cell)) object_p->AddFloatValue(a_scalar_skw.c_str(),a_value);
            else if(!has_empty_field || is_param_cell)
            {
                int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_FLOAT,a_scalar_skw);
                if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_scalar_ikw,a_scalar_skw,IMECPreObject::VTY_FLOAT,a_descr_p,object_p);
                object_p->AddFloatValue(a_scalar_skw.c_str(),ind,a_value);
            }
            // this is Dyna specific: if the value is negative, its abs value is the ID of an object
            if (0. > a_value) a_id= (MYOBJ_INT)abs(a_value);
        }
    }
    break ;
    case VTYPE_STRING:
        {
            int a_cell_size = 0;
            a_cell_fmt = GetFormatSize(a_cell_fmt, is_free_size_format, a_cell_size);// loc_get_fmt_size(a_cell_fmt, is_free_size_format);
            static char a_value[500];
            scanString(a_cur_cell,a_cell_fmt,a_cell_size,a_value,&a_ok);
             if(a_ok) 
             {
                a_cur_cell+=a_cell_size;
                if(ind<0) object_p->AddStringValue(a_scalar_skw.c_str(),a_value);     
                else      
                {
                    int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_STRING,a_scalar_skw);
                    if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_scalar_ikw,a_scalar_skw,IMECPreObject::VTY_STRING,a_descr_p,object_p);
                    object_p->AddStringValue(a_scalar_skw.c_str(),ind,a_value);
                }                       
                /* either we have a string or an id then we need to try to read the id 
                  * in the string a_value
                  */
                sscanf(a_value,"%u",&a_id) ;
            }
        }
            break;
    default:
        break;
    }

    if(is_param_cell)
    {            
        if (0 >= a_id) 
        {
            // it is the scalar, so set parametername with scalar skeyword.
             object_p->SetParameterName(param_str.c_str(),a_scalar_skw.c_str(),ind, is_parameter_negated);
        }
        else
        {
            //it is the object,so set parameter name with object skeyword
            object_p->SetParameterName(param_str.c_str(), a_object_skw.c_str(), ind, is_parameter_negated);
        }
    }   
    // put flag and id in object_p
    
    readCell_SCALAR_OR_OBJECT_setObject (a_id, cell_format_p, object_p, model_p, descr_p, ind);
    
          
    return a_ok ? a_cur_cell : NULL; 
}

// put flag and id in object_p

void MECIDataReader::readCell_SCALAR_OR_OBJECT_setObject(MYOBJ_INT                           id,
                                                         const PseudoFileFormatCell_t *cell_format_p,
                                                         IMECPreObject                 *object_p,
                                                         void       *model_p,
                                                         const PseudoDescriptor_t     *descr_p,
                                                         int                           ind)
{
    const IDescriptor *a_descr_p       = (const MvDescriptor_t *)descr_p;
    const ff_cell_t      *a_cell_format_p = (const ff_cell_t *)cell_format_p;
  
    int   a_flag_ikw=END_ARGS,a_object_ikw=END_ARGS,a_scalar_ikw=END_ARGS;
    MCDS_get_ff_cell_attributes(a_cell_format_p,
                                CELL_FLAG_IKW,  &a_flag_ikw,
                                CELL_OBJECT_IKW,&a_object_ikw,
                                CELL_SCALAR_IKW,&a_scalar_ikw,
                                END_ARGS);
    string       a_flag_skw     = a_descr_p->getSKeyword(a_flag_ikw);
    string       a_object_skw   = a_descr_p->getSKeyword(a_object_ikw);
    value_type_e a_cell_vtype = a_descr_p->getValueType(a_flag_ikw);
    /* This is the original code of the CELL_SCALAR_OR_OBJECT block in the switch in readCell(),
     * except for a_id which is renamed to id (CS#787#28_06_06) */
    if (0 >= id) {
        // it is the scalar, so set the "is_object" flag to false (0)
        if(ind<0) 
        {
           if(a_cell_vtype == VTYPE_BOOL)
              object_p->AddBoolValue(a_flag_skw.c_str(),false);
           else
              object_p->AddIntValue(a_flag_skw.c_str(),0);
        }
        else      
        {
            if(a_cell_vtype == VTYPE_BOOL)
            {
                int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_BOOL,a_flag_skw);
                if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_flag_ikw,a_flag_skw,IMECPreObject::VTY_BOOL,a_descr_p,object_p);
                object_p->AddBoolValue(a_flag_skw.c_str(),ind,false);
            }
            else
            {
                int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,a_flag_skw);
                if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_flag_ikw,a_flag_skw,IMECPreObject::VTY_INT,a_descr_p,object_p);
                object_p->AddIntValue(a_flag_skw.c_str(),ind,0);
            }
        }
        // Add cell to update manager, because the "is_object" flag might be set to true later on
        if (myUpdateManager_p) myUpdateManager_p->addCell (a_cell_format_p, ind); 
    } else {
        // it is the object, so set the "is_object" flag to true (1)
        if(ind<0) 
        {
            if(a_cell_vtype == VTYPE_BOOL)
                object_p->AddBoolValue(a_flag_skw.c_str(),true);
            else
                object_p->AddIntValue(a_flag_skw.c_str(),1);
        }
        else
        {
            if(a_cell_vtype == VTYPE_BOOL)
            {
                int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_BOOL,a_flag_skw);
                if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_flag_ikw,a_flag_skw,IMECPreObject::VTY_BOOL,a_descr_p,object_p);
                object_p->AddBoolValue(a_flag_skw.c_str(),ind,true);
            }
            else
            {
                int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,a_flag_skw);
                if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_flag_ikw,a_flag_skw,IMECPreObject::VTY_INT,a_descr_p,object_p);
                object_p->AddIntValue(a_flag_skw.c_str(),ind,1);
            }
        } 
        // put id in object_p
        object_type_e  a_cell_otype     = a_descr_p->getObjectType(a_object_ikw);
        const string  &a_cell_otype_str = MV_get_type(a_cell_otype); 
        bool           a_is_treated     = false;//model_p->IsTreated(a_cell_otype_str.c_str());
        //
        if(a_is_treated) {

        } else {
            if(ind<0) object_p->AddObjectValue(a_object_skw.c_str(),a_cell_otype_str.c_str(),id);
            else
            {
                int a_cell_ind = object_p->GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_OBJECT,a_object_skw);
                if(a_cell_ind<0) a_cell_ind=loc_reserve_array(a_object_ikw,a_object_skw,IMECPreObject::VTY_OBJECT,a_descr_p,object_p);
                object_p->AddObjectValue(a_object_skw.c_str(),ind,a_cell_otype_str.c_str(),id);
            }
        }

        // Reset the scalar to 0 (CS#S_195883#15_06_10)
        value_type_e a_scalar_vtype = a_descr_p->getValueType(a_scalar_ikw);
        string       a_scalar_skw   = a_descr_p->getSKeyword(a_scalar_ikw);
        switch(a_scalar_vtype) {
            case VTYPE_INT:
                if(ind<0) object_p->AddIntValue(a_scalar_skw.c_str(),0);
                else      object_p->AddIntValue(a_scalar_skw.c_str(),ind,0);
                break;
            case VTYPE_UINT:
                if(ind<0) object_p->AddUIntValue(a_scalar_skw.c_str(),0);
                else      object_p->AddUIntValue(a_scalar_skw.c_str(),ind,0);
                break;
            case VTYPE_FLOAT:
                if(ind<0) object_p->AddFloatValue(a_scalar_skw.c_str(),0);
                else      object_p->AddFloatValue(a_scalar_skw.c_str(),ind,0);
                break;
            default:
                break;
        }
        
    }
}


int MECIDataReader::scanInt(const char *cell,const char *format,
                            int &nb_chars, 
                            bool *a_ok_p,
                            bool do_check_format, bool *is_empty_field) 
    const
{
  if (do_check_format) {
      int length_format = (int)strlen (format);
      if (1 > length_format) return 0;
      if ('d' != format[length_format-1]) {
          // the format is not an integer format, so we try to read a float
          return (int) scanDouble(cell,format,nb_chars,a_ok_p, is_empty_field);
      }
  }

  int a_result  = 0, a_nb_read = 0;
  if (nb_chars>0) // This means that it is a fix-format cell
  {
      char *a_cell_end_p = const_cast<char *>(cell+nb_chars);
      char  a_cell_end   = (*a_cell_end_p);
      *a_cell_end_p='\0';
      a_nb_read = sscanf(cell,format,&a_result);
      *a_cell_end_p=a_cell_end;
  }
  else // This means that it is a free-size-format cell
  {
      //use-case of ,, when no data between two separater
      if (*cell == ',')
      {
          nb_chars = GetCellFreeSize(cell);
          a_nb_read = -2;
      }
      else
      {
      a_nb_read = sscanf(cell,"%d",&a_result);
      if(a_nb_read!=0){
          nb_chars = GetCellFreeSize (cell);
          /*Added to check whether rest of the fields is having spaces*/
          char *a_cell_end = const_cast<char *>(cell+nb_chars);
          int indx = 0;
          size_t length_cell =  strlen (a_cell_end);          
          while ((indx<length_cell) && IsWhitespace(a_cell_end[indx])) indx++;
          if(length_cell == indx)
          {
              nb_chars = nb_chars + indx; //add rest of the length or white spaces
          }
      }
  }
  }

  
  // We no longer consider an unreadable field as an error which stops reading.
  // Instead, we put a message and consider as 0.
  if(a_nb_read==0) {
      std::string err_msg = "";
      char buffer[BufferSize];
      const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
      _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
      if (a_cur_full_name)
      {
          sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
      err_msg.append(buffer);
          err_msg.append("\n");
      }
      if (nb_chars > 0)
          sprintf(buffer, getMsg(24), nb_chars);
      else
          sprintf(buffer, getMsg(25));
      err_msg.append(buffer);
      err_msg.append("(");
      if (nb_chars > 0 && nb_chars <= (int)strlen(cell))
      {
          string a_cell(cell, nb_chars);
          err_msg.append(a_cell);
      }
      else
      err_msg.append(cell);
      err_msg.append(")");
      //
      myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
      }

  // if(a_ok_p!=NULL) *a_ok_p=(a_nb_read!=0);
  if(a_ok_p!=NULL) *a_ok_p=true;
  

  if(is_empty_field!=NULL && (a_nb_read == -1 || a_nb_read == -2))
      *is_empty_field = true;
  else if(is_empty_field!=NULL)
      *is_empty_field = false;

  return a_result;
}



unsigned int MECIDataReader::scanUInt(const char *cell,const char *format,
                            int &nb_chars, 
                            bool *a_ok_p,
                            bool do_check_format) 
    const
{
  if (do_check_format) {
      int length_format = (int)strlen (format);
      if (1 > length_format) return 0;
      if ('u' != format[length_format-1]) {
          // the format is not an integer format, so we try to read a float
          return (int) scanDouble(cell,format,nb_chars,a_ok_p);
      }
  }
  
  unsigned int a_result  = 0, a_nb_read = 0;
  if (nb_chars>0) // This means that it is a fix-format cell
  {
      char *a_cell_end_p = const_cast<char *>(cell+nb_chars);
      char  a_cell_end   = (*a_cell_end_p);
      *a_cell_end_p='\0';
      a_nb_read = sscanf(cell,format,&a_result);
      *a_cell_end_p=a_cell_end;
  }
  else // This means that it is a free-size-format cell
  {
      //use-case of ,, when no data between two separater:
      if (*cell == ',')
      {
          nb_chars = GetCellFreeSize(cell);
          a_nb_read = -2;
      }
      else
      {
      a_nb_read = sscanf(cell,"%u",&a_result);
      if(a_nb_read!=0){
          nb_chars = GetCellFreeSize (cell);
          /*Added to check whether rest of the fields is having spaces*/
          char *a_cell_end = const_cast<char *>(cell+nb_chars);
          int indx = 0;
          size_t length_cell =  strlen (a_cell_end);          
          while ((indx<length_cell) && IsWhitespace(a_cell_end[indx])) indx++;
          if(length_cell == indx)
          {
              nb_chars = nb_chars + indx; //add rest of the length or white spaces
          }
      }
  }
  }

  
  // We no longer consider an unreadable field as an error which stops reading.
  // Instead, we put a message and consider as 0.
  if(a_nb_read==0) {
      std::string err_msg = "";
      char buffer[BufferSize];
      const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
      _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
      if (a_cur_full_name)
      {
          sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
      err_msg.append(buffer);
          err_msg.append("\n");
      }
      if (nb_chars > 0)
          sprintf(buffer, getMsg(24), nb_chars);
      else
          sprintf(buffer, getMsg(25));
      err_msg.append(buffer);
      err_msg.append("(");
      if (nb_chars > 0 && nb_chars <= (int)strlen(cell))
      {
          string a_cell(cell, nb_chars);
          err_msg.append(a_cell);
      }
      else
      err_msg.append(cell);
      err_msg.append(")");
      //
      myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
  }

  // if(a_ok_p!=NULL) *a_ok_p=(a_nb_read!=0);
  if(a_ok_p!=NULL) *a_ok_p=true;
  
  
  return a_result;
}

MYOBJ_INT MECIDataReader::scanObject(const char *cell, const char *format,
    int &nb_chars, 
    bool *a_ok_p,
    bool do_check_format, bool *is_empty_field) 
    const // Revisit if MYOBJ_INT type is changed and see if scanObject Method needs any changes to be done.
{
    if (do_check_format) {
        int length_format = (int)strlen(format);
        if (1 > length_format) return 0;
        if (!('d' == format[length_format - 1] || 'u' == format[length_format - 1])) {
            // the format is not an integer format, so we try to read a float
            return (int)scanDouble(cell, format, nb_chars, a_ok_p, is_empty_field);
        }
    }

    MYOBJ_INT a_result = 0, a_nb_read = 0;
    if (nb_chars>0) // This means that it is a fix-format cell
    {
        char *a_cell_end_p = const_cast<char *>(cell + nb_chars);
        char  a_cell_end = (*a_cell_end_p);
        *a_cell_end_p = '\0';
        a_nb_read = sscanf(cell, format, &a_result);
        *a_cell_end_p = a_cell_end;
    }
    else // This means that it is a free-size-format cell
    {
        //use-case of ,, when no data between two separater
        if (*cell == ',')
        {
            nb_chars = GetCellFreeSize(cell);
            a_nb_read = -2;
        }
        else
        {
        a_nb_read = sscanf(cell, "%u", &a_result);
        if (a_nb_read != 0) {
            nb_chars = GetCellFreeSize(cell);
            /*Added to check whether rest of the fields is having spaces*/
            char *a_cell_end = const_cast<char *>(cell + nb_chars);
            int indx = 0;
            size_t length_cell = strlen(a_cell_end);
            while ((indx<length_cell) && IsWhitespace(a_cell_end[indx])) indx++;
            if (length_cell == indx)
            {
                nb_chars = nb_chars + indx; //add rest of the length or white spaces
            }
        }
    }
    }
    
    // We no longer consider an unreadable field as an error which stops reading.
    // Instead, we put a message and consider as 0.
    if (a_nb_read == 0) {
        std::string err_msg = "";
        char buffer[BufferSize];
        const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
        _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
        if (a_cur_full_name)
        {
            sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
        err_msg.append(buffer);
            err_msg.append("\n");
        }
        if (nb_chars > 0)
            sprintf(buffer, getMsg(24), nb_chars);
        else
            sprintf(buffer, getMsg(25));
        err_msg.append(buffer);
        err_msg.append("(");
        if (nb_chars > 0 && nb_chars <= (int)strlen(cell))
        {
            string a_cell(cell, nb_chars);
            err_msg.append(a_cell);
        }
        else
        err_msg.append(cell);
        err_msg.append(")");
        //
        myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
        }
    else if (a_nb_read > 0 && a_result > INT_MAX)
    {
        int a_cell_length_local = GetCellFreeSize(cell);
        int a_sgn = 0;
        int i = 0;
        for (i = 0; i < a_cell_length_local; i++)
        {
            if (cell[i] == ' ')
                continue;
            else
            {
                if (cell[i] == '-')
                    a_sgn = -1;
                break;
            }
        }
        if (a_sgn == -1)
        {
            std::string err_msg = "";
            char buffer[BufferSize];
            const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
            _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
            if (a_cur_full_name)
            {
                sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
            err_msg.append(buffer);
                err_msg.append("\n");
            }
            sprintf(buffer, getMsg(34));
            err_msg.append(buffer);
            err_msg.append("(");
            if (nb_chars > 0 && nb_chars <= (int)strlen(cell))
            {
                string a_cell(cell, nb_chars);
                err_msg.append(a_cell);
            }
            else
            err_msg.append(cell);
            err_msg.append(")");
            //
            myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
        }
    }

    // if(a_ok_p!=NULL) *a_ok_p=(a_nb_read!=0);
    if (a_ok_p != NULL) *a_ok_p = true;
    

    if (is_empty_field != NULL && (a_nb_read == -1 || a_nb_read == -2))
        *is_empty_field = true;
    else if (is_empty_field != NULL)
        *is_empty_field = false;

    return a_result;
}



double MECIDataReader::scanDouble(const char* cell, const char* format,
    int& nb_chars, 
    bool* a_ok_p, bool* is_empty_field) const {
    double a_result = 0;
    int    a_nb_read;

    a_result = hc_scan_double(cell, format, nb_chars, &a_nb_read);

    
    // We no longer consider an unreadable field as an error which stops reading.
    // Instead, we put a message and consider as 0.
    if (a_nb_read == 0) {
        std::string err_msg = "";
      char buffer[BufferSize];
      const char* a_cur_full_name = myReadContext_p->getCurrentFullName();
      _HC_LONG  a_cur_line = myReadContext_p->getCurrentLine();
      if (a_cur_full_name)
      {
          sprintf(buffer, getMsg(0), a_cur_line, a_cur_full_name);
      err_msg.append(buffer);
          err_msg.append("\n");
      }
      if (nb_chars > 0)
          sprintf(buffer, getMsg(26), nb_chars);
      else
          sprintf(buffer, getMsg(27));
      err_msg.append(buffer);
      err_msg.append("(");
      if (nb_chars > 0 && nb_chars <= (int)strlen(cell))
      {
          string a_cell(cell, nb_chars);
          err_msg.append(a_cell);
      }
      else
      err_msg.append(cell);
      err_msg.append(")");
      myReadContext_p->displayMessage(myReadContext_p->MSG_ERROR, err_msg.c_str());
    }

    // if(a_ok_p!=NULL) *a_ok_p=(a_nb_read!=0);
    if (a_ok_p != NULL) *a_ok_p = true;
    //
    // Count number of read characters for "free size" cells (CS#792#11_07_06 beg)
    if ((nb_chars == 0) && (a_nb_read != 0)) {
        nb_chars = GetCellFreeSize(cell);
    }
    
    if (is_empty_field != NULL && (a_nb_read == -1 || a_nb_read == -2))
        *is_empty_field = true;
    else if (is_empty_field != NULL)
        *is_empty_field = false;

    return a_result;
}

char* MECIDataReader::scanString(const char* cell, const char* format,
    int& nb_chars, 
    char* value, bool* a_ok_p)const
{
    char* a_cell_end_p = const_cast<char*>(cell + nb_chars);
    char  a_cell_end = (*a_cell_end_p);
    //
    if (nb_chars > 0) { 
        *a_cell_end_p = '\0';
        //
        strcpy(value, cell);
        myReadContext_p->killBlanksEnd(value);
        
  } else {
        nb_chars = 0; // might be <0, but shouldn't
        int length_cell = (int)strlen(cell);

        /*to read entire line %-.200s*/
        if (format[1] == '-')
        {
            if (format[2] == '.')
                nb_chars = myLineLength;
        }
        else if (format[1] == '.')
            nb_chars = myLineLength;

        if (!nb_chars)
            while ((nb_chars < length_cell) && !IsCellSeparator(cell[nb_chars])) nb_chars++;

        strncpy(value, cell, nb_chars);
        value[nb_chars] = '\0';
        if (nb_chars < length_cell)
        {
            nb_chars++;
        }
    }
    //
    if (a_ok_p != NULL) *a_ok_p = true;

    if (nb_chars > 0)
        *a_cell_end_p = a_cell_end;
    //
    return value;
}

int MECIDataReader::GetCellFreeSize (const char *cell) const
{
    size_t nb_chars =0;
    size_t length_cell =  strlen (cell);
    
    // go to beginning of interpreted field
    while ((nb_chars<length_cell) && IsWhitespace(cell[nb_chars])) nb_chars++;
    // go to end of interpreted field
    while ((nb_chars<length_cell) && !IsWhitespace(cell[nb_chars]) && !IsCellSeparator(cell[nb_chars])) nb_chars++;
    // go to next char which is not a whitespace.
    while ((nb_chars<length_cell) && IsWhitespace(cell[nb_chars])) nb_chars++;
    //go to next char which is not a whitespace but could be a separator 
    while ((nb_chars < length_cell) && (!IsWhitespace(cell[nb_chars]) && IsCellSeparator(cell[nb_chars])))
    {
        if (IsCellSeparator(cell[nb_chars]) && IsCellSeparator(cell[nb_chars + 1]))
        {
            nb_chars++;
            break;
        }
        nb_chars++;
    }
    return (int) nb_chars;
}

bool MECIDataReader::IsWhitespace (char c) const
{
    return c <= 0x20;
}


bool MECIDataReader::IsCellSeparator (char c) const
{
    return (c == ',') || IsWhitespace (c);
}

/* --------- Messages --------- */

const char *MECIDataReader::getMsg(int ind) const {
  return MV_get_msg_array(MSGT_MODEL_IO)[ind];
}

const char *MECIDataReader::GetFormatSize(const char *fmt_p, bool is_free_size, int &fmt_size) {

    const char *a_fmtp = mySyntaxInfos_p->GetFormatSize(fmt_p, is_free_size, fmt_size);
    
    if (a_fmtp)
        return a_fmtp;

    if (is_free_size && !a_fmtp)
    {
        fmt_size = 0;
        return fmt_p;
    }
    const string &fmt = fmt_p;
    int a_size = atoi(fmt.substr(1, fmt.find_first_of(".sdilfeg") - 1).c_str());
    fmt_size = a_size<0 ? (-a_size) : a_size;

    return fmt_p;
}

unsigned int MECIDataReader::GetCellSize(const PseudoFileFormatCell_t *cell_format_p) {
    
    int a_nb_chars = 0;
    //
    const ff_cell_t *a_cell_format_p = (const ff_cell_t *)cell_format_p;
    ff_cell_type_e  a_cell_type = CELL_UNKNOWN;
    MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_TYPE, &a_cell_type, END_ARGS);
    //
    switch (a_cell_type) {
    case CELL_COMMENT: 
    {
        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_SIZE, &a_nb_chars, END_ARGS);
    }
    break;
    case CELL_VALUE:
    case CELL_DIR_RADIO:
    case CELL_DIR_FLAGS:
    case CELL_APPEND_OPTIONS:
    {
        const char *a_fmt = NULL;
        MCDS_get_ff_cell_attributes(a_cell_format_p, CELL_FORMAT, &a_fmt, END_ARGS);
        a_nb_chars = 0;
        a_fmt = GetFormatSize(a_fmt, false, a_nb_chars);
    }
    break;
    default:
        break;
    }
    //
    return a_nb_chars;
}

int MECIDataReader::GetLineLength(int max_len, int myLineLength)
{
    if (max_len <= 0)
        return myLineLength;
    if (max_len <= myLineLength)
        return max_len;
    else
        return myLineLength;
}
/* --------- Static functions --------- */

static int loc_get_fmt_size(const string &fmt, bool is_free_size) {
  if (is_free_size) return 0; 
  int a_size=atoi(fmt.substr(1,fmt.find_first_of(".sdilfeg")-1).c_str());
  return a_size<0 ? (-a_size) : a_size;
}
static int loc_ff_get_fmt_size(const string &linebuff) {
	int a_size =0, len = 0;
  	const char *buff = linebuff.c_str();
  	len = (int)strlen(buff);

	for(a_size = 0; a_size < len; a_size++)
	{
		if(buff[a_size] != ' ' && buff[a_size] != '\t')//for leading spaces and tab
		{					
			for(int j = a_size; j < len; j++)
			{
				if(buff[j] == ' ' || buff[j] == '\t' || buff[j] == '\n')
					break;
				++a_size;   //in addition to above leading spaces add till get the next space			
			}
			break;
		}
	}	
	
	return a_size;
}


bool loc_is_ikeyword_valid(int ikeyword,const IMECPreObject &object,const IDescriptor *descr_p) {
  bool a_is_valid       = false;
  bool a_is_conditional = false;
  //
  int a_domains=HCDI_get_all_domains();
  MvDependenceList_t a_dependences;
  descr_p->getDependences(a_domains,&a_dependences);
  //
  MvDependenceList_t::iterator a_it_begin = a_dependences.begin();
  MvDependenceList_t::iterator a_it_end   = a_dependences.end();
  MvDependenceList_t::iterator a_it;
  for(a_it=a_it_begin;!a_is_valid && a_it!=a_it_end;++a_it) {
    const MvDependence_t *a_depend_p = (*a_it);
    int                   a_nb_conds = a_depend_p->getNbConditions();
    //
    for(int i=0;(!a_is_valid) && i<a_nb_conds;++i) {
      const MvIKeywordSet_t &a_ikeywords=a_depend_p->getIKeywords(i);
      if(ikeyword<a_ikeywords) {
	if(!a_is_conditional) a_is_conditional=true;
	//
	const MvExpression_t *a_expr_p=a_depend_p->getExpressionPtr(i);
	a_is_valid=object.EvaluateExpression((const PseudoExpression_t *)a_expr_p,
					     (const PseudoDescriptor_t *)descr_p);
	//
	if(a_is_valid) for(int j=0;a_is_valid && j<i;++j) {
	  const MvExpression_t *a_prev_expr_p=a_depend_p->getExpressionPtr(j);
	  a_is_valid=(!object.EvaluateExpression((const PseudoExpression_t *)a_prev_expr_p,
						 (const PseudoDescriptor_t *)descr_p));
	}
      }
    }
    //
    if(!a_is_valid) {
      const MvIKeywordSet_t &a_ikeywords=a_depend_p->getDefaultIKeywords();
      if(ikeyword<a_ikeywords) {
	if(!a_is_conditional) a_is_conditional=true;
	//
	a_is_valid=true;
	for(int j=0;a_is_valid && j<a_nb_conds;++j) {
	  const MvExpression_t *a_prev_expr_p=a_depend_p->getExpressionPtr(j);
	  a_is_valid=(!object.EvaluateExpression((const PseudoExpression_t *)a_prev_expr_p,
						 (const PseudoDescriptor_t *)descr_p));
	}
      }
    }
  }
  //
  return a_is_conditional ? a_is_valid : true;
}


void DataReaderUpdateManager_t::clear ()
{
    myCellFormatList.clear ();
    myCellIndexList.clear ();
}

void DataReaderUpdateManager_t::addCell (const ff_cell_t *cell_p, int cell_index)
{
    myCellFormatList.push_back (cell_p);
    myCellIndexList.push_back (cell_index);
}



static int loc_reserve_array(int                          cell_ikw,
		      const string                &cell_skw,
		      IMECPreObject::MyValueType_e  vtype,
		      const IDescriptor        *descr_p,
		      IMECPreObject                *object_p)
{
  attribute_type_e a_cell_atype = descr_p->getAttributeType(cell_ikw);
  int              a_nb_values  = 0;
  //
  if(a_cell_atype==ATYPE_STATIC_ARRAY) {
    a_nb_values=descr_p->getSize(cell_ikw);
  } else {
    int    a_size_ikw      = descr_p->getSizeIKeyword(cell_ikw);
    string a_size_skeyword = descr_p->getSKeyword(a_size_ikw);
    //
    a_nb_values=object_p->GetIntValue(a_size_skeyword.c_str());
  }
  //
  if(a_nb_values<=0) return -1;
  //
  int a_array_ind=-1;
  switch(vtype) {
  case IMECPreObject::VTY_BOOL:   a_array_ind=object_p->AddBoolArray(cell_skw.c_str(),a_nb_values);   break;
  case IMECPreObject::VTY_INT:    a_array_ind=object_p->AddIntArray(cell_skw.c_str(),a_nb_values);    break;
  case IMECPreObject::VTY_UINT:   a_array_ind=object_p->AddUIntArray(cell_skw.c_str(),a_nb_values);   break;
  case IMECPreObject::VTY_FLOAT:  a_array_ind=object_p->AddFloatArray(cell_skw.c_str(),a_nb_values);  break;
  case IMECPreObject::VTY_STRING: a_array_ind=object_p->AddStringArray(cell_skw.c_str(),a_nb_values); break;
  case IMECPreObject::VTY_OBJECT: a_array_ind=object_p->AddObjectArray(cell_skw.c_str(),a_nb_values); break;
  default:                       break;
  }
  //
  return a_array_ind;
}
 
static int loc_reserve_array(int                           cell_ikw,
                             const char                   *cell_skw,
                             IMECPreObject::MyValueType_e  vtype,
                             const IDescriptor*            descr_p,
                             IMECPreObject*                object_p)
{
    attribute_type_e a_cell_atype = descr_p->getAttributeType(cell_ikw);
    int              a_nb_values = 0;
    //
    if (a_cell_atype == ATYPE_STATIC_ARRAY) {
        a_nb_values = descr_p->getSize(cell_ikw);
    }
    else {
        int    a_size_ikw = descr_p->getSizeIKeyword(cell_ikw);
        string a_size_skeyword = descr_p->getSKeyword(a_size_ikw);
        //
        a_nb_values = object_p->GetIntValue(a_size_skeyword.c_str());
    }
    //
    if (a_nb_values <= 0) return -1;
    //
    int a_array_ind=-1;
    switch (vtype) {
    case IMECPreObject::VTY_BOOL:   a_array_ind=object_p->AddBoolArray(cell_skw, a_nb_values);   break;
    case IMECPreObject::VTY_INT:    a_array_ind=object_p->AddIntArray(cell_skw, a_nb_values);    break;
    case IMECPreObject::VTY_UINT:   a_array_ind=object_p->AddUIntArray(cell_skw, a_nb_values);   break;
    case IMECPreObject::VTY_FLOAT:  a_array_ind=object_p->AddFloatArray(cell_skw, a_nb_values);  break;
    case IMECPreObject::VTY_STRING: a_array_ind=object_p->AddStringArray(cell_skw, a_nb_values); break;
    case IMECPreObject::VTY_OBJECT: a_array_ind=object_p->AddObjectArray(cell_skw, a_nb_values); break;
    default:                       break;
    }
    //
    return a_array_ind;
}




//++///////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      This function is used to get the number of the total elements in an array
//Example:
//      Given an Array A[SIZE1][SIZE2][SIZE3], you'll get SIZE1*SIZE2*SIZE3 
//Parameters:
//      MECIModelFactory *model_p: a pointer to the model object
//Return value:
//      int: returns the total array size if it's a multidimensional array
//      or else, returns 0
//Attention
//      The static sizes come directly from descriptor
//      The dynamic sizes come from pre-object by using ikeyword stocked in descriptor
//Modification History:

//--///////////////////////////////////////////////////////////////////////////////
int MECIDataReader::getMultidimensionalArraySize(const PseudoFileFormatCard_t *card_format_p,
                                   IMECPreObject                 *object_p,
                                   const PseudoDescriptor_t     *descr_p)
{
   const IDescriptor *a_descr_p       = (const MvDescriptor_t *)descr_p;
   int a_cell_ikw =isMultiArray(card_format_p,descr_p) ;
   int a_index = 0;
   string a_index_skw = "";
   if(a_cell_ikw ==-1)
   {
      return 0;
   }
   else
   {
      MvSizeVector  sizeArrayVector;
      int size = 1;
      a_descr_p->getDimensionSize(a_cell_ikw,sizeArrayVector);
      
      for(unsigned int i = 0; i< sizeArrayVector.size();i++)
      {
         if(!sizeArrayVector[i].isRealSize)
         {
            a_index_skw = a_descr_p->getSKeyword(sizeArrayVector[i].size);
            a_index = object_p->GetIntValue(a_index_skw.c_str());
            size = size * a_index;
         }
         else
         {
            size = size *sizeArrayVector[i].size;
         }
      }
      return size;
   }
}

int MECIDataReader::isMultiArray(const PseudoFileFormatCard_t *card_format_p,
                                   const PseudoDescriptor_t     *descr_p)
{
  const ff_card_t      *a_card_format_p = (const ff_card_t *)card_format_p;
  const IDescriptor *a_descr_p       = (const MvDescriptor_t *)descr_p;
  ff_cell_t *a_cell_format_p=NULL;
  int a_cell_ikw =0;
  MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,0,(void *)(&a_cell_format_p));
  MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_IKEYWORD,&a_cell_ikw,END_ARGS);
  if(!a_descr_p->isMultiDimensionalArray(a_cell_ikw))
  {
      return -1;
  }
  else
  {
      return a_cell_ikw;
  } 
}


/* Check if parameter is used for the cell: Assuming '&' is parameter defining character [only applicable for lsdyna and radioss]*/
bool MECIDataReader::isParameterCell (const char *cell,int &nb_chars, string &param_str, bool *is_negated_p)
{
    bool isParameter = false;

    /* If the cell only has one character, there is no room for a parameter name,
    * so it cannot be a parameter cell.
    * This is used for example in *CONTACT to check for presence of a "&" as a trigger for an
    * optional card (2nd MPP card) */
    if(1 == nb_chars) return false;

    //check for parameter cell.
    //Remove leading blank spaces if any from the cell
    const char* a_cell_wo_space = myReadContext_p->killBlanksBegin(cell);
    /************************************************************************************
                            Check for parameter string
    1. First char can have "-", in which case it will be a negated parameter reference    
    2. Otherwise, in case of "&" as first character, it is a regular parameter reference.
    In all of these cases, this is a parameter reference
    *************************************************************************************/
    if(a_cell_wo_space==NULL || a_cell_wo_space[0]=='\0' ||
        (nb_chars>0 && (a_cell_wo_space-cell)>=nb_chars)) // complete cell (and maybe more) only space
        return isParameter;
    if(a_cell_wo_space[0] == '-')
    {        
        if(a_cell_wo_space[1] == mySyntaxInfos_p->getParameterSymbol()[0])
        {
            isParameter = true;
            if(is_negated_p) *is_negated_p = true;
        }
    }
    else if(a_cell_wo_space[0] == mySyntaxInfos_p->getParameterSymbol()[0])
    {
        isParameter = true;
    }

    int cell_length = 0;
    if(nb_chars == 0) // This means that it is a free-size-format cell
    {
        cell_length = (int)strlen(cell);
    }
    else
    {
        cell_length = nb_chars;
    }
    //extract the parameter string
    if(isParameter)
    {
        // return false if parameter handling is deactivated
        if(-1 == myNoParametersFlag)
        { // get flag if not yet done
            myNoParametersFlag = 0;
            if(-1 == myNoParametersBitmask)
            {
                const CFGKernel* pKernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
                if(pKernel) myNoParametersBitmask = pKernel->get_data_hierarchy_bitmask("NO_PARAMETERS");
                else        myNoParametersBitmask = 0; // shouldn't happen
            }
            if(nullptr != myObject_p && 0 != myNoParametersBitmask)
            {
                string otypestr = myObject_p->GetKernelFullType(), subtypestr = "";
                if(otypestr.size() > 0 && '/' == otypestr[0]) otypestr.erase(0, 1);
                size_t slashpos = otypestr.find('/');
                if(otypestr.npos != slashpos)
                {
                    subtypestr = otypestr.substr(slashpos + 1);
                    otypestr.resize(slashpos);
                }
                object_type_e otype = MV_get_type(otypestr);
                int flags = HCDIGetObjTypeFlags(otype, subtypestr);
                if((flags & myNoParametersBitmask) > 0) myNoParametersFlag = 1;
            }
        }
        if(1 == myNoParametersFlag) return false;

        char *a_cell_end_p = const_cast<char *>(cell+cell_length);
        char  a_cell_end   = (*a_cell_end_p);        
        *a_cell_end_p='\0';
        param_str = cell;  
        param_str.erase(param_str.find_last_not_of(' ')+1);        //Remove trailing spaces
        if(param_str.size())
        {
            param_str.erase(0,param_str.find_first_not_of(' '));        //remove leading spaces
            if(param_str.size()) {
                if(param_str[0] == '-')
                {
                    const char* paramsym = mySyntaxInfos_p->getParameterSymbol();
                    size_t len = strlen(paramsym);
                    if(param_str[1] == paramsym[0])
                    {
                        param_str = param_str.substr(2, param_str.length());
                        if(len>1)
                            param_str.erase(param_str.find_last_not_of(paramsym[1]) + 1);
                    }
                }
                else
                {
                    const char* paramsym = mySyntaxInfos_p->getParameterSymbol();
                    size_t len = strlen(paramsym);
                    if(param_str[0] == paramsym[0])
                    {
                        param_str = param_str.substr(1, param_str.length());
                        if (len > 1)
                        {
                            size_t end = param_str.find(paramsym[1]);
                            if (end != std::string::npos) // If second '%' is not found
                            {
                                param_str = param_str.substr(0, end);
                            }
                        }
                    }
                }
            }
        }
        *a_cell_end_p=a_cell_end;
    }
    return isParameter;
} 

bool MECIDataReader::replaceTextParameter(const char *cell, const int nb_chars, string &param_str,
    void *model_p, const int file_index, bool* a_ok_p)
{
    return false;
}

bool MECIDataReader::isFreeSizeCard(const char * buffer) const 
{
   return mySyntaxInfos_p->isFreeSizeCard(buffer);
}
