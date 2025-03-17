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

#include <UTILS/win32_utils.h>

#include <UTILS/str_utils.h>
#include <UTILS/memory_utils.h>
#include <UTILS/direction_utils.h>
#include <UTILS/mv_cstring.h>
#include <KERNEL/mv_type.h> 
#include <KERNEL/mv_model_descriptors.h>
#include <KERNEL_BASE/fileformat_API.h>
#include <KERNEL/Structure_fileformat_others.h>
#include <math.h>

#include <KERNEL_BASE/expression_API.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include <HCDI/hcdi_utils.h>
#include "mec_pre_object_expression_evaluator.h"
#include "mv_model_scanner.h"
#include "hcioi_utils.h"
#include "mec_data_writer.h"
#include <assert.h>

static const char *        loc_remove_leadingandtrailingwhitespaces(char *str);

/*******************/

/** At present it is duplicated later it will be moved to hwcommon **/
/* This is the maximum real value that would be output as zero; used with *compressreal(3) */
#define MAX_ZERO_TOLERANCE 1.0E-8

#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif
static string GetStringValueFromPreObject(IMECPreObject* pre_object, MvDataFeatureType_e feature_type, value_type_e val_type, attribute_type_e att_type, string& a_cell_skw, const char* a_cell_fmt, int fmt_size,
    int myremoveE, double myzeroTol);
char *strdel(char *string, int position, int length)
{
  /*
     strcpy(&string[position],&string[position+length]);
     Note: purfiy complains the above sentence,  so  change to memove
  */
  int len = (int)strlen(string); 
  memmove(string+position, string+position+length, len-position-length+1); /* add +1 for the null terminator */

  return(string);
}

bool stradddecimal(char *buf, char *str, int width, int left)
{
   if(width == (strlen(str) + 2)) // the extra two is because we are going to add ".0"
   {
      sprintf(buf,"%s.0",str);
      return true;
   }
   else if(width == (strlen(str) + 1))  // the extra one is because we are going to add the decimal point
   {
      sprintf(buf,"%s.",str);
      return true;
   }
   else if(width > (strlen(str) + 2))
   {
      if(left)
        sprintf(buf,"%s.0%*c",str,(width-(int)strlen(str)-2),' ');
      else
        sprintf(buf,"%*c%s.0",(width-(int)strlen(str)-2),' ',str);
      return true;
   }
   return false;
}
bool strformatstring(char *buf, char *str, int width, int left)
{
   if(width == strlen(str))
   {
      sprintf(buf,"%s",str);
      return true;
   }
   else if(width > strlen(str))
   {
      if(left)
        sprintf(buf,"%s%*c",str,(width-(int)strlen(str)),' ');
      else
        sprintf(buf,"%*c%s",(width-(int)strlen(str)),' ',str);
      return true;
   }
   return false;
}
char *strsprm(char *string)
{
  while (string[0] == ' ') 
    strdel(string,0,1);
  while (string[strlen(string)-1] == ' ')
    strdel(string,(int) strlen(string)-1,1);
  return(string);
}

int JRW_stricmp(const char *string1, const char *string2)
{
    /* ---------------10/2/96 JRW 9:12-------------------
    * This is taken from the 'strcmp' code in Plauger's
    * Standard C Library book page 404. All I added was
    * the tolower call on the chars being compared.
    * --------------------------------------------------*/
    /* Fixed by Sol 03/11/97 */
    for (; tolower(*string1) == tolower(*string2); ++string1, ++string2)
        if (*string1 == '\0')
            return 0;

    return ((tolower(*(unsigned char *)string1) < tolower(*(unsigned char *)string2)) ? -1 : +1);
}
static void GetDoubleValueString(char *string, int width, double realvalue, int left, int remove_e, double zero_tol, int compress_double, int roundoff_double);
static void strfixexponent(char *str);
static void strexponential(char *buf, const char *formatstr, int width, int precision, double x);
static bool strmaxprecision(char *buf, int width, double x, int left, int remove_e, double zero_tol);

std::string doubleToString(double value, int precision) {
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(precision) << value;
    std::string result = oss.str();

    // Remove trailing zeros
    size_t pos = result.find_last_not_of('0');
    if (pos != std::string::npos && pos != result.length() - 1) {
        result.erase(pos + 1);
    }

    return result;
}

bool containsChar(const char* str, char c) {
    while (*str != '\0') {
        if (*str == c) {
            return true;  // Found 
        }
        ++str;
    }
    return false;  // '-' not found
}

std::string trim(const std::string& str) {
    // Find the first non-whitespace character
    size_t start = str.find_first_not_of(" \t\n\r");

    // If all characters are whitespace, return an empty string
    if (start == std::string::npos)
        return "";

    // Find the last non-whitespace character
    size_t end = str.find_last_not_of(" \t\n\r");

    // Extract the substring between start and end (inclusive)
    return str.substr(start, end - start + 1);
}

std::vector<std::string> splitString(const std::string& str, int width) {
    std::vector<std::string> result;
    bool splitByWidth = width ? true : false;
    if (splitByWidth) {
        for (size_t i = 0; i < str.length(); i += width) {
            std::string substring = str.substr(i, width);
            substring.erase(0, substring.find_first_not_of(' '));
            substring.erase(substring.find_last_not_of(' ') + 1);
            result.push_back(substring);
        }
    }
    else {
        std::stringstream ss(str);
        std::string substring;
        while (ss >> substring) {
            result.push_back(substring);
        }
    }

    return result;
}




/*******************/

/* --------- Constructors & destructor --------- */

MECDataWriter::MECDataWriter (MECIWriteContext *writeContext_p,
                              int LineLength,bool writeFreeFormatFlag, const char *delm, int removeE, double zeroTol, 
                              bool datalinecommentFlag, bool writeDefaultValue, bool hasparameter,bool userCommentFlag) : 
    myWriteContext_p (writeContext_p),
    
    
    myLineLength (LineLength), 
    myFileFormatId ((PseudoFileFormat_e) FF_UNKNOWN), 
    myNewlineString ("\n"),
    myWriteFreeFormat(writeFreeFormatFlag),
    mydelimiter(delm),
    myremoveE(removeE),
    myzeroTol(zeroTol),
    myWriteDataLineFlag(datalinecommentFlag),
    myWriteUserCommentFlag(userCommentFlag),
    mywriteDefaultValue(writeDefaultValue),
    myhasParameter(hasparameter),
    myFileopFieldOverflow(NULL),
    mycompressDouble(0),
    myroundDouble(0),
    myCurActiveLineLength(0)
{
    myKeywordFormatType = io_types::format_type_e::FORMAT_UNDEFINED;
    myIsLastCell = false;
}

MECDataWriter::~MECDataWriter()
{
    for (auto ivec = myVecPreCardDisplayStatus.begin(); ivec != myVecPreCardDisplayStatus.end(); ++ivec)
    {
        IMECPreObject* pre_object = (IMECPreObject*)(ivec->first);
        if(pre_object)
            HCDI_ReleasePreObjectHandle(pre_object);
    }
    myVecPreCardDisplayStatus.clear();
}

/* --------- Writing pre-objects data --------- */


void MECDataWriter::WriteObjectData(const PseudoFileFormat_t *format_p,
                                    const IMECPreObject       &pre_object,
                                    const PseudoDescriptor_t *descr_p,
                                    int                       card_ind0,
                                    int                       card_ind1)

{
    WriteObjectData(format_p, pre_object, descr_p, NULL, card_ind0, card_ind1);
}

void MECDataWriter::WriteObjectData(const PseudoFileFormat_t *format_p,
                                    const IMECPreObject       &pre_object,
                                    const PseudoDescriptor_t *descr_p,
                                    const MECIModelScanner   *model_p,
                                    int                       card_ind0,
                                    int                       card_ind1)

{
  const fileformat_t *a_format_p=(const fileformat_t *)format_p;
  //
  int a_nb_cards=card_ind1+1;
  if(a_nb_cards<=0) MCDS_get_fileformat_nb_cards(a_format_p,&a_nb_cards);
  //
  for(int i=card_ind0;i<a_nb_cards;i++) {
    ff_card_t *a_card_p=NULL;
        ff_card_t *a_next_card_p = NULL;
    bool       a_do_continue=true; 
    MCDS_get_fileformat_card(a_format_p,i,&a_card_p);
        int flag = 0;
        MCDS_get_ff_card_attributes(a_card_p, CARD_FLAGS, &flag, END_ARGS);
        if (flag)
        {
            bool no_end_flag = (flag & CARD_FLAG_NO_END) ? true : false;
            if (no_end_flag && i < (a_nb_cards - 1))
            {
                i++;
                MCDS_get_fileformat_card(a_format_p, i, &a_next_card_p);
            }
        }
    a_do_continue=WriteCard((const PseudoFileFormatCard_t *)a_card_p,pre_object,descr_p, 
            model_p, -1, a_next_card_p); 
    if (!a_do_continue) break; 
  }
}


void MECDataWriter::GetIKeywordLst(const PseudoFileFormat_t *format_p,
                                   const IMECPreObject       &pre_object,
                                   const PseudoDescriptor_t *descr_p,
                                   const MECIModelScanner   *model_p,
                                   set<int>                 &ikeywordifchecklst,
                                   set<int>                 &keywordlst)
{
    if(!ikeywordifchecklst.size() || !format_p) return;
    const fileformat_t *a_format_p=(const fileformat_t *)format_p;
    int a_nb_cards= 0;
    MCDS_get_fileformat_nb_cards(a_format_p,&a_nb_cards);
    //
    for(int i=0;i<a_nb_cards;i++) {
        ff_card_t *a_card_p=NULL;
        bool       a_do_continue=true; 
        MCDS_get_fileformat_card(a_format_p,i,&a_card_p);
        GetCardIkeywords(a_card_p, pre_object, descr_p, model_p, ikeywordifchecklst, keywordlst);
    }
}

bool MECDataWriter::GetCardIkeywords(const PseudoFileFormatCard_t *card_p,
                                     const IMECPreObject           &pre_object,
                                     const PseudoDescriptor_t     *descr_p,
                                     const MECIModelScanner       *model_p,
                                     set<int>                     &ikeywordifchecklst,
                                     set<int>                     &ikeywordlst )
{
  const ff_card_t *a_card_p=(const ff_card_t *)card_p;
  bool             a_do_continue=true; 
  //
  ff_card_type_e a_card_type=CARD_UNKNOWN;
  MCDS_get_ff_card_attributes(a_card_p,CARD_TYPE,&a_card_type,END_ARGS);
  switch(a_card_type) {
  case CARD_SINGLE:
  case CARD_HEADER:
  case CARD_LIST:
  case CARD_CELL_LIST:
  case CARD_OBJECT_LIST:
  {
      if (a_card_type == CARD_CELL_LIST)
      {
          int a_size_ikey = 0;
          MCDS_get_ff_card_attributes(a_card_p, CARD_SIZE, &a_size_ikey, END_ARGS);
          if (a_size_ikey > 0)
              ikeywordlst.insert(a_size_ikey);
      }
      int a_nb_cells = 0;
      MCDS_get_ff_card_attributes(a_card_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);
      for (int j = 0; j < a_nb_cells; ++j)
      {
          ff_cell_t* a_cell_p = NULL;
          MCDS_get_ff_card_tab(a_card_p, CARD_CELL, j, (void*)(&a_cell_p));
          ff_cell_type_e a_cell_type = CELL_UNKNOWN;
          MCDS_get_ff_cell_attributes(a_cell_p, CELL_TYPE, &a_cell_type, END_ARGS);
          if (a_cell_type == CELL_COND)
              GetIfCellIkeyword(a_cell_p, pre_object, descr_p, ikeywordifchecklst, ikeywordlst);
			else if (a_cell_type == CELL_NAME_VALUE)
			{
				const ff_name_value_cell_t *a_name_value_cell_p = (const ff_name_value_cell_t *)a_cell_p;
				if (a_name_value_cell_p)
				{
					for (int i = 0; i < a_name_value_cell_p->nb_pairs; i++)
					{
						ff_name_info_t *item = a_name_value_cell_p->name_value_array[i];
						if (item)
						{
							ikeywordlst.insert(item->ikeyword);
						}
					}
				}
			}
          else
          {
              int a_cell_ikw = GetCellIkeyword((PseudoFileFormatCell_t*)a_cell_p);
              if (a_cell_ikw > 0)
                  ikeywordlst.insert(a_cell_ikw);
          }
      }
  }
  break;
  case CARD_CARD_LIST: 
      {
          int a_nb_sub_cards = 0;
          int a_nb_values_ikw = 0;
          MCDS_get_ff_card_attributes(a_card_p, CARD_SIZE, &a_nb_values_ikw, CARD_NB_CARDS,&a_nb_sub_cards, END_ARGS);
          if (a_nb_values_ikw > 0)
              ikeywordlst.insert(a_nb_values_ikw);
          for(int j=0;j<a_nb_sub_cards;++j) {
              ff_card_t *a_sub_card_p=NULL;
              MCDS_get_ff_card_tab(a_card_p,CARD_CARD,j,(void *)(&a_sub_card_p));
              //
              ff_card_type_e a_sub_card_type=CARD_UNKNOWN;
              MCDS_get_ff_card_attributes(a_sub_card_p,CARD_TYPE,&a_sub_card_type,END_ARGS);
              //
              GetCardIkeywords(a_sub_card_p, pre_object, descr_p, model_p, ikeywordifchecklst, ikeywordlst);
          }
      }
      break;
  case CARD_SUBOBJECTS: 
      {
          int    a_subobj_ikw    = END_ARGS;
          MCDS_get_ff_card_attributes(a_card_p,CARD_OBJECTS_IKW,&a_subobj_ikw,END_ARGS);
          if(a_subobj_ikw > 0)    
              ikeywordlst.insert(a_subobj_ikw);
      }
    break;
  case CARD_IF:
      {
          GetIfCardIkeywords(card_p,pre_object,descr_p,model_p, ikeywordifchecklst, ikeywordlst);
      }
      break;
  case CARD_ASSIGN:
      {
		const ff_card_assign_header_t      *a_assign_card_format_p = (const ff_card_assign_header_t *)card_p;
          if(a_assign_card_format_p->attribute_ikw)
              ikeywordlst.insert(a_assign_card_format_p->attribute_ikw);

          assign_operator_e atype = (*a_assign_card_format_p).assign_card_type;


          IDescriptor* a_descrp = (IDescriptor *)descr_p;


          switch (atype)
          {
          case ASSIGN_EXPRESSION:
          {
              int ikey = a_descrp->getIKeyword(a_assign_card_format_p->exp_str);
              if (ikey > 0)
              {
                  int att_id = a_descrp->getIdentifierValue(DOM_COMMON, ikey);
                  if (att_id >= 0)
                      ikeywordlst.insert(ikey);
              }
          }
          break;
          case ASSIGN_GET_DISPLAY_STATUS:
          {
              const ff_card_assign_displaystatus_t* a_displaystatus_assign_card_format_p = (const ff_card_assign_displaystatus_t*)card_p;
              int ikey = a_displaystatus_assign_card_format_p->att_ikey;
              if (ikey > 0)
              {
                  int att_id = a_descrp->getIdentifierValue(DOM_COMMON, ikey);
                  if (att_id >= 0)
                      ikeywordlst.insert(ikey);
              }
          }
          break;
          case ASSIGN_ATTRIB:
          {
              const ff_card_assign_Copy_t* a_assign_Copy_card_format_p = (const ff_card_assign_Copy_t*)card_p;
              int ikey = a_assign_Copy_card_format_p->ikeyword;
              if (ikey > 0)
              {
                  int att_id = a_descrp->getIdentifierValue(DOM_COMMON, ikey);
                  if (att_id >= 0)
                      ikeywordlst.insert(ikey);
              }
          }
          break;
          case ASSIGN_ADD:
          case ASSIGN_SUB:
          case ASSIGN_DIV:
          case ASSIGN_MUL:
          {
              const ff_card_assign_basic_operations_t* a_basic_opera_assign_card_format_p = (const ff_card_assign_basic_operations_t*)card_p;
              double first_val = 0.0, sec_val = 0.0;
              int ikey = a_basic_opera_assign_card_format_p->first_ikey;
              int ikey1 = a_basic_opera_assign_card_format_p->second_ikey;
              if (ikey > 0)
              {
                  int att_id = a_descrp->getIdentifierValue(DOM_COMMON, ikey);
                  if (att_id >= 0)
                      ikeywordlst.insert(ikey);
              }
              if (ikey1 > 0)
              {
                  int att_id = a_descrp->getIdentifierValue(DOM_COMMON, ikey1);
                  if (att_id >= 0)
                      ikeywordlst.insert(ikey1);
              }
              break;
          }
          default:
              break;
          }


      }
      break;
  default:
      break;
  }

  return a_do_continue; 
}
void MECDataWriter::GetIfCardIkeywords(const PseudoFileFormatCard_t *card_p,
    const IMECPreObject           &pre_object,
    const PseudoDescriptor_t     *descr_p,
    const MECIModelScanner       *model_p, 
    set<int>                     &ikeywordifchecklst,
    set<int>                     &ikeywordlst)
{
    const ff_card_t *a_card_p=(const ff_card_t *)card_p;
    //
    int  a_nb_ccls = 0;
    bool a_checked = false;
    MCDS_get_ff_card_attributes(a_card_p,CARD_NB_COND_CARD_LISTS,&a_nb_ccls,END_ARGS);
    for(int i=0; i<a_nb_ccls;++i) {
        ff_condcardlist_t *a_ccl_p  = NULL;
        expression_t      *a_expr_p = NULL;
        MCDS_get_ff_card_tab(a_card_p,CARD_COND_CARD_LIST,i,&a_ccl_p);
        MCDS_get_ff_condcardlist_expression(a_ccl_p,&a_expr_p);
        a_checked=(a_expr_p==NULL);
        //
        if(!a_checked) {
            MvExpression_t a_expr(a_expr_p,false);
            a_checked=pre_object.EvaluateExpression((PseudoExpression_t *)(&a_expr),descr_p,&ikeywordifchecklst, &ikeywordlst);
        }
        //
        if(a_checked) {
            int a_nb_sub_cards=0;
            MCDS_get_ff_condcardlist_nb_cards(a_ccl_p,&a_nb_sub_cards);
            for(int j=0;j<a_nb_sub_cards;++j) {
                ff_card_t *a_sub_card_p=NULL;
                MCDS_get_ff_condcardlist_card(a_ccl_p,j,&a_sub_card_p);
               // WriteCard((const PseudoFileFormatCard_t *)a_sub_card_p,pre_object,descr_p, model_p, ind);
                GetCardIkeywords(a_sub_card_p, pre_object, descr_p, model_p, ikeywordifchecklst, ikeywordlst);
            }
        }
    }
}

void MECDataWriter::GetIfCellIkeyword(const PseudoFileFormatCell_t        *cell_p,
                                      const IMECPreObject                  &pre_object,
                                      const PseudoDescriptor_t            *descr_p,
                                      set<int>                            &ikeywordifchecklst,
                                      set<int>                            &ikeywordlst)
{
    const ff_cell_t *a_cell_p=(const ff_cell_t *)cell_p;
    //
    int     a_nb_ccls = 0;
    bool    a_checked = false;

    MCDS_get_ff_cell_attributes(a_cell_p,CELL_NB_COND_CELL,&a_nb_ccls,END_ARGS);

    for(int i=0;i<a_nb_ccls;++i) {
        ff_condcell_t       *a_ccl_p  = NULL;
        expression_t        *a_expr_p = NULL;

        MCDS_get_ff_cell_tab(a_cell_p, CELL_COND_CELL, i, &a_ccl_p);
        MCDS_get_ff_condcell_expression(a_ccl_p, &a_expr_p);
        a_checked=(a_expr_p==NULL);
        //
        if(!a_checked) {
            MvExpression_t a_expr(a_expr_p,false);
            a_checked = pre_object.EvaluateExpression((PseudoExpression_t *)(&a_expr),descr_p, &ikeywordifchecklst, &ikeywordlst);
        }
        //
        if(a_checked) {
	        ff_cell_t    *a_sub_cell_p=NULL;
            MCDS_get_ff_condcell_cell(a_ccl_p, &a_sub_cell_p);
            int a_cell_ikw=GetCellIkeyword((PseudoFileFormatCell_t*) a_sub_cell_p);
            if(a_cell_ikw > 0)
                ikeywordlst.insert(a_cell_ikw);
        }
    }
}



int MECDataWriter::WriteComments(const PseudoFileFormat_t *format_p,
                                 const IMECPreObject       &pre_object,
                                 const PseudoDescriptor_t *descr_p,
                                 int                       card_ind0,
                                 int                       card_ind1,
                                 int                       do_write_header)
{
  if(false==myWriteDataLineFlag)
    return card_ind0;

  const fileformat_t *a_format_p=(const fileformat_t *)format_p;
  //
  int a_nb_cards=card_ind1+1;
  if(a_nb_cards<=0) MCDS_get_fileformat_nb_cards(a_format_p,&a_nb_cards);
  //
  // Loop over comment cards
  int i=card_ind0;
  while (i<a_nb_cards) {
    ff_card_t *a_card_p=NULL;
    MCDS_get_fileformat_card(a_format_p,i,&a_card_p);
    ff_card_type_e a_card_type=CARD_UNKNOWN;
    MCDS_get_ff_card_attributes(a_card_p,CARD_TYPE,&a_card_type,END_ARGS);
    if (CARD_COMMENT != a_card_type) break;
    // we only get here if it is another comment card, so write it
    if(do_write_header) 
        bool a_do_continue = WriteCommentCard(a_card_p, pre_object, descr_p);
    // go on with the next card
    i++;
  }

  // return the index of the next card, i.e. the one which has not been written yet
  return i;
}

bool MECDataWriter::WriteCommentCard(const PseudoFileFormatCard_t* card_p,
                                     const IMECPreObject& pre_object,
                                     const PseudoDescriptor_t* descr_p,
                                     int   ind,
                                     int   cell_ind0)
{
    bool a_do_continue = true;
    // Temporarily setting the freeformat flag to false to avoid writing comments in free format
    bool isFormatFree = myWriteFreeFormat;
    myWriteFreeFormat = false;
    if (card_p)
        a_do_continue = WriteSingleCard(card_p, pre_object, descr_p, ind, cell_ind0);
    myWriteFreeFormat = isFormatFree; // Setting the flag back to its original value
    return a_do_continue;
}

//
void MECDataWriter::PushToListofValidCards(const IMECPreObject   &pre_object,
                                                 ff_card_t       *v_card_p,
                                                 bool             cardStatus, int ind)
{  
    typedef std::vector<std::pair< ff_card_t *, std::pair<int, bool>>> VecCardvsDisplaystatus;
    auto &i = myVecPreCardDisplayStatus;

    if (myVecPreCardDisplayStatus.size() == 0 || i.back().first != &pre_object)
    {
        VecCardvsDisplaystatus cardptrVsDisplayStatus;
        myVecPreCardDisplayStatus.emplace_back(make_pair(&pre_object, cardptrVsDisplayStatus));
        auto& ii = myVecPreCardDisplayStatus.back();
        ii.second.reserve(10);
    }
    else if (i.back().first == &pre_object)
    {
        if (v_card_p->type == CARD_SINGLE && cardStatus == false)
        {
            if (i.back().second.back().first->type == CARD_COMMENT)
                i.back().second.back().second = make_pair(ind, false);
        }
    }
    myVecPreCardDisplayStatus.back().second.emplace_back(make_pair(v_card_p, make_pair(ind, cardStatus)));
}
//
void MECDataWriter::ComputeValidCards(const PseudoFileFormat_t *format_p,
                                      const IMECPreObject       &pre_object,
                                      const PseudoDescriptor_t *descr_p,
                                      const MECIModelScanner   *model_p,
                                      int                       card_ind0,
                                      int                       card_ind1)
{
    const fileformat_t *a_format_p = (const fileformat_t *)format_p;
    //
    int a_nb_cards = card_ind1 + 1;
    if (a_nb_cards <= 0) MCDS_get_fileformat_nb_cards(a_format_p, &a_nb_cards);
    //
    myVecPreCardDisplayStatus.reserve(4);
    //
    for (int i = card_ind0; i < a_nb_cards; ++i)
    {
        ff_card_t *v_card_p = NULL;
        MCDS_get_fileformat_card(a_format_p, i, &v_card_p);
        UpdateValidCards((const PseudoFileFormatCard_t *)v_card_p, pre_object, descr_p, model_p);
    }
}

void MECDataWriter::UpdateListCardArray(const PseudoFileFormatCard_t*       card_p,
                                        const IMECPreObject&                pre_object,
                                        const PseudoDescriptor_t*           descr_p,
                                        const MECIModelScanner*             model_p,
                                        int                                 ind)
{
   // if (ind < 0)
   //     return;
    const ff_card_t* a_card_p = (const ff_card_t*)card_p;
    const MvDescriptor_t* a_descr_p = (const MvDescriptor_t*)descr_p;
    // Number of values, number of sub-cards
    int a_nb_values_ikw = END_ARGS;
    int a_nb_values = 0;
    int a_nb_sub_cards = 0;
    MCDS_get_ff_card_attributes(a_card_p,
        CARD_SIZE, &a_nb_values_ikw,
        CARD_NB_CARDS, &a_nb_sub_cards,
        END_ARGS);
    if (a_nb_values_ikw > 0) {
        string a_nb_values_skw = a_descr_p->getSKeyword(a_nb_values_ikw);
        a_nb_values = pre_object.GetIntValue(a_nb_values_skw.c_str());
    }
    else {
        a_nb_values = (-a_nb_values_ikw);
    }


    // Loops
    for (int i = 0; i < a_nb_values; ++i) {
        for (int j = 0; j < a_nb_sub_cards; ++j) {


            ff_card_t* a_sub_card_p = NULL;// card_lst[j].second;
            MCDS_get_ff_card_tab(a_card_p,CARD_CARD,j,(void *)(&a_sub_card_p));
            //
            ff_card_type_e a_sub_card_type = CARD_UNKNOWN;// card_lst[j].first;
            MCDS_get_ff_card_attributes(a_sub_card_p,CARD_TYPE,&a_sub_card_type,END_ARGS);
            //
            switch (a_sub_card_type) {
            case CARD_BLANK:
            {
                //myWriteContext_p->WriteFile("\n");
            }
            break;
            case CARD_COMMENT:
            {
                if (true == myWriteDataLineFlag)
                {
                    //bool a_do_continue = WriteCommentCard(a_sub_card_p, pre_object, descr_p, i);
                }
            }
            break;
            case CARD_ASSIGN:
            {
                IMECPreObject* a_pre_object_p = const_cast<IMECPreObject*>(&pre_object);
                Assign(a_sub_card_p, *a_pre_object_p, descr_p, model_p, i);
            }
            break;
            case CARD_SINGLE:
            case CARD_HEADER:
            {

                //WriteSingleCardList((const PseudoFileFormatCard_t*)a_sub_card_p, a_sub_card_type, pre_object, descr_p, i, 0, umap_card_cells[a_sub_card_p]);
                //WriteSingleCard((const PseudoFileFormatCard_t *)a_sub_card_p,pre_object,descr_p,i);
                PushToListofValidCards(pre_object, (ff_card_t*)a_sub_card_p, true, i);
            }
            break;
            case CARD_IF:
            {
               // WriteIfCard((const PseudoFileFormatCard_t*)a_sub_card_p, pre_object, descr_p,
               //     model_p, i, &umap_card_cells);
                UpdateIfValidCards(a_sub_card_p, pre_object, descr_p, model_p, i);
            }
            break;
            default:
                break;
            }
        }
    }
}


//
void MECDataWriter::UpdateValidCards(const PseudoFileFormatCard_t *card_p,
                              const IMECPreObject              &pre_object,
                              const PseudoDescriptor_t         *descr_p,
							  const MECIModelScanner           *model_p, 
                              int                               ind)
{
    const ff_card_t *f_card_p = (const ff_card_t *)card_p;
    bool             v_card_status = false;
    //
    ff_card_type_e a_card_type = CARD_UNKNOWN;
    MCDS_get_ff_card_attributes(f_card_p, CARD_TYPE, &a_card_type, END_ARGS);
    //
    switch (a_card_type)
    {
        case CARD_SINGLE:
        case CARD_HEADER:
        {
            v_card_status = GetSingleCardDisplayStatus(card_p, pre_object, descr_p, ind);
            PushToListofValidCards(pre_object, (ff_card_t *)card_p, v_card_status, ind);
        }
        break;
        case CARD_SUBOBJECTS:
        {
            WriteSubobjects(card_p, pre_object, descr_p, model_p, true);
        }
        break;
        case CARD_IF:
        {
            UpdateIfValidCards(card_p, pre_object, descr_p, model_p, ind);
        }
        break;
        case CARD_CARD_LIST:
        {
            UpdateListCardArray(card_p, pre_object, descr_p, model_p, ind);
        }
        break;
        case CARD_ASSIGN:
        {
            IMECPreObject    *a_pre_object_p = const_cast<IMECPreObject *>(&pre_object);
            Assign(card_p, *a_pre_object_p, descr_p, model_p, ind);
        }
        break;
        case CARD_PREREAD:
            break;
        default:
        {
            PushToListofValidCards(pre_object, (ff_card_t *)card_p, true, ind);
        }
        break;
    }
}


bool MECDataWriter::WriteCard(const PseudoFileFormatCard_t *card_p,
                              const IMECPreObject           &pre_object,
                              const PseudoDescriptor_t     *descr_p,
                              const MECIModelScanner       *model_p, 
                              int                           ind,
    const PseudoFileFormatCard_t *next_card_p,
                              std::unordered_map< ff_card_t *, vector<card_cells_temp_t> > *umap_card_cells)
{
  const ff_card_t *a_card_p=(const ff_card_t *)card_p;
  bool             a_do_continue=true; 
  //
  ff_card_type_e a_card_type=CARD_UNKNOWN;
  MCDS_get_ff_card_attributes(a_card_p,CARD_TYPE,&a_card_type,END_ARGS);
  switch(a_card_type) {
  case CARD_BLANK:
    {
      myWriteContext_p->WriteFile("\n");
    }
    break;
  case CARD_COMMENT:
    {
      if(true==myWriteDataLineFlag)
      {
          a_do_continue = WriteCommentCard(a_card_p, pre_object, descr_p, ind);
      }
    }
    break;
  case CARD_SINGLE:
  case CARD_HEADER:
    {
       if(umap_card_cells)
       {
          ff_card_t *aa_card_p = (ff_card_t *)a_card_p;
          vector<card_cells_temp_t>  &a_card_lst = (*umap_card_cells)[aa_card_p];
          a_do_continue=WriteSingleCardList(card_p, a_card_type, pre_object,descr_p,ind, 0, a_card_lst);
       }
       else
       {
           a_do_continue = WriteSingleCard(card_p, pre_object, descr_p, ind);
            if (next_card_p)
            {
                const ff_card_t *a_next_card_p = (const ff_card_t *)next_card_p;
                ff_card_type_e a_next_card_type = CARD_UNKNOWN;
                MCDS_get_ff_card_attributes(a_next_card_p, CARD_TYPE, &a_next_card_type, END_ARGS);
                assert(a_next_card_type == CARD_CELL_LIST);
                if (a_next_card_type == CARD_CELL_LIST)
                    WriteCellList(next_card_p, pre_object, descr_p, ind, card_p);
                else
                    WriteCard(next_card_p, pre_object, descr_p, model_p);
            }
           if (CARD_HEADER == a_card_type && ind < 0)
               WriteHmComments(pre_object, model_p);
       }
    }
    break;
    
  case CARD_LIST:
    {
      WriteList(card_p,pre_object,descr_p);
    }
    break;
    
  case CARD_CELL_LIST:
    {
      WriteCellList(card_p,pre_object,descr_p,ind);
    }
    break;
  case CARD_OBJECT_LIST:
    {
      WriteObjectList(card_p,pre_object,descr_p);
    }
    break;
  case CARD_CARD_LIST: 
    {
      WriteCardList(card_p,pre_object,descr_p,model_p);
    }
    break;
    
  case CARD_SUBOBJECTS: 
    {
      WriteSubobjects(card_p,pre_object,descr_p,model_p);
    }
    break;
    
  case CARD_IF:
    {
      WriteIfCard(card_p,pre_object,descr_p,
                  model_p, 
                  ind, umap_card_cells);
    }
    break;
  case CARD_ASSIGN:
      {
          IMECPreObject    *a_pre_object_p =  const_cast<IMECPreObject *>(&pre_object);
          Assign(card_p,*a_pre_object_p,descr_p,model_p, ind);
      }
      break;
  default:
      break;
  }
    return a_do_continue;
}


void MECDataWriter::Assign(const PseudoFileFormatCard_t *card_p,
                           IMECPreObject                 &pre_object,
                           const PseudoDescriptor_t     *descr_p,
                           const MECIModelScanner       *model_p,
                           int                           ind)
{

    const ff_card_assign_header_t      *a_assign_card_format_p  = (const ff_card_assign_header_t *)card_p;
    const IDescriptor *a_descr_p = (const IDescriptor *)descr_p;
    MvModelScanner_t *mv_model = (MvModelScanner_t *)model_p;
    if (!mv_model)
        return;
    hwHCSolverInf    *mysolverinf = mv_model->getHmSolverInf();
    double value = 0.0;
    std::string value_str = "";
    assign_operator_e atype = (*a_assign_card_format_p).assign_card_type;

    value_type_e a_result = VTYPE_UNKNOWN;
    int assign_card_attrib_ikey = a_assign_card_format_p->attribute_ikw;
    a_result = a_descr_p->getValueType(assign_card_attrib_ikey);
    string skeyword = a_descr_p->getSKeyword(assign_card_attrib_ikey);

    if(a_assign_card_format_p->mode == ASSIGN_MODE_IMPORT )
        return;
    if (VTYPE_STRING == a_result && !(atype == ASSIGN_GET_ENTITY_VALUE || atype == ASSIGN_GET_CURRENT_ENTITY || atype == ASSIGN_PUSH || atype == ASSIGN_COMBINE || atype == ASSIGN_ERASE
        || atype == ASSIGN_ATTRIB))
    { 
        string exp_str;
        const char *p_exp_str = a_assign_card_format_p->exp_str;
        int a_ikeyword = a_descr_p->getIKeyword(p_exp_str);

        if (a_ikeyword > 0)
        {
            int a_ind = -1;
            if (ind < 0)
            {
                a_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, p_exp_str);
                if(a_ind >= 0)
                    p_exp_str = pre_object.GetStringValue(a_ind);
                else
                {
                    exp_str = a_descr_p->getStringDefaultValue(a_ikeyword, DOM_COMMON);
                    p_exp_str = exp_str.c_str();
                }
            }
            else
            {
                a_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, p_exp_str);
                if (a_ind >= 0)
                {
                    p_exp_str = pre_object.GetStringValue(a_ind, ind);
                }
                else
                {
                    exp_str = a_descr_p->getStringDefaultValue(a_ikeyword, DOM_COMMON);
                    p_exp_str = exp_str.c_str();
                }
            }
        }
        
        if(ind<0) 
        {
            pre_object.AddStringValue(skeyword.c_str(), p_exp_str);
        }
        else 
        {
            attribute_type_e a_atype = a_descr_p->getAttributeType(assign_card_attrib_ikey);
            int a_cell_ind = -1;
            if(a_atype == ATYPE_VALUE)
            {
                pre_object.AddStringValue(skeyword.c_str(), ind, p_exp_str);
            }
            else if(a_atype == ATYPE_DYNAMIC_ARRAY)
            {
                a_cell_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_STRING,skeyword);
                if(a_cell_ind>=0)
                    pre_object.SetStringValue(a_cell_ind,ind, p_exp_str);
            }
        }
        return;
    }
    /* Whenever any new Assign type is being added and If the attribute that is going to get updated is of the type Dynamic Array,
       make sure to call UpdatePreObjectConnectedSizeIkeywords() method */
    switch(atype)
    {
    case ASSIGN_EXPRESSION:
    {
        ExpressionEvaluatorExprTk evaluator;
        MECPreObjectExpressionEvaluator pre_object_handler(&pre_object, a_descr_p, &evaluator, ind);
        if (a_assign_card_format_p->exp_str && a_assign_card_format_p->exp_str[0] != '[')
            value = pre_object_handler.Evaluate(a_assign_card_format_p->exp_str);
        else if (a_assign_card_format_p->exp_str && (a_assign_card_format_p->exp_str[0] == '[' || a_assign_card_format_p->exp_str[0] == '@'))
        {
            std::string expression = a_assign_card_format_p->exp_str;
            if (mysolverinf)
                value = mysolverinf->EvaluateDataCode(expression);
        }
    }
    break;
    case ASSIGN_GET_NLOOKUP_VALUE:
    {
        const ff_card_assign_nlookup_t      *a_nlookup_assign_card_format_p = (const ff_card_assign_nlookup_t *)card_p;
        IMECPreObject::MyValueType_e vtype = IMECPreObject::VTY_UNKNOWN;
        int a_id_attrib_ind = -1, a_nb_obj_attrib_ind = -1;

        string id_skeyword = a_descr_p->getSKeyword(a_nlookup_assign_card_format_p->id_ikey);
        attribute_type_e a_id_atype = a_descr_p->getAttributeType(a_nlookup_assign_card_format_p->id_ikey);
        if (a_id_atype == ATYPE_VALUE)
            a_id_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, id_skeyword.c_str());
        else if (a_id_atype == ATYPE_DYNAMIC_ARRAY || a_id_atype == ATYPE_STATIC_ARRAY)
            a_id_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, id_skeyword.c_str());
        int obj_id = 0;
        if (a_id_atype == ATYPE_VALUE && a_id_attrib_ind >= 0)
            obj_id = pre_object.GetObjectId(a_id_attrib_ind);
        else if ((a_id_atype == ATYPE_DYNAMIC_ARRAY || a_id_atype == ATYPE_STATIC_ARRAY) && a_id_attrib_ind >= 0)
            obj_id = pre_object.GetObjectId(a_id_attrib_ind, ind);

        value = mysolverinf->GetNlookupValue(a_nlookup_assign_card_format_p->table_num1_val, a_nlookup_assign_card_format_p->table_num2_val, obj_id);
    }
    break;
    case ASSIGN_GET_ENTITY_VALUE:
    {
        const ff_card_assign_entity_value_t* a_ent_val_assign_card_format_p = (const ff_card_assign_entity_value_t*)card_p;

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
                IMECPreObject::MyValueType_e a_row_vtype = pre_object.GetValueType(IMECPreObject::ATY_SINGLE, row_skey.c_str());
                if (a_row_vtype == IMECPreObject::VTY_INT)
                    row = pre_object.GetIntValue(row_skey.c_str());
                else if (a_row_vtype == IMECPreObject::VTY_UINT)
                    row = pre_object.GetUIntValue(row_skey.c_str());
            }
        }
        if (coloumn_ikeyword > 0)
        {
            attribute_type_e a_coloumn_atype = a_descr_p->getAttributeType(coloumn_ikeyword);
            if (a_coloumn_atype == ATYPE_VALUE)
            {
                string coloumn_skey = a_descr_p->getSKeyword(coloumn_ikeyword);
                IMECPreObject::MyValueType_e a_coloumn_vtype = pre_object.GetValueType(IMECPreObject::ATY_SINGLE, coloumn_skey.c_str());
                if (a_coloumn_vtype == IMECPreObject::VTY_INT)
                    coloumn = pre_object.GetIntValue(coloumn_skey.c_str());
                else if (a_coloumn_vtype == IMECPreObject::VTY_UINT)
                    coloumn = pre_object.GetUIntValue(coloumn_skey.c_str());
            }
        }
        if (mysolverinf)
            mysolverinf->UpdatePreObject(pre_object, a_descr_p, assign_card_attrib_skey, entity_skeyword, obj_value, row, coloumn);
        attribute_type_e a_atype = a_descr_p->getAttributeType(assign_card_attrib_ikey);
        if (a_atype == ATYPE_DYNAMIC_ARRAY)
        {
            int a_size_ikeyword = a_descr_p->getSizeIKeyword(assign_card_attrib_ikey);
            HCDI_UpdatePreObjectConnectedSizeIkeywords(pre_object, descr_p, a_size_ikeyword);
        }
        else if (a_atype == ATYPE_SIZE)
            HCDI_UpdatePreObjectConnectedSizeIkeywords(pre_object, descr_p, assign_card_attrib_ikey);
        return;
    }
    break;
    case ASSIGN_GET_CURRENT_ENTITY:
    {
        const ff_card_assign_entity_value_t      *a_ent_val_assign_card_format_p = (const ff_card_assign_entity_value_t *)card_p;
        string obj_type_str = a_ent_val_assign_card_format_p->objTypeStr;
        string obj_value = a_ent_val_assign_card_format_p->value_Skey;
        string assign_card_attrib_skey = a_descr_p->getSKeyword(assign_card_attrib_ikey);
        if (mysolverinf)
        {
            unsigned int current_entity_type = mysolverinf->GetEntityNameGetType(obj_type_str);
            mysolverinf->UpdateProbjectFromEntType(pre_object, a_descr_p, assign_card_attrib_skey, current_entity_type, obj_value);
        }
        attribute_type_e a_atype = a_descr_p->getAttributeType(assign_card_attrib_ikey);
        if (a_atype == ATYPE_SIZE)
            HCDI_UpdatePreObjectConnectedSizeIkeywords(pre_object, descr_p, assign_card_attrib_ikey);
        return;
    }
    break;
    case ASSIGN_GET_NEXT_MAX_AVAILABLE_ID:
    {
        if (mysolverinf)
        {
            value = mysolverinf->GetNextMaxAvailableId();
        }
    }
    break;
    case ASSIGN_GET_FORMAT_TYPE:
    {
        if (mysolverinf)
        {
            value = mysolverinf->GetFormatType();
        }
    }
    break;
    case ASSIGN_GET_DISPLAY_STATUS:
    {
        const ff_card_assign_displaystatus_t      *a_displaystatus_assign_card_format_p = (const ff_card_assign_displaystatus_t *)card_p;
        int f_ikey = a_displaystatus_assign_card_format_p->att_ikey;
        string f_skey = a_descr_p->getSKeyword(a_displaystatus_assign_card_format_p->att_ikey);
        attribute_type_e desc_a_type = a_descr_p->getAttributeType(f_ikey);
        value_type_e     desc_v_type = a_descr_p->getValueType(f_ikey);
        IMECPreObject::MyAttributeType_e a_attype = IMECPreObject::ATY_UNKNOWN;
        IMECPreObject::MyValueType_e vtype = IMECPreObject::VTY_UNKNOWN;
        HCDI_DescTypeToPreobjType(desc_a_type, desc_v_type, a_attype, vtype);

        int a_array_index = pre_object.GetIndex(a_attype, vtype, f_skey.c_str());
        if (a_array_index >= 0)
            value = 1;
        else
            value = 0;
    }
    break;
    case ASSIGN_PUSH:
    {
        const ff_card_assign_push_t      *a_assign_push_format_p = (const ff_card_assign_push_t *)card_p;
        int f_ikey = a_assign_push_format_p->att_ikey;
        string f_skey = a_descr_p->getSKeyword(a_assign_push_format_p->att_ikey);
        // Descriptor and Preobject Attribute and value type of attribute whose value has to be assigned(Second Argument of Assign card which is inside _PUSH())
        attribute_type_e desc_att_a_type = a_descr_p->getAttributeType(f_ikey);
        value_type_e desc_att_v_type = a_descr_p->getValueType(f_ikey);
        IMECPreObject::MyAttributeType_e pre_obj_att_Atype = IMECPreObject::ATY_UNKNOWN;
        IMECPreObject::MyValueType_e pre_obj_att_Vtype = IMECPreObject::VTY_UNKNOWN;
        HCDI_DescTypeToPreobjType(desc_att_a_type, desc_att_v_type, pre_obj_att_Atype, pre_obj_att_Vtype);

        // Descriptor and Preobject Attribute and value type of attribute to which value has to be assigned(First Argument of Assign card)
        attribute_type_e des_assign_att_atype = a_descr_p->getAttributeType(assign_card_attrib_ikey);
        IMECPreObject::MyAttributeType_e pre_obj_assign_att_Atype = IMECPreObject::ATY_UNKNOWN;
        IMECPreObject::MyValueType_e pre_obj_assign_att_Vtype = IMECPreObject::VTY_UNKNOWN;
        HCDI_DescTypeToPreobjType(des_assign_att_atype, a_result, pre_obj_assign_att_Atype, pre_obj_assign_att_Vtype);

        //Computing the size of the attribute whose value has to be assigned(Second Argument of Assign card which is inside _PUSH())
        int a_nb_values = 0; // 
        if (pre_obj_att_Atype == IMECPreObject::ATY_SINGLE)
            a_nb_values = 1;   // For Single Attribute number of values = 1
        else
        {
            if (desc_att_a_type == ATYPE_STATIC_ARRAY)
                a_nb_values = a_descr_p->getSize(f_ikey); // Size of the Static array
            else if (desc_att_a_type == ATYPE_DYNAMIC_ARRAY)
            {
                int a_size_ikeyword = a_descr_p->getSizeIKeyword(f_ikey);
                string a_size_skeyword = a_descr_p->getSKeyword(a_size_ikeyword);
                int a_size_index = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_size_skeyword.c_str());
                if (a_size_index > 0)
                    a_nb_values = pre_object.GetIntValue(a_size_index);
            }
        }
        int a_cur_arr_size = 0, a_updated_arr_size = 0;
        // Getting the Current array size of the attribute(First Argument of Assign card)
        int a_size_ikw = a_descr_p->getSizeIKeyword(assign_card_attrib_ikey);
        string a_size_skey = a_descr_p->getSKeyword(a_size_ikw);
        int a_size_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_size_skey.c_str());
        if(a_size_ind > 0)
            a_cur_arr_size = pre_object.GetIntValue(a_size_ind);
        //Resizing the array attribute(First Argument of Assign card)
        if (a_nb_values > 0)
        {
            a_updated_arr_size = a_nb_values + a_cur_arr_size; // Updating the array size
            HCDI_AddArrayAttributesToPreObject(pre_object, descr_p, assign_card_attrib_ikey, a_updated_arr_size);
        }
        else return;

        if (pre_obj_assign_att_Vtype == IMECPreObject::VTY_STRING)
        {
            string str_val = "";
            int a_att_index = pre_object.GetIndex(pre_obj_att_Atype, pre_obj_att_Vtype, f_skey.c_str());
            if (a_att_index >= 0)
            {
                if (pre_obj_att_Atype == IMECPreObject::ATY_SINGLE)
                {
                    HCDI_GetPreObjectValueAsString(pre_object, a_att_index, str_val, pre_obj_att_Vtype, -1);
                    HCDI_UpdatePreObjectStringValue(pre_object, descr_p, assign_card_attrib_ikey, str_val, a_updated_arr_size - 1);
                }
                else
                {
                    int a_indx = 0;
                    for (int i = a_cur_arr_size; i < a_updated_arr_size; i++)
                    {
                        if (a_indx < a_nb_values)
                        {
                            HCDI_GetPreObjectValueAsString(pre_object, a_att_index, str_val, pre_obj_att_Vtype, a_indx);
                        }
                        HCDI_UpdatePreObjectStringValue(pre_object, descr_p, assign_card_attrib_ikey, str_val, i);
                        a_indx++;
                    }
                }
            }
        }
        else
        {
            double a_value = 0;
            int a_att_index = pre_object.GetIndex(pre_obj_att_Atype, pre_obj_att_Vtype, f_skey.c_str());
            if (a_att_index >= 0)
            {
                if (pre_obj_att_Atype == IMECPreObject::ATY_SINGLE)
                {
                    std::string a_value_str = "";
                    HCDI_GetPreObjectValue(pre_object, a_att_index, a_value, a_value_str, pre_obj_att_Vtype, -1);
                    if (pre_obj_att_Vtype == IMECPreObject::VTY_OBJECT || pre_obj_att_Vtype == IMECPreObject::VTY_UINT || pre_obj_att_Vtype == IMECPreObject::VTY_BOOL)
                    {
                        HCDI_UpdatePreObjectValue(pre_object, descr_p, assign_card_attrib_ikey, (unsigned int)a_value, a_value_str, a_updated_arr_size - 1);
                    }
                    else if (pre_obj_att_Vtype == IMECPreObject::VTY_INT)
                    {
                        HCDI_UpdatePreObjectValue(pre_object, descr_p, assign_card_attrib_ikey, (int)a_value, a_value_str, a_updated_arr_size - 1);
                    }
                    else HCDI_UpdatePreObjectValue(pre_object, descr_p, assign_card_attrib_ikey, a_value, a_value_str, a_updated_arr_size - 1);
                }
                else
                {
                    int a_indx = 0;
                    if (pre_obj_att_Vtype == IMECPreObject::VTY_OBJECT || pre_obj_att_Vtype == IMECPreObject::VTY_UINT || pre_obj_att_Vtype == IMECPreObject::VTY_BOOL)
                    {
                        for (int i = a_cur_arr_size; i < a_updated_arr_size; i++)
                        {
                            std::string a_value_str = "";
                            if (a_indx < a_nb_values)
                            {
                                HCDI_GetPreObjectValue(pre_object, a_att_index, a_value, a_value_str, pre_obj_att_Vtype, a_indx);
                            }
                            HCDI_UpdatePreObjectValue(pre_object, descr_p, assign_card_attrib_ikey, (unsigned int)a_value, a_value_str, i);
                            a_indx++;
                        }
                    }
                    else if (pre_obj_att_Vtype == IMECPreObject::VTY_INT)
                    {
                        int a_val = 0;
                        std::string a_value_str = "";
                        for (int i = a_cur_arr_size; i < a_updated_arr_size; i++)
                        {
                            if (a_indx < a_nb_values)
                                a_val = pre_object.GetIntValue(a_att_index, a_indx);
                            HCDI_UpdatePreObjectValue(pre_object, descr_p, assign_card_attrib_ikey, a_val, a_value_str, i);
                            a_indx++;
                        }
                    }
                    else
                    {
                        for (int i = a_cur_arr_size; i < a_updated_arr_size; i++)
                        {
                            std::string a_value_str = "";
                            double a_val = 0;
                            if (a_indx < a_nb_values)
                                a_val = pre_object.GetFloatValue(a_att_index, a_indx);
                            HCDI_UpdatePreObjectValue(pre_object, descr_p, assign_card_attrib_ikey, a_val, a_value_str, i);
                            a_indx++;
                        }
                    }
                }
            }
        }
        if (des_assign_att_atype == ATYPE_DYNAMIC_ARRAY)
        {
            int a_size_ikeyword = a_descr_p->getSizeIKeyword(assign_card_attrib_ikey);
            HCDI_UpdatePreObjectConnectedSizeIkeywords(pre_object, descr_p, a_size_ikeyword);
        }
        return;
    }
    case ASSIGN_ATTRIB:
    {
        const ff_card_assign_Copy_t      *a_assign_Copy_card_format_p = (const ff_card_assign_Copy_t *)card_p;
        int ikeyword = a_assign_Copy_card_format_p->ikeyword;
        attribute_type_e a_attrib_atype = a_descr_p->getAttributeType(ikeyword);
        attribute_type_e assign_attrib_atype = a_descr_p->getAttributeType(assign_card_attrib_ikey);
        int index_ikey = a_assign_Copy_card_format_p->index_ikey;
        int last_index_ikey = a_assign_Copy_card_format_p->last_index_ikey;
        int index = -1, last_index = -1;
        int a_size = 0;
        //Getting the size of the array attributes
        if (a_attrib_atype == ATYPE_DYNAMIC_ARRAY || a_attrib_atype == ATYPE_STATIC_ARRAY)
        {
            value_type_e ikey_des_vtype = a_descr_p->getValueType(ikeyword);
            IMECPreObject::MyValueType_e ikey_pre_obj_Vtype = HCDIGetPVTypeFromDVType(ikey_des_vtype);

            string a_skeyword = a_descr_p->getSKeyword(ikeyword);
            int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, ikey_pre_obj_Vtype, a_skeyword.c_str());
            if (a_array_index < 0)
                return;
            a_size = pre_object.GetNbValues(ikey_pre_obj_Vtype, a_array_index);
            if (a_size == 0)
                return;
        }

        // Getting the index of the array attribute whose value has to be assigned(Second Argument of Assign card which is inside _ATTRIB()).
        if (index_ikey > 0) // ASSIGN(single_att, _ATTRIB(array_att, index_att), EXPORT) index_att is given here.
        {
            std::string index_val_str = "";
            double index_val = 0.0;
            pre_object.GetExpressionValue(a_descr_p, index_ikey, -1, index_val, index_val_str); // Getting the value of the index_att from its ikeyword.
            index = (int)index_val;
            if (last_index_ikey > 0) // last_index_ikey is allowed only when index_ikey > 0(parser doesn't allow otherwise)
            {
                double last_index_val = 0.0;
                pre_object.GetExpressionValue(a_descr_p, last_index_ikey, -1, last_index_val, index_val_str);// Getting the value of the last_index_att from its ikeyword.
                last_index = (int)last_index_val;
                if (last_index < 0 || last_index <= index)
                    return;
            }
        }
        /* a_attrib_atype -> Attribute whose value has to be assigned(Second Argument of Assign card which is inside _ATTRIB()) and
               assign_attrib_atype -> attribute to which value has to be assigned(First Argument of Assign card)*/
        else if ((a_attrib_atype == ATYPE_DYNAMIC_ARRAY || a_attrib_atype == ATYPE_STATIC_ARRAY) &&
            (assign_attrib_atype == ATYPE_VALUE || assign_attrib_atype == ATYPE_SIZE))
        {
            if (ind < 0) index = 0; //ASSIGN(single_att, _ATTRIB(array_att), EXPORT) Index is 0 to copy the arrat_att[0] to the single_att
            else index = ind; //CARD_LIST{ASSIGN(single_att, _ATTRIB(array_att), EXPORT)} Here the index is nothing but the index of CARD_LIST i.e., 'ind'.
        }
        //else ASSIGN(single_att, _ATTRIB(single_att), EXPORT) Index will be '-1' as single_att is being copied here.

        if (assign_attrib_atype == ATYPE_VALUE || assign_attrib_atype == ATYPE_SIZE) // Copying (Single_Att to Single_Att) & (Arr_Att[index] to Single_Att).
        {
            pre_object.GetExpressionValue(a_descr_p, ikeyword, index, value, value_str);
            if (assign_attrib_atype == ATYPE_SIZE)
                HCDI_UpdatePreObjectConnectedSizeIkeywords(pre_object, descr_p, ikeyword);
        }
        else if (assign_attrib_atype == ATYPE_DYNAMIC_ARRAY || assign_attrib_atype == ATYPE_STATIC_ARRAY) // copying Array_Att to Array_Att
        {
            if (index < 0 || index >= a_size || last_index >= a_size)
                return;

            int start_ind_value = 0, end_ind_value = a_size;
            int updated_array_size = a_size;
            if (index >= 0)
            {
                start_ind_value = index;
                if (last_index >= 0)
                {
                    end_ind_value = last_index + 1;
                    updated_array_size = end_ind_value - start_ind_value;
                }
            }
            if (assign_attrib_atype == ATYPE_DYNAMIC_ARRAY)
            {
                HCDI_AddArrayAttributesToPreObject(pre_object, descr_p, assign_card_attrib_ikey, updated_array_size);
                int a_size_ikeyword = a_descr_p->getSizeIKeyword(assign_card_attrib_ikey);
                HCDI_UpdatePreObjectConnectedSizeIkeywords(pre_object, descr_p, a_size_ikeyword);
            }
            else if (assign_attrib_atype == ATYPE_STATIC_ARRAY)
            {
                int size = a_descr_p->getSize(assign_card_attrib_ikey);
                HCDI_AddArrayAttributesToPreObject(pre_object, descr_p, assign_card_attrib_ikey, size);
                if (size < updated_array_size)
                {
                    updated_array_size = size;
                    end_ind_value = start_ind_value + size;
                }
            }
            for (int i = start_ind_value; i < end_ind_value; i++)
            {
                std::string a_value_str = "";
                pre_object.GetExpressionValue(a_descr_p, ikeyword, i, value, a_value_str);
                HCDI_UpdatePreObjectValue(pre_object, descr_p, assign_card_attrib_ikey, value, a_value_str, i);
            }
            return;
        }
        break;
    }
    case ASSIGN_ADD:
    case ASSIGN_SUB:
    case ASSIGN_DIV:
    case ASSIGN_MUL:
        {
            const ff_card_assign_basic_operations_t      *a_basic_opera_assign_card_format_p = (const ff_card_assign_basic_operations_t *)card_p;
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

            std::string first_val_str = "";
            if(f_ikey > 0)
                pre_object.GetExpressionValue(a_descr_p, f_ikey, ind_l, first_val, first_val_str);
            else
                first_val = a_basic_opera_assign_card_format_p->firstVal;

            std::string sec_val_str = "";
            if(s_ikey > 0)
                    pre_object.GetExpressionValue(a_descr_p, s_ikey, ind_r, sec_val, sec_val_str);
            else 
                sec_val = a_basic_opera_assign_card_format_p->secondVal;
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
            default:
                break;
            }
        }
        break;
    case ASSIGN_COMBINE:
    case ASSIGN_FIND:
    case ASSIGN_ERASE:
    {
        const ff_card_assign_string_t* a_assign_string_card_p = (const ff_card_assign_string_t*)card_p;
        int f_ikey = a_assign_string_card_p->first_ikey;
        int s_ikey = a_assign_string_card_p->second_ikey;
        std::string first_str = "";
        std::string second_str = "";
        if (f_ikey > 0)
        {
            string          a_skeyword = a_descr_p->getSKeyword(f_ikey);
            attribute_type_e a_atype = a_descr_p->getAttributeType(f_ikey);
            int a_index = -1;
            if (a_atype == ATYPE_STATIC_ARRAY || a_atype == ATYPE_DYNAMIC_ARRAY)
                a_index = ind;
            int a_attrib_index = -1;
            a_attrib_index = a_index < 0 ? pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, a_skeyword.c_str()) :
                pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_skeyword.c_str());
            if (a_attrib_index >= 0)
            {
                const char* a_str_val = a_index < 0 ?
                    pre_object.GetStringValue(a_attrib_index) :
                    pre_object.GetStringValue(a_attrib_index, a_index);
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
            string          a_skeyword = a_descr_p->getSKeyword(s_ikey);
            attribute_type_e a_atype = a_descr_p->getAttributeType(s_ikey);
            int a_index = -1;
            if (a_atype == ATYPE_STATIC_ARRAY || a_atype == ATYPE_DYNAMIC_ARRAY)
                a_index = ind;
            int a_attrib_index = -1;
            a_attrib_index = a_index < 0 ? pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, a_skeyword.c_str()) :
                pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_skeyword.c_str());
            if (a_attrib_index >= 0)
            {
                const char* a_str_val = a_index < 0 ?
                    pre_object.GetStringValue(a_attrib_index) :
                    pre_object.GetStringValue(a_attrib_index, a_index);
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
            int a_skey_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, skeyword.c_str());
            if (a_skey_ind >= 0)
                pre_object.SetStringValue(a_skey_ind, final_str.c_str());
            else
                pre_object.AddStringValue(skeyword.c_str(), final_str.c_str());
        }
        else if (atype == ASSIGN_FIND)
        {
            int found = 0;
            size_t pos = first_str.find(second_str);
            if (pos != std::string::npos)
                found = 1;
            int a_skey_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, skeyword.c_str());
            if (a_skey_ind >= 0)
                pre_object.SetIntValue(a_skey_ind, found);
            else
                pre_object.AddIntValue(skeyword.c_str(), found);
        }
        else if (atype == ASSIGN_ERASE)
        {
            size_t pos = first_str.find(second_str);
            if (pos != std::string::npos)
                first_str.erase(pos, second_str.length());
            int a_skey_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, skeyword.c_str());
            if (a_skey_ind >= 0)
                pre_object.SetStringValue(a_skey_ind, first_str.c_str());
            else
                pre_object.AddStringValue(skeyword.c_str(), first_str.c_str());
        }
        return;
    }
    default:
        break;
    }
    attribute_type_e a_attrib_type = a_descr_p->getAttributeType(assign_card_attrib_ikey);
    if(ind >= 0)
    {
        if (a_attrib_type == ATYPE_VALUE || a_attrib_type == ATYPE_SIZE)
            ind = -1;
    }
    HCDI_UpdatePreObjectValue(pre_object, descr_p, assign_card_attrib_ikey, value, value_str, ind);
    if (a_attrib_type == ATYPE_SIZE)
        HCDI_UpdatePreObjectConnectedSizeIkeywords(pre_object, descr_p, assign_card_attrib_ikey);
}

void MECDataWriter::GetSingleCardCellInfo(const PseudoFileFormatCard_t *card_p,
    const IMECPreObject          &pre_object,
    const PseudoDescriptor_t     *descr_p,
    vector<card_cells_temp_t>    &loc_cell_lst)
{
    const ff_card_t *a_card_p = (const ff_card_t *)card_p;
    const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p;
    //
    int a_nb_cells = 0;
    int a_cell_length = 0;
    MCDS_get_ff_card_attributes(a_card_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);
    for (int k = 0; k < a_nb_cells; ++k) {
        ff_cell_t *a_cell_p = NULL;
        //
        MCDS_get_ff_card_tab(a_card_p, CARD_CELL, k, (void *)(&a_cell_p));
        //WriteCell((const PseudoFileFormatCell_t *)a_cell_p,pre_object,descr_p,ind);
        ff_cell_type_e a_cell_type = CELL_UNKNOWN;
        MCDS_get_ff_cell_attributes(a_cell_p, CELL_TYPE, &a_cell_type, END_ARGS);
        card_cells_temp_t     card_cell;
        card_cell.a_cell_p = a_cell_p;
        card_cell.cell_type = a_cell_type;
        card_cell.ikeyword = -1;
        card_cell.skeyword = "";
        card_cell.width = 0;
        card_cell.fmt = "";
        card_cell.val_type = VTYPE_UNKNOWN;
        card_cell.att_type = ATYPE_UNKNOWN;
        card_cell.att_iden_val = -1;
        card_cell.has_default = false;
        card_cell.att_indx = -1;
        card_cell.att_default_bool = false;
        card_cell.att_default_int = 0;
        card_cell.att_default_double = 0.;
        if (a_cell_type == CELL_VALUE)
        {
            const char           *a_cell_fmt = NULL;
            MCDS_get_ff_cell_attributes(a_cell_p, CELL_FORMAT, &a_cell_fmt, END_ARGS);
            int         a_cell_ikw = 0;
            MCDS_get_ff_cell_attributes(a_cell_p, CELL_IKEYWORD, &a_cell_ikw, END_ARGS);
            string       a_cell_skw = a_descr_p->getSKeyword(a_cell_ikw); //
            value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw); //
            attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);

            int att_iden_val = a_descr_p->getIdentifierValue(DOM_COMMON, a_cell_ikw);
            bool has_default = false;


			int fmt_size = 0;
			a_cell_fmt = GetFormatSize(a_cell_fmt, fmt_size);  //
            card_cell.ikeyword = a_cell_ikw;
            card_cell.skeyword = a_cell_skw;
            card_cell.width = fmt_size;
            card_cell.fmt = a_cell_fmt;
            card_cell.val_type = a_cell_vtype;
            card_cell.att_type = a_atype;
            card_cell.att_iden_val = att_iden_val;
            card_cell.has_default = has_default;


            int a_attrib_ind = -1;
            IMECPreObject::MyAttributeType_e atttype = IMECPreObject::ATY_SINGLE;
            if (a_atype == ATYPE_STATIC_ARRAY || a_atype == ATYPE_DYNAMIC_ARRAY)
                atttype = IMECPreObject::ATY_ARRAY;
            if (a_cell_vtype == VTYPE_BOOL)
            {
                a_attrib_ind = pre_object.GetIndex(atttype, IMECPreObject::VTY_BOOL, a_cell_skw);
                bool a_value_d = a_descr_p->getBoolDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
                card_cell.has_default = has_default;
                if (has_default)
                    card_cell.att_default_bool = a_value_d;
            }

            else if (a_cell_vtype == VTYPE_FLOAT)
            {
                a_attrib_ind = pre_object.GetIndex(atttype, IMECPreObject::VTY_FLOAT, a_cell_skw);
                double a_value_d = a_descr_p->getFloatDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
                card_cell.has_default = has_default;
                if (has_default)
                    card_cell.att_default_double = a_value_d;
            }
            else if (a_cell_vtype == VTYPE_INT)
            {
                a_attrib_ind = pre_object.GetIndex(atttype, IMECPreObject::VTY_INT, a_cell_skw);
                int a_value_d = a_descr_p->getIntDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
                card_cell.has_default = has_default;
                if (has_default)
                    card_cell.att_default_int = a_value_d;
            }
            else if (a_cell_vtype == VTYPE_OBJECT)
                a_attrib_ind = pre_object.GetIndex(atttype, IMECPreObject::VTY_OBJECT, a_cell_skw);
            else if (a_cell_vtype == VTYPE_UINT)
            {
                a_attrib_ind = pre_object.GetIndex(atttype, IMECPreObject::VTY_UINT, a_cell_skw);
                unsigned int a_value_d = a_descr_p->getUIntDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
                card_cell.has_default = has_default;
                if (has_default)
                    card_cell.att_default_int = a_value_d;
            }
            else if (a_cell_vtype == VTYPE_STRING)
            {
                a_attrib_ind = pre_object.GetIndex(atttype, IMECPreObject::VTY_STRING, a_cell_skw);
                string a_value_d = a_descr_p->getStringDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
                card_cell.has_default = has_default;
                if (has_default)
                    card_cell.att_default_string = a_value_d;
            }
            card_cell.att_indx = a_attrib_ind;
        }
        loc_cell_lst[k] = card_cell;
    }
}

bool MECDataWriter::GetSingleCardDisplayStatus(const PseudoFileFormatCard_t *card_p,
	                                           const IMECPreObject          &pre_object,
	                                           const PseudoDescriptor_t     *descr_p,
	                                           int                           ind,
	                                           int                           cell_ind0)
{
	const ff_card_t *a_card_p = (const ff_card_t *)card_p;
	const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p;
	bool haveVal = false;
    ff_card_type_e a_card_type = CARD_UNKNOWN;
    MCDS_get_ff_card_attributes(a_card_p, CARD_TYPE, &a_card_type, END_ARGS);
    //
    if (a_card_type == CARD_HEADER)
        haveVal = true;
    else
    {
        int a_nb_cells = 0;
        MCDS_get_ff_card_attributes(a_card_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);
        for (int j = cell_ind0; j < a_nb_cells; ++j)
        {
            ff_cell_t *a_cell_p = NULL;
            //
            MCDS_get_ff_card_tab(a_card_p, CARD_CELL, j, (void *)(&a_cell_p));
            if (j == 0 && a_cell_p->type == CELL_COMMENT)
                continue;
            haveVal = GetCellDisplayStatus((const PseudoFileFormatCell_t *)a_cell_p, pre_object, descr_p, ind);
            if (haveVal == true)
                return haveVal;
        }
        //
    }
    return haveVal;
}


bool MECDataWriter::WriteSingleCard(const PseudoFileFormatCard_t *card_p,
                                    const IMECPreObject       &pre_object,
                                    const PseudoDescriptor_t *descr_p,
                                    int                       ind,
                                    int                       cell_ind0)
{
  const ff_card_t *a_card_p=(const ff_card_t *)card_p;
  const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p; 
  //
  
  int a_free_ikw=END_ARGS;
  MCDS_get_ff_card_attributes(a_card_p,CARD_IS_FREE,&a_free_ikw,END_ARGS);
  if (END_ARGS!=a_free_ikw) {
      // the card is a FREE_CARD
      string a_free_skw = a_descr_p->getSKeyword(a_free_ikw);
    value_type_e a_cell_vtype = a_descr_p->getValueType(a_free_ikw);
    int a_do_write_card = 0;
    if(a_cell_vtype == VTYPE_BOOL)
    a_do_write_card = pre_object.GetBoolValue(a_free_skw.c_str());
    else if(a_cell_vtype == VTYPE_UINT)
        a_do_write_card = pre_object.GetUIntValue(a_free_skw.c_str());
    else
        a_do_write_card = pre_object.GetIntValue(a_free_skw.c_str());
      if (!a_do_write_card) { // ... and is not to be written
          return false;
      }
  }
  
  int flag = 0;
  bool no_end_flag = false;
  MCDS_get_ff_card_attributes(a_card_p, CARD_FLAGS, &flag, END_ARGS);
  if (flag)
      no_end_flag = (flag & CARD_FLAG_NO_END) ? true : false;
  int a_nb_cells = 0;
  int a_cell_length = 0;
  MCDS_get_ff_card_attributes(a_card_p,CARD_NB_CELLS,&a_nb_cells,END_ARGS);
  myIsLastCell = false; // Always set this flag to false before entering the nb_cells loop.
  myCurActiveLineLength = 0;
  for (int j = cell_ind0; j < a_nb_cells; ++j) {
      ff_cell_t* a_cell_p = NULL;
      //
      MCDS_get_ff_card_tab(a_card_p, CARD_CELL, j, (void*)(&a_cell_p));
      if (myWriteFreeFormat == true && j == a_nb_cells - 1 && !no_end_flag)
          myIsLastCell = true;
      WriteCell((const PseudoFileFormatCell_t*)a_cell_p, pre_object, descr_p, ind);
  }
  myIsLastCell = false; // Always set this flag as false after exiting from the nb_cells loop.
  //
  ff_card_type_e a_card_type=CARD_UNKNOWN;
  MCDS_get_ff_card_attributes(a_card_p,CARD_TYPE,&a_card_type,END_ARGS);
 // if(a_card_type==CARD_HEADER && (GetApplicationMode()==CODE_IS_RADIOSS || GetApplicationMode()==CODE_IS_RADIOSS_EXPLI ||
 //     GetApplicationMode()==CODE_IS_RADIOSS_IMPLI))

  if (a_card_type == CARD_HEADER )
  {
      if ((myFileFormatId >= FF_D00_5X && myFileFormatId < FF_D00_LAST)) /*need to move this as well to appendoptions*/
      {
          int unit_id = pre_object.GetUnitId();
          if (unit_id)
          {
              myWriteContext_p->WriteFile("/");
              myWriteContext_p->WriteFile("%d", unit_id);
          }
      }
      AppendHeaderCardOptions(card_p, pre_object, descr_p, ind, cell_ind0);
  }



  if (getFreeFormatFlag())
      myWriteContext_p->RemoveBlankEnd();
  if (!no_end_flag)
      myWriteContext_p->WriteFile("\n");
  return true;
}

bool MECDataWriter::WriteSingleCardList(const PseudoFileFormatCard_t *card_p,
    ff_card_type_e             card_type,
    const IMECPreObject       &pre_object,
    const PseudoDescriptor_t *descr_p,
    int                       ind,
    int                       cell_ind0,
    vector<card_cells_temp_t>  &card_cells)
{
    const ff_card_t *a_card_p=(const ff_card_t *)card_p;
    const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p; 
    int a_nb_cells = 0;
    int a_cell_length = 0;
    //MCDS_get_ff_card_attributes(a_card_p,CARD_NB_CELLS,&a_nb_cells,END_ARGS);
    a_nb_cells = (int)card_cells.size();
    myIsLastCell = false; // Always set this flag to false before entering the nb_cells loop.

    myCurActiveLineLength = 0;
    for(int j=cell_ind0;j<a_nb_cells;++j) 
    {
		if (myWriteFreeFormat == true && j == a_nb_cells - 1)
            myIsLastCell = true;
        ff_cell_t *a_cell_p=card_cells[j].a_cell_p;
        //
        //MCDS_get_ff_card_tab(a_card_p,CARD_CELL,j,(void *)(&a_cell_p));
        ff_cell_type_e a_cell_type = CELL_UNKNOWN;
        a_cell_type = card_cells[j].cell_type;
        if(CELL_VALUE == a_cell_type) 
        {
            myCurActiveLineLength += card_cells[j].width;
            WriteCell_VALUE_LIST((const PseudoFileFormatCell_t *)a_cell_p,pre_object,descr_p, &card_cells[j],ind);
        }
        else
        {
            WriteCell((const PseudoFileFormatCell_t *)a_cell_p,pre_object,descr_p,ind);
        }
    }
    myIsLastCell = false; // Always set this flag as false after exiting from the nb_cells loop.
    //
   // ff_card_type_e a_card_type=CARD_UNKNOWN;
   // MCDS_get_ff_card_attributes(a_card_p,CARD_TYPE,&a_card_type,END_ARGS);

    if(card_type==CARD_HEADER && (myFileFormatId >= FF_D00_5X && myFileFormatId <= FF_D00_2020X) )
    {
        int unit_id = pre_object.GetUnitId();
        if(unit_id)
  {
            myWriteContext_p->WriteFile("/");
            WriteInteger("%d", unit_id); 
        }
  }
    if(getFreeFormatFlag())
        myWriteContext_p->RemoveBlankEnd();
    myWriteContext_p->WriteFile("\n");

    return true; 
}







void MECDataWriter::WriteList(const PseudoFileFormatCard_t *card_p,
                              const IMECPreObject       &pre_object,
                              const PseudoDescriptor_t *descr_p)
{
  const ff_card_t      *a_card_p  = (const ff_card_t *)card_p;
  const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p;
  //
  // Line length
  int   a_line_length = -1, ikey = -1;
  MCDS_get_ff_card_attributes(a_card_p,CARD_LENGTH_MAX,&a_line_length,END_ARGS);
  MCDS_get_ff_card_attributes(a_card_p, CARD_IKEYWORD_NB_BLOCKS, &ikey, END_ARGS);

  // Number of cells
  int a_nb_cells = 0;
  MCDS_get_ff_card_attributes(a_card_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);

  if(a_line_length<=0) {
      a_line_length = myLineLength;
  }
  //
  // Writing
  int a_length=0, a_length_n=0;
  bool do_write_newline = false;
  int a_nb_cells_written = 0;
  myIsLastCell = false; // Always set this flag to false before entering the nb_cells loop.
  myCurActiveLineLength = 0;
  for(int i=0;i<a_nb_cells;++i) {
      ff_cell_t *a_cell_p=NULL;
      MCDS_get_ff_card_tab(a_card_p,CARD_CELL,i,(void *)(&a_cell_p));
      int a_cell_length=GetCellSize(a_cell_p);
      if (a_cell_length && ikey > 0)
      {
          string skey = a_descr_p->getSKeyword(ikey);
          int nb_blocks = pre_object.GetIntValue(skey.c_str());
          a_line_length = a_cell_length * nb_blocks;
      }
      //
      int a_cell_ikw=GetCellIkeyword((PseudoFileFormatCell_t*) a_cell_p);
		// to be enhanced for a card type NAME_VALUE when present 
      //
      int a_nb_values = 0;
      
      bool is_array = false;
      attribute_type_e a_attr_type = a_descr_p->getAttributeType(a_cell_ikw);
      if(a_descr_p->isMultiDimensionalArray(a_cell_ikw))
      {
          a_nb_values = getMultidimensionalArraySize(a_cell_ikw,&pre_object,descr_p);
          is_array = true;
      }
      
      else if (a_attr_type == ATYPE_STATIC_ARRAY) {
          a_nb_values = a_descr_p->getSize(a_cell_ikw);
          is_array = true;
        }
        else if (a_attr_type == ATYPE_DYNAMIC_ARRAY) {
          int a_size_ikw = a_descr_p->getSizeIKeyword(a_cell_ikw);
          
          string a_size_skw = a_descr_p->getSKeyword(a_size_ikw);
          a_nb_values = pre_object.GetIntValue(a_size_skw.c_str());
          
          is_array = true;
      }
		if (myWriteFreeFormat == true && i == a_nb_cells - 1)
            myIsLastCell = true;
      //
      if (!is_array) {
          // Write newline if necessary (CS#17_01_11)
          if(do_write_newline) {
              WriteNewline();
              do_write_newline=false;
          }
          // Write cell itself
          a_nb_cells_written++;
          WriteCell((const PseudoFileFormatCell_t *)a_cell_p,pre_object,descr_p);
          a_length+=a_cell_length;
          a_length_n += a_cell_length;

          if(a_length>=a_line_length) {
              do_write_newline = true;
              a_length=0;
              a_length_n = 0;
          }
          else if (a_length_n >= myLineLength)
          {
              do_write_newline = true;
              a_length_n = 0;
          }
        }
        else {
          for(int j=0;j<a_nb_values;j++) {
              // Write newline if necessary (CS#17_01_11)
              if(do_write_newline) {
                  WriteNewline();
                  do_write_newline=false;
              }
              // Write cell itself
              a_nb_cells_written++;
              WriteCell((const PseudoFileFormatCell_t *)a_cell_p,pre_object,descr_p,j);
              a_length+=a_cell_length;
              a_length_n+= a_cell_length;

              if (a_length >= a_line_length) 
              {
                  do_write_newline = true;
                  a_length=0;
                  a_length_n = 0;
              }
              else if(a_length_n >= myLineLength)
              {
                  do_write_newline = true;
                  a_length_n = 0;
              }
          }
      }
  }
  myIsLastCell = false; // Always set this flag as false after exiting from the nb_cells loop.
  if(a_nb_cells_written>0) myWriteContext_p->WriteFile("\n");
}


void MECDataWriter::WriteCellList(const PseudoFileFormatCard_t* card_p,
    const IMECPreObject& pre_object,
    const PseudoDescriptor_t* descr_p,
    int                       ind,
    const PseudoFileFormatCard_t* prev_card_p)
{
    const ff_card_t* a_card_p = (const ff_card_t*)card_p;
    const MvDescriptor_t* a_descr_p = (const MvDescriptor_t*)descr_p;
    // Number of values
    int a_nb_values = 0;
    int a_nb_values_ikw = END_ARGS;
    int a_is_free = 0;
    int a_nb_value = 0;
    bool a_is_freeformat = false;
    MCDS_get_ff_card_attributes(a_card_p,
        CARD_SIZE, &a_nb_values_ikw,
        CARD_IS_FREE, &a_is_free,
        END_ARGS);
    if (a_nb_values_ikw < 0) {
        a_nb_value = (-a_nb_values_ikw);
    }
    else {
        string a_nb_values_skw = a_descr_p->getSKeyword(a_nb_values_ikw);
        a_nb_value = pre_object.GetIntValue(a_nb_values_skw.c_str());
    }
    
    a_nb_values = getMultidimensionalArraySize(card_p, &pre_object, descr_p);
    if (a_nb_values == 0) a_nb_values = a_nb_value;
    
      // Line length
    int a_line_length = -1, line_length = -1;
    MCDS_get_ff_card_attributes(a_card_p,
        CARD_LENGTH_MAX, &line_length,
        END_ARGS);
    a_line_length = line_length;
    a_line_length = GetLineLength(a_line_length, myLineLength);
    
    if (a_line_length <= 0 || a_line_length > myLineLength) a_line_length = myLineLength; 
    int a_first_line_offset = 0;
    if (prev_card_p)
    {
        const ff_card_t* a_prev_card_p = (const ff_card_t*)prev_card_p;
        int flag = 0;
        MCDS_get_ff_card_attributes(a_prev_card_p, CARD_FLAGS, &flag, END_ARGS);
        if (flag)
        {
            bool no_end_flag = (flag & CARD_FLAG_NO_END) ? true : false;
            if (no_end_flag)
            {
                int a_nb_cells = 0;
                MCDS_get_ff_card_attributes(a_prev_card_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);
                for (int i = 0; i < a_nb_cells; ++i)
                {
                    ff_cell_t* a_cell_p = NULL;
                    MCDS_get_ff_card_tab(a_prev_card_p, CARD_CELL, i, (void*)(&a_cell_p));
                    a_first_line_offset += GetCellSize(a_cell_p);
                }
            }
        }
    }

    // Number of cells
    int a_nb_cells = 0;
    int a_val_pair = 0;
    MCDS_get_ff_card_attributes(a_card_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);
    // Length of cells
    int* a_cell_length_tabf = (int*)mymalloc(a_nb_cells * sizeof(int));
    for (int i = 0; i < a_nb_cells; ++i) {
        ff_cell_t* a_cell_p = NULL;
        MCDS_get_ff_card_tab(a_card_p, CARD_CELL, i, (void*)(&a_cell_p));
        //
        a_cell_length_tabf[i] = GetCellSize(a_cell_p);

        /*Added to enable flag for free format*/
        if (0 == a_cell_length_tabf[i])
        {
            a_val_pair++;
            a_is_freeformat = true;
        }
    }
    int flag = 0;
    const char* a_offset_fmt = NULL;
    const char* a_offset_val = NULL;
    bool has_offset = false;
    int block_length = 0;
    MCDS_get_ff_card_attributes(a_card_p, CARD_FLAGS, &flag, END_ARGS);
    if (flag)
    {
        bool block_together = (flag & CARD_FLAG_BLOCK_TOGETHER) ? true : false;
        if (block_together)
        {
            for (int i = 0; i < a_nb_cells; ++i)
                block_length += a_cell_length_tabf[i];
        }
        has_offset = (flag & CARD_FLAG_OFFSET) ? true : false;
        if (has_offset)
        {
            MCDS_get_ff_card_attributes(a_card_p, CARD_CELL_LIST_OFFSET_FORMAT, &a_offset_fmt, CARD_CELL_LIST_OFFSET_VALUE, &a_offset_val, END_ARGS);
        }
    }

    // Writing
    int a_length = 0;
    /* In case of CELL_LIST or FREE_CELL_LIST which is being followed by card with NO_END flag,
       the previous card(card with no_end flag) length needs to be added to a_length
       since everything is written in the same line */
    bool do_write_newline = false;
    if (a_first_line_offset > 0)
    {
        if (a_first_line_offset < a_line_length)
        {
            a_length = a_length + a_first_line_offset;
            if (a_length > a_line_length)
            {
                do_write_newline = true;
                a_length = 0;
            }
        }
        else
            do_write_newline = true;
    }

    int start = 0;
    int end = a_nb_values;
    if (ind >= 0)
    {
        int a_cell_ikw = isMultiArray(card_p, descr_p);
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
                    int a_val = pre_object.GetIntValue(a_index_skw.c_str());
                    n_val = a_val;
                }
                else
                {
                    n_val = sizeArrayVector[size - 1].size;
                }
                start = start + n_val * ind;
                end = n_val + n_val * ind;
            }
        }
        else
        {
            start = a_nb_values * ind;
            end = start + a_nb_values;
        }
    }

    myCurActiveLineLength = 0;
    bool do_write_newline_freeformat = false;
    for (int j = start; j < end; ++j) {
        // If BLOCK_TOGETHER exists for the CELL_LIST, the decision whether to proceed to the next line is being taken below
        if (block_length)
        {
            int a_total_length_with_blocked = a_length + block_length;
            if (a_total_length_with_blocked > a_line_length)
            {
                do_write_newline = true;
                a_length = 0;
            }
        }
        myIsLastCell = false; // Always set this flag to false before entering the nb_cells loop.
        for (int k = 0; k < a_nb_cells; ++k) {
            ff_cell_t* a_cell_p = NULL;
            MCDS_get_ff_card_tab(a_card_p, CARD_CELL, k, (void*)(&a_cell_p));

            a_length += a_cell_length_tabf[k];
            /*if card is free format then always write only 5 values in a line. Reason: Don't know how many character are written in WriteCell*/
            if (a_is_freeformat && block_length == 0)
            {
                int no_max_pair = 5;
                //Assuming 20 as the size of each cell
                if (a_val_pair > 1)
                    no_max_pair = (int)(myLineLength / (a_val_pair * 20));

                do_write_newline_freeformat = false;
                if ((((j + 1) % no_max_pair == 0) && k == (a_nb_cells - 1)) ||
                    ((line_length == a_line_length) && (k == a_nb_cells - 1)))
                {
                    do_write_newline_freeformat = true;
                }
            }
            if ((a_length > a_line_length) || (do_write_newline_freeformat)) {
                do_write_newline = true;
                a_length = a_cell_length_tabf[k];
            }
            // Write newline if necessary (CS#17_01_11)
            if (do_write_newline) {
                WriteNewline();
                do_write_newline = false;
                if (has_offset) {
                    int fmt_size = 0;
                    a_offset_fmt = GetFormatSize(a_offset_fmt, fmt_size);
                    if (myWriteFreeFormat == false)
                    {
                        char temp_cell_fmt[10];
                        sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                        myWriteContext_p->WriteFile(a_offset_fmt, a_offset_val);
                    }
                    else
                    {
                        myWriteContext_p->WriteFile("%s", a_offset_val);
                        string str(mydelimiter);
                        myWriteContext_p->WriteFile(str.c_str());
                    }
                    a_length += fmt_size;
                }
            }
            // to decide whether the current cell is going to be the last cell being written
            // Add the next cell length and see if it greater than the line length, the current cell is going to be last cell in the current line.
            // if block_Together usecase needs to be supported(for last comma to be not written), make changes here with respect to addition of block length
            int a_next_cell_included_length = 0;
            if (k == a_nb_cells - 1)
                a_next_cell_included_length = a_length + a_cell_length_tabf[0];
            else a_next_cell_included_length = a_length + a_cell_length_tabf[k + 1];

            //(j == a_nb_values-1 && k == a_nb_cells - 1) -> denotes last cell to be written.
            // a_next_cell_included_length > a_line_length -> denotes last cell  to be written in that line.
            // in both the above cases myIsLastCell needs to be set to true.
            if (myWriteFreeFormat == true && ((j == a_nb_values - 1 && k == a_nb_cells - 1) || a_next_cell_included_length > a_line_length))
                myIsLastCell = true;
            //if (myWriteFreeFormat == true && k == a_nb_cells - 1)
                //myIsLastCell = true;
            // Write cell itself
            WriteCell((const PseudoFileFormatCell_t*)a_cell_p, pre_object, descr_p, j);
        }

    }
    myIsLastCell = false; // Always set this flag as false after exiting from the nb_cells loop.
    if (a_nb_values > 0) myWriteContext_p->WriteFile("\n");
    // Freeing memory
    myfree(a_cell_length_tabf);
}

void MECDataWriter::WriteObjectList(const PseudoFileFormatCard_t *card_p,
				       const IMECPreObject       &pre_object,
				       const PseudoDescriptor_t *descr_p)
{
  const ff_card_t      *a_card_p  = (const ff_card_t *)card_p;
  const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p;
  //
  const char *a_comment        = NULL;
  const char *a_cell_format    = NULL;
  int         a_nb_pos_ids_ikw = END_ARGS;
  int         a_pos_ids_ikw    = END_ARGS;
  int         a_nb_neg_ids_ikw = END_ARGS;
  int         a_neg_ids_ikw    = END_ARGS;
  //
  MCDS_get_ff_card_attributes(a_card_p,
			      CARD_STRING,     &a_comment,
			      CARD_CELL_FORMAT,&a_cell_format,
			      CARD_POS_SIZE,   &a_nb_pos_ids_ikw,
			      CARD_POS_ARRAY,  &a_pos_ids_ikw,
			      CARD_NEG_SIZE,   &a_nb_neg_ids_ikw,
			      CARD_NEG_ARRAY,  &a_neg_ids_ikw,
			      END_ARGS);
  // Positive ids
  string a_nb_pos_ids_skw = a_descr_p->getSKeyword(a_nb_pos_ids_ikw);
  int    a_nb_pos_ids     = pre_object.GetIntValue(a_nb_pos_ids_skw.c_str());
  string a_pos_ids_skw    = a_descr_p->getSKeyword(a_pos_ids_ikw);
  int    a_pos_ids_ind    = pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_OBJECT,a_pos_ids_skw);
  // Negative ids
  int a_nb_neg_ids  = 0;
  int a_neg_ids_ind = -1;
  if(a_nb_neg_ids_ikw!=END_ARGS) {
    string a_nb_neg_ids_skw=a_descr_p->getSKeyword(a_nb_neg_ids_ikw);
    a_nb_neg_ids=pre_object.GetIntValue(a_nb_neg_ids_skw.c_str());
    string a_neg_ids_skw=a_descr_p->getSKeyword(a_neg_ids_ikw);
    a_neg_ids_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_OBJECT,a_neg_ids_skw);
  }
  // Writing
  int i;
  int a_line_length = -1;
  MCDS_get_ff_card_attributes(a_card_p,
					     CARD_LENGTH_MAX,&a_line_length,
					     END_ARGS);
  if(a_line_length<=0 || a_line_length > myLineLength) a_line_length=myLineLength; 
  int a_cell_length = 0;
  a_cell_format = GetFormatSize(a_cell_format, a_cell_length);
  //
  if((a_nb_pos_ids>0 || a_nb_neg_ids>0) && strlen(a_comment)>0) myWriteContext_p->WriteFile("%s\n",a_comment);
  //
  int a_length=0;
  for(i=0;i<a_nb_pos_ids;++i) {
        MYOBJ_INT a_id = pre_object.GetObjectId(a_pos_ids_ind, i);
    myWriteContext_p->WriteFile(a_cell_format,a_id);
    a_length+=a_cell_length;
    if(a_length>=a_line_length) {
      myWriteContext_p->WriteFile("\n");
      a_length=0;
    }
  }
  for(i=0;i<a_nb_neg_ids;++i) {
    int a_id=pre_object.GetObjectId(a_neg_ids_ind,i);
    myWriteContext_p->WriteFile(a_cell_format,-a_id);
    a_length+=a_cell_length;
    if(a_length>=a_line_length) {
      myWriteContext_p->WriteFile("\n");
      a_length=0;
    }
  }
  if(a_length>0) myWriteContext_p->WriteFile("\n");
}
 
void MECDataWriter::WriteCardList(const PseudoFileFormatCard_t *card_p,
		     const IMECPreObject       &pre_object,
		     const PseudoDescriptor_t *descr_p,
			 const MECIModelScanner       *model_p)
{
  const ff_card_t      *a_card_p  = (const ff_card_t *)card_p;
  const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p;
  // Number of values, number of sub-cards
  int a_nb_values_ikw = END_ARGS;
  int a_nb_values     = 0;
  int a_nb_sub_cards  = 0;
  MCDS_get_ff_card_attributes(a_card_p,
			      CARD_SIZE,    &a_nb_values_ikw,
			      CARD_NB_CARDS,&a_nb_sub_cards,
			      END_ARGS);
  if(a_nb_values_ikw>0) {
    string a_nb_values_skw=a_descr_p->getSKeyword(a_nb_values_ikw);
    a_nb_values=pre_object.GetIntValue(a_nb_values_skw.c_str());
    }
    else {
    a_nb_values=(-a_nb_values_ikw);
  }

    if (!a_nb_values) 
        return;

    MvIKeywordSet_t       a_main_ikws;
    a_descr_p->getMainArrayIKeywords(&a_main_ikws);

    static string a_main_export = "MAINS_EXPORT";
    const MvIKeywordSet_t& a_ikeywords_exportprescan = a_descr_p->getDefinition(DOM_COMMON, a_main_export);

    bool is_mains_export_allocated = true;
    //check if all the ikeywords are not allocated
    if (a_ikeywords_exportprescan.size())
    {
    MvIKeywordSet_t a_secondary_ikws;
    a_descr_p->getSizeConnectedIKeywords(a_nb_values_ikw, &a_secondary_ikws);
    MvIKeywordSet_t::iterator a_it_begin = a_secondary_ikws.begin();
    MvIKeywordSet_t::iterator a_it_end = a_secondary_ikws.end();
    MvIKeywordSet_t::iterator a_it;

    for (a_it = a_it_begin; a_it != a_it_end; ++a_it) {
        int ikeyword = *a_it;
        string a_secondary_skw = a_descr_p->getSKeyword(ikeyword);
        IMECPreObject::MyValueType_e  a_secondary_vtype = pre_object.GetValueType(IMECPreObject::ATY_ARRAY, a_secondary_skw.c_str());
        int  a_secondary_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, a_secondary_vtype, a_secondary_skw);

        {
            const bool is_in = a_ikeywords_exportprescan.find(ikeyword) != a_ikeywords_exportprescan.end();
            if (is_in && a_secondary_index < 0)
            {
                is_mains_export_allocated = false;
                break;
            }
        }
    }
    }
    if (!is_mains_export_allocated)
        return;

    
    //std::unordered_map< ff_card_t *, vector<card_cells_temp_t> > umap_card_cells;

    bool subobjpopulate_flag = true;
    if (!myIsArraySubobjects)//is subobject of array
        myumap_card_cells.clear();
    else if (myIsArraySubobjects && myumap_card_cells.size())
        subobjpopulate_flag = false;

    vector< pair <ff_card_type_e, ff_card_t *> > card_lst;
    vector<card_cells_temp_t>  loc_cell_lst;
    vector<int> assign_ikey_lst;
    if (subobjpopulate_flag)
    {
    for (int j = 0; j < a_nb_sub_cards; ++j) {
        ff_card_t *a_sub_card_p = NULL;
        MCDS_get_ff_card_tab(a_card_p, CARD_CARD, j, (void *)(&a_sub_card_p));
        //
        ff_card_type_e a_sub_card_type = CARD_UNKNOWN;
        MCDS_get_ff_card_attributes(a_sub_card_p, CARD_TYPE, &a_sub_card_type, END_ARGS);

        if (a_sub_card_type != CARD_ASSIGN)
            continue;
        else
        {
            const ff_card_assign_header_t      *a_assign_card_format_p = (const ff_card_assign_header_t *)a_sub_card_p;
            assign_ikey_lst.push_back(a_assign_card_format_p->attribute_ikw);
        }
    }
    }
    for(int j=0;j<a_nb_sub_cards;++j) {
      ff_card_t *a_sub_card_p=NULL;
      MCDS_get_ff_card_tab(a_card_p,CARD_CARD,j,(void *)(&a_sub_card_p));
      //

      ff_card_type_e a_sub_card_type=CARD_UNKNOWN;
      MCDS_get_ff_card_attributes(a_sub_card_p,CARD_TYPE,&a_sub_card_type,END_ARGS);
      //

      if (myIsArraySubobjects)
      {
          std::unordered_map< ff_card_t*, vector<card_cells_temp_t> >::const_iterator found = myumap_card_cells.find(a_sub_card_p);
          if (found != myumap_card_cells.end())
          {
              card_lst.push_back(make_pair(a_sub_card_type, a_sub_card_p));
              continue;
          }
      }

        if (a_sub_card_type != CARD_IF)
            card_lst.push_back(make_pair(a_sub_card_type, a_sub_card_p));
        loc_cell_lst.clear();
      switch(a_sub_card_type) {
        case CARD_SINGLE:
        case CARD_HEADER:
        {
            int a_nb_cells = 0;
            int a_cell_length = 0;
            MCDS_get_ff_card_attributes(a_sub_card_p, CARD_NB_CELLS, &a_nb_cells, END_ARGS);
            loc_cell_lst.reserve(a_nb_cells);
            card_cells_temp_t card_cell_temp;
            for (int k = 0; k < a_nb_cells; ++k)
                loc_cell_lst.push_back(card_cell_temp);

            GetSingleCardCellInfo(a_sub_card_p, pre_object, descr_p, loc_cell_lst);

                myumap_card_cells.insert(make_pair(a_sub_card_p, loc_cell_lst));
            break;
        }
        case CARD_IF:
        {
            vector<const ff_card_t *>    subcardlst;
            bool a_if_cond_val = GetIfCardCellInfo((const PseudoFileFormatCard_t *)a_sub_card_p, pre_object, descr_p, model_p, &assign_ikey_lst, myumap_card_cells, &subcardlst, -1);
            if (a_if_cond_val)
            {
                if (!subcardlst.size())
                    card_lst.push_back(make_pair(a_sub_card_type, a_sub_card_p));
                else
                {
                    for (int k = 0; k < subcardlst.size(); ++k)
                    {
                        ff_card_type_e a_card_type = CARD_UNKNOWN;
                        MCDS_get_ff_card_attributes(subcardlst[k], CARD_TYPE, &a_card_type, END_ARGS);
                        card_lst.push_back(make_pair(a_card_type, (ff_card_t *)subcardlst[k]));
                    }
                }
            }
        }
        break;
        default:
            break;
        }
    }
    
    a_nb_sub_cards = (int)(card_lst.size());
    // Loops
    for (int i = 0; i < a_nb_values; ++i) {
        for (int j = 0; j < a_nb_sub_cards; ++j) {


            ff_card_t *a_sub_card_p = card_lst[j].second;
            //MCDS_get_ff_card_tab(a_card_p,CARD_CARD,j,(void *)(&a_sub_card_p));
            //
            ff_card_type_e a_sub_card_type = card_lst[j].first;
            //MCDS_get_ff_card_attributes(a_sub_card_p,CARD_TYPE,&a_sub_card_type,END_ARGS);
            //
            switch (a_sub_card_type) {
      case CARD_BLANK:
          {
              myWriteContext_p->WriteFile("\n");
          }
          break;
      case CARD_COMMENT:
          {
              if(true==myWriteDataLineFlag)
              {
                  bool a_do_continue = WriteCommentCard(a_sub_card_p, pre_object, descr_p, i);
              }
          }
          break;
            case CARD_ASSIGN:
          {
                IMECPreObject    *a_pre_object_p = const_cast<IMECPreObject *>(&pre_object);
                Assign(a_sub_card_p, *a_pre_object_p, descr_p, model_p, i);
          }
          break;
            case CARD_SINGLE:
      case CARD_HEADER:
          {

                WriteSingleCardList((const PseudoFileFormatCard_t *)a_sub_card_p, a_sub_card_type, pre_object, descr_p, i, 0, myumap_card_cells[a_sub_card_p]);
                //WriteSingleCard((const PseudoFileFormatCard_t *)a_sub_card_p,pre_object,descr_p,i);
          }
            break;
      case CARD_IF:
          {
              WriteIfCard((const PseudoFileFormatCard_t *)a_sub_card_p,pre_object,descr_p,
                    model_p, i, &myumap_card_cells);
          }
          break;
      case CARD_CELL_LIST:
          WriteCellList((const PseudoFileFormatCard_t*)a_sub_card_p, pre_object, descr_p, i);
          break;

      default:
          break;
      }
    }
  }
}


void MECDataWriter::WriteSubobjects(const PseudoFileFormatCard_t *card_p,
                                    const IMECPreObject           &pre_object,
                                    const PseudoDescriptor_t     *descr_p,
                                    const MECIModelScanner       *model_p,
                                          bool                    update_validCard)
{
  if (NULL==model_p) return;
    
  const ff_card_t      *a_card_p  = (const ff_card_t *)card_p;
  IDescriptor *a_descr_p = (IDescriptor *)descr_p;
    
  int    a_subobj_ikw    = END_ARGS;
  const char *a_kfulltype = NULL;
  MCDS_get_ff_card_attributes(a_card_p,CARD_OBJECTS_IKW,&a_subobj_ikw,CARD_KFULLTYPE,&a_kfulltype,END_ARGS);
  if (END_ARGS==a_subobj_ikw) return;
  string       a_subobj_skw   = a_descr_p->getSKeyword(a_subobj_ikw);
  value_type_e a_subobj_vtype = a_descr_p->getValueType(a_subobj_ikw);
  
  int config_type = a_descr_p->getConfigType();
  int hm_type = a_descr_p->getHmType();

  if (VTYPE_OBJECT != a_subobj_vtype) return;
  object_type_e a_subobj_otype = a_descr_p->getObjectType(a_subobj_ikw);
  const string &a_subobj_otype_str = MV_get_type(a_subobj_otype);
  IDescriptor *a_subobj_descr_p = HCDI_GetDescriptorHandle((char *)a_kfulltype);
  if (NULL == a_subobj_descr_p) return;
  const PseudoFileFormat_t *a_subobj_format_p =
      (const PseudoFileFormat_t *) a_subobj_descr_p->getFileFormatPtr((MvFileFormat_e) myFileFormatId);
  if (a_subobj_format_p == NULL) 
  {
      a_subobj_format_p =
          (const PseudoFileFormat_t *) a_subobj_descr_p->getLowerFileFormatPtr((MvFileFormat_e) myFileFormatId);
      if (a_subobj_format_p == NULL) 
          return;
  }
  attribute_type_e a_atype = a_descr_p->getAttributeType(a_subobj_ikw);
  int att_id = a_descr_p->getIdentifierValue(DOM_COMMON, a_subobj_ikw);
  a_subobj_descr_p->setConfigType(config_type);
  a_subobj_descr_p->setHmType(hm_type);

  // Write 1 or more objects, corresponding to the attribute type of a_objects_ikw
  switch (a_atype)
  {
  case ATYPE_VALUE: // Write 1 object
  {
      int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_OBJECT,a_subobj_skw);
      int a_index=pre_object.GetObjectIndex(a_attrib_ind);

      int a_id = 0;
      if (att_id >= 0 && a_attrib_ind >= 0)
          a_id = pre_object.GetObjectId(a_attrib_ind);

      if (update_validCard) 
      {
          UpdateSubobjectValidCards(a_subobj_format_p, a_subobj_otype_str.c_str(), a_index, a_subobj_descr_p, model_p, a_kfulltype, a_id);
      }
      else 
      {
          WriteSubobject(a_subobj_format_p, pre_object, a_subobj_otype_str.c_str(), a_index, a_subobj_descr_p,model_p,a_kfulltype, a_id);
      }
      break;
  }
  case ATYPE_STATIC_ARRAY: // Write a fixed number of objects
  {
      int a_nb_values = a_descr_p->getSize(a_subobj_ikw);
      int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_OBJECT,a_subobj_skw);
      for (int i = 0; i < a_nb_values; i++) {
          int a_index=pre_object.GetObjectIndex(a_attrib_ind, i);

          int a_id = 0;
          if (att_id >= 0 && a_attrib_ind >= 0)
              a_id = pre_object.GetObjectId(a_attrib_ind, i);

          if (update_validCard) 
          {
              UpdateSubobjectValidCards(a_subobj_format_p, a_subobj_otype_str.c_str(), a_index, a_subobj_descr_p, model_p, a_kfulltype, a_id);
          }
          else 
          {
             WriteSubobject(a_subobj_format_p, pre_object, a_subobj_otype_str.c_str(), a_index, a_subobj_descr_p,model_p,a_kfulltype, a_id);
      }
      }
      break;
  }
  case ATYPE_DYNAMIC_ARRAY: // Write a variable number of objects
  {
      string a_size_skw = a_descr_p->getSizeSKeyword(a_subobj_ikw);
      int a_nb_values = pre_object.GetIntValue(a_size_skw.c_str());
      int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_OBJECT,a_subobj_skw);
      IMECPreObject* sub_pre_object = HCDI_GetPreObjectHandle(a_kfulltype, a_kfulltype, "", 0, 0);

      myumap_card_cells.clear();

      object_type_e etype = (object_type_e)HCDI_GetHCObjectType(a_kfulltype);
      if (etype == HCDI_OBJ_TYPE_SUBOBJECT)
           myIsArraySubobjects = true;

      for (int i = 0; i < a_nb_values; i++) {
          int a_index=pre_object.GetObjectIndex(a_attrib_ind, i);
          sub_pre_object->ClearAllAttribValues(true);
          int a_id = 0;
          if (att_id >= 0 && a_attrib_ind >= 0)
              a_id = pre_object.GetObjectId(a_attrib_ind, i);

          if (update_validCard) 
          {
              UpdateSubobjectValidCards(a_subobj_format_p, a_subobj_otype_str.c_str(), a_index, a_subobj_descr_p, model_p, a_kfulltype, a_id, i);
          }
          else 
          {
              WriteSubobject(a_subobj_format_p, pre_object, a_subobj_otype_str.c_str(), a_index, a_subobj_descr_p,model_p,a_kfulltype, a_id, sub_pre_object, i);
          }
      }
      myIsArraySubobjects = false;
      myumap_card_cells.clear();
      HCDI_ReleasePreObjectHandle(sub_pre_object);
      break;
  }
  default: 
      break;
  }
}


void MECDataWriter::UpdateSubobjectValidCards(const PseudoFileFormat_t* subobj_format_p,
                                              const char*               otype,
                                              int                       index,
                                              const PseudoDescriptor_t* subobj_descr_p,
                                              const MECIModelScanner*   model_p,
                                              const string              fulltype,
                                              int                       id,
                                              unsigned int              subobj_indx)
{
    if (0 > index) return;
    //
    IMECPreObject* a_pre_object = HCDI_GetPreObjectHandle(fulltype.c_str(), "", "", 0, 0);
    MvModelScanner_t* mv_model = (MvModelScanner_t*)model_p;

    const IDescriptor* a_descrp = (const IDescriptor*)subobj_descr_p;
    hwHCSolverInf* mysolverinf = mv_model->getHmSolverInf();
    if (a_descrp && mysolverinf)
    {
        a_pre_object->SetEntityType(HCDI_OBJ_TYPE_SUBOBJECT);
        MvIKeywordSet_t       a_main_ikws;
        MvIKeywordSet_t* p_main_ikws = NULL;
        a_descrp->getMainArrayIKeywords(&a_main_ikws);
        if (a_main_ikws.size())
            p_main_ikws = &a_main_ikws;
        mysolverinf->GetEntityPreobject((const IDescriptor*)subobj_descr_p, a_pre_object, NULL, NULL,
            NULL, false, true, true, true, subobj_indx);

        const fileformat_t *a_format_p = (const fileformat_t *)subobj_format_p;
        //
        int a_nb_cards =0;
        MCDS_get_fileformat_nb_cards(a_format_p, &a_nb_cards);
        for (int i = 0; i < a_nb_cards; ++i)
        {
            ff_card_t *v_card_p = NULL;
            MCDS_get_fileformat_card(a_format_p, i, &v_card_p);
            UpdateValidCards((const PseudoFileFormatCard_t *)v_card_p, *a_pre_object, a_descrp, model_p);
        }
	}
}

void MECDataWriter::PreTreatPreObject(obj_type_e etype, const IDescriptor* pdescrip, IMECPreObject& pre_object, const MECIModelScanner* model_p, IMECPreObject* parent_pre_object)
{

}
void MECDataWriter::WriteSubobject(const PseudoFileFormat_t *subobj_format_p,
                                   const IMECPreObject& pre_object,
                                   const char               *otype,
                                   int                       index,
                                   const PseudoDescriptor_t *subobj_descr_p,
                                   const MECIModelScanner   *model_p,
                                   const string              fulltype,
                                   MYOBJ_INT id,
                                   IMECPreObject            *sub_pre_object,
                                   unsigned int              subobj_indx)
{
    //The Subobject card is written using this function
    if (!model_p || !subobj_format_p) return;

    IMECPreObject* a_pre_object = sub_pre_object;
    if(!a_pre_object)
        a_pre_object = HCDI_GetPreObjectHandle(fulltype.c_str(), fulltype.c_str(), "", 0, 0);
    //Always component index and file index will be zero for SUBOBJECT
    //model_p->GetObjectData (otype,0, index, a_pre_object);
    //model_p->GetObjectDataExt("SUBOBJECT",index,&a_pre_object);

    MvModelScanner_t *mv_model = (MvModelScanner_t *)model_p;
    hwHCSolverInf    *mysolverinf = mv_model->getHmSolverInf();
    void * main_entity_p = mysolverinf->GetCurrentEntityPtr();
    unsigned int main_ent_type = mysolverinf->GetCurrentEntityType();

    object_type_e etype = (object_type_e)HCDI_GetHCObjectType( fulltype );   // MvFullType_t(nullptr, fulltype).getType();
    int entity_type = 0;
    if (etype != HCDI_OBJ_TYPE_SUBOBJECT)
    {
        entity_type = etype;
        a_pre_object->SetEntityType((object_type_e)etype);
        subobj_indx = UINT_MAX; 
    }
    else
    {
        unsigned int hm_entType = pre_object.GetEntityType();
        a_pre_object->SetEntityType((object_type_e)hm_entType);
    }
    bool entityfound = false;
    if (entity_type > 0)
        entityfound = true;
    if (mysolverinf)
    {
        bool a_ret_type = true;
        if (entityfound && id > 0)
            a_ret_type = mysolverinf->SetEntitybyId(entity_type, id);
        //mysolverinf->GetEntityPreobject((const IDescriptor *)subobj_descr_p, a_pre_object);
        if (a_ret_type)
        {
            GetEntityPreobject(mysolverinf, (const IDescriptor*)subobj_descr_p, a_pre_object, subobj_indx);
            IMECPreObject* p_pre_object = const_cast<IMECPreObject* >(&pre_object);
            PreTreatPreObject(etype, (const IDescriptor*)subobj_descr_p, *a_pre_object, model_p, p_pre_object);
            WriteObjectData(subobj_format_p, *a_pre_object, subobj_descr_p, model_p);
        }
        mysolverinf->SetCurrentEntity(main_ent_type, main_entity_p);
    }
    if(!sub_pre_object)
    HCDI_ReleasePreObjectHandle(a_pre_object);
}

int MECDataWriter::GetEntityPreobject(hwHCSolverInf* solverinf_p, const IDescriptor* descr_p, IMECPreObject* preobject_p, const unsigned int subobj_indx)
{
    if (!solverinf_p || !descr_p || !preobject_p)
        return -1;

	bool can_have_name = solverinf_p->CanSolverEntitiesHaveName();
	bool has_parameter = true;
	unsigned int etype = preobject_p->GetEntityType();
	if (etype > 0)
		has_parameter = solverinf_p->IsParameterSupported(etype);

	if (subobj_indx == 0) // only once for array of subobjects
	{
		mySubobjMapDefaultTypeKeywordLst.clear();
		mySubobjVecIkeyIdenVal.clear();
        mySubobjMainIkws.clear();
		descr_p->getAllIKeywords(DOM_COMMON, mySubobjMapDefaultTypeKeywordLst);
		descr_p->getIdentifiers(&mySubobjVecIkeyIdenVal);
        descr_p->getMainArrayIKeywords(&mySubobjMainIkws);
		solverinf_p->UpdateDataEnum(descr_p, etype);
	}
    if (subobj_indx != UINT_MAX && subobj_indx >= 0)
    {
        solverinf_p->GetEntityPreobject(descr_p, preobject_p, &mySubobjMapDefaultTypeKeywordLst, &mySubobjVecIkeyIdenVal, &mySubobjMainIkws, can_have_name, has_parameter, true, true, subobj_indx);
    }
    else
    {
    MvIKeywordSet_t       a_main_ikws;
    descr_p->getMainArrayIKeywords(&a_main_ikws);
        solverinf_p->GetEntityPreobject(descr_p, preobject_p, nullptr, nullptr, &a_main_ikws, can_have_name, has_parameter, true, true, subobj_indx);
    }
    return 0;
}
void MECDataWriter::UpdateIfValidCards(const PseudoFileFormatCard_t  *card_p,
	                                   const IMECPreObject           &pre_object,
	                                   const PseudoDescriptor_t      *descr_p,
	                                   const MECIModelScanner        *model_p,
	                                   int                            ind)
{
	const ff_card_t *a_card_p = (const ff_card_t *)card_p;
	//
	int  a_nb_ccls = 0;
	bool a_checked = false;
	MCDS_get_ff_card_attributes(a_card_p, CARD_NB_COND_CARD_LISTS, &a_nb_ccls, END_ARGS);
	for (int i = 0; !a_checked && i < a_nb_ccls; ++i) {
		ff_condcardlist_t *a_ccl_p = NULL;
		expression_t      *a_expr_p = NULL;
		MCDS_get_ff_card_tab(a_card_p, CARD_COND_CARD_LIST, i, &a_ccl_p);
		MCDS_get_ff_condcardlist_expression(a_ccl_p, &a_expr_p);
		a_checked = (a_expr_p == NULL);
		//
		if (!a_checked) {
			MvExpression_t a_expr(a_expr_p, false);
			a_checked = pre_object.EvaluateExpression((PseudoExpression_t *)(&a_expr), descr_p, ind);
		}
		//
		if (a_checked) {
			int a_nb_sub_cards = 0;
			MCDS_get_ff_condcardlist_nb_cards(a_ccl_p, &a_nb_sub_cards);
			for (int j = 0; j < a_nb_sub_cards; ++j) {
				ff_card_t *a_sub_card_p = NULL;
				MCDS_get_ff_condcardlist_card(a_ccl_p, j, &a_sub_card_p);
				UpdateValidCards((const PseudoFileFormatCard_t *)a_sub_card_p, pre_object, descr_p, model_p, ind);
			}
		}
	}
}
//
void MECDataWriter::WriteIfCard(const PseudoFileFormatCard_t *card_p,
                                const IMECPreObject           &pre_object,
                                const PseudoDescriptor_t     *descr_p,
                                const MECIModelScanner       *model_p, 
                                int                           ind,
                                 std::unordered_map< ff_card_t *, vector<card_cells_temp_t> > *umap_card_cells)
{
  const ff_card_t *a_card_p=(const ff_card_t *)card_p;
  //
  int  a_nb_ccls = 0;
  bool a_checked = false;
  MCDS_get_ff_card_attributes(a_card_p,CARD_NB_COND_CARD_LISTS,&a_nb_ccls,END_ARGS);
  for(int i=0;!a_checked && i<a_nb_ccls;++i) {
    ff_condcardlist_t *a_ccl_p  = NULL;
    expression_t      *a_expr_p = NULL;
    MCDS_get_ff_card_tab(a_card_p,CARD_COND_CARD_LIST,i,&a_ccl_p);
    MCDS_get_ff_condcardlist_expression(a_ccl_p,&a_expr_p);
    a_checked=(a_expr_p==NULL);
    //
    if(!a_checked) {
      MvExpression_t a_expr(a_expr_p,false);
      a_checked=pre_object.EvaluateExpression((PseudoExpression_t *)(&a_expr),descr_p,ind);
    }
    //
    if(a_checked) {
      int a_nb_sub_cards=0;
      MCDS_get_ff_condcardlist_nb_cards(a_ccl_p,&a_nb_sub_cards);
      for(int j=0;j<a_nb_sub_cards;++j) {
	ff_card_t *a_sub_card_p=NULL;
                ff_card_t *a_next_sub_card_p = NULL;
	MCDS_get_ff_condcardlist_card(a_ccl_p,j,&a_sub_card_p);
                int flag = 0;
                MCDS_get_ff_card_attributes(a_sub_card_p, CARD_FLAGS, &flag, END_ARGS);
                if (flag)
                {
                    bool no_end_flag = (flag & CARD_FLAG_NO_END) ? true : false;
                    if (no_end_flag && j < (a_nb_sub_cards - 1))
                    {
                        j++;
                        MCDS_get_ff_condcardlist_card(a_ccl_p, j, &a_next_sub_card_p);
                    }
                }
	WriteCard((const PseudoFileFormatCard_t *)a_sub_card_p,pre_object,descr_p,
                    model_p, ind, a_next_sub_card_p, umap_card_cells);
        }
    }
  }
}


bool MECDataWriter::GetIfCardCellInfo(const PseudoFileFormatCard_t *card_p,
                                      const IMECPreObject          &pre_object,
                                      const PseudoDescriptor_t     *descr_p,
    const MECIModelScanner       *model_p, vector<int>  *AsgnIkeylst,
                                      std::unordered_map< ff_card_t *, vector<card_cells_temp_t> > &umap_card_cells,
    vector<const ff_card_t *>    *subcardlst, int   ind)
{
    const ff_card_t *a_card_p=(const ff_card_t *)card_p;
    //
    int  a_nb_ccls = 0;
    bool a_checked = true; // always true to get all card list
    bool has_single_value_type = false;
    MCDS_get_ff_card_attributes(a_card_p,CARD_NB_COND_CARD_LISTS,&a_nb_ccls,END_ARGS);

//check if all are of single attribute and only if we have any number of simple 'if' conditions(not if else (or) if else if)
    if(a_nb_ccls == 1 && subcardlst)
    {
        const IDescriptor    *a_descr_p = (const MvDescriptor_t *)descr_p;
        ff_condcardlist_t *a_ccl_p  = NULL;
        expression_t      *a_expr_p = NULL;
        MCDS_get_ff_card_tab(a_card_p,CARD_COND_CARD_LIST,0,&a_ccl_p);
        MCDS_get_ff_condcardlist_expression(a_ccl_p,&a_expr_p);
        a_checked=(a_expr_p==NULL);
        //
        if(!a_checked) {
            const expression_t *a_mcds_expr_p = a_expr_p;
            expression_type_e   a_expr_type   = EXPRT_UNKNOWN;
            MCDS_get_expression_attributes(a_mcds_expr_p,EXPR_TYPE,&a_expr_type,END_ARGS);
            //
            bool a_result=false;
            switch(a_expr_type) {
            case EXPRT_ATTRIBUTE:
                {
                    int          a_ikeyword   = END_ARGS;
                    int a_rikeyword = 0;
                    MCDS_get_expression_attributes(a_mcds_expr_p, EXPR_IKEYWORD,  &a_ikeyword,
                                                                  EXPR_RHS_IKEYWORD,  &a_rikeyword, END_ARGS);  
                    attribute_type_e a_atype = a_descr_p->getAttributeType(a_ikeyword);
                if (AsgnIkeylst)
                {
                    vector<int>::iterator  a_ikey_it;
                    for (a_ikey_it = AsgnIkeylst->begin(); a_ikey_it != AsgnIkeylst->end(); ++a_ikey_it)
                    {
                        int    a_asgn_ikey = (*a_ikey_it);
                        if (a_asgn_ikey == a_ikeyword || a_asgn_ikey == a_rikeyword)
                            return true; // Returning true so that the if card will be added.
                    }
                }
                    if(a_atype == ATYPE_VALUE)
                        has_single_value_type = true;
                    if(has_single_value_type && a_rikeyword > 0)
                    {
                        attribute_type_e a_atype_r = a_descr_p->getAttributeType(a_rikeyword);
                        if(a_atype_r != ATYPE_VALUE)
                            has_single_value_type = false;
                    }
                }
                break;
            default:
                has_single_value_type = false;
                break;
            }
        }
        if(!a_checked && has_single_value_type) {
            MvExpression_t a_expr(a_expr_p,false);
            a_checked=pre_object.EvaluateExpression((PseudoExpression_t *)(&a_expr),descr_p,0);
            if(!a_checked)
                 return false;
        }
        //
        if(a_checked && has_single_value_type) {
            int a_nb_sub_cards=0;
            MCDS_get_ff_condcardlist_nb_cards(a_ccl_p,&a_nb_sub_cards);
            for(int j=0;j<a_nb_sub_cards;++j) {
                ff_card_t *a_sub_card_p=NULL;
                MCDS_get_ff_condcardlist_card(a_ccl_p,j,&a_sub_card_p);
                ff_card_type_e a_sub_card_type=CARD_UNKNOWN;
                MCDS_get_ff_card_attributes(a_sub_card_p,CARD_TYPE,&a_sub_card_type,END_ARGS);
                if(a_sub_card_type == CARD_IF)
                {
                    has_single_value_type = false;
                    break;
                }
            }
        }
    }

    a_checked = true;
    for(int i=0; i<a_nb_ccls;++i) {
        ff_condcardlist_t *a_ccl_p  = NULL;
        expression_t      *a_expr_p = NULL;
        MCDS_get_ff_card_tab(a_card_p,CARD_COND_CARD_LIST,i,&a_ccl_p);
        //
        if(a_checked) {
            int a_nb_sub_cards=0;
            MCDS_get_ff_condcardlist_nb_cards(a_ccl_p,&a_nb_sub_cards);
            for(int j=0;j<a_nb_sub_cards;++j) {
                ff_card_t *a_sub_card_p=NULL;
                MCDS_get_ff_condcardlist_card(a_ccl_p,j,&a_sub_card_p);
                vector<card_cells_temp_t>  loc_cell_lst;
                ff_card_type_e a_sub_card_type=CARD_UNKNOWN;
                MCDS_get_ff_card_attributes(a_sub_card_p,CARD_TYPE,&a_sub_card_type,END_ARGS);
                //
                loc_cell_lst.clear();

                if(has_single_value_type && subcardlst)
                    subcardlst->push_back(a_sub_card_p);
                if(a_sub_card_type == CARD_SINGLE || a_sub_card_type == CARD_HEADER)
                {
                    int a_nb_cells = 0;
                    int a_cell_length = 0;
                    MCDS_get_ff_card_attributes(a_sub_card_p,CARD_NB_CELLS,&a_nb_cells,END_ARGS);
                    loc_cell_lst.reserve(a_nb_cells);
                    card_cells_temp_t card_cell_temp;
                    for(int k=0;k<a_nb_cells;++k)
                        loc_cell_lst.push_back(card_cell_temp);
                   GetSingleCardCellInfo(a_sub_card_p, pre_object,descr_p,loc_cell_lst);
                   umap_card_cells.insert(make_pair(a_sub_card_p, loc_cell_lst));
                }
                else if(a_sub_card_type == CARD_IF)
                {
                    bool a_if_cond_val = GetIfCardCellInfo(a_sub_card_p, pre_object, descr_p, model_p, NULL, umap_card_cells, NULL, ind);
                }
      }
    }
  }
    return true;
}




void MECDataWriter::GetIfCellDisplayStatus(const PseudoFileFormatCell_t        *cell_p,
                                           const IMECPreObject                 &pre_object,
                                           const PseudoDescriptor_t            *descr_p,
                                           int                                  ind)
{
    const ff_cell_t *a_cell_p = (const ff_cell_t *)cell_p;
    //
    int     a_nb_ccls = 0;
    bool    a_checked = false;

    MCDS_get_ff_cell_attributes(a_cell_p, CELL_NB_COND_CELL, &a_nb_ccls, END_ARGS);

    for (int i = 0; !a_checked && i<a_nb_ccls; ++i) {
        ff_condcell_t       *a_ccl_p = NULL;
        expression_t        *a_expr_p = NULL;

        MCDS_get_ff_cell_tab(a_cell_p, CELL_COND_CELL, i, &a_ccl_p);
        MCDS_get_ff_condcell_expression(a_ccl_p, &a_expr_p);
        a_checked = (a_expr_p == NULL);
        //
        if (!a_checked) {
            MvExpression_t a_expr(a_expr_p, false);
            a_checked = pre_object.EvaluateExpression((PseudoExpression_t *)(&a_expr), descr_p, ind);
        }
        //
        if (a_checked) {
            ff_cell_t    *a_sub_cell_p = NULL;
            MCDS_get_ff_condcell_cell(a_ccl_p, &a_sub_cell_p);

            GetCellDisplayStatus((const PseudoFileFormatCell_t *)a_sub_cell_p, pre_object, descr_p, ind);
        }
    }
}

/*Writing of conditional cells*/
void MECDataWriter::WriteIfCell(const PseudoFileFormatCell_t        *cell_p,
                                const IMECPreObject                  &pre_object,
                                const PseudoDescriptor_t            *descr_p,
                                int                                  ind)
{
    const ff_cell_t *a_cell_p=(const ff_cell_t *)cell_p;
    //
    int     a_nb_ccls = 0;
    bool    a_checked = false;

    MCDS_get_ff_cell_attributes(a_cell_p,CELL_NB_COND_CELL,&a_nb_ccls,END_ARGS);
    myIsLastCell = false; // Always set this flag to false before entering the nb_cells loop.
    for(int i=0;!a_checked && i<a_nb_ccls;++i) {
        ff_condcell_t       *a_ccl_p  = NULL;
        expression_t        *a_expr_p = NULL;

        MCDS_get_ff_cell_tab(a_cell_p, CELL_COND_CELL, i, &a_ccl_p);
        MCDS_get_ff_condcell_expression(a_ccl_p, &a_expr_p);
        a_checked=(a_expr_p==NULL);
        //
        if(!a_checked) {
            MvExpression_t a_expr(a_expr_p,false);
            a_checked = pre_object.EvaluateExpression((PseudoExpression_t *)(&a_expr),descr_p, ind);
        }
        //
        if(a_checked) {
	        ff_cell_t    *a_sub_cell_p=NULL;
            MCDS_get_ff_condcell_cell(a_ccl_p, &a_sub_cell_p);
			if (myWriteFreeFormat == true && i == a_nb_ccls - 1)
                myIsLastCell = true;
            WriteCell((const PseudoFileFormatCell_t *)a_sub_cell_p, pre_object,descr_p, ind);
        }
    }
    myIsLastCell = false; // Always set this flag as false after exiting from the nb_cells loop.
}

void MECDataWriter::WritePairCell(const PseudoFileFormatCell_t        *cell_p,
                                const IMECPreObject                  &pre_object,
                                const PseudoDescriptor_t            *descr_p,
                                int                                  ind)
{
    const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p;
    const ff_cell_t *a_cell_p=(const ff_cell_t *)cell_p;

    const char    *a_cell_fmt         = NULL;
    int            a_cell_ikw         = END_ARGS;
    int fmt_size = 0;
    MCDS_get_ff_cell_attributes(a_cell_p,
        CELL_FORMAT,     &a_cell_fmt,
        CELL_IKEYWORD,   &a_cell_ikw,
        END_ARGS);
    a_cell_fmt = GetFormatSize(a_cell_fmt, fmt_size);
    myCurActiveLineLength += 2*fmt_size;
    //
    string     a_cell_skw  = a_descr_p->getSKeyword(a_cell_ikw);
    value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);
    if(ind >= 0)
    {
        attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
        if(a_atype == ATYPE_VALUE)
            ind = -1;
    }
    switch(a_cell_vtype)
    {
    case VTYPE_FLOAT:
    {
        //int a_value=0;
        double point = 0.0;
        bool is_parameter_negated = false;
        bool is_parameter = false;
        string param_name = "";
        if(ind<0) {
        }
        else
        {
            ind = ind*2;
            for(int i = 0; i<2 ;i++)
            {
                is_parameter = pre_object.IsParameter(a_cell_skw.c_str(),ind+i);
				
                if(is_parameter)
                {   
                    char temp_cell_fmt[10];
                    sprintf (temp_cell_fmt, "%%%ds", fmt_size);
                    param_name = pre_object.GetParameterName(a_cell_skw.c_str(),ind+i, &is_parameter_negated);
                    WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
                }
                else
                {
                    int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_FLOAT,a_cell_skw);
                    point=(pre_object.GetFloatValue(a_attrib_ind,ind+i));
                    WriteDouble(a_cell_fmt, fmt_size, point);
                }
				if (myWriteFreeFormat == true && (i == 0 || myIsLastCell == false))
                {
                    string str(mydelimiter);
                    myWriteContext_p->WriteFile(str.c_str());
                }
            }
        }
    }
        break;
    case VTYPE_OBJECT:
    {
        unsigned int object = 0;
        if (ind < 0) {
        }
        else
        {
            ind = ind * 2;
            for (int i = 0; i < 2; i++)
            {
                int a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_cell_skw);
                object = (pre_object.GetObjectId(a_attrib_ind, ind + i));


                if (myWriteFreeFormat == false)
                    WriteUInteger(a_cell_fmt, object);
                else
                {
                    string str(mydelimiter);
                    myWriteContext_p->WriteFile(str.c_str());
                }
            }
        }
    }
    break;
    }
}
bool MECDataWriter::GetCellDisplayStatus(const PseudoFileFormatCell_t *cell_p,
	                                     const IMECPreObject          &pre_object,
	                                     const PseudoDescriptor_t     *descr_p,
	                                     int                           ind)
{
    const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p;
    ff_cell_t            *a_cell_p = (ff_cell_t *)cell_p;
    //
    ff_cell_type_e a_cell_type = CELL_UNKNOWN;
    MCDS_get_ff_cell_attributes(a_cell_p, CELL_TYPE, &a_cell_type, END_ARGS);
    bool haveVal = false;
    switch (a_cell_type) {

    case CELL_COMMENT:
    {
        char *a_comment = NULL;
        MCDS_get_ff_cell_attributes(a_cell_p, CELL_STRING, &a_comment, END_ARGS);

        std::string tempComment = (a_comment != NULL) ? a_comment : "", s_comment = "";
        int count = 0;
        size_t len = tempComment.length();

        for (int i = 0; len > i; ++i)
        {
            if (tempComment[i] != ' ')
                s_comment += tempComment[i];
        }

        if (a_comment != NULL)
        {
            if (s_comment == "*" || s_comment == "+" || s_comment == "") return false;
            else return true;
        }
        else
            return false;
    }
    break;
    case CELL_VALUE:
    {
        return haveVal = GetCellDisplayStatus_VALUE(cell_p, pre_object, descr_p, ind);
    }
    break;
    case CELL_ID: /*Writes the id of an object because in FORMAT block it is defined as _ID*/
    {
        int value = 0;
        value = pre_object.GetId();
        if (value != 0)
            return true;
        else
            return false;
    }
    break;
    case CELL_COND:
    {
        GetIfCellDisplayStatus(cell_p, pre_object, descr_p, ind);
    }
    break;
    case CELL_SCALAR_OR_OBJECT:
    {
        return haveVal = Getcell_SCALAR_OR_OBJECT(cell_p, pre_object, descr_p, ind);
    }
    break;
    default:
        return true;
        break;
    }
    return false;
}

void MECDataWriter::WriteCellArrayList(const PseudoFileFormatCell_t* cell_p,
    const IMECPreObject& pre_object,
    const PseudoDescriptor_t* descr_p,
    int    ind)
{
    const MvDescriptor_t* a_descr_p = (const MvDescriptor_t*)descr_p;
    const ff_cell_t* a_cell_p = (const ff_cell_t*)cell_p;

    const char* a_cell_fmt = NULL;
    int         a_cell_ikw = END_ARGS;
    int         a_index = -1;
    MCDS_get_ff_cell_attributes(a_cell_p,
        CELL_FORMAT, &a_cell_fmt,
        CELL_IKEYWORD, &a_cell_ikw,
        CELL_LIST_INDEX, &a_index,
        END_ARGS);
    //
    //string     a_cell_skw = a_descr_p->getSKeyword(a_cell_ikw);
    value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);
    attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
    if (a_atype == ATYPE_VALUE || a_atype == ATYPE_SIZE)
        return;
    bool a_multi = a_descr_p->isMultiDimensionalArray(a_cell_ikw);
    if (ind >= 0 && a_multi)
    {
        int a_nb_values = 0;
        MvSizeVector sizeArrayVector;
        a_descr_p->getDimensionSize(a_cell_ikw, sizeArrayVector);
        size_t array_dimension = sizeArrayVector.size();
        if (array_dimension > 2)
            return;
        int size1 = 0, size2 = 0;
        bool is_real_size1 = sizeArrayVector[0].isRealSize;
        if (is_real_size1)
            size1 = sizeArrayVector[0].size;
        else
        {
            int size1_ikey = sizeArrayVector[0].size;
            string size1_skey = a_descr_p->getSKeyword(size1_ikey);
            size1 = pre_object.GetIntValue(size1_skey.c_str());
        }

        if (ind >= size1)
            return;

        bool is_real_size2 = sizeArrayVector[1].isRealSize;
        if (is_real_size2)
            size2 = sizeArrayVector[1].size;
        else
        {
            int size2_ikey = sizeArrayVector[1].size;
            string size2_skey = a_descr_p->getSKeyword(size2_ikey);
            size2 = pre_object.GetIntValue(size2_skey.c_str());
        }

        int j = ind * size2;
        int a_size = 0;
        if (a_index < 0)
        {
            a_size = j + size2;
        }
        else if (a_index <= a_nb_values)
        {
            j = j + a_index;
            a_size = j + 1;
        }

        for (int i = j; i < a_size; i++)
        {
			if (myCurActiveLineLength >= myLineLength)
			{
				myWriteContext_p->WriteFile(myNewlineString.c_str());
			}
            WriteCell_VALUE(cell_p, pre_object, descr_p, i, a_cell_ikw);
        }
    }
	else if (ind >= 0)
	{
		int a_nb_values = 0;
        static vector<int> a_vec_size;

        if (ind == 0 && a_vec_size.size())
            a_vec_size.clear();

        int a_size_ikw = a_descr_p->getSizeIKeyword(a_cell_ikw);
	    string size_skey = a_descr_p->getSKeyword(a_size_ikw);
		int a_size = pre_object.GetIntValue(size_skey.c_str());
		
        a_vec_size.push_back(a_size);//

        for (int k = 0; k < a_vec_size.size(); k++)
            a_nb_values += a_vec_size[k];

		int j = a_nb_values - a_vec_size[ind];
		a_size = 0;
		if (a_index < 0)
		{
			a_size = a_nb_values;
		}
		else if (a_index < a_nb_values)
		{
            j = a_nb_values - a_vec_size[ind] + a_index;
			a_size = j + 1;
		}

        for (int i = j; i < a_size; i++)
        {
			if (myCurActiveLineLength >= myLineLength)
			{
				myWriteContext_p->WriteFile(myNewlineString.c_str());
			}
            WriteCell_VALUE(cell_p, pre_object, descr_p, i, a_cell_ikw);
        }
	}
    else if(!a_multi)
    {
        int a_nb_values = 0;
        if (a_atype == ATYPE_STATIC_ARRAY)
            a_nb_values = a_descr_p->getSize(a_cell_ikw);
        else
        {
            int a_size_ikw = a_descr_p->getSizeIKeyword(a_cell_ikw);
            string a_size_skw = a_descr_p->getSKeyword(a_size_ikw);
            a_nb_values = pre_object.GetIntValue(a_size_skw.c_str());
        }
        int j = 0, a_size = 0;
        if (a_index < 0)
        {
            j = 0;
            a_size = a_nb_values;
        }
        else if (a_index < a_nb_values)
        {
            j = a_index;
            a_size = a_index + 1;
        }
        for (int i = j; i < a_size; i++)
        {
            if (myCurActiveLineLength >= myLineLength)
            {
                myWriteContext_p->WriteFile(myNewlineString.c_str());
            }
            WriteCell_VALUE(cell_p, pre_object, descr_p, i, a_cell_ikw);
        }
    }
}

void MECDataWriter::WriteCell(const PseudoFileFormatCell_t *cell_p,
                              const IMECPreObject       &pre_object,
                              const PseudoDescriptor_t *descr_p,
                              int                       ind)
{
  const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p;
  ff_cell_t            *a_cell_p  = (ff_cell_t *)cell_p;
  //
  ff_cell_type_e a_cell_type = CELL_UNKNOWN;
  MCDS_get_ff_cell_attributes(a_cell_p,CELL_TYPE,&a_cell_type,END_ARGS);
  switch(a_cell_type) {
    
  case CELL_COMMENT:
    {
      char *a_comment=NULL;
      MCDS_get_ff_cell_attributes(a_cell_p,CELL_STRING,&a_comment,END_ARGS);
      if (a_comment)
      {
          myWriteContext_p->WriteFile(a_comment);
          myCurActiveLineLength += (int)strlen(a_comment);
      }

    }
    break;
    
  case CELL_VALUE:
    {
      WriteCell_VALUE (cell_p, pre_object, descr_p, ind);
    }
    break;
  case CELL_ID: /*Writes the id of an object because in FORMAT block it is defined as _ID*/
    {
      const char           *a_cell_fmt = NULL;
      MCDS_get_ff_cell_attributes(a_cell_p,CELL_FORMAT,&a_cell_fmt,END_ARGS);
      int fmt_size = 0;
      a_cell_fmt = GetFormatSize(a_cell_fmt, fmt_size);
      myCurActiveLineLength += fmt_size;

      //check if id is a parameter      
      bool is_parameter_id  = pre_object.IsParameterId();  
      if(is_parameter_id)
      {
          char temp_cell_fmt[10];
          string param_name = pre_object.GetParameterIdName();
          sprintf (temp_cell_fmt, "%%%ds", fmt_size);          
          WriteParameterCell(temp_cell_fmt,param_name.c_str());
      }
      else
      {
	    if(myWriteFreeFormat == false)
          WriteInteger(a_cell_fmt, pre_object.GetId()); 
		else
        {
              myWriteContext_p->WriteFile("%d", pre_object.GetId());
            string str(mydelimiter);
            myWriteContext_p->WriteFile(str.c_str());
      }
      }

    }
    break;
  case CELL_DIR_RADIO:
    {
      const char *a_cell_fmt         = NULL;
      int         a_cell_ikw         = END_ARGS;
      int         a_cell_is_extended = 0;
      MCDS_get_ff_cell_attributes(a_cell_p,
				  CELL_FORMAT,     &a_cell_fmt,
				  CELL_IKEYWORD,   &a_cell_ikw,
				  CELL_IS_EXTENDED,&a_cell_is_extended,
				  END_ARGS);

	  int fmt_size = 0;
	  a_cell_fmt = GetFormatSize(a_cell_fmt, fmt_size);
	  myCurActiveLineLength += fmt_size;

      //
      string     a_cell_skw  = a_descr_p->getSKeyword(a_cell_ikw);
      value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);

      dir_type_e a_dir_value = DIR_UNKNOWN;
      if(ind<0) {
          if(a_cell_vtype==VTYPE_UINT)
              a_dir_value=(dir_type_e)(pre_object.GetUIntValue(a_cell_skw.c_str()));
          else
              a_dir_value=(dir_type_e)(pre_object.GetIntValue(a_cell_skw.c_str()));
        }
        else {
          if(a_cell_vtype==VTYPE_UINT) 
          {
              int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_UINT,a_cell_skw);
              a_dir_value=(dir_type_e)(pre_object.GetUIntValue(a_attrib_ind,ind));
          }
          else
          {
              int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,a_cell_skw);
              a_dir_value=(dir_type_e)(pre_object.GetIntValue(a_attrib_ind,ind));
          }
      }
      //
      const char *a_dir_str=HCDI_MV_get_direction_str(a_dir_value,a_cell_is_extended);


	  if(myWriteFreeFormat == false)
		  myWriteContext_p->WriteFile(a_cell_fmt,a_dir_str);
	  else
      { 
		  myWriteContext_p->WriteFile("%d",a_dir_str);
          string str(mydelimiter);
          myWriteContext_p->WriteFile(str.c_str());
      }
    }
    break;
  case CELL_DIR_FLAGS:
      {
          const char *a_cell_fmt = NULL;
          int         a_xdir_ikw = END_ARGS,a_ydir_ikw = END_ARGS,a_zdir_ikw = END_ARGS;
          MCDS_get_ff_cell_attributes(a_cell_p,
              CELL_FORMAT,  &a_cell_fmt,
              CELL_DIRX_IKW,&a_xdir_ikw,
              CELL_DIRY_IKW,&a_ydir_ikw,
              CELL_DIRZ_IKW,&a_zdir_ikw,
              END_ARGS);
          //
		  int fmt_size = 0;
		  a_cell_fmt = GetFormatSize(a_cell_fmt, fmt_size);
          myCurActiveLineLength += fmt_size;

          string a_xdir_skw = a_descr_p->getSKeyword(a_xdir_ikw);
          string a_ydir_skw = a_descr_p->getSKeyword(a_ydir_ikw);
          string a_zdir_skw = a_descr_p->getSKeyword(a_zdir_ikw);
          //
          value_type_e xvtype = a_descr_p->getValueType(a_xdir_ikw);
          value_type_e yvtype = a_descr_p->getValueType(a_xdir_ikw);
          value_type_e zvtype = a_descr_p->getValueType(a_xdir_ikw);
          int a_xdir=0,a_ydir=0,a_zdir=0;
          if(ind<0) {
              if(xvtype == VTYPE_UINT)
                  a_xdir=(int)pre_object.GetUIntValue(a_xdir_skw.c_str());
              else if(xvtype == VTYPE_BOOL)
              {
                  bool b_val = pre_object.GetBoolValue(a_xdir_skw.c_str());
                  if(b_val == true)
                     a_xdir = 1;
                  else
                     a_xdir = 0;
              }
              else 
                  a_xdir=pre_object.GetIntValue(a_xdir_skw.c_str());

              if(yvtype == VTYPE_UINT)
                  a_ydir=(int)pre_object.GetUIntValue(a_ydir_skw.c_str());
              else if(yvtype == VTYPE_BOOL)
              {
                  bool b_val = pre_object.GetBoolValue(a_ydir_skw.c_str());
                  if(b_val == true)
                     a_ydir = 1;
                  else
                     a_ydir = 0;
              }
              else
                  a_ydir=pre_object.GetIntValue(a_ydir_skw.c_str());

              if(zvtype == VTYPE_UINT)
                  a_zdir=(int)pre_object.GetUIntValue(a_zdir_skw.c_str());
              else if(zvtype == VTYPE_BOOL)
              {
                  bool b_val = pre_object.GetBoolValue(a_zdir_skw.c_str());
                  if(b_val == true)
                     a_zdir = 1;
                  else
                     a_zdir = 0;
              }
              else
                  a_zdir=pre_object.GetIntValue(a_zdir_skw.c_str());

        }
        else {
              int a_attrib_ind = 0;
              if(xvtype == VTYPE_UINT)
              {
                  a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_UINT,a_xdir_skw);
                  a_xdir=(int)pre_object.GetUIntValue(a_attrib_ind,ind);
              }
              else if(xvtype == VTYPE_BOOL)
              {
                  a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_BOOL,a_xdir_skw);
                  bool b_xdir=pre_object.GetBoolValue(a_attrib_ind,ind);
                  if(b_xdir==true)
                     a_xdir=1;
                  else
                     a_xdir=0;
              }
              else 
              {
                  a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,a_xdir_skw);
                  a_xdir=pre_object.GetIntValue(a_attrib_ind,ind);
              }

              if(yvtype == VTYPE_UINT)
              {
                  a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_UINT,a_ydir_skw);
                  a_ydir=(int)pre_object.GetIntValue(a_attrib_ind,ind);
              }
              else if(xvtype == VTYPE_BOOL)
              {
                  a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_BOOL,a_ydir_skw);
                  bool b_ydir=pre_object.GetBoolValue(a_attrib_ind,ind);
                  if(b_ydir==true)
                     a_ydir=1;
                  else
                     a_ydir=0;
              }
              else
              {
                  a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,a_ydir_skw);
                  a_ydir=pre_object.GetIntValue(a_attrib_ind,ind);
              }

              if(yvtype == VTYPE_UINT)
              {
                  a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_UINT,a_zdir_skw);
                  a_zdir=(int)pre_object.GetUIntValue(a_attrib_ind,ind);
              }
              else if(zvtype == VTYPE_BOOL)
              {
                  a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_BOOL,a_zdir_skw);
                  bool b_zdir=pre_object.GetBoolValue(a_attrib_ind,ind);
                  if(b_zdir==true)
                     a_zdir=1;
                  else
                     a_zdir=0;
              }
              else
              {
                  a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,a_zdir_skw);
                  a_zdir=pre_object.GetIntValue(a_attrib_ind,ind);
              }
          }
          //
          const char *a_dir_str=HCDI_MV_get_directions_str(a_xdir,a_ydir,a_zdir);


		  if(myWriteFreeFormat == false)
			  myWriteContext_p->WriteFile(a_cell_fmt,a_dir_str);
		  else
          { 
			  myWriteContext_p->WriteFile("%s",a_dir_str);
              string str(mydelimiter);
              myWriteContext_p->WriteFile(str.c_str());
      }
      }
      break;
  case CELL_DIGITS:
      {
          typedef vector<int> LocAttribIndexes_t;
          LocAttribIndexes_t  a_attrib_indexes;
          vector<int>  ikeywordstype;
          //
          int         i          = 0;
          int         a_nb_ikws  = 0;
          const char *a_cell_fmt = NULL;
          //
          MCDS_get_ff_cell_attributes(a_cell_p,
              CELL_FORMAT,      &a_cell_fmt,
              CELL_NB_IKEYWORDS,&a_nb_ikws,
              END_ARGS);
		  int fmt_size = 0;
		  a_cell_fmt = GetFormatSize(a_cell_fmt, fmt_size);
		  myCurActiveLineLength += fmt_size;
          //
          IMECPreObject::MyAttributeType_e a_atype=(ind<0 ? IMECPreObject::ATY_SINGLE : IMECPreObject::ATY_ARRAY);
          a_attrib_indexes.reserve(a_nb_ikws);
          for(i=0;i<a_nb_ikws;++i) {
              int a_ikeyword=END_ARGS;
              MCDS_get_ff_cell_tab(a_cell_p,CELL_IKEYWORD,i,(void *)(&a_ikeyword));
              //
              string a_skeyword   = a_descr_p->getSKeyword(a_ikeyword);
              value_type_e a_cell_vtype = a_descr_p->getValueType(a_ikeyword);
              int    a_attrib_ind = 0;
              if(a_cell_vtype == VTYPE_UINT)
                  a_attrib_ind = pre_object.GetIndex(a_atype,IMECPreObject::VTY_UINT,a_skeyword);
              else
                  a_attrib_ind = pre_object.GetIndex(a_atype,IMECPreObject::VTY_INT,a_skeyword);   

              if(a_attrib_ind>=0) 
              {
                a_attrib_indexes.push_back(a_attrib_ind);
                ikeywordstype.push_back(a_cell_vtype);
              }
          }
          //
          a_nb_ikws=(int)a_attrib_indexes.size();
          int a_value=0;
          if(ind<0) {
              for(i=0;i<a_nb_ikws;++i) {
                  a_value*=10;	  
                  if(ikeywordstype[i] == VTYPE_UINT)
                      a_value+=(int)pre_object.GetUIntValue(a_attrib_indexes[i]);
                  else
                      a_value+=pre_object.GetIntValue(a_attrib_indexes[i]);
              }
        }
        else {
              for(i=0;i<a_nb_ikws;++i) {
                  a_value*=10;	  
                  if(ikeywordstype[i] == VTYPE_UINT)
                      a_value+=(int)pre_object.GetUIntValue(a_attrib_indexes[i],ind);
                  else
                      a_value+=pre_object.GetIntValue(a_attrib_indexes[i],ind);
              }
          }
		  if(myWriteFreeFormat == false)
			  myWriteContext_p->WriteFile(a_cell_fmt,a_value);
		  else
          { 
			  myWriteContext_p->WriteFile("%d",a_value);
              string str(mydelimiter);
              myWriteContext_p->WriteFile(str.c_str());
      }
      }
      
  case CELL_SCALAR_OR_OBJECT:
  {
      WriteCell_SCALAR_OR_OBJECT (cell_p, pre_object, descr_p, ind); 
  }
  break;
  
  
  case CELL_FLAGGED_OBJECT:
  {
      // get info about format and object
      const char *a_cell_fmt = NULL;
      int fmt_size=0;
      int   a_object_ikw=END_ARGS, a_flag_ikw=END_ARGS;
      MCDS_get_ff_cell_attributes(a_cell_p,
				  CELL_FORMAT,    &a_cell_fmt,
                                  CELL_OBJECT_IKW,&a_object_ikw,
                                  CELL_FLAG_IKW,  &a_flag_ikw,
                                  END_ARGS);
      string       a_flag_skw     = a_descr_p->getSKeyword(a_flag_ikw);
      a_cell_fmt = GetFormatSize(a_cell_fmt, fmt_size);
	  myCurActiveLineLength += fmt_size;
      value_type_e a_cell_vtype = a_descr_p->getValueType(a_flag_ikw);
      // get the flag
      int is_flagged = 0;
      bool is_parameter = false;                    
      bool is_parameter_negated = false;
      string param_name = "";
      char temp_cell_fmt[10];
      if (ind >= 0)
      {
          attribute_type_e a_atype = a_descr_p->getAttributeType(a_flag_ikw); // adding flag_ikeyword as both flag and object attribute type will be the same
          if (a_atype == ATYPE_VALUE)
              ind = -1;
      }
      if(a_cell_vtype == VTYPE_UINT)
      {
          if(ind<0) {
              is_flagged=(int)pre_object.GetUIntValue(a_flag_skw.c_str());
            }
            else {
              int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_UINT,a_flag_skw);
              is_flagged=pre_object.GetUIntValue(a_attrib_ind,ind);
          }
      }
      else if(a_cell_vtype == VTYPE_BOOL)
      {
          if(ind<0) {
              bool b_flagged=pre_object.GetBoolValue(a_flag_skw.c_str());
              if(b_flagged==true)
                  is_flagged = 1;
              else
                  is_flagged = 0;
            }
            else {
              int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_BOOL,a_flag_skw);
              if (a_attrib_ind >= 0)
              {
                  bool b_flagged = pre_object.GetBoolValue(a_attrib_ind, ind);
                  if (b_flagged == true)
                      is_flagged = 1;
                  else
                      is_flagged = 0;
              }
          }
      }
      else
      {
          if(ind<0) {
              is_flagged=pre_object.GetIntValue(a_flag_skw.c_str());
            }
            else {
              int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,a_flag_skw);
              is_flagged=pre_object.GetIntValue(a_attrib_ind,ind);
          }
      }
      // get the ID of the object
      string a_object_skw = a_descr_p->getSKeyword(a_object_ikw);
        long long int a_value = 0;  // Revisit if MYOBJ_INT type is changed and see if FLAGGED_OBJECT needs any changes to be done.
        MYOBJ_INT a_uval = 0;
      is_parameter = pre_object.IsParameter(a_object_skw.c_str(),ind);
      if(is_parameter)
      { 
          sprintf (temp_cell_fmt, "%%%ds", fmt_size);          
          param_name = pre_object.GetParameterName(a_object_skw.c_str(),ind, &is_parameter_negated);
          if(is_flagged == 1){is_parameter_negated=true;}
          WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
      }
      else 
      {
            int a_attrib_ind=-1;
          if(ind<0) {
              a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_OBJECT,a_object_skw);
                a_uval = pre_object.GetObjectId(a_attrib_ind);
            }
            else {
              a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_OBJECT,a_object_skw);
                a_uval = pre_object.GetObjectId(a_attrib_ind, ind);
          }
          // flag the ID of the object
          if (is_flagged) {
              // this is made for Dyna:
              // If the object is flagged, we write its negative ID.
              // This might be Dyna specific, but I don't see another way to flag an ID than to have its negative value!
                a_value = 0 - (long long int)a_uval;
          } // is_flagged
            else
                a_value = (long long int)a_uval;
          // write the flagged ID of the object,
          // handle double format when writing int. This is done in Dyna. (CS#712#06_03_06 beg)
          // myWriteContext_p->WriteFile(a_cell_fmt,a_value);


          if(a_attrib_ind >=0)
          {
                bool inRange = true;
                if (a_value < 0 && fmt_size <= 10)
                    inRange = IsValueDigitInRange(a_uval, fmt_size - 1);
                else if (fmt_size > 0 && fmt_size < 10)
                    inRange = IsValueDigitInRange(a_uval, fmt_size);
                else if (fmt_size == 10)
                    inRange = a_value > INT_MAX ? false : true;

                if (inRange == true)
                {
		            if(myWriteFreeFormat == false)
                        WriteObjectId(a_cell_fmt, a_value);
		            else
                        myWriteContext_p->WriteFile("%u", a_value);
                }
                else
                {
                char a_value_str[21];
                int i_char;
                for (i_char = 0; i_char < fmt_size; ++i_char)
                    a_value_str[i_char] = '*';
                a_value_str[i_char] = '\0';
                if(myWriteFreeFormat == true)
                    fmt_size = 0;

                sprintf (temp_cell_fmt, "%%%ds", fmt_size);
                myWriteContext_p->WriteFile(temp_cell_fmt,a_value_str);
                string str(mydelimiter);
                myWriteContext_p->WriteFile(str.c_str());
          }
      }
          else
          {
                const char *a_value_str="";
                a_cell_fmt = GetFormatSize(a_cell_fmt, fmt_size);
                if(myWriteFreeFormat == true)
                {
                    fmt_size = 0;
                }
                sprintf (temp_cell_fmt, "%%%ds", fmt_size);
                myWriteContext_p->WriteFile(temp_cell_fmt,a_value_str);
  }
      }
  }
  break;
  
  
  case CELL_SCALAR_OR_STRING:
  {
      // get info about format and object
      const char *a_cell_fmt = NULL;
      int   a_flag_ikw=END_ARGS,a_scalar_ikw=END_ARGS,a_string_ikw=END_ARGS;
      MCDS_get_ff_cell_attributes(a_cell_p,
				  CELL_FORMAT,    &a_cell_fmt,
                                  CELL_FLAG_IKW,  &a_flag_ikw,
                                  CELL_SCALAR_IKW,&a_scalar_ikw,
                                  CELL_STRING_IKW,&a_string_ikw,
                                  END_ARGS);
      string       a_flag_skw     = a_descr_p->getSKeyword(a_flag_ikw);
      // test whether it is scalar or string
      int is_string = 0;

      value_type_e a_cell_vtype = a_descr_p->getValueType(a_flag_ikw); 
      if(a_cell_vtype == VTYPE_UINT)
      {
          if(ind<0) {
              is_string=pre_object.GetIntValue(a_flag_skw.c_str());
            }
            else {
              int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_UINT,a_flag_skw);
              is_string=(int)pre_object.GetUIntValue(a_attrib_ind,ind);
          }
      }
      else if(a_cell_vtype == VTYPE_BOOL)
      {
          if(ind<0) {
          
              bool a_var = pre_object.GetBoolValue(a_flag_skw.c_str());
              is_string= a_var == true ? 1 : 0;
            }
            else {
              int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_BOOL,a_flag_skw);
              bool a_var=pre_object.GetBoolValue(a_attrib_ind,ind);
              is_string= a_var == true ? 1 : 0;
          }
      }
      else
      {
          if(ind<0) {
              is_string=pre_object.GetIntValue(a_flag_skw.c_str());
            }
            else {
              int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,a_flag_skw);
              is_string=pre_object.GetIntValue(a_attrib_ind,ind);
          }
      }

      if (is_string) {
          // write the string
          WriteCell_VALUE (cell_p, pre_object, descr_p, ind, a_string_ikw); 
        }
        else { // !is_string
          // write the scalar
          WriteCell_VALUE (cell_p, pre_object, descr_p, ind, a_scalar_ikw); 
      } // !is_string
  }
  break;
  
  case CELL_COND:
    {
      WriteIfCell(cell_p,pre_object,descr_p, ind);
    }
    break;


  case CELL_PAIR:
    {
      WritePairCell(cell_p,pre_object,descr_p, ind);
    }
    break;
  case CELL_LIST:
  {
      WriteCellArrayList(cell_p, pre_object, descr_p, ind);
  }
  break;
  case CELL_BLANK:
  {
      char *a_cell_fmt = NULL;
      MCDS_get_ff_cell_attributes(a_cell_p, CELL_FORMAT, &a_cell_fmt, END_ARGS);
      int a_cell_size = 0;// loc_get_fmt_size(a_cell_fmt);

      a_cell_fmt = (char *)(GetFormatSize(a_cell_fmt, a_cell_size));
	  myCurActiveLineLength += a_cell_size;
      char temp_cell_fmt[10];
      const char *a_value_str = "";
      if (myWriteFreeFormat == true)
          a_cell_size = 0;
      sprintf(temp_cell_fmt, "%%%ds", a_cell_size);
      myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
      break;
  }
	case CELL_NAME_VALUE:
	{
		WriteNameValueCell(cell_p, pre_object, descr_p, ind);
	}
	break;
  case CELL_APPEND_OPTIONS:
  {
      int a_cell_size = 0;
      const char* a_cell_fmt = NULL;
      MCDS_get_ff_cell_attributes(a_cell_p, CELL_FORMAT, &a_cell_fmt, END_ARGS);

      a_cell_fmt = GetFormatSize(a_cell_fmt, a_cell_size);
	  myCurActiveLineLength += a_cell_size;

      ff_append_option_cell_t *a_apcell_p = (ff_append_option_cell_t *)cell_p;
      if (a_apcell_p)
      {
          vector<AppendOptions_t *> &a_vect = a_apcell_p->options;
          int size = (int)a_vect.size();
          int total_size = 0;
          char temp_cell_fmt[10];
          string param_name = "";
          bool is_parameter = false;
          bool is_parameter_negated = false;

          for (int i = 0; i < size; i++)
          {
              AppendOptions_t *data = a_vect[i];

              value_type_e vtype = data->vtype;
              if (vtype == VTYPE_INT)
              {
                  AppendOptionsOthers_t<int> *data2 = ((AppendOptionsOthers_t<int> *)data);
                  const string &skw = data2->getSkeyword();

                  is_parameter = pre_object.IsParameter(skw.c_str(), ind);
                  if (is_parameter)
                  {
                      sprintf(temp_cell_fmt, "%%%ds", a_cell_size);

                      param_name = pre_object.GetParameterName(skw.c_str(), ind, &is_parameter_negated);
                      WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
                  }
                  else
                  {
                      vector<pair <string, int>>& options_values_vect = data2->getOptionsValuesVect();
                      vector<pair <string, int>>::iterator iter_b = options_values_vect.begin();
                      vector<pair <string, int>>::iterator iter_e = options_values_vect.end();
                      vector<pair <string, int>>::iterator iter;

                      int value = pre_object.GetIntValue(skw.c_str());
                      for (iter = iter_b; iter != iter_e; ++iter)
                      {
                          if (value == (*iter).second)
                          {
                              string text = (*iter).first;
                              total_size += (int)text.size();
                              myWriteContext_p->WriteFile(text.c_str());
                          }
                      }
                  }
              }
              else if (vtype == VTYPE_FLOAT)
              {
                  AppendOptionsOthers_t<double> *data2 = ((AppendOptionsOthers_t<double> *)data);
                  const string &skw = data2->getSkeyword();

                  is_parameter = pre_object.IsParameter(skw.c_str(), ind);
                  if (is_parameter)
                  {
                      sprintf(temp_cell_fmt, "%%%ds", a_cell_size);

                      param_name = pre_object.GetParameterName(skw.c_str(), ind, &is_parameter_negated);
                      WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
                  }
                  else
                  {
                      vector<pair <string, double>>& options_values_vect = data2->getOptionsValuesVect();
                      vector<pair <string, double>>::iterator iter_b = options_values_vect.begin();
                      vector<pair <string, double>>::iterator iter_e = options_values_vect.end();
                      vector<pair <string, double>>::iterator iter;

                      double value = pre_object.GetFloatValue(skw.c_str());
                      for (iter = iter_b; iter != iter_e; ++iter)
                      {
                          if (value == (*iter).second)
                          {
                              string text = (*iter).first;
                              total_size += (int)text.size();
                              myWriteContext_p->WriteFile(text.c_str());
                          }
                      }
                  }
              }
              else if (vtype == VTYPE_UINT)
              {
                  AppendOptionsOthers_t<unsigned int> *data2 = ((AppendOptionsOthers_t<unsigned int> *)data);
                  const string &skw = data2->getSkeyword();

                  is_parameter = pre_object.IsParameter(skw.c_str(), ind);
                  if (is_parameter)
                  {
                      sprintf(temp_cell_fmt, "%%%ds", a_cell_size);

                      param_name = pre_object.GetParameterName(skw.c_str(), ind, &is_parameter_negated);
                      WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
                  }
                  else
                  {
                      vector<pair <string, unsigned int>>& options_values_vect = data2->getOptionsValuesVect();
                      vector<pair <string, unsigned int>>::iterator iter_b = options_values_vect.begin();
                      vector<pair <string, unsigned int>>::iterator iter_e = options_values_vect.end();
                      vector<pair <string, unsigned int>>::iterator iter;

                      unsigned int value = pre_object.GetUIntValue(skw.c_str());
                      for (iter = iter_b; iter != iter_e; ++iter)
                      {
                          if (value == (*iter).second)
                          {
                              string text = (*iter).first;
                              total_size += (int)text.size();
                              myWriteContext_p->WriteFile(text.c_str());
                          }
                      }
                  }
              }
              else if (vtype == VTYPE_BOOL)
              {
                  AppendOptionsOthers_t<bool> *data2 = ((AppendOptionsOthers_t<bool> *)data);
                  const string &skw = data2->getSkeyword();

                  is_parameter = pre_object.IsParameter(skw.c_str(), ind);
                  if (is_parameter)
                  {
                      sprintf(temp_cell_fmt, "%%%ds", a_cell_size);

                      param_name = pre_object.GetParameterName(skw.c_str(), ind, &is_parameter_negated);
                      WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
                  }
                  else
                  {
                      vector<pair <string, bool>>& options_values_vect = data2->getOptionsValuesVect();
                      vector<pair <string, bool>>::iterator iter_b = options_values_vect.begin();
                      vector<pair <string, bool>>::iterator iter_e = options_values_vect.end();
                      vector<pair <string, bool>>::iterator iter;

                      bool value = pre_object.GetBoolValue(skw.c_str());
                      for (iter = iter_b; iter != iter_e; ++iter)
                      {
                          if (value == (*iter).second)
                          {
                              string text = (*iter).first;
                              total_size += (int)text.size();
                              myWriteContext_p->WriteFile(text.c_str());
                          }
                      }
                  }
              }
              else if (vtype == VTYPE_STRING)
              {
                  AppendOptionsOthers_t<string> *data2 = ((AppendOptionsOthers_t<string> *)data);
                  const string &skw = data2->getSkeyword();

                  is_parameter = pre_object.IsParameter(skw.c_str(), ind);
                  if (is_parameter)
                  {
                      sprintf(temp_cell_fmt, "%%%ds", a_cell_size);

                      param_name = pre_object.GetParameterName(skw.c_str(), ind, &is_parameter_negated);
                      WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
                  }
                  else
                  {
                      vector<pair <string, string>>& options_values_vect = data2->getOptionsValuesVect();
                      vector<pair <string, string>>::iterator iter_b = options_values_vect.begin();
                      vector<pair <string, string>>::iterator iter_e = options_values_vect.end();
                      vector<pair <string, string>>::iterator iter;

                      string value = pre_object.GetStringValue(skw.c_str());
                      for (iter = iter_b; iter != iter_e; ++iter)
                      {
                          if (value == (*iter).second)
                          {
                              string text = (*iter).first;
                              total_size += (int)text.size();
                              myWriteContext_p->WriteFile(text.c_str());
                          }
                      }
                  }
              }
          }
          if (myWriteFreeFormat == false)
          {
              int pending_spaces = a_cell_size - total_size;
              while(pending_spaces > 0)
              {
                  myWriteContext_p->WriteFile(" ");
                  pending_spaces--;
              }
          }
      }
  }
    break;
  default:
    break;
  }
}


bool MECDataWriter::GetCellDisplayStatus_VALUE(const PseudoFileFormatCell_t *cell_p,
    const IMECPreObject          &pre_object,
    const PseudoDescriptor_t     *descr_p,
    int                           ind,
    int                           ikeyword)
{
    const IDescriptor *a_descr_p = (const IDescriptor *)descr_p;
    ff_cell_t         *a_cell_p = (ff_cell_t *)cell_p;
    //
    int         a_cell_ikw = ikeyword;
    if (END_ARGS == a_cell_ikw) {
        MCDS_get_ff_cell_attributes(a_cell_p, CELL_IKEYWORD, &a_cell_ikw, END_ARGS);
    }
    //
    string       a_cell_skw = a_descr_p->getSKeyword(a_cell_ikw);
    value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);

    if (ind >= 0)
    {
        attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
        if (a_atype == ATYPE_VALUE)
            ind = -1;
    }
    //
    switch (a_cell_vtype) {
    case VTYPE_INT:
    {
        int a_attrib_ind = -1;

        if (ind < 0)
            a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_cell_skw.c_str());
        else
            a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_cell_skw.c_str());

        if (a_attrib_ind > -1)
            return true;
        else
            return false;
    }
    break;
    case VTYPE_UINT:
    {
        int a_attrib_ind = -1;

        if (ind < 0)
            a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_UINT, a_cell_skw.c_str());
        else
            a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_cell_skw.c_str());

        if (a_attrib_ind > -1)
            return true;
        else
            return false;
    }
    break;
    case VTYPE_FLOAT:
    {
        int a_attrib_ind = -1;

        if (ind < 0)
            a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_FLOAT, a_cell_skw.c_str());
        else
            a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_cell_skw.c_str());

        if (a_attrib_ind > -1)
            return true;
        else
            return false;
    }
    break;
    case VTYPE_STRING:
    {
        int a_attrib_ind = -1;

        if (ind < 0)
            a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, a_cell_skw.c_str());
        else
            a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_cell_skw.c_str());

        if (a_attrib_ind > -1)
            return true;
        else
            return false;
    }
    break;
    case VTYPE_OBJECT:
    {
        int a_attrib_ind = -1;

        if (ind < 0)
            a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, a_cell_skw.c_str());
        else
            a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_cell_skw.c_str());

        if (a_attrib_ind > -1)
            return true;
        else
            return false;
    }
    break;
    default:
        break;
    }
    return false;
}



void MECDataWriter::WriteCell_VALUE(const PseudoFileFormatCell_t *cell_p,
                                    const IMECPreObject       &pre_object,
                                    const PseudoDescriptor_t *descr_p,
                                    int                       ind,
                                    int                       ikeyword)
{
    const IDescriptor* a_descr_p = (const IDescriptor*)descr_p;
    ff_cell_t* a_cell_p = (ff_cell_t*)cell_p;
    //
    const char* a_cell_fmt = NULL;
    bool is_parameter = false;
    string param_name = "";
    char temp_cell_fmt[10];
    bool is_parameter_negated = false;
    
    MCDS_get_ff_cell_attributes(a_cell_p, CELL_FORMAT, &a_cell_fmt, END_ARGS);
    int         a_cell_ikw = ikeyword;
    if (END_ARGS == a_cell_ikw) {
        MCDS_get_ff_cell_attributes(a_cell_p, CELL_IKEYWORD, &a_cell_ikw, END_ARGS);
    }
    
    string       a_cell_skw = a_descr_p->getSKeyword(a_cell_ikw);
    value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);
    int fmt_size = 0;
    a_cell_fmt = GetFormatSize(a_cell_fmt, fmt_size);
    if (myCurActiveLineLength >= myLineLength)
    {
        myCurActiveLineLength = 0;
    }
    myCurActiveLineLength += fmt_size;

    if (ind >= 0)
    {
        attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
        if (a_atype == ATYPE_VALUE)
            ind = -1;
    }

    //
    switch (a_cell_vtype) {
    case VTYPE_BOOL:
    {
        int a_value = 0;
        bool a_bool_value = false;
        is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else
        {
            int a_attrib_ind = -1;
            if (ind < 0)
            {
                a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_BOOL, a_cell_skw);
                if (a_attrib_ind >= 0)
                {
                    a_bool_value = pre_object.GetBoolValue(a_cell_skw.c_str());
                }
            }
            else
            {
                a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_cell_skw);
                a_bool_value = pre_object.GetBoolValue(a_attrib_ind, ind);
            }

            if (a_attrib_ind >= 0)
            {
                if (a_bool_value == false)
                    a_value = 0;
                else if (a_bool_value == true)
                    a_value = 1;
                if (myWriteFreeFormat == false)
                {
                    WriteInteger(a_cell_fmt, a_value);
                }
                else
                {
                    WriteInteger("%d", a_value);
                }
            }
            else
            {
                bool has_default = false;
                int a_value_d = 0;
                bool a_bool_value_d = a_descr_p->getBoolDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
                if (a_bool_value_d == false)
                    a_value_d = 0;
                else if (a_bool_value_d == true)
                    a_value_d = 1;
                if (mywriteDefaultValue == false || has_default == false)
                {
                    const char* a_value_str = "";
                    if (myWriteFreeFormat == true)
                        fmt_size = 0;
                    sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                    myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
                }
                else
                {
                    if (myWriteFreeFormat == false && a_value_d == 1)
                        WriteInteger(a_cell_fmt, a_value_d);
                    else
                    {
                        const char* a_value_empty = "";
                        myWriteContext_p->WriteFile("%s", a_value_empty);
                    }
                }
            }
        }
    }
    break;
    case VTYPE_INT:
    {
        int a_value = 0;
        is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else
        {
            int a_attrib_ind = -1;
            if (ind < 0)
            {
                a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_cell_skw);
                if (a_attrib_ind >= 0)
                {
                    a_value = pre_object.GetIntValue(a_attrib_ind);
                }
            }
            else
            {
                a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_cell_skw);
                if (a_attrib_ind >= 0)
                {
                    a_value = pre_object.GetIntValue(a_attrib_ind, ind);
                }
            }

            if (a_attrib_ind >= 0 && a_value > INT_MIN)
            {
                if (myWriteFreeFormat == false)
                {
                    bool inRange = true;
                    if (fmt_size > 0 && fmt_size < 10)
                    {
                        int val_size = fmt_size;
                        unsigned int l_value = 0;
                        if (a_value < 0)
                        {
                            l_value = -1 * a_value;
                            --val_size;
                        }
                        else
                            l_value = a_value;

                        inRange = IsValueDigitInRange(l_value, val_size);
                    }
                    if (inRange == true)
                        WriteInteger(a_cell_fmt, a_value);
                    else
                    {
                        char a_value_str[21];
                        int i_char;
                        for (i_char = 0; i_char < fmt_size; ++i_char)
                            a_value_str[i_char] = '*';
                        a_value_str[i_char] = '\0';

                        sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                        myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);

                        func_ptr fptr = GetFileopFieldOverflowPtr();
                        if (fptr != NULL)
                            (*fptr)(temp_cell_fmt, fmt_size);
                    }
                }
                else
                    myWriteContext_p->WriteFile("%d", a_value);
            }
            else
            {
                bool has_default = false;
                int a_value_d = a_descr_p->getIntDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
                if (mywriteDefaultValue == false || a_value <= INT_MIN || has_default == false)
                {
                    const char* a_value_str = "";
                    if (myWriteFreeFormat == true)
                        fmt_size = 0;
                    sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                    myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
                }
                else
                {
                    if (myWriteFreeFormat == false)
                        WriteInteger(a_cell_fmt, a_value_d);
                    else
                        WriteInteger("%d", a_value_d);
                }
            }
        }
    }
    break;
    case VTYPE_UINT:
    {
        unsigned int a_value = 0;
        is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else 
        {
            int a_attrib_ind = -1;
            if (ind < 0) {
                a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_UINT, a_cell_skw);
                a_value = pre_object.GetUIntValue(a_cell_skw.c_str());
            }
            else {
                a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_cell_skw);
                if (a_attrib_ind >= 0)
                {
                    a_value = pre_object.GetUIntValue(a_attrib_ind, ind);
                }
            }

            if (a_attrib_ind >= 0 && a_value < UINT_MAX)
            {
                if (myWriteFreeFormat == false)
                {
                    bool inRange = true;
                    if (fmt_size > 0 && fmt_size < 10)
                        inRange = IsValueDigitInRange(a_value, fmt_size);
                    if (inRange == true)
                        WriteUInteger(a_cell_fmt, a_value);
                    else
                    {
                        char a_value_str[21];
                        int i_char;
                        for (i_char = 0; i_char < fmt_size; ++i_char)
                            a_value_str[i_char] = '*';
                        a_value_str[i_char] = '\0';

                        sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                        myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);

                        func_ptr fptr = GetFileopFieldOverflowPtr();
                        if (fptr != NULL)
                            (*fptr)(temp_cell_fmt, fmt_size);
                    }
                }
                else
                    WriteUInteger("%u", a_value);
            }
            else
            {
                bool has_default = false;
                unsigned int a_value_d = a_descr_p->getUIntDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
                if (mywriteDefaultValue == false || a_value >= UINT_MAX || has_default == false)
                {
                    const char* a_value_str = "";
                    if (myWriteFreeFormat == true)
                        fmt_size = 0;
                    sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                    myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
                }
                else
                {
                    if (myWriteFreeFormat == false)
                        WriteUInteger(a_cell_fmt, a_value_d);
                    else
                        WriteUInteger("%u", a_value_d);
                }
            }
        }
    }
    break;
    case VTYPE_FLOAT:
    {
        double a_value = 0.;
        is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else
        {
            int a_attrib_ind = -1;
            if (ind < 0) {
                a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_FLOAT, a_cell_skw);
                if(a_attrib_ind >= 0)
                    a_value = pre_object.GetFloatValue(a_attrib_ind);
            }
            else {
                a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_cell_skw);
                if (a_attrib_ind >= 0)
                {
                    a_value = pre_object.GetFloatValue(a_attrib_ind, ind);
                }
            }
            if (a_attrib_ind >= 0 && a_value > -DBL_MAX)
            {
                WriteDouble(a_cell_fmt, fmt_size, a_value);
            }
            else
            {
                bool has_default = false;
                double a_value_d = a_descr_p->getFloatDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
                if (mywriteDefaultValue == false || a_value <= -DBL_MAX || has_default == false)
                {
                    const char* a_value_str = "";
                    if (myWriteFreeFormat == true)
                        fmt_size = 0;
                    sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                    myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
                }
                else
                {
                    if (myWriteFreeFormat == false)
                        WriteDouble(a_cell_fmt, fmt_size, a_value_d);
                    else
                        WriteDouble(a_cell_fmt, 0, a_value_d);
                }
            }
        }
    }
    break;
    case VTYPE_STRING:
    {
        const char* a_value = NULL;
        is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else {
            if (ind < 0) {
                
                if (a_cell_skw.compare("TITLE") == 0) {
                    a_value = pre_object.GetTitle();
                }
                else {
                    
                    a_value = pre_object.GetStringValue(a_cell_skw.c_str());
                }
            }
            else {
                int a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_cell_skw);
                if (a_attrib_ind >= 0)
                {
                    a_value = pre_object.GetStringValue(a_attrib_ind, ind);
                }
            }
            if (a_value == NULL) a_value = ""; 
            if (strchr(a_cell_fmt, 's') == NULL)
            {
                char* pos = NULL;
                pos = (char*)strchr(a_cell_fmt, 'd');
                if (pos != NULL)
                {
                    pos[0] = 's';
                }
                else
                {
                    pos = (char*)strchr(a_cell_fmt, 'l');
                    if (pos != NULL)
                    {
                        strcpy(pos, "s");
                    }
                }
            }
            string a_value_d;
            if ((*a_value == '\0') && mywriteDefaultValue == true)
            {
                bool has_default = false;
                a_value_d = a_descr_p->getStringDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
                if (has_default)
                {
                    a_value = a_value_d.c_str();
                }
            }
            if (myWriteFreeFormat == true)
                myWriteContext_p->WriteFile("%s", a_value);
            else
                myWriteContext_p->WriteFile(a_cell_fmt, a_value);
        }
    }
    break;
    case VTYPE_OBJECT:
    {
        MYOBJ_INT a_value = 0; // Revisit if MYOBJ_INT type is changed and see if VTYPE_OBJECT case needs any changes to be done.
        is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else
        {
            const char* a_value_str = "";
            int a_attrib_ind = -1;
            if (ind < 0) {
                a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, a_cell_skw);
                if (a_attrib_ind >= 0)
                {
                    a_value_str = pre_object.GetObjectName(a_attrib_ind);
                    if (a_value_str && a_value_str[0] == '\0')
                        a_value = pre_object.GetObjectId(a_attrib_ind);
                }
            }
            else {
                a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_cell_skw);

                if (a_attrib_ind >= 0)
                {
                    a_value_str = pre_object.GetObjectName(a_attrib_ind, ind);
                    if (a_value_str && a_value_str[0] == '\0')
                        a_value = pre_object.GetObjectId(a_attrib_ind, ind);
                }
            }

            if (a_attrib_ind >= 0 && a_value >= 0 && a_value < UINT_MAX && (!a_value_str || a_value_str[0] == '\0'))
            {
                bool inRange = true;
                if (fmt_size > 0 && fmt_size < 10)
                    inRange = IsValueDigitInRange(a_value, fmt_size);
                else if (fmt_size == 10)
                    inRange = a_value > INT_MAX ? false : true;
                if (inRange == true)
                {
                    if (myWriteFreeFormat == false)
                        WriteObjectId(a_cell_fmt, a_value);
                    else
                        myWriteContext_p->WriteFile("%u", a_value);
                }
                else
                {
                    char a_lvalue_str[21];
                    int i_char;
                    for (i_char = 0; i_char < fmt_size; ++i_char)
                        a_lvalue_str[i_char] = '*';
                    a_lvalue_str[i_char] = '\0';
                    if (myWriteFreeFormat == true)
                        fmt_size = 0;
                    sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                    myWriteContext_p->WriteFile(temp_cell_fmt, a_lvalue_str);

                    func_ptr fptr = GetFileopFieldOverflowPtr();
                    if (fptr != NULL)
                        (*fptr)(temp_cell_fmt, fmt_size);
                }
            }
            else
            {
                if (myWriteFreeFormat == true)
                    fmt_size = 0;

                sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
            }
        }
    }
    break;
    default:
        break;
    }
    
    if (((ind != -1 && !fmt_size && a_cell_vtype != VTYPE_STRING) || myWriteFreeFormat == true) && myIsLastCell == false)
    {
        string str(mydelimiter);
        myWriteContext_p->WriteFile(str.c_str());
    }
}

void MECDataWriter::WriteCell_VALUE_LIST(const PseudoFileFormatCell_t *cell_p,
                                    const IMECPreObject          &pre_object,
                                    const PseudoDescriptor_t    *descr_p,
                                    card_cells_temp_t           *cell_det,
                                    int                          ind,
                                    int                          ikeyword)
{
    const IDescriptor* a_descr_p = (const IDescriptor*)descr_p;
    ff_cell_t* a_cell_p = (ff_cell_t*)cell_p;
    //
    const char* a_cell_fmt = NULL;
    bool is_parameter = false;

    char temp_cell_fmt[10];
    bool is_parameter_negated = false;

    int   a_cell_ikw = cell_det->ikeyword;
    string& a_cell_skw = cell_det->skeyword;
    value_type_e a_cell_vtype = cell_det->val_type;
    int fmt_size = cell_det->width;
    //MCDS_get_ff_cell_attributes(a_cell_p,CELL_FORMAT,&a_cell_fmt,END_ARGS);
    a_cell_fmt = cell_det->fmt;
    if (ind >= 0)
    {
        if (cell_det->att_type == ATYPE_VALUE || cell_det->att_type == ATYPE_SIZE)
            ind = -1;
    }

    //
    switch (a_cell_vtype) {
    case VTYPE_BOOL:
    {
        int a_value = 0;
        bool a_bool_value = false;
        if (getHasParameterFlag())
            is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            string param_name = "";
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else
        {
            int a_attrib_ind = cell_det->att_indx;
            if (a_attrib_ind < 0)
            {
                if (ind < 0)
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_BOOL, a_cell_skw);
                else
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_cell_skw);
            }
            if (a_attrib_ind >= 0)
            {
                if (ind < 0)
                    a_bool_value = pre_object.GetBoolValue(a_cell_skw.c_str());
                else
                    a_bool_value = pre_object.GetBoolValue(a_attrib_ind, ind);
            }
            if (a_bool_value == false)
                a_value = 0;
            else if (a_bool_value == true)
                a_value = 1;
            if (a_attrib_ind >= 0)
            {

                if (myWriteFreeFormat == false)
                {
                    if (a_value == 1)
                        WriteInteger(a_cell_fmt, a_value);
                    else
                    {
                        const char* a_value_str = "";
                        sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                        myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
                    }
                }
                else
                {
                    if (a_value == 1)
                        WriteInteger("%d", a_value);
                    else
                    {
                        const char* a_value_empty = "";
                        myWriteContext_p->WriteFile("%s", a_value_empty);
                    }
                }
            }
            else
            {
                bool has_default = cell_det->has_default;
                bool a_lbool_value = cell_det->att_default_bool;
                int a_value_d = 0;
                if (a_lbool_value == false)
                    a_value_d = 0;
                else if (a_lbool_value == true)
                    a_value_d = 1;

                if (mywriteDefaultValue == false || has_default == false)
                {
                    const char* a_value_str = "";
                    if (myWriteFreeFormat == true)
                        fmt_size = 0;
                    sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                    myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
                }
                else
                {
                    if (myWriteFreeFormat == false && a_value_d == 1)
                        WriteInteger(a_cell_fmt, a_value_d);
                    else
                    {
                        const char* a_value_empty = "";
                        myWriteContext_p->WriteFile("%s", a_value_empty);
                    }
                }
            }
        }
    }
    break;
    case VTYPE_INT:
    {
        int a_value = 0;
        if (getHasParameterFlag())
            is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            string param_name = "";
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else
        {
            int a_attrib_ind = cell_det->att_indx;
            if (a_attrib_ind < 0)
            {
                if (ind < 0)
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_cell_skw);
                else
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_cell_skw);
            }
            if (a_attrib_ind >= 0)
            {
                if (ind < 0)
                    a_value = pre_object.GetIntValue(a_cell_skw.c_str());
                else
                    a_value = pre_object.GetIntValue(a_attrib_ind, ind);
            }
            if (a_attrib_ind >= 0 && a_value > INT_MIN)
            {
                if (myWriteFreeFormat == true)
                    WriteInteger("%d", a_value);
                else
                    WriteInteger(a_cell_fmt, a_value);
            }
            else
            {
                bool has_default = cell_det->has_default;
                int a_value_d = cell_det->att_default_int;
                if (mywriteDefaultValue == false || a_value <= INT_MIN || has_default == false)
                {
                    const char* a_value_str = "";
                    if (myWriteFreeFormat == true)
                        fmt_size = 0;
                    sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                    myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
                }
                else
                {
                    if (myWriteFreeFormat == false)
                        WriteInteger(a_cell_fmt, a_value_d);
                    else
                        WriteInteger("%d", a_value_d);
                }
            }
        }
    }
    break;
    case VTYPE_UINT:
    {
        unsigned int a_value = 0;
        if (getHasParameterFlag())
            is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            string param_name = "";
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else
        {
            int a_attrib_ind = cell_det->att_indx;
            if (a_attrib_ind < 0)
            {
                if (ind < 0)
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_UINT, a_cell_skw);
                else
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_cell_skw);
            }
            if (a_attrib_ind >= 0)
            {
                if (ind < 0) {
                    a_value = pre_object.GetUIntValue(a_cell_skw.c_str());
                }
                else {
                    a_value = pre_object.GetUIntValue(a_attrib_ind, ind);
                }
            }
            if (a_attrib_ind >= 0 && a_value < UINT_MAX)
            {
                if (myWriteFreeFormat == true)
                    WriteUInteger("%u", a_value);
                else
                    WriteUInteger(a_cell_fmt, a_value);
            }
            else
            {
                bool has_default = cell_det->has_default;
                unsigned int a_value_d = cell_det->att_default_int;
                if (mywriteDefaultValue == false || a_value >= UINT_MAX || has_default == false)
                {
                    const char* a_value_str = "";
                    if (myWriteFreeFormat == true)
                        fmt_size = 0;
                    sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                    myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
                }
                else
                {
                    if (myWriteFreeFormat == false)
                        WriteUInteger(a_cell_fmt, a_value_d);
                    else
                        WriteUInteger("%u", a_value_d);
                }
            }
        }
    }
    break;
    case VTYPE_FLOAT:
    {
        double a_value = 0.;
        if (getHasParameterFlag())
            is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            string param_name = "";
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else
        {
            int a_attrib_ind = cell_det->att_indx;
            if (a_attrib_ind < 0)
            {
                if (ind < 0)
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_FLOAT, a_cell_skw);
                else
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_cell_skw);
            }
            if (a_attrib_ind >= 0)
            {
                if (ind < 0) {
                    a_value = pre_object.GetFloatValue(a_cell_skw.c_str());
                }
                else {
                    a_value = pre_object.GetFloatValue(a_attrib_ind, ind);
                }
            }
            if (a_attrib_ind >= 0 && a_value > -DBL_MAX)
            {
                WriteDouble(a_cell_fmt, fmt_size, a_value);
            }
            else
            {
                bool has_default = cell_det->has_default;
                double a_value_d = cell_det->att_default_double;
                if (mywriteDefaultValue == false || a_value <= -DBL_MAX || has_default == false)
                {
                    const char* a_value_str = "";
                    if (myWriteFreeFormat == true)
                        fmt_size = 0;
                    sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                    myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
                }
                else
                {
                    WriteDouble(a_cell_fmt, fmt_size, a_value_d);
                }
            }
        }
    }
    break;
    case VTYPE_STRING:
    {
        const char* a_value = NULL;
        if (getHasParameterFlag())
            is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            string param_name = "";
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else {
            int a_attrib_ind = cell_det->att_indx;
            if (a_attrib_ind < 0)
            {
                if (ind < 0)
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, a_cell_skw);
                else
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_cell_skw);
            }
            if (a_attrib_ind >= 0)
            {
                if (ind < 0) {
                    a_value = pre_object.GetStringValue(a_attrib_ind);
                }
                else {
                    a_value = pre_object.GetStringValue(a_attrib_ind, ind);
                }
            }
            if (a_value == NULL) a_value = "";
            if (strchr(a_cell_fmt, 's') == NULL)
            {
                char* pos = NULL;
                pos = (char*)strchr(a_cell_fmt, 'd');
                if (pos != NULL)
                {
                    pos[0] = 's';
                }
                else
                {
                    pos = (char*)strchr(a_cell_fmt, 'l');
                    if (pos != NULL)
                    {
                        strcpy(pos, "s");
                    }
                }
            }
            string a_value_d;
            if ((*a_value == '\0') && mywriteDefaultValue == true)
            {
                bool has_default = cell_det->has_default;
                a_value_d = cell_det->att_default_string;
                if (has_default)
                {
                    a_value = a_value_d.c_str();
                }
            }
            if (myWriteFreeFormat == true)
                myWriteContext_p->WriteFile("%s", a_value);
            else
                myWriteContext_p->WriteFile(a_cell_fmt, a_value);
        }
    }
    break;
    case VTYPE_OBJECT:
    {
        MYOBJ_INT a_value = 0;
        if (getHasParameterFlag())
            is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
        if (is_parameter)
        {
            string param_name = "";
            sprintf(temp_cell_fmt, "%%%ds", fmt_size);
            //sprintf (temp_cell_fmt, "%s%d%s", "%",fmt_size,"s");
            param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
            WriteParameterCell(temp_cell_fmt, param_name.c_str(), is_parameter_negated);
        }
        else
        {
            const char* a_value_str = "";
            int a_attrib_ind = cell_det->att_indx;
            if (a_attrib_ind < 0)
            {
                if (ind < 0)
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, a_cell_skw);
                else
                    a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_cell_skw);
            }
            if (a_attrib_ind >= 0)
            {
                if (ind < 0) {
                    a_value_str = pre_object.GetObjectName(a_attrib_ind);
                    if (a_value_str[0] == '\0')
                        a_value = pre_object.GetObjectId(a_attrib_ind);

                }
                else {
                    a_value_str = pre_object.GetObjectName(a_attrib_ind, ind);
                    if (a_value_str[0] == '\0')
                        a_value = pre_object.GetObjectId(a_attrib_ind, ind);
                }
            }

            // Revisit if MYOBJ_INT type is changed and see if VTYPE_OBJECT case needs any changes to be done.
            if (a_attrib_ind >= 0 && a_value >= 0 && a_value < UINT_MAX && a_value_str[0] == '\0')
            {
                bool inRange = true;
                if (fmt_size > 0 && fmt_size < 10)
                    inRange = IsValueDigitInRange(a_value, fmt_size);
                else if (fmt_size == 10)
                    inRange = a_value > INT_MAX ? false : true;
                if (inRange == true)
                {
                    if (myWriteFreeFormat == false)
                        WriteObjectId(a_cell_fmt, a_value);
                    else
                        myWriteContext_p->WriteFile("%u", a_value);
                }
                else
                {
                    char a_lvalue_str[21];
                    int i_char;
                    for (i_char = 0; i_char < fmt_size; ++i_char)
                        a_lvalue_str[i_char] = '*';
                    a_lvalue_str[i_char] = '\0';
                    if (myWriteFreeFormat == true)
                        fmt_size = 0;
                    sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                    myWriteContext_p->WriteFile(temp_cell_fmt, a_lvalue_str);

                    func_ptr fptr = GetFileopFieldOverflowPtr();
                    if (fptr != NULL)
                        (*fptr)(temp_cell_fmt, fmt_size);
                }
            }
            else
            {
                if (myWriteFreeFormat == true)
                    fmt_size = 0;

                sprintf(temp_cell_fmt, "%%%ds", fmt_size);
                myWriteContext_p->WriteFile(temp_cell_fmt, a_value_str);
            }
        }
    }
    break;
    default:
        break;
    }
    
    if (((ind != -1 && !fmt_size && a_cell_vtype != VTYPE_STRING) || myWriteFreeFormat == true) && myIsLastCell == false)
    {
        string str(mydelimiter);
        myWriteContext_p->WriteFile(str.c_str());
    }
}


bool MECDataWriter::Getcell_SCALAR_OR_OBJECT(const PseudoFileFormatCell_t *cell_p,
                                             const IMECPreObject          &pre_object,
                                             const PseudoDescriptor_t     *descr_p,
                                             int                           ind)
{
    //
    const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p;
    ff_cell_t            *a_cell_p = (ff_cell_t *)cell_p;
    //
    // get info about format and object
    const char *a_cell_fmt = NULL;
    int   a_flag_ikw = END_ARGS, a_scalar_ikw = END_ARGS, a_object_ikw = END_ARGS;
    MCDS_get_ff_cell_attributes(a_cell_p,
                                CELL_FORMAT, &a_cell_fmt,
                                CELL_FLAG_IKW, &a_flag_ikw,
                                CELL_SCALAR_IKW, &a_scalar_ikw,
                                CELL_OBJECT_IKW, &a_object_ikw,
                                END_ARGS);
    string       a_flag_skw = a_descr_p->getSKeyword(a_flag_ikw);
    // test whether it is scalar or object
    int is_object = 0;
    value_type_e a_cell_vtype = a_descr_p->getValueType(a_flag_ikw);
    if (a_cell_vtype == VTYPE_UINT)
    {
        if (ind<0) {
            is_object = pre_object.GetUIntValue(a_flag_skw.c_str());
        }
        else {
            int a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_flag_skw.c_str());
            is_object = (int)pre_object.GetUIntValue(a_attrib_ind, ind);
        }
    }
    else
    {
        if (ind<0) {
            is_object = pre_object.GetIntValue(a_flag_skw.c_str());
        }
        else {
            int a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_flag_skw.c_str());
            is_object = pre_object.GetIntValue(a_attrib_ind, ind);
        }
    }

    if (is_object)
        return GetCellDisplayStatus_VALUE(cell_p, pre_object, descr_p, ind, a_object_ikw);
    else
        return GetCellDisplayStatus_VALUE(cell_p, pre_object, descr_p, ind, a_scalar_ikw);

}
//
//

void MECDataWriter::WriteCell_SCALAR_OR_OBJECT(const PseudoFileFormatCell_t *cell_p,
                                               const IMECPreObject       &pre_object,
                                               const PseudoDescriptor_t *descr_p,
                                               int                       ind)
{
  const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p;
  ff_cell_t            *a_cell_p  = (ff_cell_t *)cell_p;
  //
  // get info about format and object
  const char *a_cell_fmt = NULL;
  int   a_flag_ikw=END_ARGS,a_scalar_ikw=END_ARGS,a_object_ikw=END_ARGS;
  MCDS_get_ff_cell_attributes(a_cell_p,
                              CELL_FORMAT,    &a_cell_fmt,
                              CELL_FLAG_IKW,  &a_flag_ikw,
                              CELL_SCALAR_IKW,&a_scalar_ikw,
                              CELL_OBJECT_IKW,&a_object_ikw,
                              END_ARGS);
  string       a_flag_skw     = a_descr_p->getSKeyword(a_flag_ikw);
  // test whether it is scalar or object
  int is_object = 0;
  value_type_e a_cell_vtype = a_descr_p->getValueType(a_flag_ikw);
  if(a_cell_vtype == VTYPE_UINT)
  { 
      if(ind<0) {
          is_object=pre_object.GetUIntValue(a_flag_skw.c_str());
        }
        else {
          int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_UINT,a_flag_skw);
          is_object=(int)pre_object.GetUIntValue(a_attrib_ind,ind);
      }
  }
  else
  {
      if(ind<0) {
          is_object=pre_object.GetIntValue(a_flag_skw.c_str());
        }
        else {
          int a_attrib_ind=pre_object.GetIndex(IMECPreObject::ATY_ARRAY,IMECPreObject::VTY_INT,a_flag_skw);
          is_object=pre_object.GetIntValue(a_attrib_ind,ind);
      }
  }

  if (is_object) {
      // write the ID of the object
      WriteCell_VALUE (cell_p, pre_object, descr_p, ind, a_object_ikw); 
    }
    else { // !is_object
      // write the scalar
      WriteCell_VALUE (cell_p, pre_object, descr_p, ind, a_scalar_ikw); 
  } // !is_object
}


void MECDataWriter::WriteNameValueCell(const PseudoFileFormatCell_t        *cell_p,
	const IMECPreObject                  &pre_object,
	const PseudoDescriptor_t            *descr_p,
	int                                  ind)
{
	const MvDescriptor_t *a_descr_p = (const MvDescriptor_t *)descr_p;
	const ff_cell_t *a_cell_p = (const ff_cell_t *)cell_p;

	ff_name_value_cell_t *a_name_value_cell_p = (ff_name_value_cell_t *)a_cell_p;
	int nb_pairs = a_name_value_cell_p->nb_pairs;
	ff_name_info_t **name_value_array = a_name_value_cell_p->name_value_array;
	for (int i = 0; i < nb_pairs; i++)
	{
		//
		bool is_parameter = false;
		string param_name = "";
		bool is_parameter_negated = false;

		ff_name_info_t *item = name_value_array[i];
		int         a_cell_ikw = item->ikeyword;
		char *attr_card_string = item->name;
		string       a_cell_skw = a_descr_p->getSKeyword(a_cell_ikw);
		value_type_e a_cell_vtype = a_descr_p->getValueType(a_cell_ikw);

		if (ind >= 0)
		{
			attribute_type_e a_atype = a_descr_p->getAttributeType(a_cell_ikw);
			if (a_atype == ATYPE_VALUE)
				ind = -1;
		}

		//
		switch (a_cell_vtype) {
		case VTYPE_INT:
		{
			int a_value = 0;
			is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
			if (is_parameter)
			{
                if(a_name_value_cell_p->separator_char == ' ')
                    myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                else
                    myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
				if (strlen(attr_card_string) != 0)
				{
					myWriteContext_p->WriteFile("%s%c", attr_card_string, a_name_value_cell_p->pair_char);
				}
				param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
				WriteParameterCell("%s", param_name.c_str(), is_parameter_negated);
			}
			else
			{
				int a_attrib_ind = -1;
				if (ind < 0)
				{
					a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_cell_skw);
					if (a_attrib_ind >= 0)
					{
						a_value = pre_object.GetIntValue(a_cell_skw.c_str());
					}
					else
					{
						continue;
					}
				}
				else
				{
					a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_cell_skw);
					if (a_attrib_ind >= 0)
					{
						a_value = pre_object.GetIntValue(a_attrib_ind, ind);
					}
					else
					{
						continue;
					}
				}

                if (a_name_value_cell_p->separator_char == ' ')
                    myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                else
                    myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
				if (strlen(attr_card_string) != 0)
				{
					myWriteContext_p->WriteFile("%s%c", attr_card_string, a_name_value_cell_p->pair_char);
				}

				if (a_attrib_ind >= 0 && a_value < INT_MAX)
				{
					WriteInteger("%d", a_value);
				}
				else
				{
					bool has_default = false;
					int a_value_d = a_descr_p->getIntDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
					if (mywriteDefaultValue == false || a_value >= INT_MAX || has_default == false)
					{
						const char *a_value_str = "";
						myWriteContext_p->WriteFile("%s", a_value_str);
					}
					else
					{
						WriteInteger("%d", a_value_d);
					}
				}
			}
		}
		break;
		case VTYPE_UINT:
		{
			unsigned int a_value = 0;
			is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
			if (is_parameter)
			{
                if (a_name_value_cell_p->separator_char == ' ')
                    myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                else
                    myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
				if (strlen(attr_card_string) != 0)
				{
					myWriteContext_p->WriteFile("%s%c", attr_card_string, a_name_value_cell_p->pair_char);
				}
				param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
				WriteParameterCell("%s", param_name.c_str(), is_parameter_negated);
			}
			else
			{
				int a_attrib_ind = -1;
				if (ind < 0)
				{
					a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_UINT, a_cell_skw);
					if (a_attrib_ind >= 0)
					{
						a_value = pre_object.GetUIntValue(a_cell_skw.c_str());
					}
					else
					{
						continue;
					}
				}
				else
				{
					a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_cell_skw);
					if (a_attrib_ind >= 0)
					{
						a_value = pre_object.GetUIntValue(a_attrib_ind, ind);
					}
					else
					{
						continue;
					}
				}

                if (a_name_value_cell_p->separator_char == ' ')
                    myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                else
                    myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
				if (strlen(attr_card_string) != 0)
				{
					myWriteContext_p->WriteFile("%s%c", attr_card_string, a_name_value_cell_p->pair_char);
				}
				if (a_attrib_ind >= 0 && a_value < UINT_MAX)
				{
					WriteUInteger("%u", a_value);
				}
				else
				{
					bool has_default = false;
					unsigned int a_value_d = a_descr_p->getUIntDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
					if (mywriteDefaultValue == false || a_value >= UINT_MAX || has_default == false)
					{
						const char *a_value_str = "";
						myWriteContext_p->WriteFile("%s", a_value_str);
					}
					else
					{
						WriteUInteger("%u", a_value_d);
					}
				}
			}
		}
		break;
		case VTYPE_BOOL:
		{
			int a_value = 0;
			is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
			if (is_parameter)
			{
                if (a_name_value_cell_p->separator_char == ' ')
                    myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                else
                    myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
				if (strlen(attr_card_string) != 0)
				{
					myWriteContext_p->WriteFile("%s%c", attr_card_string, a_name_value_cell_p->pair_char);
				}
				param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
				WriteParameterCell("%s", param_name.c_str(), is_parameter_negated);
			}
			else
			{
				int a_attrib_ind = -1;
				if (ind < 0)
				{
					a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_BOOL, a_cell_skw);
					if (a_attrib_ind >= 0)
					{
						a_value = pre_object.GetBoolValue(a_cell_skw.c_str());
					}
					else
					{
						continue;
					}
				}
				else
				{
					a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_cell_skw);
					if (a_attrib_ind >= 0)
					{
						a_value = pre_object.GetBoolValue(a_attrib_ind, ind);
					}
					else
					{
						continue;
					}
				}

				if (a_value == 0)
				{
					continue;
				}
                if (a_name_value_cell_p->separator_char == ' ')
                    myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                else
                    myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
				if (strlen(attr_card_string) != 0)
				{
					myWriteContext_p->WriteFile("%s", attr_card_string);
				}
				if (a_attrib_ind < 0)
				{
					bool has_default = false;
					int a_value_d = a_descr_p->getIntDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
					if (mywriteDefaultValue == false || a_value >= INT_MAX || has_default == false)
					{
						const char *a_value_str = "";
						myWriteContext_p->WriteFile("%s", a_value_str);
					}
				}
			}
		}
		break;
		case VTYPE_FLOAT:
		{
			double a_value = 0.;
			is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
			if (is_parameter)
			{
                if (a_name_value_cell_p->separator_char == ' ')
                    myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                else
                    myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
				if (strlen(attr_card_string) != 0)
				{
					myWriteContext_p->WriteFile("%s%c", attr_card_string, a_name_value_cell_p->pair_char);
				}
				param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
				WriteParameterCell("%s", param_name.c_str(), is_parameter_negated);
			}
			else
			{
				int a_attrib_ind = -1;
				if (ind < 0)
				{
					a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_FLOAT, a_cell_skw);
					if (a_attrib_ind >= 0)
					{
						a_value = pre_object.GetFloatValue(a_cell_skw.c_str());
					}
					else
					{
						continue;
					}
				}
				else
				{
					a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_cell_skw);
					if (a_attrib_ind >= 0)
					{
						a_value = pre_object.GetFloatValue(a_attrib_ind, ind);
					}
					else
					{
						continue;
					}
				}

                if (a_name_value_cell_p->separator_char == ' ')
                    myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                else
                    myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
				if (strlen(attr_card_string) != 0)
				{
					myWriteContext_p->WriteFile("%s%c", attr_card_string, a_name_value_cell_p->pair_char);
				}
				if (a_attrib_ind >= 0 && a_value < DBL_MAX)
				{
                    WriteDouble("%lf", 0, a_value);
				}
				else
				{
					bool has_default = false;
					double a_value_d = a_descr_p->getFloatDefaultValue(a_cell_ikw, DOM_COMMON, &has_default);
					if (mywriteDefaultValue == false || a_value >= DBL_MAX || has_default == false)
					{
						const char *a_value_str = "";
						myWriteContext_p->WriteFile("%s", a_value_str);
					}
					else
					{
						WriteDouble("%lf", 0, a_value_d);
					}
				}
			}
		}
		break;
		case VTYPE_STRING:
		{
			const char *a_value = NULL;
			is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
			if (is_parameter)
			{
                if (a_name_value_cell_p->separator_char == ' ')
                    myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                else
                    myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
				if (strlen(attr_card_string) != 0)
				{
					myWriteContext_p->WriteFile("%s%c", attr_card_string, a_name_value_cell_p->pair_char);
				}
				param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
				WriteParameterCell("%s", param_name.c_str(), is_parameter_negated);
			}
			else
			{
				if (ind < 0)
				{
					if (a_cell_skw.compare("TITLE") == 0)
					{
						a_value = pre_object.GetTitle();
					}
					else
					{
						int a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, a_cell_skw);
						if (a_attrib_ind >= 0)
						{
							a_value = pre_object.GetStringValue(a_cell_skw.c_str());
						}
						else
						{
							continue;
						}
					}
				}
				else
				{
					int a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_cell_skw);
					if (a_attrib_ind >= 0)
					{
						a_value = pre_object.GetStringValue(a_attrib_ind, ind);
					}
					else
					{
						continue;
					}
				}
				if (a_value == NULL) a_value = "";

				if (*a_value != '\0')
				{
                    if (a_name_value_cell_p->separator_char == ' ')
                        myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                    else
                        myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
					if (strlen(attr_card_string) != 0)
					{
						myWriteContext_p->WriteFile("%s%c", attr_card_string, a_name_value_cell_p->pair_char);
					}

					myWriteContext_p->WriteFile("%s", a_value);
				}
			}
		}
		break;
		case VTYPE_OBJECT:
		{
			int a_value = 0;
			is_parameter = pre_object.IsParameter(a_cell_skw.c_str(), ind);
			if (is_parameter)
			{
                if (a_name_value_cell_p->separator_char == ' ')
                    myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                else
                    myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
				if (strlen(attr_card_string) != 0)
				{
					myWriteContext_p->WriteFile("%s%c", attr_card_string, a_name_value_cell_p->pair_char);
				}
				param_name = pre_object.GetParameterName(a_cell_skw.c_str(), ind, &is_parameter_negated);
				WriteParameterCell("%s", param_name.c_str(), is_parameter_negated);
			}
			else
			{
				int a_attrib_ind = -1;
				const char *a_value_str = "";

				if (ind < 0) {
					a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, a_cell_skw);
					if (a_attrib_ind >= 0)
					{
						a_value_str = pre_object.GetObjectName(a_attrib_ind);
						if (a_value_str[0] == '\0')
							a_value = pre_object.GetObjectId(a_attrib_ind);
					}

				}
				else {
					a_attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_cell_skw);

					if (a_attrib_ind >= 0)
					{
						a_value_str = pre_object.GetObjectName(a_attrib_ind, ind);
						if (a_value_str[0] == '\0')
							a_value = pre_object.GetObjectId(a_attrib_ind, ind);
					}
				}


				if (a_value == 0 && a_value_str[0] == '\0')
				{
					continue;
				}
                if (a_name_value_cell_p->separator_char == ' ')
                    myWriteContext_p->WriteFile("%c", a_name_value_cell_p->separator_char);
                else
                    myWriteContext_p->WriteFile("%c ", a_name_value_cell_p->separator_char);
				if (strlen(attr_card_string) != 0)
				{
					myWriteContext_p->WriteFile("%s%c", attr_card_string, a_name_value_cell_p->pair_char);
				}

				if (a_attrib_ind >= 0 && a_value >= 0 && a_value < INT_MAX && a_value_str[0] == '\0')
				{
					WriteInteger("%d", a_value);
				}
				else
				{
					myWriteContext_p->WriteFile("%s", a_value_str);
				}
			}
		}
		break;
		default:
			break;
		}
		
		if (((ind != -1) || myWriteFreeFormat == true) && myIsLastCell == false)
		{
			string str(mydelimiter);
			myWriteContext_p->WriteFile(str.c_str());
		}
	}
}

void MECDataWriter::WriteInteger(const char *cell_fmt, int value)
{
    int length_format =(int) strlen (cell_fmt);
    if (1 > length_format) return;

    if ('F' == cell_fmt[1] || 'd' != cell_fmt[length_format - 1]) {
        int fmt_size = 0;

        GetFormatSize(cell_fmt, fmt_size);

        char a_format[10]="";
        if (0 < fmt_size) {
            sprintf (a_format, "%%%dd", fmt_size);
            myWriteContext_p->WriteFile(a_format,value);
        }
        else {
            myWriteContext_p->WriteFile("%d",value);
        }
        
    }
    else {
        myWriteContext_p->WriteFile(cell_fmt,value);
    }
}

void MECDataWriter::WriteUInteger(const char *cell_fmt, unsigned int value)
{
    int length_format =(int) strlen (cell_fmt);
    if (1 > length_format) return;
    if ('F' == cell_fmt[1] || 'u' != cell_fmt[length_format-1]) {
        // the format is not an integer format, so we build an integer format (CS#714#06_03_06 beg)
        // myWriteContext_p->WriteFile(a_cell_fmt,(double)a_value);
        int fmt_size = 0;
        GetFormatSize(cell_fmt, fmt_size);

        char a_format[10]="";
        if (0 < fmt_size) { 
            sprintf (a_format, "%%%uu", fmt_size);
            myWriteContext_p->WriteFile(a_format,value);
            
        }
        else {
            myWriteContext_p->WriteFile("%u",value);
        }
        
    }
    else {
        myWriteContext_p->WriteFile(cell_fmt,value);
    }

}


void MECDataWriter::WriteDouble(const char *cell_fmt, unsigned int fmt_size, double value)
{
    
    //unsigned int fmt_size = 0;
    // cell_fmt = GetFormatSize(cell_fmt, fmt_size);

    bool a_align_left =false;
    if (0 == fmt_size) {
        myWriteContext_p->WriteFile(cell_fmt,value);
    }
    else {
        bool write_exp = false;
        if(fmt_size && cell_fmt[1]=='-')
            a_align_left = true;
        if (fmt_size)
        {
            int len = (int)strlen(cell_fmt);
            if (len > 0 && cell_fmt[len - 1] == 'e')
                write_exp = true;
        }
        WriteDouble (fmt_size, value, a_align_left, write_exp);
    }
    
}

void MECDataWriter::WriteDouble(int nb_chars, double value, bool left_align, bool write_exp)
{
    static char *value_str = NULL;
    static int value_str_length = -1;

    // if necessary, (re-) allocate the string to print in
    if (nb_chars > value_str_length) {
        value_str_length = nb_chars;
        value_str = (char*) myrealloc (value_str, (value_str_length + 1) * sizeof(char));
    }
#ifdef  HCIOI_USE_HC_STYLE_DOUBLE_FORMAT
    my_print_double (value, nb_chars, value_str, 0);
    if(false ==left_align)
        myWriteContext_p->WriteFile (value_str);
    else
    {
        char temp_cell_fmt[10];
        const char *a_value_str = loc_remove_leadingandtrailingwhitespaces(value_str);
        sprintf(temp_cell_fmt, "%%-%ds",nb_chars);
        myWriteContext_p->WriteFile(temp_cell_fmt,a_value_str);
    }
#else
    if(write_exp == true)
        strexponential(value_str, "%*.*E", nb_chars, -1, value);
    else
        GetDoubleValueString(value_str, nb_chars, value, (int)left_align, myremoveE, myzeroTol, mycompressDouble, myroundDouble);
    myWriteContext_p->WriteFile (value_str);
#endif
}


void MECDataWriter::WriteObjectId(const char *cell_fmt, long long int value) // Revisit if MYOBJ_INT type is changed and see if this function needs any changes or not.
{
    int length_format = (int)strlen(cell_fmt);
    if (1 > length_format) return;
    if ('d' != cell_fmt[length_format - 1]) {
        // the format is not an integer format, so we build an integer format (CS#714#06_03_06 beg)
        // myWriteContext_p->WriteFile(a_cell_fmt,(double)a_value);
        int fmt_size = 0;
        cell_fmt = GetFormatSize(cell_fmt, fmt_size);
        char a_format[10] = "";
        if (0 < fmt_size) { 
            sprintf(a_format, "%%%dd", fmt_size);
            myWriteContext_p->WriteFile(a_format, value);
            
        }
        else {
            myWriteContext_p->WriteFile("%d", value);
        }
        
    }
    else {
        myWriteContext_p->WriteFile(cell_fmt, value);
    }
}

void MECDataWriter::WriteNewline()
{
    myWriteContext_p->WriteFile(myNewlineString.c_str());
}


void MECDataWriter:: WriteParameterCell(const char *cell_fmt, const char *param_name, bool is_negated)
{
    /*
        This base implementation is used for Radioss at present.
        For LS-Dyna the implementation of derived "MECDynaDataWriter:: WriteParameterCell" is being used.
    */
    if(param_name && strlen(param_name)!=0)
    {

        //As per the radioss solver, the parameter reference string always has to be left justified. So modifying the format string
        string fmt_local = cell_fmt;               
        if(fmt_local[0] == '%')
        {
            fmt_local.insert(1,"-");
        }
        string strParamName = param_name;

        if(is_negated)
        {
            strParamName.insert(0,"-&");
        }
        else
        {
            strParamName.insert(0,"&");
        }
        myWriteContext_p->WriteFile(fmt_local.c_str(),strParamName.c_str());
    }
}


// Gets the ikeyword of the cell (or a "representative" one, if multiple).
int MECDataWriter::GetCellIkeyword(const PseudoFileFormatCell_t *cell_p) const
{
    ff_cell_t *a_cell_p  = (ff_cell_t *)cell_p;
    ff_cell_type_e a_cell_type = CELL_UNKNOWN;
    MCDS_get_ff_cell_attributes(a_cell_p,CELL_TYPE,&a_cell_type,END_ARGS);
    int a_cell_ikw = END_ARGS;

    switch(a_cell_type) {
        case CELL_VALUE:
        case CELL_DIR_RADIO:
        case CELL_PAIR:
            MCDS_get_ff_cell_attributes(a_cell_p,CELL_IKEYWORD,&a_cell_ikw,END_ARGS);
            break;
        case CELL_DIR_FLAGS:
            MCDS_get_ff_cell_attributes(a_cell_p,CELL_DIRX_IKW,&a_cell_ikw,END_ARGS);
            break;
        case CELL_FLAGGED_OBJECT:
        case CELL_SCALAR_OR_OBJECT:
        case CELL_SCALAR_OR_STRING:
            MCDS_get_ff_cell_attributes(a_cell_p,CELL_FLAG_IKW,&a_cell_ikw,END_ARGS);
            break;
        default:
            ;// nothing to do.
    }

    return a_cell_ikw;
}

const char *MECDataWriter::GetFormatSize(const char *fmt_p, int &fmt_size) {
    if (fmt_p)
    {
        const string &fmt = fmt_p;
  int a_size=atoi(fmt.substr(1,fmt.find_first_of(".sdilfeg")-1).c_str());
        fmt_size = a_size < 0 ? (-a_size) : a_size;
}
    return fmt_p;
}
unsigned int MECDataWriter::GetCellSize(const ff_cell_t *cell_format_p)
{
  int a_nb_chars=0;
  //
  ff_cell_type_e  a_cell_type = CELL_UNKNOWN;
  MCDS_get_ff_cell_attributes(cell_format_p,CELL_TYPE,&a_cell_type,END_ARGS);
  //
  switch(a_cell_type) {
  case CELL_COMMENT: 
    {
      MCDS_get_ff_cell_attributes(cell_format_p,CELL_SIZE,&a_nb_chars,END_ARGS);
    }
    break;
  case CELL_VALUE:
  case CELL_DIR_RADIO:
  case CELL_DIR_FLAGS:
  case CELL_ID:
  case CELL_APPEND_OPTIONS:
    {
      const char *a_fmt=NULL;
      MCDS_get_ff_cell_attributes(cell_format_p,CELL_FORMAT,&a_fmt,END_ARGS);
      a_nb_chars = 0;
      a_fmt = GetFormatSize(a_fmt, a_nb_chars);
    }
    break;
  case CELL_PAIR:
    {
      const char *a_fmt=NULL;
      MCDS_get_ff_cell_attributes(cell_format_p,CELL_FORMAT,&a_fmt,END_ARGS);
	  a_fmt= GetFormatSize(a_fmt, a_nb_chars);
      a_nb_chars+=a_nb_chars;
    }
    break;
  default:
    break;
  }
  //
  return a_nb_chars;
}

int MECDataWriter::GetLineLength(int a_line_length, int myLineLength)
{
    if (a_line_length <= 0 || a_line_length > myLineLength) a_line_length = myLineLength;
    return a_line_length;
}

/* --------- Static functions --------- */
static const char *loc_remove_leadingandtrailingwhitespaces(char *str)
{
    //remove leading spaces
    while(*str==' ') ++str;

    //remove trailing spaces
    char *a_char_p=str+strlen(str)-1;
    while(a_char_p>=str && *a_char_p==' ') --a_char_p;
    *(++a_char_p)='\0';
    return str;
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
//Modification History:


//--///////////////////////////////////////////////////////////////////////////////
int MECDataWriter::getMultidimensionalArraySize(const PseudoFileFormatCard_t *card_format_p,
                                                const   IMECPreObject                 *object_p,

                                                   const PseudoDescriptor_t     *descr_p)
{
   int a_cell_ikw =isMultiArray(card_format_p,descr_p) ;

   return getMultidimensionalArraySize(a_cell_ikw, object_p, descr_p);
}

int MECDataWriter::getMultidimensionalArraySize(int                       cell_ikw,
                                                const IMECPreObject       *object_p,
                                                const PseudoDescriptor_t *descr_p)
{
   const MvDescriptor_t *a_descr_p       = (const MvDescriptor_t *)descr_p;
   int a_index = 0;
   string a_index_skw = "";
   if(cell_ikw ==END_ARGS)
   {
      return 0;
   }
   else
   {
      MvSizeVector  sizeArrayVector;
      int size = 1;
      a_descr_p->getDimensionSize(cell_ikw,sizeArrayVector);
      
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

int MECDataWriter::isMultiArray(const PseudoFileFormatCard_t *card_format_p,
                                   const PseudoDescriptor_t     *descr_p)
{
  const ff_card_t      *a_card_format_p = (const ff_card_t *)card_format_p;
  const MvDescriptor_t *a_descr_p       = (const MvDescriptor_t *)descr_p;
  ff_cell_t *a_cell_format_p=NULL;
  int a_cell_ikw =END_ARGS;
  MCDS_get_ff_card_tab(a_card_format_p,CARD_CELL,0,(void *)(&a_cell_format_p));
  MCDS_get_ff_cell_attributes(a_cell_format_p,CELL_IKEYWORD,&a_cell_ikw,END_ARGS);
  if(!a_descr_p->isMultiDimensionalArray(a_cell_ikw))
  {
      return END_ARGS;
  }
  else
  {
      return a_cell_ikw;
  } 
}

bool MECDataWriter::GetFilteredIkeywordlistFromPreScan(const MECIModelScanner  *a_model_scanner, const IDescriptor &descrp,
                                                       const fileformat_t *format_p, IMECPreObject &preobject, 
                                                       map< int, vector<MvIKeywordList_t> > &atype_vtypeikeywordlst)
{
    const MvIKeywordSet_t &a_ikeywords_exportprescan=descrp.getDefinition(DOM_COMMON,"_EXPORT_PRESCAN");
    bool has_export_prescan = false;
    if(!a_ikeywords_exportprescan.size() || !format_p)
        return false;
    MvModelScanner_t *mv_model = (MvModelScanner_t *)a_model_scanner;
    hwHCSolverInf    *mysolverinf = mv_model->getHmSolverInf();
    MvIKeywordSet_t  keywordlst;
    MvIKeywordSet_t::iterator a_it_tp_begin,a_it_tp_end,a_tp_it;
    a_it_tp_begin = a_ikeywords_exportprescan.begin();
    a_it_tp_end   = a_ikeywords_exportprescan.end();
    for(a_tp_it=a_it_tp_begin;a_tp_it!=a_it_tp_end;++a_tp_it) 
    {
        int    a_ikeyword = (*a_tp_it);
        string a_skeyword = descrp.getSKeyword(a_ikeyword);
        bool r_type = mysolverinf->UpdatePreObjectValue(preobject, &descrp, a_skeyword);
        if (!r_type)
            return false;
    }
    GetIKeywordLst(format_p, preobject, (const void *)(&descrp), a_model_scanner, (MvIKeywordSet_t &)a_ikeywords_exportprescan, keywordlst);
    if(!keywordlst.size())
       return false;
    MvIKeywordSet_t  idenlocallst;
    MvIntIdentifierMap_t  iden_lst;
    descrp.getIdentifierValue(DOM_COMMON, iden_lst);
    if(iden_lst.size())
    {
        MvIntIdentifierMap_t::iterator a_it_begin,a_it_end,a_it;
        a_it_begin = iden_lst.begin();
        a_it_end   = iden_lst.end();
        for(a_it=a_it_begin;a_it!=a_it_end;++a_it) 
        {
            int  value = a_it->second;
            if(value < 0)
                idenlocallst.insert(a_it->first);
        }
    }
    keywordlst -= idenlocallst;
    if(keywordlst.size())
    {
        vector<MvIKeywordList_t>  a_vec_single_lst(VTYPE_LAST);
        vector<MvIKeywordList_t>  a_vec_array_lst(VTYPE_LAST);
        MvIKeywordSet_t::iterator a_it_keylst_begin,a_it_keylst_end,a_keylst_it;
        a_it_keylst_begin = keywordlst.begin();
        a_it_keylst_end   = keywordlst.end();
        MvIntIdentifierMap_t::iterator it_iden;
        for(a_keylst_it=a_it_keylst_begin;a_keylst_it!=a_it_keylst_end;++a_keylst_it) 
        {
            int    a_ikeyword = (*a_keylst_it);
            int    iden_val = 0;
            //MvIKeywordSet_t::iterator a_it_find;
            //const bool is_in = a_ikeywords_exportprescan.find(a_ikeyword) != a_ikeywords_exportprescan.end();
            //if(is_in)
            //    continue;
            attribute_type_e a_atype = descrp.getAttributeType(a_ikeyword);
            value_type_e a_valuetype = descrp.getValueType(a_ikeyword);
            if(a_atype == ATYPE_VALUE || a_atype == ATYPE_SIZE)
                a_vec_single_lst[a_valuetype].push_back(a_ikeyword);
            else if(a_atype == ATYPE_STATIC_ARRAY || a_atype == ATYPE_DYNAMIC_ARRAY)
                a_vec_array_lst[a_valuetype].push_back(a_ikeyword);
        }

        atype_vtypeikeywordlst[0] = a_vec_single_lst;
        atype_vtypeikeywordlst[1] = a_vec_array_lst;
    }
    return true;
}

void strfixexponent(char *str)
{
    /* Change 3 digit exponent to 2 digit -> e+010 to e+10 */
    size_t len = strlen(str);
    assert(len >= 3);
    assert(str[len - 3] == '0');
    str[len - 3] = str[len - 2];
    str[len - 2] = str[len - 1];
    str[len - 1] = '\0';
}

void strexponential(char *buf, const char *formatstr, int width, int precision, double x)
{
    if (width < 0) width = 6;
    if (precision < 0)
    {
        precision = width - 6;
        
        if (x < 0.0 || x == -0.0) --precision;
    }
    if (fabs(x) >= 1e100 || (fabs(x) < 1e-99 && fabs(x) > 0.0))  --precision;
    if (precision < 0) precision = 0;

    /* format string must contain "%*.*e" (or something similar) */


    /* Only for 3 digit exponent (values ge 1e100 and lt 1e-99) eg -> -1.29e+100 to -1.e+100 */
    if (width <= 8 && (fabs(x) >= 1e100 || (fabs(x) < 1e-99 && fabs(x) > 0.0)))
    {
        if (!JRW_stricmp(formatstr, "%*.*E"))
            sprintf(buf, "%#*.*E", width, precision, x);
        else
            sprintf(buf, formatstr, width, precision, x);

        //DBG(buf)
        //DBG_OUT
        return;
    }
    sprintf(buf, formatstr, width, precision, x);
    //DBG(buf)
    //DBG_OUT
}

void GetDoubleValueString(char *val_str, int width, double realvalue, int left, int remove_e, double zero_tol, int compress_double, int roundoff_double)
{
    char pre[82];
    char post[82];
    char string[82];
    int digits_leftof_decimal = 0;  // new variables to make feoutput write out stuff just like what feinput gets...
    int int_castofrealvalue = 0;
    int sign_of_real = 0;

    int code = 0;
    int string_edit = 0;
    int index_of_first_number = 0;

    pre[0] = '\0';
    post[0] = '\0';
	string[0] = '\0';

    if (width && compress_double == 3)
    {
        strmaxprecision(string, width, realvalue, left, remove_e, zero_tol);
        strncpy(val_str, string, width);
		val_str[width] = '\0';
        return;
    }

    if (width > 80)
    {
        width = 80;
    }
    if (compress_double == 2)
    {
        width++;
    }

    if (-1.0e-100 <= realvalue && realvalue <= 1.0e-100)
        realvalue = 0.0;
    if (width > 0)
    {
        if (realvalue < 0.0)
        {
            if (width > 1 && -pow(10.0, (double)(width - 2)) + .0001 < realvalue && realvalue < -1.0e-03 - .0001)
            {
                code = 1;
                sign_of_real = 0;
            }
            else if (-1.0e+100 + .0001 < realvalue && width > 6)
            {
                code = 3;
            }
        }
        else
        {
            if (realvalue == 0.0)
            {
                code = 1;
                sign_of_real = 1;
            }
            else if ((width > 1 && 1.0e-03 + .0001 < realvalue && realvalue < pow(10.0, (double)(width - 1)) - .0001))
            {
                code = 1;
                sign_of_real = 1;
            }
            else if (realvalue < 1.0e+100 - .0001 && width > 5)
            {
                code = 2;
            }
        }
    }
    else
        code = 4;

    switch (code)
    {
    case 1:
        if (sign_of_real) {
            int_castofrealvalue = (int)realvalue;
            digits_leftof_decimal = 0;
        }
        else {
            int_castofrealvalue = -(int)realvalue;
            digits_leftof_decimal = 0;
        }
        while (int_castofrealvalue>0)
        {
            int_castofrealvalue = int_castofrealvalue / 10;
            digits_leftof_decimal++;
        }

        if (roundoff_double)
        {
            if (left)
            {
                sprintf(string, "%-*.*f", width, min(roundoff_double, 15 - digits_leftof_decimal), realvalue);
            }
            else
            {
                sprintf(string, "%*.*f", width, min(roundoff_double, 15 - digits_leftof_decimal), realvalue);
            }
        }
        else
        {
            sprintf(string, "%*.*f", width, 15 - digits_leftof_decimal, realvalue);
        }
        // this portion of code is to round the floating point number based on the last digit, DEFECT000010408

        // If it is a negative real number, the first char in the string will be '-' and
        // we don't want to change it.
        if (sign_of_real)
            index_of_first_number = 0;
        else
            index_of_first_number = 1;

        string_edit = width - 1;

        if (string[width]>'5' || (string[width] == '5'&&string[width - 1] == '1')
            || (string[width] == '5'&&string[width - 1] == '3')
            || (string[width] == '5'&&string[width - 1] == '5')
            || (string[width] == '5'&&string[width - 1] == '7')
            || (string[width] == '5'&&string[width - 1] == '9'))
        {

            while (string_edit >= index_of_first_number)
            {
                if (string[string_edit] == '.')
                {
                    string_edit--;
                }
                else
                {
                    if (string[string_edit] == '9')
                    {
                        if (string_edit == index_of_first_number)
                        {
                            string[string_edit] = '0';
                            memmove(string + index_of_first_number + 1, string + index_of_first_number, width);
                            string[index_of_first_number] = '1';
                            string_edit = -1;
                        }
                        else
                        {
                            string[string_edit] = '0';
                            string_edit--;
                        }
                    }
                    else
                    {
                        string[string_edit]++;
                        string_edit = -1;
                    }
                }
            }
        }
        string[width] = 0;


        //DBG_ASSERT(strchr(string, 'e') == NULL)
            //DBG_ASSERT(strchr(string, 'E') == NULL)

            if (compress_double)            
            {
                char *s;

                for (s = string; *s; ++s)
                {
                    if (*s == '.')
                    {
                        break;
                    }
                }

                if (*s)
                {
                    ++s;                          // Always keep one digit to the right of '.'
                    char *end = string + width - 1;
                    for (; end > s; --end)
                    {
                        if (*end == '0')
                        {
                            // *end = ' ';
                            *end = '\0';
                            if (left)
                            {
                                strcat(post, " ");
                            }
                            else
                            {
                                strcat(pre, " ");
                            }
                        }
                        else
                        {
                            break;
                        }
                    }
                }
            }
        break;
    case 2:
    case 3:
        strexponential(string, "%*.*E", width, -1, realvalue);
        break;
    case 4:
        sprintf(string, "%g", realvalue);
        if (strchr(string, 'e') == NULL && strchr(string, '.') == NULL &&
            strchr(string, 'E') == NULL)
            strcat(string, ".0");
        break;
    }

    if (compress_double == 2)
    {
        width--;
        if (strlen(string) && string[0] == '0')
        {
            char tmpstring[82];
            tmpstring[0] = 0;

            sprintf(tmpstring, "%s", &(string[1]));
            sprintf(string, "%s", tmpstring);
        }
        else
        {
            int len;
            char *chrptr;
            if (((chrptr = strchr(string, 'e')) != NULL) ||
                ((chrptr = strchr(string, 'E')) != NULL))
            {
                char tmpstring[82];
                tmpstring[0] = 0;

                // chrptr points to either 'e' or 'E'
                --chrptr;
                // chrptr points to numeral before 'e' or 'E'
                *chrptr = '\0';
                sprintf(tmpstring, "%s", string);
                ++chrptr;
                strcat(tmpstring, chrptr);
                sprintf(string, "%s", tmpstring);
            }
            else
            {
                string[width] = '\0';
            }
            // Need to remove 1 space from either pre or post.
            if (left)
            {
                if (len = (int)strlen(post))
                    post[len - 1] = '\0';
            }
            else
            {
                if (len = (int)strlen(pre))
                    pre[len - 1] = '\0';
            }
        }
    }

    /*if (!code || ((int)strlen(string) > width && code != 4))
    {
        FileopFieldOverflow(string, width);
    }*/
    if (*pre || *post)
    {
        char tmpstring[82];
        tmpstring[0] = 0;

        //sprintf(string, "s%s", pre, string, post);
        sprintf(tmpstring, "%s%s%s", pre, string, post);
        sprintf(string, "%s", tmpstring);
    }
    strncpy(val_str, string, width);
	val_str[width] = '\0';
}

bool strmaxprecision(char *buf, int width, double x, int left, int remove_e, double zero_tol)
{
   /* Function to output a real value in the maximum precision possible within a given field width.
      There are several different ways in which the precision of a real value can be increased, relative
      to the 'standard' output:
      1. Removal of unnecessary leading zero from a floating point number (example 0.152374 can be output as 0.1523 
         or .15237 in a field of width 6)
      2. Removal of leading zeros from the exponent (example 1.02356e+015 can be output as either 1.0e+015 or 1.02e+15,
         in a field of 8, making room for an extra decimal)
      3. Similar to (2) above, we can make room for an extra decimal by removing 'e' or 'E'. Note that not all solvers 
         may read this format, so this function uses the argument 'remove_e' to make the decision.

      The "%g" format in sprintf does a pretty decent job of rounding off, but in order to maximize precision, we 
      need to pass the exact count of the significant digits necessary for the specific case. The key to obtaining 
      higher precision is to determine up front *exactly* how many 'extra' digits are going to be available for a 
      given 'x', based on a combination of any of the conditions listed above. We will then need to go back and apply 
      the conditions that resulted in the improved precision.
      
      In addition to maximizing precision, this function also removes unnecessary trailing zeros, keeping only one,
      for the sake of clarity. During output in floating point (non-exponential format), the zero is omitted if there 
      is not enough room. For instance, 123.0 would be output as 123. in a field of 4.

      Though we are trying to squeeze as much precision in as little width, we still want to output at least a decimal 
      point (and .0 where applicable / necessary) in order to make sure the output is not mistakenly treated as an 
      integer under any circumstances. However, 'keep_decimal', which is currently a const, can be made as a variable 
      and added to the argument list if we ever need to allow further compression by removing the decimal point as and 
      when it is necessary to do so.
   */
   int max_width = 80;
   int max_precision = 15;  
                            // has a good representation of the original value.
   char *has_e = NULL;
   char *has_decimal = NULL;
   char precise_string[85];
   char work_string[85];
   char out_string[85];
   int i = 0, j = 0;
   int long_float_flag = 0;
   char *precise_nospace_str = &precise_string[0];
   const int keep_decimal = 1;
   int width1 = 0, width2 = 0, width3 = 0, width4 = 0, width5 = 0, width6 = 0;
   int num_sig_digits = 0;
    
   // note that zero width should be handled by the calling function as a separate case
   
 
   if(width > max_width)
     width = max_width;
     
   if(zero_tol != 0.0)
   {
      double tolerance_val = fabs(zero_tol);
      if(tolerance_val <= MAX_ZERO_TOLERANCE)
      {
         if(fabs(x) <= tolerance_val)
           x = 0.0;
      }
   }
     
   if(x == 0.0 || x == -0.0)
   {
      if(width >= 3)
      {
         strcpy(work_string,"0.0");
         if(strformatstring(buf,work_string,width,left))
           return true;
      }
      else if(width == 2)
      {
         strcpy(buf,"0.");
         return true;
      }
      return false;
   }
   
   if(fabs(x) > (0.999*DBL_MAX)) // we don't want to allow all the way till DBL_MAX, since feinput uses this value in a special way
     return false;
     
   if(x < 0.0)
    width1 = 1;

   width2 = (int)keep_decimal;

   sprintf(precise_string,"%*.*g",max_width,max_precision,x);
   
   // remove leading spaces
   while(precise_string[i])
   {
      if(precise_string[i] == ' ')
        precise_nospace_str++;
      else
        break;
      i++;
   }
   
   has_e = strchr(precise_nospace_str,'e');
   if(!has_e)
     has_e = strchr(precise_nospace_str,'E');
     
   if(has_e == NULL)
   {
      has_decimal = strchr(precise_nospace_str,'.');
      if(!has_decimal)
      {
//       if(keep_decimal)
         if(stradddecimal(buf,precise_nospace_str,width,left))
           return true;
      }
      else
      {
         // remove trailing zeros
         j = (int)(strlen(precise_nospace_str) - 1);
         while(*(precise_nospace_str + j) == '0')
         {
            *(precise_nospace_str + j) = '\0';
            precise_nospace_str--, j--;
            if(*precise_nospace_str == '.')
            {
               precise_nospace_str++;
               *precise_nospace_str = '0';  //keep one zero
               break;
            }
         }
         if(strformatstring(buf,precise_nospace_str,width,left))
           return true;
         num_sig_digits = width - width1 - width2;
         if(num_sig_digits >= 1)
         {
            sprintf(work_string,"%*.*g",width,num_sig_digits,x);
            strsprm(work_string);
            has_e = strchr(work_string,'e');
            if(!has_e)
              has_e = strchr(precise_nospace_str,'E');
            has_decimal = strchr(work_string,'.');
           
            if(!has_e && has_decimal)
            {
               // if it fits in, we don't remove leading zero
               if(strformatstring(buf,work_string,width,left))
                 return true;
               if(work_string[0] == '0' && x != 0.0)  // remove leading zero from number such as 0.15
                 work_string[0] = ' ';
               else if(work_string[0] == '-' && work_string[1] == '0')
               {
                  work_string[1] = work_string[0];
                  work_string[0] = ' ';
               }
               j = 0;
               for(i=0;i<strlen(work_string);i++)
               {
                  if(work_string[i] != ' ')
                  {
                     out_string[j] = work_string[i];
                     j++;
                  }
               }
               out_string[j] = '\0';
               if(strformatstring(buf,out_string,width,left))
                 return true;
            }
            else if(!has_decimal && !has_e)
            {
               // if(keep_decimal)
               if(stradddecimal(buf,work_string,width,left))
                 return true;
            }
         }
      }
      long_float_flag = 1;
   }
   
   if(long_float_flag)
   {
      sprintf(precise_string,"%*.*E",max_width,max_precision,x);
      has_e = strchr(precise_string,'E');
   }
   
   width3 = 1;  // for the number before decimal point in 'e' format
   if(!remove_e)
     width4 = 1; // width required for 'e'
   width5 = 1;  // for sign of exponent
     
   char *tmp = has_e+2;

   while ( *tmp == '0')
   {
	   ++tmp;
   }

   width6 = (int) strlen(tmp);
   
   num_sig_digits = width - width1 - width2 - width3 - width4 - width5 - width6;
   if(num_sig_digits < 0)
     return false;
     
   if(num_sig_digits == 0)
   {
      if(keep_decimal)
        return false;
   }
   
   
   sprintf(work_string,"%*.*E",width,num_sig_digits,x);
   has_e = strchr(work_string,'E');
   if(*(has_e+2) == '0')
   {
      *(has_e+2) = ' ';
      if(*(has_e+3) == '0')
      {
         *(has_e+3) = ' ';
         if(*(has_e+4) == '0')  // should not happen, but just in case
           *(has_e+4) = ' ';
      }
   }
   
   if(num_sig_digits > 1)
   {
      char *zero_trail = has_e;
      --zero_trail;
      while(zero_trail && *zero_trail == '0')
      {
         *zero_trail = ' ';
         --zero_trail;
         if(*zero_trail == '.')
         {
            zero_trail++;
            *zero_trail = '0';  //keep one zero
            break;
         }
      }
   }
   
   if(remove_e)
    *(has_e) = ' ';
    
   j = 0;
   for(i=0;i<strlen(work_string);i++)
   {
      if(work_string[i] != ' ')
      {
         out_string[j] = work_string[i];
         j++;
      }
   }
   out_string[j] = '\0';
   
   if(strformatstring(buf,out_string,width,left))
     return true;

   return false;
}

string MECDataWriter::GetSolverName(const IDescriptor* descrp, int ikey, int total_cell_size, int fmt_size, char* p_prev_comment)
{
    string a_solver_str = descrp->getSolverName(ikey);
    if (!p_prev_comment)
        return a_solver_str;

    bool is_multi_solvername = false;
    if (a_solver_str.find("/") != std::string::npos)
        is_multi_solvername = true;

    value_type_e val_type = descrp->getValueType(ikey);
    if (val_type == VTYPE_OBJECT)
    {
        int nb_subtype = 0;
        if (descrp->hasObjectAttribSubtype(ikey, &nb_subtype))
        {
            if (nb_subtype > 1)
            {
                is_multi_solvername = true;
            }
        }
    }

    if (is_multi_solvername && p_prev_comment)
    {
        a_solver_str = GetCommentCard(p_prev_comment, total_cell_size, fmt_size);
    }
    return a_solver_str;
}

string MECDataWriter::GetCommentCard(const char* comment_card, int total_cell_size, int fmt_size)
{
    string a_solver_str = "";
    // replace a_solver_str with correct one from comment_card
    // parse till total_cell_size-fmt_size in comment_card
    int char_to_skip = total_cell_size - fmt_size;
    string replace_str = "";
    int length = (int)strlen(comment_card);
    if ((char_to_skip + fmt_size) <= length)
    {
        for (int ii = 0; ii < fmt_size; ii++)
            replace_str += comment_card[char_to_skip + ii];
        a_solver_str = replace_str;
    }
    return a_solver_str;
}
string GetStringValueFromPreObject(IMECPreObject* pre_object, MvDataFeatureType_e feature_type, value_type_e val_type, attribute_type_e att_type, string& a_cell_skw, const char* a_cell_fmt, int fmt_size,
    int myremoveE, double myzeroTol)
{
    if(pre_object== NULL)
        return "";
    string a_comment;
    if (val_type == VTYPE_BOOL)
    {
        int val = 0;
        int a_index = -1;
        if (att_type == ATYPE_DYNAMIC_ARRAY || att_type == ATYPE_STATIC_ARRAY)
            a_index = pre_object->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_cell_skw.c_str());
        else
            a_index = pre_object->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_BOOL, a_cell_skw.c_str());

        if (a_index >= 0)
        {
            val = (int)pre_object->GetBoolValue(a_index);
            a_comment = to_string(val);
        }
        else if (feature_type == DFT_FLAG) /*only if it is FLAG*/
        {
            a_comment = to_string(val);
        }
    }
    else if (val_type == VTYPE_INT)
    {
        int val = 0;
        int a_index = -1;
        if (att_type == ATYPE_DYNAMIC_ARRAY || att_type == ATYPE_STATIC_ARRAY)
            a_index = pre_object->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_cell_skw.c_str());
        else
            a_index = pre_object->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_cell_skw.c_str());

        if (a_index >= 0)
        {
            val = pre_object->GetIntValue(a_index);
            a_comment = to_string(val);
        }
        else if (feature_type == DFT_FLAG) /*only if it is FLAG*/
        {
            a_comment = to_string(val);
        }
    }
    else if (val_type == VTYPE_UINT)
    {
        unsigned int val = 0;
        int a_index = -1;
        if (att_type == ATYPE_DYNAMIC_ARRAY || att_type == ATYPE_STATIC_ARRAY)
            a_index = pre_object->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_cell_skw.c_str());
        else
            a_index = pre_object->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_UINT, a_cell_skw.c_str());

        if (a_index >= 0)
        {
            val = pre_object->GetUIntValue(a_index);
            a_comment = to_string(val);
        }
    }
    else if (val_type == VTYPE_FLOAT)
    {
        double val = 0.;
        int a_index = -1;
        if (att_type == ATYPE_DYNAMIC_ARRAY || att_type == ATYPE_STATIC_ARRAY)
            a_index = pre_object->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_cell_skw.c_str());
        else
            a_index = pre_object->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_FLOAT, a_cell_skw.c_str());

        if (a_index >= 0)
        {
            val = pre_object->GetFloatValue(a_index);
            char value_str[100];
            bool found = containsChar(a_cell_fmt, '-');
            int left_align = found == true ? 1 : 0; //always left align
            GetDoubleValueString(value_str, fmt_size, val, true, myremoveE, myzeroTol, 1, 0);
            string l_comment(value_str);
            a_comment = trim(l_comment);
        }
    }
    else if (val_type == VTYPE_STRING)
    {
        double val = 0.;
        int a_index = -1;
        if (att_type == ATYPE_DYNAMIC_ARRAY || att_type == ATYPE_STATIC_ARRAY)
            a_index = pre_object->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_cell_skw.c_str());
        else
            a_index = pre_object->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, a_cell_skw.c_str());

        if (a_index >= 0)
        {
            a_comment = pre_object->GetStringValue(a_index);
        }
    }
    return a_comment;
}
