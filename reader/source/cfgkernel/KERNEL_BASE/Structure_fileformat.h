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
#ifndef STRUCTURE_FILEFORMAT_H
#define STRUCTURE_FILEFORMAT_H

#include "Structure_expression.h" 
#include "Structure_types.h"

/* --------- Cell --------- */

/** Cell types */
enum ff_cell_type_s {
  CELL_UNKNOWN, 
  /** Blank cell */
  CELL_COMMENT, 
  /** Value cell (integer, float, string, object) */
  CELL_VALUE, 
  CELL_ID, 
  /** Direction radio cell (integer <-> string) */
  CELL_DIR_RADIO, 
  /** Direction flags cell (integer <-> string) */
  CELL_DIR_FLAGS, 
  /** Digits cell (digits <-> integer)*/
  CELL_DIGITS,    
  /** Cell which can be a scalar or a function id */
  CELL_SCALAR_OR_OBJECT, 
  /** Cell containing an object id which may be flagged with a minus sign */
  CELL_FLAGGED_OBJECT, 
  /** Cell which can be a scalar or a string */
  CELL_SCALAR_OR_STRING, 

  /** Cell conditional statement at cell level*/
  CELL_COND,
  CELL_PAIR, /* To write an array of pairs(i,i+1) */
  CELL_LIST, /* To write any array continuously or value at a particular index*/
  /*blank attribute*/
  CELL_BLANK,
  CELL_NAME_VALUE,
  /** Cell that has information to append to a string, e.g. keyword*/
  CELL_APPEND_OPTIONS,
  CELL_LAST
};

typedef enum ff_cell_type_s ff_cell_type_e;

typedef struct ff_cell_s {
  ff_cell_type_e      type;
} ff_cell_t;


typedef struct ff_comment_cell_s {
  ff_cell_t           cell_part;
  char               *comment;
} ff_comment_cell_t;



typedef struct ff_formated_cell_s {
  ff_cell_t           cell_part;
  char               *format;
} ff_formated_cell_t;

typedef struct ff_value_cell_s {
  ff_formated_cell_t  formated_cell_part;
  int                 ikeyword; 
} ff_value_cell_t;

typedef struct ff_dir_radio_cell_s {
  ff_value_cell_t     value_cell_part;
  int                 is_extended;
} ff_dir_radio_cell_t;

typedef struct ff_dir_flags_cell_s {
  ff_formated_cell_t  formated_cell_part;
  int                 ikeyword_tab[3]; 
} ff_dir_flags_cell_t;



typedef struct ff_digits_cell_s {
  ff_formated_cell_t  formated_cell_part;
  int                 nb_ikeywords;
  int                *ikeyword_tab;
} ff_digits_cell_t;



typedef struct ff_scalar_or_object_cell_s {
  ff_formated_cell_t  formated_cell_part;
  int                 ikeyword_tab[3]; 
} ff_scalar_or_object_cell_t;



typedef struct ff_flagged_object_cell_s {
  ff_formated_cell_t  formated_cell_part;
  int                 ikeyword_tab[2]; 
} ff_flagged_object_cell_t;



typedef struct ff_scalar_or_string_cell_s {
  ff_formated_cell_t  formated_cell_part;
  int                 ikeyword_tab[3]; 
} ff_scalar_or_string_cell_t;




typedef struct ff_condcell_s {
  expression_t   *expression;
  ff_cell_t      *cell;
} ff_condcell_t;

typedef struct ff_if_cell_s {
  ff_cell_t           cell_part;
  int                 nb_condcell; 
  ff_condcell_t     **condcell_array;
} ff_if_cell_t;

typedef struct ff_cell_list_s {
    ff_value_cell_t     value_cell_part;
    int index;
} ff_cell_list_t;

typedef struct ff_name_info_s
{
    char *name;
    int   ikeyword;
} ff_name_info_t;


typedef struct ff_name_value_cell_s {
    ff_formated_cell_t  formated_cell_part;
    int                 nb_pairs;
    ff_name_info_t    **name_value_array;
    char                pair_char;
    char                separator_char;
} ff_name_value_cell_t;
/* --------- Card --------- */

/** Card types */
enum ff_card_type_s {
  CARD_UNKNOWN,
  /** Blank card */ 
  CARD_BLANK, 
  /** Comment card */ 
  CARD_COMMENT, 
  /** Single card (one line) */ 
  CARD_SINGLE, 
  /** List of single and array cells */ 
  CARD_LIST,        
  /** List of cells (pattern) */ 
  CARD_CELL_LIST, 
  /** List of cards*/ 
  CARD_CARD_LIST,
  /** "If" card (conditional cards) */
  CARD_IF,          
  /** List of objects */
  CARD_OBJECT_LIST, 
  /** List of subobjects, which is in fact not a card but a list of independent objects. */
  CARD_SUBOBJECTS, 
  
  CARD_HEADER, 
  CARD_FREE_FORMAT, 
  CARD_PREREAD,  
  CARD_ASSIGN,
  CARD_LAST
};

/*this enum is used for the card flag and always assign base of 2 values : Ishank*/
typedef enum ff_card_flags_s
{
    CARD_FLAG_NO_OPTIONS = 1,
    CARD_FLAG_NO_COMMENT = 2,
    CARD_FLAG_NO_FREE_FORMAT = 4,
    CARD_FLAG_NO_END = 8,
    CARD_FLAG_BLOCK_TOGETHER = 16,
    CARD_FLAG_OFFSET = 32

} ff_card_flags_e;

typedef enum ff_card_type_s ff_card_type_e;

typedef struct ff_card_s {
  ff_card_type_e   type;
  int              flag;
} ff_card_t;

typedef struct ff_blank_card_s {
  ff_card_t        card_part;
} ff_blank_card_t;

typedef struct ff_single_card_s {
  ff_card_t        card_part;
  int              nb_cells;
  ff_cell_t      **cell_array;
  int              is_free;    
} ff_single_card_t;


typedef struct ff_list_card_s {
  ff_single_card_t single_card_part;
  int              length_max;
  int              ikeyword;
} ff_list_card_t;


typedef struct ff_cell_list_card_s {
  ff_single_card_t single_card_part;
  int              size;
  int        length_max;  
  char       *offset_fmt;
  char       *offset_value;
  
            
  
} ff_cell_list_card_t;

typedef struct ff_card_list_card_s {
  ff_card_t        card_part;
  int              size;
  int              nb_cards;
  ff_card_t      **card_array;
  int              is_free;
  int              nb_token;
  char           **token_array;
} ff_card_list_card_t;


typedef struct ff_condcardlist_s {
  expression_t  *expression;
  int            nb_cards;
  ff_card_t    **card_array;
} ff_condcardlist_t;



typedef struct ff_if_card_s {
  ff_card_t           card_part;
  int                 nb_condcardlists;
  ff_condcardlist_t **condcardlist_array;
} ff_if_card_t;



typedef struct ff_object_list_card_s {
  ff_card_t         card_part;
  char              *comment;
  char              *cell_format;
  int                pos_size_ikw,neg_size_ikw;
  int                pos_array_ikw,neg_array_ikw;
  int                is_free;
  int                length_max;  
  int                nb_otypes;
  obj_type_e        *otype_tab;
} ff_object_list_card_t;



typedef struct ff_subobjects_card_s {
  ff_card_t        card_part;
  int              objects_ikw;
  char            *kfulltype;
  char            *plnkatt;
  char            *clnkatt;
} ff_subobjects_card_t;

enum ff_assign_mode_s
{
  ASSIGN_MODE_IMPORT,
  ASSIGN_MODE_EXPORT
};
typedef enum ff_assign_mode_s ff_assign_mode_e;

typedef enum assign_operator_s
{
    ASSIGN_UNKNOWN,
    ASSIGN_ADD,
    ASSIGN_SUB,
    ASSIGN_DIV,
    ASSIGN_MUL,
	ASSIGN_ATTRIB,
    ASSIGN_GET_ENTITY_VALUE,
    ASSIGN_GET_CURRENT_ENTITY,
    ASSIGN_GET_NLOOKUP_VALUE,
    ASSIGN_GET_NEXT_MAX_AVAILABLE_ID,
    ASSIGN_GET_DISPLAY_STATUS,
    ASSIGN_GET_FORMAT_TYPE,
    ASSIGN_PUSH,
    ASSIGN_GET_NB_FREE_CARDS,
    ASSIGN_COMBINE,
    ASSIGN_ERASE,
    ASSIGN_FIND,
    ASSIGN_EXPRESSION
} assign_operator_e;


/*Basic structure for assign_card*/
typedef struct ff_card_assign_header_s {
    ff_card_t          card_part;
    assign_operator_e  assign_card_type;
    int                attribute_ikw;
    char              *exp_str;
    ff_assign_mode_e   mode;
} ff_card_assign_header_t;

/*Structure for Simple Assign Card*/
typedef struct ff_card_assign_basic_operations_s {
    ff_card_assign_header_t  assign_header;
    int                first_ikey;
    int                second_ikey;
    double             firstVal;
    double             secondVal;
} ff_card_assign_basic_operations_t;

/*Structure for Copying an Attribute*/
typedef struct ff_card_assign_Copy_s {
	ff_card_assign_header_t  assign_header;
	int                ikeyword;
	int                index_ikey;
    int                last_index_ikey; /* if(last_index_ikey > 0),From index_ikey to last_index_ikey the values will be copied*/
} ff_card_assign_Copy_t;

/*Structure for Getting Entity Value*/
typedef struct ff_card_assign_entity_value_s {
    ff_card_assign_header_t  assign_header;
    int                entity_ikey;
    unsigned int       row_ikey;
    unsigned int       col_ikey;
    char              *value_Skey;
    char              *objTypeStr;
} ff_card_assign_entity_value_t;

/*Structure for Nlookup Assign Card*/
typedef struct ff_card_assign_nlookup_s {
    ff_card_assign_header_t  assign_header;
    int                table_num1_val;
    int                table_num2_val;
    int                id_ikey;
} ff_card_assign_nlookup_t;

typedef struct ff_card_assign_displaystatus_s {
    ff_card_assign_header_t  assign_header;
    int                att_ikey;
} ff_card_assign_displaystatus_t;

/*Structure for Pushing attributes to an array in the form of string*/
typedef struct ff_card_assign_push_s {
    ff_card_assign_header_t  assign_header;
    int     att_ikey;
} ff_card_assign_push_t;

typedef struct ff_card_assign_string_s {
    ff_card_assign_header_t  assign_header;
    int                first_ikey;
    int                second_ikey;
    char               *value_str;
} ff_card_assign_string_t;


/* --------- Format --------- */

typedef struct fileformat_s {
  int         nb_cards;
  ff_card_t **card_array;
} fileformat_t;


#endif /* STRUCTURE_FILEFORMAT_H */




