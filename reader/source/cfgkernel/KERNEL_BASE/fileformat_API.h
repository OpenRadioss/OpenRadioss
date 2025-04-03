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
#ifndef FILEFORMAT_API_H
#define FILEFORMAT_API_H

#include "Structure_fileformat.h"
#include <HCDI/hcdi.h>

/**@name File formats, cards, cells*/
/*@{*/

#ifdef __cplusplus
extern "C" {
#endif

/**@name Cells*/
/*@{*/

  
  /** Cell attributes*/
  enum ff_cell_attribute_s {
    /** Cell's type */
    CELL_TYPE=1,
       
    CELL_SIZE,
     
    CELL_STRING,                           
    /** Format of a cell (CELL_VALUE, CELL_DIR_RADIO, CELL_DIR_FLAGS) */
    CELL_FORMAT,
    
    /** Number of integer keywords of a cell (CELL_DIGITS) */
    CELL_NB_IKEYWORDS,
    /** Integer keyword of a cell (CELL_VALUE, CELL_DIR_RADIO)<br>
        or integer keywords of a cell (CELL_DIGITS)
    */
    CELL_IKEYWORD,
    
    /** Flag for moments (CELL_DIR_RADIO) */
    CELL_IS_EXTENDED,
    /** Integer keyword of the X-direction (CELL_DIR_FLAGS) */
    CELL_DIRX_IKW,
    /** Integer keyword of the Y-direction (CELL_DIR_FLAGS) */
    CELL_DIRY_IKW,
    /** Integer keyword of the Z-direction (CELL_DIR_FLAGS) */
    CELL_DIRZ_IKW,
    
    
    /** Integer keyword of the flag (CELL_SCALAR_OR_OBJECT, CELL_SCALAR_OR_STRING) */
    CELL_FLAG_IKW,
    /** Integer keyword of the scalar (CELL_SCALAR_OR_OBJECT, CELL_SCALAR_OR_STRING) */
    CELL_SCALAR_IKW,
    /** Integer keyword of the function id (CELL_SCALAR_OR_OBJECT) */
    CELL_OBJECT_IKW,
    
    /** Integer keyword of the string (CELL_SCALAR_OR_STRING) */
    CELL_STRING_IKW,
    
    /** Number of conditional cells*/
    CELL_NB_COND_CELL,

    /** Conditional cell*/
    CELL_COND_CELL,

    /* Cell List Index */
    CELL_LIST_INDEX,
    
    /** number of name strings used for an skeyword in abaqus deck*/
    CELL_NAME_VALUE_NUMBER,
    /** ikeyword of attribute stored as name value pair*/
    CELL_NAME_VALUE_IKEYWORD,
    /** name string used for an skeyword in abaqus deck*/
    CELL_NAME_VALUE_STRING,
    /** name value pair relation[mostly = ]*/
    CARD_NAME_VALUE_PAIR_CHAR,
    /** name value pair separator[, :*]*/
    CARD_NAME_VALUE_PAIR_SEPARATOR
  };
  

  typedef enum ff_cell_attribute_s ff_cell_attribute_e;


  /** Creates a new file format cell.<br>
      Returns the error code.<br>
      
      @param cell_pfp  Double pointer on the cell to be created (output)
      @param cell_type Type of cell (input)
      
      @return 0: no error<br>
      @return 1: NULL double pointer on the cell<br>
      @return 2: wrong cell type<br>

      @see ff_cell_type_s
  */
  int MCDS_new_ff_cell(ff_cell_t **cell_pfp,ff_cell_type_e cell_type);
  


  /** Creates a new conditional list of cell (CCL).<br>
      Returns the error code.<br>
      
      @param ccl_pp   Double pointer on the CCL to be created (output)
      @param expr_p   Pointer on expression (input)
      @param cell_p   Pointer on cell (input)
      @return 0: no error<br>
      @return 1: NULL double pointer on the CCL<br>
  */

  int MCDS_new_ff_condcell(ff_condcell_t **ccl_pp, expression_t *expr_p, ff_cell_t *cell_p);

  /** Gets one or several attributes of a file format cell.<br>
      Returns the error code.<br>
      
      @param cell_p Pointer on the cell (input)
      @param ...    Must be replaced by a list of couples of 
                    attribute's names (ff_cell_attribute_e) (input)
		    and attribute's values (output), the list must be 
		    ended by the END_ARGS terminator

      @return 0: no error<br>
      @return 1: NULL pointer on cell<br>
      @return 2: wrong attribute<br>

      @see ff_cell_attribute_s
  */
  HC_DATA_DLL_API int MCDS_get_ff_cell_attributes(const ff_cell_t *cell_p,...);

  /** Sets one or several attributes of a file format cell.<br>
      Returns the error code.<br>
      
      @param cell_p Pointer on the cell (input/output)
      @param ...    Must be replaced by a list of couples of 
                    attribute's names (ff_cell_attribute_e) (input)
		    and attribute's values (input), the list must be 
		    ended by the END_ARGS terminator

      @return 0: no error<br>
      @return 1: NULL pointer on cell<br>
      @return 2: wrong attribute<br>

      @see ff_cell_attribute_s
  */
  int MCDS_set_ff_cell_attributes(ff_cell_t *cell_p,...);

  
  /** Gets a value in a table of a file format cell.<br>
      Returns the error code.<br>
      
      @param cell_p Pointer on the card (input)
      @param attrib Attribute (which table) (input)
      @param ind    Index in the table (input)
      @param data_p Pointer on the value (output)
      
      @return 0: no error<br>
      @return 1: NULL cell pointer<br>
      @return 2: wrong attribute<br>
      @return 3: index out of range<br>
      @return 4: NULL value pointer<br>

      @see ff_cell_attribute_s
  */
  HC_DATA_DLL_API int MCDS_get_ff_cell_tab(const ff_cell_t *cell_p,int attrib,int ind,void *data_p);

  /** Sets a value in a table of a file format cell.<br>
      Returns the error code.<br>
      
      @param cell_p Pointer on the card (input/output)
      @param attrib Attribute (which table) (input)
      @param ind    Index in the table (input)
      @param data_p Pointer on the value (input)
      
      @return 0: no error<br>
      @return 1: NULL cell pointer<br>
      @return 2: wrong attribute<br>
      @return 3: index out of range<br>
      @return 4: NULL value pointer<br>

      @see ff_cell_attribute_s
  */
  int MCDS_set_ff_cell_tab(ff_cell_t *cell_p,int attrib,int ind,const void *data_p);
  
 
  /** Deletes a file format cell.<br>
      Doesn't free the pointer.<br>

      @param cell_p Pointer on the cell

      @return 0: no error<br>
      @return 1: NULL pointer on the cell<br>
      @return 2: wrong or unknown cell type<br>
  */
  int MCDS_delete_ff_cell(ff_cell_t *cell_p); 



  /** Gets the expression (condition) of a conditional cell (CCL).<br>
      Returns the error code.<br>
      
      @param ccl_p   Pointer on the CCL (input)
      @param expr_pp Double pointer on the expression (output)
      
      @return 0: no error<br>
      @return 1: NULL pointer on the CCL<br>
      @return 2: NULL pointer on the number of cells<br>
  */
  HC_DATA_DLL_API int MCDS_get_ff_condcell_expression(const ff_condcell_t *ccl_p,expression_t **expr_pp);


  /** Gets the cell of a conditional cell (CCL).<br>
      Returns the error code.<br>
      
      @param ccl_p   Pointer on the CCL (input)
      @param expr_pp Double pointer on the cell (output)
      
      @return 0: no error<br>
      @return 1: NULL pointer on the CCL<br>
      @return 2: NULL pointer on the number of cells<br>
  */

  HC_DATA_DLL_API int MCDS_get_ff_condcell_cell(const ff_condcell_t *ccl_p,ff_cell_t **cell_p) ;

  /** Deletes a conditional list of cell (CCL).<br>
      Doesn't free the pointer.<br>

      @param ccl_p Pointer on the condition

      @return 0: no error<br>
      @return 1: NULL pointer on condition<br>
  */
  int MCDS_delete_ff_condcell(ff_condcell_t *ccl_p);

  /** Deletes a name value item of cell (CCL).<br>
  Doesn't free the pointer.<br>

  @param ccl_p Pointer on the condition

  @return 0: no error<br>
  @return 1: NULL pointer on condition<br>
  */
  int MCDS_delete_ff_namevaluearray(ff_name_info_t *ccl_p);
/*@}*/


/**@name Cards*/
/*@{*/

  /** Card attributes*/
  enum ff_card_attribute_s {
    /** Card's type */
    CARD_TYPE=1,
    /** Comment of a comment card */
    CARD_STRING,   
    /** Size of a cell list */
    CARD_SIZE,
    /** Length (number of chars of a line) max of a cell list */
    CARD_LENGTH_MAX, 
    /** Free format (cell list, and object list cards) */
    CARD_IS_FREE,  
    /** Number of cells */
    CARD_NB_CELLS,
    /** Cell of an array */
    CARD_CELL,
    /** Number of cards */
    CARD_NB_CARDS,
    /** Card of an array */
    CARD_CARD,
    /** Number of conditional lists of cards */
    CARD_NB_COND_CARD_LISTS, 
    /** Conditional list of card of an array */
    CARD_COND_CARD_LIST,    
    
    /** Cell format */
    CARD_CELL_FORMAT,
    /** I-Keyword of the size of the positive array */
    CARD_POS_SIZE,
    /** I-Keyword of the size of the negative array */
    CARD_NEG_SIZE,
    /** I-Keyword of the positive array */
    CARD_POS_ARRAY,
    /** I-Keyword of the negative array */
    CARD_NEG_ARRAY,
    /** Number of valid object types */
    CARD_NB_OTYPES,
    /** Array of valid object types */
    CARD_OTYPE,
    
    
    /** Integer keyword of the object or object array (CARD_SUBOBJECTS) */
    CARD_OBJECTS_IKW,
    /** character string kernel-fulltype */
    CARD_KFULLTYPE,
    
    /** Cards flag will be handled */
    CARD_FLAGS,
    /** Offet format for card_cell_list and free_cell_list will be taken care */
    CARD_CELL_LIST_OFFSET_FORMAT,
    /** Offet value for card_cell_list and free_cell_list will be taken care */
    CARD_CELL_LIST_OFFSET_VALUE,

	/*no of strings stored in free card list as stopper*/
	CARD_FREE_CARD_LIST_TOKEN_NB,
	/**array of strings stored in free card list as stopper*/
	CARD_FREE_CARD_LIST_TOKEN_STR,
    CARD_IKEYWORD_NB_BLOCKS,
    CARD_SUBOBJ_PARENT_LNK_ATT,
    CARD_SUBOBJ_CHILD_LNK_ATT
  };

  typedef enum ff_card_attribute_s ff_card_attribute_e;

  /** Creates a new file format card.<br>
      Returns the error code.<br>
      
      @param card_pfp  Double pointer on the card to be created (output)
      @param card_type Type of card (input)
      
      @return 0: no error<br>
      @return 1: NULL double pointer on the card<br>
      @return 2: wrong card type<br>

      @see ff_card_type_s
  */
  int MCDS_new_ff_card(ff_card_t **card_pfp,ff_card_type_e card_type);
  
  int MCDS_new_ff_assign_card(ff_card_t **card_pfp, assign_operator_e assign_mode);
  /** Gets one or several attributes of a file format card.<br>
      Returns the error code.<br>
      
      @param card_p Pointer on the card (input)
      @param ...    Must be replaced by a list of couples of 
                    attribute's names (ff_card_attribute_e) (input)
		    and attribute's values (output), the list must be 
		    ended by the END_ARGS terminator

      @return 0: no error<br>
      @return 1: NULL pointer on card<br>
      @return 2: wrong attribute<br>

      @see ff_card_attribute_s
  */
  HC_DATA_DLL_API int MCDS_get_ff_card_attributes(const ff_card_t *card_p,...);

  /** Sets one or several attributes of a file format card.<br>
      Returns the error code.<br>
      
      @param card_p Pointer on the card (input/output)
      @param ...    Must be replaced by a list of couples of 
                    attribute's names (ff_card_attribute_e) (input)
		    and attribute's values (input), the list must be 
		    ended by the END_ARGS terminator

      @return 0: no error<br>
      @return 1: NULL pointer on card<br>
      @return 2: wrong attribute<br>

      @see ff_card_attribute_s
  */
  int MCDS_set_ff_card_attributes(ff_card_t *card_p,...);

  /** Gets a value in a table of a file format card.<br>
      Returns the error code.<br>
      
      @param card_p Pointer on the card (input)
      @param attrib Attribute (which table) (input)
      @param ind    Index of the cell in the table (input)
      @param data_p Pointer on the value (output)
      
      @return 0: no error<br>
      @return 1: NULL card pointer<br>
      @return 2: wrong attribute<br>
      @return 3: index out of range<br>
      @return 4: NULL value pointer<br>

      @see ff_card_attribute_s
  */
  HC_DATA_DLL_API int MCDS_get_ff_card_tab(const ff_card_t *card_p,int attrib,int ind,void *data_p);

  /** Sets a value in a table of a file format card.<br>
      Returns the error code.<br>
      
      @param card_p Pointer on the card (input/output)
      @param attrib Attribute (which table) (input)
      @param ind    Index of the cell in the table (input)
      @param data_p Pointer on the value (input)
      
      @return 0: no error<br>
      @return 1: NULL card pointer<br>
      @return 2: wrong attribute<br>
      @return 3: index out of range<br>
      @return 4: NULL value pointer<br>

      @see ff_card_attribute_s
  */
  int MCDS_set_ff_card_tab(ff_card_t *card_p,int attrib,int ind,const void *data_p);

  /** Deletes a file format card.<br>
      Doesn't free the pointer.<br>

      @param card_p Pointer on the card

      @return 0: no error<br>
      @return 1: NULL pointer on card<br>
      @return 2: wrong card type<br>
  */
  int MCDS_delete_ff_card(ff_card_t *card_p);

/*@}*/



/**@name Conditional list of cards*/
/*@{*/

  /** Creates a new conditional list of card (CCL).<br>
      Returns the error code.<br>
      
      @param ccl_pp   Double pointer on the CCL to be created (output)
      @param expr_p   Expression (input)
      @param nb_cards Number of cards (input)
      
      @return 0: no error<br>
      @return 1: NULL double pointer on the CCL<br>
  */
  int MCDS_new_ff_condcardlist(ff_condcardlist_t **ccl_pp,expression_t *expr_p,int nb_cards);

  /** Gets the expression (condition) of a conditional list of card (CCL).<br>
      Returns the error code.<br>
      
      @param ccl_p   Pointer on the CCL (input)
      @param expr_pp Double pointer on the expression (output)
      
      @return 0: no error<br>
      @return 1: NULL pointer on the CCL<br>
      @return 2: NULL pointer on the number of cards<br>
  */
  HC_DATA_DLL_API int MCDS_get_ff_condcardlist_expression(const ff_condcardlist_t *ccl_p,expression_t **expr_pp);

  /** Gets the number of cards of a conditional list of card (CCL).<br>
      Returns the error code.<br>
      
      @param ccl_p      Pointer on the CCL (input)
      @param nb_cards_p Pointer on the number of cards (output)
      
      @return 0: no error<br>
      @return 1: NULL pointer on the CCL<br>
      @return 2: NULL pointer on the number of cards<br>
  */
  HC_DATA_DLL_API int MCDS_get_ff_condcardlist_nb_cards(const ff_condcardlist_t *ccl_p,int *nb_cards_p);

  /** Gets a card of a conditional list of card (CCL), at the given index.<br>
      Returns the error code.<br>
      
      @param ccl_p   Double pointer on the CCL (input)
      @param ind     Index of the card (input)
      @param card_pp Double pointer on the card (output)
      
      @return 0: no error<br>
      @return 1: NULL pointer on the CCL<br>
      @return 2: index out of bounds<br>
      @return 3: NULL double pointer on the card<br>
  */
  HC_DATA_DLL_API int MCDS_get_ff_condcardlist_card(const ff_condcardlist_t *ccl_p,int ind,ff_card_t **card_pp);

  /** Sets a card of a conditional list of card (CCL), at the given index.<br>
      Returns the error code.<br>
      
      @param ccl_p   Double pointer on the CCL (input/output)
      @param ind     Index of the card (input)
      @param card_pp Double pointer on the number of card (input)
      
      @return 0: no error<br>
      @return 1: NULL pointer on theCCL<br>
      @return 2: index out of bounds<br>
      @return 3: NULL pointer on the card<br>
  */
  int MCDS_set_ff_condcardlist_card(ff_condcardlist_t *ccl_p,int ind,ff_card_t *card_p);

  /** Deletes a conditional list of card (CCL).<br>
      Doesn't free the pointer.<br>

      @param ccl_p Pointer on the condition

      @return 0: no error<br>
      @return 1: NULL pointer on condition<br>
  */
  int MCDS_delete_ff_condcardlist(ff_condcardlist_t *ccl_p);

/*@}*/



/**@name File formats*/
/*@{*/

  /** Creates a new file format.<br>
      Returns the error code.<br>
      
      @param ff_pfp   Double pointer on the file format to be created (output)
      @param nb_cards Number of cards (input)
      
      @return 0: no error<br>
      @return 1: NULL pointer on file format<br>
  */
  int MCDS_new_fileformat(fileformat_t **ff_pfp,int nb_cards);

  /** Gets the number of cards of a new file format.<br>
      Returns the error code.<br>
      
      @param ff_p       Double pointer on the file format to be created (input)
      @param nb_cards_p Pointer on the number of cards (output)
      
      @return 0: no error<br>
      @return 1: NULL pointer on file format<br>
      @return 2: NULL pointer on the number of cards<br>
  */
  HC_DATA_DLL_API int MCDS_get_fileformat_nb_cards(const fileformat_t *ff_p,int *nb_cards_p);

  /** Gets a card of a new file format, at the given index.<br>
      Returns the error code.<br>
      
      @param ff_p    Double pointer on the file format to be created (input)
      @param ind     Index of the card (input)
      @param card_pp Double pointer on the number of card (output)
      
      @return 0: no error<br>
      @return 1: NULL pointer on file format<br>
      @return 2: index out of bounds<br>
      @return 3: NULL double pointer on the card<br>
  */
  HC_DATA_DLL_API int MCDS_get_fileformat_card(const fileformat_t *ff_p,int ind,ff_card_t **card_pp);

  /** Sets a card of a new file format, at the given index.<br>
      Returns the error code.<br>
      
      @param ff_p    Double pointer on the file format to be created (input/output)
      @param ind     Index of the card (input)
      @param card_pp Double pointer on the number of card (input)
      
      @return 0: no error<br>
      @return 1: NULL pointer on file format<br>
      @return 2: index out of bounds<br>
      @return 3: NULL double pointer on the card<br>
  */
  int MCDS_set_fileformat_card(fileformat_t *ff_p,int ind,ff_card_t *card_p);

  /** Deletes a file format.<br>
      Doesn't free the pointer.<br>

      @param ff_p Pointer on the file format

      @return 0: no error<br>
      @return 1: NULL pointer on file format<br>
  */
  int MCDS_delete_fileformat(fileformat_t *ff_p);

/*@}*/


#ifdef __cplusplus
}
#endif

/*@}*/


#endif /* FILEFORMAT_API_H */




