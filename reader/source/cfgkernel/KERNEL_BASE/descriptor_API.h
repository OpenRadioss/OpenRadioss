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
#ifndef DESCRIPTOR_API_H
#define DESCRIPTOR_API_H

#include "Structure_descriptor.h"

#define MV_ID_KEYWORD                    "_ID_"  /*Used if card has ID in first field*/
#define MV_BLANK_KEYWORD                 "_BLANK_"
/**@name Descriptor*/
/*@{*/

/**Enum for descriptor attributes*/
enum descriptor_attribute_s {
  /**@name Common attributes*/
  /*@{*/

  /* Hidden attributes */
  DESCR_COUNTER_TYPE=1,
  DESCR_COUNTER_INDEX,

  /** Attribute type (attribute_type_e)*/
  DESCR_ATTRIB_TYPE,
  /** Attribute type (value_type_e)*/
  DESCR_VALUE_TYPE,
  /** String keyword (char *)*/
  DESCR_SKEYWORD,
  /** Comment (char *)*/
  DESCR_COMMENT,
  /** Solver name (char *)*/
  DESCR_SOLVER_NAME,
  /** Field length in the format block (int)*/
  DESCR_LENGTH,
  /*@}*/

  /**@name Object descriptor attributes*/
  /*@{*/
  /** Object type (obj_type_e)*/
  DESCR_OBJECT_TYPE,
  /*@}*/

  /**@name Static array descriptor attributes*/
  /*@{*/
  /** Size (int)*/
  DESCR_SIZE,
  /*@}*/

  /**@name Dynamic array descriptor attributes*/
  /*@{*/
  /** Integer keyword of the attribute containing the array's size (int)*/
  DESCR_SIZE_IKEYWORD,
  /** String keyword of the attribute containing the array's size (char *)*/
  DESCR_SIZE_SKEYWORD,
  /** Number of connected ikeywords (arrays) of a size attribute */
  DESCR_SIZE_NB_CONN_IKEYWORDS,
  /** Connected ikeywords (indexed access) */
  DESCR_SIZE_CONN_IKEYWORD,
  /*@}*/

  
  /**@name multidimensinal array descriptor attributes*/
  /*@{*/
  /** multidimensional array's dimension*/
  DESCR_DIMENSION,
  /** size for each dimension of array*/
  DESCR_MULTI_SIZE
  /*@}*/
  
};

typedef enum descriptor_attribute_s descriptor_attribute_e;


#ifdef __cplusplus
extern "C" {
#endif


  /** Creates a new descriptor.<br>
      Returns the error code.<br>
      
      @param descr_pfp Double pointer on the descriptor to be created (output)
      
      @return 0: no error<br>
      @return 1: NULL double pointer on the descriptor<br>
  */
  int MCDS_new_descriptor(descriptor_t **descr_pfp);

  /** Is this descriptor user?<br>
      Returns the error code.<br>
      
      @param descr_p Pointer on the descriptor (input)
      @param bool_p  Pointer on the result (true if USER) (output)
      
      @return 0: no error<br>
      @return 1: NULL descriptor pointer<br>
      @return 2: NULL result pointer<br>
  */
  int MCDS_is_descriptor_user(const descriptor_t *descr_p,int *bool_p);

  /** Gets the descriptor's user ID<br>
      Returns the error code.<br>
      
      @param descr_p Pointer on the descriptor (input)
      @param id_p    Pointer on the ID (output)
      
      @return 0: no error<br>
      @return 1: NULL descriptor pointer<br>
      @return 2: NULL ID pointer<br>
  */
  int MCDS_get_descriptor_user_id(const descriptor_t *descr_p,int *id_p);

  /** Sets one or several attributes of a descriptor.<br>
      Returns the error code.<br>
      
      @param descr_p   Pointer on the descriptor (input)
      @param ikeyword  Integer keyword (input)
      @param ...       Must be replaced by a list of couples of 
                       attribute's names (descriptor_attribute_e)
                       and attribute's values, the list must be ended
                       by the END_ARGS terminator

      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
      @return 2: wrong integer keyword<br>
      @return 3: wrong attribute name (or incompatibility with the keyword)<br>

      @see descriptor_attribute_s
  */
  int MCDS_set_descriptor_attributes(const descriptor_t *descr_p,int ikeyword,...);

  /** Sets the descriptor's user ID<br>
      Returns the error code.<br>
      
      @param descr_p Pointer on the descriptor (input/output)
      @param id      ID to set (input)
      
      @return 0: no error<br>
      @return 1: NULL descriptor pointer<br>
  */
  int MCDS_set_descriptor_user_id(descriptor_t *descr_p,int id);
  
  
  int MCDS_add_descriptor_value(descriptor_t *descr_p,
				value_type_e vtype,int ikeyword,const char *skeyword,const char *comment);

  /** Adds an object attribute.<br>
      Returns the error code.<br>

      @param descr_p  Pointer on the descriptor (input)
      @param otype    Object type (input)
      @param ikeyword Integer keyword (input)
      @param skeyword String keyword (input)
      @param comment  Comment (or title) (input)

      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
  */
  int MCDS_add_descriptor_object(descriptor_t *descr_p,
				 obj_type_e otype,int ikeyword,const char *skeyword,const char *comment);
  
  /** Adds a size (of array) attribute.<br>
      Returns the error code.<br>

      @param descr_p  Pointer on the descriptor (input)
      @param ikeyword Integer keyword (input)
      @param skeyword String keyword (input)
      @param comment  Comment (or title) (input)

      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
  */
  int MCDS_add_descriptor_size(descriptor_t *descr_p,
			       int ikeyword,const char *skeyword,const char *comment);
  
  
  int MCDS_add_descriptor_value_array(descriptor_t *descr_p,
				      value_type_e vtype,int ikeyword,const char *skeyword,const char *comment,
				      attribute_type_e array_type,
                                      int dimension , dimension_size_t *size_array);

  /** Adds an array of objects. <br>
      Returns the error code.<br>

      @param descr_p    Pointer on the descriptor (input)
      @param otype      Type of objects (input)
      @param ikeyword   Integer keyword (input)
      @param skeyword   String keyword (input)
      @param comment    Comment (or title)
      @param array_type Type of array (ATYPE_STATIC_ARRAY or ATYPE_DYNAMIC_ARRAY) (input)
      @param dimension  multidimensional array's dimension
      @param size_array size to each dimension(ikeyword for a dynamic array) (input)

      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
      @return 2: wrong array type (only ATYPE_STATIC_ARRAY and ATYPE_DYNAMIC_ARRAY are allowed)<br>
      
      @see attribute_type_s
  */
  int MCDS_add_descriptor_object_array(descriptor_t *descr_p,
				       obj_type_e otype,int ikeyword,const char *skeyword,const char *comment,
				       attribute_type_e array_type,
                                       int dimension , dimension_size_t *size_array);
  
  /** Gets the integer keyword from the string keyword.<br>
      Returns the error code.<br>

      @param descr_p    Pointer on the descriptor (input)
      @param skeyword   String keyword (input)
      @param ikeyword_p Pointer on the integer keyword (output)
      
      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
      @return 2: NULL string keyword<br>
      @return 3: NULL pointer on the integer keyword<br>
  */
  int MCDS_get_descriptor_ikeyword(const descriptor_t *descr_p,const char *skeyword,int *ikeyword_p);
  
  /** Gets the string keyword from the integer keyword.<br>
      Returns the error code.<br>

      @param descr_p    Pointer on the descriptor (input)
      @param ikeyword   Integer keyword (input)
      @param skeyword_p Pointer on the string keyword (output)
      
      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
      @return 2: wrong ikeyword<br>
      @return 3: NULL pointer on the string keyword<br>
      @return 4: no skeyword found for the given ikeyword<br>
  */
  int MCDS_get_descriptor_skeyword(const descriptor_t *descr_p,int ikeyword,char **skeyword_p);

  /** Gets one or several attributes of a descriptor.<br>
      Returns the error code.<br>
      
      @param descr_p   Pointer on the descriptor (input)
      @param ikeyword  Integer keyword (input)
      @param ...       Must be replaced by a list of couples of 
                       attribute's names (descriptor_attribute_e) (input)
		     and attribute's values (output), the list must be 
		     ended by the END_ARGS terminator

      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
      @return 2: wrong integer keyword<br>
      @return 3: wrong attribute name (or incompatibility with the keyword)<br>

      @see descriptor_attribute_s
  */
  int MCDS_get_descriptor_attributes(const descriptor_t *descr_p,int ikeyword,...);

  /** Gets a value in a table of a descriptor.<br>
      Returns the error code.<br>
      
      @param descr_p  Pointer on the descriptor (input)
      @param ikeyword Integer keyword (input)
      @param attrib   Attribute (which table) (input)
      @param ind      Index of the cell in the table (input)
      @param val_p    Pointer on the value (output)
      
      @return 0: no error<br>
      @return 1: NULL descriptor pointer<br>
      @return 2: wrong attribute<br>
      @return 3: NULL value pointer<br>
      @return 4: index out of range<br>

      @see descriptor_attribute_s
  */
  int MCDS_get_descriptor_tab(const descriptor_t *descr_p,int ikeyword,int attrib,int ind,void *val_p);

  /** Gets the first ikeyword.<br>
      Returns the error code.<br>

      @param descr_p          Pointer on the descriptor (input)
      @param first_ikeyword_p Pointer on the first ikeyword (output)

      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
      @return 2: NULL (first) ikeyword pointer<br>
  */
  int MCDS_get_descriptor_first_ikeyword(const descriptor_t *descr_p,int *first_ikeyword_p);

  /** Gets the ikeyword following a current one.<br>
      Returns the error code.<br>

      @param descr_p         Pointer on the descriptor (input)
      @param ikeyword        Current ikeyword (input)
      @param next_ikeyword_p Pointer on the next ikeyword (output)

      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
      @return 2: NULL (next) ikeyword pointer<br>
  */
  int MCDS_get_descriptor_next_ikeyword(const descriptor_t *descr_p,int ikeyword,int *next_ikeyword_p);

  /** Gets the maximum ikeyword (the last one).<br>
      Returns the error code.<br>

      @param descr_p        Pointer on the descriptor (input)
      @param max_ikeyword_p Pointer on the max ikeyword (output)

      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
      @return 2: NULL max ikeyword pointer<br>
  */
  int MCDS_get_descriptor_max_ikeyword(const descriptor_t *descr_p,int *max_ikeyword_p);

  
  int MCDS_check_descriptor_ikeyword(const descriptor_t *descr_p,int ikeyword,int *isvalid_p);

  /* Adds a file format in a descriptor.<br>
     Returns the error code.<br>

     @param descr_p Pointer on the descriptor (input/output)
     @param ff_id   ID associated to the file format (input)
     @param ff_p    Pointer on the file format (input)

     @return 0: no error<br>
     @return 1: NULL pointer on descriptor<br>
  */
  int MCDS_add_descriptor_fileformat(descriptor_t *descr_p,int ff_id,fileformat_t *ff_p);

  /** Gets a file format from a descriptor (using the file format ID).<br>
      Returns the error code.<br>

      @param descr_p Pointer on the descriptor (input/output)
      @param ff_id   ID associated to the file format (input)
      @param ff_p    Pointer on the file format (input)
      
      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
      @return 2: NULL (double) pointer on file format<br>
      @return 3: file format not found<br>
  */
  int MCDS_get_descriptor_fileformat(const descriptor_t *descr_p,int ff_id,const fileformat_t **ff_pp);

  /** Gets a immediate lowest file format from a descriptor (using the file format ID).<br>
      Returns the error code.<br>

      @param descr_p Pointer on the descriptor (input/output)
      @param ff_id   ID associated to the file format (input)
      @param ff_p    Pointer on the file format (input)
      
      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
      @return 2: NULL (double) pointer on file format<br>
      @return 3: file format not found<br>
  */
  int MCDS_get_descriptor_lower_fileformat(const descriptor_t *descr_p, int ff_id, const fileformat_t **ff_pp);
  /** Deletes a descriptor.<br>
      Doesn't free the pointer.<br>

      @param descr_pf Pointer on the descriptor

      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
  */
  int MCDS_delete_descriptor(descriptor_t *descr_pf);

  /** Show the details of the given descriptor.<br>

      @param descr_pf Pointer on the descriptor

      @return 0: no error<br>
      @return 1: NULL pointer on descriptor<br>
  */
  int MCDS_show_descriptor(const descriptor_t *descr_p); 
#ifdef __cplusplus
}
#endif

/*@}*/

#endif /* DESCRIPTOR_API_H */





