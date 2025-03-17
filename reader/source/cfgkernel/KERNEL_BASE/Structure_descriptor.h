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
#ifndef STRUCTURE_DESCRIPTOR_H
#define STRUCTURE_DESCRIPTOR_H
#include "keyword_map.h"
#include "Structure_fileformat.h"

/** Descriptor attribute types */
enum attribute_type_s {
  /** Unknown attribute type */
  ATYPE_UNKNOWN,
  /** Value */
  ATYPE_VALUE,
  /** Size */
  ATYPE_SIZE, 
  /** Static array (constant size) */
  ATYPE_STATIC_ARRAY, 
  /** Dynamic array */
  ATYPE_DYNAMIC_ARRAY, 
  /** Last (to close the enum) */
  ATYPE_LAST
}; 
typedef enum attribute_type_s attribute_type_e;

/** Descriptor value types */
enum value_type_s {
  /** Unknown value type */
  VTYPE_UNKNOWN=-1, 
  /** Bool value (int) */
  VTYPE_BOOL, 
  /** Integer value (int) */
  VTYPE_INT, 
  /** Unsigned Integer value (int) */
  VTYPE_UINT, 
  /** Float value (double) */
  VTYPE_FLOAT, 
  /** String value (char *) */
  VTYPE_STRING, 
  /** Object value (header_t *) */
  VTYPE_OBJECT, 
  /** Last (to close the enum) */
  VTYPE_LAST
};
typedef enum value_type_s value_type_e;
#define VTYPE_LAST_DEFAULT (VTYPE_STRING+1)

enum counter_type_s {
  CTYPE_UNKNOWN=-1, 
  CTYPE_BOOL,
  CTYPE_INT, 
  CTYPE_UINT,
  CTYPE_FLOAT, 
  CTYPE_STRING, 
  CTYPE_OBJECT, 
  CTYPE_ARRAY, 
  CTYPE_LAST
};
typedef enum counter_type_s counter_type_e;


typedef struct attribute_descriptor_s {
  attribute_type_e  atype;
  value_type_e      vtype;
  char             *skeyword;
  char             *comment;
  char             *solver_name;
  int               index;
  int              length;
} attribute_descriptor_t;

typedef struct object_descriptor_s {
  attribute_descriptor_t adescriptor;
  obj_type_e             otype;
/*added for multiobject support*/
  obj_type_e             *allowed_types;
  char                   **comments;
  int                     num;
  char                   **subtypes;
} object_descriptor_t;

typedef struct size_descriptor_s {
  attribute_descriptor_t  adescriptor;
  int                     nb_ikeywords;
  int                    *ikeyword_array;
} size_descriptor_t;





typedef struct dimension_size_s {
  int                   size;  /*one of the multidimesional array size*/
  short                 isRealSize;  /*if it's an ikeyword(=0) or a real (=1) size in the size_array*/   
}dimension_size_t;/**/

typedef struct array_descriptor_s {
  attribute_descriptor_t adescriptor;
  int                    size;
  int                    dimension;     /*dimension*/
  dimension_size_t    *size_array; /*size to each dimension*/
} array_descriptor_t;

typedef struct object_array_descriptor_s {
  object_descriptor_t odescriptor;
  int                    size;
  int                    dimension;     /*dimension*/
  dimension_size_t    *size_array; /*size to each dimension*/
} object_array_descriptor_t;


typedef struct ff_pair_s {
  int            ff_id;
  fileformat_t *ff_p;
} ff_pair_t;

typedef struct descriptor_s {
  int                      user_id;
  int                      max_ikeyword;
  attribute_descriptor_t **attdescr_array;
  keyword_map_t           *keyword_map;
  int                      counter[CTYPE_LAST];
  int                      nb_ff;
  ff_pair_t               *ff_array;
} descriptor_t;


#endif /* STRUCTURE_DESCRIPTOR_H */




