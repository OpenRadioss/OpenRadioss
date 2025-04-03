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
#ifndef _STRUCTURE_HIERARCHICAL_H_
#define _STRUCTURE_HIERARCHICAL_H_




/* CONNECTION TYPE */
typedef enum connec_type_s
{
  CONNECTION_NO_TYPE ,
  SPOTWELD ,
  BOLT ,
  WELDING_LINE ,
  CONNEC_COMMON_NODE ,
  GLUE ,
  HEMMING
} connec_type_e ;

typedef enum create_flag_s
{
  UNKNOWN_STATUS,
  UNCREATED,
  CREATED
} create_flag_e ;



typedef enum subset_type_s {
  SUBSET_UNKNOWN,
  SUBSET_GENE,         /* M-GEN   */
  SUBSET_ASSEMBLY,
  SUBSET_LAST
} subset_type_e;





typedef enum connection_type_s {
  CONNECTION_UNKNOWN,
  CONNECTION_SPOTWELD,     /* M-GRASH */
  CONNECTION_BOLT,         /* M-GRASH */
  CONNECTION_WELDING_LINE, /* M-GRASH */
  CONNECTION_COMMON_NODES, /* M-GRASH */
  CONNECTION_GLUE,         /* M-GRASH */
  CONNECTION_HEMMING,      /* M-GRASH */
  CONNECTION_LAST
} connection_type_e;


typedef enum con_realize_state_s {
  CON_STATE_UNKNOWN,
  CON_STATE_WG_NB_SUPPORT,
  CON_STATE_PART_NOT_FOUND,
  CON_STATE_PROJ_FAIL,
  CON_STATE_LGTH_FAIL,
  CON_STATE_WG_CAO_PT,
  CON_STATE_MAT_PROP_NOT_GENE,
  CON_STATE_SET1_NODE_0,
  CON_STATE_SET2_NODE_0,
  CON_STATE_SET1_SET2_NODE_0,
  CON_STATE_SET1_PART_0,
  CON_STATE_SET2_PART_0,
  CON_STATE_SET1_SET2_PART_0,
  CON_STATE_SET1_PART_SOME,
  CON_STATE_SET2_PART_SOME,
  CON_STATE_LAST
} con_realize_state_e;

typedef enum type_include_s
{
  DEFINED,
  IMPLICATED
} type_include_e ;

/* STRUCTURE FOR THE CONNECTION */

typedef struct cao_point_s
{
/* PM:0018:04/02/2004 */
#ifdef _64BITS
  long double xyz[3] ; /* coordinate of the CAO point */
#else
  double xyz[3] ; /* coordinate of the CAO point */
#endif
  int intermediate; 
} cao_point_t ;

typedef struct cao_segment_s
{
  int ind_point[2] ; /* give the position of the two points
			in the point array */
}  cao_segment_t ;

 
/*FOR CONNEC CAO ATTRIBUTE*/
typedef enum connec_cao_attribute_s {
    CONNEC_CAO_NO_ITEM,
    CONNEC_CAO_NB_POINT,
    CONNEC_CAO_NB_SEGMENT,
    CONNEC_CAO_NB_CRIT,
    CONNEC_CAO_NB_PROP_STATUS,
    CONNEC_CAO_NB_DIAMETER,
    CONNEC_CAO_CRIT_RUPTURE_FLAG,
    CONNEC_CAO_CRIT_VALUE_1,
    CONNEC_CAO_CRIT_VALUE_2,
    CONNEC_CAO_CRIT_VALUE_3,
    CONNEC_CAO_CRIT_VALUE_4,
    CONNEC_CAO_CRIT_VALUE_5,
    CONNEC_CAO_CRIT_VALUE_ALL,
    CONNEC_CAO_CRIT_COMMENT,
    CONNEC_CAO_PROP_STATUS_ITEM,
    CONNEC_CAO_DIAMETER_ITEM,
    CONNEC_CAO_NODE,            
    CONNEC_CAO_DENSITY,
    CONNEC_CAO_STRIP,
    CONNEC_CAO_ELT_TYPE,
    CONNEC_CAO_DIAMETER,
    CONNEC_CAO_WIDTH,
    CONNEC_CAO_X1,
    CONNEC_CAO_X2,
    CONNEC_CAO_X3,
    CONNEC_CAO_V1,
    CONNEC_CAO_V2,
    CONNEC_CAO_V3,
    CONNEC_CAO_BOLT_DIAMETER,
    CONNEC_CAO_BOLT_LENGTH1,
    CONNEC_CAO_BOLT_LENGTH2,
    CONNEC_CAO_BOLT_LENGTH0, /*1+2*/
    CONNEC_CAO_BOLT_SPRING_LENGTH,
    CONNEC_CAO_BOLT_IF_SKEW,
    CONNEC_CAO_BOLT_SKEW_COORD,
    CONNEC_CAO_BOLT_SKEW_COORD_ITEM,
    
    CONNEC_CAO_NB_PAIR,
    CONNEC_CAO_NB_PAIR_ARRAY,
    CONNEC_CAO_NB_PAIR_ARRAY_ITEM,
    CONNEC_CAO_NB_PAIR_ARRAY_ITEM_PART_OF_CON_P,
    CONNEC_CAO_NB_PAIR_ARRAY_ITEM_MAT_P,
    CONNEC_CAO_NB_PAIR_ARRAY_ITEM_PROP_P,
    CONNEC_CAO_NB_PAIR_ARRAY_ITEM_P1_P,
    CONNEC_CAO_NB_PAIR_ARRAY_ITEM_P2_P,
    CONNEC_CAO_NB_PAIR_ARRAY_ITEM_SUBOBJECT_P,
    CONNEC_CAO_NB_PAIR_ARRAY_ITEM_CONTELE_P,

    CONNEC_CAO_IF_NOT_RIGID,
    CONNEC_CAO_IF_IN_TH,
    CONNEC_CAO_PART_ID,
    CONNEC_CAO_MCF_PATH,
    CONNEC_CAO_MCO_MODEL,
    CONNEC_CAO_CONTINUOUS,
    CONNEC_CAO_REALIZE_STATE,
    CONNEC_CAO_NB_MAT_STATUS,
    CONNEC_CAO_MAT_STATUS_ITEM,
    CONNEC_CAO_NB_ITEM
}connec_cao_attribute_e;

typedef struct cao_crit_s
{
  int rupture_flag ; 		/*critere rupture 0 = No, 1 = yes*/
  double crit_value[5] ; 	/* give the position of the two points
			  	in the point array */
  char *comment; 		/*commentaire*/
}  cao_crit_t ;
  


#endif


