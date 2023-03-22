Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
C macro definition to find indexes in remote structure of edges
C LEDGEFIE and IREM 

#define E_GLOBAL_ID 1
#define E_LEFT_SEG 2
#define E_LEFT_ID 3 
#define E_RIGHT_SEG 4
#define E_RIGHT_ID 5
#define E_NODE1_ID 6
#define E_NODE2_ID 7
#define E_TYPE 8
#define E_NODE1_GLOBID 9
#define E_NODE2_GLOBID 10
#define E_IM 11
#define E_LOCAL_ID 12
#define E_EBINFLG 13
#define E_LEDGE_SIZE 13
#define E_NODNX1 14
#define E_NODAMS1 15
#define E_NODNX2 16
#define E_NODAMS2 17
#define E_IPARTFRIC_E 18
#define E_IBUF_SIZE 18


#define E_X1 1
#define E_Y1 2 
#define E_Z1 3
#define E_X2 4
#define E_Y2 5 
#define E_Z2 6
#define E_VX1 7
#define E_VY1 8 
#define E_VZ1 9
#define E_VX2 10
#define E_VY2 11 
#define E_VZ2 12
#define E_MS1 13
#define E_MS2 14
#define E_GAP 15
#define E_GAPL 16
#define E_STIFE 17
#define E_MAIN 18
#define E_EDG_BIS 19
#define E_VTX_BIS 22
#define E_STIFE_MSDT_FI 41
#define E_RBUF_SIZE 41

C macro for LEDGE

#define LEDGE_LEFT_SEG 1
#define LEDGE_LEFT_ID 2 
#define LEDGE_RIGHT_SEG 3
#define LEDGE_RIGHT_ID 4
#define LEDGE_NODE1_ID 5
#define LEDGE_NODE2_ID 6
#define LEDGE_TYPE 7
#define LEDGE_GLOBAL_ID 8
#define LEDGE_WEIGHT 9
#define LEDGE_SEG1_IM 10
#define LEDGE_SEG1_I1 11
#define LEDGE_SEG1_I2 12
#define LEDGE_SEG2_IM 13
#define LEDGE_SEG2_I1 14
#define LEDGE_SEG2_I2 15

#define LEDGE_TYPE_SOLID 1
