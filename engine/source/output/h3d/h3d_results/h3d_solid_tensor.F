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
Chd|====================================================================
Chd|  H3D_SOLID_TENSOR              source/output/h3d/h3d_results/h3d_solid_tensor.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SOLID_TENSOR(
     .                  ELBUF_TAB,SOLID_TENSOR, IPARG   ,ITENS   ,IXS     ,PM       ,
     2                   EL2FA    ,NBF     ,TENS    ,EPSDOT  ,
     3                   NBPART   ,X       ,IADG    ,IPART   ,
     4                   IPARTSP  ,IPARTS  ,ISPH3D  ,IPM     ,IGEO    ,ID_ELEM   ,ITY_ELEM  ,
     5                   IS_WRITTEN_SOLID  ,LAYER_INPUT ,IR_INPUT  ,IS_INPUT  ,IT_INPUT,
     6                   H3D_PART,INFO1   ,KEYWORD  ,D  ,SOLID_TENSOR_CORNER,IS_CORNER_DATA,
     7                   IXS10   ,MAXNNOD ,ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD               
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "chara_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "spmd_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   SOLID_TENSOR(6,*), TENS(6,*),EPSDOT(6,*),PM(NPROPM,*),X(3,*),D(3,*),
     .   SOLID_TENSOR_CORNER(6,*)
      INTEGER IPARG(NPARG,*),ITENS, MAXNNOD,
     .   IXS(NIXS,*),EL2FA(*),IADG(NSPMD,*),IPM(NPROPMI,*),IXS10(6,*),
     .   NBF,NBPART,IPART(LIPART1,*),IPARTSP(*),IPARTS(*),
     .   ISPH3D,IGEO(NPROPGI,*),IS_WRITTEN_SOLID(*),ID_ELEM(*),ITY_ELEM(*),
     .   H3D_PART(*),INFO1,LAYER_INPUT,IR_INPUT,IS_INPUT,IT_INPUT,IS_CORNER_DATA,ID
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER*ncharline KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER NG
C-----------------------------------------------
       DO NG=1,NGROUP

         CALL H3D_SOLID_TENSOR_1(
     .                  ELBUF_TAB,SOLID_TENSOR, IPARG   ,ITENS   ,IXS     ,PM       ,
     .                   EL2FA    ,NBF     ,TENS    ,EPSDOT  ,
     .                   NBPART   ,X       ,IADG    ,IPART   ,
     .                   IPARTSP  ,IPARTS  ,ISPH3D  ,IPM     ,IGEO    ,ID_ELEM   ,ITY_ELEM  ,
     .                   IS_WRITTEN_SOLID  ,LAYER_INPUT ,IR_INPUT  ,IS_INPUT  ,IT_INPUT,
     .                   H3D_PART,INFO1   ,KEYWORD  ,NG ,D   ,SOLID_TENSOR_CORNER,
     .                   IS_CORNER_DATA   ,IXS10    ,MAXNNOD ,ID    )

       ENDDO
C-----------
      RETURN
      END
