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
Chd|  TKE3                          source/elements/truss/tke3.F  
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_P                       source/implicit/assem_p.F     
Chd|        PEOFF                         source/elements/beam/peoff.F  
Chd|        R4SUMG3                       source/elements/spring/r4sumg3.F
Chd|        TCOORK3                       source/elements/truss/tcoork3.F
Chd|        TKEG3                         source/elements/truss/tkeg3.F 
Chd|        TMAT3                         source/elements/truss/tmat3.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TKE3(JFT    ,JLT      ,PM      ,GEO    ,NCT   ,
     1                X      ,ELBUF_TAB,NEL     ,OFFSET ,IKGEO ,
     2                ETAG   ,IDDL     ,NDOF    ,K_DIAG ,K_LT  ,
     3                IADK   ,JDIK     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NCT(NIXT,*),OFFSET,JFT,JLT,NEL  ,IKGEO, 
     .        ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)    
      my_real 
     .   PM(*), GEO(*), X(*),
     .   KE11(9,MVSIZ),KE12(9,MVSIZ),KE22(9,MVSIZ),OFF(MVSIZ),
     .   K_DIAG(*) ,K_LT(*)
C
      TYPE (ELBUF_STRUCT_), TARGET:: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB1, NB2, NB3, NB4, NB5, NB6, NB10, NB11, LCO
      INTEGER MAT(MVSIZ),NGL(MVSIZ)
      my_real 
     .  AL(MVSIZ),KX(MVSIZ),EX(MVSIZ),EY(MVSIZ),EZ(MVSIZ)
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       GBUF => ELBUF_TAB%GBUF
C
c      NB1 =1
c      NB2 =NB1  + NEL
c      NB3 =NB2  + NEL
c      NB4 =NB3  + NEL
c      NB5 =NB4  + NEL
c      NB6 =NB5  + NEL
c      NB10=NB6
c      NB11=NB10 + NEL
c      !offsetting des super-groupes
c      NB1 =NB1 + OFFSET
c      NB2 =NB2 + OFFSET
c      NB3 =NB3 + OFFSET
c      NB4 =NB4 + OFFSET
c      NB5 =NB5 + OFFSET
c      NB6 =NB6
c      NB10=NB10 + OFFSET
C
      CALL TCOORK3(JFT     ,JLT    ,X     ,NCT    ,MAT    ,
     1             NGL     ,AL     ,EX     ,EY    ,EZ     ,
     2             GBUF%OFF,OFF    )
      CALL TMAT3(JFT    ,JLT    ,PM    ,MAT    ,GBUF%AREA,
     .           AL     ,KX     )
C-------------------------
C     MATRICE de RIGIDITE
C-------------------------
      CALL R4SUMG3(JFT    ,JLT    ,AL      ,KX      ,EX     ,
     1             EY     ,EZ     ,KE11    ,KE12    ,KE22   )
      IF (IKGEO>0) CALL TKEG3(JFT    ,JLT    ,
     1                GBUF%FOR  ,AL      ,KE11   ,KE12    ,KE22   )
      CALL ASSEM_P(NIXT    ,3      ,
     1         NCT       ,NEL       ,IDDL      ,NDOF      ,K_DIAG    ,
     2         K_LT      ,IADK      ,JDIK      ,KE11      ,KE12     ,
     3         KE22       ,OFF        )
      IF (NEIG>0) CALL PEOFF(
     1                   NIXT, JFT, JLT, NCT , ETAG,
     2                   OFF )
     
      RETURN
      END
      
      
     
