Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  R12KE3                        source/elements/spring/r12ke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_R3                      source/implicit/assem_r3.F    
Chd|        ELEOFF                        source/implicit/imp_glob_k.F  
Chd|        R12MAT3                       source/elements/spring/r12mat3.F
Chd|        R12SUMG3                      source/elements/spring/r12sumg3.F
Chd|        R3COORK3                      source/elements/spring/r3coork3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE R12KE3(JFT    ,JLT   ,NEL   ,MTN      ,PM    ,
     1                  GEO    ,IXR   ,X     ,ELBUF_TAB,NPF   ,
     2                  TF     ,SKEW  ,OFFSET,FR_WAVE  ,IGEO  ,
     3                  ETAG  , IDDL  ,NDOF  ,K_DIAG   ,K_LT  ,
     4                  IADK  ,JDIK  )

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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*), NPF(*),JFT,JLT,OFFSET,NEL   ,MTN
      INTEGER IGEO(*),
     .         ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)

C     REAL
      my_real DT2T ,
     .   GEO(NPROPG,*),X(*),TF(*),SKEW(LSKEW,*),
     .   FR_WAVE(*),PM(*),KE11(9,MVSIZ),KE12(9,MVSIZ),KE22(9,MVSIZ),
     .   KE33(9,MVSIZ),KE13(9,MVSIZ),KE23(9,MVSIZ),
     .   OFF(MVSIZ),K_DIAG(*) ,K_LT(*)
C
      TYPE (ELBUF_STRUCT_), TARGET:: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .     KX(MVSIZ),DKX(MVSIZ),EX(MVSIZ),EY(MVSIZ),EZ(MVSIZ),
     .     EX2(MVSIZ),EY2(MVSIZ),EZ2(MVSIZ),BETA(MVSIZ)
      INTEGER  PID(MVSIZ),NGL(MVSIZ)
      INTEGER NB1, NB2, NB3, NB4, NB5, NB6, NB7, NB8, NB9, NB10,
     .   NB11, NB12,  NEL3, NEL4,NB15,  NB16,NBFI
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
      GBUF => ELBUF_TAB%GBUF
C
c      NB1 =1
c      NB2 =NB1  + NEL
c      NB1 =NB1  + OFFSET
c      NEL3 = 3 * NEL
c      NEL4 = 4 * NEL
C
C=======================================================================
c       NB3 =NB2  + NEL
c       NB4 =NB3  + NEL
c       NB5 =NB4  + NEL
c       IF (SCODVER.LT.101) THEN
c         NB6 =NB5  + NEL
c       ELSE
c         NB6 =NB5  + 2*NEL
c       END IF
c       NB7 =NB6  + NEL
c       NB8 =NB7  + NEL
c       NB15 =NB8  + NEL
c       NBFI=NB15  + NEL4
C......EVENTUELS SUPER-GROUPES OFFSETTING
c       NB2 =NB2  + OFFSET
c       NB3 =NB3  + OFFSET
c       NB4 =NB4  + OFFSET
c       NB5 =NB5  + OFFSET
c       NB6 =NB6  + OFFSET
c       NB7 =NB7  + OFFSET
c       NB8 =NB8  + OFFSET
c       NB15 =NB15  + 4 * OFFSET
      CALL R3COORK3(JFT    ,JLT    ,X      ,IXR   ,PID    ,
     1              NGL    ,EX     ,EY     ,EZ    ,EX2    ,
     2              EY2    ,EZ2    ,BETA   ,GBUF%OFF,OFF   )
      CALL R12MAT3(JFT    ,JLT    ,GEO    ,KX     ,PID    ,
     1             DKX    ,BETA   ,GBUF%LENGTH,GBUF%FOR,GBUF%TOTDEPL,
     2             TF     ,NPF    ,GBUF%POSX,IGEO  )
C-------------------------
C     MATRICE de RIGIDITE
C-------------------------
      CALL R12SUMG3(JFT    ,JLT    ,KX      ,DKX    ,EX     ,
     1              EY     ,EZ     ,EX2     ,EY2    ,EZ2    ,
     2              KE11   ,KE12   ,KE13    ,KE22   ,KE23   ,
     3              KE33   )
C-----------------------------------------------\

              IF (NEIG.GT.0) CALL ELEOFF(
     1                   JFT, JLT, IXR,NIXR ,3  , ETAG, OFF)
         CALL ASSEM_R3(NIXR    ,3        ,
     1       IXR       ,NEL       ,IDDL      ,NDOF      ,K_DIAG    ,
     2       K_LT      ,IADK      ,JDIK      ,KE11       ,KE12      ,
     3       KE13      ,KE22      ,KE23      ,KE33       ,OFF    )

      RETURN
      END
