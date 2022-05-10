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
Chd|  RUSER32KE3                    source/elements/spring/ruser32ke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_P                       source/implicit/assem_p.F     
Chd|        PEOFF                         source/elements/beam/peoff.F  
Chd|        R1COORK3                      source/elements/spring/r1coork3.F
Chd|        R4SUMG3                       source/elements/spring/r4sumg3.F
Chd|        RUSER32MAT3                   source/elements/spring/ruser32mat3.F
Chd|        TKEG3                         source/elements/truss/tkeg3.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE RUSER32KE3 (JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                  GEO    ,IXR   ,X     ,ELBUF_TAB,NPF   ,
     2                  TF     ,SKEW  ,OFFSET,FR_WAVE,
     3                  IKGEO  ,IGEO ,
     4                  ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     5                  IADK  ,JDIK  )
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*), NPF(*),JFT,JLT,OFFSET,NEL,MTN,IKGEO,
     .        IGEO(*), ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)
      my_real DT2T ,
     .   GEO(NPROPG,*),X(*),TF(*),SKEW(LSKEW,*),
     .   FR_WAVE(*),PM(*),KE11(9,MVSIZ),KE12(9,MVSIZ),KE22(9,MVSIZ),
     .   OFF(MVSIZ),K_DIAG(*) ,K_LT(*)
C
      TYPE (ELBUF_STRUCT_), TARGET:: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     AL(MVSIZ),KX(MVSIZ),EX(MVSIZ),EY(MVSIZ),EZ(MVSIZ)
      INTEGER  I,PID(MVSIZ),NGL(MVSIZ),
     .   NB1, NB2, NB3, NB4, NB5, NB6, NB7, NB8, NB9, NB10,
     .   NB11, NB12,  NEL3, NEL4, NEL5, NB15, NB16, NBFI,
     .   NB13, NB14, NUVAR
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
      GBUF => ELBUF_TAB%GBUF
C
c      NB1 =1
c      NB2 =NB1  + NEL
c      NEL3 = 3 * NEL
c      NEL4 = 4 * NEL
c      NEL5 = 5 * NEL
c      NB3 =NB2  + NEL3
c      NB4 =NB3  + NEL
c      NB5 =NB4  + NEL3
c      NB6 =NB5  
c      NB7 =NB6  
c      NB8 =NB7  
c      NB9 =NB8  + NEL3
c      NB10=NB9  + NEL3
c      NB11=NB10 
c      NB12=NB11 
c      NB13=NB12 
c      NB14=NB13 
C
c       IF (SCODVER < 101) THEN
c         NB15 =NB14  + NEL3
c       ELSE
c         NB15 =NB14  + 2*NEL3
c       END IF
      NUVAR =  NINT(GEO(25,IXR(1,1)))
      CALL R1COORK3(JFT    ,JLT    ,X      ,IXR   ,PID    ,
     1              NGL    ,AL     ,EX     ,EY    ,EZ     ,
     2              GBUF%OFF,OFF    )
C     Define local stiffness 
      CALL RUSER32MAT3(KX, JFT, JLT, GBUF%VAR, NUVAR)
C-------------------------
C     MATRICE de RIGIDITE
C-------------------------
      CALL R4SUMG3(JFT    ,JLT    ,AL      ,KX      ,EX     ,
     1             EY     ,EZ     ,KE11    ,KE12    ,KE22   )

      IF (IKGEO.EQ.1) CALL TKEG3(JFT    ,JLT    ,
     1                GBUF%FOR  ,AL      ,KE11    ,KE12    ,KE22   )


              IF (NEIG.GT.0) CALL PEOFF(
     1                   NIXR, JFT, JLT, IXR , ETAG,
     2                   OFF )

              CALL ASSEM_P(NIXR,3   ,
     1         IXR       ,NEL       ,IDDL      ,NDOF      ,K_DIAG    ,
     2         K_LT      ,IADK      ,JDIK      ,KE11       ,KE12     ,
     3         KE22       ,OFF       )
C-----------------------------------------------
      RETURN
      END
