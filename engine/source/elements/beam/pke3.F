Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  PKE3                          source/elements/beam/pke3.F   
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_P                       source/implicit/assem_p.F     
Chd|        PCOORK3                       source/elements/beam/pcoork3.F
Chd|        PEOFF                         source/elements/beam/peoff.F  
Chd|        PKEG3                         source/elements/beam/pke3.F   
Chd|        PMAT3                         source/elements/beam/pmat3.F  
Chd|        PSUMG3                        source/elements/beam/psumg3.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PKE3(JFT   ,JLT    ,NEL  ,MTN      ,ISMSTR,
     1                 PM   ,NCC    ,X    ,ELBUF_TAB,GEO   ,
     2                 OFFSET, IKGEO,
     3                 ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     4                 IADK  ,JDIK   )
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
      INTEGER NCC(NIXP,*),JFT,JLT,OFFSET,NEL ,MTN,ISMSTR,IKGEO,
     .        ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)     
      my_real 
     .   PM(*), X(*),GEO(*),
     .   KE11(36,MVSIZ),KE12(36,MVSIZ),KE22(36,MVSIZ),OFF(MVSIZ),
     .   K_DIAG(*) ,K_LT(*)
C
      TYPE (ELBUF_STRUCT_), TARGET:: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB1, NB2, NB3, NB4, NB5, NB6, NB7, NB8, NB10, 
     .        NB11, NB12,NB13, NB14, I ,J
      INTEGER  MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ)
      my_real
     .    YM(MVSIZ),G(MVSIZ),AL(MVSIZ),ALI(MVSIZ),
     .    SH1(MVSIZ),SH2(MVSIZ),
     .    AREA(MVSIZ),B1(MVSIZ),B2(MVSIZ),B3(MVSIZ),
     .    R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .    R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .    R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),FX(MVSIZ)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      GBUF => ELBUF_TAB%GBUF

      CALL PCOORK3(JFT         ,JLT      ,X      ,NCC   ,GBUF%OFF,
     2             PID         ,MAT      ,NGL    ,AL    ,ALI     , 
     3             GBUF%LENGTH,ISMSTR    ,R11    ,R12   ,R13     ,
     4             R21        ,R22       ,R23    ,R31   ,R32     ,
     5             R33        ,GBUF%SKEW ,OFF    )
      CALL PMAT3(JFT    ,JLT    ,PM     ,GEO     ,YM      ,
     2           G      ,AREA   ,B1     ,B2     ,B3       ,
     3           AL     ,ALI    ,SH1    ,SH2    ,MTN      ,
     4           MAT    ,PID    )
C-------------------------
C     MATRICE de RIGIDITE
C-------------------------
      CALL PSUMG3(JFT    ,JLT    ,YM      ,G      ,AREA   ,
     2           B1     ,B2     ,B3      ,AL     ,ALI    ,
     3           SH1    ,SH2    ,R11     ,R12    ,R13    ,
     4           R21    ,R22    ,R23     ,R31    ,R32    ,
     5           R33    ,KE11   ,KE12    ,KE22   )
      IF (IKGEO>0) THEN
       DO I =JFT ,JLT
         FX(I)=GBUF%FOR(2*NEL+I)
       ENDDO 
       CALL PKEG3(JFT    ,JLT    ,
     1            FX     ,AL     ,KE11    ,KE12    ,KE22   )
      ENDIF 
      
             IF (NEIG>0) CALL PEOFF(
     1                   NIXP, JFT, JLT, NCC, ETAG,
     2                   OFF )
             CALL ASSEM_P(NIXP,6 ,
     1         NCC       ,NEL       ,IDDL      ,NDOF      ,K_DIAG    ,
     2         K_LT      ,IADK      ,JDIK      ,KE11      ,KE12      ,
     3         KE22      ,OFF       )
C
      RETURN
      END
Chd|====================================================================
Chd|  PKEG3                         source/elements/beam/pke3.F   
Chd|-- called by -----------
Chd|        PKE3                          source/elements/beam/pke3.F   
Chd|        R13KE3                        source/elements/spring/r13ke3.F
Chd|-- calls ---------------
Chd|====================================================================
          SUBROUTINE PKEG3(JFT     ,JLT     ,
     1                   FX      ,AL      ,KE11    ,KE12    ,KE22   )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      MY_REAL 
     .    FX(*),AL(*),KE11(6,6,*),KE12(6,6,*),KE22(6,6,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J
      MY_REAL 
     .    H(MVSIZ)
C-----------
       DO EP=JFT,JLT 
        H(EP)=FX(EP)/AL(EP)
       ENDDO
       DO I=1,3
        DO EP=JFT,JLT
         KE11(I,I,EP) = KE11(I,I,EP)+H(EP)
         KE12(I,I,EP) = KE12(I,I,EP)-H(EP)
         KE22(I,I,EP) = KE22(I,I,EP)+H(EP)
        ENDDO
       ENDDO
C
      RETURN
      END
