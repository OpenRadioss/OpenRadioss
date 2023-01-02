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
Chd|  R13KE3                        source/elements/spring/r13ke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_P                       source/implicit/assem_p.F     
Chd|        PEOFF                         source/elements/beam/peoff.F  
Chd|        PKEG3                         source/elements/beam/pke3.F   
Chd|        R13MAT3                       source/elements/spring/r13mat3.F
Chd|        R13SUMG3                      source/elements/spring/r13sumg3.F
Chd|        R4COORK3                      source/elements/spring/r4coork3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE R13KE3(JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                  GEO    ,IXR   ,X     ,ELBUF_TAB,NPF   ,
     2                  TF  ,SKEW  ,OFFSET,FR_WAVE,IKGEO  ,IGEO,
     3                  ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
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
      INTEGER IXR(NIXR,*), NPF(*),JFT,JLT,OFFSET,NEL,MTN,IKGEO
      INTEGER IGEO(*),
     .         ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)

C     REAL
      my_real
     .   GEO(NPROPG,*),X(*),TF(*),SKEW(LSKEW,*),
     .   FR_WAVE(*),PM(*),KE11(36,MVSIZ),KE12(36,MVSIZ),KE22(36,MVSIZ),
     .   OFF(MVSIZ),K_DIAG(*) ,K_LT(*)
C
      TYPE (ELBUF_STRUCT_), TARGET:: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .     AL(MVSIZ),KX(MVSIZ),KY(MVSIZ) ,KZ(MVSIZ),
     .     MX(MVSIZ)  ,MY(MVSIZ)     ,MZ(MVSIZ),
     .    R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .    R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .    R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
      INTEGER  PID(MVSIZ),NGL(MVSIZ)
      INTEGER NB1, NB2, NB3, NB4, NB5, NB6, NB7, NB8, NB9, NB10,
     .   NB11, NB2A, NB2B, NB4A, NB4B, NB5A, NB5B, NB6A, NB6B, NB7A,
     .   NB7B, NB8A, NB8B, NB9A, NB9B, NB10A, NB10B, NB11A, NB11B,
     .   NB12, NB12A, NB12B, NB13, NB13A, NB13B, NB14, NEL3, NEL4,
     .   NB15, NB15A, NB15B, NB15C, NB15D, NB15E, NB15F, NB16,
     .   NB8C, NB8D,OFFSET3,OFFSET4,NBFI,IGTYP,I0,I,II(3)
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
      GBUF => ELBUF_TAB%GBUF
C
C
      DO I=1,3
        II(I) = (I-1)*NEL + 1
      ENDDO
C
C      I0 = IXR(1,1)
C      IGTYP =  NINT(GEO(12,I0))
c      NB1 =1
c      NB2 =NB1  + NEL
c      NB1 =NB1  + OFFSET
c      NEL3 = 3 * NEL
c      NEL4 = 4 * NEL
c      OFFSET3 = 3 * OFFSET
c      OFFSET4 = 4 * OFFSET
C
C=======================================================================
C      IF(IGTYP==13)THEN
C=======================================================================
c       NB3 =NB2  + NEL3
c       NB4 =NB3  + NEL
c       NB5 =NB4  + NEL3
c       IF (SCODVER<101) THEN
c         NB6 =NB5  + NEL3
c       ELSE
c         NB6 =NB5  + 2*NEL3
c       END IF
c       NB7 =NB6  + NEL3
c       NB8 =NB7  + NEL3
c       NB9 =NB8  + NEL3
c       NB10=NB9  + NEL3
c       NB11=NB10 + NEL3
c       NB12=NB11 + NEL3
c       NB13=NB12 + NEL3
c       NB14=NB13 + NEL3
c       IF (SCODVER<101) THEN
c         NB15 = NB14 + NEL3
c       ELSE
c         NB15 = NB14 + 2*NEL3
c       END IF
c       NBFI = NB15 + NEL4*6
c       NB2A=NB2  + NEL
c       NB2B=NB2A + NEL
c       NB4A=NB4  + NEL
c       NB4B=NB4A + NEL
c       NB5A=NB5  + NEL
c       NB5B=NB5A + NEL
c       NB6A=NB6  + NEL
c       NB6B=NB6A + NEL
c       NB7A=NB7  + NEL
c       NB7B=NB7A + NEL
c       NB8A=NB8  + NEL
c       NB8B=NB8A + NEL
c       NB9A=NB9  + NEL
c       NB9B=NB9A + NEL
c       NB10A=NB10  + NEL
c       NB10B=NB10A + NEL
c       NB11A=NB11 + NEL
c       NB11B=NB11A+ NEL
c       NB12A=NB12 + NEL
c       NB12B=NB12A+ NEL
c       NB13A=NB13 + NEL
c       NB13B=NB13A+ NEL
c       NB15A=NB15 + NEL4
c       NB15B=NB15A+ NEL4
c       NB15C=NB15B+ NEL4
c       NB15D=NB15C+ NEL4
c       NB15E=NB15D+ NEL4
c       NB16 =NB15E+ NEL4
c       NBFI =NB16 + 6*NEL
C
c       NB2 =NB2  + OFFSET
c       NB3 =NB3  + OFFSET
c       NB4 =NB4  + OFFSET
c       NB5 =NB5  + OFFSET
c       NB6 =NB6  + OFFSET
c       NB7 =NB7  + OFFSET
c       NB8 =NB8  + OFFSET
c       NB9 =NB9  + OFFSET
c       NB10=NB10 + OFFSET
c       NB11=NB11 + OFFSET
c       NB12=NB12 + OFFSET
c       NB13=NB13 + OFFSET
c       NB14=NB14 + OFFSET3
c       NB2A=NB2A + OFFSET
c       NB2B=NB2B + OFFSET
c       NB4A=NB4A + OFFSET
c       NB4B=NB4B + OFFSET
c       NB5A=NB5A + OFFSET
c       NB5B=NB5B + OFFSET
c       NB6A=NB6A + OFFSET
c       NB6B=NB6B + OFFSET
c       NB7A=NB7A + OFFSET
c       NB7B=NB7B + OFFSET
c       NB8A=NB8A + OFFSET
c       NB8B=NB8B + OFFSET
c       NB9A=NB9A + OFFSET
c       NB9B=NB9B + OFFSET
c       NB10A=NB10A + OFFSET
c       NB10B=NB10B + OFFSET
c       NB11A=NB11A+ OFFSET
c       NB11B=NB11B+ OFFSET
c       NB12A=NB12A+ OFFSET
c       NB12B=NB12B+ OFFSET
c       NB13A=NB13A+ OFFSET
c       NB13B=NB13B+ OFFSET
c       NB15 =NB15  + OFFSET4
c       NB15A=NB15A + OFFSET4
c       NB15B=NB15B + OFFSET4
c       NB15C=NB15C + OFFSET4
c       NB15D=NB15D + OFFSET4
c       NB15E=NB15E + OFFSET4
C    FX,FY,FZ       -> NB2,NB2A,NB2B        E               -> NB3
C    DX,DY,DZ       -> NB4,NB4A,NB4B        OFF             -> NB1
C    DPX,DPY,DPZ    -> NB6,NB6A,NB6B        DPX2,DPY2,DPZ2  -> NB7,NB7A,NB7B
C    FXEP,FYEP,FZEP -> NB12,NB12A,NB12B     X0,Y0,Z0        -> NB5,NB5A,NB5B
C    XMOM,YMOM,ZMOM -> NB8,NB8A,NB8B        RX,RY,RZ        -> NB9,NB9A,NB9B
C    RPX,RPY,RPZ    -> NB10,NB10A,NB10B     XMEP,YMEP,ZMEP  -> NB11,NB11A,NB11B
C    RPX2,RPY2,RPZ2 -> NB13,NB13A,NB13B
C    RLOC           -> NB14
      CALL R4COORK3(JFT    ,JLT    ,X      ,IXR   ,PID    ,
     2              NGL    ,AL     ,R11    ,R12   ,R13    ,
     3              R21    ,R22    ,R23    ,R31   ,R32    ,
     4              R33   ,GBUF%SKEW,GBUF%OFF,OFF   )
      CALL R13MAT3(JFT    ,JLT    ,GEO    ,KX      ,KY     ,
     2             KZ     ,MX     ,MY     ,MZ      ,PID    ,
     3             GBUF%LENGTH(II(1)),GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%TOTDEPL(II(1)),
     4             GBUF%TOTDEPL(II(2)),GBUF%TOTDEPL(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),
     5             GBUF%TOTROT(II(1)),GBUF%TOTROT(II(2)),GBUF%TOTROT(II(3)),TF     ,NPF     ,
     6             GBUF%POSX,GBUF%POSY,GBUF%POSZ,GBUF%POSXX,GBUF%POSYY,
     7             GBUF%POSZZ,IGEO  )
C-------------------------
C     MATRICE de RIGIDITE
C-------------------------
      CALL R13SUMG3(JFT    ,JLT    ,AL      ,KX      ,KY     ,
     2              KZ     ,MX     ,MY      ,MZ      ,R11    ,
     3              R12    ,R13    ,R21     ,R22     ,R23    ,
     4              R31    ,R32    ,R33     ,KE11    ,KE12   ,
     5              KE22   )
      IF (IKGEO==1) CALL PKEG3(JFT    ,JLT    ,
     1                GBUF%FOR(II(1))  ,AL      ,KE11    ,KE12    ,KE22   )


              IF (NEIG>0) CALL PEOFF(
     1                   NIXR, JFT, JLT, IXR, ETAG,
     2                   OFF )
              CALL ASSEM_P(NIXR,6   ,
     1         IXR       ,NEL       ,IDDL      ,NDOF      ,K_DIAG    ,
     2         K_LT      ,IADK      ,JDIK      ,KE11      ,KE12      ,
     3         KE22      ,OFF       )

C-----------------------------------------------
      RETURN
      END
