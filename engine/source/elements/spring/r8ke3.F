Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    r8ke3          ../engine/source/elements/spring/r8ke3.F
!||--- called by ------------------------------------------------------
!||    imp_glob_k     ../engine/source/implicit/imp_glob_k.F
!||    imp_glob_k0    ../engine/source/implicit/imp_glob_k.F
!||--- calls      -----------------------------------------------------
!||    assem_p        ../engine/source/implicit/assem_p.F
!||    peoff          ../engine/source/elements/beam/peoff.F
!||    r13mat3        ../engine/source/elements/spring/r13mat3.F
!||    r2coork3       ../engine/source/elements/spring/r2coork3.F
!||    r8chk3         ../engine/source/elements/spring/r8ke3.F
!||    r8sumg3        ../engine/source/elements/spring/r8sumg3.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE R8KE3(JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                 GEO    ,IXR   ,X     ,ELBUF_TAB,NPF   ,
     2                 TF     ,SKEW  ,OFFSET,FR_WAVE,IGEO  ,
     3                 ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     4                 IADK  ,JDIK )
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
     .        ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)

      my_real
     .   GEO(NPROPG,*),X(*),TF(*),SKEW(LSKEW,*),
     .   FR_WAVE(*),PM(*),KE11(36,MVSIZ),KE12(36,MVSIZ),KE22(36,MVSIZ),
     .   OFF(MVSIZ),K_DIAG(*) ,K_LT(*)
C
      TYPE (ELBUF_STRUCT_), TARGET:: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     KX(MVSIZ)  ,KY(MVSIZ) ,KZ(MVSIZ),
     .     MX(MVSIZ)  ,MY(MVSIZ)     ,MZ(MVSIZ),
     .    R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .    R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .    R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),AL0(MVSIZ),AL(3,MVSIZ)
      INTEGER  PID(MVSIZ),NGL(MVSIZ)
      INTEGER NB1, NB2, NB3, NB4, NB5, NB6, NB7, NB8, NB9, NB10,
     .   NB11, NB2A, NB2B, NB4A, NB4B, NB5A, NB5B, NB6A, NB6B, NB7A,
     .   NB7B, NB8A, NB8B, NB9A, NB9B, NB10A, NB10B, NB11A, NB11B,
     .   NB12, NB12A, NB12B, NB13, NB13A, NB13B, NB14, NEL3, NEL4,
     .   NB15, NB15A, NB15B, NB15C, NB15D, NB15E, NB15F, NB16,
     .   NB8C, NB8D,NBFI,IGTYP,I0,I,II(3)
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
      GBUF => ELBUF_TAB%GBUF
C
C
      DO I=1,3
        II(I) = (I-1)*NEL + 1
      ENDDO

      CALL R2COORK3(JFT    ,JLT    ,X       ,IXR    ,PID    ,
     2              NGL    ,R11    ,R12     ,R13    ,R21    ,
     3              R22    ,R23    ,R31     ,R32    ,R33    ,
     4              SKEW   ,GEO    ,GBUF%OFF,OFF    ,AL     ,
     5              IGEO )

      DO I=JFT,JLT
       AL0(I)=ONE
      ENDDO

      CALL R13MAT3(JFT    ,JLT    ,GEO    ,KX      ,KY     ,
     2             KZ     ,MX     ,MY     ,MZ      ,PID    ,
     3             AL0    ,GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%TOTDEPL(II(1)),
     4             GBUF%TOTDEPL(II(2)),GBUF%TOTDEPL(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),
     5             GBUF%TOTROT(II(1)),GBUF%TOTROT(II(2)),GBUF%TOTROT(II(3)),TF     ,NPF     ,
     6             GBUF%POSX,GBUF%POSY,GBUF%POSZ,GBUF%POSXX,GBUF%POSYY,
     7             GBUF%POSZZ,IGEO  )
C-------------------------
C     MATRICE de RIGIDITE
C-------------------------
      CALL R8SUMG3 (JFT    ,JLT     ,KX      ,KY     ,KZ     ,
     2              MX     ,MY      ,MZ      ,R11    ,R12    ,
     3              R13    ,R21     ,R22     ,R23    ,R31    ,
     4              R32    ,R33     ,KE11    ,KE12   ,KE22   )
      CALL R8CHK3(JFT    ,JLT    ,NGL    ,AL     ,OFF    ,
     1                  KX     ,KY     ,KZ     ,MX     ,MY     ,
     2                  MZ     ,R11    ,R12    ,R13    ,R21    ,
     3                  R22    ,R23    ,R31    ,R32    ,R33    )
C      ENDIF


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

!||====================================================================
!||    r8chk3   ../engine/source/elements/spring/r8ke3.F
!||--- called by ------------------------------------------------------
!||    r8ke3    ../engine/source/elements/spring/r8ke3.F
!||====================================================================
      SUBROUTINE R8CHK3(JFT    ,JLT    ,NGL    ,AL     ,OFF    ,
     1                  KX     ,KY     ,KZ     ,MX     ,MY     ,
     2                  MZ     ,R11    ,R12    ,R13    ,R21    ,
     3                  R22    ,R23    ,R31    ,R32    ,R33    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
#include      "comlock.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      INTEGER NGL(*)
      my_real
     .    AL(3,*),OFF(*),
     .    KX(*),KY(*),KZ(*),
     .    MX(*),MY(*),MZ(*),
     .    R11(*),R12(*),R13(*),
     .    R21(*),R22(*),R23(*),
     .    R31(*),R32(*),R33(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .   SCAL,ALX,ALY,ALZ,SX,SY,SZ,S1,S2
C-----------------------------------------------
      IF (IMP_CHK > 0) THEN
       SCAL = HUNDRED
       DO I=JFT,JLT
        IF(OFF(I)/=ZERO)THEN
         ALX = ABS(AL(1,I))*SCAL
         ALY = ABS(AL(2,I))*SCAL
         ALZ = ABS(AL(2,I))*SCAL
         S1 = KY(I)*ALX-MZ(I)
         S2 = KZ(I)*ALX-MY(I)
         SX = MAX(S1,S2)
         S1 = KX(I)*ALY-MZ(I)
         S2 = KZ(I)*ALY-MX(I)
         SY = MAX(S1,S2)
         S1 = KX(I)*ALZ-MY(I)
         S2 = KY(I)*ALZ-MX(I)
         SZ = MAX(S1,S2)
         IF(SX>ZERO.OR.SY>ZERO.OR.SZ>ZERO)THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IW = IMP_IW + 1
         ENDIF
        ENDIF
       ENDDO
      ENDIF
C
      RETURN
 2001 FORMAT(/'***WARNING :SPRING TYPE8 NON-ZERO LENGTH : ELEMENT NB:',
     .          I8/)
      END
