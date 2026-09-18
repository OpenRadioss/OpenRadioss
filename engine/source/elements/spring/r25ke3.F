Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you:
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    r25ke3   ../engine/source/elements/spring/r25ke3.F
!||--- called by ------------------------------------------------------
!||    imp_glob_k     ../engine/source/implicit/imp_glob_k.F
!||    imp_glob_k0    ../engine/source/implicit/imp_glob_k.F
!||--- calls      -----------------------------------------------------
!||    r4coork3, r25mat3, r13sumg3, pkeg3, peoff, assem_p
!||====================================================================
      SUBROUTINE R25KE3(JFT    ,JLT   ,NEL   ,MTN    ,PM     ,
     1                  GEO    ,IXR   ,X     ,ELBUF_TAB,NPF   ,
     2                  TF     ,SKEW  ,OFFSET,FR_WAVE,IKGEO   ,IGEO,
     3                  ETAG   ,IDDL  ,NDOF  ,K_DIAG ,K_LT    ,
     4                  IADK   ,JDIK  )
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE ELEMENT_MOD, ONLY : NIXR
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
      INTEGER IXR(NIXR,*),NPF(*),JFT,JLT,OFFSET,NEL,MTN,IKGEO
      INTEGER IGEO(*),ETAG(*),IDDL(*),NDOF(*),IADK(*),JDIK(*)
      my_real
     .   GEO(NPROPG,*),X(*),TF(*),SKEW(LSKEW,*),FR_WAVE(*),PM(*),
     .   KE11(36,MVSIZ),KE12(36,MVSIZ),KE22(36,MVSIZ),
     .   OFF(MVSIZ),K_DIAG(*),K_LT(*),
     .   AL(MVSIZ),KX(MVSIZ),KY(MVSIZ),KZ(MVSIZ),
     .   MX(MVSIZ),MY(MVSIZ),MZ(MVSIZ),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
      INTEGER PID(MVSIZ),NGL(MVSIZ),I,II(3)
      TYPE(ELBUF_STRUCT_),TARGET :: ELBUF_TAB
      TYPE(G_BUFEL_),POINTER :: GBUF
C-----------------------------------------------
      GBUF=>ELBUF_TAB%GBUF
      DO I=1,3
        II(I)=(I-1)*NEL+1
      ENDDO
C
      CALL R4COORK3(JFT,JLT,X,IXR,PID,NGL,AL,
     .              R11,R12,R13,R21,R22,R23,R31,R32,R33,
     .              GBUF%SKEW,GBUF%OFF,OFF)
      CALL R25MAT3(JFT,JLT,GEO,KX,KY,KZ,MX,MY,MZ,PID,
     .             GBUF%LENGTH(II(1)),
     .             GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),
     .             GBUF%TOTDEPL(II(1)),GBUF%TOTDEPL(II(2)),
     .             GBUF%TOTDEPL(II(3)),
     .             GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),
     .             GBUF%TOTROT(II(1)),GBUF%TOTROT(II(2)),
     .             GBUF%TOTROT(II(3)),TF,NPF,
     .             GBUF%POSX,GBUF%POSY,GBUF%POSZ,
     .             GBUF%POSXX,GBUF%POSYY,GBUF%POSZZ,IGEO)
C
      CALL R13SUMG3(JFT,JLT,AL,KX,KY,KZ,MX,MY,MZ,
     .              R11,R12,R13,R21,R22,R23,R31,R32,R33,
     .              KE11,KE12,KE22)
      IF(IKGEO==1) CALL PKEG3(JFT,JLT,GBUF%FOR(II(1)),AL,
     .                        KE11,KE12,KE22)
      IF(NEIG>0) CALL PEOFF(NIXR,JFT,JLT,IXR,ETAG,OFF)
      CALL ASSEM_P(NIXR,6,IXR,NEL,IDDL,NDOF,K_DIAG,K_LT,
     .             IADK,JDIK,KE11,KE12,KE22,OFF)
C
      RETURN
      END
