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
!||    fxbsini     ../starter/source/constraints/fxbody/fxbsini.F
!||--- called by ------------------------------------------------------
!||    initia      ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||    fsigcini    ../starter/source/constraints/fxbody/fsigcini.F
!||    fsigpini    ../starter/source/constraints/fxbody/fsigpini.F
!||    fsigsini    ../starter/source/constraints/fxbody/fsigsini.F
!||    fsigtini    ../starter/source/constraints/fxbody/fsigtini.F
!||    fsigtrini   ../starter/source/constraints/fxbody/fsigtrini.F
!||====================================================================
      SUBROUTINE FXBSINI(FXBELM, FXBSIG, NELS , NELC  , NELTG,
     .                   X     , IPARG , PM   , FXBMOD, NML  ,
     .                   NSN   , IXS   , IXC  , IXTG  , GEO  ,
     .                   FXBRPM, NFX   , IFILE, LVSIG , NSNI ,
     .                   NME   , IRCS  , IRCM0, NELT  , NELP ,
     .                   IXT   , IXP   ,IBEAM_VECTOR,RBEAM_VECTOR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBELM(*), NELS, NELC, NELTG, IPARG(NPARG,*), NML, 
     .        NSN, IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*), NFX, IFILE,
     .        LVSIG, NSNI, NME, IRCS, IRCM0, NELT, NELP, IXT(NIXT,*),
     .        IXP(NIXP,*)
      INTEGER, INTENT (IN   ) :: IBEAM_VECTOR(NELP)
      my_real
     .        FXBSIG(*), X(3,*), PM(NPROPM,*), FXBMOD(*),
     .        GEO(NPROPG,*), FXBRPM(*)
      my_real, INTENT (IN   ) :: RBEAM_VECTOR(3,NELP)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IM,IADMOD,IADEL,IADSIG, IADM, IADS, IRCM, IAD, J
      my_real
     .        RINI(3,3), VMOD(NSN*6), VSIG(LVSIG), VV(6)
C=======================================================================
      IRCM=IRCM0
      IRCM=IRCM+NME*(NSN-NSNI)
C      
      DO I=1,3
         DO II=1,3
            RINI(I,II)=FXBRPM(1+(I-1)*3+II)
         ENDDO
      ENDDO
C
      IADSIG=1
      DO IM=1,NML
        IF (IFILE==0) THEN
           IADMOD=NSN*6*(IM-1)+1
           DO I=1,NSN*6
              VMOD(I)=FXBMOD(IADMOD+I-1)
           ENDDO
        ELSEIF (IFILE==1) THEN
           IADMOD=NSNI*6*(IM-1)+1
           DO I=1,NSNI*6
              VMOD(I)=FXBMOD(IADMOD+I-1)
           ENDDO
           IADM=NSNI*6
           DO I=1,NSN-NSNI
              IRCM=IRCM+1
              READ(IFXM,REC=IRCM) (VV(II),II=1,6)
              DO II=1,6
                 VMOD(IADM+II)=VV(II)
              ENDDO
              IADM=IADM+6
           ENDDO
        ENDIF
C   1. Solid elements
        IADEL=1
        IADS=1
        IF (NELS>0) CALL FSIGSINI(
     .     FXBELM(IADEL), IPARG, X,          PM,   IXS, 
     .     GEO,           VMOD,  VSIG(IADS), RINI, NELS)
C   2. 4-nodes shell elements
        IADEL=IADEL+NELS*13
        IADS=IADS+NELS*7             
        IF (NELC>0) CALL FSIGCINI(
     .     FXBELM(IADEL), IPARG, X,          PM,   IXC, 
     .     GEO,           VMOD,  VSIG(IADS), RINI, NELC)
C   3. Truss elements
        IADEL=IADEL+NELC*10
        IADS=IADS+NELC*10
        IF (NELT>0) CALL FSIGTRINI(
     .     FXBELM(IADEL), IPARG, X,         PM,   IXT,
     .     GEO,           VMOD,  VSIG(IADS),NELT)
C   4. Beam elements
        IADEL=IADEL+NELT*7
        IADS=IADS+NELT*2
        IF (NELP>0) CALL FSIGPINI(
     .     FXBELM(IADEL), IPARG, X,          PM,   IXP, 
     .     GEO,           VMOD,  VSIG(IADS), RINI, NELP,
     .     IBEAM_VECTOR ,RBEAM_VECTOR)
C   5. 3-nodes shell elements
        IADEL=IADEL+NELP*9
        IADS=IADS+NELP*8             
        IF (NELTG>0) CALL FSIGTINI(
     .     FXBELM(IADEL), IPARG, X,          PM,   IXTG, 
     .     GEO,           VMOD,  VSIG(IADS), RINI, NELTG)
        IF (IFILE==0) THEN
           DO I=1,LVSIG
              FXBSIG(IADSIG+I-1)=VSIG(I)
           ENDDO
        ELSE
           IAD=0
           DO I=1,LVSIG/6
              IRCS=IRCS+1
              WRITE(IFXS,REC=IRCS) (VSIG(IAD+J),J=1,6)
              IAD=IAD+6
           ENDDO
           II=LVSIG-(LVSIG/6)*6
           IF (II/=0) THEN
              IRCS=IRCS+1
              WRITE(IFXS,REC=IRCS) (VSIG(IAD+J),J=1,II),(ZERO,J=II+1,6)
           ENDIF
        ENDIF
        IADSIG=IADSIG+NELS*7+NELC*10+NELT*2+NELP*8+NELTG*10
      ENDDO
C-----------
      RETURN
      END SUBROUTINE FXBSINI 
      
      
            
