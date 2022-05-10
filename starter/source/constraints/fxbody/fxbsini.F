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
Chd|  FXBSINI                       source/constraints/fxbody/fxbsini.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        FSIGCINI                      source/constraints/fxbody/fsigcini.F
Chd|        FSIGPINI                      source/constraints/fxbody/fsigpini.F
Chd|        FSIGSINI                      source/constraints/fxbody/fsigsini.F
Chd|        FSIGTINI                      source/constraints/fxbody/fsigtini.F
Chd|        FSIGTRINI                     source/constraints/fxbody/fsigtrini.F
Chd|====================================================================
      SUBROUTINE FXBSINI(FXBELM, FXBSIG, NELS , NELC  , NELTG,
     .                   X     , IPARG , PM   , FXBMOD, NML  ,
     .                   NSN   , IXS   , IXC  , IXTG  , GEO  ,
     .                   FXBRPM, NFX   , IFILE, LVSIG , NSNI ,
     .                   NME   , IRCS  , IRCM0, NELT  , NELP ,
     .                   IXT   , IXP   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBELM(*), NELS, NELC, NELTG, IPARG(NPARG,*), NML, 
     .        NSN, IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*), NFX, IFILE,
     .        LVSIG, NSNI, NME, IRCS, IRCM0, NELT, NELP, IXT(NIXT,*),
     .        IXP(NIXP,*)
      my_real
     .        FXBSIG(*), X(3,*), PM(NPROPM,*), FXBMOD(*),
     .        GEO(NPROPG,*), FXBRPM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IM,IADMOD,IADEL,IADSIG, IADM, IADS, IRCM, IAD, J
      my_real
     .        RINI(3,3), VMOD(NSN*6), VSIG(LVSIG), VV(6)
c=======================================================================
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
        IF (IFILE.EQ.0) THEN
           IADMOD=NSN*6*(IM-1)+1
           DO I=1,NSN*6
              VMOD(I)=FXBMOD(IADMOD+I-1)
           ENDDO
        ELSEIF (IFILE.EQ.1) THEN
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
C   1. TRAITEMENT DES SOLIDES
        IADEL=1
        IADS=1
        IF (NELS.GT.0) CALL FSIGSINI(
     .     FXBELM(IADEL), IPARG, X,          PM,   IXS, 
     .     GEO,           VMOD,  VSIG(IADS), RINI, NELS)
C   2. TRAITEMENT DES COQUES 4 NOEUDS
        IADEL=IADEL+NELS*13
        IADS=IADS+NELS*7             
        IF (NELC.GT.0) CALL FSIGCINI(
     .     FXBELM(IADEL), IPARG, X,          PM,   IXC, 
     .     GEO,           VMOD,  VSIG(IADS), RINI, NELC)
C   3. TRAITEMENT DES BARRES
        IADEL=IADEL+NELC*10
        IADS=IADS+NELC*10
        IF (NELT.GT.0) CALL FSIGTRINI(
     .     FXBELM(IADEL), IPARG, X,         PM,   IXT,
     .     GEO,           VMOD,  VSIG(IADS),NELT)
C   4. TRAITEMENT DES POUTRES
        IADEL=IADEL+NELT*7
        IADS=IADS+NELT*2
        IF (NELP.GT.0) CALL FSIGPINI(
     .     FXBELM(IADEL), IPARG, X,          PM,   IXP, 
     .     GEO,           VMOD,  VSIG(IADS), RINI, NELP)
C   5. TRAITEMENT DES COQUES 3 NOEUDS
        IADEL=IADEL+NELP*9
        IADS=IADS+NELP*8             
        IF (NELTG.GT.0) CALL FSIGTINI(
     .     FXBELM(IADEL), IPARG, X,          PM,   IXTG, 
     .     GEO,           VMOD,  VSIG(IADS), RINI, NELTG)
        IF (IFILE.EQ.0) THEN
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
           IF (II.NE.0) THEN
              IRCS=IRCS+1
              WRITE(IFXS,REC=IRCS) (VSIG(IAD+J),J=1,II),(ZERO,J=II+1,6)
           ENDIF
        ENDIF
        IADSIG=IADSIG+NELS*7+NELC*10+NELT*2+NELP*8+NELTG*10
      ENDDO
C-----------
      RETURN
      END     
      
      
            
