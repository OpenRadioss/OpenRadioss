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
Chd|  EIGOFF                        source/output/anim/generate/eigoff.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE EIGOFF(
     .            IXS      ,IXQ      ,IXC       ,IXT       ,IXP       ,
     .            IXR      ,IXTG     ,IBUF      ,NN        ,BUFEL     ,
     .            IPARG    ,BUFEL0   ,ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXT(NIXT,*), IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*),
     .        IBUF(*), NN, IPARG(NPARG,*)
      my_real
     .        BUFEL(*), BUFEL0(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NUMNOD), NG, MLW, ITY, NEL, NFT, IAD, NBX, II,
     .        NALL, IHBE, NPT, ISTRAIN, IEXPAN,ISROT
C=======================================================================
      DO I=1,LBUFEL
         BUFEL0(I)=BUFEL(I)
      ENDDO
      DO I=1,NUMNOD
         ITAG(I)=0
      ENDDO
      DO I=1,NN
         ITAG(IBUF(I))=1
      ENDDO
C
      DO NG=1,NGROUP
       MLW=IPARG(1,NG)
       ITY=IPARG(5,NG)
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       NBX=IPARG(4,NG) -1
       IF (ITY.EQ.1) THEN
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXS(2,II)) * ITAG(IXS(3,II)) *
     +          ITAG(IXS(4,II)) * ITAG(IXS(5,II)) *
     +          ITAG(IXS(6,II)) * ITAG(IXS(7,II)) *
     +          ITAG(IXS(8,II)) * ITAG(IXS(9,II)) 
           IF (NALL == 0) ELBUF_TAB(NG)%GBUF%OFF(I) = ZERO
        ENDDO
       ELSEIF (ITY.EQ.3) THEN
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXC(2,II)) * ITAG(IXC(3,II)) *
     +          ITAG(IXC(4,II)) * ITAG(IXC(5,II)) 
           IF (NALL == 0) ELBUF_TAB(NG)%GBUF%OFF(I) = ZERO
        ENDDO
       ELSEIF (ITY.EQ.7) THEN
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXTG(2,II)) * ITAG(IXTG(3,II)) *
     +          ITAG(IXTG(4,II))
           IF (NALL == 0) ELBUF_TAB(NG)%GBUF%OFF(I) = ZERO
        ENDDO
       ELSEIF (ITY.EQ.50) THEN
        DO I=1,NEL
         BUFEL0(NBX+I)=ZERO
        ENDDO
       ELSEIF (ITY.EQ.4) THEN
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXT(2,II)) * ITAG(IXT(3,II)) 
         IF (NALL == 0) ELBUF_TAB(NG)%GBUF%OFF(I) = ZERO
cc         IF (NALL.EQ.0) BUFEL0(NBX+I)=ZERO  
        ENDDO
       ELSEIF (ITY.EQ.5) THEN
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXP(2,II)) * ITAG(IXP(3,II)) 
         IF (NALL == 0) ELBUF_TAB(NG)%GBUF%OFF(I) = ZERO
cc         IF (NALL.EQ.0) BUFEL0(NBX+I)=ZERO  
        ENDDO
       ELSEIF (ITY.EQ.6) THEN             
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXR(2,II)) * ITAG(IXR(3,II)) 
         IF (NALL == 0) ELBUF_TAB(NG)%GBUF%OFF(I) = ZERO
cc         IF (NALL.EQ.0) BUFEL0(NBX+I)=ZERO 
        ENDDO
       ENDIF
      ENDDO
C
      RETURN
      END     
