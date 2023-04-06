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
Chd|  MODBUFEL                      source/constraints/fxbody/modbufel.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SCHREP                        source/constraints/fxbody/fsigsini.F
Chd|====================================================================
      SUBROUTINE MODBUFEL(FXBELM, FXBSIG , BUFEL, NELS  , NELC  ,
     .                    NELT  , NELP   , NELTG, FXBRPM, LBUFEL,
     .                    ASIG  , IFILE  , IRCS , LVSIG )
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBELM(*), NELS, NELC, NELT, NELP, NELTG, LBUFEL, ASIG,
     .        IFILE, IRCS, LVSIG
      my_real
     .        FXBSIG(*), BUFEL(*), FXBRPM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NFT, NFS, IG, OFFSET, LAST, MX, IAD1, IAD2,
     .        NFT1, IAD, II, IAD3
      my_real
     .        RT(9), VSIG(LVSIG), SIGL(6,MVSIZ), SIG(6,MVSIZ)
C
      DO I=1,3
         DO J=1,3
            RT(3*(J-1)+I)=FXBRPM(1+3*(I-1)+J)
         ENDDO
      ENDDO
C
      IF (IFILE==0) THEN
         DO I=1,LVSIG
            VSIG(I)=FXBSIG(I)
         ENDDO
         ASIG=ASIG+LVSIG
      ELSEIF (IFILE==1) THEN
         IAD=0
         DO I=1,LVSIG/6
            IRCS=IRCS+1
            READ(IFXS,REC=IRCS) (VSIG(IAD+J),J=1,6)
            IAD=IAD+6
         ENDDO
         II=LVSIG-(LVSIG/6)*6
         IF (II/=0) THEN
            IRCS=IRCS+1
            READ(IFXS,REC=IRCS) (VSIG(IAD+J),J=1,II)
         ENDIF
      ENDIF
C
      NFT=0
      NFS=0
      DO IG=1,NELS,MVSIZ
         OFFSET=IG-1
         LAST=MIN(MVSIZ,NELS-OFFSET)
         DO I=1,LAST
            MX=FXBELM(NFT+13)
            IAD1=FXBELM(NFT+11)
            IAD2=FXBELM(NFT+12)
            BUFEL(IAD1)=VSIG(NFS+1)
            BUFEL(IAD1+1)=VSIG(NFS+2)
            BUFEL(IAD1+2)=VSIG(NFS+3)
            BUFEL(IAD1+3)=VSIG(NFS+4)
            BUFEL(IAD1+4)=VSIG(NFS+5)
            BUFEL(IAD1+5)=VSIG(NFS+6)
            BUFEL(IAD2)=VSIG(NFS+7)
            NFT=NFT+13
            NFS=NFS+7
         ENDDO
      ENDDO
      DO IG=1,NELC,MVSIZ
         OFFSET=IG-1
         LAST=MIN(MVSIZ,NELC-OFFSET)
         DO I=1,LAST
            MX=FXBELM(NFT+10)
            IAD1=FXBELM(NFT+7)
            IAD2=FXBELM(NFT+8)
            IAD3=FXBELM(NFT+9)
            BUFEL(IAD1)=VSIG(NFS+1)
            BUFEL(IAD1+1)=VSIG(NFS+2)
            BUFEL(IAD1+2)=VSIG(NFS+3)
            BUFEL(IAD1+3)=VSIG(NFS+4)
            BUFEL(IAD1+4)=VSIG(NFS+5)
            BUFEL(IAD2)=VSIG(NFS+6)
            BUFEL(IAD2+1)=VSIG(NFS+7)
            BUFEL(IAD2+2)=VSIG(NFS+8)
            BUFEL(IAD3)=VSIG(NFS+9)
            BUFEL(IAD3+1)=VSIG(NFS+10)
            NFT=NFT+10
            NFS=NFS+10
         ENDDO
      ENDDO
      DO IG=1,NELT,MVSIZ
         OFFSET=IG-1
         LAST=MIN(MVSIZ,NELT-OFFSET)
         DO I=1,LAST
            MX=FXBELM(NFT+7)
            IAD1=FXBELM(NFT+5)
            IAD2=FXBELM(NFT+6)
            BUFEL(IAD1)=VSIG(NFS+1)
            BUFEL(IAD2)=VSIG(NFS+2)
            NFT=NFT+7
            NFS=NFS+2
         ENDDO
      ENDDO
      DO IG=1,NELP,MVSIZ
         OFFSET=IG-1
         LAST=MIN(MVSIZ,NELP-OFFSET)
         DO I=1,LAST
            MX=FXBELM(NFT+9)
            IAD1=FXBELM(NFT+6)
            IAD2=FXBELM(NFT+7)
            IAD3=FXBELM(NFT+8)
            BUFEL(IAD1)=VSIG(NFS+1)
            BUFEL(IAD1+1)=VSIG(NFS+2)
            BUFEL(IAD1+2)=VSIG(NFS+3)
            BUFEL(IAD2)=VSIG(NFS+4)
            BUFEL(IAD2+1)=VSIG(NFS+5)
            BUFEL(IAD2+2)=VSIG(NFS+6)
            BUFEL(IAD3)=VSIG(NFS+7)
            BUFEL(IAD3+1)=VSIG(NFS+8)
            NFT=NFT+9
            NFS=NFS+8
         ENDDO
      ENDDO
      DO IG=1,NELTG,MVSIZ
         OFFSET=IG-1
         LAST=MIN(MVSIZ,NELTG-OFFSET)
         DO I=1,LAST
            MX=FXBELM(NFT+9)
            IAD1=FXBELM(NFT+6)
            IAD2=FXBELM(NFT+7)
            IAD3=FXBELM(NFT+8)
            BUFEL(IAD1)=VSIG(NFS+1)
            BUFEL(IAD1+1)=VSIG(NFS+2)
            BUFEL(IAD1+2)=VSIG(NFS+3)
            BUFEL(IAD1+3)=VSIG(NFS+4)
            BUFEL(IAD1+4)=VSIG(NFS+5)
            BUFEL(IAD2)=VSIG(NFS+6)
            BUFEL(IAD2+1)=VSIG(NFS+7)
            BUFEL(IAD2+2)=VSIG(NFS+8)
            BUFEL(IAD3)=VSIG(NFS+9)
            BUFEL(IAD3+1)=VSIG(NFS+10)
            NFT=NFT+9
            NFS=NFS+10
         ENDDO
      ENDDO
C
      NFT=0
      DO IG=1,NELS,MVSIZ
         OFFSET=IG-1
         LAST=MIN(MVSIZ,NELS-OFFSET)
         NFT1=NFT
         DO I=1,LAST
            IAD1=FXBELM(NFT+11)
            SIGL(1,I)=BUFEL(IAD1) 
            SIGL(2,I)=BUFEL(IAD1+1) 
            SIGL(3,I)=BUFEL(IAD1+2) 
            SIGL(4,I)=BUFEL(IAD1+3) 
            SIGL(5,I)=BUFEL(IAD1+4) 
            SIGL(6,I)=BUFEL(IAD1+5)
            NFT=NFT+13
         ENDDO
         CALL SCHREP(LAST,SIGL,SIG,RT)
         NFT=NFT1
         DO I=1,LAST
            IAD1=FXBELM(NFT+11)
            BUFEL(IAD1)=SIG(1,I)
            BUFEL(IAD1+1)=SIG(2,I) 
            BUFEL(IAD1+2)=SIG(3,I) 
            BUFEL(IAD1+3)=SIG(4,I) 
            BUFEL(IAD1+4)=SIG(5,I) 
            BUFEL(IAD1+5)=SIG(6,I)
            NFT=NFT+13
         ENDDO
      ENDDO   
C
      RETURN
      END SUBROUTINE MODBUFEL
           
