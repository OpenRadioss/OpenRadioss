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
Chd|  FXBELNUM                      source/constraints/fxbody/fxbelnum.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FXBELNUM(
     .              FXBNOD , NSN, IPARG, ITAG  , FXBELM,
     .              IXS    , IXC, IXTG , IPARTS, IPARTC,
     .              IPARTTG, IXT, IXP  , IPARTT, IPARTP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBNOD(*), NSN, IPARG(NPARG,*), ITAG(*), FXBELM(*),
     .        IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*), IPARTS(*),
     .        IPARTC(*), IPARTTG(*), IXT(NIXT,*), IXP(NIXP,*),
     .        IPARTT(*), IPARTP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NG,NEL,NFT,ITY,II,NALL,NEL2,J,IAD,MAT,JHBE,
     .        IT1,IT2,IT3,IT4,IT5,IT6,IT7,IT8,JSROT
C=======================================================================
      DO I=1,NUMNOD
         ITAG(I)=0
      ENDDO
      DO I=1,NSN
         ITAG(ABS(FXBNOD(I)))=I
      ENDDO
      NEL2=0
C
      DO NG=1,NGROUP
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       IAD=IPARG(4,NG)
       ITY=IPARG(5,NG)
       JHBE=IPARG(23,NG)
C     1. Solid elements
       IF(ITY == 1)THEN
        DO I=1,NEL
         II=I+NFT
         IT1=MIN(1,ITAG(IXS(2,II)))
         IT2=MIN(1,ITAG(IXS(3,II)))
         IT3=MIN(1,ITAG(IXS(4,II)))
         IT4=MIN(1,ITAG(IXS(5,II)))
         IT5=MIN(1,ITAG(IXS(6,II)))
         IT6=MIN(1,ITAG(IXS(7,II)))
         IT7=MIN(1,ITAG(IXS(8,II)))
         IT8=MIN(1,ITAG(IXS(9,II)))
         NALL = IT1 * IT2 * IT3 * IT4 * IT5 * IT6 * IT7 * IT8 
         IF (NALL>0) THEN
            MAT=IXS(1,II)
            FXBELM(NEL2+1)=NG
            FXBELM(NEL2+2)=I
            DO J=1,8
               FXBELM(NEL2+2+J)=ITAG(IXS(1+J,II))
            ENDDO
            FXBELM(NEL2+13)=IPARTS(II)
            NEL2=NEL2+13
         ENDIF
        ENDDO
C     3. 4-nodes shell elements
       ELSEIF(ITY == 3)THEN
        DO I=1,NEL
         II=I+NFT
         IT1=MIN(1,ITAG(IXC(2,II)))
         IT2=MIN(1,ITAG(IXC(3,II)))
         IT3=MIN(1,ITAG(IXC(4,II)))
         IT4=MIN(1,ITAG(IXC(5,II)))
         NALL = IT1 * IT2 * IT3 * IT4
         IF (NALL>0) THEN
            FXBELM(NEL2+1)=NG
            FXBELM(NEL2+2)=I
            DO J=1,4
               FXBELM(NEL2+2+J)=ITAG(IXC(1+J,II))
            ENDDO
            FXBELM(NEL2+10)=IPARTC(II)
            NEL2=NEL2+10
         ENDIF
        ENDDO
C     4. Truss elements
       ELSEIF (ITY == 4) THEN
        DO I=1,NEL
         II=I+NFT
         IT1=MIN(1,ITAG(IXT(2,II)))
         IT2=MIN(1,ITAG(IXT(3,II)))
         NALL = IT1 * IT2
         IF (NALL>0) THEN
            NB1=IAD
            NB2=NB1+NEL
            FXBELM(NEL2+1)=NG
            FXBELM(NEL2+2)=I
            DO J=1,2
               FXBELM(NEL2+2+J)=ITAG(IXT(1+J,II))
            ENDDO
            FXBELM(NEL2+5)=NB1+I-1
            FXBELM(NEL2+6)=NB2+I-1
            FXBELM(NEL2+7)=IPARTT(II)
            NEL2=NEL2+7
         ENDIF
        ENDDO
C     5. Beam elements
       ELSEIF (ITY == 5) THEN
        DO I=1,NEL
         II=I+NFT
         IT1=MIN(1,ITAG(IXP(2,II)))
         IT2=MIN(1,ITAG(IXP(3,II)))
         NALL = IT1 * IT2
         IF (NALL>0) THEN
            NB1=IAD
            NB2=NB1+NEL
            NB3=NB2+NEL*3
            NB4=NB3+NEL*3
            NB5=NB4+NEL*2
            NB6=NB5+NEL
            NB7=NB6+NEL*3
            FXBELM(NEL2+1)=NG
            FXBELM(NEL2+2)=I
            DO J=1,3
               FXBELM(NEL2+2+J)=ITAG(IXP(1+J,II))
            ENDDO
            FXBELM(NEL2+6)=NB2+3*(I-1)
            FXBELM(NEL2+7)=NB3+3*(I-1)
            FXBELM(NEL2+8)=NB4+2*(I-1)
            FXBELM(NEL2+9)=IPARTP(II)
            NEL2=NEL2+9
         ENDIF
        ENDDO
C     7. 3-nodes shell elements
       ELSEIF(ITY == 7)THEN
        DO I=1,NEL
         II=I+NFT
         IT1=MIN(1,ITAG(IXTG(2,II)))
         IT2=MIN(1,ITAG(IXTG(3,II)))
         IT3=MIN(1,ITAG(IXTG(4,II)))
         NALL = IT1 * IT2 * IT3
         IF (NALL>0) THEN
            FXBELM(NEL2+1)=NG
            FXBELM(NEL2+2)=I
            DO J=1,3
               FXBELM(NEL2+2+J)=ITAG(IXTG(1+J,II))
            ENDDO
            FXBELM(NEL2+9)=IPARTTG(II)
            NEL2=NEL2+9
         ENDIF
        ENDDO
       ENDIF
      ENDDO
C-----------
      RETURN
      END SUBROUTINE FXBELNUM
