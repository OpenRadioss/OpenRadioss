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
Chd|  DIM_TSHEDG                    source/elements/thickshell/solidec/scdtdc3.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_TSHEDG(ELBUF_STR,NEDG, IXS ,IPARG )
C=======================================================================
      USE ELBUFDEF_MOD     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEDG
      INTEGER IXS(NIXS,*),IPARG(NPARG,NGROUP)
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP), TARGET :: ELBUF_STR
C     REAL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG, NEL, NFT,N, ITY,JHBE,IGTYP,ICSTR,ISOLNOD
      INTEGER ITAG(NUMNOD),N1,N2,NC(8,MVSIZ)
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C     REAL
C-------
      ITAG(1:NUMNOD)=0
      NEDG = 0
      DO NG=1,NGROUP
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       ITY=IPARG(5,NG)
       ICSTR= IPARG(17,NG)
       JHBE = IPARG(23,NG)
       IGTYP  = IPARG(38,NG)
       ISOLNOD= IPARG(28,NG)
       IF (IPARG(8,NG)==1) CYCLE                                 
       IF (ITY /= 1) CYCLE
       IF (IGTYP == 20.OR.IGTYP == 21.OR.IGTYP == 22)THEN
         GBUF => ELBUF_STR(NG)%GBUF
C--------only linear for the moment       
        IF(ISOLNOD==6)THEN
          DO I=1,NEL
 	        N = NFT+I
            NC(1:3,I)=IXS(2:4,N)
            NC(4:6,I)=IXS(6:8,N)
          ENDDO
            DO I=1,NEL
              IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	          N1 = NC(1,I)
 	          N2 = NC(4,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(2,I)
 	          N2 = NC(5,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(3,I)
 	          N2 = NC(6,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
            ENDDO
        ELSEIF(ISOLNOD==8)THEN
          DO I=1,NEL
 	        N = NFT+I
            NC(1:8,I)=IXS(2:9,N)
          ENDDO
          IF (JHBE==14) THEN
             SELECT CASE (ICSTR)
              CASE(100)                                             
                DO I=1,NEL
                  IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	              N1 = NC(1,I)
 	              N2 = NC(4,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(2,I)
 	              N2 = NC(3,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(5,I)
 	              N2 = NC(8,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(6,I)
 	              N2 = NC(7,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
                ENDDO
              CASE(10)                                          
                DO I=1,NEL
                  IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	              N1 = NC(1,I)
 	              N2 = NC(5,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(2,I)
 	              N2 = NC(6,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(3,I)
 	              N2 = NC(7,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(4,I)
 	              N2 = NC(8,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
                ENDDO
              CASE(1)                                          
                DO I=1,NEL
                  IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	              N1 = NC(1,I)
 	              N2 = NC(2,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(4,I)
 	              N2 = NC(3,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(5,I)
 	              N2 = NC(6,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(8,I)
 	              N2 = NC(7,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
                ENDDO
             END SELECT 
          ELSEIF (JHBE==15) THEN
            DO I=1,NEL
              IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	          N1 = NC(1,I)
 	          N2 = NC(5,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(2,I)
 	          N2 = NC(6,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(3,I)
 	          N2 = NC(7,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(4,I)
 	          N2 = NC(8,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
            ENDDO
          END IF
        END IF
       END IF
      ENDDO 
C
      RETURN
      END
