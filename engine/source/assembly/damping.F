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
Chd|  DAMPING51                     source/assembly/damping.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE DAMPING51(
     1  NODFT  ,NODLT          ,DIM    ,V      ,
     2  VR     ,A      ,AR     ,MS     ,IN     ,
     3  DAMPR  ,DAMP   ,IGRNOD ,WEIGHT ,TAGSLV_RBY,
     4  SKEW   ,ICONTACT, I_DAMP_RDOF_TAB ,NODXI_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "sms_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .   NODFT,NODLT,DIM,ITASK,
     .   WEIGHT(*),ICONTACT(*),I_DAMP_RDOF_TAB(*),NODXI_SMS(*), TAGSLV_RBY(*)
C     REAL
      my_real
     .   V(3,*), VR(3,*), A(3,*), AR(3,*) ,MS(*), IN(*),
     .   DAMPR(NRDAMP,*), DAMP(DIM,*), SKEW(LSKEW,*)
      my_real,
     .   DIMENSION(3,NUMNOD) :: VSKW,ASKW,DAMPSKW
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ND,IGR,ISK
C     REAL
      my_real
     .   AA,DA,DW,BETASDT,OMEGA,FACTB,DAMPT,D_TSTART,D_TSTOP
C-----------------------------------------------
C     C = a M + b K
C======================================================================|

      IF(IDTMINS==2.OR.IDTMINS_INT/=0)GOTO 1000

C-----------------------------------------------
      DW = ZERO
      DO ND=1,NDAMP
        IGR   = NINT(DAMPR(2,ND))
        ISK   = NINT(DAMPR(15,ND))
        FACTB = DAMPR(16,ND)
        DAMPT  = MIN(DT1,DT2)*FACTB
        D_TSTART = DAMPR(17,ND)
        D_TSTOP  = DAMPR(18,ND)
       IF (TT>=D_TSTART .AND. TT<=D_TSTOP) THEN
        IF(ISK<=1)THEN
C----- Damping sur dof rotation et seulement -----
          IF (DAMPR(19,ND)>0) GOTO 250
C-------------------------------------------------
          DAMPA = DAMPR(3,ND)
          DAMPB = DAMPR(4,ND)
          BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
          OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
          DO N=1,IGRNOD(IGR)%NENTITY
           I=IGRNOD(IGR)%ENTITY(N)
           IF(TAGSLV_RBY(I)/=0) CYCLE
           DA = A(1,I) - DAMPA*V(1,I) - BETASDT *(A(1,I) - DAMP(1,I))
           DA = DA * OMEGA - A(1,I)
           DAMP(1,I) = A(1,I)
           A(1,I)    = A(1,I) + DA
           DW =DW+MS(I)*DA*(V(1,I)+HALF*A(1,I)*DT1)*DT12*WEIGHT(I)
          ENDDO
          DAMPA = DAMPR(5,ND)
          DAMPB = DAMPR(6,ND)
          BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
          OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
          DO N=1,IGRNOD(IGR)%NENTITY
           I=IGRNOD(IGR)%ENTITY(N)
           IF(TAGSLV_RBY(I)/=0) CYCLE
           DA = A(2,I) - DAMPA*V(2,I) - BETASDT *(A(2,I) - DAMP(2,I))
           DA = DA * OMEGA - A(2,I)
           DAMP(2,I) = A(2,I)
           A(2,I)    = A(2,I) + DA
           DW =DW+MS(I)*DA*(V(2,I)+HALF*A(2,I)*DT1)*DT12*WEIGHT(I)
          ENDDO
          DAMPA = DAMPR(7,ND)
          DAMPB = DAMPR(8,ND)
          BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
          OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
          DO N=1,IGRNOD(IGR)%NENTITY
           I=IGRNOD(IGR)%ENTITY(N)
           IF(TAGSLV_RBY(I)/=0) CYCLE
           DA = A(3,I) - DAMPA*V(3,I) - BETASDT *(A(3,I) - DAMP(3,I))
           DA = DA * OMEGA - A(3,I)
           DAMP(3,I) = A(3,I)
           A(3,I)    = A(3,I) + DA
           DW =DW+MS(I)*DA*(V(3,I)+HALF*A(3,I)*DT1)*DT12*WEIGHT(I)
          ENDDO
250       CONTINUE
          IF(IRODDL/=0)THEN
           DAMPA = DAMPR(9,ND)
           DAMPB = DAMPR(10,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN
              IF (ICONTACT(I)/=0) I_DAMP_RDOF_TAB(I) = DAMPR(19,ND)
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 350
	    ENDIF
C--------------------------------------------------------
            DA = AR(1,I) - DAMPA*VR(1,I)
     .         - BETASDT *(AR(1,I)-DAMP(4,I))
            DA = DA * OMEGA - AR(1,I)
            DAMP(4,I) = AR(1,I)
            AR(1,I)	= AR(1,I) + DA
            DW = DW+IN(I)*DA*(VR(1,I)+HALF*AR(1,I)*DT1)*DT12*WEIGHT(I)
350         CONTINUE
           ENDDO
           DAMPA = DAMPR(11,ND)
           DAMPB = DAMPR(12,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 450
	    ENDIF
C--------------------------------------------------------
            DA = AR(2,I) - DAMPA*VR(2,I)
     .         - BETASDT *(AR(2,I)-DAMP(5,I))
            DA = DA * OMEGA - AR(2,I)
            DAMP(5,I) = AR(2,I)
            AR(2,I)	= AR(2,I) + DA
            DW = DW+IN(I)*DA*(VR(2,I)+HALF*AR(2,I)*DT1)*DT12*WEIGHT(I)
450         CONTINUE
           ENDDO
           DAMPA = DAMPR(13,ND)
           DAMPB = DAMPR(14,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 550
	      I_DAMP_RDOF_TAB(I)=I_DAMP_RDOF_TAB(I)-1
	    ENDIF
C--------------------------------------------------------
            DA = AR(3,I) - DAMPA*VR(3,I)
     .         - BETASDT *(AR(3,I)-DAMP(6,I))
            DA = DA * OMEGA - AR(3,I)
            DAMP(6,I) = AR(3,I)
            AR(3,I)	= AR(3,I) + DA
            DW = DW+IN(I)*DA*(VR(3,I)+HALF*AR(3,I)*DT1)*DT12*WEIGHT(I)
550         CONTINUE
           ENDDO
          END IF
        ELSE
C----- Damping sur dof rotation et seulement -----
          IF (DAMPR(19,ND)>0) GOTO 650
C-------------------------------------------------
#include "vectorize.inc"
          DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            VSKW(1,I)= SKEW(1,ISK)*V(1,I)
     .                +SKEW(2,ISK)*V(2,I)
     .                +SKEW(3,ISK)*V(3,I)
            VSKW(2,I)= SKEW(4,ISK)*V(1,I)
     .                +SKEW(5,ISK)*V(2,I)
     .                +SKEW(6,ISK)*V(3,I)
            VSKW(3,I)= SKEW(7,ISK)*V(1,I)
     .                +SKEW(8,ISK)*V(2,I)
     .                +SKEW(9,ISK)*V(3,I)
            ASKW(1,I)= SKEW(1,ISK)*A(1,I)
     .                +SKEW(2,ISK)*A(2,I)
     .                +SKEW(3,ISK)*A(3,I)
            ASKW(2,I)= SKEW(4,ISK)*A(1,I)
     .                +SKEW(5,ISK)*A(2,I)
     .                +SKEW(6,ISK)*A(3,I)
            ASKW(3,I)= SKEW(7,ISK)*A(1,I)
     .                +SKEW(8,ISK)*A(2,I)
     .                +SKEW(9,ISK)*A(3,I)
            DAMPSKW(1,I)= SKEW(1,ISK)*DAMP(1,I)
     .                   +SKEW(2,ISK)*DAMP(2,I)
     .                   +SKEW(3,ISK)*DAMP(3,I)
            DAMPSKW(2,I)= SKEW(4,ISK)*DAMP(1,I)
     .                   +SKEW(5,ISK)*DAMP(2,I)
     .                   +SKEW(6,ISK)*DAMP(3,I)
            DAMPSKW(3,I)= SKEW(7,ISK)*DAMP(1,I)
     .                   +SKEW(8,ISK)*DAMP(2,I)
     .                   +SKEW(9,ISK)*DAMP(3,I)
          END DO
          DAMPA = DAMPR(3,ND)
          DAMPB = DAMPR(4,ND)
          BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
          OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
          DO N=1,IGRNOD(IGR)%NENTITY
           I=IGRNOD(IGR)%ENTITY(N)
           IF(TAGSLV_RBY(I)/=0) CYCLE
           DA = ASKW(1,I) - DAMPA*VSKW(1,I)
     .                   - BETASDT *(ASKW(1,I) - DAMPSKW(1,I))
           DA = DA * OMEGA - ASKW(1,I)
           DAMPSKW(1,I) = ASKW(1,I)
           ASKW(1,I)    = ASKW(1,I) + DA
           DW =DW
     .        +MS(I)*DA*(VSKW(1,I)+HALF*ASKW(1,I)*DT1)*DT12*WEIGHT(I)
          ENDDO
          DAMPA = DAMPR(5,ND)
          DAMPB = DAMPR(6,ND)
          BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
          OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
         DO N=1,IGRNOD(IGR)%NENTITY
           I=IGRNOD(IGR)%ENTITY(N)
           IF(TAGSLV_RBY(I)/=0) CYCLE
           DA = ASKW(2,I) - DAMPA*VSKW(2,I)
     .                   - BETASDT *(ASKW(2,I) - DAMPSKW(2,I))
           DA = DA * OMEGA - ASKW(2,I)
           DAMPSKW(2,I) = ASKW(2,I)
           ASKW(2,I)    = ASKW(2,I) + DA
           DW =DW
     .        +MS(I)*DA*(VSKW(2,I)+HALF*ASKW(2,I)*DT1)*DT12*WEIGHT(I)
          ENDDO
          DAMPA = DAMPR(7,ND)
          DAMPB = DAMPR(8,ND)
          BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
          OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
          DO N=1,IGRNOD(IGR)%NENTITY
           I=IGRNOD(IGR)%ENTITY(N)
           IF(TAGSLV_RBY(I)/=0) CYCLE
           DA = ASKW(3,I) - DAMPA*VSKW(3,I)
     .                   - BETASDT *(ASKW(3,I) - DAMPSKW(3,I))
           DA = DA * OMEGA - ASKW(3,I)
           DAMPSKW(3,I) = ASKW(3,I)
           ASKW(3,I)    = ASKW(3,I) + DA
           DW =DW
     .        +MS(I)*DA*(VSKW(3,I)+HALF*ASKW(3,I)*DT1)*DT12*WEIGHT(I)
          ENDDO
#include "vectorize.inc"
          DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            A(1,I)= SKEW(1,ISK)*ASKW(1,I)
     .       	   +SKEW(4,ISK)*ASKW(2,I)
     .       	   +SKEW(7,ISK)*ASKW(3,I)
            A(2,I)= SKEW(2,ISK)*ASKW(1,I)
     .       	   +SKEW(5,ISK)*ASKW(2,I)
     .       	   +SKEW(8,ISK)*ASKW(3,I)
            A(3,I)= SKEW(3,ISK)*ASKW(1,I)
     .             +SKEW(6,ISK)*ASKW(2,I)
     .             +SKEW(9,ISK)*ASKW(3,I)
            DAMP(1,I)= SKEW(1,ISK)*DAMPSKW(1,I)
     .                +SKEW(4,ISK)*DAMPSKW(2,I)
     .                +SKEW(7,ISK)*DAMPSKW(3,I)
            DAMP(2,I)= SKEW(2,ISK)*DAMPSKW(1,I)
     .                +SKEW(5,ISK)*DAMPSKW(2,I)
     .                +SKEW(8,ISK)*DAMPSKW(3,I)
            DAMP(3,I)= SKEW(3,ISK)*DAMPSKW(1,I)
     .                +SKEW(6,ISK)*DAMPSKW(2,I)
     .                +SKEW(9,ISK)*DAMPSKW(3,I)
          END DO
650       CONTINUE	  
          IF(IRODDL/=0)THEN
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            VSKW(1,I)= SKEW(1,ISK)*VR(1,I)
     .                +SKEW(2,ISK)*VR(2,I)
     .                +SKEW(3,ISK)*VR(3,I)
            VSKW(2,I)= SKEW(4,ISK)*VR(1,I)
     .                +SKEW(5,ISK)*VR(2,I)
     .                +SKEW(6,ISK)*VR(3,I)
            VSKW(3,I)= SKEW(7,ISK)*VR(1,I)
     .                +SKEW(8,ISK)*VR(2,I)
     .                +SKEW(9,ISK)*VR(3,I)
            ASKW(1,I)= SKEW(1,ISK)*AR(1,I)
     .                +SKEW(2,ISK)*AR(2,I)
     .                +SKEW(3,ISK)*AR(3,I)
            ASKW(2,I)= SKEW(4,ISK)*AR(1,I)
     .                +SKEW(5,ISK)*AR(2,I)
     .                +SKEW(6,ISK)*AR(3,I)
            ASKW(3,I)= SKEW(7,ISK)*AR(1,I)
     .                +SKEW(8,ISK)*AR(2,I)
     .                +SKEW(9,ISK)*AR(3,I)
            DAMPSKW(1,I)= SKEW(1,ISK)*DAMP(4,I)
     .                   +SKEW(2,ISK)*DAMP(5,I)
     .                   +SKEW(3,ISK)*DAMP(6,I)
            DAMPSKW(2,I)= SKEW(4,ISK)*DAMP(4,I)
     .                   +SKEW(5,ISK)*DAMP(5,I)
     .                   +SKEW(6,ISK)*DAMP(6,I)
            DAMPSKW(3,I)= SKEW(7,ISK)*DAMP(4,I)
     .                   +SKEW(8,ISK)*DAMP(5,I)
     .                   +SKEW(9,ISK)*DAMP(6,I)
           END DO
           DAMPA = DAMPR(9,ND)
           DAMPB = DAMPR(10,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN
              IF (ICONTACT(I)/=0) I_DAMP_RDOF_TAB(I) = DAMPR(19,ND)	      
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 750	 	      	       
	    ENDIF
C--------------------------------------------------------	    
            DA = ASKW(1,I) - DAMPA*VSKW(1,I) 
     .                    - BETASDT *(ASKW(1,I) - DAMPSKW(1,I))
            DA = DA * OMEGA - ASKW(1,I)
            DAMPSKW(1,I) = ASKW(1,I)
            ASKW(1,I)    = ASKW(1,I) + DA
            DW =DW
     .        +IN(I)*DA*(VSKW(1,I)+HALF*ASKW(1,I)*DT1)*DT12*WEIGHT(I)
750         CONTINUE     
           ENDDO
           DAMPA = DAMPR(11,ND)
           DAMPB = DAMPR(12,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN	      
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 850	 	      	       
	    ENDIF
C--------------------------------------------------------	    
            DA = ASKW(2,I) - DAMPA*VSKW(2,I) 
     .                    - BETASDT *(ASKW(2,I) - DAMPSKW(2,I))
            DA = DA * OMEGA - ASKW(2,I)
            DAMPSKW(2,I) = ASKW(2,I)
            ASKW(2,I)    = ASKW(2,I) + DA
            DW =DW
     .        +IN(I)*DA*(VSKW(2,I)+HALF*ASKW(2,I)*DT1)*DT12*WEIGHT(I)
850         CONTINUE     
           ENDDO
           DAMPA = DAMPR(13,ND)
           DAMPB = DAMPR(14,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 950
	      I_DAMP_RDOF_TAB(I)=I_DAMP_RDOF_TAB(I)-1	      	       
	    ENDIF
C--------------------------------------------------------	    
            DA = ASKW(3,I) - DAMPA*VSKW(3,I) 
     .                    - BETASDT *(ASKW(3,I) - DAMPSKW(3,I))
            DA = DA * OMEGA - ASKW(3,I)
            DAMPSKW(3,I) = ASKW(3,I)
            ASKW(3,I)    = ASKW(3,I) + DA
            DW =DW
     .        +IN(I)*DA*(VSKW(3,I)+HALF*ASKW(3,I)*DT1)*DT12*WEIGHT(I)
950         CONTINUE     
           ENDDO
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            AR(1,I)= SKEW(1,ISK)*ASKW(1,I)
     .       	    +SKEW(4,ISK)*ASKW(2,I)
     .       	    +SKEW(7,ISK)*ASKW(3,I)
            AR(2,I)= SKEW(2,ISK)*ASKW(1,I)
     .       	    +SKEW(5,ISK)*ASKW(2,I)
     .       	    +SKEW(8,ISK)*ASKW(3,I)
            AR(3,I)= SKEW(3,ISK)*ASKW(1,I)
     .              +SKEW(6,ISK)*ASKW(2,I)
     .              +SKEW(9,ISK)*ASKW(3,I)
            DAMP(4,I)= SKEW(1,ISK)*DAMPSKW(1,I)
     .                +SKEW(4,ISK)*DAMPSKW(2,I)
     .                +SKEW(7,ISK)*DAMPSKW(3,I)
            DAMP(5,I)= SKEW(2,ISK)*DAMPSKW(1,I)
     .                +SKEW(5,ISK)*DAMPSKW(2,I)
     .                +SKEW(8,ISK)*DAMPSKW(3,I)
            DAMP(6,I)= SKEW(3,ISK)*DAMPSKW(1,I)
     .                +SKEW(6,ISK)*DAMPSKW(2,I)
     .                +SKEW(9,ISK)*DAMPSKW(3,I)
           END DO
          END IF
        END IF
       ENDIF
      ENDDO
C
#include "lockon.inc"
         TFEXT = TFEXT + DW
#include "lockoff.inc"
      RETURN
C-----------------------------------------------
C     AMS
C-----------------------------------------------
 1000 CONTINUE
      DW = ZERO
      DO ND=1,NDAMP
        IGR   = NINT(DAMPR(2,ND))
        ISK   = NINT(DAMPR(15,ND))
        FACTB = DAMPR(16,ND)
        DAMPT  = MIN(DT1,DT2)*FACTB
        D_TSTART = DAMPR(17,ND)
        D_TSTOP  = DAMPR(18,ND)
        IF (TT>=D_TSTART .AND. TT<=D_TSTOP) THEN
        IF(ISK<=1)THEN
C-------------------------------------------------
          IF(IRODDL/=0)THEN
           DAMPA = DAMPR(9,ND)
           DAMPB = DAMPR(10,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN
              IF (ICONTACT(I)/=0) I_DAMP_RDOF_TAB(I) = DAMPR(19,ND)
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 351
	    ENDIF
C--------------------------------------------------------
            DA = AR(1,I) - DAMPA*VR(1,I)
     .         - BETASDT *(AR(1,I)-DAMP(4,I))
            DA = DA * OMEGA - AR(1,I)
            DAMP(4,I) = AR(1,I)
            AR(1,I)	= AR(1,I) + DA
            DW = DW+IN(I)*DA*(VR(1,I)+HALF*AR(1,I)*DT1)*DT12*WEIGHT(I)
351         CONTINUE
           ENDDO
           DAMPA = DAMPR(11,ND)
           DAMPB = DAMPR(12,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 451
	    ENDIF
C--------------------------------------------------------
            DA = AR(2,I) - DAMPA*VR(2,I)
     .         - BETASDT *(AR(2,I)-DAMP(5,I))
            DA = DA * OMEGA - AR(2,I)
            DAMP(5,I) = AR(2,I)
            AR(2,I)	= AR(2,I) + DA
            DW = DW+IN(I)*DA*(VR(2,I)+HALF*AR(2,I)*DT1)*DT12*WEIGHT(I)
451         CONTINUE
           ENDDO
           DAMPA = DAMPR(13,ND)
           DAMPB = DAMPR(14,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 551
	      I_DAMP_RDOF_TAB(I)=I_DAMP_RDOF_TAB(I)-1
	    ENDIF
C--------------------------------------------------------
            DA = AR(3,I) - DAMPA*VR(3,I)
     .         - BETASDT *(AR(3,I)-DAMP(6,I))
            DA = DA * OMEGA - AR(3,I)
            DAMP(6,I) = AR(3,I)
            AR(3,I)	= AR(3,I) + DA
            DW = DW+IN(I)*DA*(VR(3,I)+HALF*AR(3,I)*DT1)*DT12*WEIGHT(I)
551         CONTINUE
           ENDDO
          END IF
        ELSE
          IF(IRODDL/=0)THEN
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            VSKW(1,I)= SKEW(1,ISK)*VR(1,I)
     .                +SKEW(2,ISK)*VR(2,I)
     .                +SKEW(3,ISK)*VR(3,I)
            VSKW(2,I)= SKEW(4,ISK)*VR(1,I)
     .                +SKEW(5,ISK)*VR(2,I)
     .                +SKEW(6,ISK)*VR(3,I)
            VSKW(3,I)= SKEW(7,ISK)*VR(1,I)
     .                +SKEW(8,ISK)*VR(2,I)
     .                +SKEW(9,ISK)*VR(3,I)
            ASKW(1,I)= SKEW(1,ISK)*AR(1,I)
     .                +SKEW(2,ISK)*AR(2,I)
     .                +SKEW(3,ISK)*AR(3,I)
            ASKW(2,I)= SKEW(4,ISK)*AR(1,I)
     .                +SKEW(5,ISK)*AR(2,I)
     .                +SKEW(6,ISK)*AR(3,I)
            ASKW(3,I)= SKEW(7,ISK)*AR(1,I)
     .                +SKEW(8,ISK)*AR(2,I)
     .                +SKEW(9,ISK)*AR(3,I)
            DAMPSKW(1,I)= SKEW(1,ISK)*DAMP(4,I)
     .                   +SKEW(2,ISK)*DAMP(5,I)
     .                   +SKEW(3,ISK)*DAMP(6,I)
            DAMPSKW(2,I)= SKEW(4,ISK)*DAMP(4,I)
     .                   +SKEW(5,ISK)*DAMP(5,I)
     .                   +SKEW(6,ISK)*DAMP(6,I)
            DAMPSKW(3,I)= SKEW(7,ISK)*DAMP(4,I)
     .                   +SKEW(8,ISK)*DAMP(5,I)
     .                   +SKEW(9,ISK)*DAMP(6,I)
           END DO
           DAMPA = DAMPR(9,ND)
           DAMPB = DAMPR(10,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN
              IF (ICONTACT(I)/=0) I_DAMP_RDOF_TAB(I) = DAMPR(19,ND)	      
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 751	 	      	       
	    ENDIF
C--------------------------------------------------------	    
            DA = ASKW(1,I) - DAMPA*VSKW(1,I) 
     .                    - BETASDT *(ASKW(1,I) - DAMPSKW(1,I))
            DA = DA * OMEGA - ASKW(1,I)
            DAMPSKW(1,I) = ASKW(1,I)
            ASKW(1,I)    = ASKW(1,I) + DA
            DW =DW
     .        +IN(I)*DA*(VSKW(1,I)+HALF*ASKW(1,I)*DT1)*DT12*WEIGHT(I)
751         CONTINUE     
           ENDDO
           DAMPA = DAMPR(11,ND)
           DAMPB = DAMPR(12,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN	      
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 851	 	      	       
	    ENDIF
C--------------------------------------------------------	    
            DA = ASKW(2,I) - DAMPA*VSKW(2,I) 
     .                    - BETASDT *(ASKW(2,I) - DAMPSKW(2,I))
            DA = DA * OMEGA - ASKW(2,I)
            DAMPSKW(2,I) = ASKW(2,I)
            ASKW(2,I)    = ASKW(2,I) + DA
            DW =DW
     .        +IN(I)*DA*(VSKW(2,I)+HALF*ASKW(2,I)*DT1)*DT12*WEIGHT(I)
851         CONTINUE     
           ENDDO
           DAMPA = DAMPR(13,ND)
           DAMPB = DAMPR(14,ND)
           BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
           OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
C----- Damping seulement pour les noeuds en contact avec temporisation -----
            IF (DAMPR(19,ND)/=0) THEN
              IF (I_DAMP_RDOF_TAB(I)==0) GOTO 951
	      I_DAMP_RDOF_TAB(I)=I_DAMP_RDOF_TAB(I)-1	      	       
	    ENDIF
C--------------------------------------------------------	    
            DA = ASKW(3,I) - DAMPA*VSKW(3,I) 
     .                    - BETASDT *(ASKW(3,I) - DAMPSKW(3,I))
            DA = DA * OMEGA - ASKW(3,I)
            DAMPSKW(3,I) = ASKW(3,I)
            ASKW(3,I)    = ASKW(3,I) + DA
            DW =DW
     .        +IN(I)*DA*(VSKW(3,I)+HALF*ASKW(3,I)*DT1)*DT12*WEIGHT(I)
951         CONTINUE     
           ENDDO
#include "vectorize.inc"
           DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            IF(TAGSLV_RBY(I)/=0) CYCLE
            AR(1,I)= SKEW(1,ISK)*ASKW(1,I)
     .       	    +SKEW(4,ISK)*ASKW(2,I)
     .       	    +SKEW(7,ISK)*ASKW(3,I)
            AR(2,I)= SKEW(2,ISK)*ASKW(1,I)
     .       	    +SKEW(5,ISK)*ASKW(2,I)
     .       	    +SKEW(8,ISK)*ASKW(3,I)
            AR(3,I)= SKEW(3,ISK)*ASKW(1,I)
     .              +SKEW(6,ISK)*ASKW(2,I)
     .              +SKEW(9,ISK)*ASKW(3,I)
            DAMP(4,I)= SKEW(1,ISK)*DAMPSKW(1,I)
     .                +SKEW(4,ISK)*DAMPSKW(2,I)
     .                +SKEW(7,ISK)*DAMPSKW(3,I)
            DAMP(5,I)= SKEW(2,ISK)*DAMPSKW(1,I)
     .                +SKEW(5,ISK)*DAMPSKW(2,I)
     .                +SKEW(8,ISK)*DAMPSKW(3,I)
            DAMP(6,I)= SKEW(3,ISK)*DAMPSKW(1,I)
     .                +SKEW(6,ISK)*DAMPSKW(2,I)
     .                +SKEW(9,ISK)*DAMPSKW(3,I)
           END DO
          END IF
        END IF
       ENDIF
      ENDDO
C
#include "lockon.inc"
         TFEXT = TFEXT + DW
#include "lockoff.inc"
C
      RETURN
      END
C
Chd|====================================================================
Chd|  DAMPING44                     source/assembly/damping.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE DAMPING44(
     1  NODFT  ,NODLT          ,DIM    ,V      ,
     2  VR     ,A      ,AR     ,MS     ,IN     ,
     3  DAMPR  ,DAMP   ,IGRNOD ,WEIGHT ,TAGSLV_RBY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .   NODFT,NODLT,DIM,ITASK,
     .   WEIGHT(*),TAGSLV_RBY(*)
C     REAL
      my_real
     .   V(3,*), VR(3,*), A(3,*), AR(3,*) ,MS(*), IN(*),
     .   DAMPR(4,*), DAMP(DIM,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ND,IGR
C     REAL
      my_real
     .   BETASDT,AA,DA,DW,OMEGA
C-----------------------------------------------
C     C = a M + b K
C======================================================================|
      DW = ZERO
      DO ND=1,NDAMP
        IGR   = NINT(DAMPR(2,ND))
        DAMPA = DAMPR(3,ND)
        DAMPB = DAMPR(4,ND)
        BETASDT= -MIN(DAMPB,DT1,DT2)*DT1/MAX(DT1*DT1,EM30)
        OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
        DO J=1,3
          IF(IRODDL==0)THEN
#include "vectorize.inc"
            DO N=1,IGRNOD(IGR)%NENTITY
              I=IGRNOD(IGR)%ENTITY(N)
              IF(TAGSLV_RBY(I)/=0) CYCLE
              DA = A(J,I) - DAMPA*V(J,I) - BETASDT *(A(J,I) - DAMP(J,I))
              DA = DA * OMEGA - A(J,I)
              DAMP(J,I) = A(J,I)
              A(J,I)    = A(J,I) + DA
              DW =DW+MS(I)*DA*(V(J,I)+HALF*A(J,I)*DT1)*DT12*WEIGHT(I)
            ENDDO
          ELSE
#include "vectorize.inc"
            DO N=1,IGRNOD(IGR)%NENTITY
              I=IGRNOD(IGR)%ENTITY(N)
              IF(TAGSLV_RBY(I)/=0) CYCLE
              DA = A(J,I) - DAMPA*V(J,I) - BETASDT *(A(J,I) - DAMP(J,I))
              DA = DA * OMEGA - A(J,I)
              DAMP(J,I) = A(J,I)
              A(J,I)    = A(J,I) + DA
              DW =DW+MS(I)*DA*(V(J,I)+HALF*A(J,I)*DT1)*DT12*WEIGHT(I)
              DA = AR(J,I) - DAMPA*VR(J,I)
     .           - BETASDT *(AR(J,I)-DAMP(J+3,I))
              DA = DA * OMEGA - AR(J,I)
              DAMP(J+3,I) = AR(J,I)
              AR(J,I)     = AR(J,I) + DA
              DW = DW+IN(I)*DA*(VR(J,I)+HALF*AR(J,I)*DT1)*DT12*WEIGHT(I)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
#include "lockon.inc"
         TFEXT = TFEXT + DW
#include "lockoff.inc"
C
      RETURN
      END
C--------------------------
C          DAMPING alpha M + beta K
C--------------------------
Chd|====================================================================
Chd|  DAMPING                       source/assembly/damping.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE DAMPING(NODFT,NODLT,V ,VR,A ,AR ,DAMP,MS,IN,
     .                   IGRNOD,DIM,ITASK,WEIGHT,TAGSLV_RBY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .   NODFT,NODLT,DIM,ITASK,
     .   WEIGHT(*),TAGSLV_RBY(*)
C     REAL
      my_real
     .   V(3,*) ,VR(3,*),A(3,*) ,AR(3,*) ,DAMP(DIM,*),MS(*),IN(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N
C     REAL
      my_real
     .   BETASDT,AA,DA,DW,OMEGA
C-----------------------------------------------
C
C     C = a M + b K
C
C
C     Fint(1)  = Fint0(0) + K v(1/2) dt1 + C v(1)
C     Fint0(1) = Fint0(0) + K v(1/2) dt1
C     Fint(1)  = Fint0(1) + C v(1)
C     Fint(1)  = Fint0(1) + [a M + b K] v(1)
C
C     a0(1) = a0(0) - K/M v(1/2) dt1
C     a(1)  = a0(1) - [a + b K/M] v(1)
C
C     a(1)  = a0(1) - a v(1) - b/dt1 K/M v(1) dt1
C     a(1)  = a0(1) - a (v(1/2) + a(1) dt1/2) - b K/M (v(1/2) + a(1) dt1/2)
C     a(1)  = a0(1) - a v(1/2) - a a(1) dt1/2 - b/dt1 [a0(1) - a0(0)]
C            - b K/M a(1) dt1/2
C
C     a(1)  ~= a0(1) - a v(1/2) - a a(1) dt1/2 - b/dt1 [a0(1) - a0(0)]
C     a(1)[1+a dt1/2] ~= a0(1) - a v(1/2) - b/dt1 [a0(1) - a0(0)]
C
C     a(1) = [a0(1) - a v(1/2) - b/dt1 [a0(1) - a0(0)]]/[1+a dt1/2]
C     da = a(1)-a0(1)
C     da = [a0(1) - a v(1/2) - b/dt1 [a0(1) - a0(0)]]/[1+a dt1/2] - a0(1)
C-----------------------------------------------
C
C     a(1) ~= [1-a dt1/2- b/dt1]a0(1) - a v(1/2) + b/dt1 a0(0)
C
C-----------------------------------------------

      DW = ZERO
C
      IF(DAMPB>=0.0)THEN
       BETASDT= -MIN(DAMPB,DT1,DT2)*DT1/MAX(DT1*DT1,EM30)
      ELSE
       BETASDT= DAMPB
      ENDIF
      OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
C
      IF(IDAMPG==0)THEN
       DO J=1,3
        IF(IRODDL==0)THEN
#include "vectorize.inc"
         DO I=NODFT,NODLT
          IF(TAGSLV_RBY(I)/=0) CYCLE
          DA = A(J,I) - DAMPA*V(J,I) - BETASDT *(A(J,I) - DAMP(J,I))
          DA = DA * OMEGA - A(J,I)
          DAMP(J,I) = A(J,I)
          A(J,I)    = A(J,I) + DA
          DW=DW+WEIGHT(I)*(MS(I)*DA*(V(J,I)+HALF*A(J,I)*DT1)*DT12)
         ENDDO
        ELSE
#include "vectorize.inc"
         DO I=NODFT,NODLT
          IF(TAGSLV_RBY(I)/=0) CYCLE
          DA = A(J,I) - DAMPA*V(J,I) - BETASDT *(A(J,I) - DAMP(J,I))
          DA = DA * OMEGA - A(J,I)
          DAMP(J,I) = A(J,I)
          A(J,I)    = A(J,I) + DA
          DW = DW+MS(I)*DA*(V(J,I)+HALF*A(J,I)*DT1)*DT12*WEIGHT(I)
          DA = AR(J,I) - DAMPA*VR(J,I) - BETASDT *(AR(J,I)-DAMP(J+3,I))
          DA = DA * OMEGA - AR(J,I)
          DAMP(J+3,I) = AR(J,I)
          AR(J,I)     = AR(J,I) + DA
          DW =DW+WEIGHT(I)*IN(I)*DA*(VR(J,I)+HALF*AR(J,I)*DT1)*DT12
         ENDDO
        ENDIF
       ENDDO
      ELSEIF(ITASK==0)THEN
       DO J=1,3
        IF(IRODDL==0)THEN
#include "vectorize.inc"
         DO N=1,IGRNOD(IDAMPG)%NENTITY
          I=IGRNOD(IDAMPG)%ENTITY(N)
          IF(TAGSLV_RBY(I)/=0) CYCLE
          DA = A(J,I) - DAMPA*V(J,I) - BETASDT *(A(J,I) - DAMP(J,I))
          DA = DA * OMEGA - A(J,I)
          DAMP(J,I) = A(J,I)
          A(J,I)    = A(J,I) + DA
          DW =DW+WEIGHT(I)*MS(I)*DA*(V(J,I)+HALF*A(J,I)*DT1)*DT12
         ENDDO
        ELSE
#include "vectorize.inc"
         DO N=1,IGRNOD(IDAMPG)%NENTITY
          I=IGRNOD(IDAMPG)%ENTITY(N)
          IF(TAGSLV_RBY(I)/=0) CYCLE
          DA = A(J,I) - DAMPA*V(J,I) - BETASDT *(A(J,I) - DAMP(J,I))
          DA = DA * OMEGA - A(J,I)
          DAMP(J,I) = A(J,I)
          A(J,I)    = A(J,I) + DA
          DW = DW+MS(I)*DA*(V(J,I)+HALF*A(J,I)*DT1)*DT12*WEIGHT(I)
          DA = AR(J,I) - DAMPA*VR(J,I) - BETASDT *(AR(J,I)-DAMP(J+3,I))
          DA = DA * OMEGA - AR(J,I)
          DAMP(J+3,I) = AR(J,I)
          AR(J,I)     = AR(J,I) + DA
          DW =DW+WEIGHT(I)*IN(I)*DA*(VR(J,I)+HALF*AR(J,I)*DT1)*DT12
         ENDDO
        ENDIF
       ENDDO
      ENDIF
C
#include "lockon.inc"
c        EDAMP = EDAMP + DW
         TFEXT = TFEXT + DW
#include "lockoff.inc"
C
      RETURN
      END
