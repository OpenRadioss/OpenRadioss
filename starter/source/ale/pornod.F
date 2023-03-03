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
Chd|  PORNOD                        source/ale/pornod.F           
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE PORNOD(GEO  ,IXS   ,IXQ   ,NODPOR ,ICODE  ,
     .                  ITAB ,NPBY  ,LPBY  ,IGEO)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is marking and storing nodes related to porous option 
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),IGEO(NPROPGI,NUMGEO)
      INTEGER,INTENT(IN) :: ICODE(NUMNOD),ITAB(NUMNOD),NPBY(NNPBY,*),LPBY(*)
      INTEGER,INTENT(INOUT) :: NODPOR(*)
      my_real,INTENT(INOUT) :: GEO(NPROPG,NUMGEO)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(NUMNOD) :: ITAG
      INTEGER IG,N,I,J,K,IC,IC1,IC2,IC3,IC4,JWARN,IRB,KRB,P
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: INDEX
      INTEGER SORT,IWORK(70000),IT
      CHARACTER*nchartitle,TITR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      !--------------------!
      !      TAGGING       !
      !--------------------!
      NUMPOR=0
      DO I=1,NUMNOD
        ITAG(I)=0
      END DO
C-----------------------------------------------      
      DO IG=1,NUMGEO
        IF(INT(GEO(12,IG)) /= 15)CYCLE !IG
        IF(N2D == 0)THEN
          DO I=1,NUMELS
            IF(IXS(10,I) /= IG)CYCLE !I
            DO J=2,9
              IF(ITAG(IXS(J,I)) == 0)ITAG(IXS(J,I))=IG
            END DO !J=2,9
          END DO ! I=1,NUMELS
        ELSE
          DO I=1,NUMELQ
            IF(IXQ(6,I) /= IG)CYCLE !I
            DO J=2,5
              IF(ITAG(IXQ(J,I)) == 0)ITAG(IXQ(J,I))=IG
            END DO ! J=2,5
          END DO !I=1,NUMELQ
        ENDIF
	
      !--------------------!
      !  COUNT AND STORE   !
      !--------------------!
      N=0
      JWARN=0
      DO I=1,NUMNOD
        IF(ITAG(I) /= IG)CYCLE !I
        IC=ICODE(I)
        IC1=IC/512
        IC2=(IC-512*IC1)/64
        IC3=(IC-512*IC1-64*IC2)/8
        IC4=IC-512*IC1-64*IC2-8*IC3
        IF(N2D == 0)THEN
          IF(IC4 == 7)CYCLE !I
        ELSE
          IF(IC4 >= 6)CYCLE !I
        ENDIF
        IF(INT(GEO(30,IG)) /= 0 .AND. IC1 /= 0)THEN
           JWARN = JWARN+1
           CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IG),LTITR)
           CALL ANCMSG(MSGID=358,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,I1=IGEO(1,IG),C1=TITR,I2=ITAB(I))
        ENDIF
        N=N+1
        NODPOR(NUMPOR+N)=I
      END DO !I=1,NUMNOD

      !---------------------------!
      ! Sorting nodes by porosity !
      ! (spmd order)              !
      !---------------------------!
      ALLOCATE(INDEX(N,3))
      DO I=1,N
        INDEX(I,3)=NODPOR(NUMPOR+I)
      ENDDO
      IF(N > 0) CALL MY_ORDERS(0,IWORK,INDEX(1,3),INDEX,N,1)
      DO I=1,N
        IT = INDEX(I,1)
        NODPOR(NUMPOR+I)=INDEX(IT,3)
      ENDDO
      DEALLOCATE(INDEX)
      !-----------------------------------------
      !WARNING HONEYCOMB POROUS MEDIUM PID=',IG
      !-----------------------------------------      
      IF(JWARN > 0) THEN
         CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IG),LTITR)
         CALL ANCMSG(MSGID=359,MSGTYPE=MSGWARNING,ANMODE=ANINFO,I1=IGEO(1,IG),C1=TITR,I2=JWARN)
      ENDIF      
      GEO(31,IG)=N+.1
      NUMPOR=NUMPOR+N
      IRB=INT(GEO(29,IG))
      IF(IRB /= 0)THEN
        K=1
        DO KRB=1,NRBYKIN
          IF(NPBY(1,KRB) == IRB)THEN
            GEO(33,IG) = KRB+ EM01
            GEO(34,IG) = LPBY(K)+EM01
          ENDIF
          K=K+NPBY(2,KRB)
        END DO !KRB=1,NRBYKIN
        IF(GEO(33,IG) == ZERO)THEN
          GEO(29,IG)=EM01
          CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IG),LTITR)
          CALL ANCMSG(MSGID=360,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,I1=IGEO(1,IG),C1=TITR,I2=IRB)
        ELSE
        ! main node RB replicate on all procs for SPMD calculation of porosity
          IF(IMACH == 3) THEN
            DO P = 1, NSPMD
	      CALL IFRONTPLUS(IRB,P) 
            ENDDO
          ENDIF
        ENDIF !IF (GEO(33,IG) == ZERO)
      ENDIF !IF(IRB /= 0)
      END DO !IG=1,NUMGEO
C-----------------------------------------------      
      RETURN
      END
