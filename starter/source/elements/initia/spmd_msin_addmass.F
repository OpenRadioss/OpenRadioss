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
Chd|  SPMD_MSIN_ADDMASS             source/elements/initia/spmd_msin_addmass.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_MSIN_ADDMASS(
     1               IXS    ,IXS10  ,IXS20  ,IXS16  ,IXQ      ,
     2               IXC    ,IXT    ,IXP    ,IXR    ,IXTG     ,
     3               IXUR   ,MSS    ,MSSX   ,MSQ    ,MSC      ,
     4               MST    ,MSP    ,MSR    ,MSTG   ,MSUR     ,
     5               PTG    ,EVUR   ,MS     ,INDEX  ,ITRI     ,
     6               GEO    ,SH4TREE,SH3TREE,PARTSAV,IPMAS    ,
     7               IPARTS ,IPARTQ ,IPARTC ,IPARTT   ,
     8               IPARTP ,IPARTR ,IPARTUR,IPARTTG,TOTADDMAS,
     9               IPART  ,THK    ,PM     ,PART_AREA,
     A               ADDEDMS,ITAB   ,PARTSAV1_PON,ELE_AREA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE OPTIONDEF_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "rnur_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS10(6,*),IXS20(12,*),IXS16(8,*),
     .   IXQ(NIXQ,*),IXC(NIXC,*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .   IXTG(6,*),IXUR(6,*),INDEX(*), ITRI(*),SH4TREE(KSH4TREE,*),
     .   SH3TREE(KSH3TREE,*),IPARTS(*),IPARTQ(*),IPARTC(*),
     .   IPARTT(*),IPARTP(*),IPARTR(*),IPARTUR(*),IPARTTG(*),
     .   IPART(LIPART1,*),ITAB(*)
C     REAL ou REAL*8
      my_real
     .   MSS(8,*),MSSX(12,*),MSQ(*),MSC(*),MST(*),MSP(*),MSR(3,*),
     .   MSTG(*),MSUR(*),PTG(3,*),EVUR(3,*),MS(*),GEO(NPROPG,*),
     .   PARTSAV(20,*),TOTADDMAS,PART_AREA(*),THK(*),
     .   ADDEDMS(*),PM(NPROPM,*),PARTSAV1_PON(NPART),ELE_AREA(*)
C
      INTEGER IDEB
      TYPE (ADMAS_)  , DIMENSION(NODMAS)  :: IPMAS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, II, IGTYP, WORK(70000),IP,KAD,IGM,IPM,NMAS,
     .   FLAG
C
      my_real
     .   MASS,KMASS,AREA_EL
C-----------------------------------------------
C
!      PARTSAV1_PON(1:NPART)=ZERO
C
      DO I = 1, NUMELS
        ITRI(I) = IXS(11,I)
      ENDDO
C
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELS8,1)

      IDEB=NUMELS8+1
      CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS10,1)

      DO J=1,NUMELS10
           index(IDEB+J-1) = index(IDEB+J-1)+numels8
      ENDDO

      IDEB = IDEB + NUMELS10
      CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS20,1)
      DO j = 1, NUMELS20
          index(IDEB+J-1) = index(IDEB+J-1)+numels8+numels10
      ENDDO

      IDEB = IDEB + NUMELS20
      CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS16,1)
      DO j = 1, NUMELS16
          index(IDEB+J-1) = index(IDEB+J-1)+numels8+numels10+numels20
      ENDDO
C
      DO IGM=1,NODMAS
        NMAS = IPMAS(IGM)%NPART
        DO II = 1,NMAS
          IPM = IPMAS(IGM)%PARTID(II)
C  NUMELS
          DO J=1,NUMELS
            I = INDEX(J)
            IP = IPARTS(I)
            IF(IP == IPM)THEN
              DO K=1,8
                N = IXS(K+1,I)
                KMASS = MSS(K,I) / PARTSAV1_PON(IP)
                MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
                MS(N) = MS(N) + MASS
                TOTADDMAS = TOTADDMAS + MASS
              ENDDO
            ENDIF
          ENDDO
C  NUMELS10
          IF(NUMELS10.GT.0) THEN
            DO J=1,NUMELS10
              I = INDEX(NUMELS8+J)
              IP = IPARTS(I)
              IF(IP == IPM)THEN
                DO K=1,6
                  N = IXS10(K,I-NUMELS8)
                  KMASS = MSSX(K,I) / PARTSAV1_PON(IP)
                  MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
                  IF(N.NE.0)THEN
                    MS(N) = MS(N) + MASS
                    TOTADDMAS = TOTADDMAS + MASS
                  END IF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
C  NUMELS20
          IF(NUMELS20.GT.0)THEN
            DO J=1,NUMELS20
              I = INDEX(NUMELS8+NUMELS10+J)
              IP = IPARTS(I)
               IF(IP == IPM)THEN
                DO K=1,12
                  N = IXS20(K,I-NUMELS8-NUMELS10)
                  KMASS = MSSX(K,I) / PARTSAV1_PON(IP)
                  MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
                  IF(N.NE.0)THEN
                    MS(N) = MS(N) + MASS
                    TOTADDMAS = TOTADDMAS + MASS
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
C  NUMELS20
          IF(NUMELS16.GT.0)THEN
            DO J=1,NUMELS16
              I = INDEX(NUMELS8+NUMELS10+NUMELS20+J)
              IP = IPARTS(I)
              IF(IP == IPM)THEN
                DO K=1,8
                  N = IXS16(K,I-NUMELS8-NUMELS10-NUMELS20)
                  KMASS = MSSX(K,I) / PARTSAV1_PON(IP)
                  MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
                  IF(N.NE.0)THEN
                    MS(N) = MS(N) + MASS
                    TOTADDMAS = TOTADDMAS + MASS
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C  NUMELQ
      DO I = 1, NUMELQ
        ITRI(I) = IXQ(7,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELQ,1)
C
      DO IGM=1,NODMAS
        NMAS = IPMAS(IGM)%NPART
        DO II = 1,NMAS
          IPM = IPMAS(IGM)%PARTID(II)
          DO J=1,NUMELQ
            I = INDEX(J)
            IP = IPARTQ(I)
            IF(IP == IPM)THEN
              KMASS = MSQ(I) / PARTSAV1_PON(IP)
              MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
              DO K=1,4
                N = IXQ(K+1,I)
                MS(N) = MS(N) + MASS
                TOTADDMAS = TOTADDMAS + MASS
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C NUMELC


C===============================================
C     Compute area of part
      DO I = 1, NUMELTG
        ITRI(I) = IXTG(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELTG,1)
C P/ON computation of PART_AREA
      DO J=1,NUMELTG
        I = INDEX(J)
        IP = IPARTTG(I)
        AREA_EL = ELE_AREA(I+NUMELC)
        PART_AREA(IP) = PART_AREA(IP) + AREA_EL
      ENDDO
      DO I = 1, NUMELC
        ITRI(I) = IXC(7,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELC,1)
C P/ON computation of PART_AREA
      DO J=1,NUMELC
        I = INDEX(J)
        IP = IPARTC(I)
        AREA_EL = ELE_AREA(I)
        PART_AREA(IP) = PART_AREA(IP) + AREA_EL
      ENDDO
C===============================================
C
      DO IGM=1,NODMAS
        NMAS = IPMAS(IGM)%NPART
        FLAG = IPMAS(IGM)%WEIGHT_FLAG
        DO II = 1,NMAS
          IPM = IPMAS(IGM)%PARTID(II)
          IF(NADMESH.EQ.0)THEN
            DO J=1,NUMELC
              I = INDEX(J)
              IP = IPARTC(I)
              IF(IP == IPM)THEN
                IF(FLAG == 0)THEN
                  KMASS = MSC(I) / PARTSAV1_PON(IP)
                ELSE IF(FLAG == 1)THEN
                  AREA_EL = ELE_AREA(I)*QUART
                  KMASS = AREA_EL / PART_AREA(IP)
                END IF
                MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
                DO K=1,4
                  N = IXC(K+1,I)
                  MS(N) = MS(N) + MASS
                  TOTADDMAS = TOTADDMAS + MASS
                ENDDO
              ENDIF
            ENDDO

          ELSE
            IF(ISTATCND==0)THEN
              DO J=1,NUMELC
                I = INDEX(J)
                IF(SH4TREE(3,I) >= 0)THEN
                  IP = IPARTC(I)
                  IF(IP == IPM)THEN
                    IF(FLAG == 0)THEN
                      KMASS = MSC(I) / PARTSAV1_PON(IP)
                    ELSE IF(FLAG == 1)THEN
                      AREA_EL = ELE_AREA(I)*QUART
                      KMASS = AREA_EL / PART_AREA(IP)
                    END IF
                    MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
                    DO K=1,4
                      N = IXC(K+1,I)
                      MS(N) = MS(N) + MASS
                      TOTADDMAS = TOTADDMAS + MASS
                    ENDDO
                  ENDIF
                ENDIF
              ENDDO
            ELSE
              DO J=1,NUMELC
                I = INDEX(J)
                IF(SH4TREE(3,I) == 0 .OR. SH4TREE(3,I) == -1)THEN
                  IP = IPARTC(I)
                  IF(IP == IPM)THEN
                    IF(FLAG == 0)THEN
                      KMASS = MSC(I) / PARTSAV1_PON(IP)
                    ELSE IF(FLAG == 1)THEN
                      AREA_EL = ELE_AREA(I)*QUART
                      KMASS = AREA_EL / PART_AREA(IP)
                    END IF
                    MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
                    DO K=1,4
                      N = IXC(K+1,I)
                      MS(N) = MS(N) + MASS
                      TOTADDMAS = TOTADDMAS + MASS
                    ENDDO
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C NUMELT
      DO I = 1, NUMELT
        ITRI(I) = IXT(5,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELT,1)
C
      DO IGM=1,NODMAS
        NMAS = IPMAS(IGM)%NPART
        DO II = 1,NMAS
          IPM = IPMAS(IGM)%PARTID(II)
          DO J=1,NUMELT
            I = INDEX(J)
            IP = IPARTT(I)
            IF(IP == IPM)THEN
              KMASS = MST(I) / PARTSAV1_PON(IP)
              MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
              DO K=1,2
                N = IXT(K+1,I)
                MS(N) = MS(N) + MASS
                TOTADDMAS = TOTADDMAS + MASS
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C NUMELP
      DO I = 1, NUMELP
        ITRI(I) = IXP(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELP,1)
C
      DO IGM=1,NODMAS
        NMAS = IPMAS(IGM)%NPART
        DO II = 1,NMAS
          IPM = IPMAS(IGM)%PARTID(II)
          DO J=1,NUMELP
            I = INDEX(J)
            IP = IPARTP(I)
            IF(IP == IPM)THEN
              KMASS = MSP(I) / PARTSAV1_PON(IP)
              MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
              N = IXP(2,I)
              MS(N) = MS(N) + MASS
              TOTADDMAS = TOTADDMAS + MASS
              N = IXP(3,I)
              MS(N) = MS(N) + MASS
              TOTADDMAS = TOTADDMAS + MASS
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C NUMELR
      DO I = 1, NUMELR
        ITRI(I) = IXR(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELR,1)
C
      DO IGM=1,NODMAS
        NMAS = IPMAS(IGM)%NPART
        DO II = 1,NMAS
          IPM = IPMAS(IGM)%PARTID(II)
          DO J=1,NUMELR
            I = INDEX(J)
            IP = IPARTR(I)
            IF(IP == IPM)THEN
              DO K=1,2
                N = IXR(K+1,I)
                KMASS = MSR(K,I) / PARTSAV1_PON(IP)
                MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
                MS(N) = MS(N) + MASS
                TOTADDMAS = TOTADDMAS + MASS
              ENDDO
              IGTYP = NINT(GEO(12,IXR(1,I)))
              IF(IGTYP.EQ.12) THEN
                N = IXR(4,I)
                KMASS = MSR(3,I) / PARTSAV1_PON(IP)
                MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
                MS(N) = MS(N) + MASS
               TOTADDMAS = TOTADDMAS + MASS
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C NUMELTG
      DO I = 1, NUMELTG
        ITRI(I) = IXTG(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELTG,1)

      DO IGM=1,NODMAS
        NMAS = IPMAS(IGM)%NPART
        DO II = 1,NMAS
          IPM = IPMAS(IGM)%PARTID(II)
          IF(NADMESH.EQ.0)THEN
            DO J=1,NUMELTG
              I = INDEX(J)
              IP = IPARTTG(I)
              IF(IP == IPM)THEN
!---
                IF(FLAG == 0)THEN
                  KMASS = MSTG(I) / PARTSAV1_PON(IP)
                ELSEIF(FLAG == 1)THEN
                  AREA_EL = ELE_AREA(I+NUMELC)
                  KMASS = AREA_EL / PART_AREA(IP)
                ENDIF
                MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
!---
                DO K=1,3
                  N = IXTG(K+1,I)
                  MS(N) = MS(N) + MASS*PTG(K,I)
                  TOTADDMAS = TOTADDMAS + MASS*PTG(K,I)
                ENDDO
              ENDIF
            ENDDO
          ELSE
            IF(ISTATCND==0)THEN
              DO J=1,NUMELTG
                I = INDEX(J)
                IF(SH3TREE(3,I) >= 0)THEN
                  IP = IPARTTG(I)
                  IF(IP == IPM)THEN
!---
                    IF(FLAG == 0)THEN
                      KMASS = MSTG(I) / PARTSAV1_PON(IP)
                    ELSEIF(FLAG == 1)THEN
                      AREA_EL = ELE_AREA(I+NUMELC)
                      KMASS = AREA_EL / PART_AREA(IP)
                    ENDIF
                    MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
!---
                    DO K=1,3
                      N = IXTG(K+1,I)
                      MS(N) = MS(N) + MASS*PTG(K,I)
                      TOTADDMAS = TOTADDMAS + MASS*PTG(K,I)
                    ENDDO
                  ENDIF
                ENDIF
              ENDDO
            ELSE
              DO J=1,NUMELTG
                I = INDEX(J)
                IF(SH3TREE(3,I) == 0 .OR. SH3TREE(3,I) == -1)THEN
                  IP = IPARTTG(I)
                  IF(IP == IPM)THEN
!---
                    IF(FLAG == 0)THEN
                      KMASS = MSTG(I) / PARTSAV1_PON(IP)
                    ELSEIF(FLAG == 1)THEN
                      AREA_EL = ELE_AREA(I+NUMELC)
                      KMASS = AREA_EL / PART_AREA(IP)
                    ENDIF
                    MASS  = KMASS * IPMAS(IGM)%PART(II)%RPMAS
!---
                    DO K=1,3
                      N = IXTG(K+1,I)
                      MS(N) = MS(N) + MASS*PTG(K,I)
                      TOTADDMAS = TOTADDMAS + MASS*PTG(K,I)
                    ENDDO
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C---      
      DO I=1,NPART
        IF(ADDEDMS(I) > ZERO) THEN
         PARTSAV(1,I) = PARTSAV(1,I) + ADDEDMS(I)
         PARTSAV1_PON(I) = PARTSAV1_PON(I) + ADDEDMS(I)
       ENDIF
      END DO
C---
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_PARTSAV_PON              source/elements/initia/spmd_msin_addmass.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE SPMD_PARTSAV_PON(
     1               IXS    ,IXS10  ,IXS20  ,IXS16        ,IXQ    ,
     2               IXC    ,IXT    ,IXP    ,IXR          ,IXTG   ,
     3               IXUR   ,MSS    ,MSSX   ,MSQ          ,MSC    ,
     4               MST    ,MSP    ,MSR    ,MSTG         ,MSUR   ,
     5               INDEX  ,ITRI   ,GEO    ,PARTSAV1_PON ,IPARTS ,
     6               IPARTQ ,IPARTC ,IPARTT ,IPARTP       ,IPARTR ,
     7               IPARTUR,IPARTTG,IPART  )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "rnur_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS10(6,*),IXS20(12,*),IXS16(8,*),
     .   IXQ(NIXQ,*),IXC(NIXC,*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .   IXTG(6,*),IXUR(6,*),INDEX(*), ITRI(*),
     .   IPARTS(*),IPARTQ(*),IPARTC(*),
     .   IPARTT(*),IPARTP(*),IPARTR(*),IPARTUR(*),IPARTTG(*),
     .   IPART(LIPART1,*)
C     REAL ou REAL*8
      my_real
     .   MSS(8,*),MSSX(12,*),MSQ(*),MSC(*),MST(*),MSP(*),MSR(3,*),
     .   MSTG(*),MSUR(*),GEO(NPROPG,*),PARTSAV1_PON(NPART)
C
      INTEGER IDEB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, II, IGTYP, WORK(70000),IP,KAD,IGM,IPM,NMAS,
     .   FLAG
C-----------------------------------------------
C
      PARTSAV1_PON(1:NPART)=ZERO
C
      DO I = 1, NUMELS
        ITRI(I) = IXS(11,I)
      ENDDO
C
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELS8,1)

      IDEB=NUMELS8+1
      CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS10,1)

      DO J=1,NUMELS10
           index(IDEB+J-1) = index(IDEB+J-1)+numels8
      ENDDO

      IDEB = IDEB + NUMELS10
      CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS20,1)
      DO j = 1, NUMELS20
          index(IDEB+J-1) = index(IDEB+J-1)+numels8+numels10
      ENDDO

      IDEB = IDEB + NUMELS20
      CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS16,1)
      DO j = 1, NUMELS16
          index(IDEB+J-1) = index(IDEB+J-1)+numels8+numels10+numels20
      ENDDO
C
      DO J=1,NUMELS
         I = INDEX(J)
         IP = IPARTS(I)
           DO K=1,8
              PARTSAV1_PON(IP)=PARTSAV1_PON(IP)+MSS(K,I)
           ENDDO
      ENDDO

C  NUMELS10
      IF(NUMELS10.GT.0) THEN
       DO J=1,NUMELS10
        I = INDEX(NUMELS8+J)
        IP = IPARTS(I)
        DO K=1,6
         PARTSAV1_PON(IP)=PARTSAV1_PON(IP)+MSSX(K,I)
        ENDDO
       ENDDO
      ENDIF
C  NUMELS20
      IF(NUMELS20.GT.0)THEN
       DO J=1,NUMELS20
        I = INDEX(NUMELS8+NUMELS10+J)
        IP = IPARTS(I)
        DO K=1,12
         PARTSAV1_PON(IP)=PARTSAV1_PON(IP)+MSSX(K,I)
        ENDDO
       ENDDO
      ENDIF
C  NUMELS16
      IF(NUMELS16.GT.0)THEN
       DO J=1,NUMELS16
        I = INDEX(NUMELS8+NUMELS10+NUMELS20+J)
        IP = IPARTS(I)
        DO K=1,8
         PARTSAV1_PON(IP)=PARTSAV1_PON(IP)+MSSX(K,I)
        ENDDO
       ENDDO
      ENDIF

C  NUMELQ
      DO I = 1, NUMELQ
        ITRI(I) = IXQ(7,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELQ,1)
C
      DO J=1,NUMELQ
       I = INDEX(J)
       IP = IPARTQ(I)
       PARTSAV1_PON(IP)=PARTSAV1_PON(IP)+ QUATRE * MSQ(I)
      ENDDO
      
C NUMELC
      DO I = 1, NUMELC
        ITRI(I) = IXC(7,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELC,1)
C
      DO J=1,NUMELC 
       I=INDEX(J)
       IP=IPARTC(I)
       PARTSAV1_PON(IP)=PARTSAV1_PON(IP)+ QUATRE * MSC(I)
      ENDDO

C NUMELT
      DO I = 1, NUMELT
        ITRI(I) = IXT(5,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELT,1)
C
      DO J=1,NUMELT
       I=INDEX(J)
       IP=IPARTT(I)
       PARTSAV1_PON(IP)=PARTSAV1_PON(IP)+ DEUX * MST(I)
      ENDDO
      
C NUMELP
      DO I = 1, NUMELP
        ITRI(I) = IXP(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELP,1)
C
      DO J=1,NUMELP
       I=INDEX(J)
       IP=IPARTP(I)
       PARTSAV1_PON(IP)=PARTSAV1_PON(IP)+ DEUX * MSP(I)
      ENDDO
      
C NUMELR
      DO I = 1, NUMELR
        ITRI(I) = IXR(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELR,1)
C
      DO J=1,NUMELR
       I=INDEX(J)
       IP=IPARTR(I)
       IGTYP = NINT(GEO(12,IXR(1,I)))
       IF(IGTYP==12) THEN 
        K=3
       ELSE
        K=2
       ENDIF
       DO II=1,K
        PARTSAV1_PON(IP)=PARTSAV1_PON(IP)+MSR(II,I)
       ENDDO
      ENDDO
      
C NUMELTG
      DO I = 1, NUMELTG
        ITRI(I) = IXTG(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELTG,1)
C
      DO J=1,NUMELTG
       I=INDEX(J)
       IP=IPARTTG(I)
       PARTSAV1_PON(IP)=PARTSAV1_PON(IP)+MSTG(I)
      ENDDO
C---
      RETURN
      END
