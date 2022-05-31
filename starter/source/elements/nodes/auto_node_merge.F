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
Chd|  AUTO_NODE_MERGE               source/elements/nodes/auto_node_merge.F
Chd|-- called by -----------
Chd|        HM_PREREAD_NODE               source/elements/reader/hm_preread_node.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE AUTO_NODE_MERGE(IS_DYNA,NUMNUSR,NUMCNOD,NUMNOD,ITAB,X)
C-----------------------------------------------
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)::IS_DYNA
      INTEGER,INTENT(IN) :: NUMNUSR, NUMCNOD
      INTEGER,INTENT(OUT):: NUMNOD
      INTEGER,INTENT(IN),DIMENSION(NUMNUSR+NUMCNOD)    :: ITAB
      my_real, INTENT(IN),DIMENSION(3,NUMNUSR) ::
     .         X
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "warn_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IREF, J, STAT
      INTEGER WORK(70000)
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITABM1, INDX
      my_real
     .  XODUSR, XMIN, YMIN, ZMIN, XMAX, YMAX, ZMAX, DX, DY, DZ, TOL
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ALLOCATE (ITABM1(2*(NUMNUSR+NUMCNOD)),INDX(2*(NUMNUSR+NUMCNOD)),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ITABM1')
C-----------------------------------------------
C     Possibly Merge Nodes
      DO I = 1, NUMNUSR
        INDX(I) = I
      END DO
      
      CALL MY_ORDERS(0,WORK,ITAB,INDX,NUMNUSR,1)
      
      IF(NUMNUSR.GE.1)THEN
        ITABM1(1)         = ITAB(INDX(1))
        ITABM1(NUMNUSR+1) = INDX(1)
      ENDIF
      
      IF(IS_DYNA == 0)THEN
      
        DO I = 2, NUMNUSR
          ITABM1(I) = ITAB(INDX(I))
          IF(ITABM1(I).EQ.ITABM1(I-1))THEN
            CALL ANCMSG(MSGID=56,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ITABM1(I))
          END IF
          ITABM1(NUMNUSR+I) = INDX(I)
        END DO
        NUMNOD = NUMNUSR

      ELSE ! IF(IS_DYNA == 0)THEN

        XODUSR=NUMNUSR
        XMIN =  EP20
        XMAX = -EP20
        YMIN =  EP20
        YMAX = -EP20
        ZMIN =  EP20
        ZMAX = -EP20
        DO I = 1, NUMNUSR
          XMIN = MIN(XMIN,X(1,I))       
          XMAX = MAX(XMAX,X(1,I))       
          YMIN = MIN(YMIN,X(2,I))       
          YMAX = MAX(YMAX,X(2,I))       
          ZMIN = MIN(ZMIN,X(3,I))       
          ZMAX = MAX(ZMAX,X(3,I))       
        END DO
        DX  = XMAX-XMIN
        DY  = YMAX-YMIN
        DZ  = ZMAX-ZMIN
        TOL = EM05*(DX+DY+DZ)/(THREE*EXP(THIRD*LOG(XODUSR)))

        I = 2
        DO WHILE(I <= NUMNUSR)

          ITABM1(I) = ITAB(INDX(I))

          IREF = I-1
          XMIN = X(1,INDX(IREF))       
          XMAX = X(1,INDX(IREF))       
          YMIN = X(2,INDX(IREF))       
          YMAX = X(2,INDX(IREF))       
          ZMIN = X(3,INDX(IREF))       
          ZMAX = X(3,INDX(IREF))       
         
          DO WHILE(I <= NUMNUSR .AND. ITABM1(I).EQ.ITABM1(IREF))

            XMIN = MIN(XMIN,X(1,INDX(I)))       
            XMAX = MAX(XMAX,X(1,INDX(I)))       
            YMIN = MIN(YMIN,X(2,INDX(I)))       
            YMAX = MAX(YMAX,X(2,INDX(I)))       
            ZMIN = MIN(ZMIN,X(3,INDX(I)))       
            ZMAX = MAX(ZMAX,X(3,INDX(I)))       
            
            INDX(I)           = INDX(IREF) ! Possibly merging a cnode and a node, or 2 cnodes
            ITABM1(NUMNUSR+I) = INDX(IREF)

            I = I + 1
            ITABM1(I)=ITAB(INDX(I))

          END DO
          
          IF(I > IREF+1)THEN

            DX = XMAX-XMIN
            DY = YMAX-YMIN
            DZ = ZMAX-ZMIN
            IF(DX < TOL .AND. DY < TOL .AND. DZ < TOL)THEN
              CALL ANCMSG(MSGID=1891,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ITABM1(I-1),I2=I-IREF,I3=ITABM1(I-1),R1=TOL)
            ELSE
              CALL ANCMSG(MSGID=56,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ITABM1(I-1))
            END IF

          ELSE ! IF(I > IREF+1)THEN

            ITABM1(NUMNUSR+I) = INDX(I)
            I = I + 1

          END IF

        END DO ! DO WHILE(I <= NUMNUSR)

        NUMNOD = 1
        DO I=2,NUMNUSR
           IF(ITABM1(NUMNUSR+I) == ITABM1(NUMNUSR+I-1)) CYCLE ! Twice the same ID
           NUMNOD = NUMNOD + 1
        ENDDO

      END IF ! IF(IS_DYNA == 0)THEN
C-----------------------------------------------
      DO I = 1, NUMNUSR+NUMCNOD
        INDX(I) = I
      END DO
      
      CALL MY_ORDERS(0,WORK,ITAB,INDX,NUMNUSR+NUMCNOD,1)
      
      IF(NUMNUSR+NUMCNOD.GE.1)THEN
        ITABM1(1)                 = ITAB(INDX(1))
        ITABM1(NUMNUSR+NUMCNOD+1) = INDX(1)
      ENDIF
      
      DO I = 2, NUMNUSR+NUMCNOD
        ITABM1(I) = ITAB(INDX(I))
        IF(ITABM1(I).EQ.ITABM1(I-1))THEN
          IF((INDX(I-1) < NUMNUSR .AND. INDX(I) > NUMNUSR) .OR.
     .       (INDX(I-1) > NUMNUSR .AND. INDX(I) < NUMNUSR)) THEN
C           A Node and a Cnode have the same ID
            CALL ANCMSG(MSGID=1889,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ITABM1(I))
          ELSEIF(INDX(I-1) > NUMNUSR .AND. INDX(I) > NUMNUSR)THEN
C           Two Cnode shave the same ID
            CALL ANCMSG(MSGID=1890,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ITABM1(I))
          END IF
        END IF
        ITABM1(NUMNUSR+NUMCNOD+I) = INDX(I)
      END DO
C-----------------------------------------------
      NUMNOD = NUMNOD + NUMCNOD
      DEALLOCATE(ITABM1,INDX)
      RETURN
      END
      
