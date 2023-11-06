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
Chd|  ININODE_RM                    source/materials/mat/mat019/ininode_rm.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ININODE_RM(CONNEC,IRIG_NODE,IRBY,SLN,NRB,NRSLN,
     .                      STIFN,STIFR, RMSTIFN, RMSTIFR,NUMEL,
     .                      NER)
C=======================================================================
C            EN SORTIE
C    IRBY    Rigid node
C    SLN     secnd node number by rigid material
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  IRBY(*),SLN(*),NRSLN,NRB,NUMEL,NER
        INTEGER  RBYID,IRIG_NODE(*),CONNEC(NUMEL,*)
        my_real
     .      STIFN(*),STIFR(*),RMSTIFN(*), RMSTIFR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,II,JJ,NR,NR1,NR0,KK,NRC,IR,N,NREST,IE,
     .          NER1,NER0,IEL
C     REAL

        INTEGER, DIMENSION(:), ALLOCATABLE ::
     .                              IRN,INDX,INDX0,ITAG,IRE,
     .                              INDXE, INDXE0, ITAGE
C
C  construction des materiaux rigides
C
        ALLOCATE (IRN(NUMNOD))
        IRN = 0
        NR = 0
        NRSLN =0
        DO I = 1, NUMNOD
          IF(IRIG_NODE(I) > 0) THEN
            NR = NR + 1
            IRN(NR)= I
          ENDIF
        ENDDO
C
C  allocation de tableau de travail
C
        ALLOCATE (INDX(NR),INDX0(NR),ITAG(NUMNOD))
        ALLOCATE (INDXE(NER),INDXE0(NER),ITAGE(NER))
C initialisation
        ITAG = 0
        ITAGE = 0
        INDX  = 0
        INDX0 = 0
        INDXE  = 0
        INDXE0 = 0
C
        NR1 = 0
        NRB = 1
        ITAGE(1) = NRB
        INDXE(1) = 1
C
        DO J=1,10
          IE = INDXE(1)
          II = CONNEC(IE,J)
          IF(II > 0 ) THEN
            ITAG(II) = NRB
            NR1 = NR1 + 1
            INDX(NR1) = II
          ENDIF
        ENDDO
C
        NREST = NR - NR1
        NER1 = 0

        DO I=2,NER
          NER1 = NER1 + 1
          INDXE(NER1) = I
        ENDDO
c      NRC = NR1
        KK = 0
        DO WHILE(NREST >  0 )
          NR0  = 0
          NER0 = 0
          DO I=1,NR1
            II = INDX(I)
            DO IE =1 , NER1
              IEL = INDXE(IE)
              IF(ITAGE(IEL) == 0) THEN
                DO J=1,10
                  IF(II == CONNEC(IEL, J)) THEN
                    NER0 = NER0 + 1
                    INDXE0(NER0) = IEL
                    ITAGE(IEL) = NRB
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDDO
          NR1 = 0
          IF(NER0 > 0) THEN
            DO I=1,NER0
              IEL = INDXE0(I)
              DO J=1,10
                II= CONNEC(IEL,J)
                IF(II > 0) THEN
                  IF(ITAG(II) == 0)THEN
                    ITAG(II) = NRB
                    NR1 = NR1 + 1
                    INDX(NR1) = II
                  END IF
                ENDIF
              ENDDO
            ENDDO
            IF(NR1 > 0) THEN
C nbre d'element restant
              NER0 = 0
              DO IE = 1, NER1
                IEL = INDXE(IE)
                IF(ITAGE(IEL) == 0) THEN
                  NER0 = NER0 + 1
                  INDXE0(NER0) = IEL
                ENDIF
              ENDDO
              NER1  = NER0
              DO I=1,NER0
                INDXE(I)  = INDXE0(I)
              ENDDO
C noeud restant
              NREST = NREST - NR1
            ELSE
              NRB = NRB  + 1
              NER0 = 0
              DO IE =1 , NER1
                IEL = INDXE(IE)
                IF(ITAGE(IEL) == 0) THEN
                  NER0 = NER0 + 1
                  INDXE0(NER0) = IEL
                ENDIF
              ENDDO
C creation d'un autre corps rigid
              NER1 = NER0
              DO I=1,NER1
                INDXE(I) = INDXE0(I)
              ENDDO
C
              IEL = INDXE(1)
              NR1 = 0
              DO J=1,10
                IEL = INDXE(1)
                II = CONNEC(IEL,J)
                ITAGE(IEL) = NRB
                IF(II > 0 ) THEN
                  ITAG(II) = NRB
                  NR1 = NR1 + 1
                  INDX(NR1) = II
                ENDIF
              ENDDO
C
              NREST = NREST - NR1
            ENDIF
          ELSE
            NRB = NRB + 1
C creation d'un autre corps rigid
            NER0 = 0
            DO IE =1 , NER1
              IEL = INDXE(IE)
              IF(ITAGE(IEL) == 0) THEN
                NER0 = NER0 + 1
                INDXE0(NER0) = IEL
              ENDIF
            ENDDO
C creation d'un autre corps rigid
            NER1 = NER0
            DO I=1,NER1
              INDXE(I) = INDXE0(I)
            ENDDO
            IEL = INDXE(1)
            ITAGE(IEL) = NRB
            NR1 = 0
            DO J=1,10
cc                  IEL = INDXE(1)
              II = CONNEC(IEL,J)
              IF(II > 0 ) THEN
                ITAG(II) = NRB
                NR1 = NR1 + 1
                INDX(NR1) = II
              ENDIF
            ENDDO
C
            NREST = NREST - NR1
          ENDIF
        ENDDO
C
C   creation des corps rigide
C
        KK = 0
        DO IR =1,NRB
          II = 0
          DO I=1,NR
            N = IRN(I)
            IF(ITAG(N) == IR) THEN
              II = II + 1
              IRBY(II + KK) = N
              RMSTIFN(II + KK) = STIFN(N)
              RMSTIFR(II + KK) = STIFR(N)
              STIFN(N) = EM20
              STIFR(N) = EM20
            ENDIF
          ENDDO
          SLN(IR) = II
          KK = KK + II
          NRSLN = NRSLN + II
        ENDDO
C
        DEALLOCATE(ITAG,IRN,INDX,INDX0)
        DEALLOCATE(INDXE,INDXE0,ITAGE)
        RETURN
      END

