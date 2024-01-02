Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  ITAGSL2                       source/interfaces/inter3d1/itagsl2.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        INTAB                         source/interfaces/inter3d1/i24tools.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ITAGSL2(IPARI ,NOM_OPT,ITAB  ,IKINE  ,INTBUF_TAB,
     .                   ITAGND,ICNDS10,NSTRF ,ITAGCYC,IRBE2     ,
     .                   IRBE3 ,LRBE3  )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "kincod_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER),ITAB(*),IKINE(*),ITAGND(*),ICNDS10(3,*)
      INTEGER NOM_OPT(LNOPT1,*),NSTRF(*),ITAGCYC(*)
      INTEGER , DIMENSION(NRBE2L,NRBE2), INTENT(IN) :: IRBE2
      INTEGER , DIMENSION(NRBE3L,NRBE3), INTENT(IN) :: IRBE3
      INTEGER , DIMENSION(SLRBE3), INTENT(IN) :: LRBE3
      TYPE(INTBUF_STRUCT_), DIMENSION(NINTER) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NTY,ILEV,NSN,NMN,ISL,NKIN,NOINT,NINT,KCOND,IML,NNOD,NBINTER,TYP,K0
      INTEGER, DIMENSION(:), ALLOCATABLE :: PENTAG,TAGNOS,ITAGMD
      INTEGER, DIMENSION(:), ALLOCATABLE :: IKINE1
      CHARACTER*nchartitle, TITR
      INTEGER II,N1,N2,IAD,M
      LOGICAL IS1,IS2
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C=======================================================================
      ALLOCATE( PENTAG(NUMNOD),TAGNOS(NUMNOD),ITAGMD(NUMNOD) )
      ALLOCATE( IKINE1(3*NUMNOD) )
      PENTAG(1:NUMNOD) = 0
      TAGNOS(1:NUMNOD) = 0
      IKINE1(1:3*NUMNOD) = 0
C
C     Tag des Secnds
C
      DO N=1,NINTER
        NTY  = IPARI(7,N)
        ILEV = IPARI(20,N)
        IF (NTY == 2 .and. (ILEV == 27 .or. ILEV == 28)) THEN
          NSN   = IPARI(5,N)
          NOINT = IPARI(15,N)
          DO I=1,NSN
            ISL = INTBUF_TAB(N)%NSV(I)
            NKIN  = IKINE(ISL)
C
            KCOND = IBC(NKIN)+ITF(NKIN)+IRB(NKIN)+IRB2(NKIN)+IVF(NKIN)+IRV(NKIN)+IJO(NKIN)
     .            + IRBM(NKIN)+ILMULT(NKIN)+IRLK(NKIN)+IKRBE2(NKIN)+IKRBE3(NKIN)
     .            + TAGNOS(ISL)
            IF (NBCSCYC > 0) KCOND = KCOND +ITAGCYC(ISL)
C       
            IF (KCOND /= 0) PENTAG(ISL) = 1
C--         Check of incompatibility with other spt27 or spt28 t2 interfaces - multiple connections with the same main on several interfaces already cleaned
            TAGNOS(ISL) = 1
          ENDDO
        ENDIF
      ENDDO
C
C    Tag des noeuds de section (section = IMPDISP dans certains cas)
C
     
      IF(NSECT > 0) THEN
        K0 = NSTRF(25)    
        DO N=1,NSECT
          TYP = NSTRF(K0)
          NNOD = NSTRF(K0+6)
          NBINTER = NSTRF(K0+14)
          IF ((TYP == 100).OR.(TYP == 101)) THEN
            DO I=1,NNOD
              ISL = NSTRF(K0+30+NBINTER-1+I)
              IF (TAGNOS(ISL) == 1) PENTAG(ISL) = 1
            ENDDO
          ENDIF
          K0 = NSTRF(K0+24)
        ENDDO
      ENDIF
C
C     Tag des mains pour TYPE2 symetrisees
C
      DO N=1,NINTER
        NTY = IPARI(7,N)
        IF (NTY == 2) THEN
          NMN   = IPARI(6,N)
          ILEV  = IPARI(20,N)
c
          DO I=1,NMN                   
            J = INTBUF_TAB(N)%MSR(I)   
            IF ((ILEV == 0 .OR. ILEV == 1 .OR. ILEV == 27 .OR. ILEV == 28) .AND. PENTAG(J) == 0) THEN
              PENTAG(J) = ONE
            ENDIF  
          ENDDO    
        ENDIF
      ENDDO
C-------if only middle node (Itet=2 of tetra10) is the salve ILEV == 27 .or. ILEV == 28 change to penality, 
      IF (NS10E>0) THEN
       DO N=1,NINTER
        NTY  = IPARI(7,N)
        ILEV = IPARI(20,N)
        IF (NTY == 2.AND.(ILEV == 27 .or. ILEV == 28)) THEN
          NSN   = IPARI(5,N)
          NOINT = IPARI(15,N)
          DO I=1,NSN
           ISL = INTBUF_TAB(N)%NSV(I)
           IF (ITAGND(ISL)/=0 .AND.PENTAG(ISL) /= 1) THEN
            II = IABS(ITAGND(ISL))
            N1 = ICNDS10(2,II)
            N2 = ICNDS10(3,II)
            IS1 = INTAB(NSN,INTBUF_TAB(N)%NSV,N1)
            IS2 = INTAB(NSN,INTBUF_TAB(N)%NSV,N2)
            IF (.NOT.(IS1).OR..NOT.(IS2)) PENTAG(ISL) = 1
           END IF
          ENDDO
        END IF !(NTY == 2 )
       ENDDO
C----------------------------------------------------------
      ITAGMD(1:NUMNOD)   = 0
       DO I = 1, NS10E
        N = IABS(ICNDS10(1,I))
        ITAGMD(N) = I
       END DO
C---- ITAGMD :tag nd M of int2 > NS10E  
C----         <0 tag n1,n2 S w/o penality of int2      
       DO N=1,NINTER
        NTY  = IPARI(7,N)
        IF (NTY == 2 ) THEN
         NMN =IPARI(6,N)                                 
         NSN   = IPARI(5,N)
         ILEV = IPARI(20,N)
         IF (ILEV == 27 .or. ILEV == 28) THEN
          DO I=1,NSN
           ISL = INTBUF_TAB(N)%NSV(I)
           IF (PENTAG(ISL) /= 1.AND.ITAGMD(ISL)==0) ITAGMD(ISL)=-1
          END DO 
          DO I=1,NMN
           IML = INTBUF_TAB(N)%MSR(I)
           IF (ITAGMD(IML)>0) ITAGMD(IML) = ITAGMD(IML) + NS10E
          ENDDO
         END IF
        END IF
       END DO 
       DO I = 1, NS10E
        N = IABS(ICNDS10(1,I))
        N1 = ICNDS10(2,I)
        N2 = ICNDS10(3,I)
        IF (ITAGMD(N)>NS10E.OR.PENTAG(N)==1) THEN
         IF (ITAGMD(N1)<0) PENTAG(N1)=1
         IF (ITAGMD(N2)<0) PENTAG(N2)=1
        END IF
       END DO
      END IF !(NS10E>0) THEN
C----------------------------------------------------------
C - not-autorised hierarchy :RBE3/INT2
C----------------------------------------------------------
      DO I=1,NRBE3
        IAD = IRBE3(1,I)
        NMN = IRBE3(5,I)
        DO J =1,NMN
          M = LRBE3(IAD+J)
          IF (PENTAG(M)==0) PENTAG(M)=1
        END DO
      END DO
C----------------------------------------------------------
C - not-autorised hierarchy :RBE2/INT2
C----------------------------------------------------------
      DO I=1,NRBE2
        M = IRBE2(3,I)
        IF (PENTAG(M)==0) PENTAG(M)=1
      END DO
C
      DO N=1,NINTER
        NTY  = IPARI(7,N)
        ILEV = IPARI(20,N)
        IF (NTY == 2 .and. (ILEV == 27.or. ILEV == 28)) THEN
          NSN   = IPARI(5,N)
          NOINT = IPARI(15,N)
          NINT  = N
       
c          ID=NOM_OPT(1,NINT)
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,NINT),LTITR)
          DO I=1,NSN
            ISL = INTBUF_TAB(N)%NSV(I)
            IF (PENTAG(ISL) == 1) THEN
C             penalty node - set penalty flag
              INTBUF_TAB(N)%IRUPT(I) = 1
              ITF(IKINE(ISL)) = 0
              CALL ANCMSG(MSGID=1179,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ITAB(ISL),
     .                    PRMOD=MSG_CUMU)
            ELSE
C             kinematic node - set kine flag for RWALL deactivation in Kinchk
              CALL KINSET(2,ITAB(ISL),IKINE(ISL),1,0,IKINE1(ISL))
              CALL KINSET(2,ITAB(ISL),IKINE(ISL),2,0,IKINE1(ISL))
              CALL KINSET(2,ITAB(ISL),IKINE(ISL),3,0,IKINE1(ISL))
              CALL KINSET(2,ITAB(ISL),IKINE(ISL),4,0,IKINE1(ISL))
              CALL KINSET(2,ITAB(ISL),IKINE(ISL),5,0,IKINE1(ISL))
              CALL KINSET(2,ITAB(ISL),IKINE(ISL),6,0,IKINE1(ISL))
            ENDIF
          ENDDO
        ENDIF
        CALL ANCMSG(MSGID=1179,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=NOINT,
     .                    C1=TITR,
     .                    PRMOD=MSG_PRINT)
      ENDDO
C
      WRITE(IOUT,*)''
      DEALLOCATE( PENTAG,TAGNOS,ITAGMD )
      DEALLOCATE( IKINE1 )
c-----------
      RETURN
      END
