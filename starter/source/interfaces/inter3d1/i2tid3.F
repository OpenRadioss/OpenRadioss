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
Chd|  I2TID3                        source/interfaces/inter3d1/i2tid3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NOD2EL_MOD                    share/modules1/nod2el_mod.F   
Chd|====================================================================
      SUBROUTINE I2TID3(X     ,IRECT ,ST    ,MSR   ,NSV  ,
     2                  IRTL  ,ITAB  ,IKINE ,IKINE1,DMIN ,
     3                  IPARI ,TZINF ,IDDLEVEL,
     4                  ID,TITR,INTBUF_TAB  ,DSEARCH, IPROJ,
     5                  IXS,IXC,IXS10,IXS16,IXS20,STB ,
     6                  NSN_MULTI_CONNEC,T2_ADD_CONNEC,T2_NB_CONNEC,T2_CONNEC,IXTG)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE NOD2EL_MOD 
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr07_c.2.inc"
#include      "scr17_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), MSR(*), NSV(*),IRTL(*),
     .        ITAB(*),IKINE(*),IKINE1(*),IPARI(*),
     .        IXS(NIXS,*),IXC(NIXC,*),IXS10(6,*),IXS16(8,*),IXS20(12,*),IXTG(NIXTG,*)
      INTEGER  IDDLEVEL,IPROJ,NSN_MULTI_CONNEC,T2_ADD_CONNEC(*),T2_CONNEC(*),T2_NB_CONNEC(*)
C     REAL
      my_real
     .   X(3,*),ST(2,*),DMIN(*),TZINF,DSEARCH,STB(2,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,JJ,I,J,K,L,M,IGNORE,ILEV,NUVAR,IDEL7N,
     .   NSN, NMN,NSNU,NMNU,NRTM,INTTH,IDN,IIB,KK,COMMON_NODES,DOUBLON,IADD,IDIP
      INTEGER  CPT,N1,N2,N3,N4,FLAG_SOLID,FLAG_SHELL,NNOD,NB_LIST_COMPT,
     .         LIST_COMPT(2,NINTER),FOUND,FOUND_NOD(4)
      my_real
     .   TT,LB1,LC1,LA1,AAA
        INTEGER, DIMENSION(:), ALLOCATABLE :: TAGS,TAGM
C=======================================================================
      ALLOCATE( TAGS(NUMNOD),TAGM(NUMNOD) )
      NRTM   = IPARI(4)
      NSN    = IPARI(5)
      NMN    = IPARI(6)
      ILEV   = IPARI(20)
      IGNORE = IPARI(34)
      INTTH  = IPARI(47)
      LIST_COMPT = 0
      NB_LIST_COMPT = 0
C
      TAGS(1:NUMNOD) = 0
      L=0
C
      CPT = 0
      DO II=1,NSN
        I = NSV(II)
        L = IRTL(II)
C
        IF (ILEV /= 25 .and. ILEV /= 26 .and. ILEV /= 27 .and. ILEV /= 28 .and. L /= 0) THEN
          CALL KINSET(2,ITAB(I),IKINE(I),1,0,IKINE1(I))
          CALL KINSET(2,ITAB(I),IKINE(I),2,0,IKINE1(I))
          CALL KINSET(2,ITAB(I),IKINE(I),3,0,IKINE1(I))
          CALL KINSET(2,ITAB(I),IKINE(I),4,0,IKINE1(I))
          CALL KINSET(2,ITAB(I),IKINE(I),5,0,IKINE1(I))
          CALL KINSET(2,ITAB(I),IKINE(I),6,0,IKINE1(I))
        ENDIF
        IF (L == 0 .AND. IGNORE == 0) THEN
          CALL ANCMSG(MSGID=1078,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                R1=TZINF,
     .                I1=ITAB(I) ,
     .                PRMOD=MSG_CUMU)
        ELSEIF (L == 0 .AND. IGNORE >= 2 .AND. DSEARCH == 0) THEN
          CALL ANCMSG(MSGID=1071,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(I),
     .                PRMOD=MSG_CUMU)
        CPT = CPT + 1
        ELSEIF (L == 0 .AND. IGNORE >= 1) THEN
          CALL ANCMSG(MSGID=1157,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                R1=TZINF,
     .                I1=ITAB(I) ,
     .                PRMOD=MSG_CUMU)
        CPT = CPT + 1

        ELSEIF ((ILEV == 25 .OR. ((ILEV == 27).AND.(IRECT(3,L)/=IRECT(4,L))) .OR. ILEV == 26 .OR. ILEV == 28) .and. 
     .         (ST(1,II) > ONEP5 .OR. ST(2,II) > ONEP5 .OR.
     .          ST(1,II) <-ONEP5 .OR. ST(2,II) <-ONEP5)) THEN
            IRTL(II)=0 
            CALL ANCMSG(MSGID=1158,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ITAB(I),
     .                  I2=L,
     .                  I3=ITAB(IRECT(1,L)),
     .                  I4=ITAB(IRECT(2,L)),
     .                  I5=ITAB(IRECT(3,L)),
     .                  I6=ITAB(IRECT(4,L)),
     .                  R1= ST(1,II) ,
     .                  R2= ST(2,II) ,
     .                  R3=DMIN(II)  ,
     .                  PRMOD=MSG_CUMU)
        ELSEIF ((ILEV == 27).AND.(IRECT(3,L)==IRECT(4,L)) 
     .         .and.(ST(1,II) < -FOURTH .OR. ST(2,II) < -FOURTH .OR.
     .               ST(1,II)+ ST(2,II) > ONEP25)) THEN
C--         shape funnction in space of triangle - node removed
            IRTL(II)=0 
            CALL ANCMSG(MSGID=1872,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ITAB(I),
     .                  I2=L,
     .                  I3=ITAB(IRECT(1,L)),
     .                  I4=ITAB(IRECT(2,L)),
     .                  I5=ITAB(IRECT(3,L)),
     .                  R1= ST(1,II) ,
     .                  R2= ST(2,II) ,
     .                  R3=DMIN(II)  ,
     .                  PRMOD=MSG_CUMU)
        ELSEIF ((ILEV == 27).AND.(IRECT(3,L)==IRECT(4,L)) 
     .         .and.(ST(1,II) < -ZEP01 .OR. ST(2,II) < -ZEP01 .OR.
     .               ST(1,II) + ST(2,II) > ONEP01)) THEN
C--         shape funnction in space of triangle - warning node outside
            CALL ANCMSG(MSGID=1873,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ITAB(I),
     .                  I2=L,
     .                  I3=ITAB(IRECT(1,L)),
     .                  I4=ITAB(IRECT(2,L)),
     .                  I5=ITAB(IRECT(3,L)),
     .                  R1= ST(1,II) ,
     .                  R2= ST(2,II) ,
     .                  R3=DMIN(II)  ,
     .                  PRMOD=MSG_CUMU)
        ELSEIF (ST(1,II) > ONEP02 .OR. ST(2,II) > ONEP02 .OR.
     .          ST(1,II) <-ONEP02 .OR. ST(2,II) <-ONEP02) THEN
            CALL ANCMSG(MSGID=1079,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ITAB(I),
     .                  I2=L,
     .                  I3=ITAB(IRECT(1,L)),
     .                  I4=ITAB(IRECT(2,L)),
     .                  I5=ITAB(IRECT(3,L)),
     .                  I6=ITAB(IRECT(4,L)),
     .                  R1= ST(1,II) ,
     .                  R2= ST(2,II) ,
     .                  R3=DMIN(II)  ,
     .                  PRMOD=MSG_CUMU)
        ELSE
          IF ((ILEV==27).and.(IRECT(3,L)==IRECT(4,L))) THEN
C--         specific printout for triangles
            TAGS(I) = 2
          ELSE
            TAGS(I) = 1
          ENDIF
        ENDIF
      ENDDO
C
C----------------------------------------------
C     printout of tied connections
C----------------------------------------------
C 
      IF (IPRI > 0) THEN
        IF (ILEV /= 27) THEN
          WRITE(IOUT,2022)
          DO II=1,NSN
            I = NSV(II)
            L = IRTL(II)
            IF (TAGS(I) == 1) WRITE(IOUT,'(6I10,2F8.4,1PG20.13)') ITAB(I),L,(ITAB(IRECT(JJ,L)),JJ=1,4),ST(1,II),ST(2,II),DMIN(II)
          ENDDO
        ELSE
C--       printout for quadrangles
          WRITE(IOUT,2023)
          DO II=1,NSN
            I = NSV(II)
            L = IRTL(II)
            IF (TAGS(I) == 1) WRITE(IOUT,'(6I10,2F8.4,1PG20.13)') ITAB(I),L,(ITAB(IRECT(JJ,L)),JJ=1,4),ST(1,II),ST(2,II),DMIN(II)
          ENDDO
C--       printout for triangles
          WRITE(IOUT,2024)
          DO II=1,NSN
            I = NSV(II)
            L = IRTL(II)
            IF (TAGS(I) == 2) WRITE(IOUT,'(5I10,2F8.4,1PG20.13)') ITAB(I),L,(ITAB(IRECT(JJ,L)),JJ=1,3),ST(1,II),ST(2,II),DMIN(II) 
          ENDDO
        ENDIF
      ENDIF
C
      DO II=1,NSN
        DMIN(II) = 0
      ENDDO
C
      CALL ANCMSG(MSGID=1071,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR ,
     .                PRMOD=MSG_PRINT )

      IF(CPT == NSN) THEN
        IF (L == 0 .AND. IGNORE >= 2 .AND. DSEARCH == 0) THEN
        CALL ANCMSG(MSGID=1217,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)


        ELSEIF (L == 0 .AND. IGNORE >= 1) THEN

        CALL ANCMSG(MSGID=1218,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)
        ENDIF
      ENDIF



      CALL ANCMSG(MSGID=1078,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR ,
     .                PRMOD=MSG_PRINT)


      CALL ANCMSG(MSGID=1079,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR ,
     .                PRMOD=MSG_PRINT )

      CALL ANCMSG(MSGID=1873,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR ,
     .                PRMOD=MSG_PRINT )

      CALL ANCMSG(MSGID=1157,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR ,
     .                PRMOD=MSG_PRINT )

      CALL ANCMSG(MSGID=1158,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR ,
     .                PRMOD=MSG_PRINT )

      CALL ANCMSG(MSGID=1872,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR ,
     .                PRMOD=MSG_PRINT )
C
C----------------------------------------------
c     tag valid SECONDARY and MAIN nodes
c----------------------------------------------
C
      TAGS(1:NUMNOD) = 0
C
      DO I = 1, NMN        
        TAGM(MSR(I)) = 0                  
      ENDDO
C                          
      DO II = 1,NSN                  
        I = NSV(II)                  
        J = IRTL(II)            
        IF (I > 0 .AND. J > 0) THEN  
          TAGS(II) = 1               
          DO K = 1, 4                
            M = IRECT(K,J)           
            IF (M > 0) TAGM(M) = 1              
          ENDDO                      
        ENDIF                        
      ENDDO
C----------------------------------------------
c     Check if the same MAIN/SECONDARY connection is defined in several T2 interfaces
c----------------------------------------------
      IF (((ILEV == 27).OR.(ILEV == 28)).AND.(NSN_MULTI_CONNEC > 0)) THEN
C
        DO II = 1,NSN                  
          I = NSV(II)                  
          J = IRTL(II)         
          IF ((TAGS(II) == 1).AND.(T2_NB_CONNEC(I)>1)) THEN
            IADD = T2_ADD_CONNEC(I)
            DOUBLON = 0  
C
            DO IDIP=1,T2_CONNEC(IADD)
C--           loop on already stored connections if any          
              COMMON_NODES = 0              
              DO K = 1, 4
                DO KK = 1,4                
                    IF (T2_CONNEC(IADD+5*(IDIP-1)+K) == IRECT(KK,J)) COMMON_NODES = COMMON_NODES + 1
                ENDDO 
              ENDDO
              IF (COMMON_NODES == 4) THEN
C--             connection deactivated - doublon
                TAGS(II) = 0
                DOUBLON = 1
                DO K = 1, 4                
                  M = IRECT(K,J)           
                  IF (M > 0) TAGM(M) = 0              
                ENDDO
C--             List of interfaces for output of common connections
                FOUND = 0
                DO K=1,NB_LIST_COMPT
                  IF (LIST_COMPT(1,K)==T2_CONNEC(IADD+5*(IDIP-1)+5)) FOUND=K
                ENDDO
                IF (FOUND == 0) THEN
                  NB_LIST_COMPT = NB_LIST_COMPT + 1
                  LIST_COMPT(1,NB_LIST_COMPT)=T2_CONNEC(IADD+5*(IDIP-1)+5)
                  LIST_COMPT(2,NB_LIST_COMPT)= 1
                ELSE
                  LIST_COMPT(2,FOUND) = LIST_COMPT(2,FOUND) + 1
                ENDIF
                EXIT     
              ENDIF
            ENDDO
C
            IF (DOUBLON == 0) THEN
C--           New connection - is stored
              IDIP = T2_CONNEC(IADD)   
              T2_CONNEC(IADD) = T2_CONNEC(IADD) + 1              
              DO K = 1, 3                
                T2_CONNEC(IADD+5*IDIP+K) = IRECT(K,J)
              ENDDO
              IF (IRECT(3,J) /= IRECT(4,J)) T2_CONNEC(IADD+5*IDIP+4) = IRECT(4,J)
              T2_CONNEC(IADD+5*IDIP+5) = ID
            ENDIF
C                     
          ENDIF                        
        ENDDO
C
        IF (NB_LIST_COMPT > 0) THEN
C------   Warning printout --------------
          DO I = 1,NB_LIST_COMPT
            CALL ANCMSG(MSGID=1630,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=LIST_COMPT(2,I),
     .                  I2=LIST_COMPT(1,I),
     .                  PRMOD=MSG_CUMU)
          ENDDO
C
          CALL ANCMSG(MSGID=1630,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                PRMOD=MSG_PRINT)
        ENDIF
C
      ENDIF
C----------------------------------------------
c     projection on edges for valid SECONDARY nodes outside of MAIN element
c----------------------------------------------
      IF (IPROJ == 1 .and. ILEV/=1 .and. ILEV/=30 .and. ILEV/=28) THEN
C---    Projection on edges is used only for the distribution of masses and inertia to avoid negative masses / inertia on MAIN nodes
        DO II= 1,NSN
          IF (TAGS(II) == 1) THEN
           J = IRTL(II)
           IF (IRECT(3,J)/=IRECT(4,J)) THEN
C--        square
             STB(1,II)= MIN(ONE,MAX(-1*ONE,ST(1,II)))
             STB(2,II)= MIN(ONE,MAX(-1*ONE,ST(2,II)))
           ELSE
C--        triangle
             STB(1,II)= ST(1,II)
             STB(2,II)= ST(2,II)
             IF (ILEV == 27) THEN
C--            hape functions in space of the triangle
               LB1=ST(1,II)
               LC1=ST(2,II)
             ELSE
C--            shape functions in space of quadrangle
               LB1=FOURTH*(ONE - ST(2,II))*(ONE - ST(1,II))
               LC1=FOURTH*(ONE - ST(2,II))*(ONE + ST(1,II))
             ENDIF
             LA1= ONE - LB1 - LC1
             IF(LA1 < ZERO .or. LB1 < ZERO .or. LC1 < ZERO)THEN
               IF(LA1<ZERO.and.LB1<ZERO)THEN
                 LA1 = ZERO
                 LB1 = ZERO
                 LC1 = ONE
               ELSEIF(LB1<ZERO.and.LC1<ZERO)THEN
                 LB1 = ZERO
                 LC1 = ZERO
                 LA1 = ONE
               ELSEIF(LC1<ZERO.and.LA1<ZERO)THEN
                 LC1 = ZERO
                 LA1 = ZERO
                 LB1 = ONE
               ELSEIF(LA1<ZERO)THEN
                 LA1 = ZERO
                 AAA = LB1 + LC1
                 LB1 = LB1/AAA
                 LC1 = LC1/AAA
               ELSEIF(LB1<ZERO)THEN
                 LB1 = ZERO
                 AAA = LC1 + LA1
                 LC1 = LC1/AAA
                 LA1 = LA1/AAA
               ELSEIF(LC1<ZERO)THEN
                 LC1 = ZERO
                 AAA = LA1 + LB1
                 LA1 = LA1/AAA
                 LB1 = LB1/AAA
               ENDIF
C
               IF (ILEV == 27) THEN
C--            shape functions in space of the triangle
                 STB(1,II) = LB1
                 STB(2,II) = LC1
               ELSE
C--            shape functions in space of the quadrangle
                 STB(2,II) = ONE - TWO*LB1 - TWO*LC1
                 IF (STB(2,II) < ONE-EM10) THEN
                   STB(1,II)= (LC1-LB1)/(LC1+LB1)            
                 ELSEIF (LB1 < -EM10) THEN
                   STB(1,II)= TWO
                 ELSEIF (LC1 < -EM10) THEN
                   STB(1,II)= -TWO
                 ELSE
                   STB(1,II)= ZERO
                 ENDIF
               ENDIF
C
             END IF
           ENDIF
         ENDIF
        ENDDO
      ELSEIF (IPROJ == 3 .and. ILEV/=1 .and. ILEV/=30 .and. ILEV/=28) THEN
C---    Projection on edges - used only for retrocompatibility with old iproj=1 flag
        DO II= 1,NSN
          IF (TAGS(II) == 1) THEN
           J = IRTL(II)
           IF (IRECT(3,J)/=IRECT(4,J)) THEN
C--        square
             ST(1,II)= MIN(ONE,MAX(-1*ONE,ST(1,II)))
             ST(2,II)= MIN(ONE,MAX(-1*ONE,ST(2,II)))
           ELSE
C--        triangle
             IF (ILEV == 27) THEN
C--            shape functions in space of the triangle
               LB1=ST(1,II)
               LC1=ST(2,II)
             ELSE
C--            shape functions in space of quadrangle
               LB1=FOURTH*(ONE - ST(2,II))*(ONE - ST(1,II))
               LC1=FOURTH*(ONE - ST(2,II))*(ONE + ST(1,II))
             ENDIF
             LA1= ONE - LB1 - LC1
             IF(LA1 < ZERO .or. LB1 < ZERO .or. LC1 < ZERO)THEN
               IF(LA1<ZERO.and.LB1<ZERO)THEN
                 LA1 = ZERO
                 LB1 = ZERO
                 LC1 = ONE
               ELSEIF(LB1<ZERO.and.LC1<ZERO)THEN
                 LB1 = ZERO
                 LC1 = ZERO
                 LA1 = ONE
               ELSEIF(LC1<ZERO.and.LA1<ZERO)THEN
                 LC1 = ZERO
                 LA1 = ZERO
                 LB1 = ONE
               ELSEIF(LA1<ZERO)THEN
                 LA1 = ZERO
                 AAA = LB1 + LC1
                 LB1 = LB1/AAA
                 LC1 = LC1/AAA
               ELSEIF(LB1<ZERO)THEN
                 LB1 = ZERO
                 AAA = LC1 + LA1
                 LC1 = LC1/AAA
                 LA1 = LA1/AAA
               ELSEIF(LC1<ZERO)THEN
                 LC1 = ZERO
                 AAA = LA1 + LB1
                 LA1 = LA1/AAA
                 LB1 = LB1/AAA
               ENDIF
C
               IF (ILEV == 27) THEN
C--            shape functions in space of the triangle
                 ST(1,II) = LB1
                 ST(2,II) = LC1
               ELSE
C--            shape functions in space of the quadrangle
                 ST(2,II) = ONE - TWO*LB1 - TWO*LC1
                 IF (ST(2,II) < ONE-EM10) THEN
                   ST(1,II)= (LC1-LB1)/(LC1+LB1)            
                 ELSEIF (LB1 < -EM10) THEN
                   ST(1,II)= TWO
                 ELSEIF (LC1 < -EM10) THEN
                   ST(1,II)= -TWO
                 ELSE
                   ST(1,II)= ZERO
                 ENDIF
               ENDIF
C
             END IF
           ENDIF
           STB(1,II)=ST(1,II)
           STB(2,II)=ST(2,II)
         ENDIF
        ENDDO
      ELSE
        DO II= 1,NSN
           STB(1,II)=ST(1,II)
           STB(2,II)=ST(2,II)          
        ENDDO
      ENDIF
C     
C----------------------------------------------
c     Update NSN and MNM                        
C----------------------------------------------
      NSNU = 0                                  
      DO I = 1,NSN
        IF (TAGS(I) == 1) THEN               
          NSNU = NSNU+1   
          INTBUF_TAB%NSV(NSNU) = INTBUF_TAB%NSV(I)
        ENDIF                                
      ENDDO                                  
C
      NMNU = 0                                  
      DO I = 1, NMN                              
        M = MSR(I)                                
        IF (TAGM(M) == 1) THEN                   
          NMNU = NMNU+1                            
          INTBUF_TAB%MSR(NMNU) = INTBUF_TAB%MSR(I)    
        ENDIF                                    
      ENDDO
      IPARI(5) = NSNU
      IPARI(6) = NMNU
C-----------------------------------------------
C     Compact INT BUFFER
C-----------------------------------------------
C
      J = 0
      DO I = 1,NSN
        IF (TAGS(I) == 1) THEN                  
          J=J+1
          INTBUF_TAB%IRTLM(J) = INTBUF_TAB%IRTLM(I)
        ENDIF
      ENDDO
      IF (ILEV == 10 .OR. ILEV == 11 .OR. ILEV == 12 .OR.
     .    ILEV == 20 .OR. ILEV == 21 .OR. ILEV == 22) THEN
        J = 0
        DO I = 1,NSN
          IF (TAGS(I) == 1) THEN
            J = J+1
            INTBUF_TAB%IRUPT(J) = INTBUF_TAB%IRUPT(I)
          ENDIF
        ENDDO
      ELSEIF ((ILEV == 27).OR.(ILEV == 28)) THEN
        J = 0
        DO I = 1,NSN
          IF (TAGS(I) == 1) THEN
            J = J+1
            INTBUF_TAB%IRUPT(J) = INTBUF_TAB%IRUPT(I)
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
C       Compact REAL BUFFER
C-----------------------------------------------
        IDEL7N = IPARI(17)                
        NUVAR  = IPARI(35)
C-----------------------------------------------
        J = 0
        DO I= 1,NSN
          IF (TAGS(I) == 1) THEN
            J = J+1
            INTBUF_TAB%CSTS(1+2*(J-1))   = INTBUF_TAB%CSTS(1+2*(I-1))
            INTBUF_TAB%CSTS(1+2*(J-1)+1) = INTBUF_TAB%CSTS(1+2*(I-1)+1)
            INTBUF_TAB%CSTS_BIS(1+2*(J-1))   = INTBUF_TAB%CSTS_BIS(1+2*(I-1))
            INTBUF_TAB%CSTS_BIS(1+2*(J-1)+1) = INTBUF_TAB%CSTS_BIS(1+2*(I-1)+1)
          ENDIF
        ENDDO
        J = 0
        DO I = 1,NSN
          IF (TAGS(I) == 1) THEN
            J=J+1
            INTBUF_TAB%DPARA(1+7*(J-1))   = INTBUF_TAB%DPARA(1+7*(I-1))
            INTBUF_TAB%DPARA(1+7*(J-1)+1) = INTBUF_TAB%DPARA(1+7*(I-1)+1)
            INTBUF_TAB%DPARA(1+7*(J-1)+2) = INTBUF_TAB%DPARA(1+7*(I-1)+2)
            INTBUF_TAB%DPARA(1+7*(J-1)+3) = INTBUF_TAB%DPARA(1+7*(I-1)+3)
            INTBUF_TAB%DPARA(1+7*(J-1)+4) = INTBUF_TAB%DPARA(1+7*(I-1)+4)
            INTBUF_TAB%DPARA(1+7*(J-1)+5) = INTBUF_TAB%DPARA(1+7*(I-1)+5)
            INTBUF_TAB%DPARA(1+7*(J-1)+6) = INTBUF_TAB%DPARA(1+7*(I-1)+6)
          ENDIF
        ENDDO
        J = 0
        DO I = 1,NMN
          IF (TAGM(MSR(I)) == 1) THEN
            J=J+1
            INTBUF_TAB%NMAS(J)      = INTBUF_TAB%NMAS(I)
            INTBUF_TAB%NMAS(NMNU+J) = INTBUF_TAB%NMAS(NMN+I)
          ENDIF
        ENDDO
        IF (IDEL7N /= 0)THEN
          J = 0
          DO I = 1,NSN
            IF (TAGS(I) == 1) THEN
              J=J+1
              INTBUF_TAB%SMAS(J)  = INTBUF_TAB%SMAS(I)
              INTBUF_TAB%SINER(J) = INTBUF_TAB%SINER(I)
            ENDIF
          ENDDO
        ENDIF
C-----------------------------------------------
        IF (ILEV==10 .OR. ILEV==11 .OR. ILEV==12 .OR. ILEV==20 .OR. 
     .      ILEV==21 .OR. ILEV==22 .OR. INTTH > 0) THEN
          J = 0
          DO I = 1,NSN
            IF (TAGS(I) == 1) THEN
              J=J+1
              INTBUF_TAB%AREAS2(J) = INTBUF_TAB%AREAS2(I)
              DO K = 0,NUVAR-1
                INTBUF_TAB%UVAR(1+NUVAR*(J-1)+K) = 
     .                     INTBUF_TAB%UVAR(1+NUVAR*(I-1)+K)
              ENDDO                                  
            ENDIF
          ENDDO   
        ENDIF
        IF (ILEV==10 .OR. ILEV==11 .OR. ILEV==12) THEN
          J = 0
          DO I = 1,NSN
            IF (TAGS(I) == 1) THEN
              J=J+1
              INTBUF_TAB%SMAS(J)  = INTBUF_TAB%SMAS(I)
              INTBUF_TAB%SINER(J) = INTBUF_TAB%SINER(I)
c              INTBUF_TAB%AREAS2(J) = INTBUF_TAB%AREAS2(I)
              DO K = 0,NUVAR-1
                INTBUF_TAB%UVAR(1+NUVAR*(J-1)+K) = 
     .                     INTBUF_TAB%UVAR(1+NUVAR*(I-1)+K)
              ENDDO                       
              DO K = 0,2
                INTBUF_TAB%XM0(1+3*(J-1)+K) = INTBUF_TAB%XM0(1+3*(I-1)+K)
                INTBUF_TAB%DSM(1+3*(J-1)+K) = INTBUF_TAB%DSM(1+3*(I-1)+K)
                INTBUF_TAB%FSM(1+3*(J-1)+K) = INTBUF_TAB%FSM(1+3*(I-1)+K)
              ENDDO             
            ENDIF
          ENDDO          
        ELSEIF (ILEV==20 .OR. ILEV==21 .OR. ILEV==22) THEN
          J = 0
          DO I = 1,NSN                          
            IF (TAGS(I) == 1) THEN              
              J = J+1                              
              INTBUF_TAB%SMAS(J)  = INTBUF_TAB%SMAS(I)  
              INTBUF_TAB%SINER(J) = INTBUF_TAB%SINER(I)  
c              INTBUF_TAB%AREAS2(J) = INTBUF_TAB%AREAS2(I)
              DO K = 0,NUVAR-1
                INTBUF_TAB%UVAR(1+NUVAR*(J-1)+K) = 
     .            INTBUF_TAB%UVAR(1+NUVAR*(I-1)+K)
              ENDDO                       
              DO K = 0,2
                INTBUF_TAB%XM0(1+3*(J-1)+K) = INTBUF_TAB%XM0(1+3*(I-1)+K)
                INTBUF_TAB%DSM(1+3*(J-1)+K) = INTBUF_TAB%DSM(1+3*(I-1)+K)
                INTBUF_TAB%FSM(1+3*(J-1)+K) = INTBUF_TAB%FSM(1+3*(I-1)+K)
              ENDDO             
            ENDIF                               
          ENDDO  
          DO K = 0,5                                 
            INTBUF_TAB%RUPT(1+K) = INTBUF_TAB%RUPT(1+K)             
          ENDDO                                      
        ELSEIF (ILEV == 25) THEN
          J = 0
          DO I = 1,NSN
            IF (TAGS(I) == 1) THEN              
              J = J+1                             
              INTBUF_TAB%SMAS(J)         = INTBUF_TAB%SMAS(I)
              INTBUF_TAB%SINER(J)        = INTBUF_TAB%SINER(I)
              INTBUF_TAB%SPENALTY(J)     = INTBUF_TAB%SPENALTY(I)
              INTBUF_TAB%STFR_PENALTY(J) = INTBUF_TAB%STFR_PENALTY(I)
              DO K = 0,8
                INTBUF_TAB%SKEW(1+9*(J-1)+K) = INTBUF_TAB%SKEW(1+9*(I-1)+K)
              ENDDO                       
              DO K = 0,2
                INTBUF_TAB%DSM(1+3*(J-1)+K)  = INTBUF_TAB%DSM(1+3*(I-1)+K)
                INTBUF_TAB%FSM(1+3*(J-1)+K)  = INTBUF_TAB%FSM(1+3*(I-1)+K)
                INTBUF_TAB%FINI(1+3*(J-1)+K) = INTBUF_TAB%FINI(1+3*(I-1)+K)
              ENDDO             
            ENDIF                               
          ENDDO                    
        ELSEIF (ILEV == 26) THEN
          J = 0
          DO I = 1,NSN
            IF (TAGS(I) == 1) THEN              
              J = J+1                             
              INTBUF_TAB%SMAS(J)         = INTBUF_TAB%SMAS(I)
              INTBUF_TAB%SINER(J)        = INTBUF_TAB%SINER(I)
              INTBUF_TAB%SPENALTY(J)     = INTBUF_TAB%SPENALTY(I)
              INTBUF_TAB%STFR_PENALTY(J) = INTBUF_TAB%STFR_PENALTY(I)
              DO K = 0,8
                INTBUF_TAB%SKEW(1+9*(J-1)+K) = INTBUF_TAB%SKEW(1+9*(I-1)+K)
              ENDDO
              DO K = 0,11
                INTBUF_TAB%DSM(1+12*(J-1)+K) = INTBUF_TAB%DSM(1+12*(I-1)+K)
                INTBUF_TAB%FSM(1+12*(J-1)+K) = INTBUF_TAB%FSM(1+12*(I-1)+K)
              ENDDO             
              DO K = 0,23
                INTBUF_TAB%FINI(1+24*(J-1)+K) = INTBUF_TAB%FINI(1+24*(I-1)+K)
              ENDDO             
            ENDIF                               
          ENDDO                    
        ELSEIF (ILEV == 27) THEN
          J = 0
          DO I = 1,NSN
            IF (TAGS(I) == 1) THEN              
              J = J+1                             
              INTBUF_TAB%SMAS(J)         = INTBUF_TAB%SMAS(I)
              INTBUF_TAB%SINER(J)        = INTBUF_TAB%SINER(I)
              INTBUF_TAB%SPENALTY(J)     = INTBUF_TAB%SPENALTY(I)
              INTBUF_TAB%STFR_PENALTY(J) = INTBUF_TAB%STFR_PENALTY(I)
              DO K = 0,8
                INTBUF_TAB%SKEW(1+9*(J-1)+K) = INTBUF_TAB%SKEW(1+9*(I-1)+K)
              ENDDO                       
              DO K = 0,2
                INTBUF_TAB%DSM(1+3*(J-1)+K)  = INTBUF_TAB%DSM(1+3*(I-1)+K)
                INTBUF_TAB%FSM(1+3*(J-1)+K)  = INTBUF_TAB%FSM(1+3*(I-1)+K)
                INTBUF_TAB%FINI(1+3*(J-1)+K) = INTBUF_TAB%FINI(1+3*(I-1)+K)
              ENDDO             
            ENDIF                               
          ENDDO
C
C-----------------------------------------------
C--- Check MAIN segment type :
C           solid MAIN surface -> MSEGTYP2 = 0
C           shell MAIN surface -> MSEGTYP2 = 1
C           coating shell treated as solid MAIN surface -> MSEGTYP2 = 0
C-----------------------------------------------
C
            DO I = 1,NRTM
              INTBUF_TAB%MSEGTYP2(I) = 0 
              N1 = IRECT(1,I)
              N2 = IRECT(2,I)
              N3 = IRECT(3,I)
              N4 = IRECT(4,I)
              IF(N4 == 0) N4 = N3
C-->          Check of solids elements connected to 1st node
C             Warning : if the segment is inside the solid it's also considered as a solid segment
              FLAG_SOLID = 0
              DO J = KNOD2ELS(N1)+1,KNOD2ELS(N1+1)
                II = NOD2ELS(J)
                FOUND_NOD(1)=1
                FOUND_NOD(2:4)=0
                DO K = 2,9
                  IF (IXS(K,II)==N2) FOUND_NOD(2) = 1
                  IF (IXS(K,II)==N3) FOUND_NOD(3) = 1
                  IF (IXS(K,II)==N4) FOUND_NOD(4) = 1
                END DO
                IF ((II>NUMELS8).AND.(II<=NUMELS8+NUMELS10)) THEN
                  IIB = II-NUMELS8
                  DO K = 1,6
                    IF (IXS10(K,IIB)==N2) FOUND_NOD(2) = 1
                    IF (IXS10(K,IIB)==N3) FOUND_NOD(3) = 1
                    IF (IXS10(K,IIB)==N4) FOUND_NOD(4) = 1
                  END DO
                ELSEIF ((II>NUMELS8+NUMELS10).AND.(II<= NUMELS8+NUMELS10+NUMELS16)) THEN
                  IIB = II-NUMELS8-NUMELS10
                  DO K = 1,8
                    IF (IXS16(K,IIB)==N2) FOUND_NOD(2) = 1
                    IF (IXS16(K,IIB)==N3) FOUND_NOD(3) = 1
                    IF (IXS16(K,IIB)==N4) FOUND_NOD(4) = 1
                  END DO
                ELSEIF (II>NUMELS8+NUMELS10+NUMELS16) THEN
                  IIB = II-NUMELS8-NUMELS10-NUMELS16
                  DO K = 1,12
                    IF (IXS20(K,IIB)==N2) FOUND_NOD(2) = 1
                    IF (IXS20(K,IIB)==N3) FOUND_NOD(3) = 1
                    IF (IXS20(K,IIB)==N4) FOUND_NOD(4) = 1
                  END DO
                ENDIF
                NNOD = FOUND_NOD(1)+FOUND_NOD(2)+FOUND_NOD(3)+FOUND_NOD(4)
                IF (NNOD == 4) FLAG_SOLID = 1
              ENDDO
C-->          Check of shells elements connected to 1st node
              FLAG_SHELL = 0
              DO J = KNOD2ELC(N1)+1,KNOD2ELC(N1+1)
                II = NOD2ELC(J)
                FOUND_NOD(1)=1
                FOUND_NOD(2:4)=0
                DO K = 2,5
                  IF (IXC(K,II)==N2) FOUND_NOD(2) = 1
                  IF (IXC(K,II)==N3) FOUND_NOD(3) = 1
                  IF (IXC(K,II)==N4) FOUND_NOD(4) = 1
                END DO
                NNOD = FOUND_NOD(1)+FOUND_NOD(2)+FOUND_NOD(3)+FOUND_NOD(4)
                IF (NNOD == 4) FLAG_SHELL = 1
              ENDDO
              DO J = KNOD2ELTG(N1)+1,KNOD2ELTG(N1+1)
                II = NOD2ELTG(J)
                FOUND_NOD(1)=1
                FOUND_NOD(2:4)=0
                DO K = 2,4
                  IF (IXTG(K,II)==N2) FOUND_NOD(2) = 1
                  IF (IXTG(K,II)==N3) FOUND_NOD(3) = 1
                  IF (IXTG(K,II)==N4) FOUND_NOD(4) = 1
                END DO
                NNOD = FOUND_NOD(1)+FOUND_NOD(2)+FOUND_NOD(3)+FOUND_NOD(4)
                IF (NNOD == 4) FLAG_SHELL = 1
              ENDDO
C-->
              IF ((FLAG_SHELL == 1).AND.(FLAG_SOLID == 0)) THEN
C-->            shell MAIN segment
                INTBUF_TAB%MSEGTYP2(I) = 1
              ELSE
C-->            solid MAIN segment or coating shell
                INTBUF_TAB%MSEGTYP2(I) = 0
              ENDIF
            ENDDO
C
        ELSEIF (ILEV == 28) THEN
          J = 0
          DO I = 1,NSN
            IF (TAGS(I) == 1) THEN              
              J = J+1                             
              INTBUF_TAB%SMAS(J)         = INTBUF_TAB%SMAS(I)
              INTBUF_TAB%SINER(J)        = INTBUF_TAB%SINER(I)
              INTBUF_TAB%SPENALTY(J)     = INTBUF_TAB%SPENALTY(I)
              INTBUF_TAB%STFR_PENALTY(J) = INTBUF_TAB%STFR_PENALTY(I)
              DO K = 0,8
                INTBUF_TAB%SKEW(1+9*(J-1)+K) = INTBUF_TAB%SKEW(1+9*(I-1)+K)
              ENDDO                       
              DO K = 0,2
                INTBUF_TAB%DSM(1+3*(J-1)+K)  = INTBUF_TAB%DSM(1+3*(I-1)+K)
                INTBUF_TAB%FSM(1+3*(J-1)+K)  = INTBUF_TAB%FSM(1+3*(I-1)+K)
                INTBUF_TAB%FINI(1+3*(J-1)+K) = INTBUF_TAB%FINI(1+3*(I-1)+K)
              ENDDO             
            ENDIF                               
          ENDDO                      
        ENDIF
C-----------
 1000 FORMAT(
     +  /,
     + '  SECONDARY NODE        NEAREST SEGMENT                    MAIN NODES',
     +'                                S                  T                ',
     +'    DIST'/
     +  /)
 2022 FORMAT(//
     +'     SECONDARY  NEAREST                  MAIN NODES                       SECONDARY '/
     +'     NODE   SEGMENT                                              S       T        DIST')
 2023 FORMAT(//'     PROJECTION ON 4 NODES SEGMENTS '//
     +'     SECONDARY  NEAREST                  MAIN NODES                       SECONDARY '/
     +'     NODE   SEGMENT                                              S       T        DIST')
 2024 FORMAT(//'     PROJECTION ON 3 NODES SEGMENTS '//
     +'     SECONDARY  NEAREST        MAIN NODES                       SECONDARY '/
     +'     NODE       SEGMENT                                S       T        DIST')   	    
C-----------

      DEALLOCATE( TAGS,TAGM )
      RETURN
      END
