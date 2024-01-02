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
Chd|  MOD_SPBUC3                    source/elements/sph/spbuc3.F  
Chd|-- called by -----------
Chd|        SPBUC3                        source/elements/sph/spbuc3.F  
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MOD_SPBUC3
      implicit none
      INTEGER*8, DIMENSION(:), ALLOCATABLE ::  IV
      INTEGER, DIMENSION(:), ALLOCATABLE ::  KV,NV,IVS,
     .                                       IAUX,KXSPR
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IXSPR

      END MODULE MOD_SPBUC3
Chd|====================================================================
Chd|  SPBUC3                        source/elements/sph/spbuc3.F  
Chd|-- called by -----------
Chd|        SPHTRI                        source/elements/sph/sphtri.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPTRIVOX                      source/elements/sph/sptrivox.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MOD_SPBUC3                    source/elements/sph/spbuc3.F  
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|        SPH_STRUCT_MOD                share/modules/sph_struct_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPBUC3(X     ,KXSP   ,IXSP  ,NOD2SP,NSN   ,
     2                  SPBUF ,MA     ,JVOIS ,JSTOR ,JPERM ,
     3                  DVOIS ,IREDUCE,BMINMA,NSNR  ,NSP2SORTF,
     4                  NSP2SORTL,ITASK,KREDUCE,LGAUGE ,GAUGE ) 
C============================================================================
C   M o d u l e s
C-----------------------------------------------
        USE TRI7BOX
        USE SPHBOX
        USE MOD_SPBUC3
        USE MESSAGE_MOD
        USE SPH_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NSNR,NSP2SORTF,NSP2SORTL
C     REAL
      INTEGER KXSP(NISP,*), IXSP(KVOISPH,*), NOD2SP(*),
     .   MA(*), JVOIS(*), JSTOR(*), JPERM(*), IREDUCE,ITASK,
     .   KREDUCE(*),LGAUGE(3,*)
C     REAL
      my_real
     .   X(3,*),SPBUF(NSPBUF,*),DVOIS(*), BMINMA(12), GAUGE(LLGAUGE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L, N0, N1, N2, N3, N4, I, J, NBMX, N11, N12, N14, N,
     .  INOD,JNOD,NVOIS,NVOIS1,NVOIS2,K,M,NN,NS,MS,JK,IERROR,JTASK,
     .  MY_ADRV,NVOISS
C     REAL
      my_real
     .        MARGE, AAA, 
     .        AAA2, XI,YI,ZI,XJ,YJ,ZJ, D2,D1X,D1Y,D1Z,DK
      INTEGER NBX,NBY,NBZ
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8
C-----------------------------------------------
c!!!!      MARGE = TZINF-MAX(GAP,PMAX_GAP)  ! il s agit bien de la marge

      AAA = SQRT(NSN /
     .           ((BMINMA(7)-BMINMA(10))*(BMINMA(8)-BMINMA(11))
     .           +(BMINMA(8)-BMINMA(11))*(BMINMA(9)-BMINMA(12))
     .           +(BMINMA(9)-BMINMA(12))*(BMINMA(7)-BMINMA(10))))

      AAA = 0.75*AAA

      NBX = NINT(AAA*(BMINMA(7)-BMINMA(10)))
      NBY = NINT(AAA*(BMINMA(8)-BMINMA(11)))
      NBZ = NINT(AAA*(BMINMA(9)-BMINMA(12)))
      NBX = MAX(NBX,1)
      NBY = MAX(NBY,1)
      NBZ = MAX(NBZ,1)

      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      LVOXEL8 = LVOXEL      

      IF(RES8 > LVOXEL8) THEN
        AAA = LVOXEL
        AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
        AAA = AAA**(THIRD)
        NBX = INT((NBX+2)*AAA)-2
        NBY = INT((NBY+2)*AAA)-2
        NBZ = INT((NBZ+2)*AAA)-2
        NBX = MAX(NBX,1)
        NBY = MAX(NBY,1)
        NBZ = MAX(NBZ,1)
      ENDIF
      
      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      
      IF(RES8 > LVOXEL8) THEN
        NBX = MIN(100,MAX(NBX8,1))
        NBY = MIN(100,MAX(NBY8,1))
        NBZ = MIN(100,MAX(NBZ8,1))
      ENDIF

C     initialisation complete de VOXEL
C (en // SMP il y a possibilite de redondance de traitement mais no pb)
      DO I=INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)
        VOXEL1(I)=0
      ENDDO
      INIVOXEL = MAX(INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)+1)

      IF(ITASK==0)THEN
        ALLOCATE(KXSPR(NSNR),IXSPR(KVOISPH,NSNR),STAT=IERROR)
        IF(IERROR/=0)THEN
          CALL ANCMSG(MSGID=248,ANMODE=ANINFO_BLIND)
          CALL ARRET(2)
        END IF
        KXSPR(1:NSNR)=0
      END IF
C
      CALL MY_BARRIER
C--------------------------------------------------
C     VOXEL SORT
C--------------------------------------------------
      CALL SPTRIVOX(
     1      NSN     ,NSNR   ,X       ,BMINMA  ,NOD2SP ,
     2      NBX     ,NBY    ,NBZ     ,MARGE   ,ITASK  ,
     3      MA      ,SPBUF  ,JVOIS   ,JSTOR   ,JPERM  ,
     4      DVOIS   ,IREDUCE,NSP2SORTF,NSP2SORTL,VOXEL1 ,
     5      KXSP    ,IXSP   ,KREDUCE ,LGAUGE  ,GAUGE  ,
     6      KXSPR   ,IXSPR  )

C--------------------------------------------------
C     SYMETRISE VOISINS
C--------------------------------------------------
c     CALL MY_BARRIER fait dans SPTRIVOX
      IF(ITASK==0)THEN

        ALLOCATE(NV(NUMSPH*NTHREAD),IV(NUMSPH*NTHREAD),IVS(NUMSPH),
     .           KV(NUMSPH),IAUX(KVOISPH*(NUMSPH+NSNR)),
     .           STAT=IERROR)

        IF(IERROR/=0)THEN
          CALL ANCMSG(MSGID=248,ANMODE=ANINFO_BLIND)
          CALL ARRET(2)
        END IF
      END IF
C
      CALL MY_BARRIER
C
      NV(ITASK*NUMSPH+1:(ITASK+1)*NUMSPH)=0

      DO NS=NSP2SORTF,NSP2SORTL
        N     =MA(NS)
        KV(N) =NS
      END DO
C-------        
C
      CALL MY_BARRIER
C
C-------        
      MY_ADRV=ITASK*NUMSPH
      DO NS=NSP2SORTF,NSP2SORTL
        N =MA(NS)
        INOD=KXSP(3,N)
        NVOIS=KXSP(5,N)
        DO K=1,NVOIS
          JNOD=IXSP(K,N)
          IF(JNOD > 0) THEN
            M=NOD2SP(JNOD)
            IF(SPBUF(1,M) < SPBUF(1,N) .OR.
     .        (SPBUF(1,M) == SPBUF(1,N) .AND. KXSP(8,M) < KXSP(8,N)) )THEN

              MS=KV(M)
              NV(MY_ADRV+MS)=NV(MY_ADRV+MS)+1

            END IF
          ELSE
C on ne fait rien pour le remote
          ENDIF
        END DO
      END DO      

      DO N=ITASK+1,NSNR,NTHREAD
        NVOIS=KXSPR(N)
        DO K=1,NVOIS
          JNOD=IXSPR(K,N)
          IF(JNOD > 0) THEN
            M=NOD2SP(JNOD)
          ELSE
            print *,'internal error'
          END IF

          MS=KV(M)
          NV(MY_ADRV+MS)=NV(MY_ADRV+MS)+1

        END DO
      END DO      
C-------        
C
      CALL MY_BARRIER
C
C-------        
      IF(ITASK==0)THEN

        IV(1)=1
        DO NS=1,NSP2SORT-1
          IV(NS+1)=IV(NS)
          DO JTASK=1,NTHREAD
            IV(NS+1)=IV(NS+1)+NV((JTASK-1)*NUMSPH+NS)
          END DO
        END DO

      END IF
C-------        
C
      CALL MY_BARRIER
C
C-------        
      DO NS=NSP2SORTF,NSP2SORTL
        IVS(NS)=IV(NS)
      END DO

      DO JTASK=1,NTHREAD-1
        DO NS=NSP2SORTF,NSP2SORTL
          IV(JTASK*NUMSPH+NS)=IV((JTASK-1)*NUMSPH+NS)+NV((JTASK-1)*NUMSPH+NS)
        END DO
      END DO
C-------        
C
      CALL MY_BARRIER
C
C-------        
      DO NS=NSP2SORTF,NSP2SORTL
        N =MA(NS)
        INOD=KXSP(3,N)
        NVOIS=KXSP(5,N)
        DO K=1,NVOIS
          JNOD=IXSP(K,N)
          IF(JNOD > 0) THEN
            M=NOD2SP(JNOD)
            IF(SPBUF(1,M) < SPBUF(1,N) .OR.
     .        (SPBUF(1,M) == SPBUF(1,N) .AND. KXSP(8,M) < KXSP(8,N)) )THEN

              MS=KV(M)
              IAUX(IV(ITASK*NUMSPH+MS))=INOD
              IV(ITASK*NUMSPH+MS)=IV(ITASK*NUMSPH+MS)+1

            END IF
          ELSE
C on ne fait rien pour le remote
          ENDIF
        END DO
      END DO      

      DO N=ITASK+1,NSNR,NTHREAD
        NVOIS=KXSPR(N)
        DO K=1,NVOIS
          JNOD=IXSPR(K,N)
          IF(JNOD > 0) THEN
            M=NOD2SP(JNOD)
          ELSE
            print *,'internal error'
          END IF

          MS=KV(M)
          IAUX(IV(ITASK*NUMSPH+MS))=-N
          IV(ITASK*NUMSPH+MS)=IV(ITASK*NUMSPH+MS)+1

        END DO
      END DO      
C-------        
C
      CALL MY_BARRIER
C
C-------        
      DO NS=NSP2SORTF,NSP2SORTL
        N =MA(NS)

        INOD=KXSP(3,N)
        XI = X(1,INOD)
        YI = X(2,INOD)
        ZI = X(3,INOD)
 
        NVOIS=KXSP(5,N)
        DO K=1,NVOIS
          JNOD=IXSP(K,N)
          IF(JNOD > 0) THEN
            M=NOD2SP(JNOD)
            XJ = X(1,JNOD)
            YJ = X(2,JNOD)
            ZJ = X(3,JNOD)
            AAA = SPBUF(1,N)+SPBUF(1,M)
          ELSE
            NN=-JNOD
            XJ = XSPHR(3,NN)
            YJ = XSPHR(4,NN)
            ZJ = XSPHR(5,NN)
            AAA = SPBUF(1,N)+XSPHR(2,NN)
          ENDIF

          AAA2 = AAA*AAA

C a faire : symetrie parfaite <=> X(hmin)-X(hmax)
          D1X = XI - XJ
          D1Y = YI - YJ
          D1Z = ZI - ZJ
          D2 = D1X*D1X+D1Y*D1Y+D1Z*D1Z

          JVOIS(K)=JNOD
          DVOIS(K)=D2/AAA2
       
        END DO

        NVOISS=0
        DO JTASK=1,NTHREAD
          NVOISS=NVOISS+NV((JTASK-1)*NUMSPH+NS)
        END DO
        NVOIS=NVOIS+NVOISS
        IF(NVOIS>KVOISPH)THEN
          IREDUCE=1
          KREDUCE(N)=1
        END IF

        DO K=1,NVOISS
          JNOD=IAUX(IVS(NS)+K-1)
          IF(JNOD > 0) THEN
            M=NOD2SP(JNOD)
            XJ = X(1,JNOD)
            YJ = X(2,JNOD)
            ZJ = X(3,JNOD)
            AAA = SPBUF(1,N)+SPBUF(1,M)
          ELSE
            NN=-JNOD
            XJ = XSPHR(3,NN)
            YJ = XSPHR(4,NN)
            ZJ = XSPHR(5,NN)
            AAA = SPBUF(1,N)+XSPHR(2,NN)
          ENDIF

          AAA2 = AAA*AAA

C a faire : symetrie parfaite <=> X(hmin)-X(hmax)
          D1X = XI - XJ
          D1Y = YI - YJ
          D1Z = ZI - ZJ
          D2 = D1X*D1X+D1Y*D1Y+D1Z*D1Z

          JVOIS(KXSP(5,N)+K)=JNOD
          DVOIS(KXSP(5,N)+K)=D2/AAA2
       
        END DO

        BOOL_SPH_SORT(N) = .FALSE.
        IF(KREDUCE(N)/=0 .AND. NVOIS > KVOISPH) THEN
          BOOL_SPH_SORT(N) = .TRUE.

          CALL MYQSORT(NVOIS,DVOIS,JPERM,IERROR)
           DO K=1,NVOIS
            JSTOR(K)=JVOIS(K)
           ENDDO
           DO K=1,KVOISPH
            JVOIS(K)=JSTOR(JPERM(K))
           ENDDO

           DK=DVOIS(KVOISPH)
C-----------------
C Choix des cellules a conserver tq distance < DK pour eviter pb de parith/on
           NVOIS=0
           DO K=1,KVOISPH
            IF(DVOIS(K)<DK)THEN
              NVOIS=NVOIS+1
            END IF
           END DO
        END IF

        NVOIS=MIN(NVOIS,KVOISPH)
        KXSP(5,N)=NVOIS

        NVOIS1=0
        NVOIS2=NVOIS+1
        DO K=1,NVOIS
         JK       =JVOIS(K)
         DK       =DVOIS(K)

         IF(DK < ONE)THEN
           NVOIS1=NVOIS1+1
           IXSP(NVOIS1,N)=JK
         ELSE
           NVOIS2=NVOIS2-1
           IXSP(NVOIS2,N)=JK
         END IF
        ENDDO
        KXSP(4,N)=NVOIS1
        IF(NVOIS1>LVOISPH)IREDUCE=1

      END DO      
C-------        
C
      CALL MY_BARRIER
C
      IF(ITASK==0) DEALLOCATE(KXSPR,IXSPR,IV,IVS,KV,NV,IAUX)
C--------------------------------------------------
      RETURN
      END
