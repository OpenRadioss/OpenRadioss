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
Chd|  FVBRIC3                       source/airbag/fvbric3.F       
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FVBRIC3(IBUF,  IBUFA,  NNT,   NBRIC, NNA,
     .                   TBRIC, BRNA,   NCONA, ITAB,  
     .                   ILVOUT,NB_NODE,IVINI, VINI,  KMESH, V  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUF(*), NNT, NBRIC, NNA, ITAB(*), ILVOUT,
     .        BRNA(8,*), NCONA(16,*), NB_NODE, IVINI, KMESH
      INTEGER, DIMENSION(NNA), INTENT(IN) :: IBUFA
      INTEGER, DIMENSION(2, NBRIC), INTENT(IN) :: TBRIC
      my_real VINI(3), V(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, N, II, JJ, KK, NN, NTYPE, IMAX
      INTEGER IBRCO(31,NNA), ITAG(NNA), JTAG(NB_NODE)
      INTEGER TAG(8), REDIR(8), NNAI
      my_real VMAX, VMIN, VEL

      DATA TAG /1,1,2,2,2,3,3,4/
C
      DO I=1,NNA
         DO J=1,31
           IBRCO(J,I)=0
         ENDDO
C
         DO J=1,16
           NCONA(J,I)=0
         ENDDO
      ENDDO
C
C Solides connectes au noeud N
C
      DO I=1,NBRIC
         NTYPE=TBRIC(2,I)
         IF(NTYPE==2) THEN
           REDIR(1)=1
           REDIR(2)=3
           REDIR(3)=5
           REDIR(4)=6
           DO J=1,4
             N=BRNA(REDIR(J),I)
             JJ=IBRCO(1,N)
             JJ=JJ+1
             IF(JJ>30) CYCLE
             IBRCO(1,N)=JJ
             IBRCO(JJ+1,N)=I         
           ENDDO
         ELSEIF(NTYPE==3) THEN
           REDIR(1)=1
           REDIR(2)=2
           REDIR(3)=3
           REDIR(4)=5
           REDIR(5)=6
           REDIR(6)=7
           DO J=1,6
             N=BRNA(REDIR(J),I)
             JJ=IBRCO(1,N)
             JJ=JJ+1
             IF(JJ>12) CYCLE
             IBRCO(1,N)=JJ
             IBRCO(JJ+1,N)=I         
           ENDDO
         ELSEIF(NTYPE==4) THEN
           DO J=1,5
             N=BRNA(J,I)
             JJ=IBRCO(1,N)
             JJ=JJ+1
             IF(JJ>12) CYCLE
             IBRCO(1,N)=JJ
             IBRCO(JJ+1,N)=I         
           ENDDO
         ELSEIF(NTYPE==1) THEN
           DO J=1,8
             N=BRNA(J,I)
             JJ=IBRCO(1,N)
             JJ=JJ+1
             IF(JJ>12) CYCLE
             IBRCO(1,N)=JJ
             IBRCO(JJ+1,N)=I         
           ENDDO
         ENDIF
      ENDDO
C
      IF(ILVOUT >=3 ) THEN
        WRITE(IOUT,2000)
        DO I=1,NNA
          N=ITAB(IBUFA(I))
          K=IBRCO(1,I)
          KK=MIN(K,12)
          WRITE(IOUT,'(15I10)')I,N,K,(IBRCO(L+1,I),L=1,KK)
        ENDDO
      ENDIF
C
C  Noeud solide appartenant a des coques airbag ou internes => lagrangien
C
      DO I=1,NB_NODE
        JTAG(I)=0
      ENDDO
      DO I=1,NNT
        JTAG(IBUF(I))=1
      ENDDO
      DO I=1,NNA
        J=IBUFA(I)
        IF(JTAG(J)==1) NCONA(2,I)=1
      ENDDO
C
C  Noeuds voisins pour calcul de la vitesse de grille
C
      NNAI=0
      DO I=1,NNA
         IF(NCONA(2,I)/=0) CYCLE
         NNAI=NNAI+1
         II=IBRCO(1,I)
         DO N=1,NNA
           ITAG(N)=0
         ENDDO
         DO J=1,II
           JJ=IBRCO(J+1,I)
           NTYPE=TBRIC(2,JJ)
           IF(NTYPE==2) THEN
             REDIR(1)=1
             REDIR(2)=3
             REDIR(3)=5
             REDIR(4)=6
             DO K=1,4
               KK=BRNA(REDIR(K),JJ)
               ITAG(KK)=ITAG(KK)+1
             ENDDO
           ELSEIF(NTYPE==3) THEN
             REDIR(1)=1
             REDIR(2)=2
             REDIR(3)=3
             REDIR(4)=5
             REDIR(5)=6
             REDIR(6)=7
             DO K=1,6
               KK=BRNA(REDIR(K),JJ)
               ITAG(KK)=ITAG(KK)+1
             ENDDO
           ELSEIF(NTYPE==4) THEN
             DO K=1,5
               KK=BRNA(K,JJ)
               ITAG(KK)=ITAG(KK)+1
             ENDDO
           ELSEIF(NTYPE==1) THEN
             DO K=1,8
               KK=BRNA(K,JJ)
               ITAG(KK)=ITAG(KK)+1
             ENDDO
           ENDIF
         ENDDO
C
         IF(II > 8) THEN
            IMAX=4
         ELSE
            IMAX=TAG(II)
         ENDIF
         DO N=1,NNA
         IF(N==I) CYCLE
         IF(ITAG(N)>=IMAX) THEN
C  Le noeud N appartient a au moins IMAX solides voisins
           NN=NCONA(1,I)
           NN=NN+1
           IF(NN>14) GO TO 100
           NCONA(1,I)=NN
           NCONA(NN+2,I)=N
         ENDIF
         ENDDO
 100  CONTINUE
      ENDDO
C
C Initial velocity of internal gas nodes
      IVINI=0
      IF(NNAI > 0) THEN
         VMAX=ZERO
         VMIN=EP30
         DO I=1,NNT
            J=IBUF(I)
            VEL=V(1,J)*V(1,J)+V(2,J)*V(2,J)+V(3,J)*V(3,J)
            IF(VEL > VMAX) VMAX=VEL
            IF(VEL < VMIN) VMIN=VEL		 
         ENDDO
         IF(VMIN == VMAX) THEN
            IF(VMIN > ZERO) THEN
              IVINI=1
              J=IBUF(1)
              VINI(1)=V(1,J)
              VINI(2)=V(2,J)
              VINI(3)=V(3,J)
            ELSE
              IVINI=0
              VINI(1)=ZERO
              VINI(2)=ZERO
              VINI(3)=ZERO
            ENDIF
         ELSE
            IVINI=1
            VINI(1)=ZERO
            VINI(2)=ZERO
            VINI(3)=ZERO
            WRITE(IOUT,'(/A/)') 'FVMBAG - WARNING NON UNIFORM INITIAL VELOCITY : INTERNAL GAS NODE VELOCITIES ARE NOT INITIALIZED'
         ENDIF
      ENDIF
C
C Set to zero initial velocity of internal brick nodes (kmesh=1)
      IF(NNAI > 0 .AND. IVINI == 1 .AND. KMESH == 1) THEN
        DO I=1,NNA
          IF(NCONA(2,I)/=0) CYCLE
          J=IBUFA(I)
          V(1,J)=ZERO
          V(2,J)=ZERO
          V(3,J)=ZERO
        ENDDO	  
      ENDIF
C
      WRITE(IOUT,3000) NBRIC,NNA,NNAI
      IF(ILVOUT >= 3) THEN
        WRITE(IOUT,1000)
        DO I=1,NNA
          IF(NCONA(2,I)/=0) CYCLE
C Noeud interne
          N=ITAB(IBUFA(I))
          K=NCONA(1,I)
          WRITE(IOUT,'(15I10)') N,(ITAB(IBUFA(NCONA(L+2,I))),L=1,K)
        ENDDO
      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3--
 1000 FORMAT(
     ./5X,'FVMBAG  -  LIST OF CONNECTED NODES - SOLID GRID VELOCITY   '
     ./5X,'-----------------------------------------------------------'
     ./5X,' NODE',8X,'N1',8X,'N2',8X,'N3',8X,'N4',8X,'N5',8X,'N6',8X,
     .'N7',8X,'N8',8X,'N9',7X,'N10',7X,'N11',7X,'N12',7X,'N13',7X,'N14')
C
 2000 FORMAT(
     ./5X,'FVMBAG  -  LIST OF CONNECTED SOLID   '
     ./5X,'-----------------------------------------------------------'
     ./6X,'NODE LOC-GLOB NB SOLIDS',8X,'N1',8X,'N2',8X,'N3',8X,'N4',8X,
     .'N5',8X,'N6',8X,'N7',8X,'N8',8X,'N9',7X,'N10',7X,'N11',7X,'N12')
C
 3000 FORMAT(/5X,'FVMBAG : ADDITIONAL BRICK GROUP         ',
     .       /5X,'-------------------------------         ',
     .       /5X,'NUMBER OF ADDITIONAL BRICKS . . . . . .=',I10,
     .       /5X,'NUMBER OF ADDITIONAL BRICK NODES. . . .=',I10,
     .       /5X,'NUMBER OF INTERNAL BRICK NODES. . . . .=',I10)
      RETURN
      END

