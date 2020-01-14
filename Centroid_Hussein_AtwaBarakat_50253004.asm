#
          # HARD CODED INPUTS
          # INPUTS | WHERE/HOW THEY ARE STORED
          # Data Point coordinates for Centroid1 are stored in .word directive called Centriod1_x and Centriod1_y
          # Coordinates for Centroid2 are stored in .word directive called Centriod2_x and Centriod2_y
          # 10 data point inputs are seperated by x and y values and stored in pointArray_xValues and pointArray_yValues

          #Final output consists of the new points for the Centroids

#
.data
#Centroid 1 input
Centriod1_x: .word 2 # point E
Centriod1_y: .word 4
#Centroid 2 input
Centriod2_x: .word 6 # point I
Centriod2_y: .word 2
#pointArray_xValues: .word pointA_x, pointB_x, pointC_x, pointD_x, pointE_x, pointF_x, pointG_x, pointH_x, pointI_x, pointJ_x
#pointArray_yValues: .word pointA_y, pointB_y, pointC_y, pointD_y, pointE_y, pointF_y, pointG_y, pointH_y, pointI_y, pointJ_y
pointArray_xValues: .word 2, 5, 1, 3, 2, 2, 4, 5, 6, 5
pointArray_yValues: .word 2, 3, 5, 3, 4, 1, 2, 1, 2, 2


printC1: .asciiz "\nCluster1: "
printCtotal: .asciiz "\n# of elements: "
printC2: .asciiz "\nCluster2: "
printLeftBrkt: .asciiz "("
printComma: .asciiz ","
printRightBrkt: .asciiz ")"
printCentroid1: .asciiz "\nNew Centriod1: "
printCentroid2: .asciiz "\nNew Centroid2: "
printEndLine: .asciiz " \n"

#array capable of storing 10 x points for Centroid1 cluster
cluster1ArrayX:# .word 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .align 2
  .space 40
#array capable of storing 10 y points for Centroid1 cluster
cluster1ArrayY: #.word 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .align 2
  .space 40
#array capable of storing 10 x points for Centroid2 cluster
cluster2ArrayX: #.word 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .align 2
  .space 40
#array capable of storing 10 y points for Centroid2 cluster
cluster2ArrayY: #.word 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .align 2
  .space 40


.text
.globl updatingCentroid

main:
  la $t1, pointArray_xValues # $t1 = &pointArray_xValues
  la $t2, pointArray_yValues # $t2 = &pointArray_yValues
  lw $s1, Centriod1_x # $s1 = Centriod1_x
  lw $s2, Centriod1_y # $s2 = Centriod1_y
  lw $s3, Centriod2_x # $s1 = Centriod2_x
  lw $s4, Centriod2_y # $s2 = Centriod2_y
  la $t3, cluster1ArrayX # $s3 = &cluster1ArrayX
  la $t4, cluster1ArrayY # $s4 = &cluster2ArrayY
  la $t5, cluster2ArrayX # $s3 = &cluster2ArrayX
  la $t6, cluster2ArrayY # $s4 = &cluster2ArrayY
  ori $t8, $0, 0
  ori $t9, $0, 0


  jal DistanceComparsion
  jal updatingCentroid
  #Tell the system program is done, if this is not there till be never ending program, infinite recursion
  li $v0, 10
  syscall
.end main


updatingCentroid:
#call distance comparsion procedure to group cluster1Arrays and
 #jal DistanceComparsion
#make room on stack for 9 registers
  li $s0, -36
  add $sp, $sp, $s0
  sw $t1, 0($sp)
  sw $t2, 4($sp)
  sw $a1, 8($sp)
  sw $t0, 12($sp)
  sw $s5, 16($sp)
  sw $s6, 20($sp)
  sw $s7, 24($sp)
  sw $t7, 28($sp)
  sw $a0, 32($sp)
############################################################################
#summing up all of cluster1ArrayX and dividing by total number of points in cluster1 to find NEW Centroid1_x
  lw $a0, 0($t3)
  lw $t1, 4($t3)
  lw $t2, 8($t3)
  lw $a1, 12($t3)
  lw $t0, 16($t3)
  lw $s5, 20($t3)
  lw $s6, 24($t3)
  lw $s7, 28($t3)
  lw $t7, 32($t3)
  add $a0, $a0, $t1 # $a0 + $t1
  add $a0, $a0, $t2 # $a0 + $t1 + $t2
  add $a0, $a0, $a1 # $a0 + $t1 + $t2 + $a1
  add $a0, $a0, $t0 # $a0 + $t1 + $t2 + $a1 + $t0
  add $a0, $a0, $s5 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5
  add $a0, $a0, $s6 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6
  add $a0, $a0, $s7 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6 + $s7
  add $a0, $a0, $t7 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6 + $s7 + $t7
  div $s1, $a0, $t8 # average of cluster1ArrayX aka new Centroid1_x store in $s1


#summing up all of cluster1ArrayY and dividing by total number of points in cluster1 to find NEW Centroid1_Y
  lw $a0, 0($t4)
  lw $t1, 4($t4)
  lw $t2, 8($t4)
  lw $a1, 12($t4)
  lw $t0, 16($t4)
  lw $s5, 20($t4)
  lw $s6, 24($t4)
  lw $s7, 28($t4)
  lw $t7, 32($t4)
  add $a0, $a0, $t1 # $a0 + $t1
  add $a0, $a0, $t2 # $a0 + $t1 + $t2
  add $a0, $a0, $a1 # $a0 + $t1 + $t2 + $a1
  add $a0, $a0, $t0 # $a0 + $t1 + $t2 + $a1 + $t0
  add $a0, $a0, $s5 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5
  add $a0, $a0, $s6 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6
  add $a0, $a0, $s7 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6 + $s7
  add $a0, $a0, $t7 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6 + $s7 + $t7
  div $s2, $a0, $t8 # average of cluster1ArrayY aka new Centroid1_y store in $s2


#summing up all of cluster2ArrayX and dividing by total number of points in cluster2 to find NEW Centroid2_x
  lw $a0, 0($t5)
  lw $t1, 4($t5)
  lw $t2, 8($t5)
  lw $a1, 12($t5)
  lw $t0, 16($t5)
  lw $s5, 20($t5)
  lw $s6, 24($t5)
  lw $s7, 28($t5)
  lw $t7, 32($t5)
  add $a0, $a0, $t1 # $a0 + $t1
  add $a0, $a0, $t2 # $a0 + $t1 + $t2
  add $a0, $a0, $a1 # $a0 + $t1 + $t2 + $a1
  add $a0, $a0, $t0 # $a0 + $t1 + $t2 + $a1 + $t0
  add $a0, $a0, $s5 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5
  add $a0, $a0, $s6 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6
  add $a0, $a0, $s7 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6 + $s7
  add $a0, $a0, $t7 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6 + $s7 + $t7
  div $s3, $a0, $t9 # average of cluster2ArrayX aka new Centroid2_x store in $s3

#summing up all of cluster2ArrayY and dividing by total number of points in cluster2 to find NEW Centroid2_y
  lw $a0, 0($t6)
  lw $t1, 4($t6)
  lw $t2, 8($t6)
  lw $a1, 12($t6)
  lw $t0, 16($t6)
  lw $s5, 20($t6)
  lw $s6, 24($t6)
  lw $s7, 28($t6)
  lw $t7, 32($t6)
  add $a0, $a0, $t1 # $a0 + $t1
  add $a0, $a0, $t2 # $a0 + $t1 + $t2
  add $a0, $a0, $a1 # $a0 + $t1 + $t2 + $a1
  add $a0, $a0, $t0 # $a0 + $t1 + $t2 + $a1 + $t0
  add $a0, $a0, $s5 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5
  add $a0, $a0, $s6 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6
  add $a0, $a0, $s7 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6 + $s7
  add $a0, $a0, $t7 # $a0 + $t1 + $t2 + $a1 + $t0 + $s5 + $s6 + $s7 + $t7
  div $s4, $a0, $t9 # average of cluster2ArrayY aka new Centroid2_y store in $s4
#################################################################################

  #print label new centroid1
  li $v0, 4
  la $a0, printCentroid1
  syscall
  #print left brakt
  li $v0, 4
  la $a0, printLeftBrkt
  syscall
  #Print value of Centroid1_x
  li $v0, 1
  move $a0, $s1
  syscall
  #print printEndLine
  li $v0, 4
  la $a0, printComma
  syscall
  #Print value of Centroid1_y
  li $v0, 1
  move $a0, $s2
  syscall
  #print righ brakt
  li $v0, 4
  la $a0, printRightBrkt
  syscall

  #print left brakt
  li $v0, 4
  la $a0, printCentroid2
  syscall
  #print left brakt
  li $v0, 4
  la $a0, printLeftBrkt
  syscall
  #Print value of Centroid2_x
  li $v0, 1
  move $a0, $s3
  syscall
  #print printComma
  li $v0, 4
  la $a0, printComma
  syscall
  #Print value of Centroid2_y
  li $v0, 1
  move $a0, $s4
  syscall
  #print right brakt
  li $v0, 4
  la $a0, printRightBrkt
  syscall
  #print right brakt
  li $v0, 4
  la $a0, printEndLine
  syscall

  #restoring registers with their orignal values before function call and restoring stack
    lw $t1, 0($sp)
    lw $t2, 4($sp)
    lw $a1, 8($sp)
    lw $t0, 12($sp)
    lw $s5, 16($sp)
    lw $s6, 20($sp)
    lw $s7, 24($sp)
    lw $t7, 28($sp)
    lw $a0, 32($sp)
    addi $sp, $sp, 36

  jr $ra #return back to main after executing




























  DistanceComparsion:
  addi $sp, $sp, -8    # adjust stack pointer to accommodate stack frame (subtract 4 bytes)
  sw $ra, 0($sp)       #   save a copy of our return address
  sw $t7, 4($sp)
  #Computing EuclideanDistance between point A and Centroid1
      lw $a0, 0($t1) #pointA x value
      lw $a1, 0($t2)#pointA y value
      move $a2, $s1 #Centriod1 x value
      move $a3, $s2 #Centriod1 y value
      jal EuclideanDistance #EuclideanDistance between centroid1 and pointA is stored in return register $v1 and moved into $a0
      move $s5, $v1 #storing EuclideanDistance of pointA and centroid1 into $s5
  #Computing EuclideanDistance between point A and Centroid2
      lw $a0, 0($t1) #pointA x value
      lw $a1, 0($t2) #pointA y value
      move $a2, $s3 #Centriod2 x value
      move $a3, $s4 #Centriod2 y value
      jal EuclideanDistance #EuclideanDistance between centroid2 and pointA is stored in return register $v1 and moved into $a0
      move $s6, $v1 #storing EuclideanDistance of pointA and centroid2 into $s6

      blt $s5, $s6, storeInClusterCentroid1A #check if EuclideanDistance with centroid1 is < EuclideanDistance with centroid2 and if so place that point in cluster of centroid1 otherwise store in cluster of centroid2.
      #else storeInClusterCentroid2
      lw $t7, 0($t1) #storing pointA_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t5) #storing pointA_x into cluster2ArrayX
      addi $t5, $t5, 4 #moving pointer for cluster2ArrayX to next index
      lw $t7, 0($t2) #storing pointA_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t6) #storing pointA_x into cluster2ArrayY
      addi $t6, $t6, 4 #moving pointer for cluster2ArrayY to next index

      addi $t9, $t9, 1 # number of elements in cluster 2 tracker

      j skipStoringInCentroid1A
  storeInClusterCentroid1A:
      lw $t7, 0($t1) #storing pointA_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t3) #storing pointA_x into cluster1ArrayX
      addi $t3, $t3, 4 #moving pointer for cluster1ArrayX to next index
      lw $t7, 0($t2) #storing pointA_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t4) #storing pointA_y into cluster1ArrayY
      addi $t4, $t4, 4 #moving pointer for cluster1ArrayY to next index

      addi $t8, $t8, 1 # number of elements in cluster 1 tracker

      j skipStoringInCentroid1A

    skipStoringInCentroid1A:
  ###########################################################################################################
    #Computing EuclideanDistance between point B and Centroid1
          lw $a0, 4($t1) #pointB x value
          lw $a1, 4($t2)#pointB y value
          move $a2, $s1 #Centriod1 x value
          move $a3, $s2 #Centriod1 y value
          jal EuclideanDistance #EuclideanDistance between centroid1 and pointB is stored in return register $v1 and moved into $a0
          move $s5, $v1 #storing EuclideanDistance of pointB and centroid1 into $s5
    #Computing EuclideanDistance between point B and Centroid2
          lw $a0, 4($t1) #pointB x value
          lw $a1, 4($t2) #pointB y value
          move $a2, $s3 #Centriod2 x value
          move $a3, $s4 #Centriod2 y value
          jal EuclideanDistance #EuclideanDistance between centroid2 and pointA is stored in return register $v1 and moved into $a0
          move $s6, $v1 #storing EuclideanDistance of pointA and centroid2 into $s6

          blt $s5, $s6, storeInClusterCentroid1B #check if EuclideanDistance with centroid1 is < EuclideanDistance with centroid2 and if so place that point in cluster of centroid1 otherwise store in cluster of centroid2.
          #else storeInClusterCentroid2B
          lw $t7, 4($t1) #storing pointB_x into register $t7 to store into cluster2ArrayX in next instruction
          sw $t7, 0($t5) #storing pointA_x into cluster2ArrayX
          addi $t5, $t5, 4 #moving pointer for cluster2ArrayX to next index
          lw $t7, 4($t2) #storing pointB_y into register $t7 to store into cluster2ArrayY in next instruction
          sw $t7, 0($t6) #storing pointA_x into cluster2ArrayY
          addi $t6, $t6, 4 #moving pointer for cluster2ArrayY to next index

          addi $t9, $t9, 1 # number of elements in cluster 2 tracker

          j skipStoringInCentroid1B
      storeInClusterCentroid1B:
          lw $t7, 4($t1) #storing pointB_x into register $t7 to store into cluster2ArrayX in next instruction
          sw $t7, 0($t3) #storing pointA_x into cluster1ArrayX
          addi $t3, $t3, 4 #moving pointer for cluster1ArrayX to next index
          lw $t7, 4($t2) #storing pointB_y into register $t7 to store into cluster2ArrayY in next instruction
          sw $t7, 0($t4) #storing pointA_y into cluster1ArrayY
          addi $t4, $t4, 4 #moving pointer for cluster1ArrayY to next index

          addi $t8, $t8, 1 # number of elements in cluster 1 tracker

          j skipStoringInCentroid1B

        skipStoringInCentroid1B:
  ############################################################################################
        #Computing EuclideanDistance between point C and Centroid1
              lw $a0, 8($t1) #pointC x value
              lw $a1, 8($t2)#pointC y value
              move $a2, $s1 #Centriod1 x value
              move $a3, $s2 #Centriod1 y value
              jal EuclideanDistance #EuclideanDistance between centroid1 and pointA is stored in return register $v1 and moved into $a0
              move $s5, $v1 #storing EuclideanDistance of pointC and centroid1 into $s5
        #Computing EuclideanDistance between point C and Centroid2
              lw $a0, 8($t1) #pointC x value
              lw $a1, 8($t2) #pointC y value
              move $a2, $s3 #Centriod2 x value
              move $a3, $s4 #Centriod2 y value
              jal EuclideanDistance #EuclideanDistance between centroid2 and pointC is stored in return register $v1 and moved into $a0
              move $s6, $v1 #storing EuclideanDistance of pointC and centroid2 into $s6

              blt $s5, $s6, storeInClusterCentroid1C #check if EuclideanDistance with centroid1 is < EuclideanDistance with centroid2 and if so place that point in cluster of centroid1 otherwise store in cluster of centroid2.
              #else storeInClusterCentroid2
              lw $t7, 8($t1) #storing pointC_x into register $t7 to store into cluster2ArrayX in next instruction
              sw $t7, 0($t5) #storing pointA_x into cluster2ArrayX
              addi $t5, $t5, 4 #moving pointer for cluster2ArrayX to next index
              lw $t7, 8($t2) #storing pointC_y into register $t7 to store into cluster2ArrayY in next instruction
              sw $t7, 0($t6) #storing pointA_x into cluster2ArrayY
              addi $t6, $t6, 4 #moving pointer for cluster2ArrayY to next index

              addi $t9, $t9, 1 # number of elements in cluster 2 tracker


              j skipStoringInCentroid1C
          storeInClusterCentroid1C:
              lw $t7, 8($t1) #storing pointC_x into register $t7 to store into cluster2ArrayX in next instruction
              sw $t7, 0($t3) #storing pointA_x into cluster1ArrayX
              addi $t3, $t3, 4 #moving pointer for cluster1ArrayX to next index
              lw $t7, 8($t2) #storing pointC_y into register $t7 to store into cluster2ArrayY in next instruction
              sw $t7, 0($t4) #storing pointA_y into cluster1ArrayY
              addi $t4, $t4, 4 #moving pointer for cluster1ArrayY to next index

              addi $t8, $t8, 1 # number of elements in cluster 1 tracker

              j skipStoringInCentroid1C

            skipStoringInCentroid1C:
  ###########################################################################################################
        #Computing EuclideanDistance between point D and Centroid1
              lw $a0, 12($t1) #pointD x value
              lw $a1, 12($t2)#pointD y value
              move $a2, $s1 #Centriod1 x value
              move $a3, $s2 #Centriod1 y value
              jal EuclideanDistance #EuclideanDistance between centroid1 and pointD is stored in return register $v1 and moved into $a0
              move $s5, $v1 #storing EuclideanDistance of pointD and centroid1 into $s5
        #Computing EuclideanDistance between point D and Centroid2
              lw $a0, 12($t1) #pointD x value
              lw $a1, 12($t2) #pointD y value
              move $a2, $s3 #Centriod2 x value
              move $a3, $s4 #Centriod2 y value
              jal EuclideanDistance #EuclideanDistance between centroid2 and pointD is stored in return register $v1 and moved into $a0
              move $s6, $v1 #storing EuclideanDistance of pointD and centroid2 into $s6

              blt $s5, $s6, storeInClusterCentroid1D #check if EuclideanDistance with centroid1 is < EuclideanDistance with centroid2 and if so place that point in cluster of centroid1 otherwise store in cluster of centroid2.
              #else storeInClusterCentroid2
              lw $t7, 12($t1) #storing pointD_x into register $t7 to store into cluster2ArrayX in next instruction
              sw $t7, 0($t5) #storing pointA_x into cluster2ArrayX
              addi $t5, $t5, 4 #moving pointer for cluster2ArrayX to next index
              lw $t7, 12($t2) #storing pointD_y into register $t7 to store into cluster2ArrayY in next instruction
              sw $t7, 0($t6) #storing pointA_x into cluster2ArrayY
              addi $t6, $t6, 4 #moving pointer for cluster2ArrayY to next index

              addi $t9, $t9, 1 # number of elements in cluster 2 tracker

              j skipStoringInCentroid1D
          storeInClusterCentroid1D:
              lw $t7, 12($t1) #storing pointD_x into register $t7 to store into cluster2ArrayX in next instruction
              sw $t7, 0($t3) #storing pointA_x into cluster1ArrayX
              addi $t3, $t3, 4 #moving pointer for cluster1ArrayX to next index
              lw $t7, 12($t2) #storing pointD_y into register $t7 to store into cluster2ArrayY in next instruction
              sw $t7, 0($t4) #storing pointA_y into cluster1ArrayY
              addi $t4, $t4, 4 #moving pointer for cluster1ArrayY to next index

              addi $t8, $t8, 1 # number of elements in cluster 1 tracker

              j skipStoringInCentroid1D

            skipStoringInCentroid1D:
  ###########################################################################################################
        #Computing EuclideanDistance between point E and Centroid1
              lw $a0, 16($t1) #pointE x value
              lw $a1, 16($t2)#pointE y value
              move $a2, $s1 #Centriod1 x value
              move $a3, $s2 #Centriod1 y value
              jal EuclideanDistance #EuclideanDistance between centroid1 and pointA is stored in return register $v1 and moved into $a0
              move $s5, $v1 #storing EuclideanDistance of pointE and centroid1 into $s5
        #Computing EuclideanDistance between point E and Centroid2
              lw $a0, 16($t1) #pointE x value
              lw $a1, 16($t2) #pointE y value
              move $a2, $s3 #Centriod2 x value
              move $a3, $s4 #Centriod2 y value
              jal EuclideanDistance #EuclideanDistance between centroid2 and pointA is stored in return register $v1 and moved into $a0
              move $s6, $v1 #storing EuclideanDistance of pointA and centroid2 into $s6

              blt $s5, $s6, storeInClusterCentroid1E #check if EuclideanDistance with centroid1 is < EuclideanDistance with centroid2 and if so place that point in cluster of centroid1 otherwise store in cluster of centroid2.
              #else storeInClusterCentroid2
              lw $t7, 16($t1) #storing pointE_x into register $t7 to store into cluster2ArrayX in next instruction
              sw $t7, 0($t5) #storing pointA_x into cluster2ArrayX
              addi $t5, $t5, 4 #moving pointer for cluster2ArrayX to next index
              lw $t7, 16($t2) #storing pointE_y into register $t7 to store into cluster2ArrayY in next instruction
              sw $t7, 0($t6) #storing pointA_x into cluster2ArrayY
              addi $t6, $t6, 4 #moving pointer for cluster2ArrayY to next index

              addi $t9, $t9, 1 # number of elements in cluster 2 tracker

              j skipStoringInCentroid1E
          storeInClusterCentroid1E:
              lw $t7, 16($t1) #storing pointE_x into register $t7 to store into cluster2ArrayX in next instruction
              sw $t7, 0($t3) #storing pointA_x into cluster1ArrayX
              addi $t3, $t3, 4 #moving pointer for cluster1ArrayX to next index
              lw $t7, 16($t2) #storing pointE_y into register $t7 to store into cluster2ArrayY in next instruction
              sw $t7, 0($t4) #storing pointA_y into cluster1ArrayY
              addi $t4, $t4, 4 #moving pointer for cluster1ArrayY to next index

              addi $t8, $t8, 1 # number of elements in cluster 1 tracker


              j skipStoringInCentroid1E

            skipStoringInCentroid1E:
  ###########################################################################################################

  #Computing EuclideanDistance between point F and Centroid1
      lw $a0, 20($t1) #pointF x value
      lw $a1, 20($t2)#pointF y value
      move $a2, $s1 #Centriod1 x value
      move $a3, $s2 #Centriod1 y value
      jal EuclideanDistance #EuclideanDistance between centroid1 and pointF is stored in return register $v1 and moved into $a0
      move $s5, $v1 #storing EuclideanDistance of pointF and centroid1 into $s5
  #Computing EuclideanDistance between point F and Centroid2
      lw $a0, 20($t1) #pointF x value
      lw $a1, 20($t2) #pointF y value
      move $a2, $s3 #Centriod2 x value
      move $a3, $s4 #Centriod2 y value
      jal EuclideanDistance #EuclideanDistance between centroid2 and pointF is stored in return register $v1 and moved into $a0
      move $s6, $v1 #storing EuclideanDistance of pointA and centroid2 into $s6

      blt $s5, $s6, storeInClusterCentroid1F #check if EuclideanDistance with centroid1 is < EuclideanDistance with centroid2 and if so place that point in cluster of centroid1 otherwise store in cluster of centroid2.
      #else storeInClusterCentroid2
      lw $t7, 20($t1) #storing pointF_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t5) #storing pointA_x into cluster2ArrayX
      addi $t5, $t5, 4 #moving pointer for cluster2ArrayX to next index
      lw $t7, 20($t2) #storing pointF_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t6) #storing pointA_x into cluster2ArrayY
      addi $t6, $t6, 4 #moving pointer for cluster2ArrayY to next index

      addi $t9, $t9, 1 # number of elements in cluster 2 tracker


      j skipStoringInCentroid1F
  storeInClusterCentroid1F:
      lw $t7, 20($t1) #storing pointF_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t3) #storing pointA_x into cluster1ArrayX
      addi $t3, $t3, 4 #moving pointer for cluster1ArrayX to next index
      lw $t7, 20($t2) #storing pointF_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t4) #storing pointA_y into cluster1ArrayY
      addi $t4, $t4, 4 #moving pointer for cluster1ArrayY to next index

      addi $t8, $t8, 1 # number of elements in cluster 1 tracker

      j skipStoringInCentroid1F

    skipStoringInCentroid1F:
  ###########################################################################################################
  #Computing EuclideanDistance between point G and Centroid1
      lw $a0, 24($t1) #pointG x value
      lw $a1, 24($t2)#pointG y value
      move $a2, $s1 #Centriod1 x value
      move $a3, $s2 #Centriod1 y value
      jal EuclideanDistance #EuclideanDistance between centroid1 and pointA is stored in return register $v1 and moved into $a0
      move $s5, $v1 #storing EuclideanDistance of pointG and centroid1 into $s5
  #Computing EuclideanDistance between point G and Centroid2
      lw $a0, 24($t1) #pointG x value
      lw $a1, 24($t2) #pointG y value
      move $a2, $s3 #Centriod2 x value
      move $a3, $s4 #Centriod2 y value
      jal EuclideanDistance #EuclideanDistance between centroid2 and pointA is stored in return register $v1 and moved into $a0
      move $s6, $v1 #storing EuclideanDistance of pointA and centroid2 into $s6

      blt $s5, $s6, storeInClusterCentroid1G #check if EuclideanDistance with centroid1 is < EuclideanDistance with centroid2 and if so place that point in cluster of centroid1 otherwise store in cluster of centroid2.
      #else storeInClusterCentroid2
      lw $t7, 24($t1) #storing pointG_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t5) #storing pointA_x into cluster2ArrayX
      addi $t5, $t5, 4 #moving pointer for cluster2ArrayX to next index
      lw $t7, 24($t2) #storing pointG_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t6) #storing pointA_x into cluster2ArrayY
      addi $t6, $t6, 4 #moving pointer for cluster2ArrayY to next index

      addi $t9, $t9, 1 # number of elements in cluster 2 tracker


      j skipStoringInCentroid1G
  storeInClusterCentroid1G:
      lw $t7, 24($t1) #storing pointG_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t3) #storing pointA_x into cluster1ArrayX
      addi $t3, $t3, 4 #moving pointer for cluster1ArrayX to next index
      lw $t7, 24($t2) #storing pointG_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t4) #storing pointA_y into cluster1ArrayY
      addi $t4, $t4, 4 #moving pointer for cluster1ArrayY to next index

      addi $t8, $t8, 1 # number of elements in cluster 1 tracker

      j skipStoringInCentroid1G

    skipStoringInCentroid1G:
  ###########################################################################################################
  #Computing EuclideanDistance between point H and Centroid1
      lw $a0, 28($t1) #pointH x value
      lw $a1, 28($t2)#pointH y value
      move $a2, $s1 #Centriod1 x value
      move $a3, $s2 #Centriod1 y value
      jal EuclideanDistance #EuclideanDistance between centroid1 and pointA is stored in return register $v1 and moved into $a0
      move $s5, $v1 #storing EuclideanDistance of pointH and centroid1 into $s5
  #Computing EuclideanDistance between point H and Centroid2
      lw $a0, 28($t1) #pointH x value
      lw $a1, 28($t2) #pointH y value
      move $a2, $s3 #Centriod2 x value
      move $a3, $s4 #Centriod2 y value
      jal EuclideanDistance #EuclideanDistance between centroid2 and pointA is stored in return register $v1 and moved into $a0
      move $s6, $v1 #storing EuclideanDistance of pointH and centroid2 into $s6

      blt $s5, $s6, storeInClusterCentroid1H #check if EuclideanDistance with centroid1 is < EuclideanDistance with centroid2 and if so place that point in cluster of centroid1 otherwise store in cluster of centroid2.
      #else storeInClusterCentroid2
      lw $t7, 28($t1) #storing pointH_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t5) #storing pointA_x into cluster2ArrayX
      addi $t5, $t5, 4 #moving pointer for cluster2ArrayX to next index
      lw $t7, 28($t2) #storing pointH_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t6) #storing pointA_x into cluster2ArrayY
      addi $t6, $t6, 4 #moving pointer for cluster2ArrayY to next index

      addi $t9, $t9, 1 # number of elements in cluster 2 tracker

      j skipStoringInCentroid1H
  storeInClusterCentroid1H:
      lw $t7, 28($t1) #storing pointH_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t3) #storing pointA_x into cluster1ArrayX
      addi $t3, $t3, 4 #moving pointer for cluster1ArrayX to next index
      lw $t7, 28($t2) #storing pointH_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t4) #storing pointA_y into cluster1ArrayY
      addi $t4, $t4, 4 #moving pointer for cluster1ArrayY to next index

      addi $t8, $t8, 1 # number of elements in cluster 1 tracker

      j skipStoringInCentroid1H

    skipStoringInCentroid1H:
  ###########################################################################################################
  #Computing EuclideanDistance between point I and Centroid1
      lw $a0, 32($t1) #pointI x value
      lw $a1, 32($t2)#pointI y value
      move $a2, $s1 #Centriod1 x value
      move $a3, $s2 #Centriod1 y value
      jal EuclideanDistance #EuclideanDistance between centroid1 and pointA is stored in return register $v1 and moved into $a0
      move $s5, $v1 #storing EuclideanDistance of pointA and centroid1 into $s5
  #Computing EuclideanDistance between point I and Centroid2
      lw $a0, 32($t1) #pointI x value
      lw $a1, 32($t2) #pointI y value
      move $a2, $s3 #Centriod2 x value
      move $a3, $s4 #Centriod2 y value
      jal EuclideanDistance #EuclideanDistance between centroid2 and pointA is stored in return register $v1 and moved into $a0
      move $s6, $v1 #storing EuclideanDistance of pointA and centroid2 into $s6

      blt $s5, $s6, storeInClusterCentroid1I #check if EuclideanDistance with centroid1 is < EuclideanDistance with centroid2 and if so place that point in cluster of centroid1 otherwise store in cluster of centroid2.
      #else storeInClusterCentroid2
      lw $t7, 32($t1) #storing pointI_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t5) #storing pointA_x into cluster2ArrayX
      addi $t5, $t5, 4 #moving pointer for cluster2ArrayX to next index
      lw $t7, 32($t2) #storing pointI_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t6) #storing pointA_x into cluster2ArrayY
      addi $t6, $t6, 4 #moving pointer for cluster2ArrayY to next index

      addi $t9, $t9, 1 # number of elements in cluster 2 tracker

      j skipStoringInCentroid1I
  storeInClusterCentroid1I:
      lw $t7, 32($t1) #storing pointI_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t3) #storing pointA_x into cluster1ArrayX
      addi $t3, $t3, 4 #moving pointer for cluster1ArrayX to next index
      lw $t7, 32($t2) #storing pointI_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t4) #storing pointA_y into cluster1ArrayY
      addi $t4, $t4, 4 #moving pointer for cluster1ArrayY to next index

      addi $t8, $t8, 1 # number of elements in cluster 1 tracker

      j skipStoringInCentroid1I

    skipStoringInCentroid1I:
  ###########################################################################################################
  #Computing EuclideanDistance between point J and Centroid1
      lw $a0, 36($t1) #pointJ x value
      lw $a1, 36($t2)#pointJ y value
      move $a2, $s1 #Centriod1 x value
      move $a3, $s2 #Centriod1 y value
      jal EuclideanDistance #EuclideanDistance between centroid1 and pointJ is stored in return register $v1 and moved into $a0
      move $s5, $v1 #storing EuclideanDistance of pointJ and centroid1 into $s5
  #Computing EuclideanDistance between point A and Centroid2
      lw $a0, 36($t1) #pointA x value
      lw $a1, 36($t2) #pointA y value
      move $a2, $s3 #Centriod2 x value
      move $a3, $s4 #Centriod2 y value
      jal EuclideanDistance #EuclideanDistance between centroid2 and pointJ is stored in return register $v1 and moved into $a0
      move $s6, $v1 #storing EuclideanDistance of pointJ and centroid2 into $s6

      blt $s5, $s6, storeInClusterCentroid1J #check if EuclideanDistance with centroid1 is < EuclideanDistance with centroid2 and if so place that point in cluster of centroid1 otherwise store in cluster of centroid2.
      #else storeInClusterCentroid2
      lw $t7, 36($t1) #storing pointA_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t5) #storing pointA_x into cluster2ArrayX
      addi $t5, $t5, 4 #moving pointer for cluster2ArrayX to next index
      lw $t7, 36($t2) #storing pointA_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t6) #storing pointA_x into cluster2ArrayY
      addi $t6, $t6, 4 #moving pointer for cluster2ArrayY to next index

      addi $t9, $t9, 1 # number of elements in cluster 2 tracker


      j skipStoringInCentroid1J
  storeInClusterCentroid1J:
      lw $t7, 36($t1) #storing pointJ_x into register $t7 to store into cluster2ArrayX in next instruction
      sw $t7, 0($t3) #storing pointA_x into cluster1ArrayX
      addi $t3, $t3, 4 #moving pointer for cluster1ArrayX to next index
      lw $t7, 36($t2) #storing pointJ_y into register $t7 to store into cluster2ArrayY in next instruction
      sw $t7, 0($t4) #storing pointA_y into cluster1ArrayY
      addi $t4, $t4, 4 #moving pointer for cluster1ArrayY to next index

      addi $t8, $t8, 1 # number of elements in cluster 1 tracker

      j skipStoringInCentroid1J


    skipStoringInCentroid1J:

    #resetting address pointer of the cluster array 1
         move $a0, $0
         addi $a0, $a0, -1

         move $t7, $0
         addi $t7, $t7, 4
         mul $t7, $t7, $t8 #$t7 contains 4 x the number of points in cluster 1
         mul $t7, $t7, $a0 #mul by -1 to make it a negative

         add $t3, $t3, $t7
         add $t4, $t4, $t7


 #resetting address pointer of the cluster array 2
         move $t7, $0
         addi $t7, $t7, 4
         mul $t7, $t7, $t9 #$t7 contains 4 x the number of points in cluster 2
         mul $t7, $t7, $a0 #mul by -1 to make it a negative

         add $t5, $t5, $t7
         add $t6, $t6, $t7







      #print string "Cluster1:"
      li $v0, 4
      la $a0, printC1
      syscall

      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster1ArrayX int
      li $v0, 1
      lw $a0, 0($t3)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster1ArrayY int
      li $v0, 1
      lw $a0, 0($t4)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall

      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster1ArrayX int
      li $v0, 1
      lw $a0, 4($t3)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster1ArrayY int
      li $v0, 1
      lw $a0, 4($t4)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster1ArrayX int
      li $v0, 1
      lw $a0, 8($t3)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster1ArrayY int
      li $v0, 1
      lw $a0, 8($t4)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster1ArrayX int
      li $v0, 1
      lw $a0, 12($t3)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster1ArrayY int
      li $v0, 1
      lw $a0, 12($t4)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster1ArrayX int
      li $v0, 1
      lw $a0, 16($t3)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster1ArrayY int
      li $v0, 1
      lw $a0, 16($t4)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster1ArrayX int
      li $v0, 1
      lw $a0, 20($t3)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster1ArrayY int
      li $v0, 1
      lw $a0, 20($t4)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster1ArrayX int
      li $v0, 1
      lw $a0, 24($t3)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster1ArrayY int
      li $v0, 1
      lw $a0, 24($t4)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster1ArrayX int
      li $v0, 1
      lw $a0, 28($t3)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster1ArrayY int
      li $v0, 1
      lw $a0, 28($t4)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall

      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster1ArrayX int
      li $v0, 1
      lw $a0, 32($t3)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster1ArrayY int
      li $v0, 1
      lw $a0, 32($t4)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall

      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster1ArrayX int
      li $v0, 1
      lw $a0, 36($t3)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster1ArrayY int
      li $v0, 1
      lw $a0, 36($t4)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall

      #print # of elements in Cluster1
      li $v0, 4
      la $a0, printCtotal
      syscall
      #Print value of t8 to see number of elements for first cluster
      li $v0, 1
      move $a0, $t8
      move $v1, $a0
      syscall







      #print string "Cluster2:"
      li $v0, 4
      la $a0, printC2
      syscall

      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster2ArrayX int
      li $v0, 1
      lw $a0, 0($t5)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster2ArrayY int
      li $v0, 1
      lw $a0, 0($t6)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall

      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster2ArrayX int
      li $v0, 1
      lw $a0, 4($t5)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster2ArrayY int
      li $v0, 1
      lw $a0, 4($t6)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster2ArrayX int
      li $v0, 1
      lw $a0, 8($t5)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster2ArrayY int
      li $v0, 1
      lw $a0, 8($t6)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster2ArrayX int
      li $v0, 1
      lw $a0, 12($t5)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster2ArrayY int
      li $v0, 1
      lw $a0, 12($t6)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster2ArrayX int
      li $v0, 1
      lw $a0, 16($t5)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster2ArrayY int
      li $v0, 1
      lw $a0, 16($t6)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster2ArrayX int
      li $v0, 1
      lw $a0, 20($t5)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster2ArrayY int
      li $v0, 1
      lw $a0, 20($t6)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster2ArrayX int
      li $v0, 1
      lw $a0, 24($t5)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster2ArrayY int
      li $v0, 1
      lw $a0, 24($t6)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall


      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster2ArrayX int
      li $v0, 1
      lw $a0, 28($t5)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster2ArrayY int
      li $v0, 1
      lw $a0, 28($t6)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall

      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster2ArrayX int
      li $v0, 1
      lw $a0, 32($t5)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster2ArrayY int
      li $v0, 1
      lw $a0, 32($t6)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall

      #print left brakt
      li $v0, 4
      la $a0, printLeftBrkt
      syscall
      #Print value of cluster2ArrayX int
      li $v0, 1
      lw $a0, 36($t5)
      move $v1, $a0
      syscall
      #print comma
      li $v0, 4
      la $a0, printComma
      syscall
      #Print value of cluster2ArrayY int
      li $v0, 1
      lw $a0, 36($t6)
      move $v1, $a0
      syscall
      #print righ brakt
      li $v0, 4
      la $a0, printRightBrkt
      syscall

      #print # of elements in Cluster2
      li $v0, 4
      la $a0, printCtotal
      syscall
      #Print value of t9 to see number of elements for first cluster
      li $v0, 1
      move $a0, $t9
      move $v1, $a0
      syscall



      lw $ra, 0($sp)           # restore saved return address
      lw $t7, 4($sp)
      addi $sp, $sp, 8        # restore stack pointer (dispose of stack frame)
      jr $ra                   # jump to return address





      EuclideanDistance:
            #setting up stack for function use; saving original t1,t0,s0 values(which are pointA and point B in k-means main (s0 not ussed yet))
            #so we dont lose them and so we can temporial use those registers for this function run
            addi $sp, $sp, -8 #make room on stack for 3 registers
            sw $t0, 0($sp) # save $t0 on stack for use afterwards; will store $t0 = (x2-x1)^2
            sw $t1, 4($sp) # save $t1 on stack for use afterwards; will store $t1 = (y2-y1)^2
          #  sw $s0, 8($sp) # save $s0 on stack for use afterwards; store Distance

            #EuclideanDistance arthimetic
            #$a0= point's x; $a1= point's y; $a2= centroid's x; $a3= centroid's y
            sub $t0, $a2, $a0 #x2-x1
            mul $t0, $t0, $t0 #$t0 = (x2-x1)^2
            sub $t1, $a3, $a1 #y2-y1
            mul $t1, $t1, $t1 #$t1 = (y2-y1)^2
            add $a0, $t0, $t1 #$s0 = (x2-x1)^2 + (y2-y1)^2

            #restoring registers with their orignal values before function call and restoring stack
        #    lw $s0, 8($sp) # restore register $s0 for caller
            lw $t0, 0($sp) # restore register $t0 for caller
            lw $t1, 4($sp) # restore register $t1 for caller
            addi $sp, $sp, 8 #make room on stack for 3 registers

            #Print value of int
            li $v0, 1
            move $v1, $a0
            syscall
            #print printEndLine
            li $v0, 4
            la $a0, printEndLine
            syscall

            jr $ra #return back to main after executing
