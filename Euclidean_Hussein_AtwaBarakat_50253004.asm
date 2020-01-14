#
          # HARD CODED INPUTS
          # INPUTS | WHERE/HOW THEY ARE STORED
          # Data Point x1 stored in .word directive  pointA_x
          # Data Point y1 stored in .word directive  pointA_y
          # Data Point x2 stored in .word directive  Centriod1_x
          # Data Point y2 stored in .word directive  Centriod1_y
#
.data
#Centroid input
  Centriod1_x: .word 6
  Centriod1_y: .word 2
#Point input
  pointA_x: .word 1
  pointA_y: .word 5




  printEndLine: .asciiz " \n"


.text
.globl EuclideanDistance

main:
lw $a0, pointA_x
lw $a2, Centriod1_x

lw $a1, pointA_y
lw $a3, Centriod1_y
jal EuclideanDistance

#Tell the system program is done, if this is not there till be never ending program, infinite recursion
li $v0, 10
syscall
.end main


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
