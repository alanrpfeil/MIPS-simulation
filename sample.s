# Short test case for your project.
#
# Note that this is by no means a comprehensive test!
#



.text

		addiu	$a0,$0,3
		addiu	$a1,$0,-2
		subu	$s0, $s1, $s2
		sll		$t0, $t1, 4
		srl		$t1, $t4, 2
		and		$v0, $a1, $a0
		andi	$v1, $a1, 0
		or		$s2, $t0, $t6
		ori		$t0, $s2, 1
		lui		$s0, 0x00AABB48
		slt		$t0, $t1, $t0
		bne		$t3, $t4, Loop
		lw		$t0, 8($sp)
#		addi	$0,$0,0 #unsupported instruction, terminate
		sw		$t0, -16($sp)
		jal	Mystery
		jr	$ra

Mystery:
		addiu	$v0,$0,0
Loop:
		beq	$a0,$0,Done
		addu	$v0,$v0,$a1
		addiu	$a0,$a0,-1
		j Loop
Done:	
		jr		$ra