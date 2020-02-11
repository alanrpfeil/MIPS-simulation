#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);
int unsupported;

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4; //register 29 is PC* and 00400000 is program start address while maxnuminstr+maxnumdata is mult by 4 for bytes->words

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;	//increments by *word* in the memory, NOT bytes hence int initialization of memory array
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */

void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
	while (1) {					
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);
	
		if (unsupported == 1) {	//checking if unsupported instruction or not for project
			break;
		}

     /* 
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

     /* 
	 * Perform computation needed to execute d, returning computed value 
	 * in val 
	 */
        val = Execute(&d, &rVals);

		//printf("%d\n", val); //testing

	UpdatePC(&d,val);

     /* 
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem, 
	 * otherwise put -1 in *changedMem. 
	 * Return any memory value that is read, otherwise return -1.
     */
        val = Mem(&d, val, &changedMem);

     /* 
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        PrintInfo (changedReg, changedMem);

	  /*if (d.regs.r.funct == 8) {			//breaks when jr $ra is the previously-fetched instruction
			break;
		}*/
		if (d.regs.r.funct == 8) {
			break;							//breaks when 0x00000000 is the fetched instruction
		}

    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the		*IMPORTANT* //update changed mem or changed reg to the index of where the changed mem/reg is to make printing more readable.
 *  simulated instruction, otherwise -1.								*IMPORTANT*
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */

void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch. 
 */

unsigned int Fetch (int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

//using a mask to grab bits n->m of the fetched instruction
unsigned int createMask(unsigned int instruction, unsigned int lower, unsigned int upper) {
	unsigned int opMask = instruction;
	lower = (31-upper) + lower;

		opMask = opMask << (31-upper);
		opMask = opMask >> lower;
	
	return opMask;
}

unsigned int createMaskD(DecodedInstr* d, unsigned int lower, unsigned int upper) {
	int opMask = d->regs.i.addr_or_immed;
	opMask += 0x11110000;				//adding leading *ONES*

	lower = (31 - upper) + lower;

	opMask = opMask << (31 - upper);
	opMask = opMask >> lower;

	return opMask;
}

int flip(DecodedInstr* d) {				//utilizes 2's complement
	int dummyImm = 0x00000000;
	int rep = 0x00000000;

	for (int i = 0; i <= 15; i++) {
		if (i == 0) {
			rep = (createMaskD(d, 0, 0) * 1);
			rep = ~rep;
			dummyImm += rep;
			//printf("dummyImm: %d\n rep: %d\n", dummyImm, rep);	//testing
			continue;
		}
		rep = (createMaskD(d, i, i));
		rep = ~rep;
		for (int j = 0; j < i; j++) {
			rep *= 2;
		}
		dummyImm += rep;
		//printf("dummyImm: %d\n rep: %d\n", dummyImm, rep);		//testing
	}
	dummyImm--;

	//printf("%d\n", dummyImm);			//testing
	return (dummyImm);
}

/* Decode instr, returning decoded instruction. */
void Decode (unsigned int instr, DecodedInstr* d, RegVals* rVals) {
	
	d->op = createMask(instr, 26, 31);	//setting opcode to first 6 bits of the instruction using a mask defined above
	//printf("%d\n", d->op); //testing

		if (d->op == 0) {
			d->type = R;
			
			d->regs.r.rs = createMask(instr, 21, 25);
			d->regs.r.rt = createMask(instr, 16, 20);
			d->regs.r.rd = createMask(instr, 11, 15);
			d->regs.r.shamt = createMask(instr, 6, 10);
			d->regs.r.funct = createMask(instr, 0, 5);

			//printf("rs: %d\n rt: %d\n rd: %d\n shamt: %d\n funct: %d\n", d->regs.r.rs, d->regs.r.rt, d->regs.r.rd, d->regs.r.shamt, d->regs.r.funct);		//testing
		}
		else if (d->op == 2 || d->op == 3) {
			d->type = J;
			d->regs.j.target = createMask(instr, 0, 25);
		}
		else {
			d->type = I;
				
			d->regs.i.rs = createMask(instr, 21, 25);
			d->regs.i.rt = createMask(instr, 16, 20);
			d->regs.i.addr_or_immed = createMask(instr, 0, 15);

			//printf("rs: %d\n rt: %d\n imm16 or address: %d\n", d->regs.i.rs, d->regs.i.rt, d->regs.i.addr_or_immed);										//testing
		}

}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {
											//All 32 base instructions are here to print, unsupported instructions marked with unsupported = 1; 
		if (d->type == I) {
			if ((d->regs.i.addr_or_immed >> 15) == 1) {	//if 1 in front make negative
				d->regs.i.addr_or_immed = flip(d);
			}
		if (d->op == 8) { printf("addi\t$%d, $%d, %d\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed); unsupported = 1;}
		else if (d->op == 9) { printf("addiu\t$%d, $%d, %d\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed); }
		else if (d->op == 12) { printf("andi\t$%d, $%d, 0x%8.8x\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed); }
		else if (d->op == 4) { printf("beq\t$%d, $%d, 0x%8.8x\n", d->regs.i.rs, d->regs.i.rt, (mips.pc + (4 * d->regs.i.addr_or_immed) + 4)); }
		else if (d->op == 5) { printf("bne\t$%d, $%d, 0x%8.8x\n", d->regs.i.rs, d->regs.i.rt, (mips.pc + (4 * d->regs.i.addr_or_immed) + 4)); }
		else if (d->op == 36) { printf("lbu\t$%d, $%d, %d\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed); unsupported = 1;}
		else if (d->op == 37) { printf("lhu\t$%d, $%d, %d\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed); unsupported = 1;}
		else if (d->op == 48) { printf("ll\t$%d, $%d, %d\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed); unsupported = 1;}
		else if (d->op == 15) { printf("lui\t$%d, 0x%8.8x\n", d->regs.i.rt, d->regs.i.addr_or_immed); }
		else if (d->op == 35) { printf("lw\t$%d, %d($%d)\n", d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs); }
		else if (d->op == 13) { printf("ori\t$%d, $%d, 0x%8.8x\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed); }
		else if (d->op == 10) { printf("slti\t$%d, $%d, %d\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed); unsupported = 1;}
		else if (d->op == 11) { printf("sltiu\t$%d, $%d, %d\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed); unsupported = 1;}
		else if (d->op == 40) { printf("sb\t$%d, %d($%d)\n", d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs); unsupported = 1;}
		else if (d->op == 56) { printf("sc\t$%d, %d($%d)\n", d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs); }
		else if (d->op == 41) { printf("sh\t$%d, %d($%d)\n", d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs); unsupported = 1;}
		else if (d->op == 43) { printf("sw\t$%d, %d($%d)\n", d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs); }
	}
	else if (d->type == R) {
		if (d->regs.r.funct == 32) { printf("add\t$%d, $%d, $%d\n",d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); unsupported = 1;}
		else if (d->regs.r.funct == 33) { printf("addu\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); }
		else if (d->regs.r.funct == 36) { printf("and\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); }
		else if (d->regs.r.funct == 8) { printf("jr\t$%d\n",d->regs.r.rs); }
		else if (d->regs.r.funct == 39) { printf("nor\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); unsupported = 1;}
		else if (d->regs.r.funct == 37) { printf("or\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); }
		else if (d->regs.r.funct == 42) { printf("slt\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); }
		else if (d->regs.r.funct == 43) { printf("stlu\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); unsupported = 1;}
		else if (d->regs.r.funct == 0) { printf("sll\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rt, d->regs.r.shamt); }
		else if (d->regs.r.funct == 2) { printf("srl\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rt, d->regs.r.shamt); }
		else if (d->regs.r.funct == 34) { printf("sub\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); unsupported = 1;}
		else if (d->regs.r.funct == 35) { printf("subu\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); }
	}
	else {
		if (d->op == 2) { printf("j\t0x%8.8x\n", (d->regs.j.target + (mips.pc - d->regs.j.target)) - 12); }
		else if (d->op == 3) { printf("jal\t0x%8.8x\n", (d->regs.j.target + (mips.pc - d->regs.j.target)) + 8); }
	}
}

/* Perform computation needed to execute d, returning computed value */
/* I ONLY INCLUDED THE REQUIRED INSTRUCTIONS FOR THE PROJECT; WON'T COMPUTE ALL 32 BASE INSTRUCTIONS (which are still printed) */
int Execute ( DecodedInstr* d, RegVals* rVals) {

	if (d->type == R) {
		if (d->regs.r.funct == 33) {	//addu
			return (mips.registers[d->regs.r.rs] + (unsigned int)mips.registers[d->regs.r.rt]);
		}
		if (d->regs.r.funct == 35) {	//subu
			return (mips.registers[d->regs.r.rs] - (unsigned int)mips.registers[d->regs.r.rt]);
		}
		if (d->regs.r.funct == 0) {		//sll
			return (mips.registers[d->regs.r.rs] << mips.registers[d->regs.r.rt]);
		}
		if (d->regs.r.funct == 2) {		//srl
			return (mips.registers[d->regs.r.rs] >> mips.registers[d->regs.r.rt]);
		}
		if (d->regs.r.funct == 36) {	//and
			return (mips.registers[d->regs.r.rs] && mips.registers[d->regs.r.rt]);
		}
		if (d->regs.r.funct == 37) {	//or
			return (mips.registers[d->regs.r.rs] || mips.registers[d->regs.r.rt]);
		}
		if (d->regs.r.funct == 42) {	//slt
			return (mips.registers[d->regs.r.rs] < mips.registers[d->regs.r.rt]);
		}
		if (d->regs.r.funct == 8) {		//jr
			return mips.registers[31];
		}
	}
	else if (d->type == I) {
		
		/*if (d->regs.i.addr_or_immed < 0 && d->op == 9) {	//if addiu negative
			if (d->op == 9) {	//addiu
				d->regs.i.addr_or_immed = flip(d);
				return (mips.registers[d->regs.i.rs] - d->regs.i.addr_or_immed);
			}
		} */

		if (d->op == 9) {	//addiu
			return (mips.registers[d->regs.i.rs] + d->regs.i.addr_or_immed);
		}
		if (d->op == 12) {	//andi
			return (mips.registers[d->regs.r.rs] && d->regs.i.addr_or_immed);
		}
		if (d->op == 13) {	//ori
			return (mips.registers[d->regs.r.rs] || d->regs.i.addr_or_immed);
		}
		if (d->op == 15) {	//lui
			return (d->regs.i.addr_or_immed);
		}
		if (d->op == 4) {	//beq
			return (mips.registers[d->regs.r.rs] == mips.registers[d->regs.r.rt]);	//returns 0 or 1
		}
		if (d->op == 5) {	//bne
			return (mips.registers[d->regs.r.rs] != mips.registers[d->regs.r.rt]);	//returns 0 or 1
		}
		if (d->op == 35) {	//lw													//returns addr that register will load
			return mips.memory[(mips.registers[d->regs.r.rs] + d->regs.i.addr_or_immed)];
		}
		if (d->op == 43) {	//sw
			return mips.registers[d->regs.r.rt];									//value to be stored in MEM
		}
	}
	else if (d->type == J) {
		if (d->op == 2) {				//j
			return d->regs.j.target;	//returns address/tag
		}
		if (d->op == 3) {				//jal
			return d->regs.j.target;	//returns address/tag
		}
	}

  return 0;
}

/* 
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC (DecodedInstr* d, int val) {
	int pcBits;
	//printf("%d\n", val);							//tesing
	if (d->op == 2) {								//j
		val = val << 2;								/*shifting target address by 4 and concatenating first 4 bits of pc onto leftmost 4 bits of target address to get full 32-bit address*/
		pcBits = mips.pc;
		pcBits = pcBits >> 28;
		pcBits = pcBits << 28;
		val += pcBits;
		mips.pc = (val - 4);						//not including the PC+4 at the end (twice)
    }
	else if (d->op == 3) {							//jal
		mips.registers[31] = (mips.pc + 4);			//setting new return address
		val = val << 2;								/*shifting target address by 4 and concatenating first 4 bits of pc onto leftmost 4 bits of target address to get full 32-bit address*/
		pcBits = mips.pc;
		pcBits = pcBits >> 28;
		pcBits = pcBits << 28;
		val += pcBits;
		mips.pc = (val - 4);						//not including the PC+4 at the end (twice)
	}
	else if (d->regs.r.funct == 8) {				//jr
		mips.pc = val - 4;								//PC goes to return address (placed in val)
	}
	else if (d->op == 4 && val == 1) {				//beq and branch taken
		mips.pc += (d->regs.i.addr_or_immed * 4);
	}
	else if (d->op == 5 && val == 1) {				//bne and branch taken
		mips.pc += (d->regs.i.addr_or_immed * 4);
	}
		mips.pc += 4;	//continues
}

/*
 * Perform memory load or store. Place the address of any updated memory 
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value 
 * that is read, otherwise return -1. 
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory 
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1] 
 * with address 0x00400004, and so forth.
 *
 */
int Mem(DecodedInstr* d, int val, int *changedMem) {
	if (d->op == 35) {		//lw
		*changedMem = -1;
		return mips.memory[(mips.registers[d->regs.r.rs]%1024 + 1024) + d->regs.i.addr_or_immed];	//saving value in mem for regWrite
	}
	else if (d->op == 43) {	//sw
		mips.memory[(mips.registers[d->regs.r.rs]%1024 + 1024) + d->regs.i.addr_or_immed] = val;
		*changedMem = (0x00400000 + mips.registers[d->regs.r.rs] + d->regs.i.addr_or_immed);
		return 0;	//we don't perform regWrite
	}
	*changedMem = -1;
  return val;
}

/* 
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
	if (d->type == R && d->regs.r.funct != 8) {							//jr doesn't regWrite
			mips.registers[d->regs.r.rd] = val;
			*changedReg = d->regs.r.rd;
	}
	else if (d->op == 3) {												//jal writes address into $ra
		*changedReg = 31;
	}
	else if (d->type == I && d->op != 43 && d->op != 4 && d->op != 5) {	//don't regwrite if sw instruction
		mips.registers[d->regs.i.rt] = val;
		*changedReg = d->regs.i.rt;
	}
	else {
		*changedReg = -1;
	}
}
