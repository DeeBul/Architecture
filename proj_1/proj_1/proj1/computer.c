#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips            /* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

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
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;
    
    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
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
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
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
        printf ("ADDR      CONTENTS\n");
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
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

/* Decode instr, returning decoded instruction. */
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    unsigned int decode = instr; //equalling to the whole 32 bit integer
    
    decode = decode >> 26; //shifting to get first 6 bits in the instruction
    
    d->op = decode;
    
    if (decode == 0) { // Since it is zero it is R format because first 6 bits are 0
        d->type = 0;
        decode = instr;
        
        // the & operator zeros out zero bits and singles out ones bit
        decode = decode & 0x0000003f; // The hex address corresponds to the funct section of R format.
        
        d->regs.r.funct = decode;
        
        switch(decode) {
            case 33: // addu
            case 35: // subu
            case 0:  // sll
            case 2:  // srl
            case 36: // and
            case 37: // or
            case 42: // slt
            case 8:  // jr
                
            {
                decode = instr; // getting back full 32 bits
                decode = decode & 0x03e00000; // Corresponds to RS of the R format.
                decode = decode >> 21;
                d->regs.r.rs = decode;
                rVals->R_rs = mips.registers[decode];
                
                
                decode = instr;
                // 0000 0000 0001 1111 0000 0000 0000 0000
                
                decode = decode & 0x001f0000; // Corresponds to RT of the R format
                decode = decode >> 16;
                d->regs.r.rt = decode; //we use arrow because d is a pointer and . because the variables are on the stack
                rVals->R_rt = mips.registers[decode]; //same with rVals, pointer
                
                decode = instr;
                
                // 0000 0000 0000 0000 1111 1000 0000 0000
                // 0000F800
                
                decode = decode & 0x0000f800; // Corresponds to the RD of the R format
                decode = decode >> 11;
                d->regs.r.rd = decode;
                rVals->R_rd = mips.registers[decode];
                
                decode = instr;
                
                // 0000 0000 0000 0000 0000 0111 1100 0000
                // 0x000007C0
                
                decode = decode & 0x000007c0; // Corresponds to the Shamt of the R format
                decode = decode >> 6;
                d->regs.r.shamt = decode;
                
                break;
                
            }
            default: {
                exit(0);
            }
        }
    }
    // J Format
    else if(decode == 2 || decode == 3) {
        d->type = 2;
        
        decode = instr;
        
        // 0000 0011 1111 1111 1111 1111 1111 1111
        //  0x03FFFFFF
        
        decode = decode & 0x03ffffff; // corresponds to the last 26 bits of J format which is the address
        
        d->regs.j.target = decode;
    }
    
    else {
        switch(d->op) { //i format
            case 4:
            case 5:
            case 9:
            case 12:
            case 13:
            case 15:
            case 35:
            case 43:
                
            {
                d->type = 1;
                decode = instr;
                
                //  0000 0011 1110 0000 0000 0000 0000 0000
                // 0x03E0000
                
                decode = decode & 0x03e00000; // corresponds to RS of I format
                decode = decode >> 21;
                d->regs.i.rs = decode;
                rVals->R_rs = mips.registers[decode];
                
                decode = instr;
                
                //  0000 0000 0001 1111 0000 0000 0000 0000
                // 0x001F0000
                decode = decode & 0x001f0000; // corresponds to RT of I format
                decode = decode >> 16; //need to shift 16 bits to the right because we want to get those decoded bits
                
                //the & operator makes all the zeros zeros and makes singles out the ones meaning that we need to shift 16 to the right to get those bits
                d->regs.i.rt = decode;
                rVals->R_rt = mips.registers[decode];
                
                decode = instr;
                
                if(d->op == 4 || d->op == 5 || d->op == 9) { //bne beq addius
                    unsigned int signext = instr;
                    
                    signext = signext & 0x0000ffff;
                    
                    if(instr & 0x00008000) {
                        signext = signext + 0xffff0000;
                    }
                    
                    else {
                        signext = signext + 0x00000000;
                    }
                    
                    d->regs.i.addr_or_immed = signext;
                    
                    
                }
                
                else {
                    decode = decode & 0x0000ffff;
                    d->regs.i.addr_or_immed = decode;
                }
                
                break;
                
                
            }
                
            default: {
                exit(1);
            }
                
                
        }
    }
    
}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {
    
    if (d->type == 0) { //R format
        unsigned int rs = d->regs.r.rs;
        unsigned int rt = d->regs.r.rt;
        unsigned int rd = d->regs.r.rd;
        unsigned int shamt = d->regs.r.shamt;
        
        if(d->regs.r.funct == 33) {
            // addu
            printf("addu\t $%d, $%d, $%d\n", rd, rs, rt);
        }
        
        else if(d->regs.r.funct == 35) {
            printf("subu\t $%d, $%d, $%d\n", rd, rs, rt);
        }
        
        else if(d->regs.r.funct == 0) { //sll
            printf("sll\t $%d, $%d, %d\n", rd, rs, shamt);
            exit(1);
        }
        
        else if(d->regs.r.funct == 2) {
            printf("srl\t $%d, $%d, $%d\n", rd, rs, shamt);
            
        }
        
        else if(d->regs.r.funct == 36) {
            printf("and\t $%d, $%d, $%d\n", rd, rs, rt);
        }
        
        else if(d->regs.r.funct == 37) {
            printf("or\t $%d, $%d, $%d\n", rd, rs, rt);
        }
        
        else if(d->regs.r.funct == 42) {
            printf("slt\t $%d, $%d, $%d\n", rd, rs, rt);
        }
        
        else if(d->regs.r.funct == 8) {
            printf("jr\t $%d\n", rs);
        }
    }
    
    if(d->type == 1) { //I format
        unsigned int I_rs = d->regs.i.rs;
        unsigned int I_rt = d->regs.i.rt;
        unsigned int immediate = d->regs.i.addr_or_immed;
        
        if(d->op == 9) {
            printf("addiu\t $%d, $%d, %d\n", I_rt, I_rs, immediate);
        }
        
        if(d->op == 12) {
            printf("andi\t $%d, $%d, %d\n", I_rt, I_rs, immediate);
        }
        
        if(d->op == 13) {
            printf("ori\t $%d, $%d, %d\n", I_rt, I_rs, immediate);
        }
        
        if(d->op == 15) {
            printf("lui\t $%d, %d\n", I_rt, immediate);
        }
        
        if(d->op == 4) {
            unsigned int branch = (mips.pc + 4) + (immediate * 4); //pc + 4 is important because the addresses are in bytes so the next address would be +4 and so on
            printf("beq\t $%d, $%d, %d\n", I_rt, I_rs, branch);// for branching the "immediate section" would be (pc + 4) * whatever the number of lines it has to branch to
        }
        
        if(d->op == 5) {
            unsigned int branch = (mips.pc + 4) + (immediate * 4);
            printf("bne\t $%d, $%d, %d\n", I_rt, I_rs, branch);
        }
        
        if(d->op == 35) {
            printf("lw\t $%d, %d($%d)\n", I_rt, I_rs, immediate);
        }
        
        if(d->op == 43) {
            printf("sw\t $%d, %d($%d)\n", I_rt, I_rs, immediate);
        }
    }
    
    if(d->type == 2) { //J Format
        unsigned int jump = d->regs.j.target;
        // unsigned int addr = mips.pc;
        
        
        if(d->op == 2) {
            // val = val << 2;
            // jump = jump << 2;
            // addr = addr & 0xf0000000;
            // addr = jump+addr;
            
            
            printf("j\t %x\n", jump); //fix to get the first 2 hex values in front
        }
        if(d->op == 3) {
            printf("jal\t %x\n", jump);
        }
        
    }
    
    /* Your code goes here */
}

/* Perform computation needed to execute d, returning computed value */
int Execute ( DecodedInstr* d, RegVals* rVals) {
    
    if(d->type == 0) { //R format
        
        if(d->regs.r.funct == 33) {
            rVals->R_rd = rVals->R_rs + rVals->R_rt;
            return rVals->R_rd;
        }
        
        else if(d->regs.r.funct == 0) {
            rVals->R_rd = rVals->R_rt << d->regs.r.shamt;
            return rVals->R_rd;
        }
        
        else if(d->regs.r.funct == 2) {
            rVals->R_rd = rVals->R_rt >> d->regs.r.shamt;
            return rVals->R_rd;
        }
        
        else if(d->regs.r.funct == 35) {
            rVals->R_rd = rVals->R_rs - rVals->R_rt;
            return rVals->R_rd;
        }
        
        else if(d->regs.r.funct == 36) {
            rVals->R_rd = rVals->R_rs & rVals->R_rt;
            return rVals->R_rd;
        }
        
        else if(d->regs.r.funct == 37) {
            rVals->R_rd = rVals->R_rs | rVals->R_rt;
            return rVals->R_rd;
        }
        else if(d->regs.r.funct == 42) {
            rVals->R_rd = (rVals->R_rs < rVals->R_rt) ? 1 : 0;
            return rVals->R_rd;
        }
        
        else if(d->regs.r.funct == 8) {
            return d->regs.r.rs;
        }
    }
    
    else if (d->op == 2 || d->op == 3) {
        return d->regs.j.target;
    }
    
    else if(d->type == 1) {
        
        unsigned int immediate = d->regs.i.addr_or_immed;
        
        if(d->op == 9) {
            rVals->R_rt = rVals->R_rs + immediate;
            return rVals->R_rt;
        }
        
        else if(d->op == 12) {
            rVals->R_rt = rVals->R_rs & immediate;
            return rVals->R_rt;
        }
        
        else if (d->op == 13) {
            rVals->R_rt = rVals->R_rs | immediate;
            return rVals->R_rt;
        }
        else if(d->op == 15) {
            rVals->R_rt = immediate << 16;
            return rVals->R_rt;
        }
        
        else if(d->op == 4) {
            if(rVals->R_rs == rVals->R_rt || rVals->R_rs - rVals->R_rt == 0) {
                return immediate;
            }
            else {
                return 0;
            }
        }
        else if(d->op == 5) {
            if(rVals->R_rs != rVals->R_rt || rVals->R_rs - rVals->R_rt != 0) {
                return immediate;
            }
            else {
                return 0;
            }
        }
        
        else if (d->op == 35) {
            int lw = rVals->R_rs + immediate;
            return lw;
        }
        
        else if (d->op == 43) {
            int sw = rVals->R_rs + immediate;
            return sw;
        }
    }
    
    return 0;
}

/*
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {
    
    // if statements made program go to jal only but switch statements went through the whole program ???
    
    switch (d->op) {
            
        case 0: {
            if (d->regs.r.funct == 8) { //jr
                mips.pc = mips.registers[val];
            }
            
            else {
                mips.pc+=4;
            }
            
            break;
        }
            
        case 4:
        case 5: {
            
            if ((val & 0x80000000) < 0) {
                
                mips.pc = mips.pc + 4 - (val * 4);
                
            } else {
                
                mips.pc = mips.pc + 4 + (val * 4);
            }
            
            break;
        }
        case 3:
            
            mips.registers[31] = mips.pc + 4;
            
        case 2:  {
            //jump
            unsigned int MIPSPC = mips.pc;
            
            //Shift val by 2 to the left to add 00
            val = val << 2;
            
            //Isolating the first 4 bits of the PC
            MIPSPC = MIPSPC & 0xf0000000;
            
            
            //Grab the 4 bits from MIPSPC, the 26 from val plus the 00
            mips.pc = val + MIPSPC;
            
            break;
        }
            
        default:
            mips.pc+=4;
            
    }
    /* Your code goes here */
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
int Mem( DecodedInstr* d, int val, int *changedMem) {
    /* Your code goes here */
    
    if(d->type == 0) { //r format
        if (d->regs.r.funct == 35) { //subu
            *changedMem = -1;
            val = val - 0x00400000; //since this is where we start in mips memory we want to take the difference so we can see the index
            val = val / 4; // from there we divide by four from the index because to get to the address
            rVals.R_rt = mips.memory[val]; //store adress into RT
            return rVals.R_rt;
        }
    }
    
    else if(d->type == 1) {
        if(d->op == 43) { //sltu
            *changedMem = val;
            val = val - 0x00400000;
            val = val / 4;
            mips.memory[val] = rVals.R_rt;
            return *changedMem;
        }
    }
    
    else {
        *changedMem = -1;
    }
    
    
    return 0;
}

/*
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    /* Your code goes here */
    
    if(d->type == 0) { //makes sure is r format
        if(d->regs.r.funct == 8) { //when it is equal to addi
            *changedReg = -1; //dont do anything 
        }
        else {
            mips.registers[d->regs.r.rd] = rVals.R_rd; //if not equal to an i format instruction add the value of rd to mips register
            *changedReg = d->regs.r.rd; //change register
        }
    }
    
    else if(d->type == 1) {
        if (d->op == 43 || d->op == 4 || d->op == 5) {
            *changedReg = -1;
        }
        else {
            mips.registers[d->regs.i.rt] = rVals.R_rt;
            *changedReg = d->regs.i.rt;
        }
    }
    
    else {
        *changedReg = -1;
    }
}

