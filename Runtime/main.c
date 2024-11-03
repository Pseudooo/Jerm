#include <stdio.h>

#define LDCNST 1
#define STLOC 2
#define LDLOC 3
#define ADD 4
#define SUB 5

int exec(long* code, long code_len);

int main()
{
	/*
		IL example for
		```
		var a = 1;
		var b = 2;
		var c = a + b;
		```
	*/
	// long code[] = {
	// 	3, // Locals Size
	// 	2, // Max stack size
	// 	1, // Opcode ldcnst
	// 	2, // 	ldcnst param - value to load to stack
	// 	2, // OpCode stloc
	// 	0, // 	stloc param - locals offset of a
	// 	1, // opcode ldcnst
	// 	1, // 	ldcnst param - value to load to stack
	// 	2, // OpCode stloc
	// 	1, // 	stloc param - locals offset of b
	// 	3, // OpCode ldloc
	// 	0, // 	ldloc param - locals offset of a
	// 	3, // OpCode ldloc
	// 	1, // 	ldloc param - locals offset of b
	// 	5, // OpCode add
	// 	2, // OpCode stloc
	// 	2, // 	stloc param - locals offset of c
	// };

	long code[] = {
		3,2,1,1,2,0,1,2,2,1,3,0,3,1,4,2,2
	};

	exec(code, 17);
	return 0;
}

int exec(long* code, long code_len) 
{
	long locals_size = code[0], stack_size = code[1];

	//Stack stucture is locals > stack_size > stack body
	long stack_frame[1 + locals_size + stack_size];

	long* locals = stack_frame; 
	long* curr_stack_size = &stack_frame[locals_size];
	*curr_stack_size = 0l;
	long* stack_body = &stack_frame[locals_size + 1];

	long curr = 2;
	while(1) 
	{
		if(curr == code_len) {
			printf("*** END ***\n");
			break;
		}

		long opCode = code[curr];
		long jmp = 0;
		printf("Have opcode %ld at %ld with stack size %ld\n", opCode, curr, *curr_stack_size);

		if(opCode == LDCNST) 
		{
			long param = code[curr + 1];
			stack_body[*curr_stack_size] = param;
			printf("LDCNST Loading %ld to s%ld\n", param, *curr_stack_size);

			(*curr_stack_size)++;
			jmp = 2;
		}
		else if(opCode == LDLOC) 
		{
			long param = code[curr + 1];
			stack_body[*curr_stack_size] = locals[param];
			printf("LDLOC Loading .%ld (%ld) to s%ld\n", param, locals[param], *curr_stack_size);

			(*curr_stack_size)++;
			jmp = 2;
		}
		else if(opCode == STLOC) 
		{
			long param = code[curr + 1];
			locals[param] = stack_body[*curr_stack_size - 1];
			printf("STLOC Storing s%ld (%ld) to .%ld\n", *curr_stack_size - 1, stack_body[*curr_stack_size - 1], param);

			(*curr_stack_size)--;
			jmp = 2;
		}
		else if(opCode == ADD) 
		{
			long right = stack_body[*curr_stack_size - 1];
			long left = stack_body[*curr_stack_size - 2];
			printf("ADD Executing s%ld (%ld) + s%ld (%ld)\n", *curr_stack_size - 2, left, *curr_stack_size - 1, right);

			(*curr_stack_size) -= 2;
			stack_body[*curr_stack_size] = left + right;
			(*curr_stack_size)++;
			jmp = 1;
		}
		else if(opCode == SUB) 
		{
			long right = stack_body[*curr_stack_size - 1];
			long left = stack_body[*curr_stack_size - 2];
			printf("SUB Executing s%ld (%ld) - s%ld (%ld)\n", *curr_stack_size - 2, left, *curr_stack_size - 1, right);

			(*curr_stack_size) -= 2;
			stack_body[*curr_stack_size] = left - right;
			(*curr_stack_size)++;
			jmp = 1;
		}

		printf("Jumping %ld\n", jmp);
		curr = curr + jmp;
	}

	printf("Res: %ld & %ld & %ld\n", locals[0], locals[1], locals[2]);
}
