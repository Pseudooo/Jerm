#include <stdio.h>

#define LDCNST 1
#define STLOC 2
#define LDLOC 3
#define ADD 4
#define SUB 5

int exec(int* code, int code_len);

int main(int argc, char* argv[])
{
	if(argc != 2) {
		printf("Invalid number of arguments");
		return 1;
	}

	char* filePath = argv[1];
	FILE* f = fopen(filePath, "r");
	if(f == NULL) {
		printf("Error opening file");
		return 1;
	}

	int code[1024];
	for(int i = 0; i < 1024; i++)
		code[i] = -1;

	fread(code, sizeof(int), 1024, f);
	fclose(f);

	int code_len = 0;
	while(code[code_len] != -1)
		code_len++;

	exec(code, code_len);
	return 0;
}


int exec(int* code, int code_len)
{
	int locals_size = code[0], stack_size = code[1];

	//Stack structure is locals > stack_size > stack body
	int stack_frame[1 + locals_size + stack_size];

	int* locals = stack_frame;
	int* curr_stack_size = &stack_frame[locals_size];
	*curr_stack_size = 0l;
	int* stack_body = &stack_frame[locals_size + 1];

	int curr = 2;
	while(1) 
	{
		if(curr == code_len) {
			printf("*** END ***\n");
			break;
		}

		int opCode = code[curr];
		int jmp = 0;
		printf("Have opcode %ld at %ld with stack size %ld\n", opCode, curr, *curr_stack_size);

		if(opCode == LDCNST) 
		{
			int param = code[curr + 1];
			stack_body[*curr_stack_size] = param;
			printf("LDCNST Loading %ld to s%ld\n", param, *curr_stack_size);

			(*curr_stack_size)++;
			jmp = 2;
		}
		else if(opCode == LDLOC) 
		{
			int param = code[curr + 1];
			stack_body[*curr_stack_size] = locals[param];
			printf("LDLOC Loading .%ld (%ld) to s%ld\n", param, locals[param], *curr_stack_size);

			(*curr_stack_size)++;
			jmp = 2;
		}
		else if(opCode == STLOC) 
		{
			int param = code[curr + 1];
			locals[param] = stack_body[*curr_stack_size - 1];
			printf("STLOC Storing s%ld (%ld) to .%ld\n", *curr_stack_size - 1, stack_body[*curr_stack_size - 1], param);

			(*curr_stack_size)--;
			jmp = 2;
		}
		else if(opCode == ADD) 
		{
			int right = stack_body[*curr_stack_size - 1];
			int left = stack_body[*curr_stack_size - 2];
			printf("ADD Executing s%ld (%ld) + s%ld (%ld)\n", *curr_stack_size - 2, left, *curr_stack_size - 1, right);

			*curr_stack_size -= 2;
			stack_body[*curr_stack_size] = left + right;
			(*curr_stack_size)++;
			jmp = 1;
		}
		else if(opCode == SUB) 
		{
			int right = stack_body[*curr_stack_size - 1];
			int left = stack_body[*curr_stack_size - 2];
			printf("SUB Executing s%ld (%ld) - s%ld (%ld)\n", *curr_stack_size - 2, left, *curr_stack_size - 1, right);

			*curr_stack_size -= 2;
			stack_body[*curr_stack_size] = left - right;
			(*curr_stack_size)++;
			jmp = 1;
		}

		printf("Jumping %ld\n", jmp);
		curr = curr + jmp;
	}

	printf("Program execution finished, locals state is:\n");
	for(int i = 0; i < locals_size; i++)
		printf("locals[%d]: %d\n", i, locals[i]);
}
