#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

int main(int argc, char ** argv) {
	int devnull;

	if (argc < 2) {
		printf("Usage: %s command [args...]\n", argv[0]);
		return 1;
	}

	devnull = open("/dev/null", O_WRONLY);

	if (devnull < 0) {
		perror("open");
		return 1;
	}

	if (devnull != 1) dup2(devnull, 1);
	if (devnull != 2) dup2(devnull, 2);
	if (devnull != 1 && devnull != 2) close(devnull);

	execvp(argv[1], argv + 1);

	return 1;
}
