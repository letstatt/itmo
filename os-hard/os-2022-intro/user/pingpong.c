#include "kernel/types.h"
#include "user/user.h"

void readStrAndPrint(int readable_pipe, char* buf, int length);

void sendStr(int writeble_pipe, char* buf, int length);

int main() {
  // pipe[0] - readable, pipe[1] - writable
  int p2c[2];
  int c2p[2];

  if (pipe(&p2c[0]) < 0 || pipe(&c2p[0]) < 0) {
    printf("pingpong: pipe() failed\n");
    exit(1);
  }

  char buf[5];

  switch (fork()) {
    case -1: {
      printf("pingpong: fork() failed\n");
      break;
    }
    case 0: {
      // child's code
      readStrAndPrint(p2c[0], &buf[0], 5);
      sendStr(c2p[1], "pong\0", 5);
      exit(0);
    }
    default: {
      // parent's code
      sendStr(p2c[1], "ping\0", 5);
      readStrAndPrint(c2p[0], &buf[0], 5);
      break;
    }
  }

  // smart handles would be handy
  close(p2c[0]);
  close(p2c[1]);
  close(c2p[0]);
  close(c2p[1]);
  exit(0);
}

void readStrAndPrint(int readable_pipe, char* buf, int length) {
  read(readable_pipe, &buf, length);
  printf("%d: got %s\n", getpid(), &buf);
}

void sendStr(int writeble_pipe, char* buf, int length) {
  write(writeble_pipe, buf, length);
}
