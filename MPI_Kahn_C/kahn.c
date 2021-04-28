#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <string.h>

#include "kahn.h"

// #define DEBUG

channel *new_channel() {
   channel *chan = (channel*) malloc(sizeof(channel));
    int fd[2];
    pipe(fd);
    chan->fd_in = fd[1];
    chan->fd_out = fd[0];
    return chan;
    }

void put_int(int n, channel *chan) {
    write(chan->fd_in, &n, sizeof(int));
    }

int get_int(channel *chan) {
    int res;
    while(read(chan->fd_out, &res, sizeof(int)) == 0) {};
    return res;
    }

void put_double(double x,channel *chan) {
    write(chan->fd_in, &x, sizeof(double));
    }

double get_double(channel *chan) {
    double res;
    while(read(chan->fd_out, &res, sizeof(double)) == 0) {};
    return res;
    }

// Prend en entrée une liste de fonction et une liste d'arguments)
void doco(int nb_proc, process processes[], void **arguments) {

#ifdef DEBUG
    printf("Entering doco to fork %d processes\n", nb_proc);
#endif

    pid_list = malloc(nb_proc*sizeof(pid_t));
    int is_main = 1;

    for (int i=0; i<nb_proc; i++) {
        if (!is_main) { break; }

        pid_t pid = fork();
        if (pid == 0) {
            is_main = !is_main;
            (*processes[i])(arguments[i]);
        }
        else { 

#ifdef DEBUG
            printf("Creating [%d]\n", (int)pid);
#endif
            pid_list[i] = pid;
        }
    }
    wait(NULL);
}