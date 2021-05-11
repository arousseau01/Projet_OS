#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <string.h>
#include <assert.h>

#include "kahn.h"

/*      Implémentation de l'interface kahn.h
        Objets encapsulés:
            - _kahn_data_size   // donne les tailles en octets des types utilisés
            - _pid_list         // liste des pid des processus forkés par doco
            - DEBUG             // si défini, affichage d'informations pour debugger
            - _return_process() // fonction du processus retourné par return_()
*/

//#define DEBUG

/* ***   COMMUNICATION   *** */

channel *new_channel() {
    channel *chan = (channel*) malloc(sizeof(channel));
    int fd[2];
    pipe(fd);   // initiation d'une pipe, retourne les file descriptor de in et out dans fd
    chan->fd_in = fd[1];
    chan->fd_out = fd[0];
    return chan;
    }

static int _kahn_data_size[] = {
    sizeof(int),
    sizeof(float),
    sizeof(double),
    sizeof(long double)
    };
    
void put(void *value, int cnt, channel *chan, Kahn_Datatype dtype) 
{
#ifdef DEBUG
    printf("[%d] Kahn::put ; dtype = %d, size_value = %d\n", (int)getpid(), dtype, _kahn_data_size[dtype]);
#endif
    assert((0 <= dtype) && (dtype < _nb_kahn_datatype));
    write(chan->fd_in, value,cnt* _kahn_data_size[dtype]);
}

void get(void *value, int cnt,channel *chan, Kahn_Datatype dtype) 
{
#ifdef DEBUG
    printf("[%d] Kahn::get ; dtype = %d, size_value = %d\n", (int)getpid(), dtype, _kahn_data_size[dtype]);
#endif
    assert((0 <= dtype) && (dtype < _nb_kahn_datatype));
    while(read(chan->fd_out, value, cnt* _kahn_data_size[dtype]) == 0) {};
}

/* ***   RETURN BIND RUN *** */

void *_return_process(void *value)
{
    return value;
}

process *return_(void *value)
{
    process *p = malloc(sizeof(*p));
    p->f = _return_process;
    p->arg = value;
    return p;
}

void *run(process *p)
{
    return (p->f)(p->arg);
}

process *bind(process *p1, process *p2)
{
    process *p = malloc(sizeof(*p));
    p->f = p2->f;
    p->arg = run(p1);
    return p;
}


/* ***   DOCO   *** */

void doco(int nb_proc, process *processes[]) {

#ifdef DEBUG
    printf("Entering doco, nb_proc = %d\n", nb_proc);
    fflush(stdout);
#endif

    if (nb_proc > 0){
        pid_t pid = fork();
        if (pid == 0) {
            /* SON */
            run(*processes);
            exit(0);
        }
        else {
            /* FATHER */
#ifdef DEBUG
            printf("Creating [%d]\n", (int)pid);
#endif
            doco(--nb_proc, ++processes);
            wait(NULL);
        }
    }
}
