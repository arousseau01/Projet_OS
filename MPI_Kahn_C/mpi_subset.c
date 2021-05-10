#include <stdio.h>        
#include <stdlib.h>        
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <sys/sysinfo.h>
#include <sys/types.h>
#include <unistd.h>

#include "mpi_subset.h"
#include "kahn.h"

/*      Implémentation de l'interface mpi_subset.h reposant sur l'implémentation de KPN

        Objets encapsulés:
            - _mpi_init         // indique si environnement MPI initié
            - _mpi_size         // taille MPI_Comm_World
            - _mpi_rank         // rang du processus au sein de MPI_Comm_World
            - _mpi_channels_global  // tableau 2D de pointeurs vers des canaux de communication: 1 canal pour chaque paire de processus
            - _mpi_barrier_channel  // canal permettant la synchronisation
            - _mpi_barrier_counter  // entier communiqué entre processus via _mpi_barrier_channel
            - _mpi_process_argument // structure de données passées aux processus MPI
            - _kahn_to_mpi_datatype // conversion de MPI_Datatype à KAHN_DATATYPE

        Fonctions internes:
            - _mpi_getargs()    // parsing des arguments (recherche d'option -np, -n) et contrôle du nombre de slots disponibles (philosophie MPI: pas plus de processus que de slots)
            - _mpi_allocate_channel()    // initie les canaux (appel à new_channel de KPN)
            - _mpi_process()    // fonction élementaire processus MPI: change les variables de rang et de canaux attribués puis return (continue éxécution programme à la suite de l'appel MPI_Init)
            - _mpi_split()      // lancement des processus MPI (appel à doco de KPN)
        
        MPI_Init()  // appel _mpi_args ; _mpi_allocate_channel ; _mpi_split
        MPI_Send()  // appel put de KPN
        MPI_Receive // appel get de KPN
*/

//#define DEBUG

static int _mpi_init = 0;
static int _mpi_size = 1;
static int _mpi_rank = 0;

static int _mpi_getargs(int argc, char **argv)
/* 
    Parsing des arguments, initie _mpi_size après vérification de la compatibilité avce le nombre de slots disponibles
*/
{
    while(argv++,--argc) {
        if (strcmp(*argv, "-n") == 0 || strcmp(*argv, "-np") == 0) {
            if (argc > 1) {
                _mpi_size = (int)strtol((const char *)*(++argv), NULL, 10);
                argc--;
                if (_mpi_size > get_nprocs()) {
                    printf("There are not enough slots available in the system to satisfy the %d slots that were requested (%d slots available only)\n", 
                    _mpi_size, get_nprocs());
                    exit(1);
                } 
            }
        }
    }
#ifdef DEBUG
    printf("_mpi_args SUCESS, _mpi_size = %d\n", _mpi_size);
#endif
return MPI_SUCCESS;
}

/* table des canaux de communication entre processus */
static channel ***_mpi_channels_global; 

/* canal pour synchronisation */
static channel *_mpi_barrier_channel;
static int _mpi_barrier_counter = 0;

static int _mpi_allocate_channels()
/* Ouverture des canaux */
{   
    int size = _mpi_size;

    _mpi_channels_global = malloc(size* sizeof(**_mpi_channels_global));
    for (int i=0; i<size; i++) {
        _mpi_channels_global[i] = malloc(size* sizeof(*_mpi_channels_global));
    }

    for (int i=0; i<size; i++) {
        _mpi_channels_global[i][i] = new_channel();
        for (int j=i+1; j<size; j++) {
            channel *chan = new_channel();
            _mpi_channels_global[i][j] = chan;
            _mpi_channels_global[j][i] = chan;
        }
    }

    _mpi_barrier_channel = malloc(sizeof(*_mpi_barrier_channel));
    _mpi_barrier_channel = new_channel();
    put(&_mpi_barrier_counter, 1, _mpi_barrier_channel, KAHN_INT);

#ifdef DEBUG
    printf("_mpi_allocate_channels\n");
#endif
return MPI_SUCCESS;
}

/* Structure de passage du rang et des canaux à un processus */
typedef struct _mpi_process_argument {
    channel **channels;
    int rank; 
} _mpi_process_argument;

static channel **_mpi_channels;

void _mpi_process(_mpi_process_argument *arg) 
/* 
    Processus élémentaire passé à doco: initie les variables rank et channels[] et termine (suivi par execution du reste du programme) 
*/
{   
#ifdef DEBUG
    printf("_mpi_process initiation with rank = %d\n", arg->rank);
#endif
    _mpi_rank = arg->rank;
    _mpi_channels = arg->channels;
    return;
}

static int _mpi_split()
/* Appel de doco */
{
    _mpi_channels = _mpi_channels_global[0];

    if (_mpi_size == 1) { return MPI_SUCCESS; } 

#ifdef DEBUG
    printf("_mpi_split : have to fork %d process(es)\n", _mpi_size-1);
#endif

    process *processes = malloc((_mpi_size-1)* sizeof(process));
    _mpi_process_argument **args = malloc((_mpi_size-1)* sizeof(*args));

    for (int i=0; i<_mpi_size-1; i++) {
        processes[i] = _mpi_process;
        _mpi_process_argument *arg = malloc(sizeof(_mpi_process_argument));
        arg->rank = i+1;
        arg->channels = _mpi_channels_global[i+1];
        args[i] = arg;
    }
    
    doco(_mpi_size-1, processes,(void **)args);

    return MPI_SUCCESS;
}


int MPI_Init(int argc, char **argv)
/* 
    Initiation de l'environnement MPI 
*/
{
    assert(_mpi_init == 0);

    if (_mpi_getargs(argc, argv) != MPI_SUCCESS ||
        _mpi_allocate_channels() != MPI_SUCCESS ||
        _mpi_split() != MPI_SUCCESS) { return !MPI_SUCCESS; }

    _mpi_init = 1;
    return MPI_SUCCESS;
}

int MPI_Finalize(void)
{   
    assert(_mpi_init);
    _mpi_init = 0;
    return MPI_SUCCESS;
}

int MPI_Comm_size(MPI_Comm comm, int *psize)
/* 
    Renvoi la taille du communicateur dans l'adresse psize
*/
{   
    assert(_mpi_init);
    assert(comm == MPI_COMM_WORLD);
    assert(psize != NULL);

    *psize = _mpi_size;

    return MPI_SUCCESS;
}
int MPI_Comm_rank(MPI_Comm comm, int *prank)
/*
    Renvoi le rang du processus appelant 
*/
{   
    assert(_mpi_init);
    assert(comm == MPI_COMM_WORLD);
    assert(prank != NULL);

    *prank = _mpi_rank;

    return MPI_SUCCESS;
}

/* Point-to-point communication */

Kahn_Datatype _khan2mpi_dtype(MPI_Datatype dtype)
/*
    Conversion de MPI_Datatype à Kahn_Datatype
*/
{
    switch (dtype)
    {
    case MPI_BYTE:
        return KAHN_BYTE;
    case MPI_INT:
        return KAHN_INT;
    case MPI_FLOAT:
        return KAHN_FLOAT;
    case MPI_DOUBLE:
        return KAHN_DOUBLE;
    case MPI_LONG_DOUBLE:
        return KAHN_LONG_DOUBLE;
    
    default:
        return KAHN_INT;
    }
}

int MPI_Send(void *buf, int cnt, MPI_Datatype dtype, int dest, int tag, MPI_Comm comm)
/*
    Envoi un buffer au processus de rang dest 
    tag non implémenté
*/
{  
#ifdef DEBUG
    printf("Mpi_Send : cnt = %d, dest = %d, tag = %d\n", cnt, dest, tag);
#endif
    assert(_mpi_init);
    assert(buf != NULL);
    assert(dest >= 0 && dest < _mpi_size && dest != _mpi_rank);
    assert(tag >= 0);
    assert(comm == MPI_COMM_WORLD);

    channel *chan = _mpi_channels[dest];

    put(buf, cnt, chan, _khan2mpi_dtype(dtype));
    
    return MPI_SUCCESS;
}

int MPI_Receive(void *buf, int cnt, MPI_Datatype dtype, int src, int tag,
 MPI_Comm comm)
 /*
    Reçoit un buffer du processus de rang src
    tag non implémenté
*/
 {

#ifdef DEBUG
    printf("Mpi_Receive : cnt = %d, src = %d, tag = %d\n", cnt, src, tag);
#endif

    assert(_mpi_init);
    assert(buf != NULL);
    assert(src >= 0 && src < _mpi_size && src != _mpi_rank);
    assert(tag >= 0);
    assert(comm == MPI_COMM_WORLD);

    channel *chan = _mpi_channels[src];

    get(buf, cnt, chan, _khan2mpi_dtype(dtype));
    

    return MPI_SUCCESS;
 }

 int MPI_Barrier(void)
 /* 
    Synchronise les processus: bloquant tant que tous les processus n'y sont par arrivé
*/
{  
     get(&_mpi_barrier_counter, 1, _mpi_barrier_channel, KAHN_INT);
     _mpi_barrier_counter ++;
     put(&_mpi_barrier_counter, 1, _mpi_barrier_channel, KAHN_INT);

#ifdef DEBUG
    printf("[%d][%d] MPI_Barrier : _mpi_barrier_counter = %d\n", (int)getpid(), _mpi_rank, _mpi_barrier_counter);
#endif

     while((_mpi_barrier_counter % _mpi_size) != 0) {
         get(&_mpi_barrier_counter, 1, _mpi_barrier_channel, KAHN_INT);
         put(&_mpi_barrier_counter, 1, _mpi_barrier_channel, KAHN_INT);
     }

     return MPI_SUCCESS;
 }