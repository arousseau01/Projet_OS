# Projet d'implémentation de réseaux de processus de Kahn (KPN)

## Repertoires:
### Implémentation de KPN
- UnixVersion: implémentation basée sur des processus UNIX communiquant via des pipes (OCaml)
- NetworkVersion: implémentation basée sur des processus UNIX communiquant via des stream sockets (OCaml)
- SequentialVersion: *** description ***
- MpiVersion: implémentation utilisant l'API de MPI (OCaml)
### Implémentation de MPI
- MPI_Kahn : implémentation de MPI utilisant l'implémentation UnixVersion (OCaml)
- MPI_Kahn_C : implémentation de MPI utilisant une implémentation de KPN en C (C)
