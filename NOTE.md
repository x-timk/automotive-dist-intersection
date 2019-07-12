# Assunzioni

## Environment

Nell'environment è presente un record erlang con due grafi identici, uno orientato l'altro non orientato.
E' stato scelto di salvare la posizione dei veicoli solo nel grafo non orientato.


### Casi di crash:
 * get_shortest_path(ShortestPaths, V2): crash nel caso in cui V2 non esista;
 * delete_car_from_vertex: la funzione va avanti anche se la macchina non è presente nel nodo. (Mi appoggio a lists:delete).