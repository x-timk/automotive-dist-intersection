
# Elenco API Esposte
Vengono quì specificate le funzioni che sono chiamabili esternamente per ogni modulo
## Modulo ENVIRONMENT
Le funzioni possono essere chiamate da un qualsiasi modulo esterno in maniera standard (dim_env:nomefun(args)
Elenco API:
 - spawn_car(CarName, CarDesc, StartPos, EndPos, Speed)
Usata per ordinare all'env di generare una nuova auto
   - CarName: Nome Macchina. Usare un atom() in questo caso. L'atom viene registrato dalla vm erlang; Si utilizzerà il CarName invece del pid per inviare messaggi all'auto
   - CarDesc: Una stringa di descrizione per l'auto
   - StartPos: posizione di partenza dell'auto. I nomi dei nodi del grafo sono atomi.
   - EndPos: posizione alla quale l'auto desidera arrivare. Da StartPos ed EndPos verrà automaticamente calcolato il percorso più breve (si utilizza dijkstra)
   - Speed: Velocità di movimento dell'auto. Es: se speed vale 2000 allora l'auto tenterà di muoversi in avanti ogni 2 secondi

 - broadcast_disc(FromCar)
 Simula il broadcast dell'auto FromCar. Vengono calcolati i vicini di FromCar e quindi viene forwardato il messaggio disc.

 - req_prox_sensor_data(FromCar, Position) ->
Chiamata richiesta dati dal sensore di prossimità dell'auto
 - notify_move(CarName, CurrentPos, NextPos) ->
Chiamata utilizzata da veicolo per notificare all'env il cambio della posizione


## Modulo VEHICLE
Il veicolo è una macchina a stati finiti con un il suo set di variabili globali che vengono aggiornate mano a mano che vengono ricevuti eventi.
 - env: Identificativo environment
 - name: Identificativo da usare per contattare auto
 - desc: Descrizione
 - route: Percorso nel grafo
 - neighbourPids: Macchine a portata wifi
 - speed: velocità
 - isleader: booleano che identifica leader
 - leader: ID macchina leader
 - electionPids: Macchine che partecipano a elezione/agreement. Questo potrebbe essere potenzialmente diverso da neighbourPids.

Ogni veicolo espone delle API per poterci interagire.
In ogni stato la chiamata ad una API potrebbe avere effetti diversi.
Di seguito si riporta l'elenco completo delle API e, per ogni stato del veicolo, le azioni intraprese alla RICEZIONE di queste chiamate.

### send_disc(DestCar, FromCar): 
Viene utilizzata dall'env per forwardare il disc proveniente da FromCar verso DestCar
 - Inqueue: Ignoro il messaggio
 - Discover: mi salvo DestCar tra le auto a me conosciute (neighbourPids) e invio come risposta a DestCar un send_hello
 - Election/Slave/Mfetch: mi salvo DestCar tra le auto a me conosciute (neighbourPids) e invio un hello e un wait come risposta


### send_hello(Car, FromCar)
Utilizzato per inviare messaggio hello a Car da FromCar
 - Inqueue: Ignoro il messaggio
 - Discover/Election/Slave/Mfetch: mi salvo FromCar tra le auto a me conosciute (neighbourPids)

### send_wait(Car, FromCar, Reason)
Invio un messaggio di wait a Car da FromCar con una Reason


### send_leader_exists(DestCar)
Funzione Sincrona.
Interrogo DestCar chiedendogli se si trova in elezione/agreement.
Funzione utilizzata dalle auto in discover per capire se in un incrocio ci sono veicoli che sono già in fase di elezione/agreement. Se è così allora non posso andare in stato election, ma devo rimanere in discover. 

 - Discover: rispondo false (io non mi trovo nè in elezione nè in agreement)
 - Election/Slave/Mfetch: rispondo true

### send_bully_elect(Car)
Funzione sincrona che manda messaggio elezione a Car (bully algo). Si aspetta una risposta bullyans / bullynoans (se il veicolo a cui ho mandato la richiesta è morto) / wait (non dovrebbe mai capitare)
 - Discover: mi salvo i neighbourPids attuali in electionPids. Rispondo con bullyans e passo in stato Election
 - Election: rispondo con bullyans e rimango in Election
 - Slave/Mfetch: non dovrei mai ricevere questo evento in questi stati

### send_bully_coord(Car)
Funzione che manda messaggio coord (bully algo) a Car.
 - Discover: mi salvo i neighbourPids attuali in electionPids. Passo in stato Slave
 - Election: Passo in stato Slave
 - Slave/Mfetch: non dovrei mai ricevere questo evento in questi stati


