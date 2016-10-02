# 2048
Course Comparative Programming Languages: Distributed 2048

**Academic Year**: 2014-2015 (1st semester - 2nd Master of Science in Engineering: Computer Science)

**Score:** Maximum Score

## About
A fault-resistant, concurrent version of the popular game [2048](https://gabrielecirulli.github.io/2048/) (*written in Erlang*).

## Use
<p align="left"><img src="https://github.com/matt77hias/2048/blob/master/res/2048.png"></p>

```erlang
% 2048 with a blaster killing random tiles (the killing frequency can be manually modified)
main:play()
% 2048 without a blaster killing random tiles
main:playnoblaster()
```

## Design
This is a brief description of the state (via parameter passing) of the manager and the tile processes and the new messages exchanged between them. For a more detailed description, I refer to the actual code and the very extensive logging (possible to enable/disable different items). 

### The state of the manager process
*	`State`: This variable refers to an atom representing the state the manager process is currently in. The manager process can be in one of five possible states:
  *	`passive`: the manager process is currently not handling a movement;
  *	`up`/`dn`/`lx`/`rx`: the manager process is currently handling an up/down/left/right movement.
When the manager process is currently handling a movement, it will not handle a new movement.
*	`Previous`: This variable refers to a list of 16 integer values. This list represents the snapshot of the board at the time of the last **completely** handled movement. This snapshot of the board will be compared with the snapshot of the board at the time of the next completely handled movement in order to determine if some change happened. If no change is observed, no new random value (2 or 4) will be added to the board.
*	`Snapshot`: This variable refers to a list of 16 integer values. This list represents the snapshot of the board at the time of the last **partially** handled movement. A movement is handled completely if the four `lineshots` (columns or rows depending on the kind of movement) from the responsible tile processes are received and handled at the manager process (Task 3). A movement is handled partially each time one of the four `lineshots` (except the last one) is received and handled at the manager process. Note that the order of receiving and handling the four `lineshots` is not known in advance. A `lineshot` refers to a list of four tuple values containing a tile id and a tile value.
*	`Finished`: This variable refers to a map. The keys of the map refer to the tile ids of the tile processes that **still** (Task 3) need to send a `lineshot` to the manager process (at most four). The values of the map refer to the four tile ids of the corresponding tile processes that are involved in that `lineshot` construction. 
*	`TileMap`: This variable refers to a map. The keys of the map refer to the process identifiers of the tile processes. The values of the map refer to the corresponding tile ids. This mapping is needed, because the manager process will only be informed of the process identifier of a killed tile process. 
*	`Rollback`: This variable refers to a list of 16 atom values. This list represents the current snapshot of the state of the tile processes. A tile process can be in one of three states (from the point of view of the manager process):
  *	`r` (ready): the tile process is ready. Only `lineshots` from ready tile processes will be accepted at the manager process. The manager process rejects `lineshots` from non-ready tile processes.
  *	`rc` (rollback closed): the tile process is closed which means that the tile process will reject all future messages (except rollback related messages and die messages) and the manager process is waiting for an acknowledgement from the tile process. 
  *	`ra` (rollback acknowledged): an acknowledgement is received and handled at the manager process. If all four tile processes of a line (column or row depending on the kind of movement which is stored in the state) are in `ra`, the full line can be resynchronized from a known point (Snapshot) in order to handle the movement correctly.
  
### The state of a tile process
*	`State`: This variable refers to the state of the tile process (from the point of view of the tile process itself). A tile process can be in one of six possible states:
  *	passive: the tile process is currently not handling a movement.
  *	`up`/`dn`/`lx`/`rx`: the tile process is currently handling an up/down/left/right movement;
  *	closed: the tile process will reject all future messages (except rollback related messages and die messages)
Note that at most one tile process of a line can handle a movement at the same time.
*	`Id`: This variable refers to the id of a tile process.
*	`Value`: This variable refers to the value of a tile process.
*	`Merged`: This variable refers to the merged status of a tile process.
*	`Buffer`: This variable refers to a map. The keys of the map refer to the tile ids of the future tile processes for which the value and merged status (= value of the map) is already received.
*	`BufferSize`: This variable refers to the number of future tile processes for which the value and merged status must be received.
*	`Step`: This variable refers to an integer that can be added to the tile process’ own tile id in order to iterate the tile ids of its future tile processes.
*	`Check`: This variable refers to a function that checks if the generated tile ids with the Step variable are valid future tile ids.

### Newly defined messages that the manager process can sent to a tile process
* `{rc, NValue}`
  * The tile process goes to `closed`, sends a `{ra, Id}` message to the manager process, resets most of its state to default values and its value to `NValue`. So the tile process enters a known state and will not leave this state. (Task 2)
* `ro`
  * The tile process goed to `passive` (it is opened again). (Task 2)
* `new_turn`
  * The tile process resets its state to default values. A tile process must clean up its data to handle future movements correctly. The manager sends this message to all tile processes after completely handling a movement.

### Newly defined messages that can be sent to the manager process
* `{finished, Id, Lineshot}`
  * The tile process with identifier `Id` notifies the manager process that its part of the movement handling is finished, by sending the `lineshot` of the line it is responsible for. (Task 3)
* `{collectedData, R}`
  * The `R` indicates if a random value must be generated and added (if possible) or not. This is an internal message (manager process to manager process). This message is also sent in the `manage()` function at start-up.
* `{'EXIT', Pid, _}`
  * All tile processes are linked to the manager process. If a tile process exits this message is received at the manager. (Task 2)
* `{ra, Id}`
  * An acknowledgement to an `{rc, NValue}`. (Task 2)
  
All tile processes keep trying to send a message to another tile process till the message is successfully deposited in the mailbox of the other tile process. The manager process never tries more than once to send a message to a tile process because the manager process must also handle failed tile processes. Note that depositing a message in some process’ mailbox is not the same as that process receiving the message. If a process exits, all the messages in its mailbox are lost (and so unhandled).

### When a tile process dies
If a tile process dies and the manager process is `passive`, than that process must only be replaced. If a tile process dies and the manager process is handling a movement and the tile process doesn’t belong to a line under construction, than that process must only be replaced. If a tile process dies and the manager process is handling a movement  and the tile process belongs to a line under construction, than each of the other three processes that are `r` (ready) of that line must be sent a `{rc, NValue}` message. The ones that are `rc`, will sent an `ra` or die in which case the manager process will sent a new `{rc, NValue}` message while handling their dead. The ones that are `ra`, are still waiting and no new message must be sent to them. The process that died, is replaced and will be sent a `{rc, NValue}` message independent of its state. Note that every replacement process is started with the corresponding value from the `Snapshot`. If the process is involved in a line construction this value could be overridden in the meantime, there for a `{rc, NValue}` message contains this value again. If the process is not involved in a line construction, it will not receive a `{rc, NValue}` message but still needs to know its value, therefor this value is always passed at start-up.
