# Sequencer

## Prerequisite
[ECLiPSe ≥ 6.1](https://eclipseclp.org/index.html)

## How to run
Start eclipse by running `eclipse`, compile the sequencer program by running `compile('sequencer').` and then generate a shuffled list by running `sequence([LIST_OF_TRACKS],X)`.

## Example
Input `sequence([track(1,"lana del rey",185), track(2, "kent", 180), track(3, "moto boy", 185), track(4, "moto boy", 190)],X).`
will generate four possible sequences:
```
X = [track(2, "kent", 180), track(3, "moto boy", 185), track(1, "lana del rey", 185), track(4, "moto boy", 190)]

X = [track(3, "moto boy", 185), track(2, "kent", 180), track(1, "lana del rey", 185), track(4, "moto boy", 190)]

X = [track(4, "moto boy", 190), track(1, "lana del rey", 185), track(2, "kent", 180), track(3, "moto boy", 185)]

X = [track(4, "moto boy", 190), track(1, "lana del rey", 185), track(3, "moto boy", 185), track(2, "kent", 180)]
```

## Structure
The sequencer can currently handle tracks on the following form: `track(ID, Artist, BPM)`.

## Constraints
The constraints are currently:
- Two consecutive songs can differ at most 5 in BPM.
- Two consecutive songs can't have the same artist.