# Stack with Elimination

In this task, you need to implement a modified version of the classic Treiber 
concurrent stack algorithm, which optimizes `push(..)`-s and `pop()`-s
under high contention via the *elimination* technique. Please see the
following paper for algorithm details:

* [A scalable lock-free stack algorithm](https://dl.acm.org/doi/pdf/10.1145/1007912.1007944?casa_token=Umtt5Muc0AUAAAAA:_DB1hb53y4tLqHCDamM1MLcQFE2AbhDwTMs7y5ZnoQ2Wmimgd_cHB37nbO3ZaOG8i2P6wRyLvJcIcQ)

To test your solution, please run:

* `./gradlew build` on Linux or MacOS
* `gradlew build` on Windows