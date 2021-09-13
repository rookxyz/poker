## Solution Details
The program implements an algorithm for sorting poker hands according to their strength for the follwing game types:
* Texas Holdem
* Omaha Holdem
* Five Card Draw
The program is written in Scala programming language.

### How to run
1. Make sure that scripts _run.sh_ and _prepare.sh_ are executable. Use _chmod +x_ if necessary.
2. Run _prepare.sh_ script to install the required prequisites.
3. To run the program execute the shell script _run.sh_. The program reads from lines from standard input. 
Use redirection _<_ to read input from file.

Example input:
```
texas-holdem 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d
texas-holdem 2h3h4h5d8d KdKs 9hJh
omaha-holdem 3d3s4d6hJc Js2dKd8c KsAsTcTs Jh2h3c9c Qc8dAd6c 7dQsAc5d
five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c
```
 
Example output:
```
Ac4d=Ad4s 5d6d As9s KhKd
KdKs 9hJh
Qc8dAd6c KsAsTcTs Js2dKd8c 7dQsAc5d Jh2h3c9c
4c8h2h6c9c Ah9d6s2cKh Kd9sAs3cQs 7h4s4h8c9h Tc5h6dAc5c
```

### Implementation Notes

Changes to:
* plugins.sbt - added _sbt-assembly_ plugin  
* build.sbt - defined program main class, set _*.jar_ location
* prepare.sh - added commands to create fat jar
* run.sh - added _eval_ command to run the application, otherwise the file contents were not redirected to
the standard input. changed from _sbt run_ to _java -jar_ to run the program.
