future plans for idp
=====

Currently the isodat data processor implements its own version of a very basic isodat file reader. The next version will incorporate the isoread package directly and take advantage of all the functionality introduced in that package. 

With this switch, the hope is also to implement a "process code display" feature that allows the user to copy the code for all data loading, peak assignment and other data operations in the GUI directly into an RMD file to make the data reduction 100% reproducible. 

Additionally, idp will likely be rolled into a much more comprehensive implemtation of [CSIDE](http://www.github.com/sebkopf/cside). 