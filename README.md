# AdventOfCode2019

My answers to the challenges for 2019 posted on https://adventofcode.com/

Written in Haskell.

### Compiler

See the compiler folder for more information, this compiles code into the IntCode machine.

### Day 1

Day 1 was almost trivial with a functional language. I decided to use type synonyms to fit with the story presented in the challenge, to make type signatures readable in this context.

I have opted to copy/paste the 'puzzle input' into my code directly, as opposed to pointlessly dealing with strings each day.

### Day 2

Day 2 began building the IntCode Program. Again type synonyms were used to fit the presented storyline. The main computational loop was achieved using recursion. 

### Day 3

To save myself handling strings, I decided to use Find/Replace to put my input into a form that fit the datatype which I decided to use. I calculated the pairs of lines which intersected, and then mapped the taxicab distance to these co-ordinates (which from the origin is equivalent to uncurrying addition). From the context of the question, I found it was safe to assume that two lines only have a single point of intersection.

Using part a, part b only required a recursive function to follow the wires. 

### Day 4

Day 4 required a function to define what was considered a valid password, and then to filter using this function upon the input range.

For part b I effectively implemented run-length-encoding and checked for the presence of a 2.

### Day 5

Day 5 built upon the IntCode Program from day 2. This extension lead me to re-structure the implementation I had previously made. Using a datatype made the program clear and readable, giving opcodes names rather than numbers.

The output of the machine is a tuple of the memory and a list of the computers outputs, with the earliest output first in the list. I decided to provide input by a single named value, as only one input was required.

Part b extended this further, the structure of the compute function made adding operations straightforward.

### Day 6

Day 6 was some implementation of graph theory. In keeping with previous days, I opted not to do string handling and edited my input before placing it inline with my code.

The definition suggested that the orbits were in fact represented by a tree structure.

The number of direct and indirect orbits for a given planet was, therefore, the length of the path from 'COM' to the planet. With the aid of a few auxiliary functions, this was achieved with a fold.

For part b, I implemented a breadth-first-search across the structure. 

### Day 7

Day 7 asked to implement a chain of the IntCode programs from Day 5. The compute function had to be extended to accept a list of inputs. From here, it was simple to chain the programs together using outputs as inputs.

Part b wanted to simulate a concurrent system which inputs and outputs multiple times. This was achieved with manipulation of the compute function and a recursive function. 

The desired effect was achieved by 'pausing' the execution of the computes, saving the memory state and pointer value so that it could be restarted on the next round. This allowed for the chains to be iterated through until all computations halted and the final output was achieved.

### Day 8

Shockingly, on day 8 I decided to take the puzzle input as a string since a string is already a list in Haskell and so I could map over it. This implementation for part A could have been simpler if I did not split the layers into rows, but I wanted the answer to correctly reflect the question.

This question was easy using standard prelude functions.

Part B was achieved using a recursive function which built the layers from the top down. The Maybe structure was completely unnecessary but I thought it made the 'Transparency' more apparent. Perhaps a datatype with this keyword would have been better.

I opted to output the image using 'X' for a black pixel and ' ' for a white. From a distance this was legible.

### Day 9

Day 9 again revisited the IntCode program from before and the implementation of Relative Base Access. How useful this is in the real world, I am not sure. Debugging gave me the opportunity to improve the legibility of my code from days 5 and 7. 

Part b was trivial to implement but exposed the inefficiencies of both my choice of the use of case statements and the computing power of my hardworking little laptop (or lack thereof).
