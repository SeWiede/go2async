# go2async
The executable of this project generates vhdl code of an asynchronous circuit derived from a go function.  

This project is in its very early stages and underwent few practical tests.

# Usage
Main go file is in cmd/

To generate an async circuit run:
go run go2async.go <generate|gen|g> <go file> [outfile]

A single .go file with only a package name and at least one function is accepted. The function has to contain at least one parameter, one statement and exactly one return statement at the end. There are some examples in example/goexamples/

The resulting .vhd and the provided third_party/vhd/entities.vhd in are needed for a quartus project. 
To test the circuit you have to instantiate a 'Scope' entity which the function name as architecture name. Set the ports as follows:

```
  [name:] entity work.Scope(<function name>)
  generic map(
	  DATA_OUT_WIDTH => <function name>_OUT_DATA_WIDTH,
	  DATA_IN_WIDTH => <function name>_IN_DATA_WIDTH
  )
  port map (
   rst => <reset pin>,

   -- Input channel
   in_req => <in pin>,
   in_ack => <out pin>,
   in_data => <parameters in a vector>,

   -- Output channel
   out_ack => <in pin>
   out_req => <out pin>,
   out_data => <return values in a vector>,
  );
```

A full example adjusted for the terasic DE0-CV board can be found in example/vhd. Don't forget to include third_party/vhd/entities.vhd and third_party/vhd/delay_element.vhd in your project.

# Note 
    - Uints and ints are downsized to 4-bit values (use u/int8, 16, etc. for bigger variables)
      - int/uint can be resized with --intSize <number>
    - the used delays are purely estimated with few practical tests

# References
Most of third_party/vhd/entities.vhd comes directly or derived from (BSD-2 licensed):
https://github.com/zuzkajelcicova/Async-Click-Library
Thank you!