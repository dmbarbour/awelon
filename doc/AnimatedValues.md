
# Animated Values

An 'animated value' is a pure function of time. Examples include video data, gifs, sound clips, rotating icons. Animated values are significantly distinct from reactive values: the latter are also time-varying, but are based on observing an external source. 

## Pattern Suggestion

When rendering any animated value for a human, the human should have control over the rendering: pause, reverse, loop, fast or slow, or even providing a timeline function. 

Perhaps, in general, just specifying a "wall-clock time to render time" function would be good for all purposes. However, I also need something easy to edit. It may be useful to model all animated values as a function of *relative* time (i.e. 0 to N seconds). In this case, we always have some minimal control to specify the start time, and that specifier may also use a simple DSL.

## Hybrid Animated Reactive

A hybrid reactive, animated value introduces special challenges regarding 'smooth' transitions. This can be addressed a number of ways, such as stateless stable models, or exponential decay of history.




