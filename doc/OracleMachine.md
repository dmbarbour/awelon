

Causal commutativity enables concurrency between independent objects, and implies very little synchronization. Howerver, if indeterminism is deemed necessary for imperative programs (e.g. for performance or progress reasons) developers can leverage linear [oracle machines](http://en.wikipedia.org/wiki/Oracle_machine) to peek under the hood and decide race conditions, e.g. akin to `μO.[(a*b)→O*((b*a)+(a*b))]`. Oracle machines have nice properties for recording and replaying decision history, and thus for regression testing and maintenance. 


