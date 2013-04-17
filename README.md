bankers-erlgorithm
==================

Implementation of the Banker's Algorithm in Erlang for a concurrent software class. I do not own the [assignment writeup][1].

Abstract
--------

This program models the [Banker's Algorithm][2] in a simplistic setting where there is a *Banker* with a finite set of resources that it must loan to *Client*s who promise to repay their loans at some point; the *Banker* must distribute the resources in a way so as to avoid deadlock.

Entities
--------

### Banker

#### State

The *Banker* has an initial _capital_ which is the total finite amount of resources which may be lent to *Client*s. It also must keep track of its _cash_on_hand_ which is the amount of resources not yet lent to a *Client*.

#### Behavior

The *Banker* lends resources to *Clients* which register with it on the condition that their requests will not result in an unsafe state. An unsafe state is a state in which there are not enough free resources to fully satisfy the _claim_ of any *Client*. (Read more on the Banker's Algorithm to get an understanding of this kind of deadlock). Before lending resources, the *Banker* will check to see if the loan request, if fulfilled, will result in an unsafe state. If so, the request is rejected, and the *Client* must wait until enough resources are free in order to satisfy its request. The *Banker* will notify the *Client* when this occurs. If the request is safe, the *Banker* loans the requested resources and things continue as normal.

#### Functions

##### start(Capital)

Spawn the *Banker* process (registered as `banker`) with the specified _Capital_. The *Banker* will act as a supervisory process to catch the `exit` of each *Client*.

##### status()

Returns a tuple `{Capital, CashOnHand, NClients}` to the caller. `NClients` is the number of currently attached client processes.

##### attach(Limit)

A *Client* attaches to the *Banker* with the given `Limit` on the resources it can request. For simplicity, we'll assume that the limit never exceeds the capital. (The *Banker* `link`s to the *Client*.)

##### request(NUnits)

An attached *Client* requests `NUnits` more resource from the *Banker*. To simplify things, we'll assume that the request never exceeds the remaining claim. If the request would result in a safe state it is granted immediately; otherwise the request is deferred until it can be granted without creating an unsafe state.

##### release(NUnits)

An attached *Client* releases `NUnits` of resource (we'll assume does not exceed the *Client*'s current loan). The *Banker* adjusts the *Client*'s information, updates _cash_on_hand_, and determines whether any deferred requests can be granted.

### Client

#### State

A *Client* has an initial _limit_ to the number of resources it will request to be lent. The *Client* keeps track of the current number of resources which are lent to it, i.e. its _loan_, and although it is a function of its _limit_ and its _loan_ it also keeps track of the number of resources which it may still request, i.e. its _claim_.

#### Behavior

At the beginning of its lifetime, the *Client* will attach to the *Banker* and declare its _limit_. The *Client* makes loan requests to the *Banker* during its lifetime, and it must promise to return the resources which are lent to it - although this simulation will prepare for cases in which the *Client* does not do so.

#### Functions

##### start(Limit, N)

A new *Client* process is spawned with a `Limit` on the number of resources it can request and a number `N` of requests and releases it will make to the *Banker* process. We assume that the limit does not exceed the *Banker*'s capital.
The *Client* attaches to the *Banker* and engages in `N` interactions, after which it exits. Each interaction is a randomly selected request or release (with the constraint that requests require a non-zero claim and releases require a non-zero loan).  For requests, the number of units randomly varies from 1 to claim; for releases the number randomly varies from 1 to loan.


[1]: http://www.se.rit.edu/~se441/spring_2013/Assignments/Banker.html "Robust Banker's Algorithm"
[2]: http://en.wikipedia.org/wiki/Banker%27s_algorithm "Banker's Algorithm - Wikipedia"
