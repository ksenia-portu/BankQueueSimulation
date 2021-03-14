About the project
=================

This project is solving a small discrete-event simulation task. It is a
sequential discrete-event model, with one channel, with infinite queue
following FIFO algorithm and inter-arrival time is exponentially
distributed and the service time is betta-distributed.

The task
========

Simulation of a bank where only one customer can be served at a time and
with constraints that at any particular instance of time only one
customer arrives.

If an employee of the bank is not available i.e serving another customer
then all other customers have to wait forming a queue following FIFO
algorithm.

This program analyses the average wait time and max wait times a
customer has to wait for their turn, along with the average and max
queue lengths of customers in front of the teller.

The program outputs to stdout the answers to the following questions:

-   Given only yellow customers, what are the average and maximum
    customer waiting times?
-   Given only red customers, what are the average and maximum queue
    lengths in-front of the teller?
-   Which type of customer(yellow, red or blue) gives the gives the
    closest value between the average and maximum customer waiting
    times?

**Background** The inter-arrival time between customers described by
this function: 
F(t) = 1 - e ^ ( -t / a) a = 100 
this function is
cumulative distribution function (CDF). From this we can get probability
density function (PDF), which is the derivative of CDF: 
f (t) = (1/a) * e ^ ( -t / a)

Processing time can be modelled with beta distribution: 
pdf = p * x ^ ( alpha - 1 ) * ( 1 - x ) ^ ( beta - 1)

General approach for discrete-event simulation
==============================================

In a DES model

-   Changes in the system state occur only at discrete points in time.
-   Events move the simulation model from state to state.

1.  A sequential DES typically use three data structures

-   the system clock variable
-   the state variables
-   an event list of pending events

1.  Each event corresponds to a future change in system state
2.  Each event is time-stamped
3.  Repeatedly remove and process smallest time-stamped event

-   change system state appropriately
-   schedule new events

Selecting the smallest time-stamped event is crucial. Otherwise, we
simulate a system in which future events can affect the past.

Solution
========

This sequential DES task can be modelled in Haskell using lenses by
using the general approach for discrete-event simulation, like it was
done in this solution:
<https://github.com/DeepakKapiswe/QueueingSystemSimulation.git>

In my solution I will use:

-   \`forkIO\` for running processes in parallel
-   \`STM \` for properly handling shared state (atomical transactions)
-   lenses for handling nested data structures
-   \`list\` as a container for list of customers waiting and list of
    generated customers as well (for performance reason it is better to
    use another container type, but this model doesn\'t care much about
    performance)

Model
=====

1. parallel process StopSimulation
----------------------------------

Check if the generated list of customers and the list of customers
waiting are empty 
  then 
    kill process Customer Arrived
    kill process Customer Serviced 
    return summary of the current simulation state

2. parallel process Customer Arrived
------------------------------------

Get customer from the generated list.
If arrival time of the customer is <= time now of simulation state 
  then 
    transaction 
    { add customer to the waiting list 
      delete customer from the generated list 
      }

3. parallel process Customer Serviced
-------------------------------------

1.  if the waiting list is empty 
      if generated list is empty 
      then return current simulation state 
      else transaction 
      { get one customer from generated list 
        time now = customer arrived 
        add customer to waiting list 
        delete customer from generated list 
        }

2.  if waiting list is not empty 
        transaction 
        { get customer from waiting list 
          time now = + processing time
          waitingTime = timeNow - (arrivalTime customer) 
          lengthOfQueue = length of waiting list 
          delete customer from waiting list 
          numberServedCustomers = + 1 
          }
