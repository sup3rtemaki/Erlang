-module(ea).
-export([start/1]).
% -compile(export_all).

%% A Quick Explanation of how it works...
%% 1. Randomly generated population.
%% 2. Create tournament of that population.
%% 3. Run the fitness function on the population of that tournament.
%% 4. Apply the crossover to our two best individuals in the tournament.
%% (Think of it as breeding the fittest.  This will yield two children)
%% 5. Perform a mutation (5% chance of mutation) if we hit our mark,
%% or just return the child as-is.
%% 6. Check our stopping criteria to see if we are done
%% (we check the two children we just created).
%% 7. Add those children to a new population, which (once it reaches the same
%% size as our original population) will replace our original (or parent) population.
%% We define what a "perfect" individual looks like.
%% Our "perfect" genome is the alphabet in this case.
-define(CORRECT, "ABCDEFGHIJKLMNOPQRSTUVWXYZ").
-define(POPSIZE, 20). %% Size of our population for each tournament

start(TournySize) -> % Fire this guy off to start the tournaments for each population.
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
	run([],TournySize).

run(Pop,TSize) -> %first run of tournaments (no new generations)
	case run_tournament(Pop) of
		[ChildA,ChildB] ->
			ChildPop = [ChildA,ChildB],
			run([],TSize,ChildPop);
		ok ->
			ok
	end.

run(Pop,TSize,ChildPop) -> %Accumulate children until we have enough to replace our parents
	case run_tournament(Pop) of
		[ChildA,ChildB] ->
			NewChildPop = [ChildA,ChildB],
			NewPop = lists:append(ChildPop,NewChildPop),
			case length(NewPop) =:= ?POPSIZE of
				true ->
					run(NewPop,TSize);
				false ->
					run([],TSize,NewPop)
			end;
		ok ->
			ok
	end.

%% Population Creation
create_populations(PopSize) ->
	gen_pop([],PopSize).

gen_pop(Pops,0) ->
	Pops;
gen_pop(Pops,N) ->
	NewPops = lists:append(Pops,[fill(length(?CORRECT))]),
	gen_pop(NewPops,N-1).

fill(N) -> % Fill in our "individual" with their own genome from our "perfect" one.
	fill([],N).

fill(P,0) ->
	P;
fill(P,N) ->
	Pos = random:uniform(length(?CORRECT)),
	E = lists:nth(Pos,?CORRECT),
	NP = P ++ [E],
	fill(NP,N-1).

%% Fitness
%% Here we keep score of how "fit" our individual is...
%% How well do they measure up to our "perfect" genome?
population_fitness(Pops) ->
	Results = get_fitness(Pops,[]),
	lists:reverse(lists:keysort(2,Results)).

get_fitness([], Result) ->
	Result;
get_fitness([PH|PT], Result) when is_list(Result) ->
	{Person,Score} = fit(PH),
	NewResult = lists:append(Result,[{Person,Score}]),
	get_fitness(PT,NewResult).

fit(L) ->
	fit(?CORRECT,L,0,L).

fit([],[],Score,Orig) -> 
	{Orig,Score};
fit([AH|AT],[BH|BT],Score,Orig) ->
	NewScore = case AH =:= BH of
		true ->
			Score + 1;
		false ->
			Score
	end,
	fit(AT,BT,NewScore,Orig).

%% Tournament Creation
%% If we are not given a population, we will generate one,
%% otherwise, we will use the one passed in (the children we just generated)
run_tournament(Population) ->
	Individuals = case Population of
		[] ->
			create_populations(?POPSIZE);
		_ ->
			Population
	end,
	Results = population_fitness(Individuals), % How fit is our population
	{Winner,_WinningScore} = hd(Results), % Get the alpha of our population
	{RunnerUp,_RunnerUpScore} = lists:nth(2,Results), % Get the second alpha
	[{a,ChildA},{b,ChildB}] = crossover(Winner,RunnerUp), % Breed them together
	[MutatedChildA, MutatedChildB] = [mutate(ChildA),mutate(ChildB)],
	case criteria_check(MutatedChildA, MutatedChildB) of
		true ->
			io:format("DONE~n"),
			io:format("Children Are:~n"),
			io:format("ChildA: ~p~nChildB: ~p~n",[MutatedChildA,MutatedChildB]),
			ok;
		false ->
			[MutatedChildA, MutatedChildB]
	end.

%% Crossover and Mutation functions
%% This is a single point crossover.  You can read more on crossover's here:
%% http://www.obitko.com/tutorials/genetic-algorithms/crossover-mutation.php
crossover(ParentA,ParentB) ->
	COPoint = random:uniform(length(?CORRECT)),
	ChildA = lists:sublist(ParentA,COPoint) ++ lists:sublist(ParentB,COPoint+1,length(ParentB)),
	ChildB = lists:sublist(ParentB,COPoint) ++ lists:sublist(ParentA,COPoint+1,length(ParentA)),
	[{a, ChildA},{b, ChildB}].

% All we mutate with here is a single random element in our sequence.
mutate(Child) ->
	case random:uniform(100) =< 5 of
		true ->
			MP = random:uniform(length(Child)), %Mutation Point (MP)
			lists:sublist(Child,MP - 1) ++ [lists:nth(random:uniform(length(?CORRECT)),?CORRECT)] ++ lists:sublist(Child,MP+1,length(Child));
		false ->
			Child
	end.

%% Criteria Check to halt on.
%% Here we check to see if we've hit what we think of as "complete".
%% You'll notice on the AlphaLimit that it is set to 15.
%% We finish if either Child A or Child B are good enough for us to stop.
%% So if they are 15 or better, we'll stop and see what our near-perfect genome is.
criteria_check(ChildA,ChildB) -> %%This could be a lot better!!!
	{CA,ChildAFitness} = fit(ChildA),
	{CB,ChildBFitness} = fit(ChildB),
	AlphaLimit = 15,
	case ChildAFitness >= AlphaLimit of
		true ->
			io:format("A: ~p scored: ~p.~n",[CA,ChildAFitness]),
			true;
	 	false->
			case ChildBFitness >= AlphaLimit of
				true ->
					io:format("B: ~p scored: ~p.~n",[CB,ChildBFitness]),
					true;
				false ->
					false
			end
	end.