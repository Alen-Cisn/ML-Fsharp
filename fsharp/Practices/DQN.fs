namespace Practices


(*
    State space: S
    Action space: A
    Agent: RL algorithm that learns to behave optimally.
    Policy function: a function π; s → Pr(A|s), where s ∈ S. The strategy of the agent, a mapping from states to the probabilistically best actions.
        For example, epsilon-greedy policy: π(a|s) = argmax_a Q_π(s,a) with probability 1-ε, and a random action with probability ε.
    Optimal policy: a function π* = argmax E(R|π). The policy that yields the maximum expected utility.
        RL tries to find the optimal policy training. There are two ways: direct (teach what to do) and indirect (let the agent explore).
    Value function: V_π(s) = E(R|s, π). Maps a state, s, to the expected rewards that can be obtained by following π from s.
    Action-value function: Q_π(s,a) = E(R|s,a,π). Maps a state-action pair, (s,a), to the expected rewards that can be obtained by following π from s and taking action a.
        This is often called the Q-function, and their result Q_π(s,a) is called the Q-value.
*)
(*
    Q-learning: A particular RL algorithm that learns the optimal action values.
        Update rule: Q(s_t,a_t) = Q(s_t,a_t) + α [r_{t+1} + γ max_a Q(s_{t+1},a) - Q(s_t,a_t)]
                         ^           ^         ^    ^       ^           ^            ^
                         |           |         |    |       |           |            |
                       Updated    Previous  Step  Obversed  Discount    Max          |
                       Q-value    Q-value   Size  Reward    Factor      Q-value      |
                                    └────────────────────────────────────────────────┘
        The Q value at time t is updated to be the current predicted Q value plus the amount of value we expect in the future, given that we play optimally from our current state.
        pseudocode:
            let updateQValue oldQValue reward state stepSize discount =
                let term2 = (reward + discount * max([Q(state, action) for action in actions]))
                let term2 = term2 - oldQValue
                let term2 = term2 * stepSize
                oldQValue + term2
        α is the learning rate, γ is the discount factor. These are hyperparameters.

*)
module DQN =
    open System

    open TorchSharp
    open type TorchSharp.torch.nn
    open type TorchSharp.torch.optim
    open Plotly.NET

    open Practices.Environments
    open Utils.IOUtils

    let [<Literal>] NumberOfActions = 4
    let startGridWorld() =
        let rec readSize () =
            Console.WriteLine "Please, enter a size (default is 4): "
            match tryReadInt () with
            | Ok size -> size
            | Error errorValue -> 
                Console.WriteLine (fst errorValue)
                readSize ()

        let size = readSize ()

        let rec readSeed () =
            Console.WriteLine "Please, enter a seed (default is size of the grid): "
            match tryReadInt () with
            | Ok seed -> seed
            | Error errorValue -> 
                match snd errorValue with
                | "" ->
                    size 
                | _ -> 
                    Console.WriteLine (fst errorValue)
                    readSeed ()

        let seed = readSeed ()
        let random = Random seed
        let environment = Gridworld(size, StaticMode)
        let board = environment.Board.RenderNp()
        let inputDimension = Array3D.length1 board * Array3D.length2 board * Array3D.length3 board
        let firstHiddenDimension = inputDimension / 2
        let secondHiddenDimension = inputDimension / 4
        let outputDimension = NumberOfActions

        let model :  torch.nn.Module<torch.Tensor, torch.Tensor> = 
            Sequential
                [| 
                    "lin1", Linear(inputDimension   , firstHiddenDimension, dtype=torch.ScalarType.Float64) :> torch.nn.Module<torch.Tensor,torch.Tensor>;
                    "relu1", ReLU() :> torch.nn.Module<torch.Tensor,torch.Tensor>;
                    "lin2", Linear(firstHiddenDimension, secondHiddenDimension, dtype=torch.ScalarType.Float64) :> torch.nn.Module<torch.Tensor,torch.Tensor>;
                    "relu2", ReLU() :> torch.nn.Module<torch.Tensor,torch.Tensor>;
                    "lin3", Linear(secondHiddenDimension, outputDimension, dtype=torch.ScalarType.Float64) :> torch.nn.Module<torch.Tensor,torch.Tensor>;
                    "relu3", ReLU() :> torch.nn.Module<torch.Tensor,torch.Tensor>
                |]

        let lossFunction = MSELoss()
        let learningRate = 1e-3
        let gamma = 0.9
        let epsilon = 1.0

        let rec train (model: torch.nn.Module<torch.Tensor, torch.Tensor>) (optimizer: torch.optim.Optimizer) (generator: torch.Generator) (epsilon: float) (gamma: float) (epochs: int) (losses: float list) =
            // Reset the environment
            let environment = Gridworld(size, StaticMode)


            let rec trainStep epsilon state losses =
                // Output is a tensor of size NumberOfActions with the Q-values for each action
                // we convert the vector of matrices into a single dimension vector
                let dirtyStateVector = 
                    [|
                    for i in 0 .. Array3D.length1 state - 1 do
                        for j in 0 .. Array3D.length2 state - 1 do
                            for k in 0 .. Array3D.length3 state - 1 do
                                // Add noise to the state in order to avoid "dead" neurons and make the model less overfit.
                                yield float (int state[i,j,k]) + random.NextDouble() / 10.
                    |]
                let stateTensor = torch.tensor(dirtyStateVector, dtype=torch.ScalarType.Float64)
                // Get the Q-values for the current state for each action
                let qValues = model.forward stateTensor
                // Get the action with the highest Q-value
                let actionInt = 
                    if random.NextDouble() < epsilon then
                        random.NextInt64 NumberOfActions 
                    else
                        qValues.argmax(dim=0).item<int64>()
                let action = 
                    match actionInt with
                    | 0L -> MoveUp
                    | 1L -> MoveDown
                    | 2L -> MoveLeft
                    | 3L -> MoveRight
                    | _ -> failwith "Invalid action"

                // Take the action in the environment
                environment.MakeMove action
                let reward_t1 = environment.Reward()
                let nextState = environment.Board.RenderNp()

                let dirtyNextStateVector = 
                    [|
                    for i in 0 .. Array3D.length1 nextState - 1 do
                        for j in 0 .. Array3D.length2 nextState - 1 do
                            for k in 0 .. Array3D.length3 nextState - 1 do
                                yield float (int nextState[i,j,k]) + random.NextDouble() / 10.
                    |]
                let nextStateTensor = torch.tensor(dirtyNextStateVector, dtype=torch.ScalarType.Float64)
                let qValues_t1 = 
                    use _ = torch.no_grad()
                    model.forward nextStateTensor

                let maxQValue_t1Tensor = fst (qValues_t1.max(dim=0).ToTuple())
                let maxQValue_t1 = maxQValue_t1Tensor.item<float>()

                // Calculate with the update rule of Q-learning
                let secondTerm = 
                    if reward_t1 = -1 then
                        float reward_t1 + gamma * float maxQValue_t1
                    else
                        float reward_t1

                let y = torch.tensor([|secondTerm|], dtype=torch.ScalarType.Float64)
                let actionIndexTensor = torch.tensor([|int64 actionInt|], dtype=torch.ScalarType.Int64)
                let x = qValues.gather(dim=0, index=actionIndexTensor)
                let loss = lossFunction.forward(x, y)
                optimizer.zero_grad()
                loss.backward()
                let lossValue = loss.item<float>()
                let tensor = optimizer.step()

                let losses = lossValue :: losses
                if reward_t1 <> -1 then
                    losses
                else
                    trainStep epsilon nextState losses

            // Get the initial state
            let state = environment.Board.RenderNp()

            // Train the model
            let losses = trainStep epsilon state losses

            let epsilon = 
                if epsilon > 0.01 then
                    epsilon - 1.0 / float epochs
                else
                    epsilon

            if epochs > 0 then
                train model optimizer generator epsilon gamma (epochs - 1) losses
            else
                losses
        
        let startTraining (model: torch.nn.Module<torch.Tensor, torch.Tensor>) (learningRate: float) (epsilon: float) (gamma: float) (epochsInTens: int) =
            let optimizer = torch.optim.Adam(model.parameters(), lr=learningRate)
            let generator = new torch.Generator(uint64 seed)
            train  model optimizer generator epsilon gamma (epochsInTens * 10) []
        
        let epochsInTens = 100
        let losses = startTraining model learningRate epsilon gamma epochsInTens |> List.rev
        // Plot the losses with Plotly.NET
        let x = [0 .. (epochsInTens * 10)]
        let y = losses

        Chart.Line(x, y)
            |> Chart.withTitle "Losses"
            |> Chart.withXAxisStyle "Epoch"
            |> Chart.withYAxisStyle "Loss"
            |> Chart.show
        ()
