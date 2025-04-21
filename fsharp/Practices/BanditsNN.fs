namespace Practices

module BanditsNN =
    open System
    open TorchSharp
    open type TorchSharp.torch.nn
    open type TorchSharp.torch.optim
    open Plotly.NET
    open Utils.IOUtils
    open Utils.TorchUtils


    type ContextBandit(arms: int, random: Random) =
        let banditMatrix = Array2D.init arms arms (fun _ _ -> random.NextSingle())
        let mutable currentState = random.Next(0, arms)

        // Simulates 'arms' Bernoulli trials with probability 'prob' and returns the number of successes.
        let calculateReward (prob: float32) : float32 =
            seq { 0 .. arms - 1 }
            |> Seq.sumBy (fun _ -> if random.NextSingle() < prob then 1f else 0f)

        // Updates the current state to a new random value.
        let updateState () : unit =
            currentState <- random.Next(0, arms)

        // Returns the current state of the bandit.
        member _.CurrentState = currentState

        // Chooses an arm, calculates the reward based on the current state and the chosen arm's probability,
        // updates the state, and returns the calculated reward.
        member this.ChooseArm(arm: int) : float32 =
            if arm < 0 || arm >= arms then
                invalidArg (nameof arm) "Arm index out of bounds."

            let prob = banditMatrix.[currentState, arm]
            let reward = calculateReward prob
            updateState()
            reward

        // Expose the bandit matrix if needed (optional)
        member _.BanditMatrix = banditMatrix

    let startTest() =
        let rec readNumberOfArms () =
            Console.WriteLine "Please, enter a number of arms: "
            match tryReadInt () with
            | Ok numberOfArms -> numberOfArms
            | Error errorValue -> 
                Console.WriteLine (fst errorValue)
                readNumberOfArms ()

        let numberOfArms = readNumberOfArms ()

        let rec readSeed () =
            Console.WriteLine "Please, enter a seed (default is number of arms): "
            match tryReadInt () with
            | Ok seed -> seed
            | Error errorValue -> 
                match snd errorValue with
                | "" -> 
                    numberOfArms
                | _ -> 
                    Console.WriteLine (fst errorValue)
                    readSeed ()

        let seed = readSeed ()

        let batchSize= 10
        let inputDimension = numberOfArms
        let hiddenDimension = 150
        let outputDimension = numberOfArms

        // Explicitly type the model as a Module mapping Tensor -> Tensor
        let model : torch.nn.Module<torch.Tensor, torch.Tensor> = 
            Sequential
                [| "lin1", Linear(inputDimension   , hiddenDimension) :> torch.nn.Module<_,_>;
                "relu1", ReLU() :> torch.nn.Module<_,_>;
                "lin2", Linear(hiddenDimension, outputDimension) :> torch.nn.Module<_,_>;
                "relu2", ReLU() :> torch.nn.Module<_,_> |]


        let lossFunction = MSELoss()

        let random = Random seed
        let environment = ContextBandit(numberOfArms, random)

        // Add explicit type annotation to the model parameter
        let rec train
            (environment: ContextBandit)
            (model: torch.nn.Module<torch.Tensor, torch.Tensor>)
            (optimizer: torch.optim.Optimizer)
            (generator: torch.Generator)
            (epochs: int)
            (rewards: float32 list) =
            if epochs = 0 then
                // Base case: Training complete, return accumulated rewards
                rewards
            else
                (* Diagram of the neural network:
                                                                                    Layer 1
                ┌────────────────────────┐   N    ┌────────────┐   R¹⁰    ┌────────────┐         ┌──────┐
                │environment.CurrentState│───────>│oneHotEncode│─────────˃│ θ₁: 10×100 │────────>│ ReLU │───┐
                └────────────────────────┘        └────────────┘          └────────────┘         └──────┘   │
                                                                                                            │ R¹⁰⁰
                                                                                    Layer 2                 │
                ┌────────────┐   P¹⁰  ┌───────────────┐       R¹⁰      ┌──────┐         ┌────────────┐      │
                │samp. action│<───────│    softmax    │<─────┬─────────│ ReLU │<────────│ θ₂: 100×10 │<─────┘
                └────────────┘        └───────────────┘      │         └──────┘         └────────────┘
                    N │                                      │
                      ˅                                      ˅
                ┌─────────────────────┐   Reward: N   ┌────────────┐   R¹⁰    ┌─────────┐
                │environment.ChooseArm│──────────────>│lossFunction│─────────>│Optimizer│
                └─────────────────────┘               └────────────┘          └─────────┘
                *)
                // --- 1. State Preparation ---
                // Get the current state from the environment
                let discreteState = environment.CurrentState
                // One-hot encode the discrete state
                let oneHotState = oneHotEncode (numberOfArms, discreteState, 1.0f)
                // Convert the state representation to a tensor for the model
                let currentStateTensor = torch.tensor oneHotState
                // --- 2. Forward Pass (Prediction) ---
                // Pass the state through the neural network to get predicted values (Q-values) for each arm.
                // Output shape should be [numberOfArms] due to corrected outputDimension.
                let predictedRewards = model.forward currentStateTensor

                // --- 3. Action Selection (Epsilon-Greedy or Softmax Sampling) ---
                // Convert predicted rewards (logits) into a probability distribution over actions using softmax.
                // Temperature (tau > 1.0) encourages exploration by making probabilities more uniform.
                let probabilityDistribution = softmax predictedRewards 1.3f 0L // Using temperature tau=2.0
                // Normalize explicitly (optional, softmax should be close)
                let normalizedProbabilityDistribution = probabilityDistribution / torch.sum probabilityDistribution
                // Sample an action (arm index) from the probability distribution.
                // Actions with higher predicted rewards have higher probability of being chosen.
                let chosenArmIndex64 = sampleIndexFromProbabilities normalizedProbabilityDistribution (Some generator)
                let chosenArmIndex = int chosenArmIndex64 // Convert to int for environment interaction

                // --- 4. Environment Interaction ---
                // Execute the chosen action in the environment and observe the reward.
                // The environment also transitions to its next state internally.
                let currentReward = environment.ChooseArm chosenArmIndex

                // --- 5. Target Calculation (for Loss) ---
                // Create a target tensor for the loss function. Start by cloning the original predictions.
                // Detach the clone from the computation graph; gradients should not flow back through the target.
                let targetRewards = predictedRewards.clone().detach()
                // Create a tensor scalar for the actual reward received.
                // Ensure dtype and device match the target tensor for assignment.
                let actualRewardTensor = torch.tensor(currentReward, dtype=predictedRewards.dtype, device=predictedRewards.device)
                // Modify the target tensor: Update the value for the *chosen* action to be the *actual* reward received.
                // The targets for unchosen actions remain the original predicted values.
                targetRewards.[chosenArmIndex64] <- actualRewardTensor
                

                // --- 6. Loss Calculation ---
                // Calculate the loss between the network's predictions (predictedRewards) and the target (targetRewards).
                // MSELoss compares the prediction for the chosen arm with the actual reward, 
                // and the predictions for unchosen arms with themselves (resulting in zero loss for those arms).
                let loss = lossFunction.forward(predictedRewards, targetRewards)

                // --- 7. Backpropagation and Optimization ---
                // Clear gradients from the previous step before calculating new ones.
                optimizer.zero_grad()
                // Compute gradients of the loss with respect to all model parameters.
                loss.backward()
                // Update the model parameters based on the computed gradients using the optimizer algorithm (e.g., Adam).
                optimizer.step() |> ignore

                // --- 8. Recursion ---
                // Continue training for the remaining epochs with the updated model and optimizer state.
                train environment model optimizer generator (epochs-1) (currentReward :: rewards )


        let startTraining
            (model: torch.nn.Module<torch.Tensor, torch.Tensor>)
            (environment: ContextBandit)
            (learningRate: float32)
            (epochsInTens: int) =
            let optimizer = Adam(model.parameters(), float learningRate)
            let generator = new torch.Generator(uint64 seed)
            let rewards = []
            train environment model optimizer generator (epochsInTens * 10) rewards

        let trainingIterationsInTens = 500

        let rewards = startTraining model environment 2e-2f trainingIterationsInTens
        let reversedRewards = rewards |> List.rev

        //print the rewards
        Console.WriteLine $"Rewards: {String.Join(',', reversedRewards)}"

        let calculateNewMean (currentMean: float32) (newReward: float32) (count: int) =
            // Handle count=0 case to avoid division by zero when calculating the first mean
            if count = 0 then newReward 
            else (newReward + float32 count * currentMean) / (float32 count + 1f)

        let accumulatedRewardMeans =
            reversedRewards // Input list should be in chronological order
            |> List.scan (fun (mean, count) reward -> 
                // 'mean' is the accumulated mean from the *previous* step
                // 'count' is the number of elements processed *before* this one
                let newCount = count + 1
                // Calculate the new mean based on the previous mean, current reward, and previous count
                let newMean = calculateNewMean mean reward count 
                // Return the new state: (newly calculated mean, updated count)
                newMean, newCount
            ) (0.0f, 0) // Initial state: Mean is 0.0 before processing any elements, count is 0
            |> List.map fst // Extract just the mean value from each state tuple (mean, count)
            |> List.tail // Remove the initial state (0.0f, 0) which was just a placeholder
        Console.WriteLine $"Accumulated reward means: {String.Join(',', accumulatedRewardMeans)}"


        let chart = 
            Chart.Scatter([1..trainingIterationsInTens*10], accumulatedRewardMeans, mode=StyleParam.Mode.Markers)
            |> Chart.withXAxisStyle "Número de intentos"
            |> Chart.withYAxisStyle "Promedio acumulado"

        chart |> Chart.show