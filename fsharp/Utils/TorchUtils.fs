namespace Utils

module TorchUtils =
    open TorchSharp
    open type TorchSharp.torch.nn
    
    let oneHotEncode (n: int, position: int, value: float32) =
        Array.init n (fun i -> if i = position then value else 0.0f)

    /// Applies softmax with a temperature scaling factor.
    /// Temperature < 1.0 makes the distribution sharper (more peaky).
    /// Temperature > 1.0 makes the distribution softer (more uniform).
    let softmax (logits: torch.Tensor) (tau: float32) (dim: int64) =
        if tau <= 0.0f then
            failwith "Temperature must be positive." 
        else
            let scaledLogits = logits / tau
            functional.softmax(scaledLogits, dim)

    let sampleIndexFromProbabilities (probabilities: torch.Tensor) (generator: torch.Generator option) : int64 =
        if probabilities.dim() <> 1L || probabilities.shape.[0] = 0L then
            invalidArg (nameof probabilities) "Input probabilities must be a non-empty 1D tensor."
    
        let num_samples = 1L
        let replacement = true
    
        let sampledIndexTensor = 
            match generator with
            | Some gen -> torch.multinomial(probabilities, num_samples, replacement, generator = gen)
            | None     -> torch.multinomial(probabilities, num_samples, replacement)
            
        sampledIndexTensor.item<int64>()