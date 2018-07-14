#r "packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll"
#r "packages/Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"

open System
open System.Security.Cryptography
open System.Text
open Newtonsoft.Json
open System.Diagnostics

//######################## Model ###############################################
type Transaction(fromAddress: string, toAddress:string, amount:decimal) = 
    member val FromAddress = fromAddress with get,set
    member val ToAddress = toAddress with get,set
    member val Amount = amount with get,set

type Block(timestamp:DateTime, previousHash:string, data:Transaction list) =

    let _calculateHash timestamp previousHash data nonce=
        let sha256 = SHA256.Create()
        let inputText = sprintf "%s-%s-%s-%s" timestamp previousHash data nonce
        let inputBytes = Encoding.ASCII.GetBytes inputText
        let outputBytes = sha256.ComputeHash inputBytes
        Convert.ToBase64String outputBytes

    let mutable _calculatedHash = _calculateHash (DateTime.Now.ToLongDateString()) null "{}" "0"
    member val Index = 0 with get,set
    member val Timestamp = timestamp with get,set
    member val PreviousHash = previousHash with get,set
    member val Data = data with get,set
    member this.Hash with get() = _calculatedHash and set(value) = _calculatedHash <- value
    member val Nonce = 0 with get,set                          
    member this.CalculateHash():string =
        let serializedData = JsonConvert.SerializeObject this.Data
        _calculateHash (this.Timestamp.ToLongDateString()) this.PreviousHash serializedData (this.Nonce.ToString())
    
    member this.Main(difficulty:int) =
        let leadingZeros = new string('0', difficulty)
        let hashIsNull = fun () -> _calculatedHash |> isNull
        let isTooShort = fun () -> _calculatedHash.Length < leadingZeros.Length
        let notEnoughLeadingZeros = fun () -> _calculatedHash.Substring(0, difficulty) <> leadingZeros

        while hashIsNull() || isTooShort() || notEnoughLeadingZeros() do
            this.Nonce <- this.Nonce + 1
            _calculatedHash <- this.CalculateHash()

type BlockChain(difficulty:int, reaward:decimal) =
        let mutable blocks:Block list = []
        let mutable transactions: Transaction list = []
        let addGenesisBlock = fun () -> blocks <- [new Block(DateTime.Now, null, [])]
        let getLastestBlock = fun () -> blocks |> List.head
            
        do addGenesisBlock()

        member private this.AddBlock(block: Block)=
            let lastBlock = getLastestBlock()
            block.PreviousHash <- lastBlock.Hash
            block.Index <- lastBlock.Index + 1
            block.Main(this.Difficulty)
            blocks <- (blocks |> List.append [ block ])

        member val Difficulty = difficulty with get,set
        member val Reaward = reaward with get,set
        member this.Blocks with get() = blocks and set(value) = blocks <- value  

        member this.IsValid() =
            let lastBlock = getLastestBlock()
            let previousBlock = blocks |> List.find(fun block -> block.Index = (lastBlock.Index - 1))
            lastBlock.Hash = lastBlock.CalculateHash() && lastBlock.PreviousHash = previousBlock.Hash

        member this.AddTransaction(transaction:Transaction) =
            transactions <- transactions |> List.append [transaction]

        member this.ProcessPendingTransactions(minerAddress:string)  =
            let lastBlock = getLastestBlock()
            this.AddBlock(Block(DateTime.Now, lastBlock.Hash, transactions))

            transactions <- []
            this.AddTransaction(Transaction(null,minerAddress,this.Reaward))


//######################## Tests ###############################################
let stopWatch = new Stopwatch()
stopWatch.Start()

let roboCoin = BlockChain(difficulty = 2, reaward = 100M)
roboCoin.AddTransaction(Transaction("b","a",120M))
roboCoin.AddTransaction(Transaction("b","a",200M))
roboCoin.AddTransaction(Transaction("a","b",150M))
roboCoin.ProcessPendingTransactions("MinerA")
printfn "Is valid? :%b" (roboCoin.IsValid())

roboCoin.AddTransaction(Transaction("d","c",331M))
roboCoin.ProcessPendingTransactions("MinerB")
printfn "Is valid? :%b" (roboCoin.IsValid())

stopWatch.Stop()

printfn "Current blockchain %s" (JsonConvert.SerializeObject(roboCoin));;
printfn "Mining time: %s" (stopWatch.Elapsed.ToString())