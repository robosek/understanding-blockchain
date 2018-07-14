#r "packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll"
#r "packages/Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"

open System
open System.Security.Cryptography
open System.Text
open Newtonsoft.Json
open System.Diagnostics
open System.IO

//######################## Types ###############################################
type Hash = Hash of string
type Nonce = Nonce of int
type Address = Address of string

//######################## Common helpers ###############################################
module Helpers = 
    let toRawHash (Hash hash) =
        hash

//######################## Model ###############################################
type Transaction(fromAddress: Address, toAddress:Address, amount:decimal) =
    let (Address fromAddr) = fromAddress
    let (Address toAddr) = toAddress

    member val FromAddress:string = fromAddr with get,set
    member val ToAddress:string = toAddr with get,set
    member val Amount = amount with get,set

type Block(timestamp:DateTime, previousHash:Hash, data:Transaction list) =
    let createInputHashText (timestamp:DateTime) (Hash previousHash) (data:Transaction list) (Nonce nonce) =
        let dateTimeText = timestamp.ToLongDateString()
        let serializedData = JsonConvert.SerializeObject data
        sprintf "%s-%s-%s-%O" dateTimeText previousHash serializedData nonce

    let calculateHash (timestamp:DateTime) (previousHash: Hash) (data:Transaction list) (nonce:Nonce)=
        let sha256 = SHA256.Create()
        let inputText = createInputHashText timestamp previousHash data nonce
        let inputBytes = Encoding.ASCII.GetBytes inputText
        let outputBytes = sha256.ComputeHash inputBytes
        Hash(Convert.ToBase64String outputBytes)

    let mutable calculatedHash = 
        calculateHash DateTime.Now (Hash("")) [] (Nonce(0))

    member val Index = 0 with get,set
    member val Timestamp = timestamp with get,set
    member val PreviousHash = Helpers.toRawHash previousHash with get,set
    member val Data = data with get,set
    member this.Hash with get() = Helpers.toRawHash calculatedHash and set(value) = calculatedHash <- Hash(value)
    member val Nonce = 0 with get,set                          
    member this.CalculateHash() =
        calculateHash this.Timestamp (Hash(this.PreviousHash)) this.Data (Nonce(this.Nonce))
    member this.Main(difficulty:int) =
        let leadingZeros = new string('0', difficulty)
        let hashIsNull = fun () -> (Helpers.toRawHash calculatedHash) |> isNull
        let isTooShort = fun () -> (Helpers.toRawHash calculatedHash).Length < leadingZeros.Length
        let notEnoughLeadingZeros = fun () -> (Helpers.toRawHash calculatedHash).Substring(0, difficulty) <> leadingZeros

        while hashIsNull() || isTooShort() || notEnoughLeadingZeros() do
            this.Nonce <- this.Nonce + 1
            calculatedHash <- this.CalculateHash()

type BlockChain(difficulty:int, reward:decimal) =
        let mutable blocks:Block list = []
        let mutable transactions: Transaction list = []
        let addGenesisBlock = fun () -> blocks <- [new Block(DateTime.Now, (Hash("")), [])]
        let getLastestBlock = fun () -> blocks |> List.head
            
        do addGenesisBlock()

        member private this.AddBlock(block: Block)=
            let lastBlock = getLastestBlock()
            block.PreviousHash <- lastBlock.Hash
            block.Index <- lastBlock.Index + 1
            block.Main(this.Difficulty)
            blocks <- (blocks |> List.append [ block ])

        member val Difficulty = difficulty with get,set
        member val Reaward = reward with get,set
        member this.Blocks with get() = blocks and set(value) = blocks <- value  

        member this.IsValid() =
            let lastBlock = getLastestBlock()
            let previousBlock = blocks |> List.find(fun block -> block.Index = (lastBlock.Index - 1))
            lastBlock.Hash = Helpers.toRawHash(lastBlock.CalculateHash()) && lastBlock.PreviousHash = previousBlock.Hash

        member this.AddTransaction(transaction:Transaction) =
            transactions <- transactions |> List.append [transaction]

        member this.ProcessPendingTransactions(minerAddress:Address)  =
            let lastBlock = getLastestBlock()
            this.AddBlock(Block(DateTime.Now, Hash(lastBlock.Hash), transactions))

            transactions <- []
            this.AddTransaction(Transaction((Address("")),minerAddress,this.Reaward))


//######################## Tests ###############################################
let stopWatch = new Stopwatch()
stopWatch.Start()

let addressA = (Address("a"))
let addressB = (Address("b"))
let addressC = (Address("c"))
let addressD = (Address("d"))
let minerA = (Address("MinerA"))
let minerB = (Address("MinerB"))

let roboCoin = BlockChain(difficulty = 2, reward = 100M)
roboCoin.AddTransaction(Transaction(addressB,addressA,120M))
roboCoin.AddTransaction(Transaction(addressB,addressA,200M))
roboCoin.AddTransaction(Transaction(addressA,addressB,150M))
roboCoin.ProcessPendingTransactions(minerA )
printfn "Is valid? :%b" (roboCoin.IsValid())

roboCoin.AddTransaction(Transaction(addressD,addressC,331M))
roboCoin.ProcessPendingTransactions(minerB)
printfn "Is valid? :%b" (roboCoin.IsValid())

stopWatch.Stop()

let serializedBlockChain = JsonConvert.SerializeObject(roboCoin)
printfn "Current blockchain %s" serializedBlockChain
printfn "Mining time: %s" (stopWatch.Elapsed.ToString())

File.WriteAllText("blockchain.json", serializedBlockChain)