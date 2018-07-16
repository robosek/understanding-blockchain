#r "packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll"
#r "packages/Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"

open System
open System.Security.Cryptography
open System.Text
open Newtonsoft.Json
open System.Diagnostics
open System.IO

//######################## Types ###############################################
module ConstrainedTypes = 
    type Hash =  private Hash of string
    type Address = private Address of string

    module Hash = 
        let create str = 
            if String.IsNullOrEmpty str then
                Hash String.Empty
            else
                Hash str
        
        let value (Hash rawHash) = rawHash

    module Address =
        let create str = 
            if String.IsNullOrEmpty str then
                Address String.Empty
            else
                Address str

        let value (Address rawAddress) = rawAddress
        

type Nonce = Nonce of int

type Transaction = {
    FromAddress: ConstrainedTypes.Address
    ToAddress: ConstrainedTypes.Address
    Amount: decimal
}

//######################## Modules ###############################################
module Block =
    open ConstrainedTypes
    type Block = {
        Index: int
        Timestamp: DateTime
        PreviousHash: Hash
        Hash: Hash
        Transactions: Transaction list
        Nonce: Nonce
    } 

    let private createInputHashText (timestamp:DateTime) (previousHash: Hash) (data:Transaction list) (Nonce nonce) =
        let dateTimeText = timestamp.ToLongDateString()
        let serializedData = JsonConvert.SerializeObject data
        sprintf "%s-%s-%s-%O" dateTimeText (Hash.value previousHash) serializedData nonce
    let calculateHash (timestamp:DateTime) (previousHash: Hash) (data:Transaction list) (nonce: Nonce) =
        let sha256 = SHA256.Create()
        let inputText = createInputHashText timestamp previousHash data nonce
        let inputBytes = Encoding.ASCII.GetBytes inputText
        let outputBytes = sha256.ComputeHash inputBytes
        Hash.create(Convert.ToBase64String outputBytes)
    let create index timestamp previousHash data nonce = 
        {
            Index = index
            Timestamp = timestamp
            PreviousHash = previousHash
            Hash = calculateHash timestamp previousHash data nonce
            Transactions = data
            Nonce = nonce
        }
    let rec main difficulty (block:Block) =
        let rawHash = Hash.value block.Hash
        let (Nonce rawNonce) = block.Nonce
        let newNonce = Nonce(rawNonce + 1)

        let leadingZeros = new string('0', difficulty)
        let isTooShort (text:string) = text.Length < difficulty
        let notEnoughLeadingZeros (text:string) = text.Substring(0, difficulty) <> leadingZeros

        match rawHash with
        | hash when isTooShort hash || notEnoughLeadingZeros hash -> let newHash = calculateHash block.Timestamp block.PreviousHash block.Transactions newNonce
                                                                     main difficulty {block with Nonce = newNonce; Hash = newHash}
        | _ -> block



module BlockChain =
    open ConstrainedTypes
    open Block

    type BlockChain = {
        Difficulty: int
        Reward: decimal
        Blocks: Block list
    }

    let create difficulty reward = 
        let gensisBlock = Block.create 0 DateTime.Now (Hash.create("")) [] (Nonce(0))
        {
            Difficulty = difficulty
            Reward = reward
            Blocks = [gensisBlock]
        }
    let private addBlock (block:Block) (blockChain:BlockChain) =
        let lastBlock = blockChain.Blocks |> List.head
        let correctBlock = Block.main blockChain.Difficulty {block with PreviousHash = lastBlock.Hash; Index = lastBlock.Index + 1}
        let updatedBlocks = blockChain.Blocks |> List.append [correctBlock]
        {blockChain with Blocks = updatedBlocks}
    
    let addTransaction (transaction:Transaction) (block:Block) = 
        let updatedTransactions = block.Transactions |> List.append [transaction]
        {block with Transactions = updatedTransactions}
    
    let processTransactions miner (block:Block) (blockChain:BlockChain) =
        let updatedBlockChain = addBlock block blockChain
        let emptyBlock = {Timestamp = DateTime.Now; Index = 0; PreviousHash = Hash.create(""); Hash = Hash.create(""); Nonce = Nonce(0); Transactions = []}
        let newBlock = addTransaction {FromAddress = Address.create(""); ToAddress = miner; Amount = blockChain.Reward } emptyBlock
        updatedBlockChain, newBlock
    
    let isValid (blockChain:BlockChain) = 
        let lastBlock = blockChain.Blocks |> List.head
        let previousBlock = blockChain.Blocks |> List.find(fun block -> block.Index = (lastBlock.Index-1))
        let correctHash = lastBlock.Hash = Block.calculateHash lastBlock.Timestamp lastBlock.PreviousHash lastBlock.Transactions lastBlock.Nonce
        correctHash && lastBlock.PreviousHash = previousBlock.Hash

//######################## DTO ###############################################

module DtoConverter =
    open ConstrainedTypes
    open BlockChain
    open Block

    type TransactionDto = {
        FromAddress: string
        ToAddress: string
        Amount: decimal
    }

    type BlockDto = {
         Index: int
         Timestamp: DateTime
         PreviousHash: string
         Hash: string
         Transactions: TransactionDto list
         Nonce: int
    }
    type BlockChainDto = {
        Difficulty: int
        Reward: decimal
        Blocks: BlockDto list
    }

    let private convertTransaction (transaction:Transaction) : TransactionDto =
        {
            FromAddress = Address.value transaction.FromAddress
            ToAddress = Address.value transaction.ToAddress
            Amount = transaction.Amount
        }

    let private convertBlock (block:Block) : BlockDto =
        let (Nonce rawNonce) = block.Nonce
        {
            BlockDto.Index = block.Index
            BlockDto.Timestamp = block.Timestamp
            BlockDto.PreviousHash = Hash.value block.PreviousHash
            BlockDto.Hash = Hash.value block.Hash
            BlockDto.Transactions = block.Transactions |> List.map(convertTransaction)
            Nonce = rawNonce
        }
    let convert (blockChain:BlockChain) : BlockChainDto =
        {
            BlockChainDto.Difficulty = blockChain.Difficulty
            BlockChainDto.Reward = blockChain.Reward
            BlockChainDto.Blocks = blockChain.Blocks |> List.map(convertBlock)
        }


//######################## Tests ###############################################
open ConstrainedTypes
let stopWatch = new Stopwatch()
stopWatch.Start()

let addressA = (Address.create("a"))
let addressB = (Address.create("b"))
let addressC = (Address.create("c"))
let addressD = (Address.create("d"))
let minerA = (Address.create("MinerA"))
let minerB = (Address.create("MinerB"))

let roboCoin = BlockChain.create 2 200M
let updatedBlock = BlockChain.addTransaction({FromAddress = addressB; ToAddress=addressA;Amount=120M}) (roboCoin.Blocks |> List.head)
let updatedBlock1 = BlockChain.addTransaction({FromAddress = addressA; ToAddress=addressB;Amount=2000M}) updatedBlock
let updatedBlock2 = BlockChain.addTransaction({FromAddress = addressB; ToAddress=addressA;Amount=3000M}) updatedBlock1
let updatedBlock3 = BlockChain.addTransaction({FromAddress = addressA; ToAddress=addressB;Amount=78M}) updatedBlock2

let updatedRoboCoin, newBlock = BlockChain.processTransactions minerA updatedBlock3 roboCoin

printfn "Is valid? :%b" (BlockChain.isValid updatedRoboCoin) 

let updatedBlock4 = BlockChain.addTransaction({FromAddress = addressC; ToAddress=addressD;Amount=12000M}) newBlock
let updatedBlock5 = BlockChain.addTransaction({FromAddress = addressD; ToAddress=addressC;Amount=20200M}) updatedBlock4

let updatedRoboCoin2, newBlock2 = BlockChain.processTransactions minerB updatedBlock5 updatedRoboCoin

printfn "Is valid? :%b" (BlockChain.isValid updatedRoboCoin2) 

stopWatch.Stop()

let serializedBlockChain = JsonConvert.SerializeObject((DtoConverter.convert updatedRoboCoin2))
printfn "Current blockchain %A" serializedBlockChain
printfn "Mining time: %s" (stopWatch.Elapsed.ToString())

File.WriteAllText("blockchain.json", serializedBlockChain)