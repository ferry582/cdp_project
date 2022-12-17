# Betting Plutus Smart Contract
This project is a simple smart contract of betting game between two wallets. To start, the first wallet must place a bet, then the second wallet can take the bet. Then each wallet will guess the random solution, and those who succeed in guessing are entitled to receive all the lovelace that are at stake.

## End Points
* **Place Bet**
Used to place or offer bet made by the first wallet. <br>
User Inputs :
  - Amount -> the amount of lovelace that placer want to bet
  - Taker's Beneficiary -> the target or taker's wallet address

* **Take Bet**
Used to take the betting offer from the first wallet by the second wallet. <br>
User Inputs :
  - Amount -> the amount of lovelace that taker want to put at stake
  - Placer's Beneficiary -> the placer's wallet address
  - Start From -> the deadline value, where guessing can be done after the deadline has passed

* **Guess**
Used to guess a random value, if correct, the winner will receive all stake at the script <br>
User Inputs :
  - Your Guess -> the integer value you want to guess

## Simulation
* **Wallet 1: PlaceBet**
  - Amount: 20000000 Lovelace
  - Taker's Beneficiary: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (Wallet 2)
* **Wait: 10 Slots**<br>
* **Wallet 2: TakeBet**
  - Amount: 20000000 Lovelace
  - Placer's Beneficiary: a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2 (Wallet 1)
  - Start From: 1596059101000 (+10 Slots)
* **Wait: 10 Slots**
* **Wallet 1: Guess**
  - Your Guess: 1
* **Wait: 10 Slots**
* **Wallet 2: Guess**
  - Your Guess: 2
* **Wait: 10 Slots**

<br>
<img src="images\SuccessfulSimulation.png" width=500>
