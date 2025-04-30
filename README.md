# QuestForge

A blockchain-powered gamified to-do list application that transforms daily tasks into exciting role-playing quests, leveraging Clarity smart contracts for transparent and engaging task management.

This project is built with Clarity smart contracts for the Stacks blockchain.

## Overview

QuestForge transforms everyday tasks into epic quests through a gamified system that combines task management with RPG elements. Users create characters, complete quests (tasks), earn rewards, and collaborate with others in guilds - all secured and tracked on the blockchain.

## Core Features

- Character creation and progression system
- Quest (task) management with difficulty levels and rewards
- Social features through guild collaboration
- Achievement and reward system
- Task verification and accountability
- Leaderboards and competition mechanics

## Smart Contracts

### Character Manager (`character-manager`)

Handles user character profiles and progression:

- Character creation with different classes (Warrior, Mage, Rogue, Healer)
- Experience points and leveling system
- Skill progression tracking
- Character stats management

Key functions:
```clarity
(create-character (name (string-ascii 50)) (class (string-ascii 20)))
(add-experience (amount uint))
(level-up-skill (skill-name (string-ascii 30)))
```

### Quest System (`quest-system`)

Manages the core quest/task functionality:

- Quest creation with metadata
- Difficulty ratings and deadlines
- Quest completion tracking
- Recurring quests support
- Quest chains with prerequisites

Key functions:
```clarity
(create-quest (title (string-utf8 50)) (description (string-utf8 500)) ...)
(complete-quest (quest-id uint))
(create-quest-chain (prerequisite-quest-ids (list 10 uint)))
```

### Reward Vault (`reward-vault`)

Handles the reward economy and achievements:

- Token rewards for completed tasks
- Special badges and items
- Achievement tracking
- Time-limited reward campaigns
- Leaderboard scoring

Key functions:
```clarity
(issue-reward (user principal) (reward-id (string-ascii 24)) (amount uint))
(create-reward-campaign (campaign-id (string-ascii 24)) ...)
(claim-achievement-reward (achievement-id (string-ascii 24)))
```

### Guild Collaborator (`guild-collaborator`)

Enables social features and group activities:

- Guild creation and management
- Shared quests between members
- Group challenges
- Task verification by peers
- Guild activity tracking
- Social leaderboards

Key functions:
```clarity
(create-guild (name (string-ascii 50)) (description (string-ascii 200)))
(share-quest (guild-id uint) (quest-id uint) (title (string-ascii 100)))
(verify-task-completion (guild-id uint) (task-id uint) (task-owner principal))
```

## Getting Started

To interact with QuestForge contracts:

1. Deploy the contracts in the following order:
   - character-manager
   - quest-system
   - reward-vault
   - guild-collaborator

2. Create a character:
```clarity
(contract-call? .character-manager create-character "Hero" "warrior")
```

3. Create your first quest:
```clarity
(contract-call? .quest-system create-quest "My First Quest" "Description" u1 deadline "daily" u10 false none none (list))
```

4. Form or join a guild:
```clarity
(contract-call? .guild-collaborator create-guild "Quest Warriors" "A guild for ambitious questers")
```

## Architecture

The system is built on four main pillars:

1. **Identity and Progression (character-manager)**
   - Character profiles
   - Experience tracking
   - Level progression
   - Skill system

2. **Task Management (quest-system)**
   - Quest creation
   - Completion tracking
   - Quest chains
   - Recurring tasks

3. **Reward Economy (reward-vault)**
   - Token rewards
   - Achievements
   - Special items
   - Campaigns

4. **Social Features (guild-collaborator)**
   - Guild system
   - Collaborative questing
   - Peer verification
   - Social leaderboards

## Security Considerations

- Quest completion verification through guild peer review
- Reward issuance controlled by authorized contracts only
- Guild membership validation for collaborative features
- Experience point calculations protected against exploitation
- Campaign timelock mechanisms
- Multi-step verification for high-value rewards

## Contributing

Contributions are welcome! Please check our contribution guidelines and coding standards before submitting pull requests.

## License

[License information to be added]