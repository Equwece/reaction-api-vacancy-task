# Reaction API Vacancy Task

RESTfull API over Neo4j database for vacancy task.

## Getting Started

### Setup Neo4j DB

```bash
podman run --rm -d --publish=7474:7474 --publish=7687:7687 --volume=neo4jData:/data --name=neo4jDB docker.io/library/neo4j:3.5.35-community
```

### Setup Reaction API

Clone this repo:
```bash
git clone github.com/Equwece/reaction-api-vacancy-task
cd reaction-api-vacancy-task
```

#### Setup development environment

The project uses Nix to manage dependencies, including development ones. To get terminal session with valid development dependencies in $PATH use:
```bash
nix-shell
```

#### Specify env variables

The app needs several environment variables for work. Copy and update `.env.example`:
```bash
cp .env.example .env
```

Set `SETUP_DB` var to `True` to setup the Neo4j DB with random data at app's startup.

#### Build application
```bash
nix-shell
cabal build
```

#### Run application
```bash
nix-shell
cabal run
```

#### Run tests (you must specify test db env vars)
```bash
nix-shell
cabal test
```

## Features
- [x] Relevant Haskell data types for the domain
- [x] Creating new `reaction`
- [x] Getting `reaction` by its UUID
- [x] Search for the path between `molecules` with minimal length

### Endpoints
Full API spec is available at `/api/v1/docs`.

#### POST /api/v1/reactions
Create new reaction, specifying `reagents`, `catalysts`, `product` and their relation properties.

#### GET /api/v1/reactions/{reactionId}
Get reaction by its UUID.

#### POST /api/v1/search
Search for the minimal length path (or paths) between molecules, specifying its UUIDs.

## Generalizations and possible further development
To store new possible data types, like `reaction's` mechanism, we can create new node type `Mechanism` and new relation type `PROVIDE`: `(:Mechanism)-[:PROVIDE]->(:Reaction)`. In Haskell app we can create corresponding data type `Mechanism`.

## License
The code in this project is licensed under LGPLv3 license.
