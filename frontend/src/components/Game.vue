<template>
  <div>
    <h3 v-if="winner != null">{{ winner === 'player1' ? 'Player 1' : 'Player 2' }} wins!</h3>

    <p class="player-info player2">Player 2 ({{ game.player2 }}): {{game.state.playerStates[1].wonPieces}} won pieces, {{game.state.playerStates[1].outOfPlayPieces}} pieces out of play</p>

    <div class="container">
      <img src="./urboard.png" alt="Royal game of Ur board">

      <div
        v-for="piece in pieces"
        :class="['piece', piece.player]"
        :style="{left: piece.x + 'px', top: piece.y + 'px'}"
        @click="piece.isMine && isMyTurn ? $emit('move', piece.ix) : null">
      </div>
    </div>

    <p class="player-info player1">Player 1 ({{ game.player1 }}): {{game.state.playerStates[0].wonPieces}} won pieces, {{game.state.playerStates[0].outOfPlayPieces}} pieces out of play</p>

    <p>Last dice roll: {{ game.state.lastRoll }}</p>
    <template v-if="viewerType != null && winner == null">
      <button @click="$emit('roll')" :disabled="!isMyTurn">Roll</button>
      <button @click="$emit('add')" :disabled="!isMyTurn">Add piece</button>
      <button @click="$emit('pass')" :disabled="!isMyTurn">Pass</button>
    </template>
  </div>
</template>

<script>
import _ from 'lodash';

function positionTo2dPosition(position, player) {
  const y = (player === 0) ? 1 : -1;

  if(position < 4) { return { x: 3 - position, y: y }; }
  if(position >= 4 && position <= 11) { return { x: position - 4, y: 0 }; }
  if(position > 11) { return { x: 19 - position, y: y }; }

  return null;
}

function screenCoords(position, player) {
  const coords = positionTo2dPosition(position, player);

  if(coords == null) return null;

  const { x, y } = coords;

  return { x: 74*x + 40, y: 74*(y+1) + 51 };
}

export default {
  props: ['game', 'viewerType'],

  computed: {
    winner() {
      if(this.game == null) {
        return null;
      }

      if(this.game.state.playerStates[0].wonPieces === 7) {
        return 'player1';
      }
      else if(this.game.state.playerStates[1].wonPieces === 7) {
        return 'player2';
      }
      else return null;
    },

    pieces() {
      if(this.game == null) {
        return [];
      }

      return _.concat(
        [],
        this.game.state.playerStates[0].fieldedPieces.map((p, i) => ({ ix: i, position: p.position, player: 0 })),
        this.game.state.playerStates[1].fieldedPieces.map((p, i) => ({ ix: i, position: p.position, player: 1 }))
      )
        .map(p => {
          const { x, y } = screenCoords(p.position, p.player);
          const player = (p.player === 0) ? 'player1' : 'player2';
          return {
            ix: p.ix, x, y, player,
            isMine: this.viewerType === player
          };
        });
    },

    isMyTurn() {
      if(this.game == null) {
        return false;
      }

      return this.winner == null && this.game.state.currentPlayer === this.viewerType;
    }
  }
};
</script>

<style scoped>
.container {
  position: relative;
}

.piece {
  position: absolute;
  width: 60px;
  height: 60px;
}

.piece.player1 {
  background-color: red;
}

.piece.player2 {
  background-color: blue;
}

.player-info.player1 {
  color: red;
}

.player-info.player2 {
  color: lightblue;
}
</style>
