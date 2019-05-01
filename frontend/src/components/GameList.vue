<template>
  <div>
    <h2>Ongoing games</h2>

    <p v-if="gamesToShow == null">
      Loading...
    </p>
    <ul v-else-if="gamesToShow.length > 0">
      <li
        v-for="game in gamesToShow"
        :key="game.id"
      >
        <RouterLink :to="`/games/${game.id}`">
          {{ game.id }}
        </RouterLink>
      </li>
    </ul>
    <p v-else>
      No games to show
    </p>
  </div>
</template>

<script>
import * as rxjs from 'rxjs';
import { switchMap, map } from 'rxjs/operators';

import * as api from '../api';
import authService from '../api/auth';
import { channelPool } from '../api/pusher';

const isGameDone = game => game.state.playerStates[0].wonPieces === 7 || game.state.playerStates[1].wonPieces === 7;

const isCurrentPlayersTurn = (game, currentUserId) => game[game.state.currentPlayer] === currentUserId;

export default {
  computed: {
    gamesToShow() {
      if(this.games == null) {
        return null;
      }

      const userId = this.ownId;
      if(userId == null) {
        return [];
      }

      return this.games
        .filter(game => !isGameDone(game) && isCurrentPlayersTurn(game, userId));
    }
  },

  subscriptions() {
    return {
      ownId: authService.isAuthenticated$
        .pipe(map(isAuth => isAuth ? authService.userId : null)),

      games: authService.isAuthenticated$
        .pipe(
          switchMap(isAuthed =>
            isAuthed ?
              api.getGamesUpdates(channelPool) :
              rxjs.NEVER
          )
        )
    };
  }
};
</script>

<style scoped>
</style>
