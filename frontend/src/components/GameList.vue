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
          Game #{{ game.id }}: Vs. {{ opponentUsername(game) || '...' }} {{ isCurrentPlayersTurn(game) ? '(Your turn!)' : '' }}
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
import { switchMap, map, share, startWith, scan } from 'rxjs/operators';
import _ from 'lodash';

import * as api from '../api';
import authService from '../api/auth';
import { channelPool } from '../api/pusher';
import { forkByKey } from '../util';

const isGameDone = game => game.state.playerStates[0].wonPieces === 7 || game.state.playerStates[1].wonPieces === 7;

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

      return _.chain(this.games)
        .filter(game => !isGameDone(game))
        .sortBy(game => -game.id)
        .sortBy(game => !this.isCurrentPlayersTurn(game))
        .value();
    },
  },

  methods: {
    isCurrentPlayersTurn(game) {
      return game[game.state.currentPlayer] === this.ownId;
    },

    opponentUserId(game) {
      const ownId = this.ownId;
      if(ownId == null) {
        return null;
      }

      return game.player1 === ownId ? game.player2 : game.player1;
    },

    opponentUsername(game) {
      const uid = this.opponentUserId(game);

      if(uid == null || this.userInfos == null || this.userInfos[uid] == null) {
        return null;
      }

      return this.userInfos[uid].username;
    }
  },

  subscriptions() {
    const games = authService.isAuthenticated$
      .pipe(
        switchMap(isAuthed =>
          isAuthed ?
            api.getGamesUpdates(channelPool) :
            rxjs.NEVER
        ),
        share()
      );

    return {
      ownId: authService.isAuthenticated$
        .pipe(map(isAuth => isAuth ? authService.userId : null)),

      games,

      userInfos: games
        .pipe(
          map(gamesArr =>
            _.chain(gamesArr)
              .flatMap(game => [game.player1, game.player2])
              .uniq()
              .value()
          ),
          startWith([]),
          forkByKey(userId =>
            api.getUserInfoForUserUpdates(channelPool, userId)
              .pipe(
                map(data => ({ [userId]: data }))
              )
          ),
          scan((acc, cur) => _.assign({}, acc, cur), {}),
          startWith({})
        ),
    };
  }
};
</script>

<style scoped>
</style>
