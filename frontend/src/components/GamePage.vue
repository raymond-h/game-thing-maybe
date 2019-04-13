<template>
  <div>
    <h1>Game {{gameId}}</h1>

    <my-game
      v-if="game != null"
      :game="game"
      :viewer-type="whichPlayer"
      v-stream:roll="gameAction$"
      v-stream:move="gameAction$"
      v-stream:add="gameAction$"
      v-stream:pass="gameAction$"
    >
    </my-game>
  </div>
</template>

<script>
import * as rxjs from 'rxjs';
import { switchMap, pluck, filter, startWith, map, flatMap, ignoreElements, catchError, tap } from 'rxjs/operators';

import * as api from '../api';
import authService from '../api/auth';
import { channelPool } from '../api/pusher';

import Game from './Game';

export default {
  props: ['gameId'],

  components: {
    'my-game': Game
  },

  computed: {
    whichPlayer() {
      if(this.game == null || this.ownId == null) {
        return null;
      }

      if(this.game.player1 === this.ownId) {
        return 'player1';
      }
      else if(this.game.player2 === this.ownId) {
        return 'player2';
      }
      else return null;
    }
  },

  domStreams: ['gameAction$'],

  subscriptions() {
    return {
      ownId: authService.isAuthenticated$
        .pipe(map(isAuth => isAuth ? authService.userId : null)),

      game: this.$watchAsObservable('gameId')
        .pipe(
          filter(d => d.newValue !== d.oldValue),
          pluck('newValue'),
          startWith(this.gameId),
          switchMap((gameId) =>
            (gameId != null) ?
              api.getGameUpdates(channelPool, gameId) :
              rxjs.NEVER
          )
        ),

      gameActions: this.gameAction$
        .pipe(
          map(ev => {
            switch(ev.event.name) {
              case 'roll': return { tag: 'moveRollDice' };
              case 'add': return { tag: 'moveAddPiece' };
              case 'move': return { tag: 'moveMovePiece', contents: ev.event.msg };
              case 'pass': return { tag: 'movePass' };
              default: return null;
            }
          }),
          flatMap(move =>
            api.performMove(this.gameId, move)
          ),
          ignoreElements(),
          catchError((err, obs) => rxjs.concat(rxjs.of(err), obs)),
          tap(err => alert(err.body.error))
        )
    };
  }
};
</script>

<style scoped>
</style>
