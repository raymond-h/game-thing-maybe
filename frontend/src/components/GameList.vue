<template>
  <div>
    <h2>Ongoing games</h2>

    <p v-if="games == null">Loading...</p>
    <ul v-else-if="games.length > 0">
      <li
        v-for="game in games"
        :key="game.id"
      >
        <router-link :to="`/games/${game.id}`">{{game.id}}</router-link>
      </li>
    </ul>
    <p v-else>No games to show</p>
  </div>
</template>

<script>
import * as rxjs from 'rxjs';
import { switchMap } from 'rxjs/operators';

import * as api from '../api';
import authService from '../api/auth';
import { channelPool } from '../api/pusher';

export default {
  subscriptions() {
    return {
      games: authService.isAuthenticated$
        .pipe(
          switchMap(isAuthed =>
            isAuthed ?
              api.getGamesUpdates(channelPool) :
              rxjs.NEVER
          )
        )
    }
  }
};
</script>

<style scoped>
</style>
