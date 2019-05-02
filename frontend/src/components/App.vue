<template>
  <div class="container">
    <h1 class="title-bar">
      {{ title }}
      <button
        v-if="!authenticated"
        @click="login"
      >
        Log in / Sign up
      </button>
      <button
        v-if="authenticated"
        @click="logout"
      >
        Log out
      </button>
    </h1>
    <div class="contents">
      <!-- eslint-disable -->
      <router-view />
    </div>
  </div>
</template>

<script>
import authService from '../api/auth';
import * as api from '../api';
import { channelPool } from '../api/pusher';
import * as rxjs from 'rxjs';
import { flatMap, switchMap } from 'rxjs/operators';

export default {
  subscriptions() {
    return {
      authenticated: authService.isAuthenticated$,
      userInfo: authService.isAuthenticated$
        .pipe(
          switchMap(isAuthed =>
            isAuthed ?
              api.getUserInfoUpdates(channelPool, authService.userId) :
              rxjs.NEVER
          )
        ),
    };
  },

  computed: {
    title() {
      if(this.userInfo == null || this.userInfo.username == null) return 'Welcome!';

      return `Hello ${this.userInfo.username}!`;
    }
  },

  methods: {
    login() {
      authService.login();
    },

    logout() {
      authService.logout();
    }
  }
};
</script>

<style>
body {
  margin: 0;
}
</style>

<style scoped>
.container {
  width: 100vw;
  height: 100vh;
  display: grid;
  grid-template-columns: auto minmax(auto, 1000px) auto;
  grid-template-rows: min-content auto;
}

.title-bar {
  color: white;
  background: rgb(0, 255, 0);
  margin: 0;
  padding: 8px 16px;
  display: flex;
  align-items: center;
  grid-column-start: 2;
  grid-row-start: 1;
}

.title-bar > * {
  margin: 8px;
}

.contents {
  background: rgb(127, 127, 255);
  grid-column-start: 2;
  grid-row-start: 2;
  padding: 16px;
}
</style>
