<template>
  <div class="container">
    <h1 class="title-bar">
      {{ title }}
      <button
        v-if="!authenticated"
        @click="login"
      >
        Login
      </button>
      <button
        v-if="authenticated"
        @click="logout"
      >
        Logout
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

export default {
  data() {
    return { authenticated: false, userInfo: null };
  },

  computed: {
    title() {
      if(this.userInfo == null) return 'hello world';

      return `hello ${this.userInfo.name}`;
    }
  },

  mounted() {
    this._authSubs =
      authService.isAuthenticated$
        .subscribe(isAuth => this.authenticated = isAuth);

    this._userInfoSubs =
      authService.userInfo$.subscribe(userInfo => this.userInfo = userInfo);
  },

  beforeDestroy() {
    this._authSubs.unsubscribe();
    this._userInfoSubs.unsubscribe();
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
  grid-column-start: 2;
  grid-row-start: 1;
}

.contents {
  background: rgb(127, 127, 255);
  grid-column-start: 2;
  grid-row-start: 2;
}
</style>
