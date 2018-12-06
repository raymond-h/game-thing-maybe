import Vue from 'vue';
import VueRouter from 'vue-router';

import App from './components/App';
import router from './router';

Vue.use(VueRouter);

new Vue({
  el: '#root',
  render: h => h(App),
  router
});
